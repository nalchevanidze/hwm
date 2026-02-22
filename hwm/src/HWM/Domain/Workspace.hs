{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Workspace
  ( Workspace,
    WorkGroup (..),
    pkgRegistry,
    PkgRegistry,
    WorkspaceRef (..),
    memberPkgs,
    selectGroup,
    buildWorkspace,
    askWorkspaceGroups,
    resolveWorkspaces,
    forWorkspace,
    forWorkspaceTuple,
    parseWorkspaceRef,
    forWorkspaceCore,
    editWorkgroup,
    allPackages,
    getMembers,
  )
where

import Control.Monad.Error.Class
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (toJSON),
    genericToJSON,
    withText,
  )
import Data.Aeson.Types
  ( defaultOptions,
  )
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), availableOptions, chalk, commonPrefix, genMaxLen, monadStatus, padDots, slugify, statusIcon, subPathSign)
import HWM.Core.Has (Has (..), askEnv)
import HWM.Core.Pkg (Pkg (..), PkgName, makePkg)
import HWM.Core.Result
import HWM.Domain.Dependencies (DependencyGraph, sortByDependencyHierarchy)
import HWM.Runtime.Files (cleanRelativePath)
import HWM.Runtime.UI (MonadUI, putLine, sectionWorkspace)
import Relude

type Workspace = Map Name WorkGroup

data WorkspaceRef = WorkspaceRef
  { wsRefGroupId :: Name,
    wsRefMemberId :: Maybe Name
  }
  deriving (Show, Eq, Generic)

instance FromJSON WorkspaceRef where
  parseJSON = withText "WorkspaceRef" $ pure . parseWorkspaceRef

instance ToJSON WorkspaceRef where
  toJSON (WorkspaceRef g Nothing) = toJSON g
  toJSON (WorkspaceRef g (Just m)) = toJSON $ g <> "/" <> m

parseWorkspaceRef :: Text -> WorkspaceRef
parseWorkspaceRef input = case T.breakOn "/" input of
  (pkg, "") -> WorkspaceRef pkg Nothing -- No slash found
  (grp, rest) -> WorkspaceRef grp (Just (T.drop 1 rest)) -- Drop the "/"

data WorkGroup = WorkGroup
  { dir :: Maybe FilePath,
    members :: [Name],
    prefix :: Maybe Text
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON WorkGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

memberPkgs :: (MonadIO m, MonadError Issue m) => (Name, WorkGroup) -> m [Pkg]
memberPkgs (name, WorkGroup {..}) = traverse (makePkg name dir prefix) members

getMembers :: (MonadIO m, MonadError Issue m) => Workspace -> m [Pkg]
getMembers ws = do
  let groups = Map.toList ws
  pkgs <- traverse memberPkgs groups
  pure $ concat pkgs

allPackages ::
  ( MonadReader env m,
    Has env Workspace,
    MonadIO m,
    MonadError Issue m
  ) =>
  m [Pkg]
allPackages = askEnv >>= getMembers

type PkgRegistry = Map PkgName WorkGroup

resolveGroup :: (MonadIO m, MonadError Issue m) => (Name, WorkGroup) -> m PkgRegistry
resolveGroup g = Map.fromList . map ((,snd g) . pkgName) <$> memberPkgs g

pkgRegistry :: (MonadIO m, MonadError Issue m) => Workspace -> m PkgRegistry
pkgRegistry = fmap Map.unions . traverse resolveGroup . Map.toList

askWorkspaceGroups :: (MonadReader env m, Has env Workspace) => m Workspace
askWorkspaceGroups = asks obtain

resolveWorkspaces :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Workspace) => [Name] -> m [(Name, [Pkg])]
resolveWorkspaces names = do
  ws <- askWorkspaceGroups
  allPkgs <- (S.toList . S.fromList) . concat <$> traverse (resolveTarget ws) names
  let grouped = groupByGroupName allPkgs
  pure grouped

groupByGroupName :: [Pkg] -> [(Name, [Pkg])]
groupByGroupName pkgs =
  let sorted = sortOn pkgGroup pkgs
      grouped = groupBy (\a b -> pkgGroup a == pkgGroup b) sorted
   in [(maybe "" pkgGroup (viaNonEmpty head g), g) | g <- grouped, not (null g)]

resolveTarget :: (MonadIO m, MonadError Issue m) => Workspace -> Text -> m [Pkg]
resolveTarget ws target = do
  let (g, n) = parseWorkspaceRef target
  members <- selectGroup g ws >>= memberPkgs . (g,)
  resolveT members n

resolveT :: (MonadError Issue m) => [Pkg] -> Maybe Name -> m [Pkg]
resolveT pkgs Nothing = pure pkgs
resolveT pkgs (Just target) =
  case find (\p -> target == pkgMemberId p) pkgs of
    Just p -> pure [p]
    Nothing -> throwError $ fromString $ toString $ "Target not found: " <> target

selectGroup :: (MonadError Issue m) => Name -> Workspace -> m WorkGroup
selectGroup name groups = maybe (throwError $ fromString $ toString ("Workspace group \"" <> name <> "\" not found! " <> availableOptions (Map.keys groups))) pure (Map.lookup name groups)

buildWorkspace :: (Monad m, MonadError Issue m) => DependencyGraph -> [Pkg] -> m Workspace
buildWorkspace graph = fmap (Map.fromList . concat) . traverse groupToWorkspace . groupBy sameGroup . sortOn pkgGroup
  where
    sameGroup left right = pkgGroup left == pkgGroup right
    groupToWorkspace [] = pure []
    groupToWorkspace (pkg : pkgs) = do
      sortPkgs <- sortByDependencyHierarchy graph (pkg : pkgs)
      let (prefix, members) = commonPrefix (map pkgMemberId sortPkgs)
      pure
        [ ( if T.null (pkgGroup pkg) then "libs" else slugify (pkgGroup pkg),
            WorkGroup
              { dir = cleanRelativePath (Just $ toString (pkgGroup pkg)),
                members,
                prefix = prefix
                -- publish = derivePublish (pkgGroup pkg : members) TODO: move into release logic
              }
          )
        ]

-- derivePublish :: [Name] -> Maybe Bool
-- derivePublish names
--   | any (`elem` nonPublish) loweredNames = Nothing
--   | otherwise = Just True
--   where
--     loweredNames = map T.toLower names
--     nonPublish = ["examples", "example", "bench", "benchmarks"]

forWorkspace :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env Workspace) => (Pkg -> m ()) -> m ()
forWorkspace f = forWorkspaceCore $ \pkg -> do
  status <- monadStatus (f pkg)
  pure $ statusIcon status

forWorkspaceCore :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env Workspace) => (Pkg -> m Text) -> m ()
forWorkspaceCore f = do
  gs <- Map.toList <$> askWorkspaceGroups
  sectionWorkspace
    $ for_ gs
    $ \(name, wg) -> do
      putLine ""
      putLine $ "• " <> chalk Bold name
      pkgs <- memberPkgs (name, wg)
      let maxLen = genMaxLen (map pkgMemberId pkgs)
      for_ pkgs $ \pkg -> do
        status <- f pkg
        putLine $ subPathSign <> padDots maxLen (pkgMemberId pkg) <> status

forWorkspaceTuple :: (MonadUI m) => [(Text, [Pkg])] -> (Pkg -> m Text) -> m ()
forWorkspaceTuple ws f = sectionWorkspace $ do
  let maxLen = genMaxLen (map pkgMemberId $ concatMap snd ws)
  for_ ws $ \(name, pkgs) -> do
    putLine ""
    putLine $ "• " <> chalk Bold name
    for_ pkgs $ \pkg -> do
      status <- f pkg
      putLine (subPathSign <> padDots maxLen (pkgMemberId pkg) <> status)

editWorkgroup :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env Workspace) => Name -> (WorkGroup -> WorkGroup) -> m (Workspace, WorkGroup)
editWorkgroup name f = do
  ws <- askWorkspaceGroups
  c <- selectGroup name ws
  pure (Map.adjust f name ws, c)
