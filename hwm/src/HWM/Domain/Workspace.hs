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
    buildWorkspace,
    resolveWorkspaces,
    forWorkspace,
    forWorkspaceTuple,
    parseWorkspaceRef,
    forWorkspaceCore,
    addWorkgroupMember,
    allPackages,
    resolveWsPkgs,
    WsPkgs,
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

type WsPkgs = [(Name, [Pkg])]

resolveGroup :: (MonadIO m, MonadError Issue m) => (Name, WorkGroup) -> m PkgRegistry
resolveGroup g = Map.fromList . map ((,snd g) . pkgName) <$> memberPkgs g

pkgRegistry :: (MonadIO m, MonadError Issue m) => Workspace -> m PkgRegistry
pkgRegistry = fmap Map.unions . traverse resolveGroup . Map.toList

askWorkspace :: (MonadReader env m, Has env Workspace) => m Workspace
askWorkspace = asks obtain

groupByGroupName :: [Pkg] -> WsPkgs
groupByGroupName pkgs =
  let sorted = sortOn pkgGroup pkgs
      grouped = groupBy (\a b -> pkgGroup a == pkgGroup b) sorted
   in [(maybe "" pkgGroup (viaNonEmpty head g), g) | g <- grouped, not (null g)]

resolveWorkspaces :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Workspace) => [Name] -> m WsPkgs
resolveWorkspaces = resolveWsPkgs . map parseWorkspaceRef

resolveWsPkgs :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Workspace) => [WorkspaceRef] -> m WsPkgs
resolveWsPkgs = fmap (groupByGroupName . (S.toList . S.fromList) . concat) . traverse resolveWsRef

resolveWsRef :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Workspace) => WorkspaceRef -> m [Pkg]
resolveWsRef wsRef = do
  ws <- askWorkspace
  members <- selectGroup (wsRefGroupId wsRef) ws >>= memberPkgs . (wsRefGroupId wsRef,)
  resolveT members (wsRefMemberId wsRef)

selectGroup :: (MonadError Issue m) => Name -> Workspace -> m WorkGroup
selectGroup name groups = maybe (throwError $ fromString $ toString ("Workspace group \"" <> name <> "\" not found! " <> availableOptions (Map.keys groups))) pure (Map.lookup name groups)

resolveT :: (MonadError Issue m) => [Pkg] -> Maybe Name -> m [Pkg]
resolveT pkgs Nothing = pure pkgs
resolveT pkgs (Just target) =
  case find (\p -> target == pkgMemberId p) pkgs of
    Just p -> pure [p]
    Nothing -> throwError $ fromString $ toString $ "Target not found: " <> target

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
              }
          )
        ]

forWorkspace :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env Workspace) => (Pkg -> m ()) -> m ()
forWorkspace f = forWorkspaceCore $ \pkg -> do
  status <- monadStatus (f pkg)
  pure $ statusIcon status

forWorkspaceCore :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env Workspace) => (Pkg -> m Text) -> m ()
forWorkspaceCore f = do
  gs <- Map.toList <$> askWorkspace
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

addWorkgroupMember :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env Workspace) => Name -> Name -> m (Workspace, WorkGroup)
addWorkgroupMember name memberId = do
  ws <- askWorkspace
  w <- selectGroup name ws
  if memberId `elem` members w
    then
      throwError
        Issue
          { issueTopic = memberId,
            issueMessage = "A member package with name \"" <> memberId <> "\" already exists in workspace group \"" <> name <> "\".",
            issueSeverity = SeverityError,
            issueDetails = Nothing
          }
    else pure (Map.adjust (\g -> g {members = members g <> [memberId]}) name ws, w)
