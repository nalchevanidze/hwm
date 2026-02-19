{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Workspace
  ( WorkspaceGroup (..),
    pkgGroupName,
    pkgRegistry,
    PkgRegistry,
    memberPkgs,
    selectGroup,
    canPublish,
    buildWorkspaceGroups,
    askWorkspaceGroups,
    resolveWorkspaces,
    forWorkspace,
    forWorkspaceTuple,
    parseWorkspaceId,
    forWorkspaceCore,
    editWorkgroup,
    existsWokspaceGroup,
  )
where

import Control.Monad.Error.Class
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (toJSON),
    genericToJSON,
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
import HWM.Core.Has (Has (..))
import HWM.Core.Pkg (Pkg (..), PkgName, makePkg)
import HWM.Core.Result
import HWM.Domain.Dependencies (DependencyGraph, sortByDependencyHierarchy)
import HWM.Runtime.Files (cleanRelativePath)
import HWM.Runtime.UI (MonadUI, putLine, sectionWorkspace)
import Relude

data WorkspaceGroup = WorkspaceGroup
  { name :: Name,
    dir :: Maybe FilePath,
    members :: [Name],
    prefix :: Maybe Text,
    publish :: Maybe Bool
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON WorkspaceGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

memberPkgs :: (MonadIO m, MonadError Issue m) => WorkspaceGroup -> m [Pkg]
memberPkgs WorkspaceGroup {..} = traverse (makePkg name dir prefix) members

pkgGroupName :: WorkspaceGroup -> Name
pkgGroupName WorkspaceGroup {..} = name

type PkgRegistry = Map PkgName WorkspaceGroup

resolveGroup :: (MonadIO m, MonadError Issue m) => WorkspaceGroup -> m PkgRegistry
resolveGroup g = Map.fromList . map ((,g) . pkgName) <$> memberPkgs g

pkgRegistry :: (MonadIO m, MonadError Issue m) => [WorkspaceGroup] -> m PkgRegistry
pkgRegistry = fmap Map.unions . traverse resolveGroup

askWorkspaceGroups :: (MonadReader env m, Has env [WorkspaceGroup]) => m [WorkspaceGroup]
askWorkspaceGroups = asks obtain

resolveWorkspaces :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env [WorkspaceGroup]) => [Name] -> m [(Name, [Pkg])]
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

parseWorkspaceId :: Text -> (Text, Maybe Text)
parseWorkspaceId input = case T.breakOn "/" input of
  (pkg, "") -> (pkg, Nothing) -- No slash found
  (grp, rest) -> (grp, Just (T.drop 1 rest)) -- Drop the "/"

resolveTarget :: (MonadIO m, MonadError Issue m) => [WorkspaceGroup] -> Text -> m [Pkg]
resolveTarget ws target = do
  let (g, n) = parseWorkspaceId target
  members <- selectGroup g ws >>= memberPkgs
  resolveT members n

resolveT :: (MonadError Issue m) => [Pkg] -> Maybe Name -> m [Pkg]
resolveT pkgs Nothing = pure pkgs
resolveT pkgs (Just target) =
  case find (\p -> target == pkgMemberId p) pkgs of
    Just p -> pure [p]
    Nothing -> throwError $ fromString $ toString $ "Target not found: " <> target

existsWokspaceGroup :: Name -> [WorkspaceGroup] -> Bool
existsWokspaceGroup name groups = not $ any ((== name) . pkgGroupName) groups

selectGroup :: (MonadError Issue m) => Name -> [WorkspaceGroup] -> m WorkspaceGroup
selectGroup name groups =
  maybe (throwError $ fromString $ toString ("Workspace group \"" <> name <> "\" not found! " <> availableOptions (map pkgGroupName groups))) pure (find ((== name) . pkgGroupName) groups)

canPublish :: WorkspaceGroup -> Bool
canPublish WorkspaceGroup {publish} = fromMaybe False publish

buildWorkspaceGroups :: (Monad m, MonadError Issue m) => DependencyGraph -> [Pkg] -> m [WorkspaceGroup]
buildWorkspaceGroups graph = fmap concat . traverse groupToWorkspace . groupBy sameGroup . sortOn pkgGroup
  where
    sameGroup left right = pkgGroup left == pkgGroup right
    groupToWorkspace [] = pure []
    groupToWorkspace (pkg : pkgs) = do
      sortPkgs <- sortByDependencyHierarchy graph (pkg : pkgs)
      let (prefix, members) = commonPrefix (map pkgMemberId sortPkgs)
      pure
        [ WorkspaceGroup
            { name = if T.null (pkgGroup pkg) then "libs" else slugify (pkgGroup pkg),
              dir = cleanRelativePath (Just $ toString (pkgGroup pkg)),
              members,
              prefix = prefix,
              publish = derivePublish (pkgGroup pkg : members)
            }
        ]

derivePublish :: [Name] -> Maybe Bool
derivePublish names
  | any (`elem` nonPublish) loweredNames = Nothing
  | otherwise = Just True
  where
    loweredNames = map T.toLower names
    nonPublish = ["examples", "example", "bench", "benchmarks"]

forWorkspace :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env [WorkspaceGroup]) => (Pkg -> m ()) -> m ()
forWorkspace f = forWorkspaceCore $ \pkg -> do
  status <- monadStatus (f pkg)
  pure $ statusIcon status

forWorkspaceCore :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env [WorkspaceGroup]) => (Pkg -> m Text) -> m ()
forWorkspaceCore f = do
  gs <- askWorkspaceGroups
  sectionWorkspace
    $ for_ gs
    $ \g -> do
      putLine ""
      putLine $ "• " <> chalk Bold (pkgGroupName g)
      pkgs <- memberPkgs g
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

editWorkgroup :: (MonadIO m, MonadUI m, MonadIssue m, MonadError Issue m, MonadReader env m, Has env [WorkspaceGroup]) => Name -> (WorkspaceGroup -> WorkspaceGroup) -> m ([WorkspaceGroup], WorkspaceGroup)
editWorkgroup name f = do
  ws <- askWorkspaceGroups
  c <- selectGroup name ws
  pure (map (\g -> if pkgGroupName g == name then f g else g) ws, c)
