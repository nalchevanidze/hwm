{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Matrix
  ( BuildEnv (..),
    Matrix (..),
    BuildEnvironment (..),
    getBuildEnvironments,
    getBuildEnvironment,
    hkgRefs,
    printEnvironments,
    getTestedRange,
    removeEnvironmentByName,
    newEnv,
    existsEnviroment,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    genericParseJSON,
    genericToJSON,
  )
import Data.Foldable (Foldable (..))
import Data.List ((\\))
import qualified Data.Map as M
import Data.Traversable (for)
import HWM.Core.Common
  ( Check (..),
    Name,
  )
import HWM.Core.Formatting (Color (..), Format (..), availableOptions, chalk)
import HWM.Core.Has (Has, HasAll, askEnv)
import HWM.Core.Pkg (Pkg (..), PkgName, pkgId)
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Domain.Bounds (TestedRange (..))
import HWM.Domain.Workspace (WorkspaceGroup, memberPkgs)
import HWM.Runtime.Cache (Cache, Registry (currentEnv), VersionMap, getLatestNightlySnapshot, getRegistry, getSnapshot, getVersions)
import HWM.Runtime.Files (aesonYAMLOptions)
import HWM.Runtime.UI (MonadUI, forTable, sectionEnvironments)
import Relude

type Extras = VersionMap

data Matrix = Matrix
  { defaultEnvironment :: Name,
    environments :: [BuildEnv]
  }
  deriving
    ( Generic,
      Show
    )

newEnv :: Name -> Version -> Name -> BuildEnv
newEnv name ghc resolver =
  BuildEnv
    { name = name,
      ghc = ghc,
      resolver = resolver,
      extraDeps = Nothing,
      exclude = Nothing,
      allowNewer = Nothing
    }

instance FromJSON Matrix where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Matrix where
  toJSON = genericToJSON aesonYAMLOptions

instance
  ( MonadError Issue m,
    MonadReader env m,
    Has env Matrix,
    Has env [WorkspaceGroup],
    Has env Cache,
    MonadIO m
  ) =>
  Check m Matrix
  where
  check Matrix {..} = traverse_ check environments

data BuildEnv = BuildEnv
  { name :: Name,
    ghc :: Version,
    resolver :: Name,
    extraDeps :: Maybe Extras,
    exclude :: Maybe [Text],
    allowNewer :: Maybe Bool
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

instance FromJSON BuildEnv where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON BuildEnv where
  toJSON = genericToJSON aesonYAMLOptions

instance
  ( MonadError Issue m,
    MonadReader env m,
    Has env [WorkspaceGroup],
    Has env Cache,
    MonadIO m
  ) =>
  Check m BuildEnv
  where
  check BuildEnv {..} =
    sequence_
      [ traverse_ check (maybe [] hkgRefs extraDeps),
        checkPkgNames exclude
      ]

checkPkgNames ::
  ( MonadError Issue m,
    MonadReader env m,
    Has env [WorkspaceGroup],
    MonadIO m
  ) =>
  Maybe [Text] ->
  m ()
checkPkgNames ls = do
  known <- map pkgId <$> askAllPackages
  let unknown = fromMaybe [] ls \\ known
  unless (null unknown) (throwError $ fromString ("unknown packages: " <> show unknown))

data BuildEnvironment = BuildEnvironment
  { buildEnv :: BuildEnv,
    buildPkgs :: [Pkg],
    buildName :: Name,
    buildExtraDeps :: Maybe Extras,
    buildResolver :: Name
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

instance Format BuildEnvironment where
  format BuildEnvironment {..} = buildName <> " (" <> format (ghc buildEnv) <> ")"

getBuildEnvironments ::
  ( MonadReader env m,
    Has env Matrix,
    Has env [WorkspaceGroup],
    MonadIO m,
    MonadError Issue m
  ) =>
  m [BuildEnvironment]
getBuildEnvironments = do
  envs <- environments <$> askEnv
  for envs $ \env -> do
    pkgs <- askAllPackages
    pure
      BuildEnvironment
        { buildEnv = env,
          buildPkgs = excludePkgs env pkgs,
          buildName = name env,
          buildExtraDeps = extraDeps env,
          buildResolver = resolver env
        }
  where
    excludePkgs build = filter (\p -> pkgId p `notElem` fromMaybe [] (exclude build))

getBuildEnvironment ::
  ( MonadReader env m,
    Has env Matrix,
    Has env [WorkspaceGroup],
    Has env Cache,
    MonadIO m,
    MonadError Issue m
  ) =>
  Maybe Name ->
  m BuildEnvironment
getBuildEnvironment inputName = do
  envs <- getBuildEnvironments
  defaultname <- defaultEnvironment <$> askEnv
  case inputName of
    Just name -> matchEnv envs name (select envs name)
    Nothing -> do
      cachedName <- currentEnv <$> getRegistry
      matchEnv envs defaultname (select envs cachedName <|> select envs defaultname)
  where
    select envs name = find ((name ==) . buildName) envs
    matchEnv envs name = maybe (throwError $ fromString $ toString ("No matching Environment for input '" <> name <> "'! " <> availableOptions (map buildName envs))) pure

data HkgRef = HkgRef
  { pkgName :: PkgName,
    pkgVersion :: Version
  }
  deriving (Eq, Ord)

instance
  ( MonadError Issue m,
    MonadReader env m,
    HasAll env [[WorkspaceGroup], Cache],
    MonadIO m
  ) =>
  Check m HkgRef
  where
  check HkgRef {..} = getVersions pkgName >>= checkElem (pkgVersion ==) . toList
    where
      checkElem f xs =
        if isJust (find f xs)
          then pure ()
          else
            throwError
              $ fromString
              $ toString
                ("No matching version for '" <> format pkgVersion <> "'! " <> availableOptions xs)

hkgRefs :: VersionMap -> [HkgRef]
hkgRefs = map (uncurry HkgRef) . M.toList

instance Format HkgRef where
  format HkgRef {..} = format pkgName <> "-" <> format pkgVersion

askGroups :: (MonadReader env m, Has env [WorkspaceGroup]) => m [WorkspaceGroup]
askGroups = askEnv

askAllPackages ::
  ( MonadReader env m,
    Has env [WorkspaceGroup],
    MonadIO m,
    MonadError Issue m
  ) =>
  m [Pkg]
askAllPackages = do
  groups <- askGroups
  concat <$> traverse memberPkgs groups

existsEnviroment :: (MonadReader env m, Has env Matrix) => Name -> m Bool
existsEnviroment n = do
  envs <- environments <$> askEnv
  pure $ isJust $ find ((n ==) . name) envs

printEnvironments :: (Monad m, MonadUI m, MonadReader env m, Has env [WorkspaceGroup], Has env Matrix, MonadIO m, MonadError Issue m, Has env Cache) => Maybe Name -> m ()
printEnvironments name = do
  active <- getBuildEnvironment name
  def <- defaultEnvironment <$> askEnv
  environments <- getBuildEnvironments
  sectionEnvironments (Just $ format def) $ forTable 0 environments $ \env ->
    ( format env,
      if env == active
        then chalk Cyan (buildResolver env <> " (active)")
        else chalk Gray (buildResolver env)
    )

getTestedRange :: (Monad m, MonadReader env m, Has env [WorkspaceGroup], Has env Matrix, MonadIO m, MonadError Issue m) => m TestedRange
getTestedRange = do
  env <- getBuildEnvironments
  legacy <- getSnapshot (minimum $ map buildResolver env)
  nightly <- getLatestNightlySnapshot
  pure TestedRange {legacy = legacy, nightly = nightly}

-- | Remove an environment from the matrix by name
removeEnvironmentByName :: Name -> Matrix -> Matrix
removeEnvironmentByName envName matrix =
  matrix {environments = filter ((envName /=) . name) (environments matrix)}
