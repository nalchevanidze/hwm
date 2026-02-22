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

module HWM.Domain.Environments
  ( EnviromentTarget (..),
    Environments (..),
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
import qualified Data.Map as Map
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
import HWM.Domain.Workspace (Workspace, allPackages)
import HWM.Runtime.Cache (Cache, Registry (currentEnv), VersionMap, getLatestNightlySnapshot, getRegistry, getSnapshot, getVersions)
import HWM.Runtime.Files (aesonYAMLOptions, aesonYAMLOptionsAdvanced)
import HWM.Runtime.UI (MonadUI, forTable, sectionEnvironments)
import Relude

type Extras = VersionMap

data Environments = Environments
  { envDefault :: Name,
    envTargets :: Map Name EnviromentTarget
  }
  deriving
    ( Generic,
      Show
    )

newEnv :: Version -> Name -> EnviromentTarget
newEnv ghc resolver =
  EnviromentTarget
    { ghc = ghc,
      resolver = resolver,
      extraDeps = Nothing,
      exclude = Nothing,
      allowNewer = Nothing
    }

prefix :: String
prefix = "env"

instance FromJSON Environments where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced prefix)

instance ToJSON Environments where
  toJSON = genericToJSON (aesonYAMLOptionsAdvanced prefix)

instance
  ( MonadError Issue m,
    MonadReader env m,
    Has env Environments,
    Has env Workspace,
    Has env Cache,
    MonadIO m
  ) =>
  Check m Environments
  where
  check Environments {..} = traverse_ check envTargets

data EnviromentTarget = EnviromentTarget
  { ghc :: Version,
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

instance FromJSON EnviromentTarget where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON EnviromentTarget where
  toJSON = genericToJSON aesonYAMLOptions

instance
  ( MonadError Issue m,
    MonadReader env m,
    Has env Workspace,
    Has env Cache,
    MonadIO m
  ) =>
  Check m EnviromentTarget
  where
  check EnviromentTarget {..} =
    sequence_
      [ traverse_ check (maybe [] hkgRefs extraDeps),
        checkPkgNames exclude
      ]

checkPkgNames ::
  ( MonadError Issue m,
    MonadReader env m,
    Has env Workspace,
    MonadIO m
  ) =>
  Maybe [Text] ->
  m ()
checkPkgNames ls = do
  known <- map pkgId <$> allPackages
  let unknown = fromMaybe [] ls \\ known
  unless (null unknown) (throwError $ fromString ("unknown packages: " <> show unknown))

data BuildEnvironment = BuildEnvironment
  { buildEnv :: EnviromentTarget,
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
    Has env Environments,
    Has env Workspace,
    MonadIO m,
    MonadError Issue m
  ) =>
  m [BuildEnvironment]
getBuildEnvironments = do
  envs <- envTargets <$> askEnv
  for (Map.toList envs) $ \(name, env) -> do
    pkgs <- allPackages
    pure
      BuildEnvironment
        { buildEnv = env,
          buildPkgs = excludePkgs env pkgs,
          buildName = name,
          buildExtraDeps = extraDeps env,
          buildResolver = resolver env
        }
  where
    excludePkgs build = filter (\p -> pkgId p `notElem` fromMaybe [] (exclude build))

getBuildEnvironment ::
  ( MonadReader env m,
    Has env Environments,
    Has env Workspace,
    Has env Cache,
    MonadIO m,
    MonadError Issue m
  ) =>
  Maybe Name ->
  m BuildEnvironment
getBuildEnvironment inputName = do
  envs <- getBuildEnvironments
  defaultname <- envDefault <$> askEnv
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
    HasAll env [Workspace, Cache],
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

existsEnviroment :: (MonadReader env m, Has env Environments) => Name -> m Bool
existsEnviroment n = do
  envs <- envTargets <$> askEnv
  pure $ isJust $ Map.lookup n envs

printEnvironments :: (Monad m, MonadUI m, MonadReader env m, Has env Workspace, Has env Environments, MonadIO m, MonadError Issue m, Has env Cache) => Maybe Name -> m ()
printEnvironments name = do
  active <- getBuildEnvironment name
  def <- envDefault <$> askEnv
  environments <- getBuildEnvironments
  sectionEnvironments (Just $ format def) $ forTable 0 environments $ \env ->
    ( format env,
      if env == active
        then chalk Cyan (buildResolver env <> " (active)")
        else chalk Gray (buildResolver env)
    )

getTestedRange :: (Monad m, MonadReader env m, Has env Workspace, Has env Environments, MonadIO m, MonadError Issue m) => m TestedRange
getTestedRange = do
  env <- getBuildEnvironments
  legacy <- getSnapshot (minimum $ map buildResolver env)
  nightly <- getLatestNightlySnapshot
  pure TestedRange {legacy = legacy, nightly = nightly}

-- | Remove an environment from the matrix by name
removeEnvironmentByName :: Name -> Environments -> Environments
removeEnvironmentByName envName matrix =
  matrix {envTargets = Map.delete envName (envTargets matrix)}
