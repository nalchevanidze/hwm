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
  ( Enviroment (..),
    Environments (..),
    BuildEnvironment (..),
    StackEnvironment (..),
    getBuildEnvironments,
    getBuildEnvironment,
    hkgRefs,
    printEnvironments,
    getTestedRange,
    removeEnvironmentByName,
    newEnv,
    existsEnviroment,
    environmentHash,
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
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable (for)
import HWM.Core.Common
  ( Check (..),
    Name,
  )
import HWM.Core.Formatting (Color (..), Format (..), availableOptions, chalk)
import HWM.Core.Has (Has (..), HasAll, askEnv)
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Result (Issue)
import HWM.Core.Version (Era (..), Version, selectEra)
import HWM.Domain.Bounds (TestedRange (..))
import HWM.Domain.Workspace (Workspace, WorkspaceRef, allPackages, checkWorkspaceRefs, isMember)
import HWM.Runtime.Cache (Cache, Registry (currentEnv), VersionMap, getLatestNightlySnapshot, getRegistry, getSnapshot, getVersions)
import HWM.Runtime.Files (Signature, aesonYAMLOptions, aesonYAMLOptionsAdvanced, genSignature)
import HWM.Runtime.UI (MonadUI, forTable_, sectionEnvironments)
import Relude

type Extras = VersionMap

data Environments = Environments
  { envDefault :: Name,
    envTargets :: Map Name Enviroment
  }
  deriving
    ( Generic,
      Show
    )

newEnv :: Version -> Enviroment
newEnv ghc =
  Enviroment
    { ghc = ghc,
      exclude = Nothing,
      stack = Nothing
    }

environmentHash :: Environments -> Signature
environmentHash Environments {..} =
  genSignature $ Set.toList $ Set.fromList $ map toSig $ concatMap Map.toList $ mapMaybe (extraDeps <=< stack) (toList envTargets)
  where
    toSig (pkg, v) = format pkg <> "-" <> format v

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
    Has env Signature,
    MonadIO m
  ) =>
  Check m Environments
  where
  check Environments {..} = do
    fileSig <- askEnv
    traverse_ (checkTarget fileSig) envTargets
    where
      signature = environmentHash Environments {..}
      checkTarget fileSig Enviroment {..}
        | fileSig == signature = checkExclude
        -- checking all hkgRefs is expensive, so we skip it if the signature matches
        | otherwise = sequence_ [traverse_ check (maybe [] hkgRefs (extraDeps =<< stack)), checkExclude]
        where
          checkExclude = checkWorkspaceRefs (fromMaybe [] exclude)

data Enviroment = Enviroment
  { ghc :: Version,
    exclude :: Maybe [WorkspaceRef],
    stack :: Maybe StackEnvironment
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

instance FromJSON Enviroment where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Enviroment where
  toJSON = genericToJSON aesonYAMLOptions

data StackEnvironment = StackEnvironment
  { resolver :: Maybe Name,
    extraDeps :: Maybe Extras,
    allowNewer :: Maybe Bool
  }
  deriving (Generic, Show, Ord, Eq)

instance FromJSON StackEnvironment where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON StackEnvironment where
  toJSON = genericToJSON aesonYAMLOptions

data BuildEnvironment = BuildEnvironment
  { buildGHC :: Version,
    buildPkgs :: [Pkg],
    buildName :: Name,
    buildExtraDeps :: Maybe Extras,
    buildResolver :: Name,
    buildAllowNewer :: Maybe Bool
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

instance Format BuildEnvironment where
  format BuildEnvironment {..} = buildName <> " (" <> format buildGHC <> ")"

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
        { buildPkgs = excludePkgs env pkgs,
          buildName = name,
          buildExtraDeps = extraDeps =<< stack env,
          buildResolver = fromMaybe (eraStackageResolverName $ selectEra (ghc env)) (resolver =<< stack env),
          buildGHC = ghc env,
          buildAllowNewer = stack env >>= allowNewer
        }
  where
    excludePkgs build pkgs =
      let excluseion = fromMaybe [] (exclude build)
       in filter (not . (\pkg -> any (`isMember` pkg) excluseion)) pkgs

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
  sectionEnvironments (Just $ format def) $ forTable_ environments $ \env ->
    ( format env,
      pure
        $ if env == active
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
