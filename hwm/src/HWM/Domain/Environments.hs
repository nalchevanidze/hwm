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
  ( Environments (..),
    EnviromentProfile (..),
    BuildEnvironment (..),
    StackEnvironment (..),
    getBuildEnvironments,
    getBuildEnvironment,
    hkgRefs,
    printEnvironments,
    getTestedRange,
    removeEnvironmentByName,
    existsEnviroment,
    environmentHash,
    NixEnvironment (..),
    Feature (..),
    selectEnvironments,
    overrideBuilder,
    mkEnvironments,
    mkEnvironment,
    addProfile,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON (..), ToJSON (toJSON), genericParseJSON, genericToJSON, object)
import Data.Aeson.Types (Value (..))
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
import HWM.Domain.Build (Builder (..))
import HWM.Domain.Workspace (Workspace, WorkspaceRef, allPackages, checkWorkspaceRefs, isMember)
import HWM.Runtime.Cache (Cache, Registry (currentEnv), VersionMap, getLatestNightlySnapshot, getRegistry, getSnapshot, getVersions)
import HWM.Runtime.Files (Signature, aesonYAMLOptions, aesonYAMLOptionsAdvanced, genSignature)
import HWM.Runtime.UI (MonadUI, forTable_, sectionEnvironments)
import Relude

type Extras = VersionMap

data Environments = Environments
  { envsBuilder :: Maybe Builder,
    envsDefault :: Name,
    envsNix :: Maybe Bool,
    envsStack :: Maybe Bool,
    envsProfiles :: Map Name EnviromentProfile
  }
  deriving
    ( Generic,
      Show
    )

addProfile :: Name -> EnviromentProfile -> Environments -> Environments
addProfile name profile envs = envs {envsProfiles = Map.insert name profile (envsProfiles envs)}

mkEnvironment :: Version -> EnviromentProfile
mkEnvironment ghc =
  EnviromentProfile
    { profileGhc = ghc,
      profileExclude = Nothing,
      profileStack = Nothing,
      profileNix = Nothing,
      profileBuilder = Nothing
    }

mkEnvironments :: Version -> Environments
mkEnvironments ghc =
  let defaultEnv = ("default", mkEnvironment ghc)
   in Environments {envsDefault = fst defaultEnv, envsProfiles = Map.fromList [defaultEnv], envsStack = Nothing, envsNix = Nothing, envsBuilder = Nothing}

environmentHash :: Environments -> Signature
environmentHash Environments {..} =
  genSignature $ Set.toList $ Set.fromList $ map toSig $ concatMap Map.toList $ mapMaybe (extraDeps <=< unfeature <=< profileStack) (toList envsProfiles)
  where
    toSig (pkg, v) = format pkg <> "-" <> format v

prefix :: String
prefix = "envs"

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
    traverse_ (checkTarget fileSig) envsProfiles
    where
      signature = environmentHash Environments {..}
      checkTarget fileSig EnviromentProfile {..}
        | fileSig == signature = checkExclude
        -- checking all hkgRefs is expensive, so we skip it if the signature matches
        | otherwise = sequence_ [traverse_ check (maybe [] hkgRefs (extraDeps =<< unfeature =<< profileStack)), checkExclude]
        where
          checkExclude = checkWorkspaceRefs (fromMaybe [] profileExclude)

data Feature a = Enabled a | Disabled deriving (Generic, Show, Ord, Eq)

unfeature :: Feature a -> Maybe a
unfeature (Enabled a) = Just a
unfeature Disabled = Nothing

isEnabled :: Maybe Bool -> Maybe (Feature a) -> Bool
isEnabled _ (Just (Enabled _)) = True
isEnabled _ (Just Disabled) = False
isEnabled global Nothing = fromMaybe False global

instance FromJSON (Feature StackEnvironment) where
  parseJSON (Bool b) = pure $ if b then Enabled (StackEnvironment Nothing Nothing Nothing) else Disabled
  parseJSON v = Enabled <$> parseJSON v

instance FromJSON (Feature NixEnvironment) where
  parseJSON (Bool b) = pure $ if b then Enabled NixEnvironment else Disabled
  parseJSON v = Enabled <$> parseJSON v

instance (ToJSON a) => ToJSON (Feature a) where
  toJSON (Enabled se) = toJSON se
  toJSON Disabled = Bool False

data NixEnvironment = NixEnvironment deriving (Generic, Show, Ord, Eq)

instance ToJSON NixEnvironment where
  toJSON NixEnvironment = object []

instance FromJSON NixEnvironment where
  parseJSON (Object _) = pure NixEnvironment
  parseJSON _ = fail "Invalid Nix environment configuration. Expected an object or a boolean."

data EnviromentProfile = EnviromentProfile
  { profileGhc :: Version,
    profileExclude :: Maybe [WorkspaceRef],
    profileStack :: Maybe (Feature StackEnvironment),
    profileNix :: Maybe (Feature NixEnvironment),
    profileBuilder :: Maybe Builder
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

profilePrefix :: String
profilePrefix = "profile"

instance FromJSON EnviromentProfile where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced profilePrefix)

instance ToJSON EnviromentProfile where
  toJSON = genericToJSON (aesonYAMLOptionsAdvanced profilePrefix)

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
    buildAllowNewer :: Maybe Bool,
    buildStack :: Bool,
    buildNix :: Bool,
    buildBuilder :: Builder
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

overrideBuilder :: Builder -> BuildEnvironment -> BuildEnvironment
overrideBuilder builder env = env {buildBuilder = builder}

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
  globalEnv <- askEnv
  envs <- envsProfiles <$> askEnv
  for (Map.toList envs) $ \(name, env) -> do
    pkgs <- allPackages
    pure
      BuildEnvironment
        { buildPkgs = excludePkgs env pkgs,
          buildName = name,
          buildExtraDeps = extraDeps =<< unfeature =<< profileStack env,
          buildResolver = fromMaybe (eraStackageResolverName $ selectEra (profileGhc env)) (resolver =<< unfeature =<< profileStack env),
          buildGHC = profileGhc env,
          buildAllowNewer = profileStack env >>= unfeature >>= allowNewer,
          buildStack = isEnabled (envsStack globalEnv) (profileStack env),
          buildNix = isEnabled (envsNix globalEnv) (profileNix env),
          buildBuilder = fromMaybe (CabalBuilder False) (profileBuilder env <|> envsBuilder globalEnv)
        }
  where
    excludePkgs build pkgs =
      let excluseion = fromMaybe [] (profileExclude build)
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
  defaultname <- envsDefault <$> askEnv
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
  envs <- envsProfiles <$> askEnv
  pure $ isJust $ Map.lookup n envs

printEnvironments :: (Monad m, MonadUI m, MonadReader env m, Has env Workspace, Has env Environments, MonadIO m, MonadError Issue m, Has env Cache) => Maybe Name -> m ()
printEnvironments name = do
  active <- getBuildEnvironment name
  def <- envsDefault <$> askEnv
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
  matrix {envsProfiles = Map.delete envName (envsProfiles matrix)}

selectEnvironments :: (MonadError Issue m, MonadIO m, MonadReader env m, Has env Workspace, Has env Environments, Has env Cache) => [Name] -> m [BuildEnvironment]
selectEnvironments ["all"] = getBuildEnvironments
selectEnvironments [] = getBuildEnvironment Nothing >>= \env -> pure [env]
selectEnvironments names = for names (getBuildEnvironment . Just)
