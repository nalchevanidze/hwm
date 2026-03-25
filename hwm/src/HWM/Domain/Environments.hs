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
    EnvironmentProfile (..),
    BuildEnvironment (..),
    StackBuildConfig (..),
    SyncMode (..),
    TargetModes (..),
    StackEnvironment (..),
    getBuildEnvironments,
    getBuildEnvironment,
    hkgRefs,
    printEnvironments,
    getTestedRange,
    removeEnvironmentByName,
    existsEnvironment,
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
import HWM.Core.Formatting (Color (..), Format (..), availableOptions, chalk, formatList)
import HWM.Core.Has (Has (..), HasAll, askEnv)
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Sync (SyncMode (..))
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
  { envsDefault :: Name,
    envsProfiles :: Map Name EnvironmentProfile,
    envsBuilder :: Maybe Builder,
    envsTargets :: Maybe TargetsPolicy
  }
  deriving
    ( Generic,
      Show
    )

addProfile :: Name -> EnvironmentProfile -> Environments -> Environments
addProfile name profile envs = envs {envsProfiles = Map.insert name profile (envsProfiles envs)}

mkEnvironment :: Version -> EnvironmentProfile
mkEnvironment ghc =
  EnvironmentProfile
    { profileGhc = ghc,
      profileExclude = Nothing,
      profileStack = Nothing,
      profileNix = Nothing,
      profileBuilder = Nothing,
      profileTargets = Nothing
    }

mkEnvironments :: Version -> Environments
mkEnvironments ghc =
  let defaultEnv = ("default", mkEnvironment ghc)
   in Environments
        { envsDefault = fst defaultEnv,
          envsProfiles = Map.fromList [defaultEnv],
          envsTargets = Nothing,
          envsBuilder = Nothing
        }

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
      checkTarget fileSig EnvironmentProfile {..}
        | fileSig == signature = checkExclude
        -- checking all hkgRefs is expensive, so we skip it if the signature matches
        | otherwise = sequence_ [traverse_ check (maybe [] hkgRefs (extraDeps =<< unfeature =<< profileStack)), checkExclude]
        where
          checkExclude = checkWorkspaceRefs (fromMaybe [] profileExclude)

data Feature a = Enabled a | Disabled deriving (Generic, Show, Ord, Eq)

unfeature :: Feature a -> Maybe a
unfeature (Enabled a) = Just a
unfeature Disabled = Nothing

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

data EnvironmentProfile = EnvironmentProfile
  { profileGhc :: Version,
    profileExclude :: Maybe [WorkspaceRef],
    profileStack :: Maybe (Feature StackEnvironment),
    profileNix :: Maybe (Feature NixEnvironment),
    profileBuilder :: Maybe Builder,
    profileTargets :: Maybe TargetsPolicy
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

profilePrefix :: String
profilePrefix = "profile"

instance FromJSON EnvironmentProfile where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced profilePrefix)

instance ToJSON EnvironmentProfile where
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

data TargetsPolicy = TargetsPolicy
  { tpCabal :: Maybe SyncMode,
    tpStack :: Maybe SyncMode,
    tpNix :: Maybe SyncMode,
    tpHie :: Maybe SyncMode,
    tpPackages :: Maybe SyncMode
  }
  deriving (Generic, Show, Ord, Eq)

targetsPrefix :: String
targetsPrefix = "tp"

instance FromJSON TargetsPolicy where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced targetsPrefix)

instance ToJSON TargetsPolicy where
  toJSON = genericToJSON (aesonYAMLOptionsAdvanced targetsPrefix)

data TargetModes = TargetModes
  { targetCabal :: SyncMode,
    targetStack :: SyncMode,
    targetNix :: SyncMode,
    targetHie :: SyncMode,
    targetPackages :: SyncMode
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

data StackBuildConfig = StackBuildConfig
  { stackResolver :: Name,
    stackExtraDeps :: Maybe Extras,
    stackAllowNewer :: Maybe Bool
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

data BuildEnvironment = BuildEnvironment
  { buildGHC :: Version,
    buildPkgs :: [Pkg],
    buildName :: Name,
    buildStack :: StackBuildConfig,
    buildBuilder :: Builder,
    buildTargets :: TargetModes
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

instance Format TargetModes where
  format TargetModes {..} =
    let enabled :: [Text]
        enabled =
          ["cabal" | active targetCabal]
            <> ["stack" | active targetStack]
            <> ["nix" | active targetNix]
            <> ["hie" | active targetHie]
     in "targets=" <> if null enabled then "-" else formatList "," enabled
    where
      active mode = mode /= SyncModeIgnore

deriveTargetModes :: Builder -> Bool -> Bool -> TargetModes
deriveTargetModes builder stackEnabled nixEnabled =
  case builder of
    CabalBuilder {inNixDevelopment = False} -> TargetModes {targetCabal = SyncModeSync, targetStack = SyncModeIgnore, targetNix = SyncModeIgnore, targetHie = SyncModeSync, targetPackages = SyncModeSync}
    CabalBuilder {inNixDevelopment = True} ->
      TargetModes
        { targetCabal = SyncModeSync,
          targetStack = SyncModeIgnore,
          targetNix = if nixEnabled then SyncModeSync else SyncModeIgnore,
          targetHie = SyncModeSync,
          targetPackages = SyncModeSync
        }
    StackBuilder ->
      TargetModes
        { targetCabal = SyncModeIgnore,
          targetStack = if stackEnabled then SyncModeSync else SyncModeIgnore,
          targetNix = SyncModeIgnore,
          targetHie = SyncModeSync,
          targetPackages = SyncModeSync
        }
    NixBuilder ->
      TargetModes
        { targetCabal = SyncModeIgnore,
          targetStack = SyncModeIgnore,
          targetNix = if nixEnabled then SyncModeSync else SyncModeIgnore,
          targetHie = SyncModeSync,
          targetPackages = SyncModeSync
        }

applyTargetsPolicy :: Maybe TargetsPolicy -> Maybe TargetsPolicy -> TargetModes -> TargetModes
applyTargetsPolicy globalPolicy profilePolicy defaults =
  defaults
    { targetCabal = pick tpCabal (targetCabal defaults),
      targetStack = pick tpStack (targetStack defaults),
      targetNix = pick tpNix (targetNix defaults),
      targetHie = pick tpHie (targetHie defaults),
      targetPackages = pick tpPackages (targetPackages defaults)
    }
  where
    pick getter fallback = fromMaybe fallback ((getter =<< profilePolicy) <|> (getter =<< globalPolicy))

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
    let builder = fromMaybe (CabalBuilder False) (profileBuilder env <|> envsBuilder globalEnv)
    let stackEnabled = case profileStack env of
          Just Disabled -> False
          _ -> True
    let nixEnabled = case profileNix env of
          Just Disabled -> False
          _ -> True
    let targetModes = applyTargetsPolicy (envsTargets globalEnv) (profileTargets env) (deriveTargetModes builder stackEnabled nixEnabled)
    pure
      BuildEnvironment
        { buildPkgs = excludePkgs env pkgs,
          buildName = name,
          buildStack =
            StackBuildConfig
              { stackExtraDeps = extraDeps =<< unfeature =<< profileStack env,
                stackResolver = fromMaybe (eraStackageResolverName $ selectEra (profileGhc env)) (resolver =<< unfeature =<< profileStack env),
                stackAllowNewer = profileStack env >>= unfeature >>= allowNewer
              },
          buildGHC = profileGhc env,
          buildBuilder = builder,
          buildTargets = targetModes
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

existsEnvironment :: (MonadReader env m, Has env Environments) => Name -> m Bool
existsEnvironment n = do
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
          then chalk Cyan (stackResolver (buildStack env) <> " (active)")
          else chalk Gray (stackResolver (buildStack env))
    )

getTestedRange :: (Monad m, MonadReader env m, Has env Workspace, Has env Environments, MonadIO m, MonadError Issue m) => m TestedRange
getTestedRange = do
  env <- getBuildEnvironments
  legacy <- getSnapshot (minimum $ map (stackResolver . buildStack) env)
  nightly <- getLatestNightlySnapshot
  pure TestedRange {legacy = legacy, nightly = nightly}

-- | Remove an environment from the matrix by name, with optional default migration.
removeEnvironmentByName :: (MonadError Issue m) => Name -> Maybe Name -> Environments -> m Environments
removeEnvironmentByName envName maybeNewDefault envs@Environments {..}
  | not (Map.member envName envsProfiles) = throwError $ fromString $ "Environment '" <> toString envName <> "' does not exist."
  | Map.size envsProfiles <= 1 = throwError "Cannot remove the last environment. Add another environment first."
  | envName /= envsDefault && isJust maybeNewDefault = throwError "--set-default can only be used when removing the current default environment."
  | otherwise =
      let nextProfiles = Map.delete envName envsProfiles
       in if envName /= envsDefault
            then pure envs {envsProfiles = nextProfiles}
            else case maybeNewDefault of
              Nothing -> throwError "Removing the current default environment requires --set-default <ENV>."
              Just newDefault
                | newDefault == envName -> throwError "--set-default cannot point to the environment being removed."
                | not (Map.member newDefault nextProfiles) -> throwError $ fromString $ "Environment '" <> toString newDefault <> "' does not exist after removal."
                | otherwise -> pure envs {envsProfiles = nextProfiles, envsDefault = newDefault}

selectEnvironments :: (MonadError Issue m, MonadIO m, MonadReader env m, Has env Workspace, Has env Environments, Has env Cache) => [Name] -> m [BuildEnvironment]
selectEnvironments ["all"] = getBuildEnvironments
selectEnvironments [] = getBuildEnvironment Nothing >>= \env -> pure [env]
selectEnvironments names = for names (getBuildEnvironment . Just)
