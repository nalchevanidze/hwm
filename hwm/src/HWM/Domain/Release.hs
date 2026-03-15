{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Release
  ( Release (..),
    ArtifactConfig (..),
    ArchiveFormat (..),
    formatArchiveTemplate,
    ReleaseArtifactConfigs,
    getArtifact,
    selectedArtifacts,
    resolveArtifactConfig,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Data.Aeson (FromJSON (..), ToJSON (toJSON), genericParseJSON, genericToJSON)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Yaml (Value (..))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), formatTemplate)
import HWM.Core.Has (Has)
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Pkg (Pkg)
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Domain.Environments (BuildEnvironment, Environments, getBuildEnvironment)
import HWM.Domain.Workspace (Workspace, WorkspaceRef, resolveWorkspaces)
import HWM.Runtime.Cache (Cache)
import HWM.Runtime.Files (aesonYAMLOptionsAdvanced)
import HWM.Runtime.Platform (Platform (..))
import Relude

type Publishables = Map Name [WorkspaceRef]

data Release = Release
  { rlsArtifacts :: Maybe (Map Name ArtifactConfig),
    rlsPublish :: Maybe Publishables
  }
  deriving
    ( Generic,
      Show
    )

prefix :: String
prefix = "rls"

instance FromJSON Release where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced prefix)

instance ToJSON Release where
  toJSON = genericToJSON (aesonYAMLOptionsAdvanced prefix)

type ReleaseArtifactConfigs = Map Name ArtifactConfig

getArtifact :: (MonadError Issue m) => Name -> ReleaseArtifactConfigs -> m ArtifactConfig
getArtifact name cfgs = case Map.lookup name cfgs of
  Just cfg -> pure cfg
  Nothing -> throwError $ fromString $ "Artifact \"" <> toString name <> "\" not found in release configuration."

selectedArtifacts :: (MonadError Issue m) => Maybe Name -> ReleaseArtifactConfigs -> m [(Name, ArtifactConfig)]
selectedArtifacts (Just target) cfgs = do
  cfg <- getArtifact target cfgs
  pure [(target, cfg)]
selectedArtifacts Nothing cfgs = pure $ Map.toList cfgs

prefixArtifactConfigRaw :: String
prefixArtifactConfigRaw = "_arc"

instance FromJSON ArtifactConfigRaw where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced prefixArtifactConfigRaw)

data ArtifactConfigRaw = ArtifactConfigRaw
  { _arcSource :: Text,
    _arcEnvironments :: Maybe [Name],
    _arcFormats :: Maybe [ArchiveFormat],
    _arcGhcOptions :: Maybe [Text],
    _arcNameTemplate :: Maybe Text
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

instance FromJSON ArtifactConfig where
  parseJSON (String x) = pure $ defaultArchiveConfig x
  parseJSON v = do
    ArtifactConfigRaw {..} <- parseJSON v
    pure
      $ ArtifactConfig
        { arcSource = _arcSource,
          arcEnvironments = fromMaybe (arcEnvironments $ defaultArchiveConfig _arcSource) _arcEnvironments,
          arcFormats = fromMaybe (arcFormats $ defaultArchiveConfig _arcSource) _arcFormats,
          arcGhcOptions = fromMaybe (arcGhcOptions $ defaultArchiveConfig _arcSource) _arcGhcOptions,
          arcNameTemplate = fromMaybe (arcNameTemplate $ defaultArchiveConfig _arcSource) _arcNameTemplate
        }

instance ToJSON ArtifactConfig where
  toJSON v
    | isDefaultArchiveConfig v = String (arcSource v)
    | otherwise = genericToJSON (aesonYAMLOptionsAdvanced prefix) v

data ArtifactConfig = ArtifactConfig
  { arcSource :: Text,
    arcEnvironments :: [Name],
    arcFormats :: [ArchiveFormat],
    arcGhcOptions :: [Text],
    arcNameTemplate :: Text
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

getArtifactEnvironments :: (Has env Cache, MonadError Issue m, Has env Environments, Has env Workspace, MonadReader env m, MonadIO m) => Release -> m [(Pkg, [BuildEnvironment])]
getArtifactEnvironments Release {..} = do
  let cfgs = toList (fromMaybe mempty rlsArtifacts)
  for cfgs $ \(ArtifactConfig {..}) -> do
    (_, pkg) <- resolveArtifactConfig ArtifactConfig {..}
    envs <- for arcEnvironments (getBuildEnvironment . Just)
    pure (pkg, envs)

resolveArtifactConfig :: (MonadError Issue m, Has env Workspace, MonadReader env m, MonadIO m) => ArtifactConfig -> m (Text, Pkg)
resolveArtifactConfig ArtifactConfig {..} = do
  let (workspaceId, executableName) = second (T.drop 1) (T.breakOn ":" arcSource)
  optTarget <- listToMaybe . concatMap snd <$> resolveWorkspaces [workspaceId]
  pkg <- maybe (throwError $ fromString $ toString $ "Package \"" <> workspaceId <> "\" not found in any workspace. Check package name and workspace configuration.") pure optTarget
  pure (executableName, pkg)

data ArchiveFormat = Zip | TarGz
  deriving (Generic, Show, Ord, Eq)

instance FromJSON ArchiveFormat where
  parseJSON = parseJSON >=> parse

instance Parse ArchiveFormat where
  parse "zip" = pure Zip
  parse "tar.gz" = pure TarGz
  parse s = fail $ "Invalid archive format: " <> toString s <> ". Supported: zip, tar.gz."

instance ToJSON ArchiveFormat where
  toJSON Zip = String "zip"
  toJSON TarGz = String "tar.gz"

defaultFormat :: Text
defaultFormat = "{{binary}}-v{{version}}-{{os}}-{{arch}}"

formatArchiveTemplate :: Name -> Version -> Platform -> Text -> Text
formatArchiveTemplate name version platform =
  formatTemplate
    [ ("binary", name),
      ("version", format version),
      ("os", format $ os platform),
      ("arch", format $ arch platform)
    ]

defaultArchiveConfig :: Text -> ArtifactConfig
defaultArchiveConfig src =
  ArtifactConfig
    { arcSource = src,
      arcEnvironments = [],
      arcFormats = [TarGz, Zip],
      arcGhcOptions =
        [ "-O2", -- High-level optimization
          "-split-sections", -- Enables dead-code elimination at the function level
          "-optl-s", -- Tells the linker to strip symbols
          "-threaded" -- Essential for modern CLI concurrency
        ],
      arcNameTemplate = defaultFormat
    }

isDefaultArchiveConfig :: ArtifactConfig -> Bool
isDefaultArchiveConfig arc = arc == defaultArchiveConfig (arcSource arc)
