{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Release
  ( Release (..),
    ArtifactConfig (..),
    ArchiveFormat (..),
    formatArchiveTemplate,
    ReleaseArtifactConfigs,
    getArtifact,
    selectedArtifacts,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Map as Map
import Data.Yaml (Value (..))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), formatTemplate)
import HWM.Core.Parsing (Parse (..))
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Domain.Workspace (WorkspaceRef)
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

data ArtifactConfig = ArtifactConfig
  { arcSource :: Text,
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

instance FromJSON ArtifactConfig where
  parseJSON (String x) = pure $ defaultArchiveConfig x
  parseJSON v = genericParseJSON (aesonYAMLOptionsAdvanced prefix) v

instance ToJSON ArtifactConfig where
  toJSON v
    | isDefaultArchiveConfig v = String (arcSource v)
    | otherwise = genericToJSON (aesonYAMLOptionsAdvanced prefix) v
