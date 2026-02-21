{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Release
  ( Release (..),
    ArchiveConfig (..),
    ArchiveFormat (..),
    formatArchiveTemplate,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value,
    genericParseJSON,
    genericToJSON,
  )
import Data.Yaml (Value (..))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), formatTemplate)
import HWM.Core.Version (Version)
import HWM.Runtime.Files (aesonYAMLOptionsAdvanced)
import HWM.Runtime.Platform (Platform (..))
import Relude

data Release = Release
  { rlsArchive :: Maybe (Map Name ArchiveConfig),
    rlsPublish :: Maybe (Map Name Value)
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

data ArchiveConfig = ArchiveConfig
  { arcSource :: Text,
    arcFormat :: ArchiveFormat,
    arcGhcOptions :: [Text],
    arcNameTemplate :: Text
  }
  deriving
    ( Generic,
      Show,
      Ord,
      Eq
    )

data ArchiveFormat = Zip | TarGz | Auto
  deriving (Generic, Show, Ord, Eq)

instance FromJSON ArchiveFormat where
  parseJSON (String "zip") = pure Zip
  parseJSON (String "tar.gz") = pure TarGz
  parseJSON (String "auto") = pure Auto
  parseJSON _ = fail "Invalid archive format"

instance ToJSON ArchiveFormat where
  toJSON Zip = String "zip"
  toJSON TarGz = String "tar.gz"
  toJSON Auto = String "auto"

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

defaultArchiveConfig :: Text -> ArchiveConfig
defaultArchiveConfig src =
  ArchiveConfig
    { arcSource = src,
      arcFormat = Zip,
      arcGhcOptions =
        [ "-O2", -- High-level optimization
          "-split-sections", -- Enables dead-code elimination at the function level
          "-optl-s", -- Tells the linker to strip symbols
          "-threaded" -- Essential for modern CLI concurrency
        ],
      arcNameTemplate = defaultFormat
    }

isDefaultArchiveConfig :: ArchiveConfig -> Bool
isDefaultArchiveConfig arc = arc == defaultArchiveConfig (arcSource arc)

instance FromJSON ArchiveConfig where
  parseJSON (String x) = pure $ defaultArchiveConfig x
  parseJSON v = genericParseJSON (aesonYAMLOptionsAdvanced prefix) v

instance ToJSON ArchiveConfig where
  toJSON v
    | isDefaultArchiveConfig v = String (arcSource v)
    | otherwise = genericToJSON (aesonYAMLOptionsAdvanced prefix) v
