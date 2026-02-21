{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Release
  ( Release (..),
    ArchiveConfig (..),
    ArchiveFormat (..),
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
import HWM.Runtime.Files (aesonYAMLOptionsAdvanced)
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
    arcStrip :: Bool,
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
  parseJSON (String "zip") = pure Zip
  parseJSON (String "tar.gz") = pure TarGz
  parseJSON _ = fail "Invalid archive format"

instance ToJSON ArchiveFormat where
  toJSON Zip = String "zip"
  toJSON TarGz = String "tar.gz"

defaultArchiveConfig :: Text -> ArchiveConfig
defaultArchiveConfig src =
  ArchiveConfig
    { arcSource = src,
      arcFormat = Zip,
      arcStrip = True,
      arcNameTemplate = "{{binary}}-v{{version}}-{{os}}-{{arch}}"
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
