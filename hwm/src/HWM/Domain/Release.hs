{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Release
  ( Release (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value,
    genericParseJSON,
    genericToJSON,
  )
import HWM.Core.Common (Name)
import HWM.Runtime.Files (aesonYAMLOptionsAdvanced)
import Relude

data Release = Release
  { rlsArchive :: Maybe (Map Name Value),
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
