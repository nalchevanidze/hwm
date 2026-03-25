{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Sync (SyncMode (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import HWM.Core.Formatting (Format (..))
import HWM.Core.Parsing (Parse (..))
import Relude

data SyncMode = SyncModeSync | SyncModeCheck | SyncModeIgnore
  deriving (Generic, Show, Ord, Eq)

instance Parse SyncMode where
  parse "sync" = pure SyncModeSync
  parse "check" = pure SyncModeCheck
  parse "ignore" = pure SyncModeIgnore
  parse x = fail $ "Invalid sync mode: " <> toString x <> ". Expected one of: sync, check, ignore"

instance Format SyncMode where
  format SyncModeSync = "sync"
  format SyncModeCheck = "check"
  format SyncModeIgnore = "ignore"

instance FromJSON SyncMode where
  parseJSON (String x) = parse x
  parseJSON _ = fail "Invalid sync mode. Expected one of: sync, check, ignore"

instance ToJSON SyncMode where
  toJSON = String . format
