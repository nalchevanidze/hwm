{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Build (Builder (..)) where

import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
import HWM.Core.Formatting (Format (..))
import Relude

data Builder
  = CabalBuilder
  | StackBuilder
  | NixBuilder
  deriving (Generic, Show, Ord, Eq)

instance FromJSON Builder where
  parseJSON (String "cabal") = pure CabalBuilder
  parseJSON (String "stack") = pure StackBuilder
  parseJSON (String "nix") = pure NixBuilder
  parseJSON _ = fail "Invalid builder. Expected 'cabal', 'stack', or 'nix'."

instance ToJSON Builder where
  toJSON = String . format

instance Format Builder where
  format CabalBuilder = "cabal"
  format StackBuilder = "stack"
  format NixBuilder = "nix"
