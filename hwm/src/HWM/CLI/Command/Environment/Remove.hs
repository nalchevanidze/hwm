{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Remove (EnvRemoveOptions, runEnvRemove) where

import HWM.Core.Parsing (ParseCLI (..))
import Options.Applicative (pure)
import HWM.Domain.ConfigT (ConfigT)
import Relude

data EnvRemoveOptions = EnvRemoveOptions
  deriving (Show)

instance ParseCLI EnvRemoveOptions where
  parseCLI = pure EnvRemoveOptions


runEnvRemove :: EnvRemoveOptions -> ConfigT ()
runEnvRemove _ = pure ()
