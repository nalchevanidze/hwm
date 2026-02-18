{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Add (EnvAddOptions, runEnvAdd) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (pure)
import Relude

instance ParseCLI EnvAddOptions where
  parseCLI = pure EnvAddOptions

data EnvAddOptions = EnvAddOptions
  deriving (Show)

runEnvAdd :: EnvAddOptions -> ConfigT ()
runEnvAdd _ = pure ()
