{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Ls (EnvLsOptions, runEnvLs) where

import HWM.Core.Parsing (ParseCLI (..))
import Options.Applicative (pure)
import HWM.Domain.ConfigT (ConfigT)
import Relude

data EnvLsOptions = EnvLsOptions
  deriving (Show)

instance ParseCLI EnvLsOptions where
  parseCLI = pure EnvLsOptions


runEnvLs :: EnvLsOptions -> ConfigT ()
runEnvLs _ = pure ()
