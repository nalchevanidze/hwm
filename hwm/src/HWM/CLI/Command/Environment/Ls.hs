{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Ls (EnvLsOptions, runEnvLs) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Environments (printEnvironments)
import Relude

data EnvLsOptions = EnvLsOptions
  deriving (Show)

instance ParseCLI EnvLsOptions where
  parseCLI = pure EnvLsOptions

runEnvLs :: EnvLsOptions -> ConfigT ()
runEnvLs _ = printEnvironments Nothing
