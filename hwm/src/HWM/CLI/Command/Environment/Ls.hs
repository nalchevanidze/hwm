{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Ls (EnvLsOptions, runEnvLs) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Matrix (getBuildEnvironment, getBuildEnvironments, printEnvironments)
import Relude

data EnvLsOptions = EnvLsOptions
  deriving (Show)

instance ParseCLI EnvLsOptions where
  parseCLI = pure EnvLsOptions

runEnvLs :: EnvLsOptions -> ConfigT ()
runEnvLs _ = do
  active <- getBuildEnvironment Nothing
  envs <- getBuildEnvironments
  printEnvironments active envs
