{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.SetDefault (EnvSetDefaultOptions, runEnvSetDefault) where

import HWM.Core.Parsing (ParseCLI (..))
import Options.Applicative (pure)
import HWM.Domain.ConfigT (ConfigT)
import Relude

data EnvSetDefaultOptions = EnvSetDefaultOptions
  deriving (Show)

instance ParseCLI EnvSetDefaultOptions where
  parseCLI = pure EnvSetDefaultOptions


runEnvSetDefault :: EnvSetDefaultOptions -> ConfigT ()
runEnvSetDefault _ = pure ()
