{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module HWM.CLI.Command.Environment.Remove (EnvRemoveOptions, runEnvRemove) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Config (Config(..))
import HWM.Domain.Matrix (removeEnvironmentByName)
import Options.Applicative (help, metavar, strArgument)
import Relude

newtype EnvRemoveOptions = EnvRemoveOptions
  {envName :: Name}
  deriving (Show)

instance ParseCLI EnvRemoveOptions where
  parseCLI =
    EnvRemoveOptions
      <$> strArgument (metavar "ENVIRONMENT" <> help "Name of the environment to remove")

runEnvRemove :: EnvRemoveOptions -> ConfigT ()
runEnvRemove EnvRemoveOptions{..} =
  updateConfig
    ( \cfg@Config{..} ->
        let mtx' = removeEnvironmentByName envName matrix
         in pure cfg {matrix = mtx'}
    )
    (pure ())
