{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Remove (EnvRemoveOptions, runEnvRemove) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Environments (printEnvironments, removeEnvironmentByName)
import Options.Applicative (help, long, metavar, strArgument, strOption)
import Relude

data EnvRemoveOptions = EnvRemoveOptions
  { envName :: Name,
    envSetDefault :: Maybe Name
  }
  deriving (Show)

instance ParseCLI EnvRemoveOptions where
  parseCLI =
    EnvRemoveOptions
      <$> strArgument (metavar "ENVIRONMENT" <> help "Name of the environment to remove")
      <*> optional
        ( strOption
            ( long "set-default"
                <> metavar "ENV"
                <> help "Required when removing current default environment; choose the new default."
            )
        )

runEnvRemove :: EnvRemoveOptions -> ConfigT ()
runEnvRemove EnvRemoveOptions {..} =
  updateConfig
    ( \cfg@Config {..} -> do
        nextEnvs <- removeEnvironmentByName envName envSetDefault cfgEnvironments
        pure cfg {cfgEnvironments = nextEnvs}
    )
    printEnvironments
