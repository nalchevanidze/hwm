{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Remove (EnvRemoveOptions, runEnvRemove) where

import Control.Monad.Except (MonadError (throwError))
import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Environments (existsEnviroment, printEnvironments, removeEnvironmentByName)
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
runEnvRemove EnvRemoveOptions {..} = do
  exists <- existsEnviroment envName
  unless exists $ throwError $ fromString $ "Environment '" <> toString envName <> "' does not exist."
  updateConfig
    ( \cfg@Config {..} ->
        let evs' = removeEnvironmentByName envName enviroments
         in pure cfg {enviroments = evs'}
    )
    (printEnvironments Nothing)
