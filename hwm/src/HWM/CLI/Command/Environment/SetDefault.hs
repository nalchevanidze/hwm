{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.SetDefault (EnvSetDefaultOptions, runEnvSetDefault) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Environments (Environments (..), printEnvironments)
import qualified HWM.Domain.Environments as Environments
import Options.Applicative (help, metavar, strArgument)
import Relude

newtype EnvSetDefaultOptions = EnvSetDefaultOptions {envName :: Text}
  deriving (Show)

instance ParseCLI EnvSetDefaultOptions where
  parseCLI = EnvSetDefaultOptions <$> strArgument (metavar "ENV" <> help "Name of the environment to set as default")

runEnvSetDefault :: EnvSetDefaultOptions -> ConfigT ()
runEnvSetDefault EnvSetDefaultOptions {..} = do
  _ <- Environments.getBuildEnvironment (Just envName)
  let setDefaultEnv cfg = cfg {enviroments = (enviroments cfg) {envDefault = envName}}
  updateConfig (pure . setDefaultEnv) $ printEnvironments Nothing
