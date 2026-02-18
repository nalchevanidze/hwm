{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HWM.CLI.Command.Environment.SetDefault (EnvSetDefaultOptions, runEnvSetDefault) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import qualified HWM.Domain.Matrix as Matrix
import Options.Applicative (help, metavar, strArgument)
import Relude

newtype EnvSetDefaultOptions = EnvSetDefaultOptions {envName :: Text}
  deriving (Show)

instance ParseCLI EnvSetDefaultOptions where
  parseCLI = EnvSetDefaultOptions <$> strArgument (metavar "ENV" <> help "Name of the environment to set as default")

runEnvSetDefault :: EnvSetDefaultOptions -> ConfigT ()
runEnvSetDefault EnvSetDefaultOptions {..} = do
  -- Validate environment exists using Matrix.getBuildEnvironment
  _ <- Matrix.getBuildEnvironment (Just envName)
  let setDefaultEnv cfg = cfg { matrix = (matrix cfg) { Matrix.defaultEnvironment = envName } }
  updateConfig (pure . setDefaultEnv) $ do
    putText $ "Default environment set to: " <> envName <> "\n"
