{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Add (EnvAddOptions, runEnvAdd) where

import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), padDots)
import HWM.Core.Parsing (Parse (..), ParseCLI (..))
import HWM.Core.Version (Version)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Environments (Environments (..), existsEnviroment, newEnv, printEnvironments)
import HWM.Runtime.UI (putLine, section)
import Options.Applicative (help, metavar, strArgument)
import Options.Applicative.Builder (argument, str)
import Relude

data EnvAddOptions = EnvAddOptions
  { envName :: Name,
    envGHC :: Version
  }
  deriving (Show)

instance ParseCLI EnvAddOptions where
  parseCLI =
    EnvAddOptions
      <$> strArgument (metavar "NAME" <> help "Name of the environment to add")
      <*> argument (str >>= parse) (metavar "GHC" <> help "GHC version (e.g. 8.10.7)")

size :: Int
size = 16

runEnvAdd :: EnvAddOptions -> ConfigT ()
runEnvAdd EnvAddOptions {..} = do
  exists <- existsEnviroment envName
  if exists
    then do
      printEnvironments Nothing
      throwError $ fromString $ "Environment '" <> toString envName <> "' already exists."
    else section "new environment" $ do
      putLine $ padDots size "name" <> envName
      updateConfig
        ( \cfg@Config {..} -> do
            putLine $ padDots size "ghc" <> format envGHC
            pure cfg {cfgEnvironments = cfgEnvironments {envTargets = Map.insert envName (newEnv envGHC) (envTargets cfgEnvironments)}}
        )
        (pure ())
