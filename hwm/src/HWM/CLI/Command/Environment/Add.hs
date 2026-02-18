{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Add (EnvAddOptions, runEnvAdd) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Matrix (BuildEnv (..), Matrix (..))
import HWM.Runtime.Cache (getSnapshotGHC)
import Options.Applicative (help, metavar, strArgument)
import Relude

data EnvAddOptions = EnvAddOptions
  { envName :: Name,
    envResolver :: Name
  }
  deriving (Show)

instance ParseCLI EnvAddOptions where
  parseCLI =
    EnvAddOptions
      <$> strArgument (metavar "NAME" <> help "Name of the environment to add")
      <*> strArgument (metavar "RESOLVER" <> help "Stackage resolver (e.g. lts-21.24)")

runEnvAdd :: EnvAddOptions -> ConfigT ()
runEnvAdd EnvAddOptions {..} = do
  ghc <- getSnapshotGHC envResolver
  let newEnv =
        BuildEnv
          { name = envName,
            ghc = ghc,
            resolver = envResolver,
            extraDeps = Nothing,
            exclude = Nothing,
            allowNewer = Nothing
          }
  updateConfig
    ( \cfg@Config {..} ->
        let envs' = environments matrix ++ [newEnv]
            matrix' = matrix {environments = envs'}
         in pure cfg {matrix = matrix'}
    )
    (pure ())
