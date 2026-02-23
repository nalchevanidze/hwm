{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Environment.Root
  ( EnvCommand (..),
    runEnv,
  )
where

import HWM.CLI.Command.Environment.Add (EnvAddOptions, runEnvAdd)
import HWM.CLI.Command.Environment.Ls (EnvLsOptions, runEnvLs)
import HWM.CLI.Command.Environment.Remove (EnvRemoveOptions, runEnvRemove)
import HWM.CLI.Command.Environment.SetDefault (EnvSetDefaultOptions, runEnvSetDefault)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (command, info, progDesc, subparser)
import Relude

-- | Subcommands for `hwm environment`
data EnvCommand
  = EnvAdd EnvAddOptions
  | EnvRemove EnvRemoveOptions
  | EnvSetDefault EnvSetDefaultOptions
  | EnvLs EnvLsOptions
  deriving (Show)

runEnv :: EnvCommand -> ConfigT ()
runEnv cmd = case cmd of
  EnvAdd opts -> runEnvAdd opts
  EnvRemove opts -> runEnvRemove opts
  EnvSetDefault opts -> runEnvSetDefault opts
  EnvLs opts -> runEnvLs opts

instance ParseCLI EnvCommand where
  parseCLI =
    subparser
      ( mconcat
          [ command "add" (info (EnvAdd <$> parseCLI) (progDesc "Add a new environment.")),
            command "remove" (info (EnvRemove <$> parseCLI) (progDesc "Remove an environment.")),
            command "set-default" (info (EnvSetDefault <$> parseCLI) (progDesc "Set the default environment.")),
            command "ls" (info (EnvLs <$> parseCLI) (progDesc "List all environments."))
          ]
      )
