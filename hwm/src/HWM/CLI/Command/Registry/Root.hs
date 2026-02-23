{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Root
  ( RegistryCommand (..),
    runRegistry,
  )
where

import HWM.CLI.Command.Registry.Add (RegistryAddOptions, runRegistryAdd)
import HWM.CLI.Command.Registry.Audit (RegistryAuditOptions, runRegistryAudit)
import HWM.CLI.Command.Registry.Ls (RegistryLsOptions, runRegistryLs)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (command, info, progDesc, subparser)
import Relude

-- | Subcommands for `hwm registry`
data RegistryCommand
  = RegistryAdd RegistryAddOptions
  | RegistryAudit RegistryAuditOptions
  | RegistryLs RegistryLsOptions
  deriving (Show)

runRegistry :: RegistryCommand -> ConfigT ()
runRegistry registryCommand =
  case registryCommand of
    RegistryAdd opts -> runRegistryAdd opts
    RegistryAudit opts -> runRegistryAudit opts
    RegistryLs opts -> runRegistryLs opts

instance ParseCLI RegistryCommand where
  parseCLI =
    subparser
      ( mconcat
          [ command "add" (info (RegistryAdd <$> parseCLI) (progDesc "Add a dependency to the registry.")),
            command "audit" (info (RegistryAudit <$> parseCLI) (progDesc "Audit and optionally fix the registry.")),
            command "ls" (info (RegistryLs <$> parseCLI) (progDesc "List registry entries."))
          ]
      )
