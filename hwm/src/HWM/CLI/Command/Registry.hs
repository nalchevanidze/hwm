{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry
  ( RegistryOptions (..),
    RegistryCommand (..),
    runRegistry,
  )
where

import HWM.CLI.Command.Registry.Add (runRegistryAdd)
import HWM.CLI.Command.Registry.Audit (runRegistryAudit)
import HWM.CLI.Command.Registry.Ls (runRegistryLs)
import HWM.Domain.ConfigT (ConfigT)
import Relude
import HWM.Core.Pkg (PkgName)

-- | Subcommands for `hwm registry`
data RegistryCommand
  = RegistryAdd {regPkg :: PkgName, regTarget :: Maybe Text}
  | RegistryAudit {regFix :: Bool , fixForce :: Bool}
  | RegistryLs {regSearch :: Maybe Text}
  deriving (Show)

-- | Options for the registry command
newtype RegistryOptions = RegistryOptions
  { registryCommand :: RegistryCommand
  }
  deriving (Show)

runRegistry :: RegistryOptions -> ConfigT ()
runRegistry RegistryOptions {registryCommand} =
  case registryCommand of
    RegistryAdd {regPkg, regTarget} -> runRegistryAdd regPkg regTarget
    RegistryAudit {regFix, fixForce} -> runRegistryAudit regFix fixForce
    RegistryLs {regSearch} -> runRegistryLs regSearch
