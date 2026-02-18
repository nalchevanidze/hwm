{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}


module HWM.CLI.Command.Registry (
    RegistryOptions(..),
    RegistryCommand(..),
    runRegistry
) where

import Relude
import qualified Data.Text as T
import HWM.Core.Pkg (PkgName(..))
import HWM.CLI.Command.Registry.Add (runRegistryAdd)
import HWM.CLI.Command.Registry.Audit (runRegistryAudit)
import HWM.CLI.Command.Registry.Ls (runRegistryLs)

-- | Subcommands for `hwm registry`
data RegistryCommand
    = RegistryAdd { regPkg :: Text, regTarget :: Maybe Text }
    | RegistryAudit { regFix :: Bool }
    | RegistryLs { regSearch :: Maybe Text }
    deriving (Show)

-- | Options for the registry command
data RegistryOptions = RegistryOptions
    { registryCommand :: RegistryCommand
    }
    deriving (Show)

runRegistry :: RegistryOptions -> ConfigT ()
runRegistry RegistryOptions{registryCommand} =
    case registryCommand of
        RegistryAdd{regPkg, regTarget} -> runRegistryAdd regPkg regTarget
        RegistryAudit{regFix} -> runRegistryAudit regFix
        RegistryLs{regSearch} -> runRegistryLs regSearch
