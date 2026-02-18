{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry (
    RegistryOptions(..),
    RegistryCommand(..),
    runRegistry
) where

import Relude
import HWM.Domain.ConfigT (ConfigT)

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
        RegistryAdd{regPkg, regTarget} -> do
            -- TODO: Integrate logic from Add.hs here
            pure ()
        RegistryAudit{regFix} -> do
            -- TODO: Integrate logic from Outdated.hs here
            pure ()
        RegistryLs{regSearch} -> do
            -- TODO: List registry entries
            pure ()
