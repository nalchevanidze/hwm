{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Ls (WorkspaceLsOptions, runWorkspaceLs) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Workspace (forWorkspace)
import HWM.Integrations.Toolchain.Package (validatePackage)
import Relude

data WorkspaceLsOptions = WorkspaceLsOptions
  deriving (Show)

instance ParseCLI WorkspaceLsOptions where
  parseCLI = pure WorkspaceLsOptions

runWorkspaceLs :: WorkspaceLsOptions -> ConfigT ()
runWorkspaceLs _ = forWorkspace validatePackage
