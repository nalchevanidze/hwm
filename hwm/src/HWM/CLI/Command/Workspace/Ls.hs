{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Ls (WorkspaceLsOptions, runWorkspaceLs) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Workspace (printWorkspace)
import Relude
import HWM.Integrations.Toolchain.Package (validatePackage)

data WorkspaceLsOptions = WorkspaceLsOptions
  deriving (Show)

instance ParseCLI WorkspaceLsOptions where
  parseCLI = pure WorkspaceLsOptions

runWorkspaceLs :: WorkspaceLsOptions -> ConfigT ()
runWorkspaceLs _ = printWorkspace validatePackage
