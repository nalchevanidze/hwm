{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace
  ( WorkspaceCommand(..)
  , runWorkspace
  ) where

import HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd)
import HWM.CLI.Command.Workspace.Ls (WorkspaceLsOptions, runWorkspaceLs)
import HWM.Core.Parsing (ParseCLI(..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (command, info, progDesc, subparser)
import Relude

-- | Subcommands for `hwm workspace`
data WorkspaceCommand
  = WorkspaceAdd WorkspaceAddOptions
  | WorkspaceLs WorkspaceLsOptions
  deriving (Show)

runWorkspace :: WorkspaceCommand -> ConfigT ()
runWorkspace cmd = case cmd of
  WorkspaceAdd opts -> runWorkspaceAdd opts
  WorkspaceLs opts -> runWorkspaceLs opts

instance ParseCLI WorkspaceCommand where
  parseCLI =
    subparser
      ( mconcat
          [ command "add" (info (WorkspaceAdd <$> parseCLI) (progDesc "Add a new workspace."))
          , command "ls" (info (WorkspaceLs <$> parseCLI) (progDesc "List all workspaces."))
          ]
      )
