{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd) where

import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import Options.Applicative (help, metavar, strArgument)
import Relude

newtype WorkspaceAddOptions = WorkspaceAddOptions { workspaceName :: Text }
  deriving (Show)

instance ParseCLI WorkspaceAddOptions where
  parseCLI = WorkspaceAddOptions <$> strArgument (metavar "NAME" <> help "Name of the workspace to add")

runWorkspaceAdd :: WorkspaceAddOptions -> ConfigT ()
runWorkspaceAdd _ = pure () -- TODO: implement workspace addition
