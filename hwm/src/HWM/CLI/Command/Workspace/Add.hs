{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Workspace (parseWorkspaceId)
import Options.Applicative (help, metavar, strArgument)
import Relude
import HWM.Runtime.UI (putLine)

newtype WorkspaceAddOptions = WorkspaceAddOptions {workspaceId :: (Name, Maybe Name)}
  deriving (Show)

instance ParseCLI WorkspaceAddOptions where
  parseCLI = WorkspaceAddOptions . parseWorkspaceId <$> strArgument (metavar "NAME" <> help "Name of the workspace to add")

runWorkspaceAdd :: WorkspaceAddOptions -> ConfigT ()
runWorkspaceAdd (WorkspaceAddOptions (groupId, Nothing)) = do 
   -- TODO: implement workspace addition
   putLine $ "Adding workspace group: " <> groupId 
   pure ()
runWorkspaceAdd (WorkspaceAddOptions (groupId, Just memberId)) = do 
   -- TODO: implement workspace addition
   putLine $ "Adding package: " <> memberId <> " to workspace group: " <> groupId
   pure ()
