{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Workspace (WorkspaceGroup (..), askWorkspaceGroups, parseWorkspaceId, selectGroup)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, strArgument, strOption, switch)
import Relude

data WorkspaceAddOptions = WorkspaceAddOptions
  { workspaceId :: (Name, Maybe Name),
    workspaceDir :: Maybe FilePath,
    prefix :: Maybe Text,
    publish :: Maybe Bool
  }
  deriving (Show)

instance ParseCLI WorkspaceAddOptions where
  parseCLI =
    (WorkspaceAddOptions . parseWorkspaceId <$> strArgument (metavar "NAME" <> help "Name of the workspace to add"))
      <*> optional (strOption (long "dir" <> help "Directory for the workspace (defaults to group name)"))
      <*> optional (strOption (long "prefix" <> help "Prefix to add to all member package names"))
      <*> optional (switch (long "publish" <> help "Whether packages in this workspace should be published (true/false)"))

runWorkspaceAdd :: WorkspaceAddOptions -> ConfigT ()
runWorkspaceAdd (WorkspaceAddOptions {workspaceId = (groupId, Nothing), ..}) = do
  -- TODO: implement group addition
  putLine $ "Adding workspace group: " <> groupId
  updateConfig (\cfg -> pure $ cfg {workspace = workspace cfg ++ [WorkspaceGroup groupId workspaceDir [] prefix publish]}) $ pure ()
runWorkspaceAdd (WorkspaceAddOptions {workspaceId = (groupId, Just memberId), ..}) = do
  g <- selectGroup groupId =<< askWorkspaceGroups
  -- TODO: implement package addition
  putLine $ "Adding package: " <> memberId <> " to workspace group: " <> show g
  pure ()
