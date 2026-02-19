{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Result (Issue (..), MonadIssue (injectIssue), Severity (SeverityWarning))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Workspace (WorkspaceGroup (..), askWorkspaceGroups, editWorkgroup, forWorkspace, forWorkspaceTuple, parseWorkspaceId, selectGroup)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, long, metavar, strArgument, strOption, switch)
import Relude

data WorkspaceAddOptions = WorkspaceAddOptions
  { workspaceId :: (Name, Maybe Name),
    workspaceDir :: Maybe FilePath,
    prefix :: Maybe Text,
    publish :: Bool
  }
  deriving (Show)

instance ParseCLI WorkspaceAddOptions where
  parseCLI =
    (WorkspaceAddOptions . parseWorkspaceId <$> strArgument (metavar "NAME" <> help "Name of the workspace to add"))
      <*> optional (strOption (long "dir" <> help "Directory for the workspace (defaults to group name)"))
      <*> optional (strOption (long "prefix" <> help "Prefix to add to all member package names"))
      <*> switch (long "publish" <> help "Set if packages in this workspace should be published (use --publish for True, omit for False)")

runWorkspaceAdd :: WorkspaceAddOptions -> ConfigT ()
runWorkspaceAdd (WorkspaceAddOptions {workspaceId = (groupId, Nothing), ..}) = do
  -- TODO: implement group addition
  putLine $ "Adding workspace group: " <> groupId
  updateConfig (\cfg -> pure $ cfg {workspace = workspace cfg ++ [WorkspaceGroup groupId workspaceDir [] prefix (Just publish)]}) $ pure ()
runWorkspaceAdd (WorkspaceAddOptions {workspaceId = (groupId, Just memberId), ..}) = do
  when publish $ injectIssue (noEffect "publish")
  when (isJust prefix) $ injectIssue (noEffect "prefix")
  when (isJust workspaceDir) $ injectIssue (noEffect "dir")

  updateConfig
    ( \cfg ->
        ( do
            ws <- editWorkgroup groupId (\g -> g {members = members g <> [memberId]})
            pure $ cfg {workspace = ws}
        )
    )
    $ forWorkspaceTuple [] (const $ pure "")
  pure ()
  where
    noEffect label =
      Issue
        { issueTopic = memberId,
          issueMessage = "option \"--" <> label <> "\" is only relevant when adding a workspace group. They have no effect when adding a member package.",
          issueSeverity = SeverityWarning,
          issueDetails = Nothing
        }
