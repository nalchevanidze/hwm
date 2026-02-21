{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd) where

import Control.Monad.Error.Class (MonadError (throwError))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (..), Status (Checked), chalk, displayStatus, padDots, subPathSign)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Pkg (PkgName (..), mkPkgDirPath, resolvePrefix)
import HWM.Core.Result (Issue (..), MonadIssue (injectIssue), Severity (SeverityError, SeverityWarning))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (config), updateConfig)
import HWM.Domain.Workspace (WorkspaceGroup (..), editWorkgroup, existsWokspaceGroup, parseWorkspaceId)
import HWM.Integrations.Scaffold (scaffoldPackage)
import HWM.Integrations.Toolchain.Hie (syncHie)
import HWM.Integrations.Toolchain.Stack (syncStackYaml)
import HWM.Runtime.UI (putLine, sectionConfig, sectionWorkspace)
import Options.Applicative (help, long, metavar, strArgument, strOption, switch)
import Relude

data WorkspaceAddOptions = WorkspaceAddOptions
  { opsWorkspaceId :: (Name, Maybe Name),
    opsWorkspaceDir :: Maybe FilePath,
    opsPrefix :: Maybe Text,
    opsPublish :: Bool
  }
  deriving (Show)

instance ParseCLI WorkspaceAddOptions where
  parseCLI =
    (WorkspaceAddOptions . parseWorkspaceId <$> strArgument (metavar "NAME" <> help "Name of the workspace to add"))
      <*> optional (strOption (long "dir" <> help "Directory for the workspace (defaults to group name)"))
      <*> optional (strOption (long "prefix" <> help "Prefix to add to all member package names"))
      <*> switch (long "publish" <> help "Set if packages in this workspace should be published (use --publish for True, omit for False)")

runWorkspaceAdd :: WorkspaceAddOptions -> ConfigT ()
runWorkspaceAdd (WorkspaceAddOptions {opsWorkspaceId = (groupId, Nothing), ..}) = do
  wss <- asks (cfgWorkspace . config)
  if existsWokspaceGroup groupId wss
    then
      throwError
        Issue
          { issueTopic = groupId,
            issueMessage = "A workspace group \"" <> groupId <> "\" already exists.",
            issueSeverity = SeverityError,
            issueDetails = Nothing
          }
    else do
      let ws = wss ++ [WorkspaceGroup groupId opsWorkspaceDir [] opsPrefix (Just opsPublish)]
      updateConfig (\cfg -> pure $ cfg {cfgWorkspace = ws}) $ sectionWorkspace $ do
        putLine ""
        putLine $ "• " <> chalk Bold groupId <> " " <> displayStatus [("added", Checked)]
runWorkspaceAdd (WorkspaceAddOptions {opsWorkspaceId = (groupId, Just memberId), ..}) = do
  when opsPublish $ injectIssue (noEffect "publish")
  when (isJust opsPrefix) $ injectIssue (noEffect "prefix")
  when (isJust opsWorkspaceDir) $ injectIssue (noEffect "dir")
  (ws, w) <- editWorkgroup groupId (\g -> g {members = members g <> [memberId]})
  if memberId `elem` members w
    then
      throwError
        Issue
          { issueTopic = memberId,
            issueMessage = "A member package with name \"" <> memberId <> "\" already exists in workspace group \"" <> groupId <> "\".",
            issueSeverity = SeverityError,
            issueDetails = Nothing
          }
    else do
      scaffoldPackage (mkPkgDirPath (dir w) (prefix w) memberId) (PkgName $ resolvePrefix (prefix w) memberId)
      updateConfig (\cfg -> pure $ cfg {cfgWorkspace = ws})
        $ sectionWorkspace
        $ do
          putLine ""
          putLine $ "• " <> chalk Bold groupId
          putLine $ subPathSign <> padDots 16 memberId <> displayStatus [("added", Checked)]
          sectionConfig
            0
            [ ("stack.yaml", syncStackYaml $> chalk Green "✓"),
              ("hie.yaml", syncHie $> chalk Green "✓")
            ]
      pure ()
  where
    noEffect label =
      Issue
        { issueTopic = memberId,
          issueMessage = "option \"--" <> label <> "\" is only relevant when adding a workspace group. They have no effect when adding a member package.",
          issueSeverity = SeverityWarning,
          issueDetails = Nothing
        }
