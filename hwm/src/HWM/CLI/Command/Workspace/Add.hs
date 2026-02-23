{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd) where

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.Map as Map
import HWM.Core.Formatting (Color (..), Status (Checked), chalk, displayStatus, padDots, subPathSign)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Pkg (PkgName (..), mkPkgDirPath, resolvePrefix)
import HWM.Core.Result (Issue (..), MonadIssue (injectIssue), Severity (SeverityError, SeverityWarning))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, Env (config), updateConfig)
import HWM.Domain.Workspace (WorkGroup (..), WorkspaceRef (..), addWorkgroupMember, parseWorkspaceRef)
import HWM.Integrations.Scaffold (scaffoldPackage)
import HWM.Integrations.Toolchain.Hie (syncHie)
import HWM.Integrations.Toolchain.Stack (syncStackYaml)
import HWM.Runtime.UI (putLine, sectionConfig, sectionWorkspace)
import Options.Applicative (help, long, metavar, strArgument, strOption)
import Relude

data WorkspaceAddOptions = WorkspaceAddOptions
  { opsWorkspaceId :: WorkspaceRef,
    opsWorkspaceDir :: Maybe FilePath,
    opsPrefix :: Maybe Text
  }
  deriving (Show)

instance ParseCLI WorkspaceAddOptions where
  parseCLI =
    (WorkspaceAddOptions . parseWorkspaceRef <$> strArgument (metavar "NAME" <> help "Name of the workspace to add"))
      <*> optional (strOption (long "dir" <> help "Directory for the workspace (defaults to group name)"))
      <*> optional (strOption (long "prefix" <> help "Prefix to add to all member package names"))

runWorkspaceAdd :: WorkspaceAddOptions -> ConfigT ()
runWorkspaceAdd (WorkspaceAddOptions {opsWorkspaceId = WorkspaceRef groupId Nothing, ..}) = do
  wss <- asks (cfgWorkspace . config)
  if Map.member groupId wss
    then
      throwError
        Issue
          { issueTopic = groupId,
            issueMessage = "A workspace group \"" <> groupId <> "\" already exists.",
            issueSeverity = SeverityError,
            issueDetails = Nothing
          }
    else do
      let ws = Map.insert groupId (WorkGroup opsWorkspaceDir [] opsPrefix) wss
      updateConfig (\cfg -> pure $ cfg {cfgWorkspace = ws}) $ sectionWorkspace $ do
        putLine ""
        putLine $ "• " <> chalk Bold groupId <> " " <> displayStatus [("added", Checked)]
runWorkspaceAdd (WorkspaceAddOptions {opsWorkspaceId = WorkspaceRef groupId (Just memberId), ..}) = do
  when (isJust opsPrefix) $ injectIssue (noEffect "prefix")
  when (isJust opsWorkspaceDir) $ injectIssue (noEffect "dir")
  (ws, w) <- addWorkgroupMember groupId memberId
  scaffoldPackage (mkPkgDirPath (dir w) (prefix w) memberId) (PkgName $ resolvePrefix (prefix w) memberId)
  updateConfig (\cfg -> pure $ cfg {cfgWorkspace = ws})
    $ sectionWorkspace
    $ do
      putLine ""
      putLine $ "• " <> chalk Bold groupId
      putLine $ subPathSign <> padDots 16 memberId <> displayStatus [("added", Checked)]
      sectionConfig
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
