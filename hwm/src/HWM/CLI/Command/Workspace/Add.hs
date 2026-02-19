{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Workspace.Add (WorkspaceAddOptions, runWorkspaceAdd) where

import HWM.Core.Common (Name)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Workspace (WorkspaceGroup (..), askWorkspaceGroups, parseWorkspaceId, selectGroup)
import HWM.Runtime.UI (putLine)
import Options.Applicative (help, metavar, strArgument)
import Relude

newtype WorkspaceAddOptions = WorkspaceAddOptions {workspaceId :: (Name, Maybe Name)}
  deriving (Show)

instance ParseCLI WorkspaceAddOptions where
  parseCLI = WorkspaceAddOptions . parseWorkspaceId <$> strArgument (metavar "NAME" <> help "Name of the workspace to add")

runWorkspaceAdd :: WorkspaceAddOptions -> ConfigT ()
runWorkspaceAdd (WorkspaceAddOptions (groupId, Nothing)) = do
  -- TODO: implement group addition
  putLine $ "Adding workspace group: " <> groupId
  updateConfig (\cfg -> pure $ cfg {workspace = workspace cfg ++ [WorkspaceGroup groupId Nothing [] Nothing Nothing]}) $ pure ()
runWorkspaceAdd (WorkspaceAddOptions (groupId, Just memberId)) = do
  g <- selectGroup groupId =<< askWorkspaceGroups
  -- TODO: implement package addition
  putLine $ "Adding package: " <> memberId <> " to workspace group: " <> show g
  pure ()
