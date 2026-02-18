{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Publish (publish) where

import Control.Monad.Error.Class (MonadError (..))
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting
  ( Color (..),
    Format (..),
    chalk,
    genMaxLen,
    padDots,
    statusIcon,
  )
import HWM.Core.Pkg (Pkg (..))
import HWM.Core.Result (Issue, Severity (..), maxSeverity)
import HWM.Domain.ConfigT (ConfigT, askVersion)
import HWM.Domain.Workspace (WorkspaceGroup, askWorkspaceGroups, canPublish, memberPkgs, pkgGroupName, selectGroup)
import HWM.Integrations.Toolchain.Stack (sdist, upload)
import HWM.Runtime.UI (printSummary, putLine, section, sectionTableM, sectionWorkspace)
import Relude hiding (intercalate)

failIssues :: [Issue] -> ConfigT ()
failIssues [] = pure ()
failIssues issues = do
  printSummary issues
  when (maxSeverity issues == Just SeverityError) $ liftIO exitFailure

collectGroups :: Maybe Name -> [WorkspaceGroup] -> ConfigT [WorkspaceGroup]
collectGroups Nothing ws = pure $ filter canPublish ws
collectGroups (Just target) ws = do
  groups <- traverse (`selectGroup` ws) [target]
  let notPublishable = filter (not . canPublish) groups
  for_ notPublishable $ \g ->
    throwError $ fromString $ toString $ "Target group \"" <> pkgGroupName g <> "\" cannot be published. Check workspace group configuration."
  pure groups

publish :: Maybe Name -> ConfigT ()
publish target = do
  ws <- askWorkspaceGroups
  groups <- collectGroups target ws
  version <- askVersion
  when (null groups) $ throwError "No publishable groups found. Check workspace group configuration."

  sectionTableM
    0
    "publish"
    [ ("version", pure $ chalk Magenta (format version)),
      ("target", pure $ chalk Cyan (format (T.intercalate ", " (map pkgGroupName groups)))),
      ("registry", pure "hackage")
    ]

  issues <- traverse memberPkgs groups >>= traverse sdist . concat
  failIssues (concat issues)

  sectionWorkspace $ for_ groups $ \g ->
    section (chalk Bold (pkgGroupName g)) $ do
      pkgs <- memberPkgs g
      for_ pkgs $ \pkg -> do
        (status, publishIssues) <- upload pkg
        putLine $ "└── " <> padDots (genMaxLen (map pkgMemberId pkgs)) (pkgMemberId pkg) <> statusIcon status
        failIssues publishIssues
