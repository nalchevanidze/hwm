{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Audit (runRegistryAudit, RegistryAuditOptions (..)) where

import HWM.Core.Formatting (Color (..), chalk)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Core.Result (Issue (..), MonadIssue (..), Severity (..))
import HWM.Domain.Bounds (BoundCompliance (..), auditBounds, auditHasAny, formatAudit, updateDepBounds)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, updateConfig)
import HWM.Domain.Environments (getTestedRange)
import HWM.Domain.Registry (askRegistry, mapDeps, mapWithName)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.UI (putLine, section, sectionTableM, uiFormatTable)
import Options.Applicative
import Relude

data RegistryAuditOptions = RegistryAuditOptions {auditFix :: Bool, auditForce :: Bool} deriving (Show)

instance ParseCLI RegistryAuditOptions where
  parseCLI =
    RegistryAuditOptions
      <$> switch (long "fix" <> help "Automatically fix issues")
      <*> switch (long "force" <> help "Force actions")

runRegistryAudit :: RegistryAuditOptions -> ConfigT ()
runRegistryAudit RegistryAuditOptions {..} = do
  originalRegistry <- askRegistry
  range <- getTestedRange
  sectionTableM "audit" [("mode", pure (if auditFix then if auditForce then chalk Yellow "fix (force)" else chalk Cyan "fix" else "check"))]

  let dependencyAudits = filter (auditHasAny (/= Valid)) $ mapWithName (auditBounds range) originalRegistry

  if null dependencyAudits
    then do
      section "registry" $ putLine "all dependencies are up to date."
    else do
      section "registry" $ uiFormatTable $ formatAudit <$> dependencyAudits
      let errorCount = length $ filter (auditHasAny (== Conflict)) dependencyAudits
      if auditFix
        then ((\cf -> pure $ cf {cfgRegistry = Just $ mapDeps (updateDepBounds auditForce range) originalRegistry}) `updateConfig`) $ do
          syncPackages
        else do
          injectIssue
            ( Issue
                { issueDetails = Nothing,
                  issueMessage = "Found " <> show (length dependencyAudits - errorCount) <> " outdated dependencies: Run 'hwm registry audit --fix --force' to update.",
                  issueTopic = "registry",
                  issueSeverity = SeverityWarning
                }
            )
          when (errorCount > 0)
            $ injectIssue
              ( Issue
                  { issueDetails = Nothing,
                    issueMessage = "Found " <> show errorCount <> " outdated dependencies: Run 'hwm registry audit --fix' to update.",
                    issueTopic = "registry",
                    issueSeverity = SeverityError
                  }
              )
