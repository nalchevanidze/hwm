{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Audit (runRegistryAudit) where

import HWM.Core.Formatting (Color (..), chalk)
import HWM.Core.Result (Issue (..), MonadIssue (..), Severity (..))
import HWM.Domain.Bounds (BoundCompliance (..), auditBounds, auditHasAny, formatAudit, updateDepBounds)
import HWM.Domain.Config (Config (registry))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Domain.Dependencies (mapDeps, mapWithName)
import HWM.Domain.Matrix (getTestedRange)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.UI (indent, printGenTable, putLine, section, sectionConfig, sectionTableM)
import Relude

runRegistryAudit :: Bool -> ConfigT ()
runRegistryAudit regFix = do
  sectionTableM 0 "update dependencies" [("mode", pure $ chalk Cyan (if regFix then "auto-fix" else "check"))]
  originalRegistry <- asks (registry . config)
  range <- getTestedRange

  let dependencyAudits = filter (auditHasAny (/= Valid)) $ mapWithName (auditBounds range) originalRegistry
  section "audit" $ printGenTable $ formatAudit <$> dependencyAudits
  let errorCount = length $ filter (auditHasAny (== Conflict)) dependencyAudits

  if null dependencyAudits
    then do
      indent 1 $ putLine "all dependencies are up to date."
    else do
      if regFix
        then ((\cf -> pure $ cf {registry = mapDeps (updateDepBounds regFix range) originalRegistry}) `updateConfig`) $ do
          sectionConfig 0 [("hwm.yaml", pure $ chalk Green "✓")]
          syncPackages
        else do
          injectIssue
            ( Issue
                { issueDetails = Nothing,
                  issueMessage = "Found " <> show (length dependencyAudits - errorCount) <> " outdated dependencies: Run 'hwm registry audit --fix' to update.",
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