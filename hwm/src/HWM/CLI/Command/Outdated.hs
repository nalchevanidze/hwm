{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Outdated (runOutdated) where

import Data.Foldable (Foldable (maximum, minimum))
import HWM.Core.Formatting (Color (..), chalk)
import HWM.Core.Result (Issue (..), MonadIssue (..), Severity (SeverityWarning))
import HWM.Domain.Bounds (BoundCompliance (..), auditBounds, formatAudit, isAudit, updateDepBounds)
import HWM.Domain.Config (Config (registry))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Domain.Dependencies (mapDeps, mapWithName)
import HWM.Domain.Matrix (BuildEnvironment (..), getBuildEnvroments)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.Cache (getSnapshot)
import HWM.Runtime.UI (indent, printGenTable, putLine, section, sectionConfig, sectionTableM)
import Relude hiding (maxBound, minBound)

runOutdated :: Bool -> ConfigT ()
runOutdated autoFix = do
  sectionTableM 0 "update dependencies" [("mode", pure $ chalk Cyan (if autoFix then "auto-fix" else "check"))]
  originalRegistry <- asks (registry . config)
  env <- getBuildEnvroments
  legacy <- getSnapshot (minimum $ map buildResolver env)
  bleedingEdge <- getSnapshot (maximum $ map buildResolver env)
  let dependencyAudits = filter (isAudit (/= Valid)) $ mapWithName (auditBounds legacy bleedingEdge) originalRegistry

  section "audit" $ printGenTable $ formatAudit <$> dependencyAudits

  if null dependencyAudits
    then do
      indent 1 $ putLine "all dependencies are up to date."
    else do
      if autoFix
        then ((\cf -> pure $ cf {registry = mapDeps (updateDepBounds legacy bleedingEdge) originalRegistry}) `updateConfig`) $ do
          sectionConfig 0 [("hwm.yaml", pure $ chalk Green "✓")]
          syncPackages
        else
          injectIssue
            ( Issue
                { issueDetails = Nothing,
                  issueMessage = "Found " <> show (length dependencyAudits) <> " outdated dependencies: Run 'hwm outdated --fix' to update.",
                  issueTopic = "registry",
                  issueSeverity = SeverityWarning
                }
            )
