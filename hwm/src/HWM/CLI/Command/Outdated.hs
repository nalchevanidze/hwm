{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Outdated
  ( runOutdated,
    OutdatedOptions (..),
  )
where

import Data.Foldable (Foldable (minimum))
import HWM.Core.Formatting (Color (..), chalk)
import HWM.Core.Result (Issue (..), MonadIssue (..), Severity (..))
import HWM.Domain.Bounds (BoundCompliance (..), auditBounds, auditHasAny, formatAudit, updateDepBounds)
import HWM.Domain.Config (Config (registry))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Domain.Dependencies (mapDeps, mapWithName)
import HWM.Domain.Matrix (BuildEnvironment (..), getBuildEnvroments)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.Cache (getLatestNightlySnapshot, getSnapshot)
import HWM.Runtime.UI (indent, printGenTable, putLine, section, sectionConfig, sectionTableM)
import Relude hiding (maxBound, minBound)

data OutdatedOptions = OutdatedOptions
  { autoFix :: Bool,
    forceAutofix :: Bool
  }
  deriving (Show)

runOutdated :: OutdatedOptions -> ConfigT ()
runOutdated OutdatedOptions {..} = do
  sectionTableM 0 "update dependencies" [("mode", pure $ chalk Cyan (if autoFix then "auto-fix" else "check"))]
  originalRegistry <- asks (registry . config)
  env <- getBuildEnvroments
  legacy <- getSnapshot (minimum $ map buildResolver env)
  nightly <- getLatestNightlySnapshot

  let dependencyAudits = filter (auditHasAny (/= Valid)) $ mapWithName (auditBounds legacy nightly) originalRegistry

  section "audit" $ printGenTable $ formatAudit <$> dependencyAudits

  let errorCount = length $ filter (auditHasAny (== Conflict)) dependencyAudits

  if null dependencyAudits
    then do
      indent 1 $ putLine "all dependencies are up to date."
    else do
      if autoFix
        then ((\cf -> pure $ cf {registry = mapDeps (updateDepBounds forceAutofix legacy nightly) originalRegistry}) `updateConfig`) $ do
          sectionConfig 0 [("hwm.yaml", pure $ chalk Green "✓")]
          syncPackages
        else do
          injectIssue
            ( Issue
                { issueDetails = Nothing,
                  issueMessage = "Found " <> show (length dependencyAudits - errorCount) <> " outdated dependencies: Run 'hwm outdated --fix --force' to update.",
                  issueTopic = "registry",
                  issueSeverity = SeverityWarning
                }
            )
          when (errorCount > 0)
            $ injectIssue
              ( Issue
                  { issueDetails = Nothing,
                    issueMessage = "Found " <> show errorCount <> " outdated dependencies: Run 'hwm outdated --fix' to update.",
                    issueTopic = "registry",
                    issueSeverity = SeverityError
                  }
              )
