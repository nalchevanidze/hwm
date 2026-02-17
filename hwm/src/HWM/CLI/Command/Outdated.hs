{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Outdated (runOutdated) where

import Data.Foldable (Foldable (maximum, minimum))
import HWM.Core.Formatting (Color (..), Format (..), chalk)
import HWM.Core.Result (Issue (..), MonadIssue (..), Severity (SeverityWarning))
import HWM.Domain.Bounds (BoundsAudit (..), auditBounds, formatAudit, updateDepBounds)
import HWM.Domain.Config (Config (registry))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Domain.Dependencies (mapDeps, traverseDeps)
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

  section "audit" $ pure ()

  audits <- mapDeps (auditBounds legacy bleedingEdge) originalRegistry

  if null audits
    then do
      indent 1 $ putLine "all dependencies are up to date."
    else do
      indent 1 $ do
        printGenTable
          $ (\a -> [format $ auditPkgName a] <> formatAudit (minBound a) <> [chalk Dim "  &&  "] <> formatAudit (maxBound a))
          <$> audits 

      registry' <- traverseDeps updateDepBounds originalRegistry

      if autoFix
        then ((\cf -> pure $ cf {registry = registry'}) `updateConfig`) $ do
          sectionConfig 0 [("hwm.yaml", pure $ chalk Green "✓")]
          syncPackages
        else
          injectIssue
            ( Issue
                { issueDetails = Nothing,
                  issueMessage = "Found " <> show (length audits) <> " outdated dependencies: Run 'hwm outdated --fix' to update.",
                  issueTopic = "registry",
                  issueSeverity = SeverityWarning
                }
            )
