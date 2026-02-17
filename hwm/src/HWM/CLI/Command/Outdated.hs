{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Outdated (runOutdated) where

import Data.Foldable (Foldable (minimum))
import HWM.Core.Formatting (Color (..), Format (..), chalk, genMaxLen, padDots)
import HWM.Core.Result (Issue (..), MonadIssue (..), Severity (SeverityWarning))
import HWM.Domain.Bounds (printUpperBound, updateDepBounds)
import HWM.Domain.Config (Config (registry))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Domain.Dependencies (Dependency (..), toDependencyList, traverseDeps)
import HWM.Domain.Matrix (BuildEnvironment (..), getBuildEnvroments)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.Cache (clearVersions, getSnapshot)
import HWM.Runtime.UI (indent, putLine, section, sectionConfig, sectionTableM)
import Relude

runOutdated :: Bool -> ConfigT ()
runOutdated autoFix = do
  sectionTableM 0 "update dependencies" [("mode", pure $ chalk Cyan (if autoFix then "auto-fix" else "check"))]

  env <- getBuildEnvroments

  let oldest = minimum $ map buildResolver env
  sn <- getSnapshot oldest
  clearVersions
  originalRegistry <- asks (registry . config)
  section "registry" $ pure ()
  registry' <- traverseDeps (updateDepBounds sn) originalRegistry

  let updates = map snd $ filter (uncurry (/=)) (zip (toDependencyList originalRegistry) (toDependencyList registry'))
  let maxLen = genMaxLen (map (format . name) updates)

  if null updates
    then do
      indent 1 $ putLine "all dependencies are up to date."
    else do
      indent 1 $ for_ updates $ \Dependency {..} -> putLine $ padDots maxLen (format name) <> "↑ " <> printUpperBound bounds

      if autoFix
        then ((\cf -> pure $ cf {registry = registry'}) `updateConfig`) $ do
          sectionConfig 0 [("hwm.yaml", pure $ chalk Green "✓")]
          syncPackages
        else
          injectIssue
            ( Issue
                { issueDetails = Nothing,
                  issueMessage = "Found " <> show (length updates) <> " outdated dependencies: Run 'hwm outdated --fix' to update.",
                  issueTopic = "registry",
                  issueSeverity = SeverityWarning
                }
            )
