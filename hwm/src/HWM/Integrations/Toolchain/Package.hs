{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Package
  ( BoundsDiff,
    syncPackages,
    validatePackage,
    addPkgDependency,
    newPackage,
    deriveDependencyGraph,
  )
where

import Control.Monad.Except (MonadError (..))
import HWM.Core.Formatting (Format (..), Status (Checked), displayStatus)
import HWM.Core.Pkg (IsPkg (..), Pkg (..), PkgName (PkgName), pkgMemberId)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Domain.Config (getRule)
import HWM.Domain.ConfigT (ConfigT, Env (config, pkgs), askVersion)
import HWM.Domain.Dependencies (Dependencies, Dependency (Dependency), DependencyMap (..), HasDependencies (..), buildDependencyGraph, singleDeps, toDependencyList)
import qualified HWM.Domain.Dependencies as M
import HWM.Domain.Workspace (allPackages, forWorkspaceCore)
import HWM.Integrations.Toolchain.Cabal (readCabalPackage, syncCabalPackage)
import HWM.Integrations.Toolchain.Hpack (HpackPackage, emptyPackage, readHpackPackage)
import HWM.Integrations.Toolchain.Lib
  ( BoundsDiff,
    MapDeps (..),
    getBoundsDiffs,
    updateDependencies,
  )
import HWM.Runtime.Files (rewrite_, statusM)
import Relude
import System.FilePath ((</>))

newPackage :: FilePath -> PkgName -> ConfigT ()
newPackage targetDir name = do
  cfg <- asks config
  ps <- asks pkgs
  let baseName = PkgName "base"
  version <- askVersion
  base <- getRule baseName ps cfg
  let package = emptyPackage name version (M.singleDeps (Dependency baseName base))
  rewrite_ (targetDir </> "package.yaml") (const $ pure package)

packageDiffs :: (HasDependencies a) => Pkg -> a -> ConfigT [BoundsDiff]
packageDiffs pkg package = concat <$> traverse (getBoundsDiffs pkg) (collectDependencies [] package)

syncPackages :: ConfigT ()
syncPackages = forWorkspaceCore $ \pkg -> updatePackage (mapPackage pkg) pkg

mapPackage :: (MapDeps a, IsPkg a) => Pkg -> a -> ConfigT a
mapPackage pkg package = do
  result <- mapDeps (pkg, []) updateDependencies package
  (`setVersion` result) <$> askVersion

packageModifyDependencies :: (MapDeps a) => (Dependencies -> ConfigT Dependencies) -> Pkg -> a -> ConfigT a
packageModifyDependencies f pkg = mapDeps (pkg, []) onlyMain
  where
    onlyMain (_, ["dependencies"]) deps = f deps
    onlyMain _ deps = pure deps

addPkgDependency :: Dependency -> Pkg -> ConfigT Text
addPkgDependency dependency pkg = updatePackage (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency)) pkg) pkg

updateHpackFile :: (MonadIO m, MonadError Issue m) => (HpackPackage -> m HpackPackage) -> Pkg -> m Status
updateHpackFile f pkg = do
  maybe (pure Checked) (\path -> statusM path (rewrite_ path maybePackage)) (hpackFile pkg)
  where
    maybePackage Nothing =
      throwError
        $ Issue
          { issueTopic = pkgMemberId pkg,
            issueMessage = "could not find package file",
            issueSeverity = SeverityWarning,
            issueDetails = Just GenericIssue {issueFile = fromMaybe (cabalFile pkg) (hpackFile pkg)}
          }
    maybePackage (Just package) = f package

updatePackage :: (HpackPackage -> ConfigT HpackPackage) -> Pkg -> ConfigT Text
updatePackage f pkg = do
  package <- updateHpackFile f pkg
  cabal <- syncCabalPackage pkg
  pure $ displayStatus [("pkg", package), ("cabal", cabal)]

deriveDependencyGraph :: ConfigT DependencyMap
deriveDependencyGraph = buildDependencyGraph (concatMap (toDependencyList . snd) . libDependencies) <$> (allPackages >>= traverse readCabalPackage)
  where
    libDependencies = filter (\x -> fst x == ["library"]) . collectDependencies []

validatePackage :: Pkg -> ConfigT ()
validatePackage pkg = do
  currentPkg <- readHpackPackage pkg
  expectedVersion <- askVersion
  let currentVersion = getPkgVersion currentPkg
      versionMatch = currentVersion == expectedVersion
  diffs <- packageDiffs pkg currentPkg
  unless versionMatch
    $ injectIssue
      Issue
        { issueTopic = pkgMemberId pkg,
          issueMessage = "version mismatch: " <> format currentVersion <> " → " <> format expectedVersion,
          issueSeverity = SeverityWarning,
          issueDetails = Just GenericIssue {issueFile = fromMaybe (cabalFile pkg) (hpackFile pkg)}
        }
  unless (null diffs)
    $ injectIssue
      Issue
        { issueTopic = pkgMemberId pkg,
          issueMessage =
            let baseMsg =
                  if versionMatch
                    then "package out of sync (run 'hwm sync' to fix)"
                    else "package configuration diverged from expected (run 'hwm sync')"
                diffCount = length diffs
                countSuffix = if diffCount > 0 then " (" <> show diffCount <> " dependencies differ)" else ""
             in baseMsg <> countSuffix,
          issueSeverity = SeverityWarning,
          issueDetails =
            Just
              DependencyIssue
                { issueDependencies = map (\(scope, depName, actual, expected) -> (scope, format depName, format actual, format expected)) diffs,
                  issueFile = fromMaybe (cabalFile pkg) (hpackFile pkg)
                }
        }
