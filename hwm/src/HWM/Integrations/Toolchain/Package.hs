{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Package
  ( BoundsDiff,
    syncPackages,
    validatePackage,
    addPkgDependency,
    newPackage,
    deriveDependencyGraph,
    packageLibs,
    resolvePackages,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.Map as Map
import HWM.Core.Formatting (Format (..), displayStatus)
import HWM.Core.Pkg (IsPkg (..), Pkg (..), PkgName (PkgName), pkgMemberId)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Core.Version (Version)
import HWM.Domain.Config (getRule)
import HWM.Domain.ConfigT (ConfigT, Env (config, pkgs), askVersion)
import HWM.Domain.Dependencies (Dependencies, Dependency (Dependency), DependencyMap (..), HasDependencies (..), buildDependencyGraph, singleDeps, toDependencyList)
import HWM.Domain.Workspace (allPackages, forWorkspaceCore)
import HWM.Integrations.Toolchain.Cabal (syncCabalPackage)
import HWM.Integrations.Toolchain.Lib
  ( BoundsDiff,
    LibPath,
    Libraries,
    Library (..),
    MapDeps (..),
    getBoundsDiffs,
    updateDependencies,
  )
import HWM.Runtime.Files (aesonYAMLOptions, readYaml, rewrite_, statusM)
import Relude
import System.FilePath ((</>))
import HWM.Integrations.Toolchain.Stack (pkgYamlPath)

data Package = Package
  { name :: PkgName,
    version :: Version,
    library :: Maybe Library,
    dependencies :: Maybe Dependencies,
    tests :: Maybe Libraries,
    executables :: Maybe Libraries,
    benchmarks :: Maybe Libraries,
    internalLibraries :: Maybe Libraries,
    foreignLibraries :: Maybe Libraries
  }
  deriving (Show, Generic)

instance IsPkg Package where
  getPkgName = name
  getPkgVersion = version

instance FromJSON Package where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Package where
  toJSON = genericToJSON aesonYAMLOptions

packageLibs :: Pkg -> ConfigT (Map LibPath Library)
packageLibs pkg = getLibs <$> readYaml (pkgYamlPath pkg)

getLibs :: Package -> Map LibPath Library
getLibs Package {..} =
  Map.fromList
    ( comp ("lib", []) library
        <> compGroup ("test", []) tests
        <> compGroup ("exe", []) executables
        <> compGroup ("bench", []) benchmarks
    )
  where
    comp tag (Just l) = [(tag, l)]
    comp _ _ = []
    compGroup (m, tag) = concatMap mkComp . concatMap Map.toList . maybeToList
      where
        mkComp (k, lib) = comp (m, tag <> [k]) (Just lib)

newPackage :: FilePath -> PkgName -> ConfigT ()
newPackage targetDir pkgName = do
  package <- mkPackage pkgName
  savePackage (targetDir </> "package.yaml") package

mkPackage :: PkgName -> ConfigT Package
mkPackage name = do
  cfg <- asks config
  ps <- asks pkgs
  let basename = PkgName "base"
  version <- askVersion
  base <- getRule basename ps cfg
  pure
    $ Package
      { name = name,
        version = version,
        library = Just Library {sourceDirs = "src", dependencies = Just $ singleDeps (Dependency basename base), __unknownFields = Nothing},
        dependencies = Nothing,
        tests = Nothing,
        executables = Nothing,
        benchmarks = Nothing,
        internalLibraries = Nothing,
        foreignLibraries = Nothing
      }

instance MapDeps Package where
  mapDeps (pkg, p) f Package {..} = do
    newDependencies <- mapDeps (pkg, p <> ["dependencies"]) f dependencies
    newLibrary <- mapDeps (pkg, p <> ["library"]) f library
    newTests <- mapDeps (pkg, p <> ["tests"]) f tests
    newExecutables <- mapDeps (pkg, p <> ["executables"]) f executables
    newBenchmarks <- mapDeps (pkg, p <> ["benchmarks"]) f benchmarks
    newInternalLibraries <- mapDeps (pkg, p <> ["internal"]) f internalLibraries
    newForeignLibraries <- mapDeps (pkg, p <> ["foreign"]) f foreignLibraries
    pure
      $ Package
        { library = newLibrary,
          tests = newTests,
          executables = newExecutables,
          benchmarks = newBenchmarks,
          internalLibraries = newInternalLibraries,
          foreignLibraries = newForeignLibraries,
          dependencies = newDependencies,
          ..
        }

instance HasDependencies Package where
  collectDependencies xs Package {..} =
    concat
      [ collectDependencies (xs <> ["dependencies"]) dependencies,
        collectDependencies (xs <> ["library"]) library,
        collectDependencies (xs <> ["tests"]) tests,
        collectDependencies (xs <> ["executables"]) executables,
        collectDependencies (xs <> ["benchmarks"]) benchmarks,
        collectDependencies (xs <> ["internal"]) internalLibraries,
        collectDependencies (xs <> ["foreign"]) foreignLibraries
      ]

packageDiffs :: Pkg -> Package -> ConfigT [BoundsDiff]
packageDiffs pkg package = concat <$> traverse (getBoundsDiffs pkg) (collectDependencies [] package)

syncPackages :: ConfigT ()
syncPackages = forWorkspaceCore $ \pkg -> updatePackage (mapPackage pkg) pkg

mapPackage :: Pkg -> Package -> ConfigT Package
mapPackage pkg package = do
  result <- mapDeps (pkg, []) updateDependencies package
  newVersion <- askVersion
  pure $ result {version = newVersion}

packageModifyDependencies :: (Dependencies -> ConfigT Dependencies) -> Pkg -> Package -> ConfigT Package
packageModifyDependencies f pkg = mapDeps (pkg, []) onlyMain
  where
    onlyMain (_, ["dependencies"]) deps = f deps
    onlyMain _ deps = pure deps

addPkgDependency :: Dependency -> Pkg -> ConfigT Text
addPkgDependency dependency pkg = updatePackage (packageModifyDependencies (\deps -> pure (deps <> singleDeps dependency)) pkg) pkg

updatePackage :: (Package -> ConfigT Package) -> Pkg -> ConfigT Text
updatePackage f pkg = do
  let path = pkgYamlPath pkg
  package <- statusM path (rewrite_ path maybePackage)
  cabal <- syncCabalPackage pkg
  pure $ displayStatus [("pkg", package), ("cabal", cabal)]
  where
    maybePackage Nothing =
      throwError
        $ Issue
          { issueTopic = pkgMemberId pkg,
            issueMessage = "could not find package file",
            issueSeverity = SeverityWarning,
            issueDetails = Just GenericIssue {issueFile = pkgYamlPath pkg}
          }
    maybePackage (Just package) = f package

savePackage :: FilePath -> Package -> ConfigT ()
savePackage pkg package = rewrite_ pkg (const $ pure package)

resolvePackages :: (Monad m, MonadError Issue m, MonadIO m) => [Pkg] -> m [Package]
resolvePackages = traverse (readYaml . pkgYamlPath)

libDependencies :: Package -> [Dependency]
libDependencies Package {..} = concatMap (toDependencyList . snd) $ collectDependencies ["dependencies"] dependencies <> collectDependencies ["library"] library

deriveDependencyGraph :: ConfigT DependencyMap
deriveDependencyGraph = buildDependencyGraph libDependencies <$> (allPackages >>= resolvePackages)

-- | Validate package against expected version and configuration
validatePackage :: Pkg -> ConfigT ()
validatePackage pkg = do
  let path = pkgYamlPath pkg
  currentPkg <- readYaml path :: ConfigT Package
  expectedVersion <- askVersion
  let currentVersion = version currentPkg
      versionMatch = currentVersion == expectedVersion
  diffs <- packageDiffs pkg currentPkg
  unless versionMatch
    $ injectIssue
      Issue
        { issueTopic = pkgMemberId pkg,
          issueMessage = "version mismatch: " <> format currentVersion <> " → " <> format expectedVersion,
          issueSeverity = SeverityWarning,
          issueDetails = Just GenericIssue {issueFile = path}
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
                  issueFile = path
                }
        }
