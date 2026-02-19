{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Package
  ( Package (..),
    BoundsDiff,
    syncPackages,
    deriveRegistry,
    packageDiffs,
    validatePackage,
    updatePackage,
    packageModifyDependencies,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HWM.Core.Formatting (Format (..), displayStatus)
import HWM.Core.Pkg (Pkg (..), PkgName, pkgMemberId, pkgYamlPath)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Core.Version (Version)
import HWM.Domain.ConfigT (ConfigT, askVersion)
import HWM.Domain.Dependencies (Dependencies, Dependency (Dependency), DependencyGraph (DependencyGraph), externalRegistry, normalizeDependencies, toDependencyList)
import HWM.Domain.Workspace (forWorkspaceCore)
import HWM.Integrations.Toolchain.Cabal (syncCabal)
import HWM.Integrations.Toolchain.Lib
  ( BoundsDiff,
    Libraries,
    Library (..),
    checkDependencies,
    checkLibraries,
    checkLibrary,
    updateDependencies,
    updateLibraries,
    updateLibrary,
  )
import HWM.Runtime.Files (aesonYAMLOptions, readYaml, rewrite_, statusM)
import Relude

data Package = Package
  { name :: PkgName,
    version :: Version,
    library :: Maybe Library,
    dependencies :: Dependencies,
    tests :: Maybe Libraries,
    executables :: Maybe Libraries,
    benchmarks :: Maybe Libraries,
    internalLibraries :: Maybe Libraries,
    foreignLibraries :: Maybe Libraries
  }
  deriving (Show, Generic)

instance FromJSON Package where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Package where
  toJSON = genericToJSON aesonYAMLOptions

mapPackage :: Pkg -> Package -> ConfigT Package
mapPackage pkg Package {..} = do
  newLibrary <- traverse (updateLibrary pkg "library") library
  newTests <- updateLibraries pkg "tests" tests
  newExecutables <- updateLibraries pkg "executables" executables
  newBenchmarks <- updateLibraries pkg "benchmarks" benchmarks
  newInternalLibraries <- updateLibraries pkg "internal" internalLibraries
  newForeignLibraries <- updateLibraries pkg "foreign" foreignLibraries
  newDependencies <- updateDependencies pkg "dependencies" dependencies
  newVersion <- askVersion
  pure
    $ Package
      { version = newVersion,
        library = newLibrary,
        tests = newTests,
        executables = newExecutables,
        benchmarks = newBenchmarks,
        internalLibraries = newInternalLibraries,
        foreignLibraries = newForeignLibraries,
        dependencies = newDependencies,
        ..
      }

withMaybe :: (Applicative m) => (a -> m [b]) -> Maybe a -> m [b]
withMaybe = maybe (pure [])

-- | Determine whether a package already matches the expected configuration.
packageDiffs :: Pkg -> Package -> ConfigT [BoundsDiff]
packageDiffs pkg Package {..} = do
  depsDiffs <- checkDependencies pkg "dependencies" dependencies
  libraryDiffs <- withMaybe (checkLibrary pkg "library") library
  testsDiffs <- withMaybe (checkLibraries pkg "tests") tests
  executablesDiffs <- withMaybe (checkLibraries pkg "executables") executables
  benchmarksDiffs <- withMaybe (checkLibraries pkg "benchmarks") benchmarks
  internalDiffs <- withMaybe (checkLibraries pkg "internal") internalLibraries
  foreignDiffs <- withMaybe (checkLibraries pkg "foreign") foreignLibraries
  pure
    ( depsDiffs
        <> libraryDiffs
        <> testsDiffs
        <> executablesDiffs
        <> benchmarksDiffs
        <> internalDiffs
        <> foreignDiffs
    )

syncPackages :: ConfigT ()
syncPackages = forWorkspaceCore $ \pkg -> updatePackage (mapPackage pkg) pkg

packageModifyDependencies :: (Dependencies -> ConfigT Dependencies) -> Package -> ConfigT Package
packageModifyDependencies f Package {..} = do
  newDependencies <- f dependencies
  pure Package {dependencies = newDependencies, ..}

updatePackage :: (Package -> ConfigT Package) -> Pkg -> ConfigT Text
updatePackage f pkg = do
  let path = pkgYamlPath pkg
  package <- statusM path (rewrite_ path maybePackage)
  cabal <- syncCabal pkg
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

collectPackageDependencies :: Package -> [Dependency]
collectPackageDependencies Package {..} =
  normalizeDependencies
    ( toDependencyList dependencies
        <> collectLibrary library
        <> collectLibraries tests
        <> collectLibraries executables
        <> collectLibraries benchmarks
        <> collectLibraries internalLibraries
        <> collectLibraries foreignLibraries
    )
  where
    collectLibrary :: Maybe Library -> [Dependency]
    collectLibrary Nothing = []
    collectLibrary (Just Library {dependencies = Nothing}) = []
    collectLibrary (Just Library {dependencies = Just deps}) = toDependencyList deps

    collectLibraries :: Maybe Libraries -> [Dependency]
    collectLibraries Nothing = []
    collectLibraries (Just libs) = concatMap (collectLibrary . Just) (Map.elems libs)

deriveRegistry :: (Monad m, MonadError Issue m, MonadIO m) => [Pkg] -> m (Dependencies, DependencyGraph)
deriveRegistry pkgs = do
  packages <- traverse (readYaml . pkgYamlPath) pkgs
  let graph = deriveDependencyGraph packages
  let deps = externalRegistry (map pkgName pkgs) $ concatMap collectPackageDependencies packages
  pure (deps, graph)

collectCriticalDependencies :: Package -> [Dependency]
collectCriticalDependencies Package {..} = normalizeDependencies (toDependencyList dependencies <> collectLibrary library)
  where
    collectLibrary :: Maybe Library -> [Dependency]
    collectLibrary Nothing = []
    collectLibrary (Just Library {dependencies = Nothing}) = []
    collectLibrary (Just Library {dependencies = Just deps}) = toDependencyList deps

deriveDependencyGraph :: [Package] -> DependencyGraph
deriveDependencyGraph pkgs = DependencyGraph $ Map.fromList [(name pkg, internalDeps pkg) | pkg <- pkgs]
  where
    internalNames = Set.fromList (map name pkgs)
    internalDeps pkg = mapMaybe selectInternal (collectCriticalDependencies pkg)
    selectInternal (Dependency depName _) =
      if Set.member depName internalNames then Just depName else Nothing

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
