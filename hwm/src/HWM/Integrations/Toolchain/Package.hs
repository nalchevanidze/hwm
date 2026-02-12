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
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HWM.Core.Formatting (Color (..), Format (..), chalk, displayStatus, genMaxLen, padDots, subPathSign)
import HWM.Core.Pkg (Pkg (..), PkgName, pkgMemberId, pkgYamlPath)
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Core.Version (Version)
import HWM.Domain.ConfigT (ConfigT, askVersion, askWorkspaceGroups)
import HWM.Domain.Dependencies (Dependencies, Dependency (Dependency), DependencyGraph (DependencyGraph), externalRegistry, normalizeDependencies, toDependencyList)
import HWM.Domain.Workspace (memberPkgs, pkgGroupName)
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
import HWM.Runtime.UI (putLine, sectionWorkspace)
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

updatePackage :: Pkg -> Maybe Package -> ConfigT Package
updatePackage pkg Nothing =
  throwError
    $ Issue
      { issueTopic = pkgMemberId pkg,
        issueMessage = "could not find package file",
        issueSeverity = SeverityWarning,
        issueDetails = Just GenericIssue {issueFile = pkgYamlPath pkg}
      }
updatePackage pkg (Just Package {..}) = do
  let path = pkgYamlPath pkg
      pkgId = pkgMemberId pkg
  newLibrary <- traverse (updateLibrary pkgId "library" path) library
  newTests <- updateLibraries pkgId "tests" path tests
  newExecutables <- updateLibraries pkgId "executables" path executables
  newBenchmarks <- updateLibraries pkgId "benchmarks" path benchmarks
  newInternalLibraries <- updateLibraries pkgId "internal" path internalLibraries
  newForeignLibraries <- updateLibraries pkgId "foreign" path foreignLibraries
  newDependencies <- updateDependencies pkgId "dependencies" path dependencies
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

-- | Determine whether a package already matches the expected configuration.
packageDiffs :: Text -> FilePath -> Package -> ConfigT [BoundsDiff]
packageDiffs memberId path Package {..} = do
  depsDiffs <- checkDependencies memberId "dependencies" path dependencies
  libraryDiffs <- traverseLibrary "library" library
  testsDiffs <- traverseLibraries "tests" tests
  executablesDiffs <- traverseLibraries "executables" executables
  benchmarksDiffs <- traverseLibraries "benchmarks" benchmarks
  internalDiffs <- traverseLibraries "internal" internalLibraries
  foreignDiffs <- traverseLibraries "foreign" foreignLibraries
  pure
    ( depsDiffs
        <> libraryDiffs
        <> testsDiffs
        <> executablesDiffs
        <> benchmarksDiffs
        <> internalDiffs
        <> foreignDiffs
    )
  where
    traverseLibrary :: Text -> Maybe Library -> ConfigT [BoundsDiff]
    traverseLibrary _ Nothing = pure []
    traverseLibrary scope (Just lib) = checkLibrary memberId scope path lib

    traverseLibraries :: Text -> Maybe Libraries -> ConfigT [BoundsDiff]
    traverseLibraries _ Nothing = pure []
    traverseLibraries scope (Just libs) = checkLibraries memberId scope path libs

syncPackages :: ConfigT ()
syncPackages = sectionWorkspace $ do
  groups <- askWorkspaceGroups
  for_ groups $ \g -> do
    putLine ""
    putLine $ "• " <> chalk Bold (pkgGroupName g)
    dirs <- memberPkgs g
    let maxLen = genMaxLen (map pkgMemberId dirs)
    for_ dirs $ \pkg -> do
      let path = pkgYamlPath pkg
      package <- statusM path (rewrite_ path (updatePackage pkg))
      cabal <- syncCabal pkg
      putLine
        ( subPathSign
            <> padDots maxLen (pkgMemberId pkg)
            <> displayStatus [("pkg", package), ("cabal", cabal)]
        )

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
      pkgId = pkgMemberId pkg

  currentPkg <- readYaml path :: ConfigT Package
  expectedVersion <- askVersion

  let currentVersion = version currentPkg
      versionMatch = currentVersion == expectedVersion
  diffs <- packageDiffs pkgId path currentPkg

  unless versionMatch
    $ injectIssue
      Issue
        { issueTopic = pkgId,
          issueMessage = "version mismatch: " <> format currentVersion <> " → " <> format expectedVersion,
          issueSeverity = SeverityWarning,
          issueDetails = Just GenericIssue {issueFile = path}
        }

  unless (null diffs)
    $ injectIssue
      Issue
        { issueTopic = pkgId,
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
