{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Hpack
  ( HpackPackage (..),
    emptyPackage,
    readHpackPackage,
    rewriteHpackFile,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import HWM.Core.Formatting (Status (Checked))
import HWM.Core.Pkg (IsPkg (..), Pkg (..), PkgName (..))
import HWM.Core.Result (Issue (..), IssueDetails (..), Severity (..))
import HWM.Core.Version (Version)
import HWM.Domain.Dependencies (Dependencies, HasDependencies (..))
import HWM.Integrations.Toolchain.Lib
  ( Libraries,
    Library (..),
    MapDeps (..),
  )
import HWM.Runtime.Files (aesonYAMLOptionsAdvanced, readYaml, rewrite_, statusM)
import Hpack ()
import Relude

data HpackPackage = HpackPackage
  { hpackName :: PkgName,
    hpackVersion :: Version,
    hpackLibrary :: Maybe Library,
    hpackDependencies :: Maybe Dependencies,
    hpackTests :: Maybe Libraries,
    hpackExecutables :: Maybe Libraries,
    hpackBenchmarks :: Maybe Libraries,
    hpackInternalLibraries :: Maybe Libraries,
    hpackForeignLibraries :: Maybe Libraries
  }
  deriving (Show, Generic)

instance IsPkg HpackPackage where
  getPkgName = hpackName
  getPkgVersion = hpackVersion
  setVersion version pkg = pkg {hpackVersion = version}

instance FromJSON HpackPackage where
  parseJSON = genericParseJSON (aesonYAMLOptionsAdvanced "hpack")

instance ToJSON HpackPackage where
  toJSON = genericToJSON (aesonYAMLOptionsAdvanced "hpack")

instance MapDeps HpackPackage where
  mapDeps (pkg, p) f HpackPackage {..} = do
    newDependencies <- mapDeps (pkg, p <> ["dependencies"]) f hpackDependencies
    newLibrary <- mapDeps (pkg, p <> ["library"]) f hpackLibrary
    newTests <- mapDeps (pkg, p <> ["tests"]) f hpackTests
    newExecutables <- mapDeps (pkg, p <> ["executables"]) f hpackExecutables
    newBenchmarks <- mapDeps (pkg, p <> ["benchmarks"]) f hpackBenchmarks
    newInternalLibraries <- mapDeps (pkg, p <> ["internal"]) f hpackInternalLibraries
    newForeignLibraries <- mapDeps (pkg, p <> ["foreign"]) f hpackForeignLibraries
    pure
      $ HpackPackage
        { hpackLibrary = newLibrary,
          hpackTests = newTests,
          hpackExecutables = newExecutables,
          hpackBenchmarks = newBenchmarks,
          hpackInternalLibraries = newInternalLibraries,
          hpackForeignLibraries = newForeignLibraries,
          hpackDependencies = newDependencies,
          ..
        }

instance HasDependencies HpackPackage where
  collectDependencies xs HpackPackage {..} =
    concat
      [ collectDependencies (xs <> ["dependencies"]) hpackDependencies,
        collectDependencies (xs <> ["library"]) hpackLibrary,
        collectDependencies (xs <> ["tests"]) hpackTests,
        collectDependencies (xs <> ["executables"]) hpackExecutables,
        collectDependencies (xs <> ["benchmarks"]) hpackBenchmarks,
        collectDependencies (xs <> ["internal"]) hpackInternalLibraries,
        collectDependencies (xs <> ["foreign"]) hpackForeignLibraries
      ]

emptyPackage :: PkgName -> Version -> Dependencies -> HpackPackage
emptyPackage name version dependencies =
  HpackPackage
    { hpackName = name,
      hpackVersion = version,
      hpackLibrary = Just Library {sourceDirs = "src", dependencies = Just dependencies, __unknownFields = Nothing},
      hpackDependencies = Nothing,
      hpackTests = Nothing,
      hpackExecutables = Nothing,
      hpackBenchmarks = Nothing,
      hpackInternalLibraries = Nothing,
      hpackForeignLibraries = Nothing
    }

readHpackPackage :: (Monad m, MonadError Issue m, MonadIO m) => Pkg -> m HpackPackage
readHpackPackage pkg =
  maybe
    ( throwError
        $ Issue
          { issueTopic = pkgMemberId pkg,
            issueMessage = "pkg does not support hpack or could not find package file",
            issueSeverity = SeverityWarning,
            issueDetails = Just GenericIssue {issueFile = fromMaybe (cabalFile pkg) (hpackFile pkg)}
          }
    )
    readYaml
    (hpackFile pkg)

rewriteHpackFile :: (MonadIO m, MonadError Issue m) => (HpackPackage -> m HpackPackage) -> Pkg -> m Status
rewriteHpackFile f pkg = do
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