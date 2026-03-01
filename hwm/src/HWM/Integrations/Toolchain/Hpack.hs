{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Hpack (HpackPackage (..), emptyPackage) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import HWM.Core.Pkg (IsPkg (..), PkgName (..))
import HWM.Core.Version (Version)
import HWM.Domain.Dependencies (HasDependencies (..), Dependencies)
import HWM.Integrations.Toolchain.Lib
  ( HasSourceDirs (..),
    Libraries,
    Library (..),
    MapDeps (..),
  )
import HWM.Runtime.Files (aesonYAMLOptionsAdvanced)
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
  setVersion pkg version = pkg {hpackVersion = version}

instance HasSourceDirs HpackPackage where
  getSourceDirs _ HpackPackage {..} =
    getSourceDirs ("lib", []) hpackLibrary
      <> getSourceDirs ("test", []) hpackTests
      <> getSourceDirs ("exe", []) hpackExecutables
      <> getSourceDirs ("bench", []) hpackBenchmarks

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
