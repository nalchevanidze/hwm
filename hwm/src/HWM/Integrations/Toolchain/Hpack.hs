{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Hpack
  ( HpackPackage (..),
    readHpackPackage,
    newHpackPackage,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON (..), GFromJSON, ToJSON (..), Value (..), genericParseJSON, genericToJSON, withObject)
import Data.Aeson.KeyMap (delete)
import Data.Aeson.Types (Object, Zero)
import Data.Yaml.Aeson (Parser)
import GHC.Generics (Generic (..))
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Status (Checked))
import HWM.Core.Pkg (IsPkg (..), ModifyPackage (..), Pkg (..), PkgName (..))
import HWM.Core.Result (Issue (..), IssueDetails (..), Severity (..))
import HWM.Core.Version (Version)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Dependencies (Dependencies, HasDependencies (..), MapDeps (..))
import HWM.Runtime.Files (aesonYAMLOptions, aesonYAMLOptionsAdvanced, readYaml, rewriteMaybe_, rewrite_, statusM)
import Hpack ()
import Relude
import System.FilePath ((</>))

type Libraries = Map Name Library

data Library = Library
  { sourceDirs :: Name,
    dependencies :: Maybe Dependencies,
    __unknownFields :: Maybe Object
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Library where
  parseJSON = fromObject (\t o -> t {__unknownFields = o})

instance ToJSON Library where
  toJSON t = Object (toObject (genericToJSON aesonYAMLOptions t) <> fromMaybe mempty (__unknownFields t))

instance MapDeps Library where
  mapDeps ctx f Library {..} = do
    newDependencies <- traverse (f ctx) dependencies
    pure $ Library {dependencies = newDependencies, ..}

fromObject :: (Generic a, GFromJSON Zero (Rep a)) => (a -> Maybe Object -> a) -> Value -> Parser a
fromObject f v = do
  t <- genericParseJSON aesonYAMLOptions v
  o <- withObject "Lib" pure v
  pure (f t (Just o))

toObject :: Value -> Object
toObject (Object x) = delete "__unknown-fields" x
toObject _ = mempty

instance HasDependencies Library where
  collectDependencies scope (Library {..}) = collectDependencies scope dependencies

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
            issueDetails = Just GenericIssue {issueFile = cabalFile pkg}
          }
    )
    readYaml
    (hpackFile pkg)

rewriteHpackPackage :: (MonadIO m, MonadError Issue m) => (HpackPackage -> m (Maybe HpackPackage)) -> Pkg -> m Status
rewriteHpackPackage f pkg = do
  maybe (pure Checked) (\path -> statusM path (rewriteMaybe_ path maybePackage)) (hpackFile pkg)
  where
    maybePackage Nothing =
      throwError
        $ Issue
          { issueTopic = pkgMemberId pkg,
            issueMessage = "could not find package file",
            issueSeverity = SeverityWarning,
            issueDetails = Just GenericIssue {issueFile = cabalFile pkg}
          }
    maybePackage (Just package) = f package

newHpackPackage :: (MonadError Issue m, MonadIO m) => FilePath -> PkgName -> Version -> Dependencies -> m Status
newHpackPackage dir name version deps = do
  let package = emptyPackage name version deps
  rewrite_ (dir </> "package.yaml") (const $ pure package)

instance ModifyPackage HpackPackage ConfigT where
  rewrite = rewriteHpackPackage