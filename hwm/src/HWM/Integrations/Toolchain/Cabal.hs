{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Cabal
  ( syncCabalPackage,
    validateHackage,
    syncCabalProject,
    readCabalPackage,
    HasSourceDirs (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.ByteString as BS
import Data.Foldable (Foldable (..))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Distribution.PackageDescription (Benchmark (..), Executable (..), GenericPackageDescription (..), TestSuite (..), UnqualComponentName, packageDescription)
import Distribution.PackageDescription.Check (PackageCheck (..), checkPackage)
import Distribution.PackageDescription.Parsec
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Library (Library (..))
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity (normal)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), Status (..))
import HWM.Core.Options (Options (..))
import HWM.Core.Pkg (IsPkg (..), Pkg (Pkg, hpackFile))
import qualified HWM.Core.Pkg as P
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Domain.ConfigT (ConfigT)
import qualified HWM.Domain.ConfigT as CT
import HWM.Domain.Dependencies (HasDependencies (..))
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment)
import HWM.Runtime.Files (remove)
import Hpack (Result (..), defaultOptions, hpackResult, setProgramName, setTarget)
import qualified Hpack as H
import Hpack.Config (ProgramName (..))
import Relude
import System.FilePath (takeDirectory)

-- | Translate Cabal warnings into formatting status for downstream reporting.
toStatus :: PackageCheck -> Status
toStatus p
  | isError p = Invalid
  | otherwise = Warning

isError :: PackageCheck -> Bool
isError PackageDistInexcusable {} = True
isError PackageBuildImpossible {} = True
isError PackageBuildWarning {} = False
isError PackageDistSuspiciousWarn {} = False
isError PackageDistSuspicious {} = False

validateHackage :: Pkg -> FilePath -> ConfigT [Status]
validateHackage pkg path = do
  gpd <- liftIO $ readGenericPackageDescription normal path
  let ls = checkPackage gpd Nothing
  for_ ls $ \l -> do
    injectIssue
      ( Issue
          { issueMessage = "Invalid package: " <> show l,
            issueSeverity = if isError l then SeverityError else SeverityWarning,
            issueTopic = P.pkgMemberId pkg,
            issueDetails = Just GenericIssue {issueFile = path}
          }
      )
  pure (map toStatus ls)

hpackSync :: Pkg -> ConfigT Status
hpackSync Pkg {hpackFile = Nothing} = pure Checked
hpackSync pkg@Pkg {hpackFile = Just path} = do
  remove (P.cabalFile pkg)
  let programName = ProgramName $ toString $ P.pkgName pkg
  let ops = setTarget path $ setProgramName programName defaultOptions
  Result {..} <- liftIO $ hpackResult ops
  case resultStatus of
    H.OutputUnchanged -> pure Checked
    _ -> pure Updated

syncCabalPackage :: Pkg -> ConfigT Status
syncCabalPackage pkg = do
  s <- hpackSync pkg
  ls <- validateHackage pkg (P.cabalFile pkg)
  pure $ maximum (s : ls)

generateCabalProject :: [Pkg] -> Text -> Text
generateCabalProject packagePaths ghcVersion =
  T.unlines
    [ "with-compiler: ghc-" <> ghcVersion,
      "packages:\n" <> T.unlines (map (("  " <>) . format . P.pkgDirPath) packagePaths)
    ]

syncCabalProject :: ConfigT ()
syncCabalProject = do
  ops <- asks CT.options
  BuildEnvironment {..} <- getBuildEnvironment Nothing
  liftIO $ TIO.writeFile (optionsCabal ops) (generateCabalProject buildPkgs (toText buildGHC))

data CabalPackage = CabalPackage
  { cbDirectory :: FilePath,
    cbOriginal :: GenericPackageDescription
  }
  deriving (Show)

readCabalFile :: (MonadIO m, MonadError Issue m) => Pkg -> m GenericPackageDescription
readCabalFile pkg = do
  let path = P.cabalFile pkg
  content <- liftIO $ BS.readFile path
  case runParseResult (parseGenericPackageDescription content) of
    (_, Right gpd) -> pure gpd
    (_, Left (_, errors)) ->
      throwError $ fromString $ "Cabal parsing failed: " ++ show errors

readCabalPackage :: (MonadIO m, MonadError Issue m) => Pkg -> m CabalPackage
readCabalPackage pkg = do
  gpd <- readCabalFile pkg
  pure
    CabalPackage
      { cbDirectory = takeDirectory (P.cabalFile pkg),
        cbOriginal = gpd
      }

class HasSourceDirs a where
  getSourceDirs :: [Text] -> a -> [(Text, Name)]

instance (HasSourceDirs a) => HasSourceDirs (Maybe a) where
  getSourceDirs tag (Just l) = getSourceDirs tag l
  getSourceDirs _ Nothing = []

instance (HasSourceDirs a) => HasSourceDirs (Map Text a) where
  getSourceDirs tags libs = concatMap (\(name, lib) -> getSourceDirs (tags <> [name]) lib) (Map.toList libs)

instance HasSourceDirs CabalPackage where
  getSourceDirs p CabalPackage {..} = getSourceDirs p cbOriginal

instance HasSourceDirs GenericPackageDescription where
  getSourceDirs p GenericPackageDescription {..} =
    getSourceDirs (p <> ["lib"]) condLibrary
      <> getSourceDirs (p <> ["exe"]) condExecutables
      <> getSourceDirs (p <> ["test"]) condTestSuites
      <> getSourceDirs (p <> ["bench"]) condBenchmarks

instance (HasSourceDirs a) => HasSourceDirs (CondTree v c a) where
  getSourceDirs path condTree = getSourceDirs path (condTreeData condTree)

instance (HasSourceDirs a) => HasSourceDirs [(UnqualComponentName, a)] where
  getSourceDirs path = concatMap (\(name, info) -> getSourceDirs (path <> [format name]) info)

instance HasSourceDirs Library where
  getSourceDirs path Library {..} = getSourceDirs path libBuildInfo

instance HasSourceDirs Executable where
  getSourceDirs path Executable {..} = getSourceDirs path buildInfo

instance HasSourceDirs TestSuite where
  getSourceDirs path TestSuite {..} = getSourceDirs path testBuildInfo

instance HasSourceDirs Benchmark where
  getSourceDirs path Benchmark {..} = getSourceDirs path benchmarkBuildInfo

instance HasSourceDirs BuildInfo where
  getSourceDirs path buildInfo = map (withKey . getSymbolicPath) (hsSourceDirs buildInfo)
    where
      withKey dir = (T.intercalate ":" path, format dir)

instance IsPkg CabalPackage where
  getPkgName = getPkgName . cbOriginal
  getPkgVersion = getPkgVersion . cbOriginal
  setVersion version pkg = pkg {cbOriginal = setVersion version (cbOriginal pkg)}

instance HasDependencies CabalPackage where
  collectDependencies xs gpd = collectDependencies xs (cbOriginal gpd)
