{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Cabal
  ( validateHackage,
    syncCabalProject,
    readCabalPackage,
    HasSourceDirs (..),
    CabalPackage,
    newCabalPackage,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.ByteString as BS
import Data.Foldable (Foldable (..))
import qualified Data.Map as Map
import qualified Data.Text as T
import Distribution.PackageDescription (Benchmark (..), Executable (..), GenericPackageDescription (..), PackageDescription (..), PackageIdentifier (..), TestSuite (..), UnqualComponentName, emptyBuildInfo, emptyLibrary, emptyPackageDescription, mkPackageName, packageDescription)
import Distribution.PackageDescription.Check (PackageCheck (..), checkPackage)
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Library (Library (..))
import Distribution.Utils.Path (getSymbolicPath, unsafeMakeSymbolicPath)
import Distribution.Verbosity (normal)
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), Status (..))
import HWM.Core.Options (Options (..))
import HWM.Core.Pkg (IsPkg (..), PackageIO, Pkg (Pkg, hpackFile), PkgName)
import qualified HWM.Core.Pkg as P
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Core.Version (Version, toCabalVersion)
import HWM.Domain.ConfigT (ConfigT)
import qualified HWM.Domain.ConfigT as CT
import HWM.Domain.Dependencies (Dependencies (..), HasDependencies (..), MapDeps (..), mkCabalDependency, toDependencyList)
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment)
import HWM.Runtime.Files (syncFile)
import Hpack (Result (..), defaultOptions, hpackResult, setProgramName, setTarget)
import qualified Hpack as H
import Hpack.Config (ProgramName (..))
import Relude
import System.FilePath (takeDirectory, (</>))

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
  let programName = ProgramName $ toString $ P.pkgName pkg
  let ops = setTarget path $ setProgramName programName defaultOptions
  Result {..} <- liftIO $ hpackResult ops
  case resultStatus of
    H.OutputUnchanged -> pure Checked
    _ -> pure Updated

cabalSync :: (CabalPackage -> ConfigT (Maybe CabalPackage)) -> Pkg -> ConfigT Status
cabalSync mapCabal pkg = do
  cabalP <- readCabalPackage pkg
  changes <- mapCabal cabalP
  case changes of
    Nothing -> pure Checked
    Just newpackage ->
      if cbOriginal newpackage == cbOriginal cabalP
        then pure Checked
        else do
          liftIO $ writeGenericPackageDescription (P.cabalFile pkg) (cbOriginal newpackage)
          pure Updated

instance PackageIO CabalPackage ConfigT where
  rewrite = rewriteCabalPackage

rewriteCabalPackage :: (CabalPackage -> ConfigT (Maybe CabalPackage)) -> Pkg -> ConfigT Status
rewriteCabalPackage mapCabal pkg = do
  s <- hpackSync pkg
  ls <- validateHackage pkg (P.cabalFile pkg)
  cs <- cabalSync mapCabal pkg
  pure $ maximum (s : ls <> [cs])

generateCabalProject :: [Pkg] -> Text -> Text
generateCabalProject packagePaths ghcVersion =
  T.unlines
    [ "with-compiler: ghc-" <> ghcVersion,
      "packages:\n" <> T.unlines (map (("  " <>) . format . P.pkgDirPath) packagePaths)
    ]

syncCabalProject :: ConfigT Status
syncCabalProject = do
  cabalFilePath <- asks (optionsCabal . CT.options)
  BuildEnvironment {..} <- getBuildEnvironment Nothing
  syncFile cabalFilePath (generateCabalProject buildPkgs (toText buildGHC))

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

instance MapDeps CabalPackage where
  mapDeps ctx f cabalPkg = do
    newGpd <- mapDeps ctx f (cbOriginal cabalPkg)
    pure cabalPkg {cbOriginal = newGpd}

newCabalPackage :: (MonadError Issue m, MonadIO m) => FilePath -> PkgName -> Version -> Dependencies -> m Status
newCabalPackage dir name version deps = do
  let package = emptyPackage name version deps
  liftIO $ writeGenericPackageDescription (dir </> (toString name <> ".cabal")) package
  pure Checked

emptyPackage :: PkgName -> Version -> Dependencies -> GenericPackageDescription
emptyPackage (P.PkgName name) version dependencies =
  let lib =
        emptyLibrary
          { libBuildInfo =
              emptyBuildInfo
                { targetBuildDepends = map mkCabalDependency (toDependencyList dependencies),
                  hsSourceDirs = [unsafeMakeSymbolicPath "src"]
                }
          }
   in GenericPackageDescription
        { packageDescription =
            emptyPackageDescription
              { package = PackageIdentifier (mkPackageName (toString name)) (toCabalVersion version),
                library = Just lib
              },
          condLibrary = Just (CondNode lib [] []),
          condExecutables = [],
          condTestSuites = [],
          condBenchmarks = [],
          gpdScannedVersion = Nothing,
          genPackageFlags = [],
          condSubLibraries = [],
          condForeignLibs = []
        }
