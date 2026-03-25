{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Cabal
  ( validateHackage,
    syncCabalProject,
    HasSourceDirs (..),
    CabalPackage,
    newCabalPackage,
    readCabalPackage,
    nativeSdist,
    setupCabalMatrixEnvironment,
  )
where

import Control.Exception (IOException)
import Control.Exception.Base (try)
import Control.Monad.Except (MonadError (throwError))
import qualified Data.ByteString as BS
import Data.Foldable (Foldable (..))
import qualified Data.Map as Map
import qualified Data.Text as T
import Distribution.Package (packageVersion)
import Distribution.PackageDescription (Benchmark (..), Executable (..), GenericPackageDescription (..), PackageDescription (..), PackageIdentifier (..), TestSuite (..), UnqualComponentName, emptyBuildInfo, emptyLibrary, emptyPackageDescription, mkPackageName, packageDescription)
import Distribution.PackageDescription.Check (PackageCheck (..), checkPackage)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Simple.Flag (toFlag)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.Setup (SDistFlags (..), defaultSDistFlags)
import Distribution.Simple.SrcDist (sdist)
import Distribution.Text (display)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Library (Library (..))
import Distribution.Utils.Path (getSymbolicPath, unsafeMakeSymbolicPath)
import Distribution.Verbosity (normal)
import qualified Distribution.Verbosity as Verbosity
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), Status (..))
import HWM.Core.Options (Options (..))
import HWM.Core.Pkg (IsPkg (..), PackageIO, Pkg (..), PkgName)
import HWM.Core.Sync (SyncMode (..))
import qualified HWM.Core.Pkg as P
import HWM.Core.Result (Issue (..), MonadIssue (..), Severity (..))
import HWM.Core.Version (Version, toCabalVersion)
import HWM.Domain.Build (Builder (CabalBuilder, NixBuilder))
import HWM.Domain.ConfigT (ConfigT)
import qualified HWM.Domain.ConfigT as CT
import HWM.Domain.Dependencies (Dependencies (..), HasDependencies (..), MapDeps (..), mkCabalDependency, toDependencyList)
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment)
import HWM.Runtime.Files (syncFile)
import HWM.Runtime.Process (EnvVars)
import Hpack (Force (..), Options (..), Result (..), defaultOptions, hpackResult, setProgramName, setTarget)
import qualified Hpack as H
import Hpack.Config (ProgramName (..))
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute, removePathForcibly, renameFile, withCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

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

toIssue :: Pkg -> PackageCheck -> Issue
toIssue pkg check =
  Issue
    { issueMessage = "Invalid Package [Cabal check]: " <> show check,
      issueSeverity = if isError check then SeverityError else SeverityWarning,
      issueTopic = P.pkgMemberId pkg,
      issueDetails = Nothing
    }

validateHackage :: Pkg -> ConfigT Status
validateHackage pkg = do
  gpd <- liftIO $ readGenericPackageDescription normal (P.cabalFile pkg)
  let ls = checkPackage gpd Nothing
  for_ ls $ \l -> injectIssue (toIssue pkg l)
  pure (maximum (Checked : map toStatus ls))

instance PackageIO CabalPackage ConfigT where
  rewritePackage = rewriteCabalPackage
  readPackage = readCabalPackage

getChanges :: Pkg -> (CabalPackage -> ConfigT (Maybe CabalPackage)) -> ConfigT (Maybe CabalPackage)
getChanges pkg mapCabal = do
  original <- readCabalPackage pkg
  updated <- mapCabal original
  case updated of
    Nothing -> pure Nothing
    Just newpackage ->
      if cbContent newpackage == cbContent original
        then pure Nothing
        else pure (Just newpackage)

forChanges :: (Applicative f) => Maybe t -> (t -> f a) -> f Status
forChanges changes f = do
  case changes of
    Nothing -> pure Checked
    Just newpackage -> f newpackage $> Updated

rewriteCabalPackage :: (CabalPackage -> ConfigT (Maybe CabalPackage)) -> Pkg -> ConfigT Status
rewriteCabalPackage mapCabal pkg@Pkg {hpackFile = Just path} = do
  changes <- getChanges pkg mapCabal
  update <- forChanges changes $ \_ -> hpackForceUpdate pkg path
  validation <- validateHackage pkg
  pure $ max validation update
rewriteCabalPackage mapCabal pkg = do
  changes <- getChanges pkg mapCabal
  update <- forChanges changes $ \package -> liftIO $ writeGenericPackageDescription (P.cabalFile pkg) (cbContent package)
  validation <- validateHackage pkg
  pure $ max validation update

hpackForceUpdate :: (MonadIO m) => Pkg -> FilePath -> m Status
hpackForceUpdate pkg path = do
  let programName = ProgramName $ toString $ P.pkgName pkg
  let ops = setTarget path $ setProgramName programName defaultOptions {optionsForce = Force}
  Result {..} <- liftIO $ hpackResult ops
  case resultStatus of
    H.OutputUnchanged -> pure Checked
    _ -> pure Updated

generateCabalProject :: Text -> BuildEnvironment -> Text
generateCabalProject prefix BuildEnvironment {..} =
  T.unlines
    $ ["with-compiler: ghc-" <> format buildGHC | not dependsOnNix]
    <> ("packages:" : map printPkg buildPkgs)
  where
    printPkg pkg = "  " <> prefix <> format (P.pkgDirPath pkg)
    dependsOnNix = buildBuilder == CabalBuilder True || buildBuilder == NixBuilder

setupCabalMatrixEnvironment :: BuildEnvironment -> ConfigT EnvVars
setupCabalMatrixEnvironment env = do
  matrixDir <- liftIO $ makeAbsolute ".hwm/matrix/"
  liftIO $ createDirectoryIfMissing True matrixDir
  let projectPath = matrixDir <> "cabal-" <> toString (buildName env) <> ".project"
  _ <- syncFile projectPath (generateCabalProject "../../" env)
  pure [("CABAL_PROJECT_FILE", projectPath)]

syncCabalProject :: SyncMode -> ConfigT Status
syncCabalProject SyncModeSync = do
  cabalFilePath <- asks (optionsCabal . CT.options)
  env <- getBuildEnvironment Nothing
  syncFile cabalFilePath (generateCabalProject "" env)
syncCabalProject SyncModeCheck = do
  cabalFilePath <- asks (optionsCabal . CT.options)
  exists <- liftIO $ doesFileExist cabalFilePath
  pure $ if exists then Checked else Warning
syncCabalProject SyncModeIgnore = pure Ignored

data CabalPackage = CabalPackage
  { cbDirectory :: FilePath,
    cbContent :: GenericPackageDescription
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
        cbContent = gpd
      }

class HasSourceDirs a where
  getSourceDirs :: [Text] -> a -> [(Text, Name)]

instance (HasSourceDirs a) => HasSourceDirs (Maybe a) where
  getSourceDirs tag (Just l) = getSourceDirs tag l
  getSourceDirs _ Nothing = []

instance (HasSourceDirs a) => HasSourceDirs (Map Text a) where
  getSourceDirs tags libs = concatMap (\(name, lib) -> getSourceDirs (tags <> [name]) lib) (Map.toList libs)

instance HasSourceDirs CabalPackage where
  getSourceDirs p CabalPackage {..} = getSourceDirs p cbContent

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
  getPkgName = getPkgName . cbContent
  getPkgVersion = getPkgVersion . cbContent
  setVersion version pkg = pkg {cbContent = setVersion version (cbContent pkg)}

instance HasDependencies CabalPackage where
  collectDependencies xs gpd = collectDependencies xs (cbContent gpd)

instance MapDeps CabalPackage where
  mapDeps ctx f cabalPkg = do
    newGpd <- mapDeps ctx f (cbContent cabalPkg)
    pure cabalPkg {cbContent = newGpd}

newCabalPackage :: (MonadError Issue m, MonadIO m) => FilePath -> PkgName -> Version -> Dependencies -> m Status
newCabalPackage dir name version deps = do
  let package = emptyPackage name version deps
  liftIO $ writeGenericPackageDescription (dir </> (toString name <> ".cabal")) package
  pure Updated

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

err :: Pkg -> Text -> Issue
err pkg msg = Issue (P.pkgMemberId pkg) SeverityError msg Nothing

nativeSdist :: Pkg -> ConfigT ((Pkg, Maybe FilePath), [Issue])
nativeSdist pkg = do
  gpkg <- readCabalFile pkg
  outDir <- liftIO (makeAbsolute $ "./.hwm/sdist" </> toString (P.pkgName pkg))
  (filePath, issues) <- runNativeSDist pkg (flattenPackageDescription gpkg) outDir
  pure ((pkg, filePath), issues <> map (toIssue pkg) (checkPackage gpkg Nothing))

runNativeSDist :: Pkg -> PackageDescription -> FilePath -> ConfigT (Maybe FilePath, [Issue])
runNativeSDist pkg pkgDesc outDir = do
  let tarName = toString (P.pkgName pkg) <> "-" <> display (packageVersion (package pkgDesc)) <> ".tar.gz"
      -- We'll look for it here: package_dir/dist/package-version.tar.gz
      localDistDir = P.pkgDirPath pkg </> "dist"
      tempTarPath = localDistDir </> tarName
      finalPath = outDir </> tarName

  result <- liftIO $ try $ do
    -- Setup the HWM output dir
    removePathForcibly outDir
    createDirectoryIfMissing True outDir
    -- prepare the 'dist' folder inside the package dir and run sdist
    removePathForcibly localDistDir
    createDirectoryIfMissing True localDistDir
    withCurrentDirectory (P.pkgDirPath pkg) $ sdist pkgDesc (defaultSDistFlags {sDistVerbosity = toFlag Verbosity.silent}) (const "") knownSuffixHandlers
    exists <- doesFileExist tempTarPath
    if exists
      then renameFile tempTarPath finalPath >> pure (Just finalPath)
      else pure Nothing

  case result of
    Right (Just path) -> pure (Just path, [])
    Right Nothing ->
      pure (Nothing, [err pkg $ "Cabal sdist ran, but " <> toText tarName <> " was not found."])
    Left (e :: IOException) ->
      pure (Nothing, [err pkg $ "Cabal sdist IO Error: " <> show e])
