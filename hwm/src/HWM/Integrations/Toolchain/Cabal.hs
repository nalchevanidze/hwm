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
import Data.Traversable (for)
import Distribution.Package (Dependency (..), unPackageName)
import Distribution.PackageDescription (Executable (..), GenericPackageDescription (..), PackageDescription (..), PackageIdentifier (..), UnqualComponentName, ignoreConditions, packageDescription)
import Distribution.PackageDescription.Check (PackageCheck (..), checkPackage)
import Distribution.PackageDescription.Parsec
import Distribution.Simple (VersionInterval (..))
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Library (Library (..))
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity (normal)
import Distribution.Version
  ( LowerBound (..),
    UpperBound (..),
    VersionRange,
    asVersionIntervals,
  )
import qualified Distribution.Version as Cabal
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), Status (..))
import HWM.Core.Options (Options (..))
import HWM.Core.Pkg (Pkg (Pkg, hpackFile), PkgName (..))
import qualified HWM.Core.Pkg as P
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..), fromEither)
import HWM.Core.Version (Version, fromCabalVersion)
import HWM.Domain.Bounds (Bound (Bound), Bounds (..), Restriction (Max, Min))
import HWM.Domain.ConfigT (ConfigT)
import qualified HWM.Domain.ConfigT as CT
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

-- Helper to extract Haskell dependencies from a CondTree
-- It flattens the tree ignoring conditions (standard approach for workspace analysis)
flattenDeps :: (Semigroup a) => CondTree v [Dependency] a -> [Dependency]
flattenDeps condTree = snd $ ignoreConditions condTree

convertName :: Dependency -> Name
convertName (Dependency pkgName _ _) = T.pack $ unPackageName pkgName

-- | Extracts the VersionRange from a Dependency
convertBounds :: (MonadFail m) => Dependency -> m Bounds
convertBounds (Dependency _ bounds _) = toMinMax bounds

data CabalPackage = CabalPackage
  { cbName :: PkgName,
    cbVersion :: Version,
    cbDirectory :: FilePath,
    cbDependencies :: Map Name Bounds,
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
  let pd = packageDescription gpd
  let pid = package pd
  version <- fromEither "" (fromCabalVersion $ pkgVersion pid)
  let libDeps = maybe [] flattenDeps (condLibrary gpd)
  let subLibDeps = concatMap (flattenDeps . snd) (condSubLibraries gpd)
  let exeDeps = concatMap (flattenDeps . snd) (condExecutables gpd)
  let testDeps = concatMap (flattenDeps . snd) (condTestSuites gpd)

  let allDeps = libDeps ++ subLibDeps ++ exeDeps ++ testDeps
  depMap <- fmap Map.fromList $ for allDeps $ \d -> do
    bounds <- fromEither "" (convertBounds d)
    pure (convertName d, bounds)

  pure
    CabalPackage
      { cbName = PkgName . toText . unPackageName $ pkgName pid,
        cbVersion = version, -- Assuming your Version type matches
        cbDependencies = depMap,
        cbDirectory = takeDirectory (P.cabalFile pkg),
        cbOriginal = gpd
      }

isInclusive :: Cabal.Bound -> Bool
isInclusive Cabal.InclusiveBound = True
isInclusive Cabal.ExclusiveBound = False

toBounds :: (MonadFail m) => VersionInterval -> m [Bound]
toBounds (VersionInterval (LowerBound v lb) NoUpperBound) = sequence [Bound Min (isInclusive lb) <$> fromCabalVersion v]
toBounds (VersionInterval (LowerBound v lb) (UpperBound v2 ub)) = sequence [Bound Min (isInclusive lb) <$> fromCabalVersion v, Bound Max (isInclusive ub) <$> fromCabalVersion v2]

toMinMax :: (MonadFail m) => VersionRange -> m Bounds
toMinMax range = do
  intervals <- traverse toBounds (asVersionIntervals range)
  case sort (concat intervals) of
    [] -> pure $ Bounds Nothing Nothing -- -none or empty range
    intervals' ->
      case (viaNonEmpty head intervals', viaNonEmpty last intervals') of
        (Just x, Just y) -> pure $ Bounds (Just x) (Just y)
        (_, _) -> pure $ Bounds Nothing Nothing

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

instance (HasSourceDirs a) => HasSourceDirs (CondTree v c a) where
  getSourceDirs path condTree = getSourceDirs path (condTreeData condTree)

instance (HasSourceDirs a) => HasSourceDirs [(UnqualComponentName, a)] where
  getSourceDirs path = concatMap (\(name, info) -> getSourceDirs (path <> [format name]) info)

instance HasSourceDirs Library where
  getSourceDirs path Library {..} = getSourceDirs path libBuildInfo

instance HasSourceDirs Executable where
  getSourceDirs path Executable {..} = getSourceDirs path buildInfo

instance HasSourceDirs BuildInfo where
  getSourceDirs path buildInfo = map (withKey . getSymbolicPath) (hsSourceDirs buildInfo)
    where
      withKey dir = (T.intercalate ":" path, format dir)

-- <> condLibs (p <> ["test"]) condTestSuites
-- <> condLibs (p <> ["exe"]) condExecutables
-- <> condLibs (p <> ["bench"]) condBenchmarks