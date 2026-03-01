{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Cabal
  ( syncCabalPackage,
    validateHackage,
    syncCabalProject,
    fromCabal,
    parseCabal,
    scanPkgs,
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
import Distribution.PackageDescription (GenericPackageDescription (..), PackageDescription (..), PackageIdentifier (..), ignoreConditions, packageDescription)
import Distribution.PackageDescription.Check (PackageCheck (..), checkPackage)
import Distribution.PackageDescription.Parsec
import Distribution.Simple (VersionInterval (..))
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.CondTree (CondTree)
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
import HWM.Core.Pkg (Pkg (Pkg), PkgName (..), cabalFilePath)
import qualified HWM.Core.Pkg as P
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..), fromEither)
import HWM.Core.Version (Version, fromCabalVersion)
import HWM.Domain.Bounds (Bound (Bound), Bounds (..), Restriction (Max, Min))
import HWM.Domain.ConfigT (ConfigT, Env (options))
import HWM.Domain.Environments (BuildEnvironment (..), getBuildEnvironment)
import HWM.Runtime.Files (cleanRelativePath, remove)
import HWM.Runtime.Logging (debug)
import HWM.Runtime.UI
import Hpack (Result (..), defaultOptions, hpackResult, setProgramName, setTarget)
import qualified Hpack as H
import Hpack.Config (ProgramName (..))
import Relude
import System.FilePath (makeRelative, takeDirectory, takeFileName)
import System.FilePath.Glob (glob)
import HWM.Integrations.Toolchain.Stack (pkgYamlPath)

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

syncCabalPackage :: Pkg -> ConfigT Status
syncCabalPackage pkg = do
  cbl <- parseCabal (cabalFilePath pkg)
  remove (cabalFilePath pkg)
  let programName = ProgramName $ toString $ P.pkgName pkg
  debug (show cbl)
  let ops = setTarget (pkgYamlPath pkg) $ setProgramName programName defaultOptions
  Result {..} <- liftIO $ hpackResult ops
  ls <- validateHackage pkg resultCabalFile

  s <- case resultStatus of
    H.OutputUnchanged -> pure Checked
    _ -> pure Updated
  pure $ maximum (s : ls)

generateCabalProject :: [Pkg] -> Text -> Text
generateCabalProject packagePaths ghcVersion =
  T.unlines
    [ "with-compiler: ghc-" <> ghcVersion,
      "packages:\n" <> T.unlines (map (("  " <>) . format . P.pkgDirPath) packagePaths)
    ]

syncCabalProject :: ConfigT ()
syncCabalProject = do
  ops <- asks options
  BuildEnvironment {..} <- getBuildEnvironment Nothing
  liftIO $ TIO.writeFile (optionsCabal ops) (generateCabalProject buildPkgs (toText buildGHC))

data WorkspacePackage = WorkspacePackage
  { name :: PkgName,
    version :: Version,
    directory :: FilePath,
    dependencies :: Map Name Bounds
  }
  deriving (Show)

-- Helper to extract Haskell dependencies from a CondTree
-- It flattens the tree ignoring conditions (standard approach for workspace analysis)
flattenDeps :: (Semigroup a) => CondTree v [Dependency] a -> [Dependency]
flattenDeps condTree = snd $ ignoreConditions condTree

convertName :: Dependency -> Name
convertName (Dependency pkgName _ _) = T.pack $ unPackageName pkgName

-- | Extracts the VersionRange from a Dependency
convertBounds :: (MonadFail m) => Dependency -> m Bounds
convertBounds (Dependency _ bounds _) = toMinMax bounds

parseGPD :: (Monad m, MonadError Issue m) => ByteString -> m GenericPackageDescription
parseGPD content =
  let (_, result) = runParseResult (parseGenericPackageDescription content)
   in case result of
        Right gpd -> pure gpd
        Left (_, errors) ->
          -- Join all Cabal errors into a single string for the MonadFail
          throwError $ fromString $ "Cabal parsing failed: " ++ show errors

parseCabal :: (MonadIO m, MonadError Issue m) => FilePath -> m WorkspacePackage
parseCabal path = do
  content <- liftIO $ BS.readFile path
  parseGPD content >>= fromCabal path

fromCabal :: (Monad m, MonadError Issue m) => FilePath -> GenericPackageDescription -> m WorkspacePackage
fromCabal path gpd = do
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
    WorkspacePackage
      { name = PkgName . toText . unPackageName $ pkgName pid,
        version, -- Assuming your Version type matches
        dependencies = depMap,
        directory = takeDirectory path
      }

-- Your Target Types
-- data Restriction = Min | Max deriving (Show, Eq)

-- | Converts Cabal's internal Bound to your Bool
isInclusive :: Cabal.Bound -> Bool
isInclusive Cabal.InclusiveBound = True
isInclusive Cabal.ExclusiveBound = False

toBounds :: (MonadFail m) => VersionInterval -> m [Bound]
toBounds (VersionInterval (LowerBound v lb) NoUpperBound) = sequence [Bound Min (isInclusive lb) <$> fromCabalVersion v]
toBounds (VersionInterval (LowerBound v lb) (UpperBound v2 ub)) = sequence [Bound Min (isInclusive lb) <$> fromCabalVersion v, Bound Max (isInclusive ub) <$> fromCabalVersion v2]

-- | Main Conversion Function
toMinMax :: (MonadFail m) => VersionRange -> m Bounds
toMinMax range = do
  intervals <- traverse toBounds (asVersionIntervals range)
  case sort (concat intervals) of
    [] -> pure $ Bounds Nothing Nothing -- -none or empty range
    intervals' ->
      case (viaNonEmpty head intervals', viaNonEmpty last intervals') of
        (Just x, Just y) -> pure $ Bounds (Just x) (Just y)
        (_, _) -> pure $ Bounds Nothing Nothing

scanPkgs :: (MonadIO m, MonadError Issue m, MonadUI m) => FilePath -> m [Pkg]
scanPkgs root = do
  paths <- map (makeRelative root) <$> liftIO (glob "./**/**/*.cabal")
  infos <- traverse parseCabal paths
  for infos $ \WorkspacePackage {..} -> do
    let pkgDir = directory
    debug $ "Found package.yaml at: " <> format pkgDir
    let memberName = toText $ takeFileName pkgDir
    let groupName = maybe "" toText $ cleanRelativePath (Just (takeDirectory pkgDir))
    pure $ Pkg name groupName memberName pkgDir