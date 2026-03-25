{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Package
  ( syncPackages,
    syncPackagesByMode,
    validatePackages,
    addPkgDependency,
    newPackage,
    deriveDependencyGraph,
  )
where

import qualified Data.Text as T
import HWM.Core.Formatting (Status, StatusM, monadStatus)
import HWM.Core.Pkg (IsPkg (..), PackageIO (..), Pkg (..), PkgName (PkgName), PkgSource (..), cabalSource, getVersionIssues, hpackSource)
import HWM.Core.Result (Issue, MonadIssue (injectIssue))
import HWM.Core.Sync (SyncMode (..))
import HWM.Domain.Bounds (Bounds (Bounds))
import HWM.Domain.Config (getRegistryBounds)
import HWM.Domain.ConfigT (ConfigT, askVersion)
import HWM.Domain.Dependencies
  ( Dependencies,
    Dependency (..),
    DependencyBoundsIssue (..),
    DependencyMap (..),
    HasDependencies (..),
    MapDeps (..),
    buildDependencyGraph,
    collectDependencyIssues,
    fromDependencyList,
    reportDependencyIssues,
    singleDeps,
    toDependencyList,
  )
import qualified HWM.Domain.Dependencies as M
import HWM.Domain.Workspace (allPackages, forWorkspace)
import HWM.Integrations.Toolchain.Cabal (CabalPackage, newCabalPackage, readCabalPackage, validateHackage)
import HWM.Integrations.Toolchain.Hpack (HpackPackage, newHpackPackage, readHpackPackage)
import Relude

newPackage :: FilePath -> PkgName -> ConfigT [(Text, Status)]
newPackage targetDir name = do
  let baseName = PkgName "base"
  version <- askVersion
  base <- fromMaybe (Bounds Nothing Nothing) <$> getRegistryBounds baseName
  let deps = M.singleDeps (Dependency baseName base)
  hpack <- newHpackPackage targetDir name version deps
  cabal <- newCabalPackage targetDir name version deps
  pure [("hpack", hpack), ("cabal", cabal)]

syncPackages :: ConfigT ()
syncPackages = forWorkspace $ updatePackage syncPackage syncPackage

syncPackagesByMode :: SyncMode -> ConfigT ()
syncPackagesByMode SyncModeSync = syncPackages
syncPackagesByMode SyncModeCheck = validatePackages
syncPackagesByMode SyncModeIgnore = pure ()

syncPackage :: (MapDeps a, IsPkg a, HasDependencies a) => PkgSource -> a -> ConfigT (Maybe a)
syncPackage pkg package = do
  noIssues <- hasNoIssues pkg package
  if noIssues
    then pure Nothing
    else
      Just <$> do
        result <- mapDeps (pkg, []) syncDeps package
        (`setVersion` result) <$> askVersion

syncDeps :: (PkgSource, [Text]) -> Dependencies -> ConfigT Dependencies
syncDeps (pkg, path) deps =
  fromDependencyList <$> do
    (issues, results) <- unzip <$> traverse syncDep (toDependencyList deps)
    reportDependencyIssues pkg (concat issues) $> results
  where
    syncDep (Dependency depName depBounds) = do
      bounds <- getRegistryBounds depName
      pure ([DependencyBoundsIssue (T.intercalate ":" path) depName depBounds Nothing | isNothing bounds], Dependency depName (fromMaybe depBounds bounds))

addDeps :: (MapDeps a) => Dependency -> PkgSource -> a -> ConfigT (Maybe a)
addDeps dependency pkg = fmap Just . mapDeps (pkg, []) onlyMain
  where
    onlyMain (_, ["dependencies"]) deps = pure (deps <> singleDeps dependency)
    onlyMain _ deps = pure deps

addPkgDependency :: Dependency -> Pkg -> StatusM ConfigT
addPkgDependency dep = updatePackage (addDeps dep) (addDeps dep)

forFormats :: (IsString a) => (PkgSource -> Pkg -> b) -> (PkgSource -> Pkg -> b) -> Pkg -> [(a, b)]
forFormats hpack cabal pkg =
  map (\s -> ("hpack", hpack s pkg)) (maybeToList $ hpackSource pkg)
    <> [("cabal", cabal (cabalSource pkg) pkg)]

updatePackage :: (PkgSource -> HpackPackage -> ConfigT (Maybe HpackPackage)) -> (PkgSource -> CabalPackage -> ConfigT (Maybe CabalPackage)) -> Pkg -> StatusM ConfigT
updatePackage mapHpack mapCabal = forFormats (rewritePackage . mapHpack) (rewritePackage . mapCabal)

deriveDependencyGraph :: ConfigT DependencyMap
deriveDependencyGraph = buildDependencyGraph (concatMap (toDependencyList . snd) . libDependencies) <$> (allPackages >>= traverse readCabalPackage)
  where
    libDependencies = filter (\x -> fst x == ["library"]) . collectDependencies []

validatePackages :: ConfigT ()
validatePackages = forWorkspace $ forFormats hpack cabal
  where
    hpack src pkg = readHpackPackage pkg >>= validatePackage src
    cabal _ pkg = do
      s1 <- readCabalPackage pkg >>= validatePackage (cabalSource pkg)
      s2 <- validateHackage pkg
      pure (s1 <> s2)

hasNoIssues :: (IsPkg a, HasDependencies a) => PkgSource -> a -> ConfigT Bool
hasNoIssues source pkg = do
  (issues, depIssues) <- collectIssues source pkg
  let depIssues' = filter (isJust . depIssueRegistryBounds) depIssues
  pure (null issues && null depIssues')

collectIssues :: (IsPkg a, HasDependencies a) => PkgSource -> a -> ConfigT ([Issue], [DependencyBoundsIssue])
collectIssues source pkg = do
  versionIssues <- getVersionIssues source pkg
  depIssues <- collectDependencyIssues getRegistryBounds pkg
  pure (versionIssues, depIssues)

validatePackage :: (IsPkg a, HasDependencies a) => PkgSource -> a -> ConfigT Status
validatePackage source package = monadStatus $ do
  (issues, depIssues) <- collectIssues source package
  traverse_ injectIssue issues
  reportDependencyIssues source depIssues
