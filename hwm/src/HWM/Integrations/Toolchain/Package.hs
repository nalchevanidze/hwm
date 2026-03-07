{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Package
  ( syncPackages,
    validatePackages,
    addPkgDependency,
    newPackage,
    deriveDependencyGraph,
  )
where

import qualified Data.Text as T
import HWM.Core.Formatting (Status, StatusM, monadStatus)
import HWM.Core.Pkg (IsPkg (..), Pkg (..), PkgName (PkgName), PkgSource (..), cabalSource, checkVersion, hpackSource)
import HWM.Domain.Bounds (Bounds (Bounds))
import HWM.Domain.Config (getRegistryBounds)
import HWM.Domain.ConfigT (ConfigT, askVersion)
import HWM.Domain.Dependencies
  ( Dependencies,
    Dependency (..),
    DependencyMap (..),
    HasDependencies (..),
    MapDeps (..),
    buildDependencyGraph,
    detectDependencyIssue,
    fromDependencyList,
    reportDependencyIssues,
    singleDeps,
    toDependencyList,
  )
import qualified HWM.Domain.Dependencies as M
import HWM.Domain.Workspace (allPackages, forWorkspace)
import HWM.Integrations.Toolchain.Cabal (CabalPackage, newCabalPackage, readCabalPackage, rewriteCabalPackage)
import HWM.Integrations.Toolchain.Hpack (HpackPackage, newHpackPackage, readHpackPackage, rewriteHpackPackage)
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

syncPackage :: (MapDeps a, IsPkg a) => PkgSource -> a -> ConfigT a
syncPackage pkg package = do
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
      pure ([(T.intercalate ":" path, depName, depBounds, Nothing) | isNothing bounds], Dependency depName (fromMaybe depBounds bounds))

addDeps :: (MapDeps a) => Dependency -> PkgSource -> a -> ConfigT a
addDeps dependency pkg = mapDeps (pkg, []) onlyMain
  where
    onlyMain (_, ["dependencies"]) deps = pure (deps <> singleDeps dependency)
    onlyMain _ deps = pure deps

addPkgDependency :: Dependency -> Pkg -> StatusM ConfigT
addPkgDependency dep = updatePackage (addDeps dep) (addDeps dep)

updatePackage :: (PkgSource -> HpackPackage -> ConfigT HpackPackage) -> (PkgSource -> CabalPackage -> ConfigT CabalPackage) -> Pkg -> StatusM ConfigT
updatePackage mapHpack mapCabal pkg =
  map (\s -> ("hpack", rewriteHpackPackage (mapHpack s) pkg)) (maybeToList $ hpackSource pkg)
    <> [("cabal", rewriteCabalPackage (mapCabal (cabalSource pkg)) pkg)]

deriveDependencyGraph :: ConfigT DependencyMap
deriveDependencyGraph = buildDependencyGraph (concatMap (toDependencyList . snd) . libDependencies) <$> (allPackages >>= traverse readCabalPackage)
  where
    libDependencies = filter (\x -> fst x == ["library"]) . collectDependencies []

validatePackages :: ConfigT ()
validatePackages = forWorkspace $ \pkg -> map (hpack pkg) (maybeToList $ hpackSource pkg) <> [cabalTask pkg]
  where
    hpack pkg src =
      ( "hpack",
        do
          h <- readHpackPackage pkg
          validatePackage (src, h)
      )
    cabalTask pkg =
      ( "cabal",
        do
          cabal <- readCabalPackage pkg
          validatePackage (cabalSource pkg, cabal)
      )

validatePackage :: (IsPkg a, HasDependencies a) => (PkgSource, a) -> ConfigT Status
validatePackage (source, package) = do
  checkVersion source package
  diffs <- concat <$> traverse checkForDependencyIssues (collectDependencies [] package)
  monadStatus (reportDependencyIssues source diffs)
  where
    checkForDependencyIssues (path, deps) = concat <$> traverse (getIssue path) (toDependencyList deps)
    getIssue path dep = detectDependencyIssue path dep <$> getRegistryBounds (hwmDepName dep)
