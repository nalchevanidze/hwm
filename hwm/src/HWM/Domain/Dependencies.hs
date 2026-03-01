{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Dependencies
  ( Dependencies (..),
    Dependency (..),
    toDependencyList,
    fromDependencyList,
    mergeDependencies,
    normalizeDependencies,
    DependencyMap (..),
    sortByDependencyHierarchy,
    singleDeps,
    HasDependencies (..),
    collectNormalizedDependencies,
    buildDependencyGraph,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Distribution.PackageDescription (Benchmark (..), BuildInfo (..), Executable (..), GenericPackageDescription, Library (..), TestSuite (..), UnqualComponentName)
import qualified Distribution.PackageDescription as Cabal
import Distribution.Simple (UpperBound (..), VersionRange, asVersionIntervals)
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Version (LowerBound (..), VersionInterval (..))
import qualified Distribution.Version as Cabal
import HWM.Core.Formatting (Format (..), formatTable, subPathSign)
import HWM.Core.Parsing (Parse (..), firstWord)
import HWM.Core.Pkg (IsPkg (..), Pkg (..), PkgName (..))
import HWM.Core.Result (Issue (..), Severity (..))
import HWM.Core.Version (fromCabalVersion)
import HWM.Domain.Bounds (Bound (..), Bounds (..), Restriction (..), boundsBetter, hasBounds)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    length,
    null,
    show,
    toList,
  )

data Dependency = Dependency
  { name :: PkgName,
    bounds :: Bounds
  }
  deriving (Show, Eq)

instance Parse Dependency where
  parse =
    (\(name, txt) -> Dependency <$> parse name <*> parse txt)
      . firstWord

instance Format Dependency where
  format Dependency {..} = format name <> " " <> format bounds

newtype Dependencies = Dependencies {unpackDeps :: Map PkgName Bounds}
  deriving (Show)

instance Semigroup Dependencies where
  (Dependencies a) <> (Dependencies b) = Dependencies (a <> b)

singleDeps :: Dependency -> Dependencies
singleDeps (Dependency name bounds) = Dependencies (Map.singleton name bounds)

initDependencies :: [Dependency] -> Dependencies
initDependencies = Dependencies . Map.fromList . map toDuple
  where
    toDuple (Dependency a b) = (a, b)

toDependencyList :: Dependencies -> [Dependency]
toDependencyList (Dependencies m) = map (uncurry Dependency) $ Map.toList m

instance FromJSON Dependencies where
  parseJSON v = initDependencies <$> (parseJSON v >>= traverse parse . sort)

instance ToJSON Dependencies where
  toJSON = toJSON . formatTable . map format . toDependencyList

fromDependencyList :: [Dependency] -> Dependencies
fromDependencyList = initDependencies

mergeDependencies :: [Dependency] -> [Dependency]
mergeDependencies = Map.elems . foldl' step Map.empty
  where
    step acc dep =
      Map.insertWith prefer (name dep) dep acc
    prefer new old = if boundsBetter (bounds new) (bounds old) then new else old

normalizeDependencies :: [Dependency] -> [Dependency]
normalizeDependencies = filter (hasBounds . bounds) . mergeDependencies

newtype DependencyMap = DependencyMap (Map PkgName [PkgName])

buildDependencyGraph :: (IsPkg a) => (a -> [Dependency]) -> [a] -> DependencyMap
buildDependencyGraph collectDeps packages = DependencyMap $ Map.fromList [(getPkgName pkg, internalDeps pkg) | pkg <- packages]
  where
    internalNames = Set.fromList (map getPkgName packages)
    internalDeps pkg = mapMaybe selectInternal (collectDeps pkg)
      where
        selectInternal (Dependency depName _) =
          if Set.member depName internalNames then Just depName else Nothing

instance Format DependencyMap where
  format graph = T.intercalate "\n" (map (formatTree 0) (toTree graph))

formatTree :: Int -> Tree -> Text
formatTree depth (Node pkg deps) = newLine <> format pkg <> children
  where
    newLine | depth == 0 = "\n    • " | otherwise = "\n  " <> T.replicate depth "  " <> subPathSign
    children = T.intercalate "" (map (formatTree (depth + 1)) deps)

data Tree = Node PkgName [Tree]

toTree :: DependencyMap -> [Tree]
toTree (DependencyMap graph) =
  let allPkgs = Map.keysSet graph <> foldMap Set.fromList (Map.elems graph)
      dependentPkgs = foldMap Set.fromList (Map.elems graph)
      rootPkgs = Set.toList (Set.difference allPkgs dependentPkgs)
   in map (buildTree graph Set.empty) rootPkgs

buildTree :: Map PkgName [PkgName] -> Set PkgName -> PkgName -> Tree
buildTree graph visited pkg =
  if Set.member pkg visited
    then Node pkg []
    else
      let deps = Map.findWithDefault [] pkg graph
          newVisited = Set.insert pkg visited
          childTrees = map (buildTree graph newVisited) deps
       in Node pkg childTrees

topologicalSort :: DependencyMap -> Either [PkgName] [PkgName]
topologicalSort (DependencyMap graph) = goFunc [] initialZero indegreeMap
  where
    nodes = Map.keysSet graph <> foldMap Set.fromList (Map.elems graph)
    indegreeMap = foldl' updateIndegree baseIndegree (Map.toList graph)
    baseIndegree = Map.fromSet (const (0 :: Int)) nodes
    updateIndegree acc (_, deps) = foldl' increment acc deps
    increment acc dep = Map.insertWith (+) dep 1 acc
    initialZero = Set.fromList [pkg | (pkg, deg) <- Map.toList indegreeMap, deg == 0]

    goFunc acc zeros indegrees
      | Set.null zeros =
          case Map.keys (Map.filter (> 0) indegrees) of
            [] -> Right (reverse acc)
            cycleNodes -> Left cycleNodes
      | otherwise =
          let (pkg, remainingZeros) = Set.deleteFindMin zeros
              neighbours = Map.findWithDefault [] pkg graph
              (nextZeros, nextIndegrees) = foldl' reduce (remainingZeros, indegrees) neighbours
           in goFunc (pkg : acc) nextZeros nextIndegrees

    reduce (zeros, indegrees) neighbour =
      let deg = Map.findWithDefault 0 neighbour indegrees - 1
          updatedIndegrees = Map.insert neighbour deg indegrees
          updatedZeros = if deg == 0 then Set.insert neighbour zeros else zeros
       in (updatedZeros, updatedIndegrees)

sortByDependencyHierarchy :: (MonadError Issue m) => DependencyMap -> [Pkg] -> m [Pkg]
sortByDependencyHierarchy graph ns = do
  case topologicalSort graph of
    Left depCycle ->
      let cycleNames = intercalate " -> " (map toString depCycle)
       in throwError
            Issue
              { issueTopic = "dependency-resolution",
                issueSeverity = SeverityError,
                issueMessage = fromString $ "Dependency cycle detected: " <> cycleNames,
                issueDetails = Nothing
              }
    Right sortedNames ->
      let indexes = Map.fromList (zip sortedNames [0 ..] :: [(PkgName, Int)])
          findIndex pkg = Map.findWithDefault maxBound (pkgName pkg) indexes
       in pure $ sortOn (Down . findIndex) ns

class HasDependencies a where
  collectDependencies :: [Text] -> a -> [([Text], Dependencies)]

instance HasDependencies Dependencies where
  collectDependencies scope lib = [(scope, lib)]

instance (HasDependencies a) => HasDependencies (Map Text a) where
  collectDependencies scope libs = concatMap (\(name, lib) -> collectDependencies (scope <> [name]) lib) (Map.toList libs)

instance (HasDependencies a) => HasDependencies (Maybe a) where
  collectDependencies scope = maybe [] (collectDependencies scope)

collectNormalizedDependencies :: (HasDependencies a) => a -> [Dependency]
collectNormalizedDependencies package = normalizeDependencies (concatMap (toDependencyList . snd) $ collectDependencies [] package)

instance HasDependencies GenericPackageDescription where
  collectDependencies xs GenericPackageDescription {..} =
    concat
      [ collectDependencies (xs <> ["library"]) condLibrary,
        collectDependencies (xs <> ["tests"]) condTestSuites,
        collectDependencies (xs <> ["executables"]) condExecutables,
        collectDependencies (xs <> ["benchmarks"]) condBenchmarks
      ]

instance (HasDependencies deps, HasDependencies lib) => HasDependencies (CondTree v deps lib) where
  collectDependencies path condTree =
    collectDependencies path (condTreeConstraints condTree)
      <> concatMap (collectDependencies path) (condTreeComponents condTree)

instance (HasDependencies a) => HasDependencies [(UnqualComponentName, a)] where
  collectDependencies path = concatMap (\(name, info) -> collectDependencies (path <> [format name]) info)

instance (HasDependencies a) => HasDependencies (CondBranch v d a) where
  collectDependencies path = concatMap (collectDependencies path)

fromCabalDependency :: Cabal.Dependency -> Dependency
fromCabalDependency (Cabal.Dependency pkgName versionRange _) =
  Dependency (PkgName (format $ Cabal.unPackageName pkgName)) (toMinMax versionRange)

fromCabalDependencies :: [Cabal.Dependency] -> Dependencies
fromCabalDependencies = fromDependencyList . map fromCabalDependency

instance HasDependencies [Cabal.Dependency] where
  collectDependencies path deps = [(path, fromCabalDependencies deps)]

instance HasDependencies Library where
  collectDependencies path deps = collectDependencies path (libBuildInfo deps)

instance HasDependencies TestSuite where
  collectDependencies path deps = collectDependencies path (testBuildInfo deps)

instance HasDependencies Executable where
  collectDependencies path deps = collectDependencies path (buildInfo deps)

instance HasDependencies Benchmark where
  collectDependencies path buildInfo = collectDependencies path (benchmarkBuildInfo buildInfo)

instance HasDependencies Cabal.BuildInfo where
  collectDependencies path buildInfo = collectDependencies path (fromCabalDependencies (targetBuildDepends buildInfo))

isInclusive :: Cabal.Bound -> Bool
isInclusive Cabal.InclusiveBound = True
isInclusive Cabal.ExclusiveBound = False

toBounds :: VersionInterval -> [Bound]
toBounds (VersionInterval (LowerBound v lb) NoUpperBound) = [Bound Min (isInclusive lb) $ fromCabalVersion v]
toBounds (VersionInterval (LowerBound v lb) (UpperBound v2 ub)) = [Bound Min (isInclusive lb) $ fromCabalVersion v, Bound Max (isInclusive ub) $ fromCabalVersion v2]

toMinMax :: VersionRange -> Bounds
toMinMax range = do
  let intervals = map toBounds (asVersionIntervals range)
  case sort (concat intervals) of
    [] -> Bounds Nothing Nothing -- -none or empty range
    intervals' ->
      case (viaNonEmpty head intervals', viaNonEmpty last intervals') of
        (Just x, Just y) -> Bounds (Just x) (Just y)
        (_, _) -> Bounds Nothing Nothing
