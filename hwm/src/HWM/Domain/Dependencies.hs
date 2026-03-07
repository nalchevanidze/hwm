{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
    MapDeps (..),
    DependencyIssue,
    reportDependencyIssues,
    detectDependencyIssue,
    mkCabalDependency,
    collectDependencyIssues,
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
import Distribution.Package (mainLibSet)
import Distribution.PackageDescription (Benchmark (..), BuildInfo (..), Executable (..), GenericPackageDescription, Library (..), TestSuite (..), UnqualComponentName)
import qualified Distribution.PackageDescription as Cabal
import Distribution.Simple (UpperBound (..), VersionRange, asVersionIntervals)
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Version (LowerBound (..), VersionInterval (..))
import qualified Distribution.Version as Cabal
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..), formatTable, subPathSign)
import HWM.Core.Parsing (Parse (..), firstWord)
import HWM.Core.Pkg (IsPkg (..), Pkg (..), PkgName (..), PkgSource (..))
import HWM.Core.Result (Issue (..), IssueDetails (..), MonadIssue (..), Severity (..))
import HWM.Core.Version (fromCabalVersion, toCabalVersion)
import HWM.Domain.Bounds (Bound (..), Bounds (..), Restriction (..), boundsBetter, hasBounds)
import Relude

data Dependency = Dependency
  { hwmDepName :: PkgName,
    hwmDepBounds :: Bounds
  }
  deriving (Show, Eq)

instance Parse Dependency where
  parse =
    (\(name, txt) -> Dependency <$> parse name <*> parse txt)
      . firstWord

instance Format Dependency where
  format Dependency {..} = format hwmDepName <> " " <> format hwmDepBounds

newtype Dependencies = Dependencies {unpackDeps :: Map PkgName Bounds}
  deriving (Show)

instance Semigroup Dependencies where
  (Dependencies a) <> (Dependencies b) = Dependencies (a <> b)

singleDeps :: Dependency -> Dependencies
singleDeps (Dependency hwmDepName bounds) = Dependencies (Map.singleton hwmDepName bounds)

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
      Map.insertWith prefer (hwmDepName dep) dep acc
    prefer new old = if boundsBetter (hwmDepBounds new) (hwmDepBounds old) then new else old

normalizeDependencies :: [Dependency] -> [Dependency]
normalizeDependencies = filter (hasBounds . hwmDepBounds) . mergeDependencies

newtype DependencyMap = DependencyMap (Map PkgName [PkgName])

buildDependencyGraph :: (IsPkg a) => (a -> [Dependency]) -> [a] -> DependencyMap
buildDependencyGraph collectDeps packages = DependencyMap $ Map.fromList [(getPkgName pkg, internalDeps pkg) | pkg <- packages]
  where
    internalNames = Set.fromList (map getPkgName packages)
    internalDeps pkg = mapMaybe selectInternal (collectDeps pkg)
      where
        selectInternal (Dependency dName _) =
          if Set.member dName internalNames then Just dName else Nothing

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
      <> collectDependencies path (condTreeData condTree)

instance (HasDependencies a) => HasDependencies [(UnqualComponentName, a)] where
  collectDependencies path = concatMap (\(name, info) -> collectDependencies (path <> [format name]) info)

instance (HasDependencies a, HasDependencies d) => HasDependencies (CondBranch v d a) where
  collectDependencies path (CondBranch _ d a) = concatMap (collectDependencies path) d <> collectDependencies path a

fromCabalDependency :: Cabal.Dependency -> Dependency
fromCabalDependency (Cabal.Dependency pkgName versionRange _) =
  Dependency (PkgName (format $ Cabal.unPackageName pkgName)) (toMinMax versionRange)

depName :: Cabal.Dependency -> Name
depName (Cabal.Dependency pkgName _ _) = format $ Cabal.unPackageName pkgName

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

type DepsCtx = (PkgSource, [Text])

class MapDeps a where
  mapDeps :: (Monad m) => DepsCtx -> (DepsCtx -> Dependencies -> m Dependencies) -> a -> m a

instance MapDeps Dependencies where
  mapDeps ctx f = f ctx

instance (MapDeps a) => MapDeps (Map Text a) where
  mapDeps (pkg, path) f = Map.traverseWithKey (\name lib -> mapDeps (pkg, path <> [name]) f lib)

instance (MapDeps a) => MapDeps (Maybe a) where
  mapDeps ctx f = maybe (pure Nothing) (fmap Just . mapDeps ctx f)

instance MapDeps GenericPackageDescription where
  mapDeps (pkg, xs) f GenericPackageDescription {..} = do
    coLibrary <- mapDeps (pkg, xs <> ["library"]) f condLibrary
    coTestSuites <- mapDeps (pkg, xs <> ["tests"]) f condTestSuites
    coExecutables <- mapDeps (pkg, xs <> ["executables"]) f condExecutables
    coBenchmarks <- mapDeps (pkg, xs <> ["benchmarks"]) f condBenchmarks
    pure GenericPackageDescription {condLibrary = coLibrary, condTestSuites = coTestSuites, condExecutables = coExecutables, condBenchmarks = coBenchmarks, ..}

instance (MapDeps deps, MapDeps lib) => MapDeps (CondTree v deps lib) where
  mapDeps ctx f condTree = do
    newConstraints <- mapDeps ctx f (condTreeConstraints condTree)
    newComponents <- mapM (mapDeps ctx f) (condTreeComponents condTree)
    newData <- mapDeps ctx f (condTreeData condTree)
    pure condTree {condTreeConstraints = newConstraints, condTreeComponents = newComponents, condTreeData = newData}

instance (MapDeps a, MapDeps d) => MapDeps (CondBranch v d a) where
  mapDeps ctx f condBranch = do
    idTrue <- mapDeps ctx f (condBranchIfTrue condBranch)
    ifFalse <- mapM (mapDeps ctx f) (condBranchIfFalse condBranch)
    pure condBranch {condBranchIfTrue = idTrue, condBranchIfFalse = ifFalse}

instance (MapDeps a) => MapDeps [(UnqualComponentName, a)] where
  mapDeps ctx f = mapM (\(name, info) -> fmap (name,) (mapDeps (fst ctx, snd ctx <> [format name]) f info))

instance MapDeps Library where
  mapDeps ctx f lib = do
    newBuildInfo <- mapDeps ctx f (libBuildInfo lib)
    pure lib {libBuildInfo = newBuildInfo}

instance MapDeps Executable where
  mapDeps ctx f exe = do
    newBuildInfo <- mapDeps ctx f (buildInfo exe)
    pure exe {buildInfo = newBuildInfo}

instance MapDeps BuildInfo where
  mapDeps ctx f buildInfo = do
    newDeps <- mapDeps ctx f (targetBuildDepends buildInfo)
    pure buildInfo {targetBuildDepends = newDeps}

instance MapDeps TestSuite where
  mapDeps ctx f test = do
    newBuildInfo <- mapDeps ctx f (testBuildInfo test)
    pure test {testBuildInfo = newBuildInfo}

instance MapDeps Benchmark where
  mapDeps ctx f bench = do
    newBuildInfo <- mapDeps ctx f (benchmarkBuildInfo bench)
    pure bench {benchmarkBuildInfo = newBuildInfo}

instance MapDeps [Cabal.Dependency] where
  mapDeps ctx f dep = f ctx (fromCabalDependencies dep) >>= toCabalDependencies regsitry
    where
      regsitry = Map.fromList $ map (\d -> (depName d, d)) dep

toCabalDependencies :: (Monad m) => Map Name Cabal.Dependency -> Dependencies -> m [Cabal.Dependency]
toCabalDependencies registry = mapM (toCabalDependency registry) . toDependencyList

toCabalDependency :: (Monad m) => Map Name Cabal.Dependency -> Dependency -> m Cabal.Dependency
toCabalDependency registry (Dependency name bounds) = do
  (Cabal.Dependency n _ c) <- maybe (error "Dependency not found in registry") pure (Map.lookup (format name) registry)
  pure (Cabal.Dependency n (toVersionRange bounds) c)

toVersionRange :: Bounds -> VersionRange
toVersionRange (Bounds (Just minB) (Just maxB)) = Cabal.intersectVersionRanges (toRange minB) (toRange maxB)
toVersionRange (Bounds (Just minB) Nothing) = toRange minB
toVersionRange (Bounds Nothing (Just maxB)) = toRange maxB
toVersionRange (Bounds Nothing Nothing) = Cabal.anyVersion

toRange :: Bound -> VersionRange
toRange (Bound Min inc version) = if inc then Cabal.orLaterVersion (toCabalVersion version) else Cabal.laterVersion (toCabalVersion version)
toRange (Bound Max inc version) = if inc then Cabal.orEarlierVersion (toCabalVersion version) else Cabal.earlierVersion (toCabalVersion version)

type DependencyIssue = (Text, PkgName, Bounds, Maybe Bounds)

collectDependencyIssues :: (Monad m, HasDependencies a) => (PkgName -> m (Maybe Bounds)) -> a -> m [DependencyIssue]
collectDependencyIssues f pkg = concat <$> traverse checkForDependencyIssues (collectDependencies [] pkg)
  where
    checkForDependencyIssues (path, deps) = concat <$> traverse (getIssue path) (toDependencyList deps)
    getIssue path dep = detectDependencyIssue path dep <$> f (hwmDepName dep)

reportDependencyIssues :: (MonadIssue m, Applicative m) => PkgSource -> [DependencyIssue] -> m ()
reportDependencyIssues source diffs = do
  unless (null diffs)
    $ injectIssue
      Issue
        { issueTopic = pkgSourceName source,
          issueMessage = show (length diffs) <> " dependency issue(s)",
          issueSeverity = SeverityWarning,
          issueDetails =
            Just
              DependencyIssue
                { issueDependencies = map (\(scope, dName, actual, expected) -> (scope, format dName, format actual, maybe "unknown" format expected)) diffs,
                  issueFile = pkgSourceFile source
                }
        }

detectDependencyIssue :: [Text] -> Dependency -> Maybe Bounds -> [DependencyIssue]
detectDependencyIssue path (Dependency dname depBounds) registryBounds = case registryBounds of
  Nothing -> [(scope, dname, depBounds, Nothing)]
  Just expected -> ([(scope, dname, depBounds, Just expected) | depBounds /= expected])
  where
    scope = T.intercalate ":" path

mkCabalDependency :: Dependency -> Cabal.Dependency
mkCabalDependency (Dependency (PkgName name) bounds) = Cabal.Dependency (Cabal.mkPackageName (T.unpack name)) (toVersionRange bounds) mainLibSet
