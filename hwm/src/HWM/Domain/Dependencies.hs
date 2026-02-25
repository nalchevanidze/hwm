{-# LANGUAGE FlexibleContexts #-}
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
    DependencyGraph (..),
    sortByDependencyHierarchy,
    singleDeps,
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
import HWM.Core.Formatting (Format (..), formatTable, subPathSign)
import HWM.Core.Parsing (Parse (..), firstWord)
import HWM.Core.Pkg (Pkg (..), PkgName)
import HWM.Core.Result (Issue (..), Severity (..))
import HWM.Domain.Bounds (Bounds, boundsBetter, hasBounds)
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

newtype DependencyGraph = DependencyGraph (Map PkgName [PkgName])

instance Format DependencyGraph where
  format graph = T.intercalate "\n" (map (formatTree 0) (toTree graph))

formatTree :: Int -> Tree -> Text
formatTree depth (Node pkg deps) = newLine <> format pkg <> children
  where
    newLine | depth == 0 = "\n    • " | otherwise = "\n  " <> T.replicate depth "  " <> subPathSign
    children = T.intercalate "" (map (formatTree (depth + 1)) deps)

data Tree = Node PkgName [Tree]

toTree :: DependencyGraph -> [Tree]
toTree (DependencyGraph graph) =
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

topologicalSort :: DependencyGraph -> Either [PkgName] [PkgName]
topologicalSort (DependencyGraph graph) = goFunc [] initialZero indegreeMap
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

sortByDependencyHierarchy :: (MonadError Issue m) => DependencyGraph -> [Pkg] -> m [Pkg]
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
