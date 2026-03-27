{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Scanning
  ( Scenario (..),
    ScenarioTree (..),
    discoverGolden,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import HWM.Golden.Types (CaseFile (..))
import Relude
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (makeRelative, splitDirectories, takeFileName, (</>))
import Test.Hspec (expectationFailure)

data Scenario = Scenario
  { scenarioPath :: FilePath,
    scenarioDir :: FilePath,
    scenarioCasePath :: FilePath,
    scenarioCase :: CaseFile
  }

data ScenarioTree = ScenarioTree
  { treeCases :: [(String, Scenario)],
    treeChildren :: [(String, ScenarioTree)]
  }

data WorkingTree = WorkingTree
  { workCases :: [(String, Scenario)],
    workChildren :: Map.Map String WorkingTree
  }

emptyWorkingTree :: WorkingTree
emptyWorkingTree = WorkingTree {workCases = [], workChildren = Map.empty}

insertWorkingTree :: [String] -> (String, Scenario) -> WorkingTree -> WorkingTree
insertWorkingTree [] scenario tree = tree {workCases = scenario : workCases tree}
insertWorkingTree (seg : rest) scenario tree =
  let child = fromMaybe emptyWorkingTree (Map.lookup seg (workChildren tree))
      child' = insertWorkingTree rest scenario child
   in tree {workChildren = Map.insert seg child' (workChildren tree)}

toScenarioTree :: WorkingTree -> ScenarioTree
toScenarioTree WorkingTree {workCases, workChildren} =
  ScenarioTree
    { treeCases = sortOn fst workCases,
      treeChildren = map (second toScenarioTree) (Map.toAscList workChildren)
    }

buildScenarioTree :: FilePath -> [Scenario] -> ScenarioTree
buildScenarioTree prefix scenarios =
  toScenarioTree
    $ foldl'
      ( \acc meta@Scenario {scenarioPath, scenarioCase = CaseFile {caseName}} ->
          let rel = makeRelative prefix scenarioPath
              segments = filter (/= ".") (splitDirectories rel)
              (dirs, fallbackLabel) = case reverse segments of
                [] -> ([], prefix)
                l : revDirs -> (reverse revDirs, l)
              label = maybe fallbackLabel toString caseName
           in insertWorkingTree dirs (label, meta) acc
      )
      emptyWorkingTree
      scenarios

goldenRoot :: FilePath
goldenRoot = "test/golden"

discoverGoldenCases :: FilePath -> IO [FilePath]
discoverGoldenCases prefix = do
  let startDir = goldenRoot </> prefix
  exists <- doesDirectoryExist startDir
  unless exists $ expectationFailure ("Missing golden scenarios directory: " <> startDir)
  sort <$> walk goldenRoot startDir
  where
    walk :: FilePath -> FilePath -> IO [FilePath]
    walk rootDir dir = do
      hasCase <- doesFileExist (dir </> "case.yaml")
      entries <- listDirectory dir
      nested <-
        fmap concat . forM (sort entries) $ \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir then walk rootDir path else pure []
      pure ([makeRelative rootDir dir | hasCase] <> nested)

loadScenario :: FilePath -> IO (Either [Text] Scenario)
loadScenario relScenarioPath = do
  absScenarioDir <- makeAbsolute (goldenRoot </> relScenarioPath)
  let casePath = absScenarioDir </> "case.yaml"
  decoded <- Yaml.decodeFileEither casePath
  case decoded of
    Left err -> pure (Left ["Invalid YAML in " <> toText casePath <> ": " <> toText (show err :: String)])
    Right (Object root) ->
      let parsed = parseEither Yaml.parseJSON (Object root) :: Either String CaseFile
       in case parsed of
            Left parseErr -> pure (Left ["Invalid schema in " <> toText casePath <> ": " <> toText parseErr])
            Right caseFile ->
              pure
                ( Right
                    Scenario
                      { scenarioPath = relScenarioPath,
                        scenarioDir = absScenarioDir,
                        scenarioCasePath = casePath,
                        scenarioCase = caseFile
                      }
                )
    Right _ -> pure (Left ["case.yaml root must be an object: " <> toText casePath])

discoverScenarioTrees :: IO ([(FilePath, ScenarioTree)], [Text])
discoverScenarioTrees = do
  entries <- listDirectory goldenRoot
  commands <- sort <$> filterM (doesDirectoryExist . (goldenRoot </>)) entries
  triples <- forM commands $ \command -> do
    paths <- discoverGoldenCases command
    loaded <- mapM loadScenario paths
    let errors = concatMap (fromLeft []) loaded
    let scenarios = rights loaded
    pure (command, scenarios, errors)
  let scenarioGroups = [(cmd, buildScenarioTree cmd scenarios) | (cmd, scenarios, _) <- triples, not (null scenarios)]
  let allErrors = concatMap (\(_, _, errs) -> errs) triples
  pure (scenarioGroups, allErrors)

findInvalidLeafDirectories :: IO [FilePath]
findInvalidLeafDirectories = walk goldenRoot False
  where
    walk :: FilePath -> Bool -> IO [FilePath]
    walk dir inSupportTree = do
      entries <- listDirectory dir
      let childDirs = sort [dir </> e | e <- entries]
      dirs <- filterM doesDirectoryExist childDirs
      let supportHere = inSupportTree || takeFileName dir `elem` ["expected", "override"]
      nested <- fmap concat (mapM (`walk` supportHere) dirs)
      let hasCase = "case.yaml" `elem` entries
      let isLeaf = null dirs
      let isEmptyLeaf = isLeaf && null entries
      let isInvalid = isLeaf && not isEmptyLeaf && not supportHere && not hasCase
      pure (([dir | isInvalid]) <> nested)

discoverGolden :: IO ScenarioTree
discoverGolden = do
  (scenarioTrees, loadErrors) <- discoverScenarioTrees
  invalidLeafDirs <- findInvalidLeafDirectories
  let invalidLeafErrors =
        ["Invalid leaf directories without case.yaml: " <> toText (show invalidLeafDirs :: String) | not (null invalidLeafDirs)]
  let errors = loadErrors <> invalidLeafErrors
  unless (null errors) $ expectationFailure (toString (T.intercalate "\n" errors))
  pure ScenarioTree {treeCases = [], treeChildren = scenarioTrees}
