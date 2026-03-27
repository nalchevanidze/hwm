{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Scanning
  ( CaseExpect (..),
    CaseFile (..),
    Scenario (..),
    CaseTree (..),
    buildCaseTree,
    discoverGolden,
  )
where

import Data.Aeson (Value (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as M
import qualified Data.Yaml as Yaml
import HWM.Golden.Core (ExpectedFiles (..), dropEmpty)
import Relude
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (makeRelative, splitDirectories, takeFileName, (</>))
import Test.Hspec (expectationFailure)

data CaseExpect = CaseExpect
  { caseFailure :: Bool,
    caseFiles :: Maybe ExpectedFiles,
    caseCalls :: Maybe Value
  }

instance Yaml.FromJSON CaseExpect where
  parseJSON = withObject "CaseExpect" $ \o -> do
    failure <- o .:? "failure"
    files <- o .:? "files"
    calls <- o .:? "calls"
    pure CaseExpect {caseFailure = fromMaybe False failure, caseFiles = files, caseCalls = calls}

instance Yaml.ToJSON CaseExpect where
  toJSON CaseExpect {..} =
    dropEmpty $
      object
        [ "failure" .= (if caseFailure then Just True else Nothing :: Maybe Bool),
          "files" .= caseFiles,
          "calls" .= caseCalls
        ]

data CaseFile = CaseFile
  { caseProject :: FilePath,
    caseCommand :: String,
    caseEnv :: Maybe (M.Map String String),
    caseExpect :: Maybe CaseExpect,
    caseName :: Maybe Text,
    caseNotes :: Maybe Text
  }

instance Yaml.FromJSON CaseFile where
  parseJSON = withObject "CaseFile" $ \o ->
    CaseFile
      <$> o
      .: "project"
      <*> o
      .: "command"
      <*> o
      .:? "env"
      <*> o
      .:? "expect"
      <*> o
      .:? "name"
      <*> o
      .:? "notes"

instance Yaml.ToJSON CaseFile where
  toJSON CaseFile {..} =
    dropEmpty $
      object
        [ "project" .= caseProject,
          "command" .= caseCommand,
          "env" .= caseEnv,
          "name" .= caseName,
          "notes" .= caseNotes,
          "expect" .= caseExpect
        ]

data Scenario = Scenario
  { scenarioPath :: FilePath,
    scenarioDir :: FilePath,
    scenarioCasePath :: FilePath,
    scenarioCase :: CaseFile
  }

data CaseTree = CaseTree
  { treeCases :: [(String, Scenario)],
    treeChildren :: M.Map String CaseTree
  }

emptyCaseTree :: CaseTree
emptyCaseTree = CaseTree {treeCases = [], treeChildren = M.empty}

insertCaseTree :: [String] -> (String, Scenario) -> CaseTree -> CaseTree
insertCaseTree [] scenario tree = tree {treeCases = scenario : treeCases tree}
insertCaseTree (seg : rest) scenario tree =
  let child = fromMaybe emptyCaseTree (M.lookup seg (treeChildren tree))
      child' = insertCaseTree rest scenario child
   in tree {treeChildren = M.insert seg child' (treeChildren tree)}

buildCaseTree :: FilePath -> [Scenario] -> CaseTree
buildCaseTree prefix =
  foldl'
    ( \acc meta@Scenario {scenarioPath} ->
        let rel = makeRelative prefix scenarioPath
            segments = filter (/= ".") (splitDirectories rel)
            (dirs, label) = case reverse segments of
              [] -> ([], prefix)
              l : revDirs -> (reverse revDirs, l)
         in insertCaseTree dirs (label, meta) acc
    )
    emptyCaseTree

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

discoverScenarioMap :: IO (M.Map FilePath [Scenario], [Text])
discoverScenarioMap = do
  entries <- listDirectory goldenRoot
  commands <- sort <$> filterM (doesDirectoryExist . (goldenRoot </>)) entries
  triples <- forM commands $ \command -> do
    paths <- discoverGoldenCases command
    loaded <- mapM loadScenario paths
    let errors = concatMap (fromLeft []) loaded
    let metas = rights loaded
    pure (command, metas, errors)
  let scenarioMap = M.fromList [(cmd, metas) | (cmd, metas, _) <- triples]
  let allErrors = concatMap (\(_, _, errs) -> errs) triples
  pure (scenarioMap, allErrors)

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
      let isInvalid = isLeaf && not supportHere && not hasCase
      pure (([dir | isInvalid]) <> nested)

discoverGolden :: IO (Either [Text] (M.Map FilePath [Scenario]))
discoverGolden = do
  (scenarioMap, loadErrors) <- discoverScenarioMap
  invalidLeafDirs <- findInvalidLeafDirectories
  let invalidLeafErrors =
        if null invalidLeafDirs
          then []
          else ["Invalid leaf directories without case.yaml: " <> toText (show invalidLeafDirs :: String)]
  let errors = loadErrors <> invalidLeafErrors
  pure $ if null errors then Right scenarioMap else Left errors
