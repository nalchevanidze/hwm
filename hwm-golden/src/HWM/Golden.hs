{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden (goldenSpec) where

import Data.Aeson (Value (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import HWM.Golden.Core (ChangeReport (..), ExpectedFiles (..), diffChanges, inWorkDir, runHWM, sanitizeAllCabals, saveSnapshot)
import Relude
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (makeRelative, splitDirectories, takeFileName, (</>))
import qualified System.IO as IO
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, parallel, runIO, shouldBe)

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

isEmptyExpectedFiles :: ExpectedFiles -> Bool
isEmptyExpectedFiles ExpectedFiles {added, deleted, modified} = null added && null deleted && null modified

instance Yaml.ToJSON CaseExpect where
  toJSON CaseExpect {..} =
    object
      $ catMaybes
        [ if caseFailure then Just ("failure" .= caseFailure) else Nothing,
          case caseFiles of
            Just fs | not (isEmptyExpectedFiles fs) -> Just ("files" .= fs)
            _ -> Nothing,
          ("calls" .=) <$> caseCalls
        ]

data CaseFile = CaseFile
  { caseProject :: FilePath,
    caseCommand :: String,
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
      .:? "expect"
      <*> o
      .:? "name"
      <*> o
      .:? "notes"

instance Yaml.ToJSON CaseFile where
  toJSON CaseFile {..} =
    object
      $ [ "name" .= caseName,
          "project" .= caseProject,
          "command" .= caseCommand,
          "notes" .= caseNotes
        ]
      <> case caseExpect of
        Just e | not (isEmptyCaseExpect e) -> ["expect" .= e]
        _ -> []

data Scenario = Scenario
  { scenarioPath :: FilePath,
    scenarioDir :: FilePath,
    scenarioCasePath :: FilePath,
    scenarioCase :: CaseFile
  }

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

goldenCaseDirectoryTests :: Bool -> FilePath -> [Scenario] -> Spec
goldenCaseDirectoryTests updateMode prefix scenarios = renderTree (buildCaseTree prefix scenarios)
  where
    renderTree :: CaseTree -> Spec
    renderTree CaseTree {treeCases, treeChildren} = do
      forM_ (sortOn fst treeCases) $ \(label, meta) ->
        it label (goldenRun updateMode meta)
      forM_ (M.toAscList treeChildren) $ \(name, child) ->
        describe name (renderTree child)

isEmptyCaseExpect :: CaseExpect -> Bool
isEmptyCaseExpect CaseExpect {caseFailure, caseFiles, caseCalls} =
  not caseFailure
    && maybe True isEmptyExpectedFiles caseFiles
    && isNothing caseCalls

goldenRun :: Bool -> Scenario -> Expectation
goldenRun updateMode Scenario {scenarioDir, scenarioCase = CaseFile {..}, ..} = do
  let stdoutFile = scenarioDir </> "stdout.ansi"
  let expectedDir = scenarioDir </> "expected"
  inWorkDir caseProject scenarioDir $ do
    (changes, (isFailure, out)) <- runHWM caseCommand
    let actualFiles = files changes
    let actualCalls = calls changes
    sanitizeAllCabals
    if updateMode
      then do
        saveSnapshot changes expectedDir
        IO.writeFile stdoutFile out
        let expect = CaseExpect {caseFailure = isFailure, caseFiles = Just actualFiles, caseCalls = actualCalls}
        let caseFile = CaseFile caseProject caseCommand (Just expect) caseName caseNotes
        Yaml.encodeFile scenarioCasePath caseFile
      else do
        maybe False caseFailure caseExpect `shouldBe` isFailure
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
        forM_ (caseExpect >>= caseFiles) $ \v -> actualFiles `shouldBe` v
        forM_ (caseExpect >>= caseCalls) $ \v -> actualCalls `shouldBe` Just v
        diffChanges expectedDir changes

goldenSpec :: Spec
goldenSpec = do
  updateMode <- runIO ((== Just "1") <$> lookupEnv "GOLDEN_UPDATE")
  (scenarioMap, loadErrors) <- runIO discoverScenarioMap
  unless (null loadErrors) $ runIO (expectationFailure (toString (T.intercalate "\n" loadErrors)))

  invalidLeafDirs <- runIO findInvalidLeafDirectories
  unless (null invalidLeafDirs) $ runIO (expectationFailure ("Invalid leaf directories without case.yaml: " <> show invalidLeafDirs))

  let commandsWithScenarios = filter (not . null . snd) (M.toAscList scenarioMap)

  parallel $ forM_ commandsWithScenarios $ \(root, metas) -> describe (toString root) (goldenCaseDirectoryTests updateMode root metas)
