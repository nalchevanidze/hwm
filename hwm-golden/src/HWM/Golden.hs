{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden (goldenSpec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import HWM.Golden.Core (ChangeReport (..), diffChanges, inWorkDir, runHWM, sanitizeAllCabals, saveSnapshot)
import HWM.Golden.Scanning (CaseExpect (..), CaseFile (..), Scenario (..), ScenarioTree (..), discoverGolden)
import Relude
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, parallel, runIO, shouldBe)

goldenSpec :: Spec
goldenSpec = do
  updateMode <- runIO isUpdateMode
  discovered <- runIO discoverGolden
  case discovered of
    Left errs -> runIO (expectationFailure (toString (T.intercalate "\n" errs)))
    Right scenarioTree -> parallel (runScenarioTree updateMode scenarioTree)

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"

runScenarioTree :: Bool -> ScenarioTree -> Spec
runScenarioTree updateMode ScenarioTree {treeCases, treeChildren} = do
  forM_ treeCases $ \(label, scenario) ->
    it label (runScenario updateMode scenario)
  forM_ treeChildren $ \(name, child) ->
    describe name (runScenarioTree updateMode child)

runScenario :: Bool -> Scenario -> Expectation
runScenario updateMode Scenario {scenarioDir, scenarioCasePath, scenarioCase = CaseFile {..}} = do
  let stdoutFile = scenarioDir </> "stdout.ansi"
  let expectedDir = scenarioDir </> "expected"

  inWorkDir caseProject scenarioDir $ do
    (changes, (isFailure, out)) <- runHWM (fromMaybe Map.empty caseEnv) caseCommand
    sanitizeAllCabals

    if updateMode
      then do
        saveSnapshot changes expectedDir
        IO.writeFile stdoutFile out
        let expect = CaseExpect {caseFailure = isFailure, caseFiles = Just (files changes), caseCalls = calls changes}
        let nextCase = CaseFile caseProject caseCommand caseEnv (Just expect) caseName caseNotes
        Yaml.encodeFile scenarioCasePath nextCase
      else do
        maybe False caseFailure caseExpect `shouldBe` isFailure
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
        forM_ (caseExpect >>= caseFiles) $ \v -> files changes `shouldBe` v
        forM_ (caseExpect >>= caseCalls) $ \v -> calls changes `shouldBe` Just v
        diffChanges expectedDir changes
