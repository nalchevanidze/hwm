{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden (goldenSpec) where

import HWM.Golden.Assertions (diffChanges, saveSnapshot)
import HWM.Golden.CaseYaml (writeCaseFileOrdered)
import HWM.Golden.Exec (isUpdateMode, runHWM)
import HWM.Golden.Filesystem (inWorkDir, sanitizeAllCabals)
import HWM.Golden.Scanning (Scenario (..), ScenarioTree (..), discoverGolden)
import HWM.Golden.Types (CaseExpect (..), CaseFile (..), ChangeReport (..))
import Relude
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec (Expectation, Spec, describe, it, parallel, runIO, shouldBe)

goldenSpec :: Spec
goldenSpec = do
  updateMode <- runIO isUpdateMode
  scenarioTree <- runIO discoverGolden
  parallel (runScenarioTree updateMode scenarioTree)

runScenarioTree :: Bool -> ScenarioTree -> Spec
runScenarioTree updateMode ScenarioTree {treeCases, treeChildren} = do
  forM_ treeCases $ \(label, scenario) -> it label (runScenario updateMode scenario)
  forM_ treeChildren $ \(name, child) -> describe name (runScenarioTree updateMode child)

runScenario :: Bool -> Scenario -> Expectation
runScenario updateMode Scenario {scenarioDir, scenarioCasePath, scenarioCase = CaseFile {..}} = do
  let stdoutFile = scenarioDir </> "stdout.ansi"
  let expectedDir = scenarioDir </> "expected"

  inWorkDir caseProject scenarioDir caseRunner $ do
    (changes, (isFailure, out)) <- runHWM caseRunner caseCommand
    sanitizeAllCabals

    if updateMode
      then do
        saveSnapshot changes expectedDir
        IO.writeFile stdoutFile out
        let expect = CaseExpect {caseFailure = isFailure, caseFiles = Just (files changes), caseCalls = calls changes}
        let nextCase =
              CaseFile
                { caseProject = caseProject,
                  caseCommand = caseCommand,
                  caseRunner = caseRunner,
                  caseExpect = Just expect,
                  caseName = caseName,
                  caseNotes = caseNotes
                }
        writeCaseFileOrdered scenarioCasePath nextCase
      else do
        maybe False caseFailure caseExpect `shouldBe` isFailure
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
        forM_ (caseExpect >>= caseFiles) $ \v -> files changes `shouldBe` v
        forM_ (caseExpect >>= caseCalls) $ \v -> calls changes `shouldBe` Just v
        diffChanges expectedDir changes
