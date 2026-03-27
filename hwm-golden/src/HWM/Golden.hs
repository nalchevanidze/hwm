{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden (goldenSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import HWM.Golden.Core (ChangeReport (..), diffChanges, inWorkDir, runHWM, sanitizeAllCabals, saveSnapshot)
import HWM.Golden.Scanning (CaseExpect (..), CaseFile (..), CaseTree (..), Scenario (..), buildCaseTree, discoverGolden)
import Relude
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, parallel, runIO, shouldBe)

goldenCaseDirectoryTests :: Bool -> FilePath -> [Scenario] -> Spec
goldenCaseDirectoryTests updateMode prefix scenarios = renderTree (buildCaseTree prefix scenarios)
  where
    renderTree :: CaseTree -> Spec
    renderTree CaseTree {treeCases, treeChildren} = do
      forM_ (sortOn fst treeCases) $ \(label, meta) ->
        it label (goldenRun updateMode meta)
      forM_ (M.toAscList treeChildren) $ \(name, child) ->
        describe name (renderTree child)

goldenRun :: Bool -> Scenario -> Expectation
goldenRun updateMode Scenario {scenarioDir, scenarioCase = CaseFile {..}, ..} = do
  let stdoutFile = scenarioDir </> "stdout.ansi"
  let expectedDir = scenarioDir </> "expected"
  inWorkDir caseProject scenarioDir $ do
    (changes, (isFailure, out)) <- runHWM (fromMaybe M.empty caseEnv) caseCommand
    let actualFiles = files changes
    let actualCalls = calls changes
    sanitizeAllCabals
    if updateMode
      then do
        saveSnapshot changes expectedDir
        IO.writeFile stdoutFile out
        let expect = CaseExpect {caseFailure = isFailure, caseFiles = Just actualFiles, caseCalls = actualCalls}
        let caseFile = CaseFile caseProject caseCommand caseEnv (Just expect) caseName caseNotes
        Yaml.encodeFile scenarioCasePath caseFile
      else do
        maybe False caseFailure caseExpect `shouldBe` isFailure
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
        forM_ (caseExpect >>= caseFiles) $ \v -> actualFiles `shouldBe` v
        forM_ (caseExpect >>= caseCalls) $ \v -> actualCalls `shouldBe` Just v
        diffChanges expectedDir changes

runGolden :: Bool -> M.Map FilePath [Scenario] -> Spec
runGolden updateMode scenarios = do
  let commandsWithScenarios = filter (not . null . snd) (M.toAscList scenarios)
  parallel $ forM_ commandsWithScenarios $ \(root, metas) -> describe (toString root) (goldenCaseDirectoryTests updateMode root metas)

goldenSpec :: Spec
goldenSpec = do
  updateMode <- runIO ((== Just "1") <$> lookupEnv "GOLDEN_UPDATE")
  discovery <- runIO discoverGolden
  case discovery of
    Left errs -> runIO (expectationFailure (toString (T.intercalate "\n" errs)))
    Right loaded -> runGolden updateMode loaded
