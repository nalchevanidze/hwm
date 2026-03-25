{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden (Golden (..), goldenTest, goldenFailTest) where

import qualified Data.Yaml as Yaml
import HWM.Golden.Core (cleanupEmptyDeltaFiles, diffChanges, hasNoChanges, inWorkDir, runHWM, runHWMFail, sanitizeAllCabals, saveSnapshot, trackChanges)
import Relude
import System.Directory (doesFileExist, makeAbsolute, removeFile)
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec (Expectation, expectationFailure, shouldBe)

data Golden = Golden
  { cmd :: String,
    scenario :: FilePath,
    project :: FilePath
  }

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"

goldenTest :: Golden -> Expectation
goldenTest = goldenRunWith runHWM

goldenFailTest :: Golden -> Expectation
goldenFailTest = goldenRunWith runHWMFail

goldenRunWith :: (String -> IO String) -> Golden -> Expectation
goldenRunWith runFn Golden {..} = do
  goldenRoot <- makeAbsolute "test/golden"
  scenarioDir <- makeAbsolute $ "test/golden/" </> scenario
  let expectedDir = scenarioDir </> "expected"
  let stdoutFile = scenarioDir </> "stdout.ansi"
  let deltaFile = scenarioDir </> "delta.yaml"
  updateMode <- isUpdateMode
  inWorkDir project scenarioDir $ do
    (changes, out) <- trackChanges (runFn cmd)
    sanitizeAllCabals
    if updateMode
      then do
        saveSnapshot changes expectedDir
        IO.writeFile stdoutFile out
        if hasNoChanges changes
          then do
            hasDelta <- doesFileExist deltaFile
            when hasDelta (removeFile deltaFile)
          else Yaml.encodeFile deltaFile changes
        cleanupEmptyDeltaFiles goldenRoot
      else do
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
        hasDelta <- doesFileExist deltaFile
        if hasDelta
          then do
            expectedDelta <- Yaml.decodeFileEither deltaFile
            case expectedDelta of
              Right delta -> changes `shouldBe` delta
              Left parseErr -> expectationFailure ("Failed to parse delta.yaml: " <> show parseErr)
          else
            unless (hasNoChanges changes)
              $ expectationFailure
              $ "Missing delta.yaml for non-empty changes: "
              <> show changes
        diffChanges expectedDir changes
