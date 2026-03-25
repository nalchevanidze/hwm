{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden (Golden (..), goldenTest, goldenFailTest) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
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
  let deltaFile = scenarioDir </> "delta.json"
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
          else LBS.writeFile deltaFile (encode changes)
        cleanupEmptyDeltaFiles goldenRoot
      else do
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
        hasDelta <- doesFileExist deltaFile
        if hasDelta
          then do
            expectedDelta <- decode <$> LBS.readFile deltaFile
            Just changes `shouldBe` expectedDelta
          else
            unless (hasNoChanges changes)
              $ expectationFailure
              $ "Missing delta.json for non-empty changes: "
              <> show changes
        diffChanges expectedDir changes
