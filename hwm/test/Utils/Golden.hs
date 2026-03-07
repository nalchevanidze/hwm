{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Golden (Golden (..), goldenTest) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Relude
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec (Expectation, shouldBe)
import Utils.Core (diff, inWorkDir, runHWM, saveSnapshot, trackChanges)

data Golden = Golden
  { cmd :: String,
    scenario :: FilePath,
    project :: FilePath
  }

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"

goldenTest :: Golden -> Expectation
goldenTest Golden {..} = do
  scenarioDir <- makeAbsolute $ "test/golden/" </> scenario
  let expectedDir = scenarioDir </> "expected"
  let stdoutFile = scenarioDir </> "stdout.ansi"
  let deltaFile = scenarioDir </> "delta.json"
  updateMode <- isUpdateMode
  inWorkDir project scenarioDir $ do
    (changes, out) <- trackChanges (runHWM cmd)
    if updateMode
      then do
        saveSnapshot expectedDir
        IO.writeFile stdoutFile out
        LBS.writeFile deltaFile (encode changes)
      else do
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
        expectedDelta <- decode <$> LBS.readFile deltaFile
        expectedDelta `shouldBe` Just changes
        diff expectedDir