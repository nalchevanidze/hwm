{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Golden (Golden (..), goldenTest) where

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
  updateMode <- isUpdateMode
  inWorkDir project scenarioDir $ do
    (changes, out) <- trackChanges (runHWM cmd)
    print changes
    if updateMode
      then do
        saveSnapshot expectedDir
        IO.writeFile stdoutFile out
      else do
        diff expectedDir [".hwm", ".stack-work", "dist-newstyle", "*.log"]
        expectedStdout <- IO.readFile stdoutFile
        out `shouldBe` expectedStdout
