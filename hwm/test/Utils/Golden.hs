{-# LANGUAGE RecordWildCards #-}

module Utils.Golden (Golden (..), goldenTest) where

import System.Directory (makeAbsolute, removePathForcibly)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec (Expectation, shouldBe)
import Utils.Core (copyDir, diff, inWorkDir, runHWM)

data Golden = Golden
  { cmd :: String,
    scenario :: String
  }

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"

goldenTest :: Golden -> Expectation
goldenTest Golden {..} = do
  scenarioDir <- makeAbsolute $ "test/golden/" </> scenario
  let expectedDir = scenarioDir </> "expected"
  let stdoutFile = scenarioDir </> "stdout.ansi"
  updateMode <- isUpdateMode
  inWorkDir scenarioDir $ do
    stdout <- runHWM cmd
    if updateMode
      then do
        removePathForcibly expectedDir
        copyDir "." expectedDir
        writeFile stdoutFile stdout
      else do
        diff expectedDir [".hwm", ".stack-work", "dist-newstyle", "*.log"]
        expectedStdout <- readFile stdoutFile
        stdout `shouldBe` expectedStdout
