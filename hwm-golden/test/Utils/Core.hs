{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Core (assertNotModified, sanitizeAllCabals, diffChanges, trackChanges, copyLocalFiles, inWorkDir, diff, runHWM, saveSnapshot) where

import Control.Concurrent (threadDelay)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString as BS
import qualified Data.List as S
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import qualified GHC.IO.Exception as System.Exit
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, getCurrentDirectory, getModificationTime, listDirectory, makeAbsolute, removePathForcibly, setCurrentDirectory)
import System.Directory.Internal.Prelude (bracket)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.FilePath.Glob (glob)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callCommand, readCreateProcessWithExitCode, shell)
import Test.Hspec (Expectation, expectationFailure, shouldBe)

assertNotModified :: FilePath -> IO () -> Expectation
assertNotModified path action = do
  oldTime <- getModificationTime path
  threadDelay 1100000
  action
  newTime <- getModificationTime path
  newTime `shouldBe` oldTime

ignored :: [String]
ignored = [".hwm", ".stack-work", "dist-newstyle", "*.log"]

managed :: [String]
managed = [".cabal", ".yaml", ".nix", ".project"]

-- | Helper to find files HWM cares about (.cabal, .yaml, .nix, .project)
findManagedFiles :: FilePath -> IO [FilePath]
findManagedFiles dir = do
  contents <- listDirectory dir
  paths <- mapM (\path -> let p = dir </> path in (p,) <$> doesDirectoryExist p) contents
  let files = [p | (p, isDir) <- paths, not isDir, isManagedExtension p]
  subDirFiles <- concat <$> mapM (\(p, isDir) -> if isDir then findManagedFiles p else return []) paths
  return $ files ++ subDirFiles
  where
    isManagedExtension p = takeExtension p `elem` managed

copyLocalFiles :: FilePath -> IO ()
copyLocalFiles = copyDir "."

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing True dst
  callCommand $ "cp -r " <> src <> " " <> dst

copyFrom :: FilePath -> FilePath -> IO ()
copyFrom src = copyDir (src <> "/.")

inWorkDir :: FilePath -> FilePath -> IO a -> IO ()
inWorkDir project scenario m = do
  projectDir <- makeAbsolute ("test/projects/" </> project)
  overridesDir <- makeAbsolute (scenario </> "override")
  withSystemTempDirectory "hwm-golden" $ \tmpDir -> do
    let workDir = tmpDir </> "work"
    copyFrom projectDir workDir
    whenM (doesDirectoryExist overridesDir) $ copyFrom overridesDir workDir
    bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
      setCurrentDirectory workDir
      m $> ()

diff :: FilePath -> IO ()
diff expectedDir = do
  let ignoreFlags = S.unwords ["-x " ++ p | p <- ignored]
  (diffCode, diffOut, _) <-
    readCreateProcessWithExitCode
      (shell $ "diff -ruN " ++ ignoreFlags ++ " " ++ expectedDir ++ " .")
      ""
  unless (diffCode == System.Exit.ExitSuccess)
    $ expectationFailure
    $ "File diff failed:\n"
    <> diffOut

runHWM :: String -> IO String
runHWM cmd = do
  (exitCode, out, err) <- readCreateProcessWithExitCode (shell $ "hwm -- " <> cmd) ""
  unless (exitCode == System.Exit.ExitSuccess)
    $ expectationFailure ("Command failed with stdout: " <> out <> "stderr: " <> err)
  return out

data ChangeReport = ChangeReport
  { addedFiles :: [FilePath],
    deletedFiles :: [FilePath],
    modifiedFiles :: [FilePath]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

buildChangeReport :: [(FilePath, UTCTime)] -> [(FilePath, UTCTime)] -> ChangeReport
buildChangeReport oldState newState =
  let oldMap = Map.fromList oldState
      newMap = Map.fromList newState
      added = Map.keys $ Map.difference newMap oldMap
      deleted = Map.keys $ Map.difference oldMap newMap
      common = Map.intersectionWith (,) oldMap newMap
      modified = Map.keys $ Map.filter (uncurry (/=)) common
   in ChangeReport added deleted modified

trackChanges :: IO a -> IO (ChangeReport, a)
trackChanges action = do
  beforeFiles <- findManagedFiles "."
  oldTimes <- mapM (\p -> (p,) <$> getModificationTime p) beforeFiles
  threadDelay 1100000

  a <- action
  afterFiles <- findManagedFiles "."
  newTimes <- mapM (\p -> (p,) <$> getModificationTime p) afterFiles
  pure (buildChangeReport oldTimes newTimes, a)

saveSnapshot :: ChangeReport -> FilePath -> IO ()
saveSnapshot (ChangeReport added _ modified) dst = do
  removePathForcibly dst
  createDirectoryIfMissing True dst
  let filesToUpdate = added ++ modified
  forM_ filesToUpdate $ \f -> do
    let srcPath = f
    let dstPath = dst </> f
    createDirectoryIfMissing True (takeDirectory dstPath)
    copyFile srcPath dstPath

diffChanges :: FilePath -> ChangeReport -> IO ()
diffChanges expectedDir (ChangeReport added deleted modified) = do
  let filesToCompare = added ++ modified
  forM_ filesToCompare $ \f -> do
    let expectedFile = expectedDir </> f
    let actualFile = f
    expectedContent <- BS.readFile expectedFile
    actualContent <- BS.readFile actualFile
    when (expectedContent /= actualContent) $ do
      (_, diffOut, _) <-
        readCreateProcessWithExitCode
          (shell $ "diff -u " ++ expectedFile ++ " " ++ actualFile)
          ""
      expectationFailure $ "Content mismatch in " ++ f ++ ":\n" ++ diffOut
  forM_ deleted $ \f -> do
    exists <- doesPathExist f
    when exists
      $ expectationFailure
      $ "Idempotency failure: File should have been deleted but still exists: "
      ++ f

sanitizeCabal :: T.Text -> T.Text
sanitizeCabal =
  T.unlines
    . filter (not . T.isPrefixOf "-- This file has been generated")
    . T.lines

sanitizeAllCabals ::  IO ()
sanitizeAllCabals  = do
  cabalFiles <- glob "./**/*.cabal"
  forM_ cabalFiles $ \path -> do
    content <- TIO.readFile path
    let sanitized = sanitizeCabal content
    when (content /= sanitized) $ do
      TIO.writeFile path sanitized
      putStrLn $ "Sanitized: " <> path