{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Core (assertNotModified, trackChanges, copyLocalFiles, inWorkDir, diff, runHWM, saveSnapshot) where

import Control.Concurrent (threadDelay)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import qualified Data.List as S
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime)
import qualified GHC.IO.Exception as System.Exit
import Relude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, getModificationTime, listDirectory, makeAbsolute, removePathForcibly, setCurrentDirectory)
import System.Directory.Internal.Prelude (bracket)
import System.FilePath (takeExtension, (</>))
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

-- | Helper to find files HWM cares about (.cabal, .yaml, .nix, .project)
findManagedFiles :: FilePath -> IO [FilePath]
findManagedFiles dir = do
  contents <- listDirectory dir
  paths <- mapM (\path -> let p = dir </> path in (p,) <$> doesDirectoryExist p) contents
  let files = [p | (p, isDir) <- paths, not isDir, isManagedExtension p]
  subDirFiles <- concat <$> mapM (\(p, isDir) -> if isDir then findManagedFiles p else return []) paths
  return $ files ++ subDirFiles
  where
    isManagedExtension p = takeExtension p `elem` [".cabal", ".yaml", ".nix", ".project"]

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
  overridesDir <- makeAbsolute (scenario </> "overrides")
  withSystemTempDirectory "hwm-golden" $ \tmpDir -> do
    let workDir = tmpDir </> "work"
    copyFrom projectDir workDir
    whenM (doesDirectoryExist overridesDir) $ copyFrom overridesDir workDir
    bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
      setCurrentDirectory workDir
      m $> ()

diff :: FilePath -> [FilePath] -> IO ()
diff expectedDir ignoreList = do
  let ignoreFlags = S.unwords ["-x " ++ p | p <- ignoreList]
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
  (exitCode, out, err) <-
    readCreateProcessWithExitCode
      (shell $ "stack exec hwm -- " <> cmd)
      ""
  unless (exitCode == System.Exit.ExitSuccess)
    $ expectationFailure ("Command failed with stdout: " <> out <> "stderr: " <> err)
  return out

saveSnapshot :: FilePath -> IO ()
saveSnapshot dst = do
  removePathForcibly dst
  copyLocalFiles dst

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