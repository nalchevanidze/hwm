{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Assertions
  ( diff,
    diffChanges,
    saveSnapshot,
  )
where

import qualified Data.ByteString as BS
import HWM.Golden.Types (ChangeReport (..), ExpectedFiles (..))
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import qualified GHC.IO.Exception as System.Exit
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec (expectationFailure)

ignored :: [String]
ignored = [".hwm", ".stack-work", "dist-newstyle", "*.log"]

diff :: FilePath -> IO ()
diff expectedDir = do
  let args = ["-ruN"] <> concatMap (\p -> ["-x", p]) ignored <> [expectedDir, "."]
  (diffCode, diffOut, _) <- readCreateProcessWithExitCode (proc "diff" args) ""
  unless (diffCode == System.Exit.ExitSuccess)
    $ expectationFailure
    $ "File diff failed:\n"
    <> diffOut

saveSnapshot :: ChangeReport -> FilePath -> IO ()
saveSnapshot (ChangeReport (ExpectedFiles {added, modified}) _) dst = do
  whenM (doesDirectoryExist dst) $ removePathForcibly dst
  let filesToUpdate = added ++ modified
  unless (null filesToUpdate) $ do
    createDirectoryIfMissing True dst
    forM_ filesToUpdate $ \f -> do
      let srcPath = f
      let dstPath = dst </> f
      createDirectoryIfMissing True (takeDirectory dstPath)
      copyFile srcPath dstPath

diffChanges :: FilePath -> ChangeReport -> IO ()
diffChanges expectedDir (ChangeReport (ExpectedFiles {added, deleted, modified}) _) = do
  let filesToCompare = added ++ modified
  forM_ filesToCompare $ \f -> do
    let expectedFile = expectedDir </> f
    let actualFile = f
    expectedContent <- BS.readFile expectedFile
    actualContent <- BS.readFile actualFile
    when (expectedContent /= actualContent) $ do
      (_, diffOut, _) <- readCreateProcessWithExitCode (proc "diff" ["-u", expectedFile, actualFile]) ""
      expectationFailure $ "Content mismatch in " ++ f ++ ":\n" ++ diffOut
  forM_ deleted $ \f -> do
    exists <- doesPathExist f
    when exists
      $ expectationFailure
      $ "Idempotency failure: File should have been deleted but still exists: "
      ++ f
