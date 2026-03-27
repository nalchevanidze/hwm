{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Filesystem
  ( copyLocalFiles,
    inWorkDir,
    sanitizeAllCabals,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HWM.Golden.Types (CaseRunner (..))
import Relude
import System.Directory (Permissions (..), copyFile, createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, getPermissions, listDirectory, makeAbsolute, removePathForcibly, setCurrentDirectory, setPermissions)
import System.Directory.Internal.Prelude (bracket)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.IO.Temp (withSystemTempDirectory)

copyLocalFiles :: FilePath -> IO ()
copyLocalFiles = copyDir "."

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \entry -> do
    let from = src </> entry
    let to = dst </> entry
    isDir <- doesDirectoryExist from
    if isDir
      then copyDir from to
      else copyFile from to

copyFrom :: FilePath -> FilePath -> IO ()
copyFrom src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \entry -> copyDirOrFile (src </> entry) (dst </> entry)
  let hwmDir = dst </> ".hwm"
  whenM (doesDirectoryExist hwmDir) $ removePathForcibly hwmDir
  where
    copyDirOrFile from to = do
      isDir <- doesDirectoryExist from
      if isDir
        then copyDir from to
        else copyFile from to

inWorkDir :: FilePath -> FilePath -> Maybe CaseRunner -> IO a -> IO ()
inWorkDir project scenario caseRunner m = do
  repoRoot <- getCurrentDirectory
  projectDir <- makeAbsolute ("test/projects/" </> project)
  overridesDir <- makeAbsolute (scenario </> "override")
  withSystemTempDirectory "hwm-golden" $ \tmpDir -> do
    let workDir = tmpDir </> "work"
    copyFrom projectDir workDir
    whenM (doesDirectoryExist overridesDir) $ copyFrom overridesDir workDir
    installRunnerBins repoRoot workDir caseRunner
    bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
      setCurrentDirectory workDir
      m $> ()

installRunnerBins :: FilePath -> FilePath -> Maybe CaseRunner -> IO ()
installRunnerBins repoRoot workDir mRunner = do
  let bins = fromMaybe Map.empty (mRunner >>= runnerBin)
  unless (Map.null bins) $ do
    let workBinDir = workDir </> "bin"
    createDirectoryIfMissing True workBinDir
    forM_ (Map.toList bins) $ \(name, srcRel) -> do
      srcAbs <- makeAbsolute (repoRoot </> srcRel)
      let dst = workBinDir </> name
      copyFile srcAbs dst
      perms <- getPermissions dst
      setPermissions dst perms {executable = True}

sanitizeCabal :: T.Text -> T.Text
sanitizeCabal =
  T.unlines
    . filter (not . T.isPrefixOf "-- This file has been generated")
    . T.lines

sanitizeAllCabals :: IO ()
sanitizeAllCabals = do
  cabalFiles <- glob "./**/*.cabal"
  forM_ cabalFiles $ \path -> do
    content <- TIO.readFile path
    let sanitized = sanitizeCabal content
    when (content /= sanitized) $ TIO.writeFile path sanitized
