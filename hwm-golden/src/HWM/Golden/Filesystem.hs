{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Filesystem
  ( copyLocalFiles,
    inWorkDir,
    sanitizeAllCabals,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, listDirectory, makeAbsolute, removePathForcibly, setCurrentDirectory)
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
