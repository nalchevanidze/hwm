{-# LANGUAGE NamedFieldPuns #-}
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
import HWM.Golden.Types (CaseRunner (..), RunnerBin (..), RunnerBinTrace (..))
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

    forM_ (Map.toList bins) $ \(name, RunnerBin {runnerBinSrc, runnerBinTrace}) -> do
      srcAbs <- makeAbsolute (repoRoot </> runnerBinSrc)
      let dst = workBinDir </> name
      writeFileText dst (renderTraceWrapper name srcAbs runnerBinTrace)
      perms <- getPermissions dst
      setPermissions dst perms {executable = True}

renderTraceWrapper :: String -> FilePath -> Maybe RunnerBinTrace -> Text
renderTraceWrapper tool src mTrace =
  T.unlines
    $ ["#!/bin/sh", "set -eu", "log=\"${HWM_GOLDEN_INVOCATIONS:-.hwm/invocations.yaml}\"", "mkdir -p \"$(dirname \"$log\")\"", "if [ ! -f \"$log\" ] || [ ! -s \"$log\" ]; then", "  : > \"$log\"", "fi", "", "printf -- \"- tool: %s\\n\" " <> shSingleQuoted tool <> " >> \"$log\"", "printf -- \"  args:\\n\" >> \"$log\"", "for a in \"$@\"; do", "  printf -- \"    - %s\\n\" \"$a\" >> \"$log\"", "done"]
      <> renderTraceExtras (fromMaybe (RunnerBinTrace [] []) mTrace)
      <> ["", "exec " <> shSingleQuoted src <> " \"$@\""]

renderTraceExtras :: RunnerBinTrace -> [Text]
renderTraceExtras RunnerBinTrace {runnerBinTraceEnv, runnerBinTraceFiles} =
  renderEnvTrace runnerBinTraceEnv <> renderFileTrace runnerBinTraceFiles

renderEnvTrace :: [String] -> [Text]
renderEnvTrace [] = []
renderEnvTrace vars =
  ["env_written=0"]
    <> concatMap renderVar vars
  where
    renderVar v =
      [ "val=\"${" <> toText v <> ":-}\"",
        "if [ -n \"$val\" ]; then",
        "  if [ \"$env_written\" -eq 0 ]; then",
        "    printf -- \"  env:\\n\" >> \"$log\"",
        "    env_written=1",
        "  fi",
        "  val_rel=\"$val\"",
        "  case \"$val_rel\" in",
        "    \"$PWD\"/*) val_rel=${val_rel#\"$PWD/\"} ;;",
        "  esac",
        "  printf -- \"    " <> toText v <> ": %s\\n\" \"$val_rel\" >> \"$log\"",
        "fi"
      ]

renderFileTrace :: [FilePath] -> [Text]
renderFileTrace [] = []
renderFileTrace files =
  ["files_written=0"]
    <> concatMap renderFile files
  where
    renderFile fileTpl =
      [ "file_tpl=" <> shSingleQuoted fileTpl,
        "resolved_file=$(eval \"printf '%s' \\\"$file_tpl\\\"\")",
        "if [ -n \"$resolved_file\" ] && [ -f \"$resolved_file\" ]; then",
        "  if [ \"$files_written\" -eq 0 ]; then",
        "    printf -- \"  files:\\n\" >> \"$log\"",
        "    files_written=1",
        "  fi",
        "  rel_file=\"$resolved_file\"",
        "  case \"$rel_file\" in",
        "    \"$PWD\"/*) rel_file=${rel_file#\"$PWD/\"} ;;",
        "  esac",
        "  printf -- \"    %s: |\\n\" \"$rel_file\" >> \"$log\"",
        "  cat \"$resolved_file\" | sed 's/^/      /' >> \"$log\"",
        "fi"
      ]

shSingleQuoted :: String -> Text
shSingleQuoted s = "'" <> T.replace "'" "'\"'\"'" (toText s) <> "'"

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
