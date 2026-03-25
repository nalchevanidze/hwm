{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Core (assertNotModified, sanitizeAllCabals, diffChanges, trackChanges, hasNoChanges, cleanupEmptyDeltaFiles, copyLocalFiles, inWorkDir, diff, runHWM, runHWMFail, saveSnapshot) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.:?), (.=), object, withObject)
import Data.Aeson.Types ((.!=))
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as BS
import qualified Data.List as S
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import qualified GHC.IO.Exception as System.Exit
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, getCurrentDirectory, getModificationTime, listDirectory, makeAbsolute, removeFile, removePathForcibly, setCurrentDirectory)
import System.Directory.Internal.Prelude (bracket)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.FilePath.Glob (glob)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env), callCommand, readCreateProcessWithExitCode, shell)
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

ignoredDirs :: [FilePath]
ignoredDirs = [".hwm", ".stack-work", "dist-newstyle"]

managed :: [String]
managed = [".cabal", ".yaml", ".nix", ".project"]

ignoredManagedFiles :: [FilePath]
ignoredManagedFiles = ["invocations.yaml", "./invocations.yaml"]

-- | Helper to find files HWM cares about (.cabal, .yaml, .nix, .project)
findManagedFiles :: FilePath -> IO [FilePath]
findManagedFiles dir = do
  contents <- listDirectory dir
  paths <- mapM (\path -> let p = dir </> path in (p,) <$> doesDirectoryExist p) contents
  let files = [p | (p, isDir) <- paths, not isDir, isManagedExtension p, p `notElem` ignoredManagedFiles]
  subDirFiles <- concat <$> mapM collect paths
  return $ files ++ subDirFiles
  where
    isManagedExtension p = takeExtension p `elem` managed
    collect (p, isDir)
      | not isDir = pure []
      | takeFileName p `elem` ignoredDirs = pure []
      | otherwise = findManagedFiles p

copyLocalFiles :: FilePath -> IO ()
copyLocalFiles = copyDir "."

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing True dst
  callCommand $ "cp -r " <> src <> " " <> dst

copyFrom :: FilePath -> FilePath -> IO ()
copyFrom src dst = do
  copyDir (src <> "/.") dst
  let hwmDir = dst </> ".hwm"
  whenM (doesDirectoryExist hwmDir) $ removePathForcibly hwmDir

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

mkGoldenEnv :: IO [(String, String)]
mkGoldenEnv = do
  cwd <- getCurrentDirectory
  current <- getEnvironment
  let oldPath = fromMaybe "" (S.lookup "PATH" current)
  let home = ".home"
  let localBin = home </> ".local" </> "bin"
  let pathValue = S.intercalate ":" [cwd </> "bin", localBin, oldPath]
  let keep (k, _) = k /= "PATH" && k /= "HOME"
  pure $ [ ("PATH", pathValue),
           ("HOME", home),
           ("HWM_LOG_ID_FIXED", "golden"),
           ("CI", "1")
         ]
    <> filter keep current

runHWM :: String -> IO String
runHWM cmd = do
  envVars <- mkGoldenEnv
  (exitCode, out, err) <- readCreateProcessWithExitCode ((shell $ "hwm -- " <> cmd) {env = Just envVars}) ""
  unless (exitCode == System.Exit.ExitSuccess)
    $ expectationFailure ("Command failed with stdout: " <> out <> "stderr: " <> err)
  return out

runHWMFail :: String -> IO String
runHWMFail cmd = do
  envVars <- mkGoldenEnv
  (exitCode, out, err) <- readCreateProcessWithExitCode ((shell $ "hwm -- " <> cmd) {env = Just envVars}) ""
  when (exitCode == System.Exit.ExitSuccess)
    $ expectationFailure ("Command unexpectedly succeeded with stdout: " <> out)
  return (out <> err)

data ChangeReport = ChangeReport
  { addedFiles :: [FilePath],
    deletedFiles :: [FilePath],
    modifiedFiles :: [FilePath],
    invocations :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChangeReport where
  toJSON ChangeReport {..} =
    object
      $ catMaybes
        [ if null addedFiles then Nothing else Just ("addedFiles" .= addedFiles),
          if null deletedFiles then Nothing else Just ("deletedFiles" .= deletedFiles),
          if null modifiedFiles then Nothing else Just ("modifiedFiles" .= modifiedFiles),
          ("invocations" .=) <$> invocations
        ]

instance FromJSON ChangeReport where
  parseJSON = withObject "ChangeReport" $ \o ->
    ChangeReport
      <$> o .:? "addedFiles" .!= []
      <*> o .:? "deletedFiles" .!= []
      <*> o .:? "modifiedFiles" .!= []
      <*> o .:? "invocations"

buildChangeReport :: [(FilePath, UTCTime)] -> [(FilePath, UTCTime)] -> ChangeReport
buildChangeReport oldState newState =
  let oldMap = Map.fromList oldState
      newMap = Map.fromList newState
      added = Map.keys $ Map.difference newMap oldMap
      deleted = Map.keys $ Map.difference oldMap newMap
      common = Map.intersectionWith (,) oldMap newMap
      modified = Map.keys $ Map.filter (uncurry (/=)) common
   in ChangeReport added deleted modified Nothing

loadInvocations :: IO (Maybe Value)
loadInvocations = do
  let file = "invocations.yaml"
  exists <- doesPathExist file
  if not exists
    then pure Nothing
    else do
      parsed <- Yaml.decodeFileEither file
      case parsed of
        Right v -> pure (Just v)
        Left _ -> pure Nothing

trackChanges :: IO a -> IO (ChangeReport, a)
trackChanges action = do
  beforeFiles <- findManagedFiles "."
  oldTimes <- mapM (\p -> (p,) <$> getModificationTime p) beforeFiles
  threadDelay 1100000

  a <- action
  afterFiles <- findManagedFiles "."
  newTimes <- mapM (\p -> (p,) <$> getModificationTime p) afterFiles
  inv <- loadInvocations
  pure ((buildChangeReport oldTimes newTimes) {invocations = inv}, a)

hasNoChanges :: ChangeReport -> Bool
hasNoChanges (ChangeReport added deleted modified inv) = null added && null deleted && null modified && isNothing inv

cleanupEmptyDeltaFiles :: FilePath -> IO ()
cleanupEmptyDeltaFiles root = do
  deltaFiles <- glob (root </> "**/delta.yaml")
  forM_ deltaFiles $ \deltaFile -> do
    report <- Yaml.decodeFileEither deltaFile
    case report of
      Right changes | hasNoChanges changes -> removeFile deltaFile
      _ -> pure ()

saveSnapshot :: ChangeReport -> FilePath -> IO ()
saveSnapshot (ChangeReport added _ modified _) dst = do
  removePathForcibly dst
  createDirectoryIfMissing True dst
  let filesToUpdate = added ++ modified
  forM_ filesToUpdate $ \f -> do
    let srcPath = f
    let dstPath = dst </> f
    createDirectoryIfMissing True (takeDirectory dstPath)
    copyFile srcPath dstPath

diffChanges :: FilePath -> ChangeReport -> IO ()
diffChanges expectedDir (ChangeReport added deleted modified _) = do
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

sanitizeAllCabals :: IO ()
sanitizeAllCabals = do
  cabalFiles <- glob "./**/*.cabal"
  forM_ cabalFiles $ \path -> do
    content <- TIO.readFile path
    let sanitized = sanitizeCabal content
    when (content /= sanitized) $ TIO.writeFile path sanitized
