{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Core
  ( ExpectedFiles (..),
    ChangeReport (..),
    sanitizeAllCabals,
    diffChanges,
    copyLocalFiles,
    inWorkDir,
    diff,
    runHWM,
    saveSnapshot,
    dropEmpty,
    isUpdateMode,
  )
where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:?), (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.KeyMap as M
import Data.Aeson.Types ((.!=))
import qualified Data.ByteString as BS
import qualified Data.List as S
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import qualified Data.Yaml as Yaml
import qualified GHC.IO.Exception as System.Exit
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, getCurrentDirectory, getModificationTime, listDirectory, makeAbsolute, removePathForcibly, setCurrentDirectory)
import System.Directory.Internal.Prelude (bracket)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.FilePath.Glob (glob)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env), callCommand, readCreateProcessWithExitCode, shell)
import Test.Hspec (expectationFailure)

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

mkGoldenEnv :: Map.Map String String -> IO [(String, String)]
mkGoldenEnv overrides = do
  cwd <- getCurrentDirectory
  current <- getEnvironment
  let oldPath = fromMaybe "" (S.lookup "PATH" current)
  let home = ".home"
  let localBin = home </> ".local" </> "bin"
  let pathValue = S.intercalate ":" [cwd </> "bin", localBin, oldPath]
  let goldenRunnerOS = Map.lookup "RUNNER_OS" overrides <|> S.lookup "GOLDEN_RUNNER_OS" current
  let goldenRunnerArch = Map.lookup "RUNNER_ARCH" overrides <|> S.lookup "GOLDEN_RUNNER_ARCH" current
  let blocked = ["PATH", "HOME", "STACK_YAML", "CABAL_PROJECT_FILE", "RUNNER_OS", "RUNNER_ARCH"]
  let keep (k, _) = k `notElem` blocked
  let runnerVars = catMaybes [("RUNNER_OS",) <$> goldenRunnerOS, ("RUNNER_ARCH",) <$> goldenRunnerArch]
  let base =
        Map.fromList
          $ [ ("PATH", pathValue),
              ("HOME", home),
              ("HWM_LOG_ID_FIXED", "golden"),
              ("HACKAGE_AUTH_TOKEN", "golden-token"),
              ("CI", "1")
            ]
          <> runnerVars
  pure
    $ Map.toList
    $ Map.unions
      [ overrides,
        base,
        Map.fromList (filter keep current)
      ]

runHWM :: Map.Map String String -> String -> IO (ChangeReport, (Bool, String))
runHWM caseEnv cmd = trackChanges $ do
  envVars <- mkGoldenEnv caseEnv
  (exitCode, out, err) <- readCreateProcessWithExitCode ((shell $ "hwm -- " <> cmd) {env = Just envVars}) ""
  let failure = exitCode /= System.Exit.ExitSuccess
  return (failure, if failure then out <> err else out)

data ExpectedFiles = ExpectedFiles
  { added :: [FilePath],
    deleted :: [FilePath],
    modified :: [FilePath]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ExpectedFiles where
  toJSON ExpectedFiles {..} =
    dropEmpty
      $ object
        [ "added" .= added,
          "deleted" .= deleted,
          "modified" .= modified
        ]

instance FromJSON ExpectedFiles where
  parseJSON = withObject "ExpectedFiles" $ \o ->
    ExpectedFiles
      <$> o
      .:? "added"
      .!= []
      <*> o
      .:? "deleted"
      .!= []
      <*> o
      .:? "modified"
      .!= []

data ChangeReport = ChangeReport
  { files :: ExpectedFiles,
    calls :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChangeReport where
  toJSON ChangeReport {files = ExpectedFiles {..}, calls} =
    dropEmpty
      $ object
        [ "added" .= added,
          "deleted" .= deleted,
          "modified" .= modified,
          "calls" .= calls
        ]

instance FromJSON ChangeReport where
  parseJSON = withObject "ChangeReport" $ \o ->
    ChangeReport
      <$> (ExpectedFiles <$> o .:? "added" .!= [] <*> o .:? "deleted" .!= [] <*> o .:? "modified" .!= [])
      <*> o
      .:? "calls"

canonicalPath :: FilePath -> FilePath
canonicalPath p = toString (fromMaybe (toText p) (T.stripPrefix "./" (toText p)))

buildChangeReport :: [(FilePath, UTCTime)] -> [(FilePath, UTCTime)] -> ChangeReport
buildChangeReport oldState newState =
  let oldMap = Map.fromList oldState
      newMap = Map.fromList newState
      added = sort (map canonicalPath (Map.keys (Map.difference newMap oldMap)))
      deleted = sort (map canonicalPath (Map.keys (Map.difference oldMap newMap)))
      common = Map.intersectionWith (,) oldMap newMap
      modified = sort (map canonicalPath (Map.keys (Map.filter (uncurry (/=)) common)))
   in ChangeReport (ExpectedFiles added deleted modified) Nothing

loadInvocations :: IO (Maybe Value)
loadInvocations = do
  let file = "invocations.yaml"
  exists <- doesPathExist file
  if not exists
    then pure Nothing
    else do
      parsed <- Yaml.decodeFileEither file
      case parsed of
        Right (Object obj) -> pure (KM.lookup (K.fromText "calls") obj)
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
  pure ((buildChangeReport oldTimes newTimes) {calls = inv}, a)

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

dropEmpty :: Value -> Value
dropEmpty (Object o) = Object $ M.filter (not . isEmptyValue) o
dropEmpty v = v

isEmptyValue :: Value -> Bool
isEmptyValue Null = True
isEmptyValue (Object o) = M.null o
isEmptyValue (Array a) = null a || all isEmptyValue a
isEmptyValue _ = False

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"