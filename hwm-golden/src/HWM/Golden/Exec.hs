{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Exec
  ( runHWM,
    isUpdateMode,
  )
where

import qualified Data.List as S
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC.IO.Exception as System.Exit
import HWM.Golden.Changes (trackChanges)
import HWM.Golden.Scanning (CaseRunner (..))
import HWM.Golden.Types (ChangeReport)
import Relude
import System.Directory (Permissions (..), doesFileExist, findExecutable, getCurrentDirectory, getPermissions)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess (env), readCreateProcessWithExitCode, shell)

blockedEnvKeys :: [String]
blockedEnvKeys = ["PATH", "HOME", "STACK_YAML", "CABAL_PROJECT_FILE"]

mkGoldenEnv :: Maybe CaseRunner -> IO [(String, String)]
mkGoldenEnv mRunner = do
  current <- getEnvironment
  cwd <- getCurrentDirectory
  let runnerEnvOverrides = fromMaybe Map.empty (mRunner >>= runnerEnv)
      configuredPathTemplates = fromMaybe [] (mRunner >>= runnerPath)
      shouldPrependWorkBin = maybe False (not . Map.null) (mRunner >>= runnerBin)
      inherited = Map.fromList (filter (not . (`elem` blockedEnvKeys) . fst) current)
      templateVars = Map.union runnerEnvOverrides inherited
      prependPathEntries =
        ordNub
          ( [cwd </> "bin" | shouldPrependWorkBin]
              <> map (`expandTemplate` templateVars) configuredPathTemplates
          )
      pathValue = buildPath prependPathEntries (S.lookup "PATH" current)

  pure . Map.toList $ Map.insert "PATH" pathValue (Map.union runnerEnvOverrides inherited)

buildPath :: [FilePath] -> Maybe String -> String
buildPath prepend inheritedPath =
  S.intercalate ":" (prepend <> maybeToList (inheritedPath >>= nonEmptyString))
  where
    nonEmptyString s = if null s then Nothing else Just s

expandTemplate :: String -> Map.Map String String -> String
expandTemplate raw vars =
  toString
    $ foldl'
      (\acc (k, v) -> T.replace ("${" <> toText k <> "}") (toText v) acc)
      (toText raw)
      (Map.toList vars)

resolveHwmExecutable :: Maybe CaseRunner -> IO FilePath
resolveHwmExecutable caseRunner =
  case caseRunner >>= runnerBin >>= Map.lookup "hwm" of
    Just path -> validateExecutable path $> path
    Nothing -> fromMaybe "hwm" <$> findExecutable "hwm"

validateExecutable :: FilePath -> IO ()
validateExecutable path = do
  exists <- doesFileExist path
  unless exists $ fail ("Configured runner.bin.hwm does not exist: " <> path)
  perms <- getPermissions path
  unless (executable perms) $ fail ("Configured runner.bin.hwm is not executable: " <> path)

runHWM :: Maybe CaseRunner -> String -> IO (ChangeReport, (Bool, String))
runHWM caseRunner cmd = trackChanges $ do
  envVars <- mkGoldenEnv caseRunner
  hwmExe <- resolveHwmExecutable caseRunner
  let command = show hwmExe <> " -- " <> cmd
  (exitCode, out, err) <- readCreateProcessWithExitCode ((shell command) {env = Just envVars}) ""
  let failure = exitCode /= System.Exit.ExitSuccess
  pure (failure, if failure then out <> err else out)

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"
