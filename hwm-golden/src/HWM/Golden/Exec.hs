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

defaultEnv :: Map.Map String String
defaultEnv =
  Map.fromList
    [ ("HOME", ".home"),
      ("CI", "1")
    ]

mkGoldenEnv :: Maybe CaseRunner -> IO [(String, String)]
mkGoldenEnv mRunner = do
  current <- getEnvironment

  let runnerEnvOverrides = fromMaybe Map.empty (mRunner >>= runnerEnv)
  let configuredPathTemplates = fromMaybe [] (mRunner >>= runnerPath)
  let hasRunnerBins = maybe False (not . Map.null) (mRunner >>= runnerBin)

  let keep (k, _) = k `notElem` blockedEnvKeys
  let inherited = Map.fromList (filter keep current)
  let inheritedPath = S.lookup "PATH" current

  -- Merge order for template expansion:
  -- inherited < defaults < runner.env
  let templateVars = Map.unions [runnerEnvOverrides, defaultEnv, inherited]

  cwd <- getCurrentDirectory
  let configuredPathEntries = map (`expandTemplate` templateVars) configuredPathTemplates
  let autoPathEntries = [cwd </> "bin" | hasRunnerBins]
  let prependPathEntries = ordNub (autoPathEntries <> configuredPathEntries)
  let pathValue = buildPath prependPathEntries inheritedPath

  pure
    $ Map.toList
    $ Map.unions
      [ runnerEnvOverrides,
        Map.insert "PATH" pathValue defaultEnv,
        inherited
      ]

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
    Just path -> do
      exists <- doesFileExist path
      unless exists $ fail ("Configured runner.bin.hwm does not exist: " <> path)
      perms <- getPermissions path
      unless (executable perms) $ fail ("Configured runner.bin.hwm is not executable: " <> path)
      pure path
    Nothing -> fromMaybe "hwm" <$> findExecutable "hwm"

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
