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
import System.Directory (doesDirectoryExist, findExecutable, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Process (CreateProcess (env), readCreateProcessWithExitCode, shell)

mkGoldenEnv :: Maybe CaseRunner -> IO [(String, String)]
mkGoldenEnv mRunner = do
  cwd <- getCurrentDirectory
  current <- getEnvironment

  let runnerEnvOverrides = fromMaybe Map.empty (mRunner >>= runnerEnv)
  let runnerPathEntries = mRunner >>= runnerPath

  let blocked = ["PATH", "HOME", "STACK_YAML", "CABAL_PROJECT_FILE", "WORKING_DIR"]
  let keep (k, _) = k `notElem` blocked
  let inherited = Map.fromList (filter keep current)
  let inheritedPath = fromMaybe "" (S.lookup "PATH" current)

  let defaults =
        Map.fromList
          [ ("HOME", ".home"),
            ("CI", "1"),
            ("WORKING_DIR", cwd)
          ]

  let mergedForTemplate = Map.unions [runnerEnvOverrides, defaults, inherited]
  let templateVars = Map.insert "cwd" cwd mergedForTemplate

  prependPathEntries <- resolvePathEntries templateVars runnerPathEntries
  let pathValue = S.intercalate ":" (prependPathEntries <> [inheritedPath])

  let base = Map.insert "PATH" pathValue defaults

  pure
    $ Map.toList
    $ Map.unions
      [ runnerEnvOverrides,
        base,
        inherited
      ]

resolvePathEntries :: Map.Map String String -> Maybe [FilePath] -> IO [FilePath]
resolvePathEntries templateVars maybeConfigured =
  case maybeConfigured of
    Just templates -> pure (map (`expandTemplate` templateVars) templates)
    Nothing -> do
      let defaults = ["${WORKING_DIR}/bin", "${HOME}/.local/bin"]
      let rendered = map (`expandTemplate` templateVars) defaults
      filterM doesDirectoryExist rendered

expandTemplate :: String -> Map.Map String String -> String
expandTemplate raw vars =
  toString
    $ foldl'
      (\acc (k, v) -> T.replace ("${" <> toText k <> "}") (toText v) acc)
      (toText raw)
      (Map.toList vars)

runHWM :: Maybe CaseRunner -> String -> IO (ChangeReport, (Bool, String))
runHWM caseRunner cmd = trackChanges $ do
  envVars <- mkGoldenEnv caseRunner
  defaultHwmExe <- fromMaybe "hwm" <$> findExecutable "hwm"
  let hwmExe = fromMaybe defaultHwmExe (caseRunner >>= runnerBin >>= Map.lookup "hwm")
  let command = show hwmExe <> " -- " <> cmd
  (exitCode, out, err) <- readCreateProcessWithExitCode ((shell command) {env = Just envVars}) ""
  let failure = exitCode /= System.Exit.ExitSuccess
  pure (failure, if failure then out <> err else out)

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"
