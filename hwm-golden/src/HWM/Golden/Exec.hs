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
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Process (CreateProcess (env), readCreateProcessWithExitCode, shell)

mkGoldenEnv :: Maybe CaseRunner -> Map.Map String String -> IO [(String, String)]
mkGoldenEnv mRunner caseEnv = do
  cwd <- getCurrentDirectory
  current <- getEnvironment

  let runnerEnvOverrides = fromMaybe Map.empty (mRunner >>= runnerEnv)
  let runnerPathEntries = mRunner >>= runnerPath

  let blocked = ["PATH", "HOME", "STACK_YAML", "CABAL_PROJECT_FILE", "WORKING_DIR"]
  let keep (k, _) = k `notElem` blocked
  let inherited = Map.fromList (filter keep current)

  let defaults =
        Map.fromList
          [ ("HOME", ".home"),
            ("CI", "1"),
            ("WORKING_DIR", cwd)
          ]

  let mergedForTemplate = Map.unions [caseEnv, runnerEnvOverrides, defaults, inherited]
  let templateVars = Map.insert "cwd" cwd mergedForTemplate

  let pathTemplates = fromMaybe ["${WORKING_DIR}/bin", "${HOME}/.local/bin"] runnerPathEntries
  let renderedPathEntries = map (`expandTemplate` templateVars) pathTemplates
  let pathValue = S.intercalate ":" (renderedPathEntries <> maybeToList (S.lookup "PATH" current))

  let base = Map.insert "PATH" pathValue defaults

  pure
    $ Map.toList
    $ Map.unions
      [ caseEnv,
        runnerEnvOverrides,
        base,
        inherited
      ]

expandTemplate :: String -> Map.Map String String -> String
expandTemplate raw vars =
  toString
    $ foldl'
      (\acc (k, v) -> T.replace (("${" :: Text) <> toText k <> "}") (toText v) acc)
      (toText raw)
      (Map.toList vars)

runHWM :: Maybe CaseRunner -> Map.Map String String -> String -> IO (ChangeReport, (Bool, String))
runHWM caseRunner caseEnv cmd = trackChanges $ do
  envVars <- mkGoldenEnv caseRunner caseEnv
  (exitCode, out, err) <- readCreateProcessWithExitCode ((shell $ "hwm -- " <> cmd) {env = Just envVars}) ""
  let failure = exitCode /= System.Exit.ExitSuccess
  pure (failure, if failure then out <> err else out)

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"
