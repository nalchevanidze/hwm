{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Exec
  ( runHWM,
    isUpdateMode,
  )
where

import qualified Data.List as S
import qualified Data.Map.Strict as Map
import qualified GHC.IO.Exception as System.Exit
import HWM.Golden.Changes (trackChanges)
import HWM.Golden.Types (ChangeReport)
import Relude
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess (env), readCreateProcessWithExitCode, shell)

mkGoldenEnv :: Map.Map String String -> IO [(String, String)]
mkGoldenEnv overrides = do
  cwd <- getCurrentDirectory
  current <- getEnvironment
  let home = ".home"
  let localBin = home </> ".local" </> "bin"
  let pathValue = [cwd </> "bin", localBin]
  let blocked = ["PATH", "HOME", "STACK_YAML", "CABAL_PROJECT_FILE"]
  let keep (k, _) = k `notElem` blocked
  let base =
        Map.fromList
          [ ("PATH", S.intercalate ":" (pathValue <> maybeToList (S.lookup "PATH" current))),
            ("HOME", home),
            ("CI", "1")
          ]
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
  pure (failure, if failure then out <> err else out)

isUpdateMode :: IO Bool
isUpdateMode = (== Just "1") <$> lookupEnv "GOLDEN_UPDATE"
