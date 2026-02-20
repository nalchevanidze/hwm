{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.App
  ( main,
  )
where

import qualified Data.Text as T
import HWM.CLI.Command (Command (..), Options (..), currentVersion, defaultOptions, runCommand)
import HWM.Core.Parsing (ParseCLI (..), flag)
import Options.Applicative
  ( Parser,
    argument,
    command,
    customExecParser,
    fullDesc,
    help,
    helper,
    info,
    metavar,
    prefs,
    progDesc,
    showHelpOnError,
    str,
    subparser,
  )
import Relude

-- Helper for building commands (unchanged, just added type signature clarity)

run :: Parser a -> IO a
run app =
  customExecParser
    (prefs showHelpOnError)
    ( info
        (helper <*> app)
        (fullDesc <> progDesc "HWM - Haskell Workspace Manager for Monorepos")
    )

commands :: [(String, String, Parser a)] -> Parser a
commands =
  subparser
    . mconcat
    . map
      (\(name, desc, value) -> command name (info (helper <*> value) (fullDesc <> progDesc desc)))

parseCommand :: Parser Command
parseCommand =
  commands
    [ ( "sync",
        "Regenerate stack.yaml and .cabal files. Optional: switch environment.",
        Sync <$> optional (argument str (metavar "ENV" <> help "Switch to a specific environment (e.g., legacy, stable)"))
      ),
      ( "version",
        "Show version or bump it (patch | minor | major).",
        Version <$> parseCLI
      ),
      ( "run",
        "Run a script defined in hwm.yaml",
        Run <$> argument (T.pack <$> str) (metavar "SCRIPT" <> help "Name of the script to run") <*> parseCLI
      ),
      ( "status",
        "Show the current environment, version, and sync status.",
        pure Status
      ),
      ( "init",
        "Initialize a new HWM workspace by scanning the current directory.",
        Init <$> parseCLI
      ),
      ( "registry",
        "Manage the dependency registry (add, audit, ls).",
        Registry <$> parseCLI
      ),
      ( "environment",
        "Manage build environments (add, remove, set-default, ls).",
        Env <$> parseCLI
      ),
      ( "workspace",
        "Manage workspaces (add, ls).",
        Workspace <$> parseCLI
      ),
      ( "release",
        "Release related commands (package, publish).",
        Release <$> parseCLI
      )
    ]
    <|> (Run <$> argument (T.pack <$> str) mempty <*> parseCLI)

data Input = Input
  { v :: Bool,
    q :: Bool,
    cmd :: Maybe Command
  }
  deriving (Show)

parseInput :: IO Input
parseInput =
  run
    $ Input
    <$> flag 'v' "version" "Show HWM version number"
    <*> flag 'q' "quiet" "Run quietly with minimal output"
    <*> optional parseCommand

main :: IO ()
main = do
  Input {v, q, cmd} <- parseInput
  if v
    then putStrLn ("HWM v" ++ currentVersion)
    else case cmd of
      Just c -> runCommand c (defaultOptions {quiet = q})
      Nothing -> do
        putStrLn "HWM: Missing command.\nTry 'hwm --help' for usage."
        exitFailure
