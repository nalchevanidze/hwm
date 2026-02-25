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
    [ ( "init",
        "Initialize a new HWM workspace by scanning the directory structure.",
        Init <$> parseCLI
      ),
      ( "status",
        "Show current project version, active environment, and sync state.",
        pure Status
      ),
      ( "sync",
        "Generate stack.yaml/cabal files for a specific environment (defaults to 'default').",
        Sync <$> optional (argument str (metavar "ENV" <> help "Environment name or 'all' for the full matrix"))
      ),
      ( "run",
        "Execute a script defined in hwm.yaml (e.g., build, test, lint).",
        Run <$> argument (T.pack <$> str) (metavar "SCRIPT" <> help "Script name") <*> parseCLI
      ),
      ( "workspace",
        "Manage workspace groups and package members.",
        Workspace <$> parseCLI
      ),
      ( "environments",
        "Manage GHC toolchains and resolver targets.",
        Env <$> parseCLI
      ),
      ( "registry",
        "Audit and manage the dependency registry and version bounds.",
        Registry <$> parseCLI
      ),
      ( "version",
        "Display the current workspace version or bump it (patch, minor, major).",
        Version <$> parseCLI
      ),
      ( "release",
        "Manage delivery: build artifacts or publish release trains.",
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
      Just c -> runCommand c (defaultOptions {optionsQuiet = q})
      Nothing -> do
        putStrLn "HWM: Missing command.\nTry 'hwm --help' for usage."
        exitFailure
