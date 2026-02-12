{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.App
  ( main,
  )
where

import Data.Text (pack)
import HWM.CLI.Command
  ( Command (..),
    Options (..),
    currentVersion,
    defaultOptions,
    runCommand,
  )
import HWM.CLI.Command.Init (InitOptions (..))
import HWM.CLI.Command.Run (ScriptOptions (..))
import HWM.Core.Common (Name)
import HWM.Core.Parsing (Parse (..), parseOptions)
import Options.Applicative
  ( Parser,
    argument,
    command,
    customExecParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    prefs,
    progDesc,
    short,
    showHelpOnError,
    strArgument,
    subparser,
    switch,
  )
import Options.Applicative.Builder (str, strOption)
import Relude hiding (ByteString, fix)

-- Helper for building commands (unchanged, just added type signature clarity)
commands :: [(String, String, Parser a)] -> Parser a
commands =
  subparser
    . mconcat
    . map
      ( \(name, desc, value) ->
          command name (info (helper <*> value) (fullDesc <> progDesc desc))
      )

flag :: Char -> String -> String -> Parser Bool
flag s l h = switch (long l <> short s <> help h)

run :: Parser a -> IO a
run app =
  customExecParser
    (prefs showHelpOnError)
    ( info
        (helper <*> app)
        (fullDesc <> progDesc "HWM - Haskell Workspace Manager for Monorepos")
    )

parseScriptOptions :: Parser Name -> Parser ScriptOptions
parseScriptOptions name =
  ScriptOptions
    <$> name
    <*> fmap parseOptions (many (strOption (long "target" <> short 't' <> metavar "TARGET" <> help "Limit to package (core) or group (libs)")))
    <*> fmap parseOptions (many (strOption (long "env" <> short 'e' <> metavar "ENV" <> help "Run in specific env (use 'all' for full matrix)")))
    <*> many (argument (pack <$> str) (metavar "ARGS..." <> help "Arguments to forward to the script"))

parseInitOptions :: Parser InitOptions
parseInitOptions =
  InitOptions
    <$> flag 'f' "force" "Force override existing hwm.yaml"
    <*> optional (argument str (metavar "NAME" <> help "Optional project name (defaults to current directory name)"))

parseCommand :: Parser Command
parseCommand =
  commands
    [ ( "sync",
        "Regenerate stack.yaml and .cabal files. Optional: switch environment.",
        Sync <$> optional (argument str (metavar "ENV" <> help "Switch to a specific environment (e.g., legacy, stable)"))
      ),
      ( "version",
        "Show version or bump it (patch | minor | major).",
        Version <$> optional (argument (str >>= parse) (metavar "BUMP" <> help "Version bump type or specific version number"))
      ),
      ( "outdated",
        "Check for newer dependencies on Hackage.",
        Outdated <$> switch (long "fix" <> short 'f' <> help "Write changes to hwm.yaml")
      ),
      ( "publish",
        "Upload packages to Hackage/Registry.",
        Publish <$> optional (argument str (metavar "GROUP" <> help "Name of the workspace group to publish (default: all)"))
      ),
      ( "run",
        "Run a script defined in hwm.yaml",
        Run <$> parseScriptOptions (argument (pack <$> str) (metavar "SCRIPT" <> help "Name of the script to run"))
      ),
      ( "status",
        "Show the current environment, version, and sync status.",
        pure Status
      ),
      ( "init",
        "Initialize a new HWM workspace by scanning the current directory.",
        Init <$> parseInitOptions
      )
    ]
    <|> (Run <$> parseScriptOptions (strArgument (metavar "SCRIPT")))

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
