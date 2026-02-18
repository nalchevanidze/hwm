{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command
  ( Options (..),
    Command (..),
    currentVersion,
    defaultOptions,
    Bump (..),
    runCommand,
  )
where

import Data.Version (showVersion)
import HWM.CLI.Command.Init (InitOptions (..), initWorkspace)
import HWM.CLI.Command.Publish (publish)
import HWM.CLI.Command.Registry (RegistryCommand, runRegistry)
import HWM.CLI.Command.Environment (EnvCommand, runEnv)
import HWM.CLI.Command.Run (ScriptOptions, runScript)
import HWM.CLI.Command.Status (showStatus)
import HWM.CLI.Command.Sync (sync)
import HWM.CLI.Command.Version (runVersion)
import HWM.Core.Common (Name)
import HWM.Core.Options (Options (..), defaultOptions)
import HWM.Core.Version (Bump (..))
import HWM.Domain.ConfigT (ConfigT, runConfigT)
import qualified Paths_hwm as CLI
import Relude hiding (fix)

data Command
  = Sync {tag :: Maybe Name}
  | Publish {groupName :: Maybe Name}
  | Version {bump :: Maybe Bump}
  | Run {scriptName :: Name, runOptions :: ScriptOptions}
  | Status
  | Init {initOptions :: InitOptions}
  | Registry RegistryCommand
  | Env EnvCommand
  deriving (Show)

currentVersion :: String
currentVersion = showVersion CLI.version


-- | Run the top-level command
command :: Command -> ConfigT ()
command Publish {groupName} = publish groupName
command Version {bump} = runVersion bump
command Sync {tag} = sync tag
command Run {scriptName, runOptions} = runScript scriptName runOptions
command Status = showStatus
command Init {} = pure ()
command (Registry options) = runRegistry options
command (Env options) = runEnv options

-- EnvCommand and runEnv are now defined in Command.Environment

runCommand :: Command -> Options -> IO ()
runCommand Init {initOptions} ops = initWorkspace initOptions ops >> runConfigT showStatus ops
runCommand cmd ops = runConfigT (command cmd) ops
