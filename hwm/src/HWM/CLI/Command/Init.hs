{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Init (initWorkspace, InitOptions (..)) where

import Control.Monad.Except (MonadError (..))
import Data.List
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Color (Cyan), Format (format), chalk, padDots)
import HWM.Core.Options (Options (..))
import HWM.Core.Parsing (ParseCLI (..), flag)
import HWM.Core.Pkg (Pkg (..), scanPkgs)
import HWM.Core.Result (Issue)
import HWM.Core.Version (Version)
import HWM.Domain.Config (Config (..), defaultScripts)
import HWM.Domain.ConfigT (resolveResultUI, saveConfig)
import HWM.Domain.Workspace (buildWorkspace)
import HWM.Integrations.Toolchain.Package (deriveRegistry)
import HWM.Integrations.Toolchain.Stack (buildMatrix, scanStackFiles)
import HWM.Runtime.Files (forbidOverride)
import HWM.Runtime.UI (MonadUI, putLine, runUI, section)
import Options.Applicative (argument, help, metavar, str)
import Relude hiding (exitWith, notElem)
import System.Directory (getCurrentDirectory)
import System.FilePath
  ( normalise,
    takeFileName,
    (</>),
  )

size :: Int
size = 24

data InitOptions = InitOptions
  { forceOverride :: Bool,
    projectName :: Maybe Text
  }
  deriving (Show)

instance ParseCLI InitOptions where
  parseCLI =
    InitOptions
      <$> flag 'f' "force" "Force override existing hwm.yaml"
      <*> optional (argument str (metavar "NAME" <> help "Optional project name (defaults to current directory name)"))

initWorkspace :: InitOptions -> Options -> IO ()
initWorkspace InitOptions {..} opts = runUI $ resolveResultUI $ do
  root <- liftIO getCurrentDirectory
  let cfgName = fromMaybe (deriveName root) projectName
  section "init" $ do
    unless forceOverride $ forbidOverride (normalise (root </> hwm opts))
    stacks <- scanStackFiles opts root
    scanning "stack.yaml" stacks
    pkgs <- scanPkgs root
    scanning "packages" pkgs
    when (null pkgs) $ throwError "No packages listed in stack.yaml. Add at least one package before running 'hwm init'"
    (cfgRegistry, graph) <- deriveRegistry pkgs
    cfgVersion <- deriveVersion (map pkgVersion pkgs)
    cfgEnvironments <- buildMatrix pkgs stacks
    cfgWorkspace <- buildWorkspace graph pkgs
    saveConfig
      Config
        { cfgGithub = Nothing,
          cfgBounds = Nothing,
          cfgScripts = defaultScripts,
          cfgRelease = Nothing,
          ..
        }
      opts
    putLine $ padDots size "save (config)" <> chalk Cyan "hwm.yaml"

scanning :: (MonadUI m, Foldable t) => Text -> t a -> m ()
scanning name ls = putLine (padDots size ("scan (" <> name <> ")") <> format (length ls) <> " found")

deriveName :: FilePath -> Name
deriveName path =
  let candidate = takeFileName path
   in if null candidate then "workspace" else toText candidate

deriveVersion :: (MonadError Issue m) => [Version] -> m Version
deriveVersion
  versions
    | null versions = throwError "No package versions found for inference"
    | otherwise = pure $ maximum versions
