{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Options
  ( Options (..),
    defaultOptions,
    askOptions,
    whenCI,
    isCI,
    whenDebug,
  )
where

import HWM.Core.Has (Has (..))
import Relude

askOptions :: (MonadReader env m, Has env Options) => m Options
askOptions = asks obtain

data Options = Options
  { optionsHie :: FilePath,
    optionsHwm :: FilePath,
    optionsStack :: FilePath,
    optionsQuiet :: Bool,
    optionsCabal :: FilePath,
    optionsNix :: FilePath,
    optionDebug :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optionsHwm = "./hwm.yaml",
      optionsHie = "./hie.yaml",
      optionsStack = "./stack.yaml",
      optionsQuiet = False,
      optionsCabal = "./cabal.project",
      optionsNix = "./flake.nix",
      optionDebug = False
    }

isCI :: (MonadIO m) => m Bool
isCI = liftIO $ isJust <$> lookupEnv "CI"

whenCI :: (MonadIO m) => m () -> m ()
whenCI action = do
  ci <- isCI
  when ci action

whenDebug :: (MonadIO m) => m () -> m ()
whenDebug action = do
  debug <- liftIO $ (Just "true" ==) <$> lookupEnv "DEBUG"
  when debug action
