{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Options
  ( Options (..),
    defaultOptions,
    askOptions,
    whenCI,
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
    optionsNix :: FilePath
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optionsHwm = "./hwm.yaml",
      optionsHie = "./hie.yaml",
      optionsStack = "./stack.yaml",
      optionsQuiet = False,
      optionsCabal = "./cabal.project",
      optionsNix = "./flake.nix"
    }

whenCI :: (MonadIO m) => m () -> m ()
whenCI action = do
  ci <- liftIO $ isJust <$> lookupEnv "CI"
  when ci action
