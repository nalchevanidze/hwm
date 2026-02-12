{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Options
  ( Options (..),
    defaultOptions,
    askOptions,
  )
where

import HWM.Core.Has (Has (..))
import Relude

askOptions :: (MonadReader env m, Has env Options) => m Options
askOptions = asks obtain

data Options = Options
  { hie :: FilePath,
    hwm :: FilePath,
    stack :: FilePath,
    quiet :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { hwm = "./hwm.yaml",
      hie = "./hie.yaml",
      stack = "./stack.yaml",
      quiet = False
    }
