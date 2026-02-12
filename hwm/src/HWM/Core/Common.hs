{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Common
  ( Name,
    Check (..),
  )
where

import Data.Text (Text)

-- | Shared alias for human-readable identifiers (e.g. cache names).
type Name = Text

-- | Capability gate for validations that report issues inside a monad.
class Check m a where
  check :: a -> m ()
