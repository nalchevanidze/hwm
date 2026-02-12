{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Has
  ( Has (..),
    askEnv,
    HasAll,
  )
where

import Relude

class Has env a where
  obtain :: env -> a

askEnv :: (MonadReader env m, Has env a) => m a
askEnv = asks obtain

type family HasAll env (xs :: [Type]) :: Constraint where
  HasAll _ '[] = ()
  HasAll env (x ': xs) = (Has env x, HasAll env xs)
