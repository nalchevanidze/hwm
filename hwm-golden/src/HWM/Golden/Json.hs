{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.Json (dropEmpty) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as M
import Relude

dropEmpty :: Value -> Value
dropEmpty (Object o) = Object $ M.filter (not . isEmptyValue) o
dropEmpty v = v

isEmptyValue :: Value -> Bool
isEmptyValue Null = True
isEmptyValue (Object o) = M.null o
isEmptyValue (Array a) = null a || all isEmptyValue a
isEmptyValue _ = False
