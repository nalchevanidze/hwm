{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Ls (runRegistryLs) where

import qualified Data.Text as T
import HWM.Core.Formatting (Format (..), formatTable)
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Config (Config(..))
import HWM.Domain.Dependencies (toDependencyList)
import Relude

runRegistryLs :: Maybe Text -> ConfigT ()
runRegistryLs regSearch = do
  deps <- asks (toDependencyList . registry . config)
  let filtered = case regSearch of
        Nothing -> deps
        Just s -> filter (T.isInfixOf s . format) deps
  let table = formatTable (map format filtered)
      forM_ table (putStrLn . toString)