{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Ls (runRegistryLs) where

import qualified Data.Text as T
import HWM.Core.Formatting (Format (..), formatTable)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Dependencies (toDependencyList)
import HWM.Runtime.UI (putLine, section)
import Relude

runRegistryLs :: Maybe Text -> ConfigT ()
runRegistryLs regSearch = do
  deps <- asks (toDependencyList . registry . config)
  let filtered = maybe deps (\s -> filter (T.isInfixOf s . format) deps) regSearch
  section "registry" $ forM_ (formatTable (map format filtered)) putLine