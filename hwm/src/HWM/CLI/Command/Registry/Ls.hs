{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Registry.Ls (runRegistryLs, RegistryLsOptions (..)) where

import qualified Data.Text as T
import HWM.Core.Formatting (Format (..), formatTable)
import HWM.Core.Parsing (ParseCLI (..))
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, config)
import HWM.Domain.Dependencies (toDependencyList)
import HWM.Runtime.UI (putLine, section)
import Options.Applicative (help, long, metavar, short, strOption)
import Relude

newtype RegistryLsOptions = RegistryLsOptions {regSearch :: Maybe Text} deriving (Show)

instance ParseCLI RegistryLsOptions where
  parseCLI = RegistryLsOptions <$> optional (strOption (long "search" <> short 's' <> metavar "SEARCH" <> help "Filter registry entries"))

runRegistryLs :: RegistryLsOptions -> ConfigT ()
runRegistryLs RegistryLsOptions {regSearch} = do
  deps <- asks (toDependencyList . registry . config)
  let filtered = maybe deps (\s -> filter (T.isInfixOf s . format) deps) regSearch
  section "registry" $ forM_ (formatTable (map format filtered)) putLine