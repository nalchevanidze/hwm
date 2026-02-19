{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module HWM.CLI.Command.Version
  ( runVersion,
    VersionOptions,
  )
where

import HWM.Core.Formatting (Color (..), Format (..), chalk)
import HWM.Core.Parsing (Parse (..), ParseCLI (parseCLI))
import HWM.Core.Version (VersionChange (..), nextVersion)
import HWM.Domain.Bounds (versionBounds)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.UI (putLine, sectionConfig, sectionTableM)
import Options.Applicative (argument, help, metavar)
import Options.Applicative.Builder (str)
import Relude

size :: Int
size = 16

newtype VersionOptions = VersionOptions {bump :: Maybe VersionChange} deriving (Show)

instance ParseCLI VersionOptions where
  parseCLI = VersionOptions <$> optional (argument (str >>= parse) (metavar "BUMP" <> help "Bump the version. Can be one of: major, minor, patch, or a specific version like 1.2.3"))

bumpVersion :: VersionChange -> Config -> ConfigT Config
bumpVersion (BumpVersion bump) Config {..} = do
  let version' = nextVersion bump version
  sectionTableM
    size
    ("bump version (" <> format bump <> ")")
    [ ("from", pure $ format version),
      ("to", pure $ chalk Cyan (format version'))
    ]

  let bounds' = versionBounds version'
  pure Config {version = version', bounds = bounds', ..}
bumpVersion (FixedVersion version') Config {..} = do
  sectionTableM
    size
    "set version"
    [ ("from", pure $ format version),
      ("to", pure $ chalk Cyan (format version'))
    ]

  let bounds' = versionBounds version'
  pure Config {version = version', bounds = bounds', ..}

runVersion :: VersionOptions -> ConfigT ()
runVersion (VersionOptions (Just bump)) = (bumpVersion bump `updateConfig`) $ do
  sectionConfig size [("hwm.yaml", pure $ chalk Green "✓")]
  syncPackages
runVersion (VersionOptions Nothing) = do
  putLine . format . version =<< asks config
  exitSuccess
