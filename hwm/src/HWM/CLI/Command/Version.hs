{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.CLI.Command.Version
  ( runVersion,
  )
where

import HWM.Core.Formatting (Color (..), Format (..), chalk)
import HWM.Core.Version (Bump, nextVersion)
import HWM.Domain.Bounds (versionBounds)
import HWM.Domain.Config (Config (..))
import HWM.Domain.ConfigT (ConfigT, config, updateConfig)
import HWM.Integrations.Toolchain.Package (syncPackages)
import HWM.Runtime.UI (putLine, sectionConfig, sectionTableM)
import Relude

size :: Int
size = 16

bumpVersion :: Bump -> Config -> ConfigT Config
bumpVersion bump Config {..} = do
  let version' = nextVersion bump version
  sectionTableM
    size
    ("bump version (" <> format bump <> ")")
    [ ("from", pure $ format version),
      ("to", pure $ chalk Cyan (format version'))
    ]

  let bounds' = versionBounds version'
  pure Config {version = version', bounds = bounds', ..}

runVersion :: Maybe Bump -> ConfigT ()
runVersion (Just bump) = (bumpVersion bump `updateConfig`) $ do
  sectionConfig size [("hwm.yaml", pure $ chalk Green "✓")]
  syncPackages
runVersion Nothing = do
  putLine . format . version =<< asks config
  exitSuccess
