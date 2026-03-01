{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Integrations.Toolchain.Hie
  ( syncHie,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object)
import qualified Data.Text as T
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..))
import HWM.Core.Options (Options (..), askOptions)
import HWM.Core.Pkg (Pkg (..), pkgFile)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Workspace (allPackages)
import HWM.Integrations.Toolchain.Package (packageLibs)
import HWM.Runtime.Files (rewrite_)
import Relude

data Component = Component
  { path :: FilePath,
    component :: Name
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show
    )

data Components = Components
  { stackYaml :: FilePath,
    components :: [Component]
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show
    )

packHie :: Components -> Value
packHie value = object [("cradle", object [("stack", toJSON value)])]

genComponents :: Pkg -> ConfigT [Component]
genComponents pkg = concatMap comp <$> packageLibs pkg
  where
    comp (tag, sourceDirs) =
      [ Component
          { path = "./" <> pkgFile pkg (toString sourceDirs),
            component = tag
          }
      ]

syncHie :: ConfigT ()
syncHie = do
  Options {..} <- askOptions
  pkgs <- allPackages
  components <- concat <$> traverse genComponents pkgs
  rewrite_ optionsHie (const $ pure $ packHie Components {stackYaml = optionsStack, components})
