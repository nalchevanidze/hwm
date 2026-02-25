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
import qualified Data.Map as M
import HWM.Core.Common (Name)
import HWM.Core.Formatting (Format (..))
import HWM.Core.Options (Options (..), askOptions)
import HWM.Core.Pkg (Pkg (..), PkgName, pkgFile, pkgYamlPath)
import HWM.Domain.ConfigT (ConfigT)
import HWM.Domain.Workspace (allPackages)
import HWM.Integrations.Toolchain.Lib (Libraries, Library (..))
import HWM.Integrations.Toolchain.Package (Package (..))
import HWM.Runtime.Files (readYaml, rewrite_)
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

(<:>) :: (Semigroup a, IsString a) => a -> a -> a
(<:>) name tag = name <> ":" <> tag

genComponents :: Pkg -> ConfigT [Component]
genComponents path = do
  Package {..} <- readYaml (pkgYamlPath path)
  pure
    $ comp name "lib" library
    <> compGroup name "test" tests
    <> compGroup name "exe" executables
    <> compGroup name "bench" benchmarks
  where
    compGroup :: PkgName -> Text -> Maybe Libraries -> [Component]
    compGroup name tag = concatMap mkComp . concatMap M.toList . maybeToList
      where
        mkComp (k, lib) = comp name (tag <:> k) (Just lib)
    comp :: PkgName -> Text -> Maybe Library -> [Component]
    comp name tag (Just Library {sourceDirs}) =
      [ Component
          { path = "./" <> pkgFile path (toString sourceDirs),
            component = format name <:> tag
          }
      ]
    comp _ _ _ = []

syncHie :: ConfigT ()
syncHie = do
  Options {..} <- askOptions
  pkgs <- allPackages
  components <- concat <$> traverse genComponents pkgs
  rewrite_ optionsHie (const $ pure $ packHie Components {stackYaml = optionsStack, components})
