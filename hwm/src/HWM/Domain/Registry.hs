{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Registry
  ( Registry (..),
    getBounds,
    getDependencies,
    lookupBounds,
    addDependency,
    initRegistry,
    mapWithName,
    mapDeps,
  )
where

import Control.Monad.Error.Class (MonadError)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
  )
import Data.Foldable (Foldable (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import HWM.Core.Formatting (Format (..), formatTableRow)
import HWM.Core.Pkg (PkgName)
import HWM.Core.Result (Issue)
import HWM.Domain.Bounds (Bounds)
import HWM.Domain.Dependencies (Dependency (..), fromDependencyList, normalizeDependencies, unpackDeps)
import HWM.Runtime.Files (select)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    length,
    null,
    show,
    toList,
  )

newtype Registry = Registry {unpackRegistry :: Map PkgName Bounds}
  deriving (Show)

instance FromJSON Registry where
  parseJSON (Array xs) = Registry . unpackDeps <$> parseJSON (Array xs)
  parseJSON v = Registry <$> parseJSON v

instance ToJSON Registry where
  toJSON (Registry ms) = toJSON . Map.mapWithKey formatTable $ ms
    where
      formatTable key value =
        let padding = T.replicate (size - T.length (format key)) " "
         in String (padding <> formatTableRow table (T.words (format value)))
      size = maximum $ map (T.length . format) $ Map.keys ms
      table = map (T.words . format) $ Map.elems ms


getBounds :: (MonadError Issue m) => PkgName -> Registry -> m Bounds
getBounds pkgName = select "Package " pkgName . unpackRegistry

lookupBounds :: PkgName -> Registry -> Maybe Bounds
lookupBounds pkgName registry = Map.lookup pkgName (unpackRegistry registry)

getDependencies :: Registry -> [Dependency]
getDependencies (Registry m) = map (uncurry Dependency) $ Map.toList m

addDependency :: Dependency -> Registry -> Registry
addDependency (Dependency n b) (Registry m) = Registry $ Map.insert n b m

initRegistry :: [PkgName] -> [Dependency] -> Registry
initRegistry internalPkgs deps =
  let externals = filter isExternal (normalizeDependencies deps)
   in Registry . unpackDeps . fromDependencyList $ sortOn name externals
  where
    internals = Set.fromList internalPkgs
    isExternal dep = not (Set.member (name dep) internals)

mapWithName :: (PkgName -> Bounds -> b) -> Registry -> [b]
mapWithName f (Registry xs) = Map.elems $ Map.mapWithKey f xs

mapDeps :: (PkgName -> Bounds -> Bounds) -> Registry -> Registry
mapDeps f (Registry xs) = Registry $ Map.mapWithKey f xs
