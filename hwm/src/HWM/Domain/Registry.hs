{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Registry
  ( Registry (..),
    getDependencies,
    lookupBounds,
    addDependency,
    deriveRegistry,
    mapWithName,
    mapDeps,
    askRegistry,
  )
where

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
import HWM.Core.Has (Has (obtain))
import HWM.Core.Pkg (IsPkg (..), PkgName)
import HWM.Domain.Bounds (Bounds)
import HWM.Domain.Dependencies (Dependency (..), HasDependencies, collectNormalizedDependencies, fromDependencyList, normalizeDependencies, unpackDeps)
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

askRegistry :: (MonadReader env m, Has env Registry) => m Registry
askRegistry = asks obtain

newtype Registry = Registry {unpackRegistry :: Map PkgName Bounds}
  deriving (Show)

instance Semigroup Registry where
  Registry a <> Registry b = Registry (a <> b)

instance Monoid Registry where
  mempty = Registry mempty

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

lookupBounds :: PkgName -> Registry -> Maybe Bounds
lookupBounds pkgName registry = Map.lookup pkgName (unpackRegistry registry)

getDependencies :: Registry -> [Dependency]
getDependencies (Registry m) = map (uncurry Dependency) $ Map.toList m

addDependency :: Dependency -> Registry -> Registry
addDependency Dependency {..} (Registry m) = Registry $ Map.insert hwmDepName hwmDepBounds m

deriveRegistry :: (HasDependencies a, IsPkg a) => [a] -> Registry
deriveRegistry packages =
  let deps = concatMap collectNormalizedDependencies packages
      externals = filter isExternal (normalizeDependencies deps)
   in Registry . unpackDeps . fromDependencyList $ sortOn hwmDepName externals
  where
    internals = Set.fromList (map getPkgName packages)
    isExternal dep = not (Set.member (hwmDepName dep) internals)

mapWithName :: (PkgName -> Bounds -> b) -> Registry -> [b]
mapWithName f (Registry xs) = Map.elems $ Map.mapWithKey f xs

mapDeps :: (PkgName -> Bounds -> Bounds) -> Registry -> Registry
mapDeps f (Registry xs) = Registry $ Map.mapWithKey f xs
