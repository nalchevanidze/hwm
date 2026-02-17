{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Bounds
  ( Bounds,
    BoundsByName,
    versionBounds,
    updateDepBounds,
    Bound (..),
    Restriction (..),
    hasBounds,
    boundsBetter,
    auditBounds,
    BoundAudit (..),
    BoundCompliance (..),
    BoundsAudit (..),
    formatAudit,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
  )
import Data.List (maximum, minimum)
import HWM.Core.Formatting (Color (..), Format (..), chalk, formatList)
import HWM.Core.Parsing (Parse (..), fromToString, removeHead, sepBy, unconsM)
import HWM.Core.Pkg (PkgName)
import HWM.Core.Version (Bump (..), Version, dropPatch, nextVersion)
import HWM.Runtime.Cache (Snapshot, getVersion)
import Relude

data Restriction = Min | Max deriving (Show, Eq, Ord)

instance Parse Restriction where
  parse ">" = pure Min -- > 0.7.0
  parse "<" = pure Max -- <  1.0.0
  parse x = fail ("unsorted bound type" <> toString x)

instance ToString Restriction where
  toString Min = ">" -- >  0.7.0
  toString Max = "<" -- <  1.0.0

instance ToText Restriction where
  toText = fromToString

data Bound = Bound
  { restriction :: Restriction,
    orEquals :: Bool,
    version :: Version
  }
  deriving (Show, Eq)

instance Format Bound where
  format Bound {..} = unwords $ (toText restriction <> eq) : [toText version]
    where
      eq = if orEquals then "=" else " "

instance Ord Bound where
  compare a b =
    compare (version a) (version b)
      <> compare (restriction a) (restriction b)
      <> compare (orEquals a) (orEquals b)

instance Parse Bound where
  parse txt = do
    (ch, str) <- unconsM "unsorted bound type" txt
    let (orEquals, value) = removeHead '=' str
    restriction <- parse ch
    version <- parse value
    pure Bound {..}

data Bounds = Bounds
  { lowerBound :: Maybe Bound,
    upperBound :: Maybe Bound
  }
  deriving (Generic, Show, Eq)

type BoundsByName = '[]

instance Parse Bounds where
  parse "" = pure $ Bounds Nothing Nothing
  parse str = do
    bounds <- sepBy "&&" str
    let lower = find (\Bound {..} -> restriction == Min) bounds
    let upper = find (\Bound {..} -> restriction == Max) bounds
    pure $ Bounds lower upper

instance Format Bounds where
  format (Bounds lower upper) = formatList " && " $ sort $ catMaybes [lower, upper]

instance ToString Bounds where
  toString = toString . format

instance FromJSON Bounds where
  parseJSON (String p) = parse p
  parseJSON v = fail $ fromString ("cant parse Bounds expected string got" <> toString (format v))

instance ToJSON Bounds where
  toJSON = String . fromToString

versionBounds :: Version -> Bounds
versionBounds version =
  Bounds
    { lowerBound = Just $ Bound Min True (dropPatch version),
      upperBound = Just $ Bound Max False (nextVersion Minor version)
    }

hasBounds :: Bounds -> Bool
hasBounds Bounds {..} = isJust lowerBound || isJust upperBound

boundsScore :: Bounds -> Int
boundsScore Bounds {..} = length (maybeToList lowerBound) + length (maybeToList upperBound)

boundsBetter :: Bounds -> Bounds -> Bool
boundsBetter a b = boundsScore a > boundsScore b

compareBound :: (Version -> t -> Bool) -> Maybe Bound -> Maybe t -> Bool
compareBound f (Just Bound {version}) (Just target) = f version target
compareBound _ _ _ = False

auditBound :: Maybe Bound -> Maybe Version -> (Version -> Version -> Bool) -> BoundAudit
auditBound registryBound matrixVersion isConflict 
  | null registryBound = BoundAudit {auditStatus = Missing, ..}
  | compareBound isConflict registryBound matrixVersion =
      BoundAudit {auditStatus = Conflict, ..}
  | compareBound (flip isConflict) registryBound matrixVersion =
      BoundAudit {auditStatus = Unverified, ..}
  | otherwise = BoundAudit {auditStatus = Valid, ..}

auditBounds :: Snapshot -> Snapshot -> PkgName -> Bounds -> BoundsAudit
auditBounds legacy nightly name Bounds {..} =
  BoundsAudit
    { auditPkgName = name,
      minBound = auditBound lowerBound (getVersion name legacy) (>) ,
      maxBound = auditBound upperBound (getVersion name nightly) (<)
    }

pickBy :: Maybe Version -> Maybe Bound -> ([Bound] -> Bound) -> Maybe Bound
pickBy Nothing registry _ = registry
pickBy (Just stackage) registry f = Just $ f (Bound Min True stackage : toList registry)

updateDepBounds :: Snapshot -> Snapshot -> PkgName -> Bounds -> Bounds
updateDepBounds legacy nightly name Bounds {..} =
  Bounds
    { lowerBound = pickBy (getVersion name legacy) lowerBound minimum,
      upperBound = pickBy (getVersion name nightly) upperBound maximum
    }

data BoundCompliance
  = Conflict -- Was: AboveRecommended (The "Red" scenario)
  | Unverified -- Was: BelowRecommended (The "Yellow" scenario)
  | Missing -- Was: NoBound
  | Valid -- Was: Compliant
  deriving (Show, Eq)

data BoundAudit = BoundAudit
  { registryBound :: Maybe Bound,
    matrixVersion :: Maybe Version,
    auditStatus :: BoundCompliance
  }
  deriving (Show, Eq)

formatStatus :: BoundCompliance -> Text
formatStatus Conflict = chalk Red "-> "
formatStatus Unverified = chalk Yellow "-> "
formatStatus Missing = chalk Cyan " ! "
formatStatus Valid = chalk Green " ✓ "

formatAudit :: BoundAudit -> [Text]
formatAudit (BoundAudit Nothing Nothing s) = ["", formatStatus s]
formatAudit (BoundAudit Nothing (Just x) s) = [chalk Dim (format x), formatStatus s]
formatAudit (BoundAudit (Just x) Nothing s) = [chalk Dim (format x), formatStatus s]
formatAudit (BoundAudit (Just reg) (Just mat) s) = [chalk Dim (format reg), formatStatus s <> "  " <> format mat]

data BoundsAudit = BoundsAudit
  { auditPkgName :: PkgName,
    minBound :: BoundAudit,
    maxBound :: BoundAudit
  }