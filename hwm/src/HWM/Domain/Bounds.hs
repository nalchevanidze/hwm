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

import Control.Monad.Except (MonadError)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
  )
import Data.List (maximum, minimum)
import HWM.Core.Formatting (Color (..), Format (..), chalk, formatList)
import HWM.Core.Has (Has)
import HWM.Core.Parsing (Parse (..), fromToString, removeHead, sepBy, unconsM)
import HWM.Core.Pkg (PkgName)
import HWM.Core.Result (Issue (..), MonadIssue)
import HWM.Core.Version (Bump (..), Version, dropPatch, nextVersion)
import HWM.Runtime.Cache (Cache, Snapshot, getVersion)
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

isMore :: Maybe Bound -> Maybe Version -> Bool
isMore (Just Bound {version}) (Just target) = version > target
isMore _ _ = False

isLess :: Maybe Bound -> Maybe Version -> Bool
isLess (Just Bound {version}) (Just target) = version < target
isLess _ _ = False

auditLowerBound :: Maybe Bound -> Maybe Version -> BoundAudit
auditLowerBound registryBound matrixVersion
  | null registryBound = BoundAudit {auditStatus = Missing, ..}
  | isMore registryBound matrixVersion = BoundAudit {auditStatus = Conflict, ..}
  | isLess registryBound matrixVersion = BoundAudit {auditStatus = Unverified, ..}
  | otherwise = BoundAudit {auditStatus = Valid, ..}

auditUpperBound :: Maybe Bound -> Maybe Version -> BoundAudit
auditUpperBound registryBound matrixVersion
  | null registryBound = BoundAudit {auditStatus = Missing, ..}
  | isLess registryBound matrixVersion =
      BoundAudit {auditStatus = Conflict, ..}
  | isMore registryBound matrixVersion =
      BoundAudit {auditStatus = Unverified, ..}
  | otherwise = BoundAudit {auditStatus = Valid, ..}

auditBounds :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Cache, MonadIssue m) => Snapshot -> Snapshot -> PkgName -> Bounds -> m BoundsAudit
auditBounds legacy bleedingEdge name Bounds {..} = do
  pure
    $ BoundsAudit
      { auditPkgName = name,
        minBound = auditLowerBound lowerBound (getVersion name legacy),
        maxBound = auditUpperBound upperBound (getVersion name bleedingEdge)
      }

updateDepBounds :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Cache, MonadIssue m) => Snapshot -> Snapshot -> PkgName -> Bounds -> m Bounds
updateDepBounds legacy bleedingEdge name Bounds {..} = do
  let newVersion = maximum (toList upperBound <> toList (Bound Max True <$> getVersion name bleedingEdge))
  let minVersion = minimum (toList lowerBound <> toList (Bound Min True <$> getVersion name legacy))
  pure (Bounds {lowerBound = Just minVersion, upperBound = Just newVersion})

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