{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Domain.Bounds
  ( Bounds (..),
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
    auditHasAny,
    TestedRange (..),
    deriveBounds,
  )
where

import Control.Monad.Except
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
  )
import HWM.Core.Formatting (Color (..), Format (..), chalk, formatList, padDots)
import HWM.Core.Has (Has)
import HWM.Core.Parsing (Parse (..), fromToString, removeHead, sepBy, unconsM)
import HWM.Core.Pkg (PkgName)
import HWM.Core.Result (Issue)
import HWM.Core.Version (Bump (..), Version, dropPatch, nextVersion)
import HWM.Runtime.Cache (Cache, Snapshot, getVersion, getVersions)
import HWM.Runtime.UI
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

auditBound :: Maybe Bound -> Maybe Version -> (Version -> Version -> Bool) -> BoundAudit
auditBound registryBound matrixVersion isConflict
  | null registryBound = BoundAudit {auditStatus = Missing, ..}
  | match isConflict registryBound matrixVersion = BoundAudit {auditStatus = Conflict, ..}
  | match (flip isConflict) registryBound matrixVersion = BoundAudit {auditStatus = Unverified, ..}
  | otherwise = BoundAudit {auditStatus = Valid, ..}
  where
    match :: (Version -> Version -> Bool) -> Maybe Bound -> Maybe Version -> Bool
    match f (Just Bound {version}) (Just target) = f version target
    match _ _ _ = False

updateBound :: Bool -> Restriction -> BoundCompliance -> Maybe Bound -> Maybe Version -> Maybe Bound
updateBound forceOverride res compliance registryBound matrixVersion
  | compliance == Conflict = preferMatrix
  | forceOverride && compliance == Unverified = preferMatrix
  | otherwise = registryBound <|> matrixBound
  where
    preferMatrix = matrixBound <|> registryBound
    matrixBound = Bound res True <$> matrixVersion

auditBounds :: TestedRange -> PkgName -> Bounds -> BoundsAudit
auditBounds TestedRange {..} name Bounds {..} =
  BoundsAudit
    { auditPkgName = name,
      auditMinBound = auditBound lowerBound (getVersion name legacy) (>),
      auditMaxBound = auditBound upperBound (getVersion name nightly) (<)
    }

updateDepBounds :: Bool -> TestedRange -> PkgName -> Bounds -> Bounds
updateDepBounds forceOverride TestedRange {..} name Bounds {..} =
  Bounds
    { lowerBound = updateBound forceOverride Min (auditStatus $ auditMinBound audit) lowerBound (getVersion name legacy),
      upperBound = updateBound forceOverride Max (auditStatus $ auditMaxBound audit) upperBound (getVersion name nightly)
    }
  where
    audit = auditBounds TestedRange {..} name Bounds {..}

auditHasAny :: (BoundCompliance -> Bool) -> BoundsAudit -> Bool
auditHasAny f BoundsAudit {..} = any (f . auditStatus) [auditMinBound, auditMaxBound]

data BoundCompliance
  = Conflict
  | Unverified
  | Missing
  | Valid
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

formatBoundAudit :: BoundAudit -> [Text]
formatBoundAudit (BoundAudit Nothing Nothing s) = ["", formatStatus s]
formatBoundAudit (BoundAudit Nothing (Just x) s) = [chalk Dim (format x), formatStatus s]
formatBoundAudit (BoundAudit (Just x) Nothing s) = [chalk Dim (format x), formatStatus s]
formatBoundAudit (BoundAudit (Just reg) (Just mat) s) = [chalk Dim (format reg), formatStatus s <> "  " <> format mat]

data BoundsAudit = BoundsAudit
  { auditPkgName :: PkgName,
    auditMinBound :: BoundAudit,
    auditMaxBound :: BoundAudit
  }

formatAudit :: BoundsAudit -> [Text]
formatAudit a = [format $ auditPkgName a] <> formatBoundAudit (auditMinBound a) <> [chalk Dim "  &&  "] <> formatBoundAudit (auditMaxBound a)

data TestedRange = TestedRange
  { legacy :: Snapshot,
    nightly :: Snapshot
  }

deriveBounds :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Cache, MonadUI m) => PkgName -> TestedRange -> m Bounds
deriveBounds name TestedRange {..} = do
  let lower = getVersion name legacy
  let upper = getVersion name nightly

  newUpper <- maybe (head <$> getVersions name) pure upper

  section "discovery" $ do
    putLine $ padDots 16 "registry" <> "missing (initiating lookup)"
    putLine $ padDots 16 "legacy" <> maybe (chalk Red "missing") (chalk Green . format) lower <> " (min)"
    putLine $ padDots 16 "nightly" <> maybe (chalk Red "missing") ((<> " (max)") . chalk Green . format) upper
    unless (isJust lower || isJust upper) $ putLine $ padDots 16 "hackage" <> chalk Green (format newUpper) <> " (max)"

  pure
    Bounds
      { lowerBound = Bound Min True <$> lower,
        upperBound = Just (Bound Max True newUpper)
      }
