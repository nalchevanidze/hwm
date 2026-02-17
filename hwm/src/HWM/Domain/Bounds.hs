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
    getBound,
    Bound (..),
    Restriction (..),
    hasBounds,
    boundsScore,
    boundsBetter,
    auditBounds,
    BoundAudit (..),
    BoundCompliance (..),
    formatLowerStatus,
    BoundsAudit (..),
  )
where

import Control.Monad.Except (MonadError)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
  )
import Data.List (maximum, minimum)
import qualified Data.Map as M
import HWM.Core.Formatting (Color (..), Format (..), chalk, formatList)
import HWM.Core.Has (Has)
import HWM.Core.Parsing (Parse (..), fromToString, removeHead, sepBy, unconsM)
import HWM.Core.Pkg (PkgName)
import HWM.Core.Result (Issue (..), MonadIssue)
import HWM.Core.Version (Bump (..), Version, dropPatch, nextVersion)
import HWM.Runtime.Cache (Cache, Snapshot (snapshotPackages), getVersions)
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
      eq = if orEquals then "=" else ""

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

newtype Bounds = Bounds [Bound]
  deriving (Generic, Show, Eq)

type BoundsByName = '[]

instance Parse Bounds where
  parse "" = pure $ Bounds []
  parse str = Bounds <$> sepBy "&&" str

instance Format Bounds where
  format (Bounds xs) = formatList " && " $ sort xs

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
    [ Bound Min True (dropPatch version),
      Bound Max False (nextVersion Minor version)
    ]

getBound :: Restriction -> Bounds -> [Bound]
getBound v (Bounds xs) = maybeToList $ find (\Bound {..} -> restriction == v) xs

getLower :: Bounds -> Maybe Bound
getLower = listToMaybe . getBound Min

getUpper :: Bounds -> Maybe Bound
getUpper = listToMaybe . getBound Max

hasBounds :: Bounds -> Bool
hasBounds b =
  let lower = getBound Min b
      upper = getBound Max b
   in not (null lower && null upper)

boundsScore :: Bounds -> Int
boundsScore b = length (getBound Min b) + length (getBound Max b)

boundsBetter :: Bounds -> Bounds -> Bool
boundsBetter a b = boundsScore a > boundsScore b

getLatest :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Cache) => PkgName -> m Bound
getLatest = fmap (Bound Max True . head) . getVersions

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
auditBounds legacy bleedingEdge name bounds = do
  pure
    $ BoundsAudit
      { auditPkgName = name,
        minBound = auditLowerBound (getLower bounds) (M.lookup name $ snapshotPackages legacy),
        maxBound = auditUpperBound (getUpper bounds) (M.lookup name $ snapshotPackages bleedingEdge)
      }

updateDepBounds :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Cache, MonadIssue m) => PkgName -> Bounds -> m Bounds
updateDepBounds name bounds = do
  latest <- getLatest name
  let upper = getBound Max bounds
  let newVersion = maximum (latest : upper)
  _min <- initiateMin name bounds
  pure (Bounds (_min <> [newVersion]))

initiateMin :: (MonadIO m, MonadError Issue m, MonadReader env m, Has env Cache) => PkgName -> Bounds -> m [Bound]
initiateMin name bounds = do
  let mi = getBound Min bounds
  if null mi
    then do
      ls <- fmap (Bound Min True) <$> getVersions name
      pure [minimum ls]
    else pure mi

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

formatLowerStatus :: BoundCompliance -> Version -> Text
formatLowerStatus Conflict version = chalk Red $ " ↓ (" <> format version <> ")"
formatLowerStatus Unverified version = chalk Yellow $ " ↑ (" <> format version <> ")"
formatLowerStatus Missing version = chalk Cyan $ " ○ (" <> format version <> ")"
formatLowerStatus Valid version = chalk Green $ " ✓ (" <> format version <> ")"

instance Format BoundAudit where
  format (BoundAudit Nothing Nothing _) = ""
  format (BoundAudit Nothing (Just x) _) = "○ " <> format x
  format (BoundAudit (Just x) Nothing _) = format x
  format (BoundAudit (Just reg) (Just mat) auditStatus) = format reg <> formatLowerStatus auditStatus mat

data BoundsAudit = BoundsAudit
  { auditPkgName :: PkgName,
    minBound :: BoundAudit,
    maxBound :: BoundAudit
  }