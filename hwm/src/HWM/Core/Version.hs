{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Version
  ( Version,
    Bump (..),
    askVersion,
    nextVersion,
    dropPatch,
    parseGHCVersion,
    VersionChange (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
  )
import qualified Data.Text as T
import GHC.Show (Show (..))
import HWM.Core.Formatting (Format (..), formatList)
import HWM.Core.Has (Has (obtain))
import HWM.Core.Parsing (Parse (..), fromToString, sepBy)
import Relude hiding (show)

data Version = Version
  { major :: Int,
    minor :: Int,
    revision :: [Int]
  }
  deriving
    ( Generic,
      Eq
    )

askVersion :: (MonadReader env m, Has env Version) => m Version
askVersion = asks obtain

getNumber :: [Int] -> Int
getNumber (n : _) = n
getNumber [] = 0

nextVersion :: Bump -> Version -> Version
nextVersion Major Version {..} = Version {major = major + 1, minor = 0, revision = [0], ..}
nextVersion Minor Version {..} = Version {minor = minor + 1, revision = [0], ..}
nextVersion Patch Version {..} = Version {revision = [getNumber revision + 1], ..}

dropPatch :: Version -> Version
dropPatch Version {..} = Version {revision = [0], ..}

compareSeries :: (Ord a) => [a] -> [a] -> Ordering
compareSeries [] _ = EQ
compareSeries _ [] = EQ
compareSeries (x : xs) (y : ys)
  | x == y = compareSeries xs ys
  | otherwise = compare x y

instance Format Version where
  format = formatList "." . toSeries

instance Parse Version where
  parse s = either (fail . toString . (prefix <>)) pure (sepBy "." s >>= fromSeries)
    where
      prefix = "invalid version(" <> s <> ")" <> ": "

fromSeries :: (MonadFail m) => [Int] -> m Version
fromSeries [] = fail "version should have at least one number!"
fromSeries [major] = pure Version {major, minor = 0, revision = []}
fromSeries (major : (minor : revision)) = pure Version {..}

toSeries :: Version -> [Int]
toSeries Version {..} = [major, minor] <> revision

instance ToString Version where
  toString = toString . format

instance Ord Version where
  compare a b = compareSeries (toSeries a) (toSeries b)

instance Show Version where
  show = toString

instance ToText Version where
  toText = fromToString

instance FromJSON Version where
  parseJSON (String s) = parse s
  parseJSON (Number n) = parse (fromToString $ show n)
  parseJSON v = fail $ "version should be either true or string" <> toString (format v)

instance ToJSON Version where
  toJSON = String . toText

data Bump
  = Major
  | Minor
  | Patch
  deriving
    ( Generic,
      Eq
    )

instance Parse Bump where
  parse "major" = pure Major
  parse "minor" = pure Minor
  parse "patch" = pure Patch
  parse v = fail $ "Invalid bump type: " <> toString (fromToString v)

instance ToString Bump where
  toString Major = "major"
  toString Minor = "minor"
  toString Patch = "patch"

instance Format Bump where
  format Major = "major"
  format Minor = "minor"
  format Patch = "patch"

instance Show Bump where
  show = toString

instance ToText Bump where
  toText = fromToString

instance FromJSON Bump where
  parseJSON (String s) = parse s
  parseJSON v = fail $ "Invalid bump type: " <> show v

instance ToJSON Bump where
  toJSON = String . toText

parseGHCVersion :: (MonadFail m) => Text -> m Version
parseGHCVersion text = parse (fromMaybe text (T.stripPrefix "ghc-" text))

data VersionChange = FixedVersion Version | BumpVersion Bump deriving (Show)

isBump :: Text -> Bool
isBump = (`elem` ["major", "minor", "patch"])

instance Parse VersionChange where
  parse x
    | isBump x = BumpVersion <$> parse x
    | otherwise = FixedVersion <$> parse x
