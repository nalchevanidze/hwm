{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Parsing
  ( parseField,
    fromByteString,
    firstWord,
    removeHead,
    unconsM,
    sepBy,
    fromToString,
    SourceText,
    genUrl,
    Parse (..),
    parseOptions,
  )
where

import Data.ByteString.Char8 (unpack)
import Data.Char (isSeparator)
import Data.Text
  ( break,
    drop,
    intercalate,
    pack,
    singleton,
    splitOn,
    strip,
    uncons,
  )
import qualified Data.Text as T
import Relude hiding
  ( break,
    drop,
    head,
    intercalate,
    isPrefixOf,
    null,
    uncons,
    words,
  )

type SourceText = Text

parseOptions :: [Text] -> [Text]
parseOptions raw = raw >>= (map T.strip . T.splitOn ",")

parseField :: SourceText -> (SourceText, SourceText)
parseField = second (strip . drop 1) . breakAt (== ':')

firstWord :: SourceText -> (SourceText, SourceText)
firstWord = breakAt isSeparator

fromByteString :: ByteString -> SourceText
fromByteString = pack . unpack

ignoreSpaces :: SourceText -> SourceText
ignoreSpaces = T.filter (not . isSeparator)

breakAt :: (Char -> Bool) -> SourceText -> (SourceText, SourceText)
breakAt f = bimap strip strip . break f . strip

sepBy :: (MonadFail m, Parse a) => SourceText -> SourceText -> m [a]
sepBy sep = traverse parse . splitOn sep . ignoreSpaces

removeHead :: Char -> SourceText -> (Bool, SourceText)
removeHead should txt = maybe (False, txt) has (uncons txt)
  where
    has (x, xs)
      | x == should = (True, xs)
      | otherwise = (False, txt)

unconsM :: (MonadFail m) => String -> SourceText -> m (SourceText, SourceText)
unconsM m x = first singleton <$> maybe (fail $ m <> "<>: " <> toString x) pure (uncons x)

fromToString :: (ToString a) => a -> SourceText
fromToString = pack . toString

genUrl :: Text -> [Text] -> Text
genUrl domain = intercalate "/" . (domain :)

class Parse a where
  parse :: (MonadFail m) => Text -> m a

instance Parse Int where
  parse t =
    maybe (fail $ "Could not parse Int: '" <> toString t <> "'!") pure (readMaybe $ toString t)
