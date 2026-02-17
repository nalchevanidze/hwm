{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Formatting
  ( Color (..),
    chalk,
    formatStatus,
    Status (..),
    padDots,
    genMaxLen,
    separator,
    deriveStatus,
    statusFromSeverity,
    statusIcon,
    isOk,
    displayStatus,
    Format (..),
    availableOptions,
    renderSummaryStatus,
    subPathSign,
    slugify,
    commonPrefix,
    indentBlockNum,
    indentBlock,
    formatTable,
    formatList,
    monadStatus,
  )
where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable (maximum)
import qualified Data.List as List
import Data.Text (pack)
import qualified Data.Text as T
import HWM.Core.Result (MonadIssue (catchIssues), Severity (..))
import Relude

data Color
  = Red
  | Green
  | Yellow
  | Gray
  | Magenta
  | Cyan
  | Dim
  | Bold
  | White
  | None
  | RedBackground
  | GreenBackground
  | YellowBackground
  | BrightRed
  | BrightGreen
  | BrightYellow

toColor :: Color -> Text
toColor c = "\x1b[" <> T.pack (show (colorCode c)) <> "m"

colorCode :: Color -> Int
colorCode Red = 31
colorCode Green = 32
colorCode Yellow = 33
colorCode Cyan = 36
colorCode Magenta = 95
colorCode Gray = 90
colorCode Dim = 2
colorCode None = 0
colorCode Bold = 1
colorCode RedBackground = 41
colorCode GreenBackground = 42
colorCode YellowBackground = 43
colorCode BrightRed = 91
colorCode BrightGreen = 92
colorCode BrightYellow = 93
colorCode White = 37

chalk :: Color -> Text -> Text
chalk c x = toColor c <> x <> toColor None

data Status = Checked | Updated | Warning | Invalid
  deriving (Show, Eq, Ord)

deriveStatus :: [Status] -> Status
deriveStatus [] = Checked
deriveStatus statuses = maximum statuses

-- | Convert issue severity to status
statusFromSeverity :: Maybe Severity -> Status
statusFromSeverity (Just SeverityError) = Invalid
statusFromSeverity (Just SeverityWarning) = Warning
statusFromSeverity Nothing = Checked

monadStatus :: (Functor m, MonadIssue m) => m b -> m Status
monadStatus x = statusFromSeverity . fst <$> catchIssues x

displayStatus :: [(Text, Status)] -> Text
displayStatus ls =
  let status = deriveStatus (map snd ls)
   in if isOk status then statusIcon status else formatStatus ls

padDots :: Int -> Text -> Text
padDots width s = s <> " " <> chalk Dim (T.replicate (max 0 (width - T.length s)) ".") <> " "

isOk :: Status -> Bool
isOk Checked = True
isOk Updated = True
isOk _ = False

labelColor :: Status -> Color
labelColor Checked = Dim
labelColor Updated = Dim
labelColor Warning = Yellow
labelColor Invalid = Red

statusIcon :: Status -> Text
statusIcon s = case s of
  Checked -> chalk Green "✓"
  Updated -> chalk Cyan "⟳"
  Invalid -> chalk Red "✖"
  Warning -> chalk Yellow "!"

formatStatus :: [(Text, Status)] -> Text
formatStatus = T.intercalate (chalk Dim " ") . map formatItem . sortBy (comparing (Down . snd)) . filter ((/= Checked) . snd)
  where
    formatItem (label, s) = statusIcon s <> " " <> chalk (labelColor s) label

genMaxLen :: [Text] -> Int
genMaxLen names = if null names then 16 else maximum (map T.length names) + 4

separator :: Int -> Text
separator size = chalk Gray $ T.replicate size "─"

class Format a where
  format :: a -> Text

instance Format Int where
  format = show

instance Format String where
  format = pack

instance Format Text where
  format = id

instance Format Value where
  format = format . unpack . encode

availableOptions :: (Format a) => [a] -> Text
availableOptions xs = "Available options: " <> T.intercalate ", " (map format xs)

boxed :: Color -> Text -> Text
boxed color text = chalk Bold "• " <> block <> " " <> chalk color text <> " " <> block
  where
    block = chalk color "▌"

renderSummaryStatus :: Status -> Text
renderSummaryStatus Warning = boxed BrightYellow "warning"
renderSummaryStatus Invalid = boxed Red "errors"
renderSummaryStatus _ = boxed BrightGreen "success"

subPathSign :: Text
subPathSign = chalk Dim "└─- "

slugify :: Text -> Text
slugify = T.map replaceChar . T.toLower
  where
    replaceChar c
      | c == ' ' = '-'
      | c == '_' = '-'
      | c == '.' = '-'
      | otherwise = c

commonPrefix :: [Text] -> (Maybe Text, [Text])
commonPrefix [] = (Nothing, [])
commonPrefix [name] = (Nothing, [name])
commonPrefix names =
  refine names (T.dropWhileEnd (== '-') (List.foldl1' pairwisePrefix names))
  where
    pairwisePrefix a b = maybe "" (\(prefix, _, _) -> prefix) (T.commonPrefixes a b)
    refine _ candidate | T.null candidate = (Nothing, names)
    refine sources candidate
      | all (matches candidate) sources = (Just candidate, map (dropPrefix candidate) sources)
      | otherwise = refine sources (shrink candidate)
    matches prefix name
      | prefix == name = True
      | prefix `T.isPrefixOf` name = startsWithHyphen (T.drop (T.length prefix) name)
      | otherwise = False
    startsWithHyphen remainder =
      case T.uncons remainder of
        Nothing -> True
        Just ('-', _) -> True
        _ -> False
    shrink prefix =
      case T.breakOnEnd "-" prefix of
        (rest, _) | T.null rest -> ""
        (rest, _) -> T.dropWhileEnd (== '-') rest

    dropPrefix prefix text =
      let name = fromMaybe "" (T.stripPrefix prefix text)
       in if T.null name
            then "."
            else fromMaybe name (T.stripPrefix "-" name)

indentBlockNum :: Int -> Text -> Text
indentBlockNum i = indentBlock (T.replicate (i * 2) " ")

indentBlock :: Text -> Text -> Text
indentBlock prefix text
  | T.null text = text
  | otherwise =
      let linesList = T.lines text
          indented = map (prefix <>) linesList
          joined = T.intercalate "\n" indented
       in if T.isSuffixOf "\n" text
            then joined <> "\n"
            else joined

type Table = [Row]

type Row = [Text]

getSizes :: Table -> [Int]
getSizes xs = map size (transpose xs)
  where
    size :: Row -> Int
    size = maximum . map T.length

printRow :: [Int] -> Row -> Text
printRow sizes ls =
  T.strip
    $ T.intercalate "  "
    $ zipWith (\item s -> T.justifyLeft s ' ' item) ls sizes

formatTable :: [Text] -> [Text]
formatTable deps = sort $ map (printRow (getSizes table)) table
  where
    table = map words deps

formatList :: (Format a) => Text -> [a] -> Text
formatList x = T.intercalate x . map format
