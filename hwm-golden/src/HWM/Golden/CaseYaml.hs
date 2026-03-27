{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Golden.CaseYaml (writeCaseFileOrdered) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import HWM.Golden.Json (dropEmpty)
import HWM.Golden.Types (CaseExpect (..), CaseFile (..), CaseRunner (..), ExpectedFiles (..), RunnerBin (..), RunnerBinTrace (..))
import Relude

writeCaseFileOrdered :: FilePath -> CaseFile -> IO ()
writeCaseFileOrdered path caseFile = writeFileText path (renderCaseFile caseFile)

renderCaseFile :: CaseFile -> Text
renderCaseFile CaseFile {..} =
  T.unlines
    $ concat
      [ fieldText "name" caseName,
        fieldBlock "notes" caseNotes,
        ["project: " <> toText caseProject],
        ["command: " <> toText caseCommand],
        maybe [] renderRunner caseRunner,
        maybe [] renderExpect caseExpect
      ]

renderRunner :: CaseRunner -> [Text]
renderRunner CaseRunner {..} =
  let sections =
        concat
          [ maybe [] renderRunnerBins runnerBin,
            maybe [] (renderMap "env") runnerEnv,
            maybe [] (renderList "path") runnerPath
          ]
   in if null sections then [] else "runner:" : sections

renderRunnerBins :: Map.Map String RunnerBin -> [Text]
renderRunnerBins bins
  | Map.null bins = []
  | otherwise = "  bin:" : concatMap renderEntry (Map.toAscList bins)
  where
    renderEntry (name, RunnerBin {runnerBinSrc, runnerBinTrace}) =
      case runnerBinTrace of
        Nothing -> ["    " <> toText name <> ": " <> renderYamlScalar runnerBinSrc]
        Just traceCfg@(RunnerBinTrace {runnerBinTraceEnv, runnerBinTraceFiles})
          | null runnerBinTraceEnv && null runnerBinTraceFiles -> ["    " <> toText name <> ": " <> renderYamlScalar runnerBinSrc]
          | otherwise ->
              [ "    " <> toText name <> ":",
                "      src: " <> renderYamlScalar runnerBinSrc,
                "      trace:"
              ]
                <> renderTrace traceCfg

    renderTrace RunnerBinTrace {runnerBinTraceEnv, runnerBinTraceFiles} =
      concat
        [ if null runnerBinTraceEnv then [] else ["        env:"] <> ["          - " <> toText v | v <- runnerBinTraceEnv],
          if null runnerBinTraceFiles then [] else ["        files:"] <> ["          - " <> toText v | v <- runnerBinTraceFiles]
        ]

renderExpect :: CaseExpect -> [Text]
renderExpect CaseExpect {..} =
  let sections =
        concat
          [ ["  failure: true" | caseFailure],
            maybe [] renderExpectedFiles caseFiles,
            maybe [] renderCalls caseCalls
          ]
   in if null sections then [] else "expect:" : sections

renderExpectedFiles :: ExpectedFiles -> [Text]
renderExpectedFiles ExpectedFiles {added, deleted, modified, touched} =
  let sections =
        concat
          [ renderNestedList "added" added,
            renderNestedList "deleted" deleted,
            renderNestedList "modified" modified,
            renderNestedList "touched" touched
          ]
   in if null sections then [] else "  files:" : sections

renderCalls :: Value -> [Text]
renderCalls v =
  let normalized = dropEmpty v
   in case normalized of
        Object o | KM.null o -> []
        _ ->
          let encoded = T.lines (TE.decodeUtf8 (Yaml.encode normalized))
           in "  calls:" : map ("    " <>) encoded

renderMap :: Text -> Map.Map String String -> [Text]
renderMap _ m | Map.null m = []
renderMap label m =
  ["  " <> label <> ":"]
    <> ["    " <> toText k <> ": " <> renderYamlScalar v | (k, v) <- Map.toAscList m]

renderYamlScalar :: String -> Text
renderYamlScalar s
  | isSafeUnquoted s = toText s
  | otherwise = quoteYaml s

isSafeUnquoted :: String -> Bool
isSafeUnquoted [] = False
isSafeUnquoted xs =
  all isSafeChar xs
    && not (all isDigit xs)
    && lowered
    `notElem` ambiguous
  where
    lowered = T.toLower (toText xs)
    ambiguous = ["true", "false", "null", "~", "yes", "no", "on", "off"]
    isSafeChar c = isAlphaNum c || c `elem` ("-_./" :: String)

quoteYaml :: String -> Text
quoteYaml s =
  let t = toText s
      escaped = T.replace "\"" "\\\"" (T.replace "\\" "\\\\" t)
   in "\"" <> escaped <> "\""

renderList :: Text -> [FilePath] -> [Text]
renderList _ [] = []
renderList label items =
  ["  " <> label <> ":"] <> ["    - " <> toText x | x <- items]

renderNestedList :: Text -> [FilePath] -> [Text]
renderNestedList _ [] = []
renderNestedList label items =
  ["    " <> label <> ":"] <> ["      - " <> toText x | x <- items]

fieldText :: Text -> Maybe Text -> [Text]
fieldText _ Nothing = []
fieldText label (Just v) = [label <> ": " <> v]

fieldBlock :: Text -> Maybe Text -> [Text]
fieldBlock _ Nothing = []
fieldBlock label (Just v) =
  let ls = T.lines v
   in (label <> ": |") : map ("  " <>) ls
