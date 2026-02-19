{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Runtime.UI
  ( MonadUI (..),
    UIT,
    runUIT,
    runUI,
    putLine,
    indent,
    section,
    sectionWorkspace,
    sectionEnvironments,
    sectionConfig,
    sectionTableM,
    forTable,
    printSummary,
    statusIndicator,
    runSpinner,
    printGenTable,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (MonadError (..))
import Data.List (groupBy, maximum, (!!))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import HWM.Core.Formatting (Color (..), Format (..), Status (..), chalk, indentBlockNum, padDots, renderSummaryStatus, subPathSign)
import HWM.Core.Result
  ( Issue (..),
    IssueDetails (..),
    ResultT (..),
    Severity (..),
  )
import Relude
import System.Console.ANSI (clearLine, setCursorColumn)

data UIContext m = UIContext
  { uiWriter :: Text -> m (),
    uiIndent :: Int
  }

newtype UIT m a = UIT {_unUIT :: ReaderT (UIContext m) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans UIT where
  lift = UIT . lift

instance (MonadError err m) => MonadError err (UIT m) where
  throwError = UIT . lift . throwError
  catchError (UIT action) handler = UIT $ ReaderT $ \ctx ->
    catchError (runReaderT action ctx) (\e -> runReaderT (_unUIT (handler e)) ctx)

runUIT :: (Monad m) => (Text -> m ()) -> UIT m a -> m a
runUIT writer (UIT action) =
  let ctx = UIContext {uiWriter = writer, uiIndent = 0}
   in runReaderT action ctx

runUI :: UIT IO a -> IO a
runUI = runUIT (putStr . toString)

class (Monad m) => MonadUI m where
  uiWrite :: Text -> m ()
  uiIndentLevel :: m Int
  uiWithIndent :: (Int -> Int) -> m a -> m a

instance (Monad m) => MonadUI (UIT m) where
  uiWrite txt = UIT $ do
    UIContext {uiWriter} <- ask
    lift (uiWriter txt)
  uiIndentLevel = UIT $ asks uiIndent
  uiWithIndent f (UIT action) = UIT $ local adjust action
    where
      adjust ctx = ctx {uiIndent = f (uiIndent ctx)}

instance (MonadUI m) => MonadUI (ResultT m) where
  uiWrite txt = lift (uiWrite txt)
  uiIndentLevel = lift uiIndentLevel
  uiWithIndent f (ResultT action) = ResultT $ uiWithIndent f action

putLine :: (MonadUI m) => Text -> m ()
putLine txt = do
  level <- uiIndentLevel
  uiWrite (indentBlockNum level (txt <> "\n"))

indent :: (MonadUI m) => Int -> m a -> m a
indent amount = uiWithIndent (+ amount)

sectionWithIcon :: (MonadUI m) => Text -> Text -> m a -> m ()
sectionWithIcon emoji title action = do
  putLine ""
  putLine (emoji <> " " <> chalk Bold title)
  indent 1 action $> ()

section :: (MonadUI m) => Text -> m a -> m ()
section = sectionWithIcon "•"

sectionWorkspace :: (MonadUI m) => m a -> m ()
sectionWorkspace = sectionWithIcon "./" "workspace"

sectionEnvironments :: (MonadUI m) => Maybe Text -> m a -> m ()
sectionEnvironments title = section ("environments" <> maybe "" (\name -> chalk Dim " (default: " <> chalk Magenta name <> chalk Dim ")") title)

tableM :: (MonadUI m) => Int -> [(Text, m Text)] -> m ()
tableM minSize rows = traverse_ formatRow rows
  where
    maxLabelLen = maximum (minSize : map (T.length . fst) rows) + 2
    formatRow (label, valueM) = do
      value <- valueM
      putLine $ padDots maxLabelLen label <> value

sectionTableM :: (MonadUI m) => Int -> Text -> [(Text, m Text)] -> m ()
sectionTableM size title = section title . tableM size

sectionConfig :: (MonadUI m) => Int -> [(Text, m Text)] -> m ()
sectionConfig size = section "config" . tableM size

forTable :: (MonadUI m) => Int -> [a] -> (a -> (Text, Text)) -> m ()
forTable minSize rows f =
  tableM minSize (map (second pure . f) rows)

printGenTable :: (MonadUI m) => [[Text]] -> m ()
printGenTable rows =
  let n = if null rows then 0 else maximum (map length rows)
      padRow r = take n (r ++ repeat "")
      paddedRows = map padRow rows
      colWidths = [maximum (0 : map (T.length . (!! i)) paddedRows) | i <- [0 .. n - 1]]
      formatRow row =
        let padded = padRow row
            cells = zipWith (`T.justifyLeft` ' ') colWidths padded
         in putLine $ T.intercalate "  " cells
   in traverse_ formatRow rows

isError :: Issue -> Bool
isError i = issueSeverity i == SeverityError

renderSummaryLines :: [Issue] -> [Text]
renderSummaryLines [] = ["", renderSummaryStatus Checked, ""]
renderSummaryLines issues =
  let headerLine =
        if any isError issues
          then renderSummaryStatus Invalid
          else renderSummaryStatus Warning
      grouped = groupBy ((==) `on` issueTopic) (sortOn issueTopic issues)
      renderGroup [] = []
      renderGroup pkgIssues@(Issue {issueTopic = header} : _) =
        let l1 = "  "
            step = l1 <> subPathSign <> chalk Dim "• "
            l2 = l1 <> "    " <> subPathSign
            headerText = l1 <> chalk Dim "• " <> chalk Bold header
            renderIssue Issue {issueDetails = Nothing, issueSeverity, issueMessage} =
              [step <> chalk (levelColor issueSeverity) issueMessage]
            renderIssue Issue {issueDetails = Just (GenericIssue {issueFile}), issueSeverity, issueMessage} =
              [ step <> chalk (levelColor issueSeverity) issueMessage,
                l2 <> chalk Dim ("file: " <> format issueFile)
              ]
            renderIssue Issue {issueDetails = Just (CommandIssue {issueCommand, issueLogFile}), issueSeverity, issueMessage} =
              let cmd =
                    if T.length issueCommand > 60
                      then T.take 60 issueCommand <> chalk Dim "..."
                      else issueCommand
               in [ step <> chalk (levelColor issueSeverity) (issueMessage <> ": ") <> cmd,
                    l2 <> chalk Dim ("logs: " <> format issueLogFile)
                  ]
            renderIssue Issue {issueDetails = Just (DependencyIssue {issueDependencies, issueFile}), issueSeverity, issueMessage} =
              let groupedDeps = Map.toList $ Map.fromListWith (++) [(scope, [(depName, actual, expected)]) | (scope, depName, actual, expected) <- issueDependencies]
                  depLines = concatMap formatGroup groupedDeps
                  formatGroup (scope, deps) =
                    (l2 <> chalk Dim "• " <> chalk Dim scope) : map formatDepLine deps
                  formatDepLine (depName, actual, expected) =
                    l1 <> "        " <> subPathSign <> depName <> ": " <> chalk Dim (actual <> " → " <> expected)
               in step <> chalk (levelColor issueSeverity) issueMessage
                    : l2 <> chalk Dim ("file: " <> format issueFile)
                    : depLines
            detailLines = concatMap renderIssue pkgIssues
         in headerText : detailLines
   in ["", headerLine, ""] <> concatMap renderGroup grouped <> [""]

levelColor :: Severity -> Color
levelColor SeverityError = Red
levelColor SeverityWarning = Yellow

printSummary :: (MonadUI m) => [Issue] -> m ()
printSummary = traverse_ putLine . renderSummaryLines

statusIndicator :: (MonadIO m) => Int -> Text -> Text -> m ()
statusIndicator padding prefix msg = do
  liftIO clearLine
  liftIO $ setCursorColumn 0
  liftIO $ putStr $ toString $ "  " <> padDots padding prefix <> msg
  liftIO $ hFlush stdout

runSpinner :: (MonadIO m) => Int -> Text -> m ()
runSpinner padding prefix = loop ["◜", "◠", "◝", "◞", "◡", "◟"]
  where
    loop (f : fs) = do
      statusIndicator padding prefix f
      liftIO $ threadDelay 200000 -- 200ms
      loop (fs ++ [f])
    loop [] = pure ()
