{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HWM.Core.Result
  ( Result (..),
    ResultT (..),
    Issue (..),
    MonadIssue (..),
    Severity (..),
    IssueDetails (..),
    fromEither,
    maxSeverity,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Foldable (Foldable (..))
import Data.Text.Lazy.Builder ()
import Relude

data Severity = SeverityWarning | SeverityError
  deriving (Show, Eq, Ord)

data IssueDetails
  = CommandIssue
      { issueCommand :: Text,
        issueLogFile :: FilePath
      }
  | GenericIssue
      { issueFile :: FilePath
      }
  | DependencyIssue
      { issueDependencies :: [(Text, Text, Text, Text)],
        issueFile :: FilePath
      }
  deriving (Show, Eq, Ord)

data Issue = Issue
  { issueTopic :: Text,
    issueSeverity :: Severity,
    issueMessage :: Text,
    issueDetails :: Maybe IssueDetails
  }
  deriving (Show, Eq, Ord)

instance IsString Issue where
  fromString s =
    Issue
      { issueTopic = "general",
        issueSeverity = SeverityError,
        issueMessage = fromString s,
        issueDetails = Nothing
      }

class MonadIssue m where
  injectIssue :: Issue -> m ()
  catchIssues :: m a -> m (Maybe Severity, a)
  mapIssue :: (Issue -> Issue) -> m a -> m a

maxSeverity :: [Issue] -> Maybe Severity
maxSeverity [] = Nothing
maxSeverity issues_ = Just $ maximum $ map issueSeverity issues_

instance MonadIssue (Result Issue) where
  injectIssue issue = Success () [issue]
  catchIssues m@(Success _ ls) = (maxSeverity ls,) <$> m
  catchIssues m@Failure {} = (Just SeverityError,) <$> m
  mapIssue f (Success x ls) = Success x (map f ls)
  mapIssue f (Failure e) = Failure (f <$> e)

instance (Monad m) => MonadIssue (ResultT m) where
  injectIssue issue = ResultT $ pure $ Success () [issue]
  catchIssues (ResultT m) = ResultT $ catchIssues <$> m
  mapIssue f (ResultT m) = ResultT $ mapIssue f <$> m

data Result er a
  = Success {result :: a, issues :: [er]}
  | Failure {failure :: NonEmpty er}
  deriving (Functor)

instance Applicative (Result er) where
  pure = (`Success` [])
  Success f w1 <*> Success x w2 = Success (f x) (w1 <> w2)
  Failure (e :| es) <*> Success _ e' = Failure (e :| (es <> e'))
  Failure e <*> Failure e' = Failure (e <> e')
  Success _ e' <*> Failure (e :| es) = Failure (e :| (es <> e'))

instance Monad (Result er) where
  return = pure
  Success v w1 >>= fm = case fm v of
    (Success x w2) -> Success x (w1 <> w2)
    Failure e -> Failure e
  Failure e >>= _ = Failure e

instance MonadError er (Result er) where
  throwError = Failure . pure
  catchError (Failure e) f = f (head e)
  catchError x _ = x

newtype ResultT (m :: Type -> Type) a = ResultT
  { runResultT :: m (Result Issue a)
  }
  deriving (Functor)

instance (Applicative m) => Applicative (ResultT m) where
  pure = ResultT . pure . pure
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) app1 app2

instance (Monad m) => Monad (ResultT m) where
  return = pure
  (ResultT m1) >>= mFunc = ResultT $ do
    rs <- m1
    case rs of
      Success value w1 -> do
        result' <- runResultT (mFunc value)
        case result' of
          Success value' w2 -> pure $ Success value' (w1 <> w2)
          Failure e -> pure $ Failure e
      Failure e -> pure $ Failure e

instance MonadTrans ResultT where
  lift = ResultT . fmap pure

instance (Monad m) => MonadError Issue (ResultT m) where
  throwError = ResultT . pure . throwError
  catchError (ResultT mx) f = ResultT (mx >>= catchResultError)
    where
      catchResultError (Failure e) = runResultT (f (head e))
      catchResultError x = pure x

instance (MonadIO m) => MonadIO (ResultT m) where
  liftIO = lift . liftIO

fromEither :: (MonadError Issue m) => Text -> Either String a -> m a
fromEither context = either (throwError . fromString . ((toString context <> ": ") <>)) pure
