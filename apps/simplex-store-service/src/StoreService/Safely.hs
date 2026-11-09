{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StoreService.Safely
  ( safely,
    safelyWith,
  )
where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Logger.Simple (logError)
import Data.Text (Text)
import Simplex.Messaging.Util (tshow)

safely :: Text -> IO () -> IO Bool
safely what action = safelyWith what False (True <$ action)

-- | Asynchronous exceptions are rethrown, since that is how the race stops this thread.
safelyWith :: Text -> a -> IO a -> IO a
safelyWith what fallback action =
  try action >>= \case
    Right a -> pure a
    Left (e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
      Just _ -> throwIO e
      Nothing -> fallback <$ logError (what <> " failed; the next pass will run: " <> tshow e)
