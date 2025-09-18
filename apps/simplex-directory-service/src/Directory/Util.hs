{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Directory.Util where

import qualified Control.Exception as E
import Control.Logger.Simple
import Control.Monad.Except
import Data.Text (Text)
import Simplex.Chat.Controller
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (tshow)

vr :: ChatController -> VersionRangeChat
vr ChatController {config = ChatConfig {chatVRange}} = chatVRange
{-# INLINE vr #-}

withDB' :: Text -> ChatController -> (DB.Connection -> IO a) -> IO (Maybe a)
withDB' cxt cc a = withDB cxt cc $ ExceptT . fmap Right . a

withDB :: Text -> ChatController -> (DB.Connection -> ExceptT StoreError IO a) -> IO (Maybe a)
withDB cxt ChatController {chatStore} action = do
  r_ :: Either ChatError a <- withTransaction chatStore (runExceptT . withExceptT ChatErrorStore . action) `E.catches` handleDBErrors
  case r_ of
    Right r -> pure $ Just r
    Left e -> Nothing <$ logError ("Database error: " <> cxt <> " " <> tshow e)
