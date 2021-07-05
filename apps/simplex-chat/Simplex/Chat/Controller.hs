{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Controller where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Numeric.Natural
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent (AgentClient)
import Simplex.Messaging.Agent.Protocol (AgentErrorType)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import Simplex.Notification
import Simplex.Store (StoreError)
import Simplex.Terminal
import UnliftIO.STM

data ChatController = ChatController
  { currentUser :: User,
    smpAgent :: AgentClient,
    chatTerminal :: ChatTerminal,
    chatStore :: SQLiteStore,
    chatQ :: TBQueue ChatTransmission,
    inputQ :: TBQueue InputEvent,
    notifyQ :: TBQueue Notification,
    sendNotification :: Notification -> IO ()
  }

data InputEvent = InputCommand String | InputControl Char

data ChatError
  = ChatErrorContact ContactError
  | ChatErrorAgent AgentErrorType
  | ChatErrorStore StoreError
  deriving (Show, Exception)

data ContactError = CENotFound ContactRef | CEProfile String
  deriving (Show, Exception)

type ChatMonad m = (MonadUnliftIO m, MonadReader ChatController m, MonadError ChatError m)

newChatController :: AgentClient -> ChatTerminal -> SQLiteStore -> User -> (Notification -> IO ()) -> Natural -> STM ChatController
newChatController smpAgent chatTerminal chatStore currentUser sendNotification qSize = do
  inputQ <- newTBQueue qSize
  notifyQ <- newTBQueue qSize
  chatQ <- newTBQueue qSize
  pure ChatController {currentUser, smpAgent, chatTerminal, chatStore, chatQ, inputQ, notifyQ, sendNotification}

setActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
setActive to = asks (activeTo . chatTerminal) >>= atomically . (`writeTVar` to)

unsetActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
unsetActive a = asks (activeTo . chatTerminal) >>= atomically . (`modifyTVar` unset)
  where
    unset a' = if a == a' then ActiveNone else a'
