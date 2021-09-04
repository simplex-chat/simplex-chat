{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Controller where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (ChaChaDRG)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Numeric.Natural
import Simplex.Chat.Notification
import Simplex.Chat.Store (StoreError)
import Simplex.Chat.Terminal
import Simplex.Chat.Types
import Simplex.Messaging.Agent (AgentClient)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig)
import Simplex.Messaging.Agent.Protocol (AgentErrorType)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import System.IO (Handle)
import UnliftIO.STM

data ChatConfig = ChatConfig
  { agentConfig :: AgentConfig,
    dbPoolSize :: Int,
    tbqSize :: Natural,
    fileChunkSize :: Integer
  }

data ChatController = ChatController
  { currentUser :: TVar User,
    smpAgent :: AgentClient,
    chatTerminal :: ChatTerminal,
    chatStore :: SQLiteStore,
    idsDrg :: TVar ChaChaDRG,
    inputQ :: TBQueue InputEvent,
    notifyQ :: TBQueue Notification,
    sendNotification :: Notification -> IO (),
    chatLock :: TMVar (),
    sndFiles :: TVar (Map Int64 Handle),
    rcvFiles :: TVar (Map Int64 Handle),
    config :: ChatConfig
  }

data InputEvent = InputCommand String | InputControl Char

data ChatError
  = ChatError ChatErrorType
  | ChatErrorMessage String
  | ChatErrorAgent AgentErrorType
  | ChatErrorStore StoreError
  deriving (Show, Exception)

data ChatErrorType
  = CEGroupUserRole
  | CEGroupContactRole ContactName
  | CEGroupDuplicateMember ContactName
  | CEGroupDuplicateMemberId
  | CEGroupNotJoined GroupName
  | CEGroupMemberNotActive
  | CEGroupMemberUserRemoved
  | CEGroupMemberNotFound ContactName
  | CEGroupInternal String
  | CEFileNotFound String
  | CEFileAlreadyReceiving String
  | CEFileAlreadyExists FilePath
  | CEFileRead FilePath SomeException
  | CEFileWrite FilePath SomeException
  | CEFileSend Int64 AgentErrorType
  | CEFileRcvChunk String
  | CEFileInternal String
  deriving (Show, Exception)

type ChatMonad m = (MonadUnliftIO m, MonadReader ChatController m, MonadError ChatError m)

setActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
setActive to = asks (activeTo . chatTerminal) >>= atomically . (`writeTVar` to)

unsetActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
unsetActive a = asks (activeTo . chatTerminal) >>= atomically . (`modifyTVar` unset)
  where
    unset a' = if a == a' then ActiveNone else a'
