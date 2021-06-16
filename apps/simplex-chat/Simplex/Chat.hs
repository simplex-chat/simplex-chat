{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Simplex.Chat where

import Control.Exception (Exception)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (MonadRandom)
import Data.ByteString (ByteString)
import Data.Composition ((.:))
import Numeric.Natural (Natural)
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Store (SQLitePool, createStore)
import UnliftIO.STM

data ChatClient = ChatClient
  { dbPool :: SQLitePool,
    smpAgent :: AgentClient,
    chatQ :: TBQueue ChatEvent
  }

data ChatEvent = ChatEvent String | ChatError ChatErrorType -- stub

newChatClient :: (MonadUnliftIO m, MonadRandom m) => ChatConfig -> m ChatClient
newChatClient ChatConfig {dbFile, queueSize, smpAgentCfg} = do
  dbPool <- liftIO $ createStore dbFile 4
  smpAgent <- getSMPAgentClient smpAgentCfg
  chatQ <- newTBQueueIO queueSize
  pure ChatClient {dbPool, smpAgent, chatQ}

data ChatConfig = ChatConfig
  { dbFile :: FilePath,
    queueSize :: Natural,
    smpAgentCfg :: AgentConfig
  }

defaultChatConfig :: ChatConfig
defaultChatConfig =
  ChatConfig
    { dbFile = "simplex-chat.db",
      queueSize = 1000,
      smpAgentCfg =
        AgentConfig
          { tcpPort = undefined, -- agent does not listen to TCP
            smpServers = undefined, -- filled in from ini file
            rsaKeySize = 2048 `div` 8,
            connIdBytes = 12,
            tbqSize = 16,
            dbFile = "smp-chat.db",
            smpCfg = smpDefaultConfig
          }
    }

data ChatErrorType = ChatErrorType String | ChatErrorType2 String -- stub
  deriving (Show, Exception)

type ChatMonad m = (MonadUnliftIO m, MonadReader ChatClient m, MonadError ChatErrorType m)

addNewContact :: ChatMonad m => m SMPQueueInfo
addNewContact = pure SMPQueueInfo {}

newtype Contact = Contact Int -- stub

connectToContact :: ChatMonad m => SMPQueueInfo -> m Contact
connectToContact _ = pure $ Contact 0

newtype MessageData = MessageData ByteString -- stub

data Message = Message Int ByteString -- stub

type MessageId = Int -- stub

someChatAction :: MonadUnliftIO m => m ()
someChatAction = pure ()

sendMessage :: ChatMonad m => Contact -> MessageData -> m MessageId
sendMessage _ _ = pure 0

deleteContact :: ChatMonad m => Contact -> m ()
deleteContact _ = pure ()

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatClient m) => m ()
agentSubscriber = forever $ processEvents (subQ . smpAgent) processAgentMessage

processEvents ::
  (MonadUnliftIO m, MonadReader ChatClient m, Foldable f) =>
  (ChatClient -> TBQueue a) ->
  (a -> ExceptT ChatErrorType m (f ChatEvent)) ->
  m ()
processEvents inQ process = do
  writeQ <- asks $ atomically .: writeTBQueue . chatQ
  asks inQ
    >>= atomically . readTBQueue
    >>= runExceptT . process
    >>= \case
      Right events -> mapM_ writeQ events
      Left e -> writeQ $ ChatError e

processAgentMessage :: ChatMonad m => ATransmission 'Agent -> m (Maybe ChatEvent)
processAgentMessage (_, _connId, _cmd) = pure . Just $ ChatEvent "hello" -- stub
