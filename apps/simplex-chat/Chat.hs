{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Chat where

import Control.Exception (Exception)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (MonadRandom)
import Data.ByteString (ByteString)
import Data.Composition ((.*))
import Numeric.Natural (Natural)
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client (smpDefaultConfig)
import Store (SQLitePool, createStore)
import UnliftIO.STM

data ChatClient = ChatClient
  { dbPool :: SQLitePool,
    smpAgent :: AgentClient,
    userQ :: TBQueue ChatCommand,
    chatQ :: TBQueue ChatEvent
  }

newtype ChatCommand = ChatCommand String -- stub

data ChatEvent = ChatEvent String | ChatError ChatErrorType -- stub

newChatClient :: (MonadUnliftIO m, MonadRandom m) => ChatConfig -> m ChatClient
newChatClient ChatConfig {dbFile, queueSize, smpAgentCfg} = do
  dbPool <- liftIO $ createStore dbFile 4
  smpAgent <- getSMPAgentClient smpAgentCfg
  userQ <- newTBQueueIO queueSize
  chatQ <- newTBQueueIO queueSize
  pure ChatClient {dbPool, smpAgent, userQ, chatQ}

data ChatConfig = ChatConfig
  { dbFile :: FilePath,
    queueSize :: Natural,
    smpAgentCfg :: AgentConfig
  }

defaultChatConfig :: ChatConfig
defaultChatConfig =
  ChatConfig
    { dbFile = "simplex-chat.db",
      queueSize = 16,
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

sendMessage :: ChatMonad m => Contact -> MessageData -> m MessageId
sendMessage _ _ = pure 0

deleteContact :: ChatMonad m => Contact -> m ()
deleteContact _ = pure ()

chatClient :: (MonadUnliftIO m, MonadReader ChatClient m) => m ()
chatClient = forever $ runEvents userQ processUserCommand

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatClient m) => m ()
agentSubscriber = forever $ runEvents (subQ . smpAgent) processAgentMessage

runEvents ::
  (MonadUnliftIO m, MonadReader ChatClient m, Foldable f) =>
  (ChatClient -> TBQueue a) ->
  (a -> ExceptT ChatErrorType m (f ChatEvent)) ->
  m ()
runEvents inQ process = do
  writeQ <- asks $ atomically .* writeTBQueue . chatQ
  asks inQ
    >>= atomically . readTBQueue
    >>= runExceptT . process
    >>= \case
      Right events -> mapM_ writeQ events
      Left e -> writeQ $ ChatError e

processUserCommand :: ChatMonad m => ChatCommand -> m [ChatEvent]
processUserCommand _cmd = pure [ChatEvent "hello"] -- stub

processAgentMessage :: ChatMonad m => ATransmission 'Agent -> m (Maybe ChatEvent)
processAgentMessage (_, _connId, _cmd) = pure . Just $ ChatEvent "hello" -- stub
