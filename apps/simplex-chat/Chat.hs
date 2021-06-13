{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chat where

import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (MonadRandom)
import Data.ByteString (ByteString)
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client (smpDefaultConfig)
import Store (SQLitePool, createStore)
import UnliftIO.STM

data ChatClient = ChatClient
  { dbPool :: SQLitePool,
    smpAgent :: AgentClient
  }

newChatClient :: (MonadUnliftIO m, MonadRandom m) => ChatConfig -> m ChatClient
newChatClient ChatConfig {dbFile, smpAgentCfg} = do
  dbPool <- liftIO $ createStore dbFile 4
  smpAgent <- getSMPAgentClient smpAgentCfg
  pure ChatClient {dbPool, smpAgent}

data ChatConfig = ChatConfig
  { dbFile :: FilePath,
    smpAgentCfg :: AgentConfig
  }

defaultChatConfig :: ChatConfig
defaultChatConfig =
  ChatConfig
    { dbFile = "simplex-chat.db",
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

newtype ChatErrorType = ChatErrorType String -- stub

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

newtype ChatEvent = ChatEvent ByteString -- stub

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatClient m) => m ()
agentSubscriber = do
  a <- asks smpAgent
  forever $ do
    t <- atomically . readTBQueue $ subQ a
    runExceptT (runReaderT (processAgentMessage a t) (agentEnv a)) >>= \case
      Right _ -> pure ()
      Left _ -> pure ()

processAgentMessage :: AgentMonad m => AgentClient -> ATransmission 'Agent -> m ()
processAgentMessage c (_, connId, cmd) = pure ()
