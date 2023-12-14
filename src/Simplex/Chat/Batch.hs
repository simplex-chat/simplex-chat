{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Simplex.Chat.Batch where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except (throwError, tryError)
import Control.Monad.Trans (lift)
import Simplex.Chat.Controller (ChatError, ChatMonad, withAgentB, withStore')
import Simplex.Messaging.Agent.Batch (AgentEnvBatch, processAgentBatch)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Batch
import Simplex.Messaging.Util (tshow)
import UnliftIO

data ChatDB
type instance BatchArgs ChatDB = DB.Connection
type instance BatchError ChatDB = ChatError
type ChatBatch m = BatchVar ChatDB m

execChatBatch :: ChatMonad m => (ChatBatch m -> AgentEnvBatch m -> BatchT ChatError m r) -> m r
execChatBatch action = do
  chatBatch <- newTVarIO mempty
  agentBatch <- newTVarIO mempty
  getResult <- execEContT $ action chatBatch agentBatch
  processAll chatBatch agentBatch
  getResult >>= either throwError pure

processAll :: ChatMonad m => ChatBatch m -> AgentEnvBatch m -> m ()
processAll chatBatch agentBatch = do
  chats <- atomically $ stateTVar chatBatch (,[])
  unless (null chats) $ runEContT (processChatBatch chats) $ either (logError . tshow) pure
  agents <- atomically $ stateTVar agentBatch (,[])
  unless (null agents) $ runEContT (withAgentB $ \a -> processAgentBatch a agents) $ either (logError . tshow) pure
  unless (null chats && null agents) $ processAll chatBatch agentBatch

processChatBatch :: ChatMonad m => [BatchOperation ChatDB m] -> BatchT ChatError m ()
processChatBatch batch = do
  results <- lift $ withStore' $ \db ->
    forM batch $ \BatchOperation {step, next} ->
      tryError (step db) >>= \case
        Right ok -> pure $ next ok
        Left err -> pure $ logError (tshow err)
  sequence_ results
