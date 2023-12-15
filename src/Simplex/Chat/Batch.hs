{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Batch where

import Control.Monad.Except
import Control.Monad.IO.Unlift
import Simplex.Messaging.Agent.Batch (Batch (..), BatchEffect (..), EffectCont (..), unBatch)
import Simplex.Messaging.Agent.Client (AgentClient)
import Simplex.Messaging.Agent.Protocol (AgentErrorType)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Chat.Controller
import Simplex.Chat.Store.Shared

type ChatBatch m a = Batch ChatBatchEff ChatError m a

data ChatBatchEff m b
  = CBDatabase {dbAction :: DB.Connection -> ExceptT StoreError IO b}
  | CBAgent {agentAction :: AgentClient -> ExceptT AgentErrorType m b}

unBatch_ :: ChatMonad m => m (ChatBatch m a) -> m a
unBatch_ = unBatch ()

instance ChatMonad m => BatchEffect ChatBatchEff () ChatError m where
  execBatchEffects _ = \case
    (CBDatabase {dbAction} : _) -> (: []) <$> runExceptT (withStore dbAction)
    (CBAgent {agentAction} : _) -> (: []) <$> tryError (withAgent agentAction)
    _ -> throwError $ ChatError $ CEInternalError "not implemented"
  batchError = ChatError . CEInternalError

withStoreB :: Monad m => (DB.Connection -> ExceptT StoreError IO b) -> (b -> m (ChatBatch m a)) -> m (ChatBatch m a)
withStoreB f = pure . BEffect . EffectCont (CBDatabase f)

withStoreB' :: Monad m => (DB.Connection -> IO b) -> (b -> m (ChatBatch m a)) -> m (ChatBatch m a)
withStoreB' f = withStoreB (liftIO . f)
