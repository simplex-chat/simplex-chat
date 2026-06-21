{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Directory.Util where

import Control.Logger.Simple
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (catchAll)

storeCxt :: ChatController -> StoreCxt
storeCxt ChatController {config} = mkStoreCxt config
{-# INLINE storeCxt #-}

withDB' :: Text -> ChatController -> (DB.Connection -> IO a) -> IO (Either String a)
withDB' cxt cc a = withDB cxt cc $ ExceptT . fmap Right . a

withDB :: Text -> ChatController -> (DB.Connection -> ExceptT String IO a) -> IO (Either String a)
withDB cxt ChatController {chatStore} action = do
  r_ <- withTransaction chatStore (runExceptT . action) `catchAll` (pure . Left . show)
  case r_ of
    Left e -> logError $ "Database error: " <> cxt <> " " <> T.pack e
    Right _ -> pure ()
  pure r_
