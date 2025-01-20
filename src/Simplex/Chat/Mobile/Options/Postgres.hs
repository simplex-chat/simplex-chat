{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Mobile.Options.Postgres where

import Foreign.C.String
import Simplex.Chat.Options.DB
import Simplex.Messaging.Agent.Store.Interface (DBCreateOpts (..))

mobileDbOpts :: CString -> CString -> IO ChatDbOpts
mobileDbOpts schemaPrefix connstr = do
  dbSchemaPrefix <- peekCString schemaPrefix
  dbConnstr <- peekCString connstr
  pure $
    ChatDbOpts
      { dbConnstr,
        dbSchemaPrefix
      }

mobileDbOpts' :: ChatDbOpts -> ChatDbOpts
mobileDbOpts' = id

errorDbStr :: DBCreateOpts -> String
errorDbStr DBCreateOpts {schema} = schema
