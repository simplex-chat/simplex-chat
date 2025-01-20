{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Mobile.Options.Postgres where

import qualified Data.ByteString.Char8 as B
import Foreign.C.String
import Simplex.Chat.Options.DB
import Simplex.Messaging.Agent.Store.Interface (DBCreateOpts (..))

mobileDbOpts :: CString -> CString -> IO ChatDbOpts
mobileDbOpts param1 param2 = do
  dbSchemaPrefix <- peekCString param1
  dbConnstr <- peekCString param2
  pure $
    ChatDbOpts
      { dbConnstr,
        dbSchemaPrefix
      }

mobileDbOpts' :: ChatDbOpts -> ChatDbOpts
mobileDbOpts' = pure

errorDbStr :: DBCreateOpts -> String
errorDbStr DBCreateOpts {schema} = schema
