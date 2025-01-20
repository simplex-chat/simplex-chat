{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Mobile.Options.SQLite where

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B
import Foreign.C.String
import Simplex.Chat.Options.DB
import Simplex.Messaging.Agent.Store.Interface (DBCreateOpts (..))

mobileDbOpts :: CString -> CString -> IO ChatDbOpts
mobileDbOpts fp key = do
  dbFilePrefix <- peekCString fp
  dbKey <- BA.convert <$> B.packCString key
  pure $
    ChatDbOpts
      { dbFilePrefix,
        dbKey,
        vacuumOnMigration = True
      }

-- used to create new chat controller,
-- at that point database is already opened, and the key in options is not used
mobileDbOpts' :: ChatDbOpts -> ChatDbOpts
mobileDbOpts' ChatDbOpts {dbFilePrefix, vacuumOnMigration} =
  ChatDbOpts
    { dbFilePrefix,
      dbKey = "",
      vacuumOnMigration
    }

errorDbStr :: DBCreateOpts -> String
errorDbStr DBCreateOpts {dbFilePath} = dbFilePath
