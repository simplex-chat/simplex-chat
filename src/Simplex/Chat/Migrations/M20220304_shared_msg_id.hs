{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220304_shared_msg_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220304_shared_msg_id :: Query
m20220304_shared_msg_id =
  [sql|
    ALTER TABLE messages ADD COLUMN shared_msg_id BLOB;
    ALTER TABLE chat_items ADD COLUMN shared_msg_id BLOB;
|]
