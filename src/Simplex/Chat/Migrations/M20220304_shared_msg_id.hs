{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220304_shared_msg_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220304_shared_msg_id :: Query
m20220304_shared_msg_id =
  [sql|
    ALTER TABLE messages ADD COLUMN shared_msg_id BLOB; -- is it possible to make it unique (per chat) for a user only?
    ALTER TABLE messages ADD COLUMN reply_to_shared_msg_id BLOB;
    -- ALTER TABLE chat_items ADD COLUMN reply_to_chat_item_id BLOB;
|]
