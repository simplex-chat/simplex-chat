{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221212_chat_items_delete_at where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221212_chat_items_delete_at :: Query
m20221212_chat_items_delete_at =
  [sql|
ALTER TABLE chat_items ADD COLUMN delete_at TEXT;

CREATE INDEX idx_chat_items_delete_at ON chat_items(delete_at);
|]
