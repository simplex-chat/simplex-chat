{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221212_chat_items_timed where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221212_chat_items_timed :: Query
m20221212_chat_items_timed =
  [sql|
ALTER TABLE chat_items ADD COLUMN timed_ttl INTEGER;

ALTER TABLE chat_items ADD COLUMN timed_delete_at TEXT;

CREATE INDEX idx_chat_items_timed_delete_at ON chat_items(timed_delete_at);
|]
