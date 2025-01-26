{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250126_mentions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250126_mentions :: Query
m20250126_mentions =
  [sql|
ALTER TABLE chat_items ADD COLUMN user_mention INTEGER NOT NULL DEFAULT 0;

CREATE TABLE chat_item_mentions (
  chat_item_mention_id INTEGER PRIMARY KEY AUTOINCREMENT,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items,
  member_id BLOB NOT NULL,
  member_name TEXT NOT NULL
);

CREATE INDEX idx_chat_item_mentions_chat_item_id ON chat_item_mentions(chat_item_id);
|]

down_m20250126_mentions :: Query
down_m20250126_mentions =
  [sql|
DROP INDEX idx_chat_item_mentions_chat_item_id;
DROP TABLE chat_item_mentions;
ALTER TABLE chat_items DROP COLUMN user_mention;
|]
