{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221212_disappearing_chat_items where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221212_disappearing_chat_items :: Query
m20221212_disappearing_chat_items =
  [sql|
CREATE TABLE disappearing_chat_items (
  disappearing_chat_item_id INTEGER PRIMARY KEY,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  delete_at_ts TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
|]
