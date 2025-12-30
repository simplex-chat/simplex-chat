{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220928_settings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220928_settings :: Query
m20220928_settings =
  [sql|
CREATE TABLE settings (
  settings_id INTEGER PRIMARY KEY,
  chat_item_ttl INTEGER,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
|]
