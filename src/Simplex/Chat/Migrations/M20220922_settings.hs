{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220922_settings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220922_settings :: Query
m20220922_settings =
  [sql|
CREATE TABLE settings (
  settings_id INTEGER PRIMARY KEY,
  chat_item_ttl INTEGER,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

INSERT INTO settings (user_id)
  SELECT user_id FROM users WHERE active_user = 1 LIMIT 1;
|]
