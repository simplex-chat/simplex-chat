{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251018_chat_relays where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20251018_chat_relays :: Query
m20251018_chat_relays =
  [sql|
CREATE TABLE chat_relays(
  chat_relay_id INTEGER PRIMARY KEY,
  address TEXT NOT NULL,
  name TEXT NOT NULL,
  domains TEXT NOT NULL,
  preset INTEGER NOT NULL DEFAULT 0,
  tested INTEGER,
  enabled INTEGER NOT NULL DEFAULT 1,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  UNIQUE(user_id, address),
  UNIQUE(user_id, name)
);

CREATE INDEX idx_chat_relays_user_id ON chat_relays(user_id);

ALTER TABLE users ADD COLUMN is_user_chat_relay INTEGER NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN is_chat_relay INTEGER NOT NULL DEFAULT 0;
|]

down_m20251018_chat_relays :: Query
down_m20251018_chat_relays =
  [sql|
ALTER TABLE group_members DROP COLUMN is_chat_relay;

ALTER TABLE users DROP COLUMN is_user_chat_relay;

DROP INDEX idx_chat_relays_user_id;

DROP TABLE chat_relays;
|]
