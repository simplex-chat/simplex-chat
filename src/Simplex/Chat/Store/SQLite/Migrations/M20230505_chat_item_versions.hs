{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230505_chat_item_versions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230505_chat_item_versions :: Query
m20230505_chat_item_versions =
  [sql|
CREATE TABLE chat_item_versions ( -- contains versions only for edited chat items, including current version
  chat_item_version_id INTEGER PRIMARY KEY AUTOINCREMENT,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  msg_content TEXT NOT NULL,
  item_version_ts TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_chat_item_versions_chat_item_id ON chat_item_versions(chat_item_id);
|]

down_m20230505_chat_item_versions :: Query
down_m20230505_chat_item_versions =
  [sql|
DROP INDEX idx_chat_item_versions_chat_item_id;

DROP TABLE chat_item_versions;
|]
