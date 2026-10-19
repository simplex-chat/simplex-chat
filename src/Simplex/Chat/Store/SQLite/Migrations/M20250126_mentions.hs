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
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  member_id BLOB NOT NULL,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  display_name TEXT NOT NULL
);

CREATE INDEX idx_chat_item_mentions_group_id ON chat_item_mentions(group_id);
CREATE INDEX idx_chat_item_mentions_chat_item_id ON chat_item_mentions(chat_item_id);
CREATE UNIQUE INDEX idx_chat_item_mentions_display_name ON chat_item_mentions(chat_item_id, display_name);
CREATE UNIQUE INDEX idx_chat_item_mentions_member_id ON chat_item_mentions(chat_item_id, member_id);

CREATE INDEX idx_chat_items_groups_user_mention ON chat_items(user_id, group_id, item_status, user_mention);
|]

down_m20250126_mentions :: Query
down_m20250126_mentions =
  [sql|
DROP INDEX idx_chat_items_groups_user_mention;

DROP INDEX idx_chat_item_mentions_group_id;
DROP INDEX idx_chat_item_mentions_chat_item_id;
DROP INDEX idx_chat_item_mentions_display_name;
DROP INDEX idx_chat_item_mentions_member_id;

DROP TABLE chat_item_mentions;
ALTER TABLE chat_items DROP COLUMN user_mention;
|]
