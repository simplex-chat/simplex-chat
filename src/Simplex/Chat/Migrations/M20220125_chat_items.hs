{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220125_chat_items where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220125_chat_items :: Query
m20220125_chat_items =
  [sql|
CREATE TABLE chat_items ( -- mutable chat_items presented to user
  chat_item_id INTEGER PRIMARY KEY,
  chat_msg_id INTEGER NOT NULL, -- sent as part of the message that created the item
  item_deleted INTEGER NOT NULL, -- 1 for deleted
  item_type TEXT NOT NULL,
  item_text TEXT NOT NULL, -- textual representation
  item_props TEXT NOT NULL, -- JSON
  item_ts TEXT NOT NULL DEFAULT (datetime('now')) -- broker_ts of creating message for received, created_at for sent
);

CREATE TABLE direct_chat_items (
  chat_item_id INTEGER NOT NULL UNIQUE REFERENCES chat_items ON DELETE CASCADE,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT, -- TODO ? CASCADE
  item_sent INTEGER -- 0 for received, 1 for sent
);

CREATE TABLE group_chat_items (
  chat_item_id INTEGER NOT NULL UNIQUE REFERENCES chat_items ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT, -- NULL for sent; -- TODO ? CASCADE
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT -- TODO ? CASCADE
);

CREATE TABLE chat_item_content (
  chat_item_content_id INTEGER PRIMARY KEY,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  content_type TEXT NOT NULL,
  content_size INTEGER NOT NULL,
  content BLOB NOT NULL
);

CREATE TABLE chat_item_messages (
  chat_item_id INTEGER NOT NULL REFERENCES chat_items,
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  UNIQUE (chat_item_id, message_id)
);
|]
