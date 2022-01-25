{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220125_chat_items where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220125_chat_items :: Query
m20220125_chat_items =
  [sql|
CREATE TABLE chat_items ( -- mutable chat_items presented to user
  chat_item_id INTEGER PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE, -- NULL for sent even if group_id is not
  group_id INTEGER REFERENCES groups ON DELETE CASCADE,
  chat_msg_id INTEGER, -- sent as part of the message that created the item
  created_by_message_id INTEGER NOT NULL UNIQUE REFERENCES messages (message_id),
  item_sent INTEGER NOT NULL, -- 0 for received, 1 for sent
  item_ts TEXT NOT NULL, -- broker_ts of creating message for received, created_at for sent
  item_deleted INTEGER NOT NULL DEFAULT 0, -- 1 for deleted,
  item_text TEXT NOT NULL, -- textual representation
  item_content TEXT NOT NULL, -- JSON
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE chat_item_messages (
  chat_item_id INTEGER NOT NULL REFERENCES chat_items,
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  UNIQUE (chat_item_id, message_id)
);
|]
