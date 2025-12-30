{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230621_chat_item_moderations where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- moderations that could not be applied - for messages that haven't been received at the time of moderation
m20230621_chat_item_moderations :: Query
m20230621_chat_item_moderations =
  [sql|
CREATE TABLE chat_item_moderations (
  chat_item_moderation_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  moderator_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  item_member_id BLOB NOT NULL,
  shared_msg_id BLOB NOT NULL,
  created_by_msg_id INTEGER REFERENCES messages(message_id) ON DELETE SET NULL,
  moderated_at TEXT NOT NULL, -- broker_ts of creating message
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_chat_item_moderations_group_id ON chat_item_moderations(group_id);
CREATE INDEX idx_chat_item_moderations_moderator_member_id ON chat_item_moderations(moderator_member_id);
CREATE INDEX idx_chat_item_moderations_created_by_msg_id ON chat_item_moderations(created_by_msg_id);

CREATE INDEX idx_chat_item_moderations_group ON chat_item_moderations(group_id, item_member_id, shared_msg_id);
|]

down_m20230621_chat_item_moderations :: Query
down_m20230621_chat_item_moderations =
  [sql|
DROP INDEX idx_chat_item_moderations_group;

DROP INDEX idx_chat_item_moderations_created_by_msg_id;
DROP INDEX idx_chat_item_moderations_moderator_member_id;
DROP INDEX idx_chat_item_moderations_group_id;

DROP TABLE chat_item_moderations;
|]
