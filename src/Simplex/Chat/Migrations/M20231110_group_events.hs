{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231110_group_events where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231110_group_events :: Query
m20231110_group_events =
  [sql|
CREATE TABLE group_events (
  group_event_id INTEGER PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  chat_item_id INTEGER REFERENCES chat_items ON DELETE SET NULL,
  message_id INTEGER REFERENCES messages ON DELETE SET NULL,
  shared_msg_id BLOB NOT NULL,
  event_sent INTEGER NOT NULL, -- 0 for received, 1 for sent; below `rcvd_` fields are null for sent
  rcvd_author_member_id BLOB,
  rcvd_author_group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE, -- can be null for rcvd if author is unknown
  rcvd_from_group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_group_events_user_id ON group_events(user_id);
CREATE INDEX idx_group_events_chat_item_id ON group_events(chat_item_id);
CREATE INDEX idx_group_events_message_id ON group_events(message_id);
CREATE INDEX idx_group_events_shared_msg_id ON group_events(shared_msg_id);
CREATE INDEX idx_group_events_rcvd_author_group_member_id ON group_events(rcvd_author_group_member_id);
CREATE INDEX idx_group_events_rcvd_from_group_member_id ON group_events(rcvd_from_group_member_id);

CREATE TABLE group_events_parents (
  group_event_parent_id INTEGER NOT NULL REFERENCES group_events ON DELETE CASCADE,
  group_event_child_id INTEGER NOT NULL REFERENCES group_events ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  UNIQUE(group_event_parent_id, group_event_child_id)
);

CREATE INDEX idx_group_events_parents_group_event_parent_id ON group_events_parents(group_event_parent_id);
CREATE INDEX idx_group_events_parents_group_event_child_id ON group_events_parents(group_event_child_id);
|]

down_m20231110_group_events :: Query
down_m20231110_group_events =
  [sql|
DROP INDEX idx_group_events_user_id;
DROP INDEX idx_group_events_chat_item_id;
DROP INDEX idx_group_events_message_id;
DROP INDEX idx_group_events_shared_msg_id;
DROP INDEX idx_group_events_rcvd_author_group_member_id;
DROP INDEX idx_group_events_rcvd_from_group_member_id;
DROP TABLE group_events;

DROP INDEX idx_group_events_parents_group_event_parent_id;
DROP INDEX idx_group_events_parents_group_event_child_id;
DROP TABLE group_events_parents;
|]
