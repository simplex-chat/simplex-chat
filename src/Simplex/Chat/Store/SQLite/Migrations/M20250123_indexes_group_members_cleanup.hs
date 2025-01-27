{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250123_indexes_group_members_cleanup where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250123_indexes_group_members_cleanup :: Query
m20250123_indexes_group_members_cleanup =
  [sql|
CREATE INDEX idx_chat_items_group_id ON chat_items(group_id);

CREATE INDEX idx_connections_group_member_id ON connections(group_member_id);

DROP INDEX idx_chat_items_shared_msg_id;
CREATE INDEX idx_chat_items_group_id_shared_msg_id ON chat_items(group_id, shared_msg_id);

CREATE TABLE group_members_cleanup(
  group_member_cleanup_id INTEGER PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  local_display_name TEXT NOT NULL,
  contact_profile_id INTEGER NOT NULL,
  created_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_group_members_cleanup_user_id ON group_members_cleanup(user_id);
|]

down_m20250123_indexes_group_members_cleanup :: Query
down_m20250123_indexes_group_members_cleanup =
  [sql|
DROP INDEX idx_group_members_cleanup_user_id;

DROP TABLE group_members_cleanup;

DROP INDEX idx_chat_items_group_id_shared_msg_id;
CREATE INDEX idx_chat_items_shared_msg_id ON chat_items(shared_msg_id);

DROP INDEX idx_connections_group_member_id;

DROP INDEX idx_chat_items_group_id;
|]
