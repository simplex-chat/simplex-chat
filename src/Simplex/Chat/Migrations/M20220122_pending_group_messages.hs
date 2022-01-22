{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220122_pending_group_messages where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220122_pending_group_messages :: Query
m20220122_pending_group_messages =
  [sql|
-- pending messages for announced (memberCurrent) but not yet connected (memberActive) group members
CREATE TABLE pending_group_messages (
  pending_group_message_id INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES messages ON DELETE CASCADE,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
|]
