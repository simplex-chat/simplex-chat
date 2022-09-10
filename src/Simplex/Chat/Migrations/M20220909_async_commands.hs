{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220909_async_commands where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220909_async_commands :: Query
m20220909_async_commands =
  [sql|
CREATE TABLE async_commands (
  async_command_id INTEGER PRIMARY KEY, -- used as ACorrId
  connection_id INTEGER REFERENCES connections ON DELETE CASCADE,
  command_tag TEXT NOT NULL,
  command_status TEXT NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

ALTER TABLE msg_deliveries ADD COLUMN agent_ack_cmd_id INTEGER; -- correlation id

CREATE TABLE intros_for_group_members (
  intro_for_group_member_id INTEGER PRIMARY KEY,
  group_id INTEGER DEFAULT NULL REFERENCES groups ON DELETE CASCADE, -- required for saving group_id of snd message when sending XGrpMemInv
  group_member_id INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE, -- required for sending in XGrpMemInv
  host_connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE, -- required for XGrpMemInv back to host
  direct_connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE,
  direct_conn_req BLOB, -- initially NULL, populated on INV agent response in direct connection
  group_connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE,
  group_conn_req BLOB, -- initially NULL, populated on INV agent response in direct connection
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(direct_connection_id),
  UNIQUE(group_connection_id)
);
|]
