{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220901_intros_for_group_members where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220901_intros_for_group_members :: Query
m20220901_intros_for_group_members =
  [sql|
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
