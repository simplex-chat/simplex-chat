{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220909_commands where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220909_commands :: Query
m20220909_commands =
  [sql|
CREATE TABLE commands (
  command_id INTEGER PRIMARY KEY AUTOINCREMENT, -- used as ACorrId
  connection_id INTEGER REFERENCES connections ON DELETE CASCADE,
  command_function TEXT NOT NULL,
  command_status TEXT NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

ALTER TABLE msg_deliveries ADD COLUMN agent_ack_cmd_id INTEGER; -- correlation id

ALTER TABLE connections ADD COLUMN conn_req_inv BLOB;
|]
