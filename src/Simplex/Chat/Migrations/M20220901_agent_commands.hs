{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220901_agent_commands where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220901_agent_commands :: Query
m20220901_agent_commands =
  [sql|
CREATE TABLE agent_commands (
  agent_command_id INTEGER PRIMARY KEY,
  corr_id BLOB, -- why not use agent_command_id?
  connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE,
  continuation BLOB,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
|]
