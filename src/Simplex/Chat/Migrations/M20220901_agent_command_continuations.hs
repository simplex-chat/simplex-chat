{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220901_agent_command_continuations where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220901_agent_command_continuations :: Query
m20220901_agent_command_continuations =
  [sql|
CREATE TABLE agent_command_continuations (
  agent_command_continuation_id INTEGER PRIMARY KEY,
  connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE,
  continuation TEXT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
|]
