{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241027_server_operators where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241027_server_operators :: Query
m20241027_server_operators =
  [sql|
CREATE TABLE server_operators (
  server_operator_id INTEGER PRIMARY KEY AUTOINCREMENT,
  server_operator_tag TEXT,
  trade_name TEXT NOT NULL,
  legal_name TEXT,
  server_domains TEXT,
  enabled INTEGER NOT NULL DEFAULT 1,
  smp_role_storage INTEGER NOT NULL DEFAULT 1,
  smp_role_proxy INTEGER NOT NULL DEFAULT 1,
  xftp_role_storage INTEGER NOT NULL DEFAULT 1,
  xftp_role_proxy INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE usage_conditions (
  usage_conditions_id INTEGER PRIMARY KEY AUTOINCREMENT,
  conditions_commit TEXT NOT NULL UNIQUE,
  notified_at TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE operator_usage_conditions (
  operator_usage_conditions_id INTEGER PRIMARY KEY AUTOINCREMENT,
  server_operator_id INTEGER REFERENCES server_operators (server_operator_id) ON DELETE SET NULL ON UPDATE CASCADE,
  server_operator_tag TEXT,
  conditions_commit TEXT NOT NULL,
  accepted_at TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_operator_usage_conditions_server_operator_id ON operator_usage_conditions(server_operator_id);
CREATE UNIQUE INDEX idx_operator_usage_conditions_conditions_commit ON operator_usage_conditions(conditions_commit, server_operator_id);
|]

down_m20241027_server_operators :: Query
down_m20241027_server_operators =
  [sql|
DROP INDEX idx_operator_usage_conditions_conditions_commit;
DROP INDEX idx_operator_usage_conditions_server_operator_id;

DROP TABLE operator_usage_conditions;
DROP TABLE usage_conditions;
DROP TABLE server_operators;

DELETE FROM protocol_servers WHERE host LIKE "%.simplexonflux.com,%";
|]
