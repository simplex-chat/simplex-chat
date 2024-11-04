{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241027_server_operators where

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
  role_storage INTEGER NOT NULL DEFAULT 1,
  role_proxy INTEGER NOT NULL DEFAULT 1,
  accepted_conditions_commit TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

ALTER TABLE protocol_servers ADD COLUMN server_operator_id INTEGER REFERENCES server_operators ON DELETE SET NULL;

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

CREATE INDEX idx_protocol_servers_server_operator_id ON protocol_servers(server_operator_id);
CREATE INDEX idx_operator_usage_conditions_server_operator_id ON operator_usage_conditions(server_operator_id);
CREATE UNIQUE INDEX idx_operator_usage_conditions_conditions_commit ON operator_usage_conditions(server_operator_id, conditions_commit);

INSERT INTO server_operators
  (server_operator_id, server_operator_tag, trade_name, legal_name, server_domains)
  VALUES (1, 'simplex', 'SimpleX Chat', 'SimpleX Chat Ltd', 'simplex.im');
INSERT INTO server_operators
  (server_operator_id, server_operator_tag, trade_name, legal_name, server_domains)
  VALUES (2, 'xyz', 'XYZ', 'XYZ Ltd', 'xyz.com');

-- UPDATE protocol_servers SET server_operator_id = 1 WHERE host LIKE "%.simplex.im" OR host LIKE "%.simplex.im,%";
|]

down_m20241027_server_operators :: Query
down_m20241027_server_operators =
  [sql|
DROP INDEX idx_operator_usage_conditions_conditions_commit;
DROP INDEX idx_operator_usage_conditions_server_operator_id;
DROP INDEX idx_protocol_servers_server_operator_id;

ALTER TABLE protocol_servers DROP COLUMN server_operator_id;

DROP TABLE operator_usage_conditions;
DROP TABLE usage_conditions;
DROP TABLE server_operators;
|]
