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
  enabled INTEGER NOT NULL DEFAULT 1,
  role_storage INTEGER NOT NULL DEFAULT 1,
  role_proxy INTEGER NOT NULL DEFAULT 1
);

ALTER TABLE protocol_servers ADD COLUMN server_operator_id INTEGER REFERENCES server_operators ON DELETE SET NULL;

CREATE TABLE usage_conditions_acceptance (
  usage_conditions_acceptance_id INTEGER PRIMARY KEY AUTOINCREMENT,
  server_operator_id INTEGER REFERENCES server_operators (server_operator_id),
  conditions_required_at TEXT NOT NULL,
  conditions_accepted_at TEXT,
  conditions_commit TEXT NOT NULL,
  accepted_at TEXT NOT NULL
);

CREATE INDEX idx_protocol_servers_operators ON protocol_servers(server_operator_id);
CREATE INDEX idx_usage_conditions_acceptance_server_operators ON usage_conditions_acceptance(server_operator_id);

INSERT INTO server_operators
  (server_operator_id, server_operator_tag, trade_name, legal_name)
  VALUES (1, 'simplex', 'SimpleX Chat', 'SimpleX Chat Ltd');
INSERT INTO server_operators
  (server_operator_id, server_operator_tag, trade_name, legal_name)
  VALUES (2, 'xyz', 'XYZ', 'XYZ Ltd');

UPDATE protocol_servers SET server_operator_id = 1 WHERE host LIKE "%.simplex.im" OR host LIKE "%.simplex.im,%";
|]

down_m20241027_server_operators :: Query
down_m20241027_server_operators =
  [sql|
DROP INDEX idx_usage_conditions_acceptance_server_operators;
DROP INDEX idx_protocol_servers_operators;

ALTER TABLE protocol_servers DROP COLUMN server_operator_id;

DROP TABLE server_operators;
DROP TABLE usage_conditions_acceptance;
|]
