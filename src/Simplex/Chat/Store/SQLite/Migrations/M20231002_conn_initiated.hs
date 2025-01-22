{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20231002_conn_initiated where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231002_conn_initiated :: Query
m20231002_conn_initiated =
  [sql|
ALTER TABLE connections ADD COLUMN contact_conn_initiated INTEGER NOT NULL DEFAULT 0;

UPDATE connections SET conn_req_inv = NULL WHERE conn_status IN ('ready', 'deleted');

CREATE INDEX idx_sent_probes_created_at ON sent_probes(created_at);
CREATE INDEX idx_sent_probe_hashes_created_at ON sent_probe_hashes(created_at);
CREATE INDEX idx_received_probes_created_at ON received_probes(created_at);
|]

down_m20231002_conn_initiated :: Query
down_m20231002_conn_initiated =
  [sql|
DROP INDEX idx_sent_probes_created_at;
DROP INDEX idx_sent_probe_hashes_created_at;
DROP INDEX idx_received_probes_created_at;

ALTER TABLE connections DROP COLUMN contact_conn_initiated;
|]
