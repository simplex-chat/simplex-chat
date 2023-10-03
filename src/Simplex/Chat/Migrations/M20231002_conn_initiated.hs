{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231002_conn_initiated where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231002_conn_initiated :: Query
m20231002_conn_initiated =
  [sql|
ALTER TABLE connections ADD COLUMN contact_conn_initiated INTEGER NOT NULL DEFAULT 0;

UPDATE connections SET conn_req_inv = NULL WHERE conn_status IN ('ready', 'deleted');
|]

down_m20231002_conn_initiated :: Query
down_m20231002_conn_initiated =
  [sql|
ALTER TABLE connections DROP COLUMN contact_conn_initiated;
|]
