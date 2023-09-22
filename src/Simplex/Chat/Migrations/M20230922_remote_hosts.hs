{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230922_remote_hosts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230922_remote_hosts :: Query
m20230922_remote_hosts =
  [sql|
CREATE TABLE remote_hosts (
  zone_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL,
  path TEXT NOT NULL,
  ca_key BLOB NOT NULL,
  ca_cert BLOB NOT NULL
);
|]

down_m20230922_remote_hosts :: Query
down_m20230922_remote_hosts =
  [sql| DROP TABLE remote_hosts; |]
