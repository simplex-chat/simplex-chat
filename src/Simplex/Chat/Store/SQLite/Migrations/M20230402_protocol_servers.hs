{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230402_protocol_servers where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230402_protocol_servers :: Query
m20230402_protocol_servers =
  [sql|
ALTER TABLE smp_servers RENAME TO protocol_servers;
ALTER TABLE protocol_servers ADD COLUMN protocol TEXT NOT NULL DEFAULT 'smp';
|]

down_m20230402_protocol_servers :: Query
down_m20230402_protocol_servers =
  [sql|
ALTER TABLE protocol_servers DROP COLUMN protocol;
ALTER TABLE protocol_servers RENAME TO smp_servers;
|]
