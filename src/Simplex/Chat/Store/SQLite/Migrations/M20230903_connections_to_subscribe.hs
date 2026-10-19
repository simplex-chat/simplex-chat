{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230903_connections_to_subscribe where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230903_connections_to_subscribe :: Query
m20230903_connections_to_subscribe =
  [sql|
ALTER TABLE connections ADD COLUMN to_subscribe INTEGER DEFAULT 0 NOT NULL;
CREATE INDEX idx_connections_to_subscribe ON connections(to_subscribe);
|]

down_m20230903_connections_to_subscribe :: Query
down_m20230903_connections_to_subscribe =
  [sql|
DROP INDEX idx_connections_to_subscribe;
ALTER TABLE connections DROP COLUMN to_subscribe;
|]
