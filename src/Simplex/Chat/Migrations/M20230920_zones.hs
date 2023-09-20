{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230920_zones where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230920_zones :: Query
m20230920_zones =
  [sql|
CREATE TABLE zones ( -- chat controller zones
  zone_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL,
  kind TEXT NOT NULL,
  properties TEXT NOT NULL DEFAULT '{}'
);
|]

down_m20230920_zones :: Query
down_m20230920_zones =
  [sql| DROP TABLE zones; |]
