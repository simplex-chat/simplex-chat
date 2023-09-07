{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230903_connection_needs_sub where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230903_connection_needs_sub :: Query
m20230903_connection_needs_sub =
  [sql|
ALTER TABLE connections ADD COLUMN needs_sub INTEGER DEFAULT 0 NOT NULL;
|]

down_m20230903_connection_needs_sub :: Query
down_m20230903_connection_needs_sub =
  [sql|
ALTER TABLE connections DROP COLUMN needs_sub;
|]
