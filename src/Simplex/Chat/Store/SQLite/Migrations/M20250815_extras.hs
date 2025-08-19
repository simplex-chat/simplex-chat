{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250815_extras where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250815_extras :: Query
m20250815_extras =
  [sql|
ALTER TABLE protocol_servers ADD COLUMN extras TEXT;
|]

down_m20250815_extras :: Query
down_m20250815_extras =
  [sql|
ALTER TABLE protocol_servers DROP COLUMN extras;
|]
