{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260924_conn_pres_header where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260924_conn_pres_header :: Query
m20260924_conn_pres_header =
  [sql|
ALTER TABLE connections ADD COLUMN pres_header BLOB;
|]

down_m20260924_conn_pres_header :: Query
down_m20260924_conn_pres_header =
  [sql|
ALTER TABLE connections DROP COLUMN pres_header;
|]
