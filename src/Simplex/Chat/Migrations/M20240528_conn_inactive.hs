{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240528_conn_inactive where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240528_conn_inactive :: Query
m20240528_conn_inactive =
  [sql|
ALTER TABLE connections ADD COLUMN inactive INTEGER NOT NULL DEFAULT 0;
|]

down_m20240528_conn_inactive :: Query
down_m20240528_conn_inactive =
  [sql|
ALTER TABLE connections DROP COLUMN inactive;
|]
