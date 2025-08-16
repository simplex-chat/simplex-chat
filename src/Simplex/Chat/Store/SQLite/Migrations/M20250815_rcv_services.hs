{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250815_rcv_services where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250815_rcv_services :: Query
m20250815_rcv_services =
  [sql|
ALTER TABLE users ADD COLUMN use_rcv_services TEXT;
|]

down_m20250815_rcv_services :: Query
down_m20250815_rcv_services =
  [sql|
ALTER TABLE users DROP COLUMN use_rcv_services;
|]
