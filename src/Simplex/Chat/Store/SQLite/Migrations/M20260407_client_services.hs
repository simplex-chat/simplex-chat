{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260407_client_services where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260407_client_services :: Query
m20260407_client_services =
  [sql|
ALTER TABLE users ADD COLUMN client_service INTEGER NOT NULL DEFAULT 0;
|]

down_m20260407_client_services :: Query
down_m20260407_client_services =
  [sql|
ALTER TABLE users DROP COLUMN client_service;
|]
