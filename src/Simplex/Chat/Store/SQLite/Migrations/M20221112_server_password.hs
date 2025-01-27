{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221112_server_password where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221112_server_password :: Query
m20221112_server_password =
  [sql|
ALTER TABLE smp_servers ADD COLUMN basic_auth TEXT;
|]
