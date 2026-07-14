{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260714_server_roles where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260714_server_roles :: Query
m20260714_server_roles =
  [sql|
ALTER TABLE protocol_servers ADD COLUMN role_storage INTEGER;
ALTER TABLE protocol_servers ADD COLUMN role_proxy INTEGER;
ALTER TABLE protocol_servers ADD COLUMN role_names INTEGER;
|]

down_m20260714_server_roles :: Query
down_m20260714_server_roles =
  [sql|
ALTER TABLE protocol_servers DROP COLUMN role_storage;
ALTER TABLE protocol_servers DROP COLUMN role_proxy;
ALTER TABLE protocol_servers DROP COLUMN role_names;
|]
