{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250704_groups_conn_link_prepared_connection where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250704_groups_conn_link_prepared_connection :: Query
m20250704_groups_conn_link_prepared_connection =
  [sql|
ALTER TABLE groups ADD COLUMN conn_link_prepared_connection INTEGER NOT NULL DEFAULT 0;
|]

down_m20250704_groups_conn_link_prepared_connection :: Query
down_m20250704_groups_conn_link_prepared_connection =
  [sql|
ALTER TABLE groups DROP COLUMN conn_link_prepared_connection;
|]
