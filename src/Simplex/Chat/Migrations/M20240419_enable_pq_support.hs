{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240419_enable_pq_support where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240419_enable_pq_support :: Query
m20240419_enable_pq_support =
  [sql|
UPDATE connections SET pq_support = 1 WHERE conn_type = 'contact';
|]

down_m20240419_enable_pq_support :: Query
down_m20240419_enable_pq_support =
  [sql|
|]
