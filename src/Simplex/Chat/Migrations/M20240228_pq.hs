{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240228_pq where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240228_pq :: Query
m20240228_pq =
  [sql|
ALTER TABLE connections ADD COLUMN pq_snd_enabled INTEGER;
ALTER TABLE connections ADD COLUMN pq_rcv_enabled INTEGER;
|]

down_m20240228_pq :: Query
down_m20240228_pq =
  [sql|
ALTER TABLE connections DROP COLUMN pq_snd_enabled;
ALTER TABLE connections DROP COLUMN pq_rcv_enabled;
|]
