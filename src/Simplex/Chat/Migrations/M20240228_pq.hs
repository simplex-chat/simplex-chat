{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240228_pq where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240228_pq :: Query
m20240228_pq =
  [sql|
ALTER TABLE connections ADD COLUMN enable_pq INTEGER;
ALTER TABLE connections ADD COLUMN pq_snd_enabled INTEGER;
ALTER TABLE connections ADD COLUMN pq_rcv_enabled INTEGER;
ALTER TABLE contact_requests ADD COLUMN pq_support INTEGER NOT NULL DEFAULT 0;
|]

down_m20240228_pq :: Query
down_m20240228_pq =
  [sql|
ALTER TABLE connections DROP COLUMN enable_pq;
ALTER TABLE connections DROP COLUMN pq_snd_enabled;
ALTER TABLE connections DROP COLUMN pq_rcv_enabled;
ALTER TABLE contact_requests DROP COLUMN pq_support;
|]
