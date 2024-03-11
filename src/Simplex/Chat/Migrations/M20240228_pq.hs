{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240228_pq where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240228_pq :: Query
m20240228_pq =
  [sql|
ALTER TABLE connections ADD COLUMN conn_chat_version INTEGER;
ALTER TABLE connections ADD COLUMN pq_support INTEGER NOT NULL DEFAULT 0;
ALTER TABLE connections ADD COLUMN pq_encryption INTEGER NOT NULL DEFAULT 0;
ALTER TABLE connections ADD COLUMN pq_snd_enabled INTEGER;
ALTER TABLE connections ADD COLUMN pq_rcv_enabled INTEGER;

ALTER TABLE contact_requests ADD COLUMN pq_support INTEGER NOT NULL DEFAULT 0;
|]

down_m20240228_pq :: Query
down_m20240228_pq =
  [sql|
ALTER TABLE contact_requests DROP COLUMN pq_support;

ALTER TABLE connections DROP COLUMN conn_chat_version;
ALTER TABLE connections DROP COLUMN pq_support;
ALTER TABLE connections DROP COLUMN pq_encryption;
ALTER TABLE connections DROP COLUMN pq_snd_enabled;
ALTER TABLE connections DROP COLUMN pq_rcv_enabled;
|]
