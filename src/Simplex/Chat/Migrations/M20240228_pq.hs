{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240228_pq where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240228_pq :: Query
m20240228_pq =
  [sql|
ALTER TABLE connections ADD COLUMN pq_enabled INTEGER;

ALTER TABLE chat_items ADD COLUMN pq_encryption INTEGER;

ALTER TABLE group_snd_item_statuses ADD COLUMN group_snd_pq_encryption INTEGER;
|]

down_m20240228_pq :: Query
down_m20240228_pq =
  [sql|
ALTER TABLE group_snd_item_statuses DROP COLUMN group_snd_pq_encryption;

ALTER TABLE chat_items DROP COLUMN pq_encryption;

ALTER TABLE connections DROP COLUMN pq_enabled;
|]
