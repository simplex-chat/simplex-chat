{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250120_mentions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250120_mentions :: Query
m20250120_mentions =
  [sql|
ALTER TABLE chat_items ADD COLUMN member_mentions TEXT;
|]

down_m20250120_mentions :: Query
down_m20250120_mentions =
  [sql|
ALTER TABLE chat_items DROP COLUMN member_mentions;
|]
