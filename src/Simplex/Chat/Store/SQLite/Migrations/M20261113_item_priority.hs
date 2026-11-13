{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261113_item_priority where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261113_item_priority :: Query
m20261113_item_priority =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_priority INTEGER NOT NULL DEFAULT 0;
|]

down_m20261113_item_priority :: Query
down_m20261113_item_priority =
  [sql|
ALTER TABLE chat_items DROP COLUMN item_priority;
|]
