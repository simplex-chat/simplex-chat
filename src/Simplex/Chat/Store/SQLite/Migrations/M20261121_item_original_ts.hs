{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261121_item_original_ts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261121_item_original_ts :: Query
m20261121_item_original_ts =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_original_ts TEXT;
|]

down_m20261121_item_original_ts :: Query
down_m20261121_item_original_ts =
  [sql|
ALTER TABLE chat_items DROP COLUMN item_original_ts;
|]
