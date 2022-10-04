{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221004_idx_chat_items_item_ts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221004_idx_chat_items_item_ts :: Query
m20221004_idx_chat_items_item_ts =
  [sql|
CREATE INDEX idx_chat_items_item_ts ON chat_items(user_id, item_ts);
|]
