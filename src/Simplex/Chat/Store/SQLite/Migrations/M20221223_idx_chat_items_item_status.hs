{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221223_idx_chat_items_item_status where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221223_idx_chat_items_item_status :: Query
m20221223_idx_chat_items_item_status =
  [sql|
CREATE INDEX idx_chat_items_item_status ON chat_items(item_status);
|]
