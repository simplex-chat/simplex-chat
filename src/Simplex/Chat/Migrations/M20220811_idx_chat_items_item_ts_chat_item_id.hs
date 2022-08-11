{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220811_idx_chat_items_item_ts_chat_item_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220811_idx_chat_items_item_ts_chat_item_id :: Query
m20220811_idx_chat_items_item_ts_chat_item_id =
  [sql|
CREATE INDEX idx_chat_items_item_ts_chat_item_id ON chat_items(item_ts, chat_item_id);
|]
