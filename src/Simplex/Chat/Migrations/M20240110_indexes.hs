{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240110_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240110_indexes :: Query
m20240110_indexes =
  [sql|
CREATE INDEX idx_chat_items_direct_item_status ON chat_items(user_id, contact_id, item_status);
CREATE INDEX idx_chat_items_group_item_status ON chat_items(user_id, group_id, item_status);
|]

down_m20240110_indexes :: Query
down_m20240110_indexes =
  [sql|
DROP INDEX idx_chat_items_group_item_status;
DROP INDEX idx_chat_items_direct_item_status;
|]
