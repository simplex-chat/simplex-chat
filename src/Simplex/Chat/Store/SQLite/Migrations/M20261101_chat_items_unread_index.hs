{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261101_chat_items_unread_index where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261101_chat_items_unread_index :: Query
m20261101_chat_items_unread_index =
  [sql|
DROP INDEX idx_chat_items_user_id_item_status;
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(user_id, item_status, contact_id, group_id, user_mention);
|]

down_m20261101_chat_items_unread_index :: Query
down_m20261101_chat_items_unread_index =
  [sql|
DROP INDEX idx_chat_items_user_id_item_status;
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(user_id, item_status);
|]
