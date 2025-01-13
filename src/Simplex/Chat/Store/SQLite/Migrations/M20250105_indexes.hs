{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250105_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250105_indexes :: Query
m20250105_indexes =
  [sql|
CREATE INDEX idx_chat_items_groups_msg_content_tag_item_ts ON chat_items(user_id, group_id, msg_content_tag, item_ts);
CREATE INDEX idx_chat_items_groups_msg_content_tag_deleted ON chat_items(user_id, group_id, msg_content_tag, item_deleted, item_sent);
|]

down_m20250105_indexes :: Query
down_m20250105_indexes =
  [sql|
DROP INDEX idx_chat_items_groups_msg_content_tag_item_ts;
DROP INDEX idx_chat_items_groups_msg_content_tag_deleted;
|]
