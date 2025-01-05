{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20250105_reports_msg_content where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250105_reports_msg_content :: Query
m20250105_reports_msg_content =
  [sql|
ALTER TABLE chat_items ADD COLUMN processed_by_group_member_id BLOB;

CREATE INDEX idx_chat_items_groups_msg_content_tag_item_ts ON chat_items(user_id, group_id, msg_content_tag, item_ts);
CREATE INDEX idx_chat_items_groups_msg_content_tag_item_deleted_item_ts ON chat_items(user_id, group_id, msg_content_tag, item_deleted, item_ts);
|]

down_m20250105_reports_msg_content :: Query
down_m20250105_reports_msg_content =
  [sql|
ALTER TABLE chat_items DROP COLUMN processed_by_group_member_id;

DROP INDEX idx_chat_items_groups_msg_content_tag_item_ts;
DROP INDEX idx_chat_items_groups_msg_content_tag_item_deleted_item_ts;
|]
