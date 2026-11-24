{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261124_chat_list_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261124_chat_list_indexes :: Query
m20261124_chat_list_indexes =
  [sql|
DROP INDEX idx_chat_items_user_id_item_status;
DROP INDEX idx_chat_items_contacts;
CREATE INDEX idx_chat_items_contacts ON chat_items(user_id, item_status, contact_id, created_at, chat_item_id);
DROP INDEX idx_chat_items_group_scope_stats_all;
CREATE INDEX idx_chat_items_group_scope_stats_all ON chat_items(user_id, item_status, group_id, group_scope_tag, group_scope_group_member_id, chat_item_id, user_mention);
DROP INDEX idx_chat_items_groups_msg_content_tag_deleted;
CREATE INDEX idx_chat_items_groups_msg_content_tag_deleted ON chat_items(user_id, msg_content_tag, item_deleted, group_id, item_sent);
DROP INDEX idx_chat_items_groups_item_viewed;
CREATE INDEX idx_chat_items_groups_item_viewed ON chat_items(user_id, group_id, item_viewed, item_ts);
|]

down_m20261124_chat_list_indexes :: Query
down_m20261124_chat_list_indexes =
  [sql|
DROP INDEX idx_chat_items_groups_msg_content_tag_deleted;
CREATE INDEX idx_chat_items_groups_msg_content_tag_deleted ON chat_items(user_id, group_id, msg_content_tag, item_deleted, item_sent);
DROP INDEX idx_chat_items_group_scope_stats_all;
CREATE INDEX idx_chat_items_group_scope_stats_all ON chat_items(user_id, group_id, group_scope_tag, group_scope_group_member_id, item_status, chat_item_id, user_mention);
DROP INDEX idx_chat_items_contacts;
CREATE INDEX idx_chat_items_contacts ON chat_items(user_id, contact_id, item_status, created_at);
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(user_id, item_status);
|]
