{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261113_item_priority where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261113_item_priority :: Query
m20261113_item_priority =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_priority INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_contacts_silent ON chat_items(user_id, item_status, contact_id, item_priority) WHERE item_priority < 0 AND contact_id IS NOT NULL;

CREATE INDEX idx_chat_items_groups_priority ON chat_items(user_id, item_status, group_id, group_scope_tag, group_scope_group_member_id, item_priority) WHERE item_priority <> 0 AND group_id IS NOT NULL;

ALTER TABLE delivery_tasks ADD COLUMN update_notification INTEGER NOT NULL DEFAULT 0;

ALTER TABLE chat_items ADD COLUMN item_original_ts TEXT;

ALTER TABLE group_members ADD COLUMN member_via BLOB;
|]

down_m20261113_item_priority :: Query
down_m20261113_item_priority =
  [sql|
UPDATE group_members SET member_status = 'removed'
WHERE member_via IS NOT NULL AND EXISTS (SELECT 1 FROM group_members u WHERE u.group_id = group_members.group_id AND u.member_id = group_members.member_via AND u.member_category = 'user');

UPDATE group_members SET member_status = 'announced'
WHERE member_via IS NOT NULL AND member_status = 'connected';

UPDATE chat_items SET group_member_id = NULL
WHERE item_sent = 1 AND group_member_id IS NOT NULL;

ALTER TABLE group_members DROP COLUMN member_via;

ALTER TABLE chat_items DROP COLUMN item_original_ts;

ALTER TABLE delivery_tasks DROP COLUMN update_notification;

DROP INDEX idx_chat_items_groups_priority;

DROP INDEX idx_chat_items_contacts_silent;

ALTER TABLE chat_items DROP COLUMN item_priority;
|]
