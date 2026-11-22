{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261113_item_priority where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261113_item_priority :: Text
m20261113_item_priority =
  [r|
ALTER TABLE chat_items ADD COLUMN item_priority SMALLINT NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_contacts_silent ON chat_items(user_id, item_status, contact_id, item_priority) WHERE item_priority < 0;

CREATE INDEX idx_chat_items_groups_priority ON chat_items(user_id, item_status, group_id, group_scope_tag, group_scope_group_member_id, item_priority) WHERE item_priority <> 0;

ALTER TABLE delivery_tasks ADD COLUMN update_notification SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE chat_items ADD COLUMN item_original_ts TIMESTAMPTZ;

ALTER TABLE group_members ADD COLUMN member_via BYTEA;
|]

down_m20261113_item_priority :: Text
down_m20261113_item_priority =
  [r|
UPDATE group_members SET member_status = 'removed'
WHERE member_via IN (SELECT u.member_id FROM group_members u WHERE u.group_id = group_members.group_id AND u.member_category = 'user');

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
