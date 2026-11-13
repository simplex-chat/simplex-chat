{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261113_item_priority where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261113_item_priority :: Query
m20261113_item_priority =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_priority INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_contacts_silent ON chat_items(user_id, item_status, contact_id, item_priority) WHERE item_priority < 0;

CREATE INDEX idx_chat_items_groups_silent ON chat_items(user_id, item_status, group_id, group_scope_tag, group_scope_group_member_id, item_priority) WHERE item_priority < 0;
|]

down_m20261113_item_priority :: Query
down_m20261113_item_priority =
  [sql|
DROP INDEX idx_chat_items_groups_silent;

DROP INDEX idx_chat_items_contacts_silent;

ALTER TABLE chat_items DROP COLUMN item_priority;
|]
