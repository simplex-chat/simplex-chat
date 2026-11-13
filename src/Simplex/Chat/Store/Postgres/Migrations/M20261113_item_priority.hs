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

CREATE INDEX idx_chat_items_groups_silent ON chat_items(user_id, item_status, group_id, group_scope_tag, group_scope_group_member_id, item_priority) WHERE item_priority < 0;
|]

down_m20261113_item_priority :: Text
down_m20261113_item_priority =
  [r|
DROP INDEX idx_chat_items_groups_silent;

DROP INDEX idx_chat_items_contacts_silent;

ALTER TABLE chat_items DROP COLUMN item_priority;
|]
