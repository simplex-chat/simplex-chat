{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261113_silent_indexes where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261113_silent_indexes :: Text
m20261113_silent_indexes =
  [r|
CREATE INDEX idx_chat_items_contacts_silent ON chat_items(user_id, item_status, contact_id, item_priority) WHERE item_priority < 0;

CREATE INDEX idx_chat_items_groups_priority ON chat_items(user_id, item_status, group_id, group_scope_tag, group_scope_group_member_id, item_priority) WHERE item_priority <> 0;
|]

down_m20261113_silent_indexes :: Text
down_m20261113_silent_indexes =
  [r|
DROP INDEX idx_chat_items_groups_priority;

DROP INDEX idx_chat_items_contacts_silent;
|]
