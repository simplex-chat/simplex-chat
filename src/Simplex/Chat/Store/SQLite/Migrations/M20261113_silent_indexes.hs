{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261113_silent_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261113_silent_indexes :: Query
m20261113_silent_indexes =
  [sql|
CREATE INDEX idx_chat_items_contacts_silent ON chat_items(user_id, item_status, contact_id, item_priority) WHERE item_priority < 0;

CREATE INDEX idx_chat_items_groups_silent ON chat_items(user_id, item_status, group_id, group_scope_tag, group_scope_group_member_id, item_priority) WHERE item_priority < 0;
|]

down_m20261113_silent_indexes :: Query
down_m20261113_silent_indexes =
  [sql|
DROP INDEX idx_chat_items_groups_silent;

DROP INDEX idx_chat_items_contacts_silent;
|]
