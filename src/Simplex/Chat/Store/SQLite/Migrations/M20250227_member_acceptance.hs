{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250227_member_acceptance where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250227_member_acceptance :: Query
m20250227_member_acceptance =
  [sql|
ALTER TABLE user_contact_links ADD COLUMN group_link_auto_accept TEXT;

DROP INDEX idx_chat_items_groups_history;
CREATE INDEX idx_chat_items_groups_history ON chat_items(
  user_id,
  group_id,
  include_in_history,
  item_deleted,
  group_member_id,
  item_ts,
  chat_item_id
);
|]

down_m20250227_member_acceptance :: Query
down_m20250227_member_acceptance =
  [sql|
ALTER TABLE user_contact_links DROP COLUMN group_link_auto_accept;

DROP INDEX idx_chat_items_groups_history;
CREATE INDEX idx_chat_items_groups_history ON chat_items(
  user_id,
  group_id,
  include_in_history,
  item_deleted,
  item_ts,
  chat_item_id
);
|]
