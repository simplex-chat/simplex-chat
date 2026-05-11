{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260403_item_viewed where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260403_item_viewed :: Query
m20260403_item_viewed =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_viewed INTEGER NOT NULL DEFAULT 0;
CREATE INDEX idx_chat_items_contacts_item_viewed ON chat_items(user_id, contact_id, item_viewed, created_at);
CREATE INDEX idx_chat_items_groups_item_viewed ON chat_items(user_id, group_id, item_viewed, item_ts);
|]

down_m20260403_item_viewed :: Query
down_m20260403_item_viewed =
  [sql|
DROP INDEX idx_chat_items_contacts_item_viewed;
DROP INDEX idx_chat_items_groups_item_viewed;
ALTER TABLE chat_items DROP COLUMN item_viewed;
|]
