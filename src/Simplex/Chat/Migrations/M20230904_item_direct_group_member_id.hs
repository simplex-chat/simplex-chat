{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230904_item_direct_group_member_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230904_item_direct_group_member_id :: Query
m20230904_item_direct_group_member_id =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_direct_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN quoted_message_scope TEXT;

CREATE INDEX idx_chat_items_item_direct_group_member_id ON chat_items(item_direct_group_member_id);
|]

down_m20230904_item_direct_group_member_id :: Query
down_m20230904_item_direct_group_member_id =
  [sql|
DROP INDEX idx_chat_items_item_direct_group_member_id;

ALTER TABLE chat_items DROP COLUMN quoted_message_scope;
ALTER TABLE chat_items DROP COLUMN item_direct_group_member_id;
|]
