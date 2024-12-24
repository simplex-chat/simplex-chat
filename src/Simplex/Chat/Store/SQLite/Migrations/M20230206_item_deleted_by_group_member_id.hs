{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230206_item_deleted_by_group_member_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230206_item_deleted_by_group_member_id :: Query
m20230206_item_deleted_by_group_member_id =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_deleted_by_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;

CREATE INDEX idx_chat_items_item_deleted_by_group_member_id ON chat_items(item_deleted_by_group_member_id);
|]
