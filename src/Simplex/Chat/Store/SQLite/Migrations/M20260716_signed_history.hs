{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260716_signed_history where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260716_signed_history :: Query
m20260716_signed_history =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_msg_body BLOB;
ALTER TABLE chat_items ADD COLUMN item_chat_binding TEXT;
ALTER TABLE chat_items ADD COLUMN item_signatures BLOB;
ALTER TABLE chat_items ADD COLUMN item_signed_by_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;

CREATE INDEX idx_chat_items_item_signed_by_group_member_id ON chat_items(item_signed_by_group_member_id);
|]

down_m20260716_signed_history :: Query
down_m20260716_signed_history =
  [sql|
DROP INDEX idx_chat_items_item_signed_by_group_member_id;

ALTER TABLE chat_items DROP COLUMN item_msg_body;
ALTER TABLE chat_items DROP COLUMN item_chat_binding;
ALTER TABLE chat_items DROP COLUMN item_signatures;
ALTER TABLE chat_items DROP COLUMN item_signed_by_group_member_id;
|]
