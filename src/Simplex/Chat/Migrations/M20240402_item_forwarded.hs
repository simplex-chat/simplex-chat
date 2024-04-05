{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240402_item_forwarded where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240402_item_forwarded :: Query
m20240402_item_forwarded =
  [sql|
ALTER TABLE chat_items ADD COLUMN fwd_from_tag TEXT;
ALTER TABLE chat_items ADD COLUMN fwd_from_chat_name TEXT;
ALTER TABLE chat_items ADD COLUMN fwd_from_msg_dir INTEGER;
ALTER TABLE chat_items ADD COLUMN fwd_from_contact_id INTEGER REFERENCES contacts ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN fwd_from_group_id INTEGER REFERENCES groups ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN fwd_from_chat_item_id INTEGER REFERENCES chat_items ON DELETE SET NULL;

CREATE INDEX idx_chat_items_fwd_from_contact_id ON chat_items(fwd_from_contact_id);
CREATE INDEX idx_chat_items_fwd_from_group_id ON chat_items(fwd_from_group_id);
CREATE INDEX idx_chat_items_fwd_from_chat_item_id ON chat_items(fwd_from_chat_item_id);
|]

down_m20240402_item_forwarded :: Query
down_m20240402_item_forwarded =
  [sql|
DROP INDEX idx_chat_items_fwd_from_contact_id;
DROP INDEX idx_chat_items_fwd_from_group_id;
DROP INDEX idx_chat_items_fwd_from_chat_item_id;

ALTER TABLE chat_items DROP COLUMN fwd_from_tag;
ALTER TABLE chat_items DROP COLUMN fwd_from_chat_name;
ALTER TABLE chat_items DROP COLUMN fwd_from_msg_dir;
ALTER TABLE chat_items DROP COLUMN fwd_from_contact_id;
ALTER TABLE chat_items DROP COLUMN fwd_from_group_id;
ALTER TABLE chat_items DROP COLUMN fwd_from_chat_item_id;
|]
