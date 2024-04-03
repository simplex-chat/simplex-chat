{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240402_item_forwarded where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240402_item_forwarded :: Query
m20240402_item_forwarded =
  [sql|
ALTER TABLE chat_items ADD COLUMN forwarded_from_str TEXT;
ALTER TABLE chat_items ADD COLUMN forwarded_from_contact_id INTEGER REFERENCES contacts ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN forwarded_from_group_id INTEGER REFERENCES groups ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN forwarded_from_note_folder_id INTEGER REFERENCES note_folders ON DELETE SET NULL;

CREATE INDEX idx_chat_items_forwarded_from_contact_id ON chat_items(forwarded_from_contact_id);
CREATE INDEX idx_chat_items_forwarded_from_group_id ON chat_items(forwarded_from_group_id);
CREATE INDEX idx_chat_items_forwarded_from_note_folder_id ON chat_items(forwarded_from_note_folder_id);
|]

down_m20240402_item_forwarded :: Query
down_m20240402_item_forwarded =
  [sql|
DROP INDEX idx_chat_items_forwarded_from_contact_id;
DROP INDEX idx_chat_items_forwarded_from_group_id;
DROP INDEX idx_chat_items_forwarded_from_note_folder_id;

ALTER TABLE chat_items DROP COLUMN forwarded_from_str;
ALTER TABLE chat_items DROP COLUMN forwarded_from_contact_id;
ALTER TABLE chat_items DROP COLUMN forwarded_from_group_id;
ALTER TABLE chat_items DROP COLUMN forwarded_from_note_folder_id;
|]
