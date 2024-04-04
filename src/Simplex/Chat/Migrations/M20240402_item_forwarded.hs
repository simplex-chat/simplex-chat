{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240402_item_forwarded where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240402_item_forwarded :: Query
m20240402_item_forwarded =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_forwarded INTEGER;
|]

down_m20240402_item_forwarded :: Query
down_m20240402_item_forwarded =
  [sql|
ALTER TABLE chat_items DROP COLUMN item_forwarded;
|]

-- ALTER TABLE chat_items DROP COLUMN forwarded_from_str;
-- ALTER TABLE chat_items DROP COLUMN forwarded_from_contact_id;
-- ALTER TABLE chat_items DROP COLUMN forwarded_from_group_id;
-- ALTER TABLE chat_items DROP COLUMN forwarded_from_note_folder_id;
