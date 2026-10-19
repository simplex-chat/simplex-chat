{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220404_files_status_fields where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220404_files_status_fields :: Query
m20220404_files_status_fields =
  [sql|
ALTER TABLE files ADD COLUMN cancelled INTEGER; -- 1 for cancelled
ALTER TABLE files ADD COLUMN ci_file_status TEXT; -- CIFileStatus

DELETE FROM chat_items
WHERE chat_item_id IN (
  SELECT chat_item_id
  FROM files
);
|]
