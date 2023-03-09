{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230304_file_description where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- this table includes file descriptions for the recipients for both sent and received files
-- in the latter case the user is the recipient

m20230304_file_description :: Query
m20230304_file_description =
  [sql|
CREATE TABLE recipient_file_descriptions (
  file_descr_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_descr_size INTEGER NOT NULL,
  file_descr_status TEXT NOT NULL,
  file_descr_text TEXT NOT NULL
);

ALTER TABLE rcv_files ADD COLUMN file_descr_id INTEGER NULL
  REFERENCES recipient_file_descriptions(file_descr_id) ON DELETE RESTRICT;

ALTER TABLE snd_files ADD COLUMN file_descr_id INTEGER NULL
  REFERENCES recipient_file_descriptions(file_descr_id) ON DELETE RESTRICT;

 -- this is a private file description allowing to delete the file from the server
ALTER TABLE files ADD COLUMN snd_file_descr_text TEXT NULL;
|]
