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
  rcp_file_descr_id INTEGER PRIMARY KEY AUTOINCREMENT,
  rcp_file_descr_text TEXT NOT NULL
);

ALTER TABLE rcv_files ADD COLUMN rcp_file_descr_id TEXT NULL;

ALTER TABLE snd_files ADD COLUMN rcp_file_descr_id TEXT NULL;
|]
