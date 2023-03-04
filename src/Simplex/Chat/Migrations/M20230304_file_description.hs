{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230304_file_description where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230304_file_description :: Query
m20230304_file_description =
  [sql|
ALTER TABLE rcv_files ADD COLUMN file_descr_rcv_file_id INTEGER NULL REFERENCES rcv_files (rcv_file_id) ON DELETE CASCADE;
ALTER TABLE rcv_files ADD COLUMN file_descr_text TEXT NULL;
|]
