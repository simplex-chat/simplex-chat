{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230411_xftp_file_descriptions_extra where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230411_xftp_file_descriptions_extra :: Query
m20230411_xftp_file_descriptions_extra =
  [sql|
DELETE FROM xftp_file_descriptions;

ALTER TABLE xftp_file_descriptions ADD COLUMN file_id INTEGER NOT NULL REFERENCES files ON DELETE CASCADE;
CREATE INDEX idx_xftp_file_descriptions_file_id ON xftp_file_descriptions(file_id);

ALTER TABLE xftp_file_descriptions ADD COLUMN extra INTEGER NOT NULL DEFAULT 0;
|]
