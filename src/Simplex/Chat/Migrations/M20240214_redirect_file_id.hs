{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240214_redirect_file_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240214_redirect_file_id :: Query
m20240214_redirect_file_id =
  [sql|
ALTER TABLE files ADD COLUMN redirect_file_id INTEGER REFERENCES files ON DELETE CASCADE;

CREATE INDEX idx_files_redirect_file_id on files(redirect_file_id);
|]

down_m20240214_redirect_file_id :: Query
down_m20240214_redirect_file_id =
  [sql|
DROP INDEX idx_files_redirect_file_id;

ALTER TABLE files DROP COLUMN redirect_file_id;
|]
