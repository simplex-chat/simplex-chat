{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230411_extra_xftp_file_descriptions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230411_extra_xftp_file_descriptions :: Query
m20230411_extra_xftp_file_descriptions =
  [sql|
CREATE TABLE extra_xftp_file_descriptions (
  extra_file_descr_id INTEGER PRIMARY KEY,
  file_id INTEGER NOT NULL REFERENCES files ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  file_descr_text TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_extra_xftp_file_descriptions_file_id ON extra_xftp_file_descriptions(file_id);
CREATE INDEX idx_extra_xftp_file_descriptions_user_id ON extra_xftp_file_descriptions(user_id);

CREATE INDEX idx_xftp_file_descriptions_user_id ON xftp_file_descriptions(user_id);
|]

down_m20230411_extra_xftp_file_descriptions :: Query
down_m20230411_extra_xftp_file_descriptions =
  [sql|
DROP INDEX idx_xftp_file_descriptions_user_id;

DROP INDEX idx_extra_xftp_file_descriptions_user_id;
DROP INDEX idx_extra_xftp_file_descriptions_file_id;

DROP TABLE extra_xftp_file_descriptions;
|]
