{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230123_recreate_snd_files where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- nullable connection_id
m20230123_recreate_snd_files :: Query
m20230123_recreate_snd_files =
  [sql|
DROP INDEX idx_snd_files_last_inline_msg_delivery_id;
DROP INDEX idx_snd_files_group_member_id;
DROP INDEX idx_snd_files_connection_id;
DROP INDEX idx_snd_files_file_id;

CREATE TABLE new_snd_files (
  snd_file_id INTEGER PRIMARY KEY,
  file_id INTEGER NOT NULL REFERENCES files ON DELETE CASCADE,
  connection_id INTEGER REFERENCES connections ON DELETE CASCADE,
  file_status TEXT NOT NULL, -- new, accepted, connected, completed
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  file_inline TEXT,
  last_inline_msg_delivery_id INTEGER,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

INSERT INTO new_snd_files
  (file_id, connection_id, file_status, group_member_id, file_inline, last_inline_msg_delivery_id, created_at, updated_at)
SELECT
  file_id, connection_id, file_status, group_member_id, file_inline, last_inline_msg_delivery_id, created_at, updated_at
  FROM snd_files;

DROP TABLE snd_files;
ALTER TABLE new_snd_files RENAME TO snd_files;

CREATE UNIQUE INDEX idx_snd_files_last_inline_msg_delivery_id ON snd_files(last_inline_msg_delivery_id);
CREATE INDEX idx_snd_files_group_member_id ON snd_files(group_member_id);
CREATE INDEX idx_snd_files_connection_id ON snd_files(connection_id);
CREATE INDEX idx_snd_files_file_id ON snd_files(file_id);
|]
