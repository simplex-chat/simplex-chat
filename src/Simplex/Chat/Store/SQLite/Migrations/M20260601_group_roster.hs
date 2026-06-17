{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260601_group_roster where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260601_group_roster :: Query
m20260601_group_roster =
  [sql|
ALTER TABLE groups ADD COLUMN roster_version INTEGER;
ALTER TABLE groups ADD COLUMN roster_msg_body BLOB;
ALTER TABLE groups ADD COLUMN roster_msg_chat_binding TEXT;
ALTER TABLE groups ADD COLUMN roster_msg_signatures BLOB;
ALTER TABLE groups ADD COLUMN roster_sending_owner_gm_id INTEGER;
ALTER TABLE groups ADD COLUMN roster_broker_ts TEXT;
ALTER TABLE groups ADD COLUMN roster_blob BLOB;

CREATE TABLE rcv_roster_transfers(
  roster_transfer_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  from_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  roster_version INTEGER NOT NULL,
  roster_digest BLOB NOT NULL,
  sending_owner_gm_id INTEGER NOT NULL,
  broker_ts TEXT NOT NULL,
  roster_msg_body BLOB,
  roster_msg_chat_binding TEXT,
  roster_msg_signatures BLOB,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
) STRICT;
CREATE UNIQUE INDEX idx_rcv_roster_transfers_group_id_from_member_id ON rcv_roster_transfers(group_id, from_member_id);
CREATE INDEX idx_rcv_roster_transfers_from_member_id ON rcv_roster_transfers(from_member_id);

ALTER TABLE files ADD COLUMN shared_msg_id BLOB;
ALTER TABLE files ADD COLUMN file_type TEXT NOT NULL DEFAULT 'normal';
ALTER TABLE files ADD COLUMN roster_transfer_id INTEGER;
CREATE INDEX idx_files_group_id_shared_msg_id ON files(group_id, shared_msg_id);
CREATE INDEX idx_files_roster_transfer_id ON files(roster_transfer_id);
|]

down_m20260601_group_roster :: Query
down_m20260601_group_roster =
  [sql|
DROP INDEX idx_files_roster_transfer_id;
DROP INDEX idx_files_group_id_shared_msg_id;
ALTER TABLE files DROP COLUMN roster_transfer_id;
ALTER TABLE files DROP COLUMN file_type;
ALTER TABLE files DROP COLUMN shared_msg_id;

DROP INDEX idx_rcv_roster_transfers_from_member_id;
DROP INDEX idx_rcv_roster_transfers_group_id_from_member_id;
DROP TABLE rcv_roster_transfers;

ALTER TABLE groups DROP COLUMN roster_blob;
ALTER TABLE groups DROP COLUMN roster_broker_ts;
ALTER TABLE groups DROP COLUMN roster_sending_owner_gm_id;
ALTER TABLE groups DROP COLUMN roster_msg_signatures;
ALTER TABLE groups DROP COLUMN roster_msg_chat_binding;
ALTER TABLE groups DROP COLUMN roster_msg_body;
ALTER TABLE groups DROP COLUMN roster_version;
|]
