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
ALTER TABLE groups ADD COLUMN roster_pending_version INTEGER;
ALTER TABLE groups ADD COLUMN roster_pending_digest BLOB;
ALTER TABLE groups ADD COLUMN roster_pending_msg_body BLOB;
ALTER TABLE groups ADD COLUMN roster_pending_msg_chat_binding TEXT;
ALTER TABLE groups ADD COLUMN roster_pending_msg_signatures BLOB;
ALTER TABLE groups ADD COLUMN roster_pending_sending_owner_gm_id INTEGER;
ALTER TABLE groups ADD COLUMN roster_pending_broker_ts TEXT;
ALTER TABLE files ADD COLUMN shared_msg_id BLOB;
ALTER TABLE files ADD COLUMN file_type TEXT NOT NULL DEFAULT 'normal';
CREATE INDEX idx_files_group_id_shared_msg_id ON files(group_id, shared_msg_id);
|]

down_m20260601_group_roster :: Query
down_m20260601_group_roster =
  [sql|
DROP INDEX IF EXISTS idx_files_group_id_shared_msg_id;
ALTER TABLE files DROP COLUMN file_type;
ALTER TABLE files DROP COLUMN shared_msg_id;
ALTER TABLE groups DROP COLUMN roster_pending_broker_ts;
ALTER TABLE groups DROP COLUMN roster_pending_sending_owner_gm_id;
ALTER TABLE groups DROP COLUMN roster_pending_msg_signatures;
ALTER TABLE groups DROP COLUMN roster_pending_msg_chat_binding;
ALTER TABLE groups DROP COLUMN roster_pending_msg_body;
ALTER TABLE groups DROP COLUMN roster_pending_digest;
ALTER TABLE groups DROP COLUMN roster_pending_version;
ALTER TABLE groups DROP COLUMN roster_blob;
ALTER TABLE groups DROP COLUMN roster_broker_ts;
ALTER TABLE groups DROP COLUMN roster_sending_owner_gm_id;
ALTER TABLE groups DROP COLUMN roster_msg_signatures;
ALTER TABLE groups DROP COLUMN roster_msg_chat_binding;
ALTER TABLE groups DROP COLUMN roster_msg_body;
ALTER TABLE groups DROP COLUMN roster_version;
|]
