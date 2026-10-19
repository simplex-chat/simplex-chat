{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260602_group_roster where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260602_group_roster :: Text
m20260602_group_roster =
  [r|
ALTER TABLE groups ADD COLUMN roster_version BIGINT;
ALTER TABLE groups ADD COLUMN roster_msg_body BYTEA;
ALTER TABLE groups ADD COLUMN roster_msg_chat_binding TEXT;
ALTER TABLE groups ADD COLUMN roster_msg_signatures BYTEA;
ALTER TABLE groups ADD COLUMN roster_sending_owner_gm_id BIGINT;
ALTER TABLE groups ADD COLUMN roster_broker_ts TIMESTAMPTZ;
ALTER TABLE groups ADD COLUMN roster_blob BYTEA;

CREATE TABLE rcv_roster_transfers(
  roster_transfer_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_id BIGINT NOT NULL REFERENCES groups ON DELETE CASCADE,
  from_member_id BIGINT NOT NULL REFERENCES group_members ON DELETE CASCADE,
  roster_version BIGINT NOT NULL,
  roster_digest BYTEA NOT NULL,
  sending_owner_gm_id BIGINT NOT NULL,
  broker_ts TIMESTAMPTZ NOT NULL,
  roster_msg_body BYTEA,
  roster_msg_chat_binding TEXT,
  roster_msg_signatures BYTEA,
  created_at TEXT NOT NULL DEFAULT (now()),
  updated_at TEXT NOT NULL DEFAULT (now())
);
CREATE UNIQUE INDEX idx_rcv_roster_transfers_group_id_from_member_id ON rcv_roster_transfers(group_id, from_member_id);
CREATE INDEX idx_rcv_roster_transfers_from_member_id ON rcv_roster_transfers(from_member_id);

ALTER TABLE files ADD COLUMN shared_msg_id BYTEA;
ALTER TABLE files ADD COLUMN file_type TEXT NOT NULL DEFAULT 'normal';
ALTER TABLE files ADD COLUMN roster_transfer_id BIGINT;
CREATE INDEX idx_files_group_id_shared_msg_id ON files(group_id, shared_msg_id);
CREATE INDEX idx_files_roster_transfer_id ON files(roster_transfer_id);
|]

down_m20260602_group_roster :: Text
down_m20260602_group_roster =
  [r|
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
