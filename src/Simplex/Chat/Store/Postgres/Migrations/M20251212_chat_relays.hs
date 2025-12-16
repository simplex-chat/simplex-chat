{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251212_chat_relays where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20251212_chat_relays :: Text
m20251212_chat_relays =
  T.pack
    [r|
CREATE TABLE chat_relays(
  chat_relay_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  address BYTEA NOT NULL,
  name TEXT NOT NULL,
  domains TEXT NOT NULL,
  preset SMALLINT NOT NULL DEFAULT 0,
  tested SMALLINT,
  enabled SMALLINT NOT NULL DEFAULT 1,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  deleted SMALLINT NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT (now()),
  updated_at TEXT NOT NULL DEFAULT (now())
);
CREATE INDEX idx_chat_relays_user_id ON chat_relays(user_id);
CREATE UNIQUE INDEX idx_chat_relays_user_id_address ON chat_relays(user_id, address);
CREATE UNIQUE INDEX idx_chat_relays_user_id_name ON chat_relays(user_id, name);

ALTER TABLE users ADD COLUMN is_user_chat_relay SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN use_relays SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN creating_in_progress SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN relay_own_status TEXT;

ALTER TABLE groups ADD COLUMN relay_request_inv_id BYTEA;
ALTER TABLE groups ADD COLUMN relay_request_group_link BYTEA;
ALTER TABLE groups ADD COLUMN relay_request_peer_chat_min_version INTEGER;
ALTER TABLE groups ADD COLUMN relay_request_peer_chat_max_version INTEGER;
ALTER TABLE groups ADD COLUMN relay_request_failed SMALLINT DEFAULT 0;

ALTER TABLE group_profiles ADD COLUMN group_link BYTEA;

CREATE TABLE group_relays(
  group_relay_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_id BIGINT NOT NULL REFERENCES groups ON DELETE CASCADE,
  group_member_id BIGINT NOT NULL REFERENCES group_members ON DELETE CASCADE,
  chat_relay_id BIGINT NOT NULL REFERENCES chat_relays ON DELETE CASCADE,
  relay_status TEXT NOT NULL,
  relay_link BYTEA,
  created_at TEXT NOT NULL DEFAULT (now()),
  updated_at TEXT NOT NULL DEFAULT (now())
);
CREATE INDEX idx_group_relays_group_id ON group_relays(group_id);
CREATE UNIQUE INDEX idx_group_relays_group_member_id ON group_relays(group_member_id);
CREATE INDEX idx_group_relays_chat_relay_id ON group_relays(chat_relay_id);

ALTER TABLE group_members ADD COLUMN is_relay SMALLINT NOT NULL DEFAULT 0;
|]

down_m20251212_chat_relays :: Text
down_m20251212_chat_relays =
  T.pack
    [r|
ALTER TABLE users DROP COLUMN is_user_chat_relay;

ALTER TABLE groups DROP COLUMN use_relays;

ALTER TABLE groups DROP COLUMN creating_in_progress;

ALTER TABLE groups DROP COLUMN relay_own_status;

ALTER TABLE groups DROP COLUMN relay_request_inv_id;
ALTER TABLE groups DROP COLUMN relay_request_group_link;
ALTER TABLE groups DROP COLUMN relay_request_peer_chat_min_version;
ALTER TABLE groups DROP COLUMN relay_request_peer_chat_max_version;
ALTER TABLE groups DROP COLUMN relay_request_failed;

ALTER TABLE group_profiles DROP COLUMN group_link;

DROP INDEX idx_group_relays_group_id;
DROP INDEX idx_group_relays_group_member_id;
DROP INDEX idx_group_relays_chat_relay_id;
DROP TABLE group_relays;

DROP INDEX idx_chat_relays_user_id;
DROP INDEX idx_chat_relays_user_id_address;
DROP INDEX idx_chat_relays_user_id_name;
DROP TABLE chat_relays;

ALTER TABLE group_members DROP COLUMN is_relay;
|]
