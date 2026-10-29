{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261028_channel_multi_owner where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261028_channel_multi_owner :: Text
m20261028_channel_multi_owner =
  [r|
ALTER TABLE groups ADD COLUMN link_data_version BIGINT;
ALTER TABLE groups ADD COLUMN link_roster_version BIGINT;
ALTER TABLE groups ADD COLUMN link_roster_seen_at TIMESTAMPTZ;
ALTER TABLE groups ADD COLUMN members_catchup SMALLINT NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN roster_pending_msg_id BIGINT;
ALTER TABLE groups ADD COLUMN link_owner_data BYTEA;

ALTER TABLE group_members ADD COLUMN owner_inv_msg_id BYTEA;
ALTER TABLE group_members ADD COLUMN owner_inv_accepted SMALLINT NOT NULL DEFAULT 0;
ALTER TABLE group_members ADD COLUMN relay_web_domain TEXT;
|]

down_m20261028_channel_multi_owner :: Text
down_m20261028_channel_multi_owner =
  [r|
ALTER TABLE group_members DROP COLUMN relay_web_domain;
ALTER TABLE group_members DROP COLUMN owner_inv_accepted;
ALTER TABLE group_members DROP COLUMN owner_inv_msg_id;

ALTER TABLE groups DROP COLUMN link_owner_data;
ALTER TABLE groups DROP COLUMN roster_pending_msg_id;
ALTER TABLE groups DROP COLUMN members_catchup;
ALTER TABLE groups DROP COLUMN link_roster_seen_at;
ALTER TABLE groups DROP COLUMN link_roster_version;
ALTER TABLE groups DROP COLUMN link_data_version;
|]
