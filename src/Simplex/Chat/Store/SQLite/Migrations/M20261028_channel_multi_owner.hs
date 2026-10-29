{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261028_channel_multi_owner where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261028_channel_multi_owner :: Query
m20261028_channel_multi_owner =
  [sql|
ALTER TABLE groups ADD COLUMN link_data_version INTEGER;
ALTER TABLE groups ADD COLUMN link_roster_version INTEGER;
ALTER TABLE groups ADD COLUMN link_roster_seen_at TEXT;
ALTER TABLE groups ADD COLUMN members_catchup INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN roster_pending_msg_id INTEGER;
ALTER TABLE groups ADD COLUMN link_owner_data BLOB;

ALTER TABLE group_members ADD COLUMN owner_inv_msg_id BLOB;
ALTER TABLE group_members ADD COLUMN owner_inv_accepted INTEGER NOT NULL DEFAULT 0;
ALTER TABLE group_members ADD COLUMN relay_web_domain TEXT;
|]

down_m20261028_channel_multi_owner :: Query
down_m20261028_channel_multi_owner =
  [sql|
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
