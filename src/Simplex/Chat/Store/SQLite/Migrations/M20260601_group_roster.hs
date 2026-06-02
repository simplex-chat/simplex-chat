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
|]

down_m20260601_group_roster :: Query
down_m20260601_group_roster =
  [sql|
ALTER TABLE groups DROP COLUMN roster_broker_ts;
ALTER TABLE groups DROP COLUMN roster_sending_owner_gm_id;
ALTER TABLE groups DROP COLUMN roster_msg_signatures;
ALTER TABLE groups DROP COLUMN roster_msg_chat_binding;
ALTER TABLE groups DROP COLUMN roster_msg_body;
ALTER TABLE groups DROP COLUMN roster_version;
|]
