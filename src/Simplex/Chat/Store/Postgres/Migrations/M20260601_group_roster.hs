{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260601_group_roster where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260601_group_roster :: Text
m20260601_group_roster =
  [r|
ALTER TABLE groups ADD COLUMN roster_version INTEGER;
ALTER TABLE groups ADD COLUMN roster_msg_body BYTEA;
ALTER TABLE groups ADD COLUMN roster_msg_chat_binding TEXT;
ALTER TABLE groups ADD COLUMN roster_msg_signatures BYTEA;
ALTER TABLE groups ADD COLUMN roster_sending_owner_gm_id BIGINT;
ALTER TABLE groups ADD COLUMN roster_broker_ts TIMESTAMPTZ;
|]

down_m20260601_group_roster :: Text
down_m20260601_group_roster =
  [r|
ALTER TABLE groups DROP COLUMN roster_broker_ts;
ALTER TABLE groups DROP COLUMN roster_sending_owner_gm_id;
ALTER TABLE groups DROP COLUMN roster_msg_signatures;
ALTER TABLE groups DROP COLUMN roster_msg_chat_binding;
ALTER TABLE groups DROP COLUMN roster_msg_body;
ALTER TABLE groups DROP COLUMN roster_version;
|]
