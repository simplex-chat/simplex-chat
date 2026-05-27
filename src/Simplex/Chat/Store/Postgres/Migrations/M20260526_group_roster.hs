{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260526_group_roster where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260526_group_roster :: Text
m20260526_group_roster =
  [r|
ALTER TABLE groups ADD COLUMN roster_version BIGINT;
ALTER TABLE groups ADD COLUMN roster_msg BYTEA;

ALTER TABLE group_members ADD COLUMN delivered_roster_version BIGINT;
|]

down_m20260526_group_roster :: Text
down_m20260526_group_roster =
  [r|
ALTER TABLE group_members DROP COLUMN delivered_roster_version;

ALTER TABLE groups DROP COLUMN roster_msg;
ALTER TABLE groups DROP COLUMN roster_version;
|]
