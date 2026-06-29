{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260629_member_roster_served where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260629_member_roster_served :: Text
m20260629_member_roster_served =
  [r|
ALTER TABLE group_members ADD COLUMN roster_served_version BIGINT;
|]

down_m20260629_member_roster_served :: Text
down_m20260629_member_roster_served =
  [r|
ALTER TABLE group_members DROP COLUMN roster_served_version;
|]
