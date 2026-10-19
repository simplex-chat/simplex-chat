{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261010_invite_member_via_group where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261010_invite_member_via_group :: Text
m20261010_invite_member_via_group =
  [r|
ALTER TABLE group_members ADD COLUMN invited_via_group_member_id BIGINT REFERENCES group_members(group_member_id) ON DELETE SET NULL;
CREATE INDEX idx_group_members_invited_via_group_member_id ON group_members(invited_via_group_member_id);
|]

down_m20261010_invite_member_via_group :: Text
down_m20261010_invite_member_via_group =
  [r|
DROP INDEX idx_group_members_invited_via_group_member_id;
ALTER TABLE group_members DROP COLUMN invited_via_group_member_id;
|]
