{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261027_member_role_index where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261027_member_role_index :: Text
m20261027_member_role_index =
  [r|
CREATE INDEX idx_group_members_member_role ON group_members(member_role, user_id, group_id);
CREATE INDEX idx_group_members_group_id_contact_id ON group_members(group_id, contact_id) WHERE contact_id IS NOT NULL;
|]

down_m20261027_member_role_index :: Text
down_m20261027_member_role_index =
  [r|
DROP INDEX idx_group_members_group_id_contact_id;
DROP INDEX idx_group_members_member_role;
|]
