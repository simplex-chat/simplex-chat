{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260926_member_role_index where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260926_member_role_index :: Text
m20260926_member_role_index =
  [r|
CREATE INDEX idx_group_members_group_id_member_role ON group_members(user_id, group_id, member_role);
DROP INDEX idx_group_members_group_id;
|]

down_m20260926_member_role_index :: Text
down_m20260926_member_role_index =
  [r|
CREATE INDEX idx_group_members_group_id ON group_members(user_id, group_id);
DROP INDEX idx_group_members_group_id_member_role;
|]
