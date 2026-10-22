{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261018_member_role_index where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261018_member_role_index :: Text
m20261018_member_role_index =
  [r|
CREATE INDEX idx_group_members_user_id_member_role ON group_members(user_id, member_role, group_id);
|]

down_m20261018_member_role_index :: Text
down_m20261018_member_role_index =
  [r|
DROP INDEX idx_group_members_user_id_member_role;
|]
