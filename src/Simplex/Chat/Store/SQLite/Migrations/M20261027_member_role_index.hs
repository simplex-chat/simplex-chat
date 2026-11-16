{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261027_member_role_index where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261027_member_role_index :: Query
m20261027_member_role_index =
  [sql|
CREATE INDEX idx_group_members_user_id_member_role ON group_members(user_id, member_role, group_id);
CREATE INDEX idx_group_members_group_id_contact_id ON group_members(group_id, contact_id) WHERE contact_id IS NOT NULL;
|]

down_m20261027_member_role_index :: Query
down_m20261027_member_role_index =
  [sql|
DROP INDEX idx_group_members_group_id_contact_id;
DROP INDEX idx_group_members_user_id_member_role;
|]
