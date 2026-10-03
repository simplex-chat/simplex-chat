{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260926_member_role_index where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260926_member_role_index :: Query
m20260926_member_role_index =
  [sql|
CREATE INDEX idx_group_members_group_id_member_role ON group_members(user_id, group_id, member_role);
DROP INDEX idx_group_members_group_id;
|]

down_m20260926_member_role_index :: Query
down_m20260926_member_role_index =
  [sql|
CREATE INDEX idx_group_members_group_id ON group_members(user_id, group_id);
DROP INDEX idx_group_members_group_id_member_role;
|]
