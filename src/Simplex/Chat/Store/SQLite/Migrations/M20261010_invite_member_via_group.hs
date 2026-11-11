{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261010_invite_member_via_group where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261010_invite_member_via_group :: Query
m20261010_invite_member_via_group =
  [sql|
ALTER TABLE group_members ADD COLUMN invited_via_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE SET NULL;
CREATE INDEX idx_group_members_invited_via_group_member_id ON group_members(invited_via_group_member_id);
|]

down_m20261010_invite_member_via_group :: Query
down_m20261010_invite_member_via_group =
  [sql|
DROP INDEX idx_group_members_invited_via_group_member_id;
ALTER TABLE group_members DROP COLUMN invited_via_group_member_id;
|]
