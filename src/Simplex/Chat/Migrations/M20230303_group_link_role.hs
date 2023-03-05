{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230303_group_link_role where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230303_group_link_role :: Query
m20230303_group_link_role =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE user_contact_links ADD COLUMN group_link_member_role TEXT CHECK (group_link_member_role NOT NULL); -- member or observer
UPDATE user_contact_links SET group_link_member_role = 'member';

PRAGMA ignore_check_constraints=OFF;
|]
