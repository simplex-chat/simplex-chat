{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251230_strict_tables where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20251230_strict_tables :: Query
m20251230_strict_tables =
  [sql|
UPDATE group_members
SET member_role = CAST(member_role as TEXT),
    member_restriction = CAST(member_restriction as TEXT);

UPDATE user_contact_links SET group_link_member_role = CAST(group_link_member_role as TEXT);

UPDATE app_settings SET app_settings = CAST(app_settings as TEXT);
|]

down_m20251230_strict_tables :: Query
down_m20251230_strict_tables =
  [sql|
UPDATE group_members
SET member_role = CAST(member_role as BLOB),
    member_restriction = CAST(member_restriction as BLOB);

UPDATE user_contact_links SET group_link_member_role = CAST(group_link_member_role as BLOB);

UPDATE app_settings SET app_settings = CAST(app_settings as BLOB);
|]
