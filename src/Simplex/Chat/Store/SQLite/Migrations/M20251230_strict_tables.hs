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

PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = CASE
  WHEN LOWER(SUBSTR(sql, -15)) = ') without rowid' THEN sql || ', STRICT'
  WHEN SUBSTR(sql, -1) = ')' THEN sql || ' STRICT'
  ELSE sql
END
WHERE type = 'table' AND name != 'sqlite_sequence';

UPDATE sqlite_master
SET sql = replace(sql, 'call_state BLOB NOT NULL', 'call_state TEXT NOT NULL')
WHERE type = 'table' AND name = 'calls';

PRAGMA writable_schema=0;
|]

down_m20251230_strict_tables :: Query
down_m20251230_strict_tables =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = CASE
  WHEN LOWER(SUBSTR(sql, -8)) = ', strict' THEN SUBSTR(sql, 1, LENGTH(sql) - 8)
  WHEN LOWER(SUBSTR(sql, -7)) = ' strict' THEN SUBSTR(sql, 1, LENGTH(sql) - 7)
  ELSE sql
END
WHERE type = 'table' AND name != 'sqlite_sequence';

UPDATE sqlite_master
SET sql = replace(sql, 'call_state TEXT NOT NULL', 'call_state BLOB NOT NULL')
WHERE type = 'table' AND name = 'calls';

PRAGMA writable_schema=0;

UPDATE group_members
SET member_role = CAST(member_role as BLOB),
    member_restriction = CAST(member_restriction as BLOB);

UPDATE user_contact_links SET group_link_member_role = CAST(group_link_member_role as BLOB);

UPDATE app_settings SET app_settings = CAST(app_settings as BLOB);
|]
