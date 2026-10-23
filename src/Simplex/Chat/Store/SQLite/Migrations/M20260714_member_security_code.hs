{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260714_member_security_code where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260714_member_security_code :: Query
m20260714_member_security_code =
  [sql|
ALTER TABLE group_members ADD COLUMN member_security_code TEXT;
ALTER TABLE group_members ADD COLUMN member_security_code_verified_at TEXT;
|]

down_m20260714_member_security_code :: Query
down_m20260714_member_security_code =
  [sql|
ALTER TABLE group_members DROP COLUMN member_security_code;
ALTER TABLE group_members DROP COLUMN member_security_code_verified_at;
|]
