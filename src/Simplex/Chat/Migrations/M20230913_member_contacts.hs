{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230913_member_contacts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230913_member_contacts :: Query
m20230913_member_contacts =
  [sql|
ALTER TABLE contacts ADD COLUMN pending_contact_member_id INTEGER;
ALTER TABLE contacts ADD COLUMN to_join INTEGER;
|]

down_m20230913_member_contacts :: Query
down_m20230913_member_contacts =
  [sql|
ALTER TABLE contacts DROP COLUMN to_join;
ALTER TABLE contacts DROP COLUMN pending_contact_member_id;
|]
