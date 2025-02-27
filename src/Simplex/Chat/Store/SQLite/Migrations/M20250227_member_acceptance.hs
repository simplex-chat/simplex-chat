{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250227_member_acceptance where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250227_member_acceptance :: Query
m20250227_member_acceptance =
  [sql|
ALTER TABLE user_contact_links ADD COLUMN group_link_auto_accept TEXT NULL;
|]

down_m20250227_member_acceptance :: Query
down_m20250227_member_acceptance =
  [sql|
ALTER TABLE user_contact_links DROP COLUMN group_link_auto_accept;
|]
