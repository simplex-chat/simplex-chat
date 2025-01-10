{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230303_group_link_role where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230303_group_link_role :: Query
m20230303_group_link_role =
  [sql|
ALTER TABLE user_contact_links ADD COLUMN group_link_member_role TEXT NULL; -- member or observer
|]
