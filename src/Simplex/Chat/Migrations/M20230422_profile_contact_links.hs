{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230422_profile_contact_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230422_profile_contact_links :: Query
m20230422_profile_contact_links =
  [sql|
ALTER TABLE group_profiles ADD COLUMN group_link BLOB;
|]

down_m20230422_profile_contact_links :: Query
down_m20230422_profile_contact_links =
  [sql|
ALTER TABLE group_profiles DROP COLUMN group_link;
|]
