{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250512_member_admission where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250512_member_admission :: Query
m20250512_member_admission =
  [sql|
ALTER TABLE group_profiles ADD COLUMN member_admission TEXT;
|]

down_m20250512_member_admission :: Query
down_m20250512_member_admission =
  [sql|
ALTER TABLE group_profiles DROP COLUMN member_admission;
|]
