{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221211_group_description where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221211_group_description :: Query
m20221211_group_description =
  [sql|
ALTER TABLE group_profiles ADD COLUMN description TEXT NULL;
|]
