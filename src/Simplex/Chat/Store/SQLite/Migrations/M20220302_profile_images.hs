{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220302_profile_images where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220302_profile_images :: Query
m20220302_profile_images =
  [sql|
    ALTER TABLE contact_profiles ADD COLUMN image TEXT;
    ALTER TABLE group_profiles ADD COLUMN image TEXT;
|]
