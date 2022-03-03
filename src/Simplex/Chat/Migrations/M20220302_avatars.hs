{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220302_avatars where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220302_avatars :: Query
m20220302_avatars =
  [sql|
    ALTER TABLE contact_profiles ADD COLUMN image TEXT;
    ALTER TABLE group_profiles ADD COLUMN image TEXT;
|]
