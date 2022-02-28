{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220228_avatars where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220228_avatars :: Query
m20220228_avatars =
    [sql|
    ALTER TABLE contact_profiles ADD COLUMN image BLOB;
    ALTER TABLE group_profiles ADD COLUMN image BLOB;
|]
