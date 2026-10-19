{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220824_profiles_local_alias where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220824_profiles_local_alias :: Query
m20220824_profiles_local_alias =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE contact_profiles ADD COLUMN local_alias TEXT DEFAULT '' CHECK (local_alias NOT NULL);
UPDATE contact_profiles SET local_alias = '';

PRAGMA ignore_check_constraints=OFF;
|]
