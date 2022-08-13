{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220812_incognito_profiles where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220812_incognito_profiles :: Query
m20220812_incognito_profiles =
  [sql|
ALTER TABLE connections ADD COLUMN incognito_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL; -- only set for direct connections

ALTER TABLE contact_profiles ADD COLUMN incognito INTEGER; -- 1 for incognito
|]
