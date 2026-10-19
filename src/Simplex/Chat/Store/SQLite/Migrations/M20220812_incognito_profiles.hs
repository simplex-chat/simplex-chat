{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220812_incognito_profiles where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220812_incognito_profiles :: Query
m20220812_incognito_profiles =
  [sql|
ALTER TABLE connections ADD COLUMN custom_user_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL; -- only set for direct connections

ALTER TABLE group_members ADD COLUMN member_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL; -- member profile id if incognito profile was saved for member (used when invitation is received via incognito direct connection with host)

ALTER TABLE contact_profiles ADD COLUMN incognito INTEGER; -- 1 for incognito
|]
