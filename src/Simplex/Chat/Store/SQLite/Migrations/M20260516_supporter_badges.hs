{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260516_supporter_badges where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260516_supporter_badges :: Query
m20260516_supporter_badges =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN badge_proof BLOB;
ALTER TABLE contact_profiles ADD COLUMN badge_pres_header BLOB;
ALTER TABLE contact_profiles ADD COLUMN badge_expiry TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_type TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_verified INTEGER NOT NULL DEFAULT 0;
ALTER TABLE contact_profiles ADD COLUMN badge_extra TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_master_key BLOB;
ALTER TABLE contact_profiles ADD COLUMN badge_signature BLOB;
|]

down_m20260516_supporter_badges :: Query
down_m20260516_supporter_badges =
  [sql|
ALTER TABLE contact_profiles DROP COLUMN badge_signature;
ALTER TABLE contact_profiles DROP COLUMN badge_master_key;
ALTER TABLE contact_profiles DROP COLUMN badge_extra;
ALTER TABLE contact_profiles DROP COLUMN badge_verified;
ALTER TABLE contact_profiles DROP COLUMN badge_type;
ALTER TABLE contact_profiles DROP COLUMN badge_expiry;
ALTER TABLE contact_profiles DROP COLUMN badge_proof;
ALTER TABLE contact_profiles DROP COLUMN badge_pres_header;
|]
