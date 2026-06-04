{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260516_supporter_badges where

import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

m20260516_supporter_badges :: Query
m20260516_supporter_badges =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN badge_proof BYTEA;
ALTER TABLE contact_profiles ADD COLUMN badge_pres_header BYTEA;
ALTER TABLE contact_profiles ADD COLUMN badge_expiry TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_type TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_verified BOOLEAN;
|]

down_m20260516_supporter_badges :: Query
down_m20260516_supporter_badges =
  [sql|
ALTER TABLE contact_profiles DROP COLUMN badge_verified;
ALTER TABLE contact_profiles DROP COLUMN badge_type;
ALTER TABLE contact_profiles DROP COLUMN badge_proof;
ALTER TABLE contact_profiles DROP COLUMN badge_pres_header;
ALTER TABLE contact_profiles DROP COLUMN badge_expiry;
|]
