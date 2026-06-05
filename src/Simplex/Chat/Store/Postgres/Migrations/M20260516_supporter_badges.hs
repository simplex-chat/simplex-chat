{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260516_supporter_badges where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260516_supporter_badges :: Text
m20260516_supporter_badges =
  [r|
ALTER TABLE contact_profiles ADD COLUMN badge_proof BYTEA;
ALTER TABLE contact_profiles ADD COLUMN badge_pres_header BYTEA;
ALTER TABLE contact_profiles ADD COLUMN badge_expiry TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_type TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_verified SMALLINT NOT NULL DEFAULT 0;
|]

down_m20260516_supporter_badges :: Text
down_m20260516_supporter_badges =
  [r|
ALTER TABLE contact_profiles DROP COLUMN badge_verified;
ALTER TABLE contact_profiles DROP COLUMN badge_type;
ALTER TABLE contact_profiles DROP COLUMN badge_proof;
ALTER TABLE contact_profiles DROP COLUMN badge_pres_header;
ALTER TABLE contact_profiles DROP COLUMN badge_expiry;
|]
