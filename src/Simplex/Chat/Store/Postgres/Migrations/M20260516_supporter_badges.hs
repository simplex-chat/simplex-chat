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
ALTER TABLE contact_profiles ADD COLUMN badge_expiry TIMESTAMPTZ;
ALTER TABLE contact_profiles ADD COLUMN badge_type TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_verified SMALLINT;
ALTER TABLE contact_profiles ADD COLUMN badge_extra TEXT;
ALTER TABLE contact_profiles ADD COLUMN badge_master_key BYTEA;
ALTER TABLE contact_profiles ADD COLUMN badge_signature BYTEA;
ALTER TABLE contact_profiles ADD COLUMN badge_key_idx BIGINT;
|]

down_m20260516_supporter_badges :: Text
down_m20260516_supporter_badges =
  [r|
ALTER TABLE contact_profiles DROP COLUMN badge_key_idx;
ALTER TABLE contact_profiles DROP COLUMN badge_signature;
ALTER TABLE contact_profiles DROP COLUMN badge_master_key;
ALTER TABLE contact_profiles DROP COLUMN badge_extra;
ALTER TABLE contact_profiles DROP COLUMN badge_verified;
ALTER TABLE contact_profiles DROP COLUMN badge_type;
ALTER TABLE contact_profiles DROP COLUMN badge_proof;
ALTER TABLE contact_profiles DROP COLUMN badge_pres_header;
ALTER TABLE contact_profiles DROP COLUMN badge_expiry;
|]
