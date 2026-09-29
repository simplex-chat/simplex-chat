{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260924_simplex_name_resolved where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260924_simplex_name_resolved :: Text
m20260924_simplex_name_resolved =
  [r|
ALTER TABLE contact_profiles ADD COLUMN contact_domain_resolved_at TIMESTAMPTZ;
ALTER TABLE contact_profiles ADD COLUMN contact_domain_expires_at TIMESTAMPTZ;

ALTER TABLE groups ADD COLUMN group_domain_resolved_at TIMESTAMPTZ;
ALTER TABLE groups ADD COLUMN group_domain_expires_at TIMESTAMPTZ;
|]

down_m20260924_simplex_name_resolved :: Text
down_m20260924_simplex_name_resolved =
  [r|
ALTER TABLE contact_profiles DROP COLUMN contact_domain_resolved_at;
ALTER TABLE contact_profiles DROP COLUMN contact_domain_expires_at;

ALTER TABLE groups DROP COLUMN group_domain_resolved_at;
ALTER TABLE groups DROP COLUMN group_domain_expires_at;
|]
