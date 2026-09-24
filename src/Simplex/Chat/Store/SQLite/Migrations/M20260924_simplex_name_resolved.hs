{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260924_simplex_name_resolved where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260924_simplex_name_resolved :: Query
m20260924_simplex_name_resolved =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN contact_domain_resolved_at TEXT;
ALTER TABLE contact_profiles ADD COLUMN contact_domain_expires_at TEXT;

ALTER TABLE groups ADD COLUMN group_domain_resolved_at TEXT;
ALTER TABLE groups ADD COLUMN group_domain_expires_at TEXT;
|]

down_m20260924_simplex_name_resolved :: Query
down_m20260924_simplex_name_resolved =
  [sql|
ALTER TABLE contact_profiles DROP COLUMN contact_domain_resolved_at;
ALTER TABLE contact_profiles DROP COLUMN contact_domain_expires_at;

ALTER TABLE groups DROP COLUMN group_domain_resolved_at;
ALTER TABLE groups DROP COLUMN group_domain_expires_at;
|]
