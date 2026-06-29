{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260603_simplex_name where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260603_simplex_name :: Query
m20260603_simplex_name =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN simplex_name TEXT;
ALTER TABLE contact_profiles ADD COLUMN simplex_name_verification INTEGER;
ALTER TABLE contact_profiles ADD COLUMN simplex_name_proof TEXT;
ALTER TABLE groups ADD COLUMN simplex_name_verification INTEGER;
ALTER TABLE group_profiles ADD COLUMN simplex_name TEXT;
ALTER TABLE group_profiles DROP COLUMN group_domain;
ALTER TABLE group_profiles ADD COLUMN simplex_name_proof TEXT;
ALTER TABLE user_contact_links ADD COLUMN link_priv_sig_key BLOB;

ALTER TABLE server_operators ADD COLUMN smp_role_names INTEGER NOT NULL DEFAULT 0;
UPDATE server_operators SET smp_role_names = 1 WHERE server_operator_tag = 'simplex';
|]

down_m20260603_simplex_name :: Query
down_m20260603_simplex_name =
  [sql|
ALTER TABLE server_operators DROP COLUMN smp_role_names;

ALTER TABLE user_contact_links DROP COLUMN link_priv_sig_key;
ALTER TABLE group_profiles DROP COLUMN simplex_name_proof;
ALTER TABLE group_profiles ADD COLUMN group_domain TEXT;
ALTER TABLE group_profiles DROP COLUMN simplex_name;
ALTER TABLE groups DROP COLUMN simplex_name_verification;
ALTER TABLE contact_profiles DROP COLUMN simplex_name_proof;
ALTER TABLE contact_profiles DROP COLUMN simplex_name_verification;
ALTER TABLE contact_profiles DROP COLUMN simplex_name;
|]
