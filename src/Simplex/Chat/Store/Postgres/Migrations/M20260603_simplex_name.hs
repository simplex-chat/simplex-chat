{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260603_simplex_name where

import Data.Text (Text)
import Text.RawString.QQ (r)

-- The entity name is stored on the profile as the canonical strEncode form (TEXT):
-- contact_profiles.simplex_name for contacts and group_profiles.simplex_name for
-- channels (copied from group_profiles.group_domain, added in M20260515, which is
-- then dropped).
--
-- contact_profiles.simplex_name_verification and groups.simplex_name_verification
-- are the local 3-state verification status (NULL = not attempted, 0 = failed,
-- 1 = verified), reset to NULL when the claimed name changes.
--
-- contact_profiles.simplex_name_proof / group_profiles.simplex_name_proof hold the
-- peer's name NameClaimProof (JSON).
-- user_contact_links.link_priv_sig_key is the contact-address owner signing key
-- (captured at short-link creation) used to sign the user's own name proofs.
--
-- server_operators.smp_role_names enables name resolution for an operator's SMP
-- servers (set for the simplex operator).
m20260603_simplex_name :: Text
m20260603_simplex_name =
  [r|
ALTER TABLE contact_profiles ADD COLUMN simplex_name TEXT;
ALTER TABLE contact_profiles ADD COLUMN simplex_name_verification SMALLINT;
ALTER TABLE contact_profiles ADD COLUMN simplex_name_proof TEXT;
ALTER TABLE groups ADD COLUMN simplex_name_verification SMALLINT;
ALTER TABLE group_profiles ADD COLUMN simplex_name TEXT;
UPDATE group_profiles SET simplex_name = group_domain;
ALTER TABLE group_profiles DROP COLUMN group_domain;
ALTER TABLE group_profiles ADD COLUMN simplex_name_proof TEXT;
ALTER TABLE user_contact_links ADD COLUMN link_priv_sig_key BYTEA;

ALTER TABLE server_operators ADD COLUMN smp_role_names SMALLINT NOT NULL DEFAULT 0;
UPDATE server_operators SET smp_role_names = 1 WHERE server_operator_tag = 'simplex';
|]

down_m20260603_simplex_name :: Text
down_m20260603_simplex_name =
  [r|
ALTER TABLE server_operators DROP COLUMN smp_role_names;

ALTER TABLE user_contact_links DROP COLUMN link_priv_sig_key;
ALTER TABLE group_profiles DROP COLUMN simplex_name_proof;
ALTER TABLE group_profiles ADD COLUMN group_domain TEXT;
UPDATE group_profiles SET group_domain = simplex_name;
ALTER TABLE group_profiles DROP COLUMN simplex_name;
ALTER TABLE groups DROP COLUMN simplex_name_verification;
ALTER TABLE contact_profiles DROP COLUMN simplex_name_proof;
ALTER TABLE contact_profiles DROP COLUMN simplex_name_verification;
ALTER TABLE contact_profiles DROP COLUMN simplex_name;
|]
