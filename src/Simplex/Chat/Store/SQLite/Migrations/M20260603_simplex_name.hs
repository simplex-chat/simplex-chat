{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260603_simplex_name where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- The entity name lives only on the profile: contact_profiles.contact_domain holds
-- the peer's broadcast contact name, and the existing group_profiles.group_domain
-- (from M20260515) holds the channel name. Both store the canonical strEncode form
-- as TEXT.
--
-- contact_profiles.contact_domain_verification and groups.group_domain_verification
-- are the local 3-state verification status (NULL = not attempted, 0 = failed,
-- 1 = verified), reset to NULL when the claimed name changes.
--
-- contact_profiles.contact_domain_proof holds the peer's name NameClaimProof (JSON).
-- user_contact_links.link_priv_sig_key is the contact-address owner signing key
-- (captured at short-link creation) used to sign the user's own name proofs.
--
-- server_operators.smp_role_names enables name resolution for an operator's SMP
-- servers (set for the simplex operator).
m20260603_simplex_name :: Query
m20260603_simplex_name =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN contact_domain TEXT;
ALTER TABLE contact_profiles ADD COLUMN contact_domain_verification INTEGER;
ALTER TABLE contact_profiles ADD COLUMN contact_domain_proof TEXT;
ALTER TABLE groups ADD COLUMN group_domain_verification INTEGER;
ALTER TABLE group_profiles ADD COLUMN group_domain_proof TEXT;
ALTER TABLE user_contact_links ADD COLUMN link_priv_sig_key BLOB;

ALTER TABLE server_operators ADD COLUMN smp_role_names INTEGER NOT NULL DEFAULT 0;
UPDATE server_operators SET smp_role_names = 1 WHERE server_operator_tag = 'simplex';
|]

down_m20260603_simplex_name :: Query
down_m20260603_simplex_name =
  [sql|
ALTER TABLE server_operators DROP COLUMN smp_role_names;

ALTER TABLE user_contact_links DROP COLUMN link_priv_sig_key;
ALTER TABLE group_profiles DROP COLUMN group_domain_proof;
ALTER TABLE groups DROP COLUMN group_domain_verification;
ALTER TABLE contact_profiles DROP COLUMN contact_domain_proof;
ALTER TABLE contact_profiles DROP COLUMN contact_domain_verification;
ALTER TABLE contact_profiles DROP COLUMN contact_domain;
|]
