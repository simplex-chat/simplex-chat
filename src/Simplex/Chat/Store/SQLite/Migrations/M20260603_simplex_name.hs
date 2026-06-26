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
-- server_operators.smp_role_names enables name resolution for an operator's SMP
-- servers (set for the simplex operator).
m20260603_simplex_name :: Query
m20260603_simplex_name =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN contact_domain TEXT;
ALTER TABLE contact_profiles ADD COLUMN contact_domain_verification INTEGER;
ALTER TABLE groups ADD COLUMN group_domain_verification INTEGER;

ALTER TABLE server_operators ADD COLUMN smp_role_names INTEGER NOT NULL DEFAULT 0;
UPDATE server_operators SET smp_role_names = 1 WHERE server_operator_tag = 'simplex';
|]

down_m20260603_simplex_name :: Query
down_m20260603_simplex_name =
  [sql|
ALTER TABLE server_operators DROP COLUMN smp_role_names;

ALTER TABLE groups DROP COLUMN group_domain_verification;
ALTER TABLE contact_profiles DROP COLUMN contact_domain_verification;
ALTER TABLE contact_profiles DROP COLUMN contact_domain;
|]
