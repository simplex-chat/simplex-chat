{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260603_simplex_name where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- contacts.simplex_name and groups.simplex_name are the source of truth for the
-- entity's current name (updated when XInfo/XGrpInfo arrives).
--
-- connections.simplex_name is a TRANSIENT carrier for the connect-via-plan
-- (connect-by-name) path: when the user initiates a connection by typing
-- #name.simplex, the peer's profile is not yet available, so the name is
-- stashed on the connection row. When XInfo arrives and the Contact row is
-- created, the XInfo handler in Library/Subscriber.hs reads
-- connections.simplex_name and passes it to createDirectContact. After contact
-- creation, contacts.simplex_name is canonical and the connection's value
-- becomes a historical snapshot - it is intentionally never UPDATEd.
--
-- contact_profiles.simplex_name and group_profiles.simplex_name hold the peer's
-- broadcast claim (received via XInfo/XGrpInfo).
--
-- contacts.simplex_name_verified_at and groups.simplex_name_verified_at record
-- when the user last verified (via RSLV) that the peer's claimed simplex_name
-- resolves to the link stored locally. NULL means unverified; it is cleared
-- back to NULL whenever the claim changes (updateContactProfile / updateGroupProfile).
--
-- server_operators.smp_role_names enables name resolution for an operator's SMP
-- servers (set for the simplex operator).
m20260603_simplex_name :: Query
m20260603_simplex_name =
  [sql|
ALTER TABLE contacts ADD COLUMN simplex_name TEXT;
ALTER TABLE groups ADD COLUMN simplex_name TEXT;
ALTER TABLE connections ADD COLUMN simplex_name TEXT;

CREATE UNIQUE INDEX idx_contacts_simplex_name
  ON contacts(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL AND deleted = 0;

CREATE UNIQUE INDEX idx_groups_simplex_name
  ON groups(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;

ALTER TABLE contact_profiles ADD COLUMN simplex_name TEXT;
ALTER TABLE group_profiles ADD COLUMN simplex_name TEXT;

CREATE UNIQUE INDEX idx_contact_profiles_simplex_name
  ON contact_profiles(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;

CREATE UNIQUE INDEX idx_group_profiles_simplex_name
  ON group_profiles(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;

ALTER TABLE contacts ADD COLUMN simplex_name_verified_at TEXT;
ALTER TABLE groups ADD COLUMN simplex_name_verified_at TEXT;

ALTER TABLE server_operators ADD COLUMN smp_role_names INTEGER NOT NULL DEFAULT 0;
UPDATE server_operators SET smp_role_names = 1 WHERE server_operator_tag = 'simplex';
|]

down_m20260603_simplex_name :: Query
down_m20260603_simplex_name =
  [sql|
ALTER TABLE server_operators DROP COLUMN smp_role_names;

ALTER TABLE groups DROP COLUMN simplex_name_verified_at;
ALTER TABLE contacts DROP COLUMN simplex_name_verified_at;

DROP INDEX idx_group_profiles_simplex_name;
DROP INDEX idx_contact_profiles_simplex_name;
ALTER TABLE group_profiles DROP COLUMN simplex_name;
ALTER TABLE contact_profiles DROP COLUMN simplex_name;

DROP INDEX idx_groups_simplex_name;
DROP INDEX idx_contacts_simplex_name;
ALTER TABLE connections DROP COLUMN simplex_name;
ALTER TABLE groups DROP COLUMN simplex_name;
ALTER TABLE contacts DROP COLUMN simplex_name;
|]
