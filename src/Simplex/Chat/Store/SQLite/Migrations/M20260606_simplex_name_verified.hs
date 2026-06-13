{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260606_simplex_name_verified where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- contacts.simplex_name_verified_at and groups.simplex_name_verified_at record
-- the timestamp when the user last verified that the peer's claimed simplex_name
-- resolves (via RSLV) to the link stored locally for the contact/group.
-- NULL means the claim is unverified and the UI should show an indicator.
-- The column is cleared back to NULL whenever the simplex_name claim changes
-- (updateContactProfile / updateGroupProfile).
m20260606_simplex_name_verified :: Query
m20260606_simplex_name_verified =
  [sql|
ALTER TABLE contacts ADD COLUMN simplex_name_verified_at TEXT;
ALTER TABLE groups ADD COLUMN simplex_name_verified_at TEXT;
|]

down_m20260606_simplex_name_verified :: Query
down_m20260606_simplex_name_verified =
  [sql|
ALTER TABLE groups DROP COLUMN simplex_name_verified_at;
ALTER TABLE contacts DROP COLUMN simplex_name_verified_at;
|]
