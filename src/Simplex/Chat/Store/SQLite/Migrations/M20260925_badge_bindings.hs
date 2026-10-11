{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260925_badge_bindings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260925_badge_bindings :: Query
m20260925_badge_bindings =
  [sql|
ALTER TABLE connections ADD COLUMN pres_header BLOB;

ALTER TABLE groups ADD COLUMN relay_request_public_group_id BLOB;

CREATE TABLE group_member_badge_proofs(
  group_member_id INTEGER PRIMARY KEY REFERENCES group_members ON DELETE CASCADE,
  badge_proof BLOB NOT NULL,
  badge_pres_header BLOB NOT NULL,
  badge_key_idx INTEGER NOT NULL,
  badge_type TEXT NOT NULL,
  badge_expiry TEXT NOT NULL,
  badge_extra TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;
|]

down_m20260925_badge_bindings :: Query
down_m20260925_badge_bindings =
  [sql|
DROP TABLE group_member_badge_proofs;

ALTER TABLE groups DROP COLUMN relay_request_public_group_id;

ALTER TABLE connections DROP COLUMN pres_header;
|]
