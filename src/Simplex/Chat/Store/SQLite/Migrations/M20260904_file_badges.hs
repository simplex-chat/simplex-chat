{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260904_file_badges where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260904_file_badges :: Query
m20260904_file_badges =
  [sql|
ALTER TABLE files ADD COLUMN file_max_size INTEGER;
ALTER TABLE files ADD COLUMN file_badge_status TEXT;

CREATE TABLE file_badge_proofs(
  badge_proof_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER NOT NULL REFERENCES files ON DELETE CASCADE,
  proof_kind TEXT NOT NULL,
  badge_proof BLOB NOT NULL,
  badge_pres_header BLOB NOT NULL,
  badge_key_idx INTEGER NOT NULL,
  badge_type TEXT NOT NULL,
  badge_expiry TEXT NOT NULL,
  badge_extra TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE UNIQUE INDEX idx_file_badge_proofs_file_id_kind ON file_badge_proofs(file_id, proof_kind);
|]

down_m20260904_file_badges :: Query
down_m20260904_file_badges =
  [sql|
DROP INDEX idx_file_badge_proofs_file_id_kind;
DROP TABLE file_badge_proofs;

ALTER TABLE files DROP COLUMN file_badge_status;
ALTER TABLE files DROP COLUMN file_max_size;
|]
