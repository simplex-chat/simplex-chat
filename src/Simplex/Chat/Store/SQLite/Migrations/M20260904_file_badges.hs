{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260904_file_badges where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260904_file_badges :: Query
m20260904_file_badges =
  [sql|
ALTER TABLE files ADD COLUMN file_max_size INTEGER;
ALTER TABLE files ADD COLUMN file_badge_status TEXT;

CREATE TABLE rcv_badge_proofs(
  badge_proof_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER NOT NULL REFERENCES files ON DELETE CASCADE,
  badge_proof BLOB NOT NULL,
  badge_pres_header BLOB NOT NULL,
  badge_key_idx INTEGER NOT NULL,
  badge_type TEXT NOT NULL,
  badge_expiry TEXT NOT NULL,
  badge_extra TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE INDEX idx_rcv_badge_proofs_file_id ON rcv_badge_proofs(file_id);

ALTER TABLE rcv_files ADD COLUMN badge_inv_proof_id INTEGER REFERENCES rcv_badge_proofs ON DELETE SET NULL;
ALTER TABLE rcv_files ADD COLUMN badge_descr_proof_id INTEGER REFERENCES rcv_badge_proofs ON DELETE SET NULL;

CREATE INDEX idx_rcv_files_badge_inv_proof_id ON rcv_files(badge_inv_proof_id);
CREATE INDEX idx_rcv_files_badge_descr_proof_id ON rcv_files(badge_descr_proof_id);
|]

down_m20260904_file_badges :: Query
down_m20260904_file_badges =
  [sql|
DROP INDEX idx_rcv_files_badge_descr_proof_id;
DROP INDEX idx_rcv_files_badge_inv_proof_id;

ALTER TABLE rcv_files DROP COLUMN badge_descr_proof_id;
ALTER TABLE rcv_files DROP COLUMN badge_inv_proof_id;

DROP INDEX idx_rcv_badge_proofs_file_id;
DROP TABLE rcv_badge_proofs;

ALTER TABLE files DROP COLUMN file_badge_status;
ALTER TABLE files DROP COLUMN file_max_size;
|]
