{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260904_file_badges where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260904_file_badges :: Text
m20260904_file_badges =
  [r|
ALTER TABLE files ADD COLUMN file_max_size BIGINT;
ALTER TABLE files ADD COLUMN file_badge_status TEXT;

CREATE TABLE rcv_badge_proofs(
  badge_proof_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  file_id BIGINT NOT NULL REFERENCES files ON DELETE CASCADE,
  badge_proof BYTEA NOT NULL,
  badge_pres_header BYTEA NOT NULL,
  badge_key_idx BIGINT NOT NULL,
  badge_type TEXT NOT NULL,
  badge_expiry TIMESTAMPTZ NOT NULL,
  badge_extra TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX idx_rcv_badge_proofs_file_id ON rcv_badge_proofs(file_id);

ALTER TABLE rcv_files ADD COLUMN badge_inv_proof_id BIGINT REFERENCES rcv_badge_proofs ON DELETE SET NULL;
ALTER TABLE rcv_files ADD COLUMN badge_descr_proof_id BIGINT REFERENCES rcv_badge_proofs ON DELETE SET NULL;

CREATE INDEX idx_rcv_files_badge_inv_proof_id ON rcv_files(badge_inv_proof_id);
CREATE INDEX idx_rcv_files_badge_descr_proof_id ON rcv_files(badge_descr_proof_id);
|]

down_m20260904_file_badges :: Text
down_m20260904_file_badges =
  [r|
DROP INDEX idx_rcv_files_badge_descr_proof_id;
DROP INDEX idx_rcv_files_badge_inv_proof_id;

ALTER TABLE rcv_files DROP COLUMN badge_descr_proof_id;
ALTER TABLE rcv_files DROP COLUMN badge_inv_proof_id;

DROP INDEX idx_rcv_badge_proofs_file_id;
DROP TABLE rcv_badge_proofs;

ALTER TABLE files DROP COLUMN file_badge_status;
ALTER TABLE files DROP COLUMN file_max_size;
|]
