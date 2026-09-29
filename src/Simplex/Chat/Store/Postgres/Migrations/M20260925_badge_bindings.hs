{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260925_badge_bindings where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260925_badge_bindings :: Text
m20260925_badge_bindings =
  [r|
ALTER TABLE connections ADD COLUMN pres_header BYTEA;

ALTER TABLE file_badge_proofs ALTER COLUMN file_id DROP NOT NULL;

ALTER TABLE file_badge_proofs ADD COLUMN group_member_id BIGINT REFERENCES group_members ON DELETE CASCADE;

CREATE UNIQUE INDEX idx_file_badge_proofs_group_member_id ON file_badge_proofs(group_member_id);
|]

down_m20260925_badge_bindings :: Text
down_m20260925_badge_bindings =
  [r|
DROP INDEX idx_file_badge_proofs_group_member_id;

DELETE FROM file_badge_proofs WHERE group_member_id IS NOT NULL;

ALTER TABLE file_badge_proofs DROP COLUMN group_member_id;

ALTER TABLE file_badge_proofs ALTER COLUMN file_id SET NOT NULL;

ALTER TABLE connections DROP COLUMN pres_header;
|]
