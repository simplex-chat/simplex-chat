{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260925_badge_bindings where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260925_badge_bindings :: Text
m20260925_badge_bindings =
  [r|
ALTER TABLE connections ADD COLUMN pres_header BYTEA;

CREATE TABLE group_member_badge_proofs(
  group_member_id BIGINT PRIMARY KEY REFERENCES group_members ON DELETE CASCADE,
  badge_proof BYTEA NOT NULL,
  badge_pres_header BYTEA NOT NULL,
  badge_key_idx BIGINT NOT NULL,
  badge_type TEXT NOT NULL,
  badge_expiry TIMESTAMPTZ NOT NULL,
  badge_extra TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);
|]

down_m20260925_badge_bindings :: Text
down_m20260925_badge_bindings =
  [r|
DROP TABLE group_member_badge_proofs;

ALTER TABLE connections DROP COLUMN pres_header;
|]
