{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260925_badge_store_receipts where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260925_badge_store_receipts :: Text
m20260925_badge_store_receipts =
  [r|
CREATE TABLE badge_store_receipts(
  badge_store_receipt_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  provider TEXT NOT NULL,
  transaction_ref TEXT NOT NULL,
  purchase_key BYTEA NOT NULL,
  purchase_priv_key BYTEA NOT NULL,
  master_key BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE(user_id, provider, transaction_ref)
);

CREATE INDEX idx_badge_store_receipts_user ON badge_store_receipts(user_id);

ALTER TABLE badge_purchases ADD COLUMN badge_store_receipt_id BIGINT REFERENCES badge_store_receipts;

CREATE UNIQUE INDEX idx_badge_purchases_store_receipt ON badge_purchases(badge_store_receipt_id);
|]

down_m20260925_badge_store_receipts :: Text
down_m20260925_badge_store_receipts =
  [r|
DROP INDEX idx_badge_purchases_store_receipt;

ALTER TABLE badge_purchases DROP COLUMN badge_store_receipt_id;

DROP INDEX idx_badge_store_receipts_user;

DROP TABLE badge_store_receipts;
|]
