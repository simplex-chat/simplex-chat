{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260925_badge_store_receipts where

import Data.Text (Text)
import Text.RawString.QQ (r)

-- | invoice_id is null for a receipt naming one another row holds: the transaction still has to be
-- creditable, and its reference identifies it. provider and transaction_ref are null until a receipt
-- arrives, and distinct NULLs let several rows await one at once. payment is held from the receipt's
-- arrival until the service credits or refuses it; a refused row keeps its refusal in credit_error.
m20260925_badge_store_receipts :: Text
m20260925_badge_store_receipts =
  [r|
CREATE TABLE badge_store_receipts(
  badge_store_receipt_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  invoice_id TEXT UNIQUE,
  provider TEXT,
  transaction_ref TEXT,
  purchase_key BYTEA NOT NULL,
  purchase_priv_key BYTEA NOT NULL,
  master_key BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  payment TEXT,
  next_attempt_at TIMESTAMPTZ,
  retry_delay BIGINT,
  retry_count BIGINT NOT NULL DEFAULT 0,
  credit_error TEXT,
  UNIQUE(provider, transaction_ref)
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
