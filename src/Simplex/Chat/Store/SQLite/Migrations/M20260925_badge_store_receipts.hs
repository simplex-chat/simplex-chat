{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260925_badge_store_receipts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- | invoice_id is null for a receipt naming one another row holds: the transaction still has to be
-- creditable, and its reference identifies it. provider and transaction_ref are null until a receipt
-- arrives, and distinct NULLs let several rows await one at once. payment is held from the receipt's
-- arrival until the service credits or refuses it; a refused row keeps its refusal in credit_error.
m20260925_badge_store_receipts :: Query
m20260925_badge_store_receipts =
  [sql|
CREATE TABLE badge_store_receipts(
  badge_store_receipt_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  invoice_id TEXT UNIQUE,
  provider TEXT,
  transaction_ref TEXT,
  purchase_key BLOB NOT NULL,
  purchase_priv_key BLOB NOT NULL,
  master_key BLOB NOT NULL,
  created_at TEXT NOT NULL,
  payment TEXT,
  next_attempt_at TEXT,
  retry_delay INTEGER,
  credit_error TEXT,
  UNIQUE(provider, transaction_ref)
) STRICT;

CREATE INDEX idx_badge_store_receipts_user ON badge_store_receipts(user_id);

ALTER TABLE badge_purchases ADD COLUMN badge_store_receipt_id INTEGER REFERENCES badge_store_receipts;

CREATE UNIQUE INDEX idx_badge_purchases_store_receipt ON badge_purchases(badge_store_receipt_id);
|]

down_m20260925_badge_store_receipts :: Query
down_m20260925_badge_store_receipts =
  [sql|
DROP INDEX idx_badge_purchases_store_receipt;

ALTER TABLE badge_purchases DROP COLUMN badge_store_receipt_id;

DROP INDEX idx_badge_store_receipts_user;

DROP TABLE badge_store_receipts;
|]
