{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260925_badge_store_receipts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260925_badge_store_receipts :: Query
m20260925_badge_store_receipts =
  [sql|
CREATE TABLE badge_store_receipts(
  badge_store_receipt_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  provider TEXT NOT NULL,
  transaction_ref TEXT NOT NULL,
  purchase_key BLOB NOT NULL,
  purchase_priv_key BLOB NOT NULL,
  master_key BLOB NOT NULL,
  created_at TEXT NOT NULL,
  UNIQUE(user_id, provider, transaction_ref)
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
