{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260731_user_badges where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260731_user_badges :: Query
m20260731_user_badges =
  [sql|
CREATE TABLE badge_prices(
  price_id TEXT NOT NULL PRIMARY KEY,
  badge_type TEXT NOT NULL,
  month_price INTEGER NOT NULL,
  currency TEXT NOT NULL,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL
);

CREATE TABLE badge_offers(
  offer_id TEXT NOT NULL PRIMARY KEY,
  price_id TEXT REFERENCES badge_prices,
  months INTEGER NOT NULL,
  discount_type TEXT NOT NULL,
  free_months INTEGER,
  discount INTEGER,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL
);

CREATE TABLE payments(
  payment_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  purchase_key BLOB NOT NULL,
  badge_type TEXT NOT NULL,
  price_id TEXT REFERENCES badge_prices,
  offer_id TEXT REFERENCES badge_offers,
  invoice_uuid TEXT,
  months INTEGER,
  amount INTEGER,
  currency TEXT,
  provider TEXT NOT NULL,
  provider_ref TEXT,
  invoice_url TEXT,
  invoice_address TEXT,
  invoice_crypto_amount TEXT,
  invoice_expires_at TEXT,
  evidence BLOB,
  receipt_code TEXT,
  status TEXT NOT NULL,
  exception TEXT,
  renews_at TEXT,
  grace_until TEXT,
  cancelled INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE INDEX idx_payments_purchase_key ON payments(purchase_key);

CREATE UNIQUE INDEX idx_payments_invoice_uuid ON payments(invoice_uuid);

CREATE TABLE charges(
  charge_id INTEGER PRIMARY KEY,
  payment_id INTEGER NOT NULL REFERENCES payments,
  invoice_uuid TEXT NOT NULL,
  provider_charge_ref TEXT NOT NULL,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  amount INTEGER NOT NULL,
  currency TEXT NOT NULL,
  charged_at TEXT NOT NULL,
  UNIQUE(payment_id, provider_charge_ref)
);

CREATE TABLE badge_purchases(
  badge_purchase_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  purchase_key BLOB NOT NULL,
  purchase_priv_key BLOB NOT NULL,
  master_key BLOB NOT NULL,
  badge_type TEXT NOT NULL,
  price_id TEXT REFERENCES badge_prices,
  offer_id TEXT REFERENCES badge_offers,
  payment_id INTEGER NOT NULL REFERENCES payments,
  status TEXT NOT NULL,
  badge_key_idx INTEGER,
  badge_signature BLOB,
  badge_expiry TEXT,
  alert_acked_kind TEXT,
  alert_acked_episode TEXT,
  alert_snooze_until TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  UNIQUE(purchase_key)
);

CREATE UNIQUE INDEX idx_badge_purchases_live ON badge_purchases(user_id, (badge_type = 'investor'))
  WHERE status IN ('acquiring', 'issued');

ALTER TABLE users ADD COLUMN shown_badge_id INTEGER REFERENCES badge_purchases ON DELETE SET NULL;
|]
    <> badgeLedgerTable
    <> [sql|
ALTER TABLE badge_ledger ADD COLUMN entry_type_unknown INTEGER NOT NULL DEFAULT 0;

ALTER TABLE badge_ledger ADD COLUMN entry_type_value TEXT;

CREATE UNIQUE INDEX idx_badge_ledger_uuid ON badge_ledger(entry_uuid);

CREATE INDEX idx_badge_ledger_purchase ON badge_ledger(badge_purchase_id, entry_id);

CREATE INDEX idx_badge_ledger_invoice ON badge_ledger(invoice_id);

CREATE INDEX idx_badge_ledger_charge ON badge_ledger(charge_id);

CREATE INDEX idx_badge_ledger_from_purchase ON badge_ledger(from_purchase_id);

CREATE INDEX idx_badge_ledger_to_purchase ON badge_ledger(to_purchase_id);

CREATE TABLE badge_issuances(
  issuance_id INTEGER PRIMARY KEY,
  badge_purchase_id INTEGER NOT NULL REFERENCES badge_purchases ON DELETE CASCADE,
  period_start TEXT,
  period_end TEXT,
  expiry TEXT,
  entry_id INTEGER REFERENCES badge_ledger,
  created_at TEXT NOT NULL
);

CREATE INDEX idx_badge_issuances_purchase ON badge_issuances(badge_purchase_id, issuance_id);
|]

badgeLedgerTable :: Query
badgeLedgerTable =
  [sql|
CREATE TABLE badge_ledger(
  entry_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entry_uuid TEXT NOT NULL,
  badge_purchase_id INTEGER NOT NULL REFERENCES badge_purchases ON DELETE CASCADE,
  change_months INTEGER NOT NULL,
  balance_months INTEGER NOT NULL,
  balance_start_ts TEXT NOT NULL,
  balance_badge_type TEXT NOT NULL,
  was_paused_since TEXT,
  service_created_at TEXT NOT NULL,
  created_at TEXT NOT NULL,
  entry_type TEXT NOT NULL,
  entry_credit_type TEXT,
  entry_debit_type TEXT,
  invoice_id INTEGER REFERENCES payments,
  charge_id INTEGER REFERENCES charges,
  from_purchase_id INTEGER REFERENCES badge_purchases,
  to_purchase_id INTEGER REFERENCES badge_purchases
);
|]

down_m20260731_user_badges :: Query
down_m20260731_user_badges =
  [sql|
DROP TABLE badge_issuances;
DROP TABLE badge_ledger;
ALTER TABLE users DROP COLUMN shown_badge_id;
DROP TABLE badge_purchases;
DROP TABLE charges;
DROP TABLE payments;
DROP TABLE badge_offers;
DROP TABLE badge_prices;
|]
