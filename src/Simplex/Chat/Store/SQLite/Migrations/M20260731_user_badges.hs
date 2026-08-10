{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260731_user_badges where

import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Query (..))
import Database.SQLite.Simple.QQ (sql)

-- the same tables in the client and in the badge service; '@' is replaced with the table name prefix
badgeSchema :: Text -> Query
badgeSchema pfx = withPrefix pfx badgeSchemaTables

badgeSchemaDown :: Text -> Query
badgeSchemaDown pfx = withPrefix pfx badgeSchemaTablesDown

withPrefix :: Text -> Query -> Query
withPrefix pfx = Query . T.replace "@" pfx . fromQuery

badgeSchemaTables :: Query
badgeSchemaTables =
  [sql|
CREATE TABLE @invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY,
  provider TEXT NOT NULL,
  price INTEGER NOT NULL,
  discount_amount INTEGER,
  credit_amount INTEGER,
  amount INTEGER NOT NULL,
  currency TEXT NOT NULL,
  payment_url TEXT,
  payment_address TEXT,
  payment_crypto_currency TEXT,
  payment_crypto_amount TEXT,
  expires_at TEXT NOT NULL,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE TABLE @payments(
  payment_id TEXT NOT NULL PRIMARY KEY,
  invoice_id TEXT REFERENCES @invoices,
  provider TEXT NOT NULL,
  provider_ref TEXT,
  amount INTEGER,
  currency TEXT,
  status TEXT NOT NULL,
  exception TEXT,
  subscription_renews_at TEXT,
  grace_until TEXT,
  cancelled INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE INDEX @idx_payments_provider_ref ON @payments(provider, provider_ref);

CREATE INDEX @idx_payments_invoice ON @payments(invoice_id);

CREATE TABLE @subscription_charges(
  charge_id TEXT NOT NULL PRIMARY KEY,
  payment_id TEXT NOT NULL REFERENCES @payments ON DELETE CASCADE,
  provider_charge_ref TEXT NOT NULL,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  amount INTEGER NOT NULL,
  currency TEXT NOT NULL,
  charged_at TEXT NOT NULL,
  UNIQUE(payment_id, provider_charge_ref)
);

CREATE TABLE @badge_prices(
  price_id TEXT NOT NULL PRIMARY KEY,
  badge_type TEXT NOT NULL,
  month_price INTEGER NOT NULL,
  currency TEXT NOT NULL,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL
);

CREATE TABLE @badge_offers(
  offer_id TEXT NOT NULL PRIMARY KEY,
  price_id TEXT REFERENCES @badge_prices,
  months INTEGER NOT NULL,
  free_months INTEGER,
  discount INTEGER,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL
);

CREATE TABLE @badge_purchases(
  badge_purchase_id INTEGER PRIMARY KEY AUTOINCREMENT,
  purchase_key BLOB NOT NULL,
  master_key BLOB NOT NULL,
  initial_badge_type TEXT NOT NULL,
  current_badge_type TEXT NOT NULL,
  payment_id TEXT REFERENCES @payments,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  UNIQUE(purchase_key),
  UNIQUE(payment_id)
);

CREATE TABLE @badge_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES @invoices ON DELETE CASCADE,
  badge_purchase_id INTEGER NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
  price_id TEXT NOT NULL REFERENCES @badge_prices,
  offer_id TEXT REFERENCES @badge_offers,
  months INTEGER NOT NULL,
  created_at TEXT NOT NULL
);

CREATE INDEX @idx_badge_invoices_purchase ON @badge_invoices(badge_purchase_id);

CREATE TABLE @badge_subscription_changes(
  change_id TEXT NOT NULL PRIMARY KEY,
  badge_purchase_id INTEGER NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
  from_badge_type TEXT NOT NULL,
  to_badge_type TEXT NOT NULL,
  from_provider_ref TEXT,
  to_provider_ref TEXT,
  effective TEXT NOT NULL,
  status TEXT NOT NULL,
  effective_at TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE INDEX @idx_badge_subscription_changes_purchase ON @badge_subscription_changes(badge_purchase_id);

CREATE TABLE @badge_ledger(
  entry_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entry_uuid TEXT NOT NULL,
  badge_purchase_id INTEGER NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
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
  payment_id TEXT REFERENCES @payments,
  charge_id TEXT REFERENCES @subscription_charges,
  from_purchase_id INTEGER REFERENCES @badge_purchases,
  to_purchase_id INTEGER REFERENCES @badge_purchases
);

CREATE UNIQUE INDEX @idx_badge_ledger_uuid ON @badge_ledger(entry_uuid);

CREATE INDEX @idx_badge_ledger_purchase ON @badge_ledger(badge_purchase_id, entry_id);

CREATE INDEX @idx_badge_ledger_payment ON @badge_ledger(payment_id);

CREATE INDEX @idx_badge_ledger_charge ON @badge_ledger(charge_id);

CREATE INDEX @idx_badge_ledger_from_purchase ON @badge_ledger(from_purchase_id);

CREATE INDEX @idx_badge_ledger_to_purchase ON @badge_ledger(to_purchase_id);

CREATE TABLE @badge_issuances(
  issuance_id TEXT NOT NULL PRIMARY KEY,
  badge_purchase_id INTEGER NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
  entry_id INTEGER REFERENCES @badge_ledger,
  badge_type TEXT NOT NULL,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  expiry TEXT NOT NULL,
  credential BLOB NOT NULL,
  created_at TEXT NOT NULL
);

CREATE INDEX @idx_badge_issuances_purchase ON @badge_issuances(badge_purchase_id, issuance_id);
|]

badgeSchemaTablesDown :: Query
badgeSchemaTablesDown =
  [sql|
DROP TABLE @badge_issuances;
DROP TABLE @badge_ledger;
DROP TABLE @badge_subscription_changes;
DROP TABLE @badge_invoices;
DROP TABLE @badge_purchases;
DROP TABLE @subscription_charges;
DROP TABLE @payments;
DROP TABLE @invoices;
DROP TABLE @badge_offers;
DROP TABLE @badge_prices;
|]

m20260731_user_badges :: Query
m20260731_user_badges =
  badgeSchema ""
    <> [sql|
ALTER TABLE badge_purchases ADD COLUMN user_id INTEGER REFERENCES users ON DELETE CASCADE;

ALTER TABLE badge_purchases ADD COLUMN purchase_priv_key BLOB;

ALTER TABLE badge_purchases ADD COLUMN alert_acked_kind TEXT;

ALTER TABLE badge_purchases ADD COLUMN alert_acked_episode TEXT;

ALTER TABLE badge_purchases ADD COLUMN alert_snooze_until TEXT;

ALTER TABLE payments ADD COLUMN evidence BLOB;

ALTER TABLE payments ADD COLUMN receipt_code TEXT;

ALTER TABLE badge_ledger ADD COLUMN entry_type_unknown INTEGER NOT NULL DEFAULT 0;

ALTER TABLE badge_ledger ADD COLUMN entry_type_value TEXT;

CREATE INDEX idx_badge_purchases_user ON badge_purchases(user_id);

ALTER TABLE users ADD COLUMN shown_badge_id INTEGER REFERENCES badge_purchases ON DELETE SET NULL;
|]

down_m20260731_user_badges :: Query
down_m20260731_user_badges =
  [sql|
ALTER TABLE users DROP COLUMN shown_badge_id;
|]
    <> badgeSchemaDown ""
