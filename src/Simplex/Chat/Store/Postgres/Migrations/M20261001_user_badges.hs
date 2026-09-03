{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261001_user_badges where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

-- the same tables in the client and in the badge service; '@' is replaced with the table name prefix
badgeSchema :: Text -> Text
badgeSchema pfx = withPrefix pfx badgeSchemaTables

badgeSchemaDown :: Text -> Text
badgeSchemaDown pfx = withPrefix pfx badgeSchemaTablesDown

withPrefix :: Text -> Text -> Text
withPrefix = T.replace "@"

badgeSchemaTables :: Text
badgeSchemaTables =
  [r|
CREATE TABLE @invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY,
  provider TEXT NOT NULL,
  price BIGINT NOT NULL,
  discount_amount BIGINT,
  credit_amount BIGINT,
  amount BIGINT NOT NULL,
  currency TEXT NOT NULL,
  payment_url TEXT,
  payment_address TEXT,
  payment_crypto_currency TEXT,
  payment_crypto_amount TEXT,
  expires_at TIMESTAMPTZ NOT NULL,
  status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE @payments(
  payment_id TEXT NOT NULL PRIMARY KEY,
  invoice_id TEXT REFERENCES @invoices,
  provider TEXT NOT NULL,
  provider_ref TEXT,
  amount BIGINT,
  currency TEXT,
  status TEXT NOT NULL,
  exception TEXT,
  subscription_renews_at TIMESTAMPTZ,
  grace_until TIMESTAMPTZ,
  cancelled SMALLINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX @idx_payments_provider_ref ON @payments(provider, provider_ref);

CREATE INDEX @idx_payments_invoice ON @payments(invoice_id);

CREATE TABLE @subscription_charges(
  charge_id TEXT NOT NULL PRIMARY KEY,
  payment_id TEXT NOT NULL REFERENCES @payments ON DELETE CASCADE,
  provider_charge_ref TEXT NOT NULL,
  period_start TIMESTAMPTZ NOT NULL,
  period_end TIMESTAMPTZ NOT NULL,
  amount BIGINT NOT NULL,
  currency TEXT NOT NULL,
  charged_at TIMESTAMPTZ NOT NULL,
  UNIQUE(payment_id, provider_charge_ref)
);

CREATE TABLE @badge_prices(
  price_id TEXT NOT NULL PRIMARY KEY,
  badge_type TEXT NOT NULL,
  month_price BIGINT NOT NULL,
  currency TEXT NOT NULL,
  status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE @badge_offers(
  offer_id TEXT NOT NULL PRIMARY KEY,
  price_id TEXT REFERENCES @badge_prices,
  months SMALLINT NOT NULL,
  free_months SMALLINT,
  discount SMALLINT,
  status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX @idx_badge_offers_price ON @badge_offers(price_id);

CREATE TABLE @badge_purchases(
  badge_purchase_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  purchase_key BYTEA NOT NULL,
  master_key BYTEA NOT NULL,
  initial_badge_type TEXT NOT NULL,
  current_badge_type TEXT NOT NULL,
  payment_id TEXT REFERENCES @payments,
  status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  UNIQUE(purchase_key),
  UNIQUE(payment_id)
);

CREATE TABLE @badge_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES @invoices ON DELETE CASCADE,
  badge_purchase_id BIGINT NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
  price_id TEXT NOT NULL REFERENCES @badge_prices,
  offer_id TEXT REFERENCES @badge_offers,
  months SMALLINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX @idx_badge_invoices_purchase ON @badge_invoices(badge_purchase_id);

CREATE INDEX @idx_badge_invoices_offer ON @badge_invoices(offer_id);

CREATE INDEX @idx_badge_invoices_price ON @badge_invoices(price_id);

CREATE TABLE @badge_subscription_changes(
  change_id TEXT NOT NULL PRIMARY KEY,
  badge_purchase_id BIGINT NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
  from_badge_type TEXT NOT NULL,
  to_badge_type TEXT NOT NULL,
  from_provider_ref TEXT,
  to_provider_ref TEXT,
  effective TEXT NOT NULL,
  status TEXT NOT NULL,
  effective_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX @idx_badge_subscription_changes_purchase ON @badge_subscription_changes(badge_purchase_id);

CREATE TABLE @badge_ledger(
  entry_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  entry_uuid TEXT NOT NULL,
  badge_purchase_id BIGINT NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
  change_months SMALLINT NOT NULL,
  balance_months SMALLINT NOT NULL,
  balance_start_ts TIMESTAMPTZ NOT NULL,
  balance_anchor_ts TIMESTAMPTZ NOT NULL,
  balance_badge_type TEXT NOT NULL,
  was_paused_since TIMESTAMPTZ,
  service_created_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  entry_type TEXT NOT NULL,
  entry_credit_type TEXT,
  entry_debit_type TEXT,
  payment_id TEXT REFERENCES @payments,
  charge_id TEXT REFERENCES @subscription_charges,
  from_purchase_id BIGINT REFERENCES @badge_purchases,
  to_purchase_id BIGINT REFERENCES @badge_purchases
);

CREATE UNIQUE INDEX @idx_badge_ledger_uuid ON @badge_ledger(entry_uuid);

CREATE INDEX @idx_badge_ledger_purchase ON @badge_ledger(badge_purchase_id, entry_id);

CREATE INDEX @idx_badge_ledger_payment ON @badge_ledger(payment_id);

CREATE INDEX @idx_badge_ledger_charge ON @badge_ledger(charge_id);

CREATE INDEX @idx_badge_ledger_from_purchase ON @badge_ledger(from_purchase_id);

CREATE INDEX @idx_badge_ledger_to_purchase ON @badge_ledger(to_purchase_id);

CREATE TABLE @badge_issuances(
  issuance_id TEXT NOT NULL PRIMARY KEY,
  badge_purchase_id BIGINT NOT NULL REFERENCES @badge_purchases ON DELETE CASCADE,
  entry_id BIGINT REFERENCES @badge_ledger,
  badge_type TEXT NOT NULL,
  period_start TIMESTAMPTZ NOT NULL,
  period_end TIMESTAMPTZ NOT NULL,
  expiry TIMESTAMPTZ NOT NULL,
  credential BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX @idx_badge_issuances_purchase ON @badge_issuances(badge_purchase_id, issuance_id);

CREATE INDEX @idx_badge_issuances_entry ON @badge_issuances(entry_id);
|]

badgeSchemaTablesDown :: Text
badgeSchemaTablesDown =
  [r|
DROP INDEX @idx_badge_issuances_purchase;
DROP INDEX @idx_badge_issuances_entry;
DROP TABLE @badge_issuances;
DROP INDEX @idx_badge_ledger_uuid;
DROP INDEX @idx_badge_ledger_purchase;
DROP INDEX @idx_badge_ledger_payment;
DROP INDEX @idx_badge_ledger_charge;
DROP INDEX @idx_badge_ledger_from_purchase;
DROP INDEX @idx_badge_ledger_to_purchase;
DROP TABLE @badge_ledger;
DROP INDEX @idx_badge_subscription_changes_purchase;
DROP TABLE @badge_subscription_changes;
DROP INDEX @idx_badge_invoices_purchase;
DROP INDEX @idx_badge_invoices_offer;
DROP INDEX @idx_badge_invoices_price;
DROP TABLE @badge_invoices;
DROP TABLE @badge_purchases;
DROP TABLE @subscription_charges;
DROP INDEX @idx_payments_provider_ref;
DROP INDEX @idx_payments_invoice;
DROP TABLE @payments;
DROP TABLE @invoices;
DROP INDEX @idx_badge_offers_price;
DROP TABLE @badge_offers;
DROP TABLE @badge_prices;
|]

m20261001_user_badges :: Text
m20261001_user_badges =
  badgeSchema ""
    <> [r|
ALTER TABLE badge_purchases ADD COLUMN user_id BIGINT REFERENCES users ON DELETE CASCADE;

ALTER TABLE badge_purchases ADD COLUMN purchase_priv_key BYTEA;

ALTER TABLE badge_purchases ADD COLUMN alert_acked_kind TEXT;

ALTER TABLE badge_purchases ADD COLUMN alert_acked_episode TEXT;

ALTER TABLE badge_purchases ADD COLUMN alert_snooze_until TIMESTAMPTZ;

ALTER TABLE payments ADD COLUMN evidence BYTEA;

ALTER TABLE payments ADD COLUMN receipt_code TEXT;

ALTER TABLE badge_ledger ADD COLUMN entry_type_unknown SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE badge_ledger ADD COLUMN entry_type_value TEXT;

CREATE INDEX idx_badge_purchases_user ON badge_purchases(user_id);

ALTER TABLE users ADD COLUMN shown_badge_id BIGINT REFERENCES badge_purchases ON DELETE SET NULL;

CREATE INDEX idx_users_shown_badge ON users(shown_badge_id);

CREATE TABLE badge_code_redemptions(
  badge_code_redemption_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  code TEXT NOT NULL,
  purchase_key BYTEA NOT NULL,
  purchase_priv_key BYTEA NOT NULL,
  master_key BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE(user_id, code)
);

CREATE INDEX idx_badge_code_redemptions_user ON badge_code_redemptions(user_id);

ALTER TABLE badge_purchases ADD COLUMN badge_code_redemption_id BIGINT REFERENCES badge_code_redemptions;

CREATE UNIQUE INDEX idx_badge_purchases_code_redemption ON badge_purchases(badge_code_redemption_id);
|]

down_m20261001_user_badges :: Text
down_m20261001_user_badges =
  [r|
DROP INDEX idx_badge_purchases_code_redemption;
DROP INDEX idx_badge_purchases_user;
DROP INDEX idx_users_shown_badge;
ALTER TABLE users DROP COLUMN shown_badge_id;
|]
    <> badgeSchemaDown ""
    <> [r|
DROP INDEX idx_badge_code_redemptions_user;
DROP TABLE badge_code_redemptions;
|]
