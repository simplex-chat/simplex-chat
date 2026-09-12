{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations, servicePrefix, withPrefix) where

import Data.List (sortOn)
import Data.Text (Text)
import Simplex.Chat.Store.Postgres.Migrations.M20261001_user_badges (badgeSchema, badgeSchemaDown, withPrefix)
import Simplex.Messaging.Agent.Store.Shared (Migration (..))
import Text.RawString.QQ (r)

badgeServiceSchemaMigrations :: [Migration]
badgeServiceSchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20260806_badge_service_schema", m20260806_badge_service_schema, Just down_m20260806_badge_service_schema),
    ("20260831_badge_service_web", m20260831_badge_service_web, Just down_m20260831_badge_service_web),
    ("20260903_payment_paid_in_full", m20260903_payment_paid_in_full, Just down_m20260903_payment_paid_in_full),
    ("20260904_payment_crypto_due", m20260904_payment_crypto_due, Just down_m20260904_payment_crypto_due),
    ("20260905_invoices_open_index", m20260905_invoices_open_index, Just down_m20260905_invoices_open_index)
  ]

-- | The client tables share this database, so the service tables are the same names behind a prefix.
servicePrefix :: Text
servicePrefix = "sx_badge_service_"

m20260806_badge_service_schema :: Text
m20260806_badge_service_schema =
  badgeSchema servicePrefix
    <> withPrefix
      servicePrefix
      [r|
ALTER TABLE @payments ADD COLUMN receipt_hash BYTEA;

CREATE TABLE @badge_codes(
  badge_code_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code_hash BYTEA NOT NULL,
  badge_type TEXT NOT NULL,
  months SMALLINT NOT NULL,
  code_payment_status TEXT NOT NULL,
  redeemed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE(code_hash)
);

ALTER TABLE @badge_purchases ADD COLUMN badge_code_id BIGINT REFERENCES @badge_codes;

CREATE UNIQUE INDEX @idx_badge_purchases_code ON @badge_purchases(badge_code_id);

CREATE TABLE @badge_code_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES @invoices ON DELETE CASCADE,
  price_id TEXT NOT NULL REFERENCES @badge_prices,
  offer_id TEXT REFERENCES @badge_offers,
  months SMALLINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX @idx_badge_code_invoices_offer ON @badge_code_invoices(offer_id);

CREATE INDEX @idx_badge_code_invoices_price ON @badge_code_invoices(price_id);
|]

down_m20260806_badge_service_schema :: Text
down_m20260806_badge_service_schema =
  withPrefix
    servicePrefix
    [r|
DROP INDEX @idx_badge_code_invoices_offer;
DROP INDEX @idx_badge_code_invoices_price;
DROP TABLE @badge_code_invoices;

DROP INDEX @idx_badge_purchases_code;
|]
    <> badgeSchemaDown servicePrefix
    <> withPrefix
      servicePrefix
      [r|
DROP TABLE @badge_codes;
|]

-- provider_ref is NOT NULL UNIQUE in the schema, but SQLite cannot ADD
-- COLUMN a NOT NULL column without a default, so it is nullable here too and the
-- unique index does the work. Kept the same on both backends.
m20260831_badge_service_web :: Text
m20260831_badge_service_web =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @badge_code_invoices ADD COLUMN code_hash BYTEA;
ALTER TABLE @badge_code_invoices ADD COLUMN provider_ref TEXT;
CREATE UNIQUE INDEX @idx_badge_code_invoices_provider_ref ON @badge_code_invoices(provider_ref);
ALTER TABLE @payments ADD COLUMN crypto_amount TEXT;
ALTER TABLE @badge_codes ADD COLUMN expires_at TIMESTAMPTZ;
ALTER TABLE @badge_codes ADD COLUMN revoked_at TIMESTAMPTZ;
|]

down_m20260831_badge_service_web :: Text
down_m20260831_badge_service_web =
  withPrefix
    servicePrefix
    [r|
DROP INDEX @idx_badge_code_invoices_provider_ref;
ALTER TABLE @badge_code_invoices DROP COLUMN provider_ref;
ALTER TABLE @badge_code_invoices DROP COLUMN code_hash;
ALTER TABLE @payments DROP COLUMN crypto_amount;
ALTER TABLE @badge_codes DROP COLUMN revoked_at;
ALTER TABLE @badge_codes DROP COLUMN expires_at;
|]

-- The provider applies its own payment tolerance, so whether an invoice is paid is its
-- verdict and cannot be recomputed from the amounts we store.
m20260903_payment_paid_in_full :: Text
m20260903_payment_paid_in_full =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @payments ADD COLUMN paid_in_full SMALLINT NOT NULL DEFAULT 0;
|]

down_m20260903_payment_paid_in_full :: Text
down_m20260903_payment_paid_in_full =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @payments DROP COLUMN paid_in_full;
|]

-- The provider knows what is still owed: it applies the payment tolerance and adds a network
-- fee after a partial payment, so the figure cannot be recomputed from the amounts we store.
m20260904_payment_crypto_due :: Text
m20260904_payment_crypto_due =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @payments ADD COLUMN crypto_due TEXT;
|]

down_m20260904_payment_crypto_due :: Text
down_m20260904_payment_crypto_due =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @payments DROP COLUMN crypto_due;
|]

-- Two filters run on every poller pass and neither may read the whole table, or the pass
-- lengthens for as long as the service keeps selling: the expiry sweep, on (status, expires_at),
-- and the read lane, which takes a window of created_at. Status leads the first because it is
-- matched by equality there; the second matches it with <>, which no index can seek, so it seeks
-- the window and filters what little that leaves.
m20260905_invoices_open_index :: Text
m20260905_invoices_open_index =
  withPrefix
    servicePrefix
    [r|
CREATE INDEX @idx_invoices_open ON @invoices(status, expires_at);

CREATE INDEX @idx_invoices_created ON @invoices(created_at);
|]

down_m20260905_invoices_open_index :: Text
down_m20260905_invoices_open_index =
  withPrefix
    servicePrefix
    [r|
DROP INDEX @idx_invoices_created;
DROP INDEX @idx_invoices_open;
|]

