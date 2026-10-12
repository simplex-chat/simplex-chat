{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations, servicePrefix, withPrefix) where

import Data.List (sortOn)
import Data.Text (Text)
import Simplex.Chat.Store.Postgres.Migrations.M20260915_user_badges (badgeSchema, badgeSchemaDown, withPrefix)
import Simplex.Messaging.Agent.Store.Shared (Migration (..))
import Text.RawString.QQ (r)

badgeServiceSchemaMigrations :: [Migration]
badgeServiceSchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20260915_badge_service_schema", m20260915_badge_service_schema, Just down_m20260915_badge_service_schema),
    ("20260918_badge_group_ops", m20260918_badge_group_ops, Just down_m20260918_badge_group_ops)
  ]

-- | The client tables share this database, so the service tables are the same names behind a prefix.
servicePrefix :: Text
servicePrefix = "sx_badge_service_"

m20260915_badge_service_schema :: Text
m20260915_badge_service_schema =
  badgeSchema servicePrefix
    <> withPrefix
      servicePrefix
      -- The payment columns are added to @payments, which badgeSchema owns. crypto_paid,
      -- crypto_due and paid_in_full record the provider's own figures: it applies a payment
      -- tolerance and adds a network fee after a partial payment, so what is owed and whether
      -- an invoice is settled are its verdicts, not amounts recomputable from what we store.
      [r|
ALTER TABLE @payments ADD COLUMN crypto_paid TEXT;

ALTER TABLE @payments ADD COLUMN crypto_due TEXT;

ALTER TABLE @payments ADD COLUMN paid_in_full SMALLINT NOT NULL DEFAULT 0;

CREATE TABLE @badge_codes(
  badge_code_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code_hash BYTEA NOT NULL,
  badge_type TEXT NOT NULL,
  months SMALLINT NOT NULL,
  code_payment_status TEXT NOT NULL,
  redeemed_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE(code_hash)
);

ALTER TABLE @badge_purchases ADD COLUMN badge_code_id BIGINT REFERENCES @badge_codes;

CREATE UNIQUE INDEX @idx_badge_purchases_code ON @badge_purchases(badge_code_id);

CREATE TABLE @badge_code_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES @invoices ON DELETE CASCADE,
  badge_code_id BIGINT NOT NULL REFERENCES @badge_codes,
  price_id TEXT NOT NULL REFERENCES @badge_prices,
  offer_id TEXT REFERENCES @badge_offers,
  provider_ref TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX @idx_badge_code_invoices_offer ON @badge_code_invoices(offer_id);

CREATE INDEX @idx_badge_code_invoices_price ON @badge_code_invoices(price_id);

-- provider_ref is the provider's own id for the invoice; unique so a webhook or poller read
-- resolves a payment to exactly one invoice.
CREATE UNIQUE INDEX @idx_badge_code_invoices_provider_ref ON @badge_code_invoices(provider_ref);
|]
    -- Two filters run on every poller pass and neither may read the whole table, or the pass
    -- lengthens for as long as the service keeps selling: the expiry sweep, on (status, expires_at),
    -- and the read lane, which takes a window of created_at. Status leads the first because it is
    -- matched by equality there; the second matches it with <>, which no index can seek, so it seeks
    -- the window and filters what little that leaves.
    <> withPrefix
      servicePrefix
      [r|
CREATE INDEX @idx_invoices_status_expires_at ON @invoices(status, expires_at);

CREATE INDEX @idx_invoices_created ON @invoices(created_at);
|]

down_m20260915_badge_service_schema :: Text
down_m20260915_badge_service_schema =
  withPrefix
    servicePrefix
    [r|
DROP INDEX @idx_invoices_created;
DROP INDEX @idx_invoices_status_expires_at;

DROP INDEX @idx_badge_code_invoices_provider_ref;
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

m20260918_badge_group_ops :: Text
m20260918_badge_group_ops =
  withPrefix
    servicePrefix
    [r|
CREATE TABLE @group(
  group_id BIGINT NOT NULL PRIMARY KEY,
  group_link TEXT NOT NULL,
  owner_bootstrapped SMALLINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL
);

ALTER TABLE @badge_codes ADD COLUMN redeem_limit INTEGER NOT NULL DEFAULT 1;

ALTER TABLE @badge_codes ADD COLUMN redeem_count INTEGER NOT NULL DEFAULT 0;

ALTER TABLE @badge_codes ADD COLUMN group_item_id BIGINT;

ALTER TABLE @badge_codes ADD COLUMN group_item_sent_at TIMESTAMPTZ;

-- Redemptions made before this migration must count against the new limit, or every code
-- redeemed already would read as unspent and could be redeemed once more.
UPDATE @badge_codes SET redeem_count = 1 WHERE redeemed_at IS NOT NULL;

DROP INDEX @idx_badge_purchases_code;

CREATE INDEX @idx_badge_purchases_code ON @badge_purchases(badge_code_id);
|]

-- The index stays non-unique, since a multi-use code may already have several purchases.
down_m20260918_badge_group_ops :: Text
down_m20260918_badge_group_ops =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @badge_codes DROP COLUMN group_item_sent_at;
ALTER TABLE @badge_codes DROP COLUMN group_item_id;
ALTER TABLE @badge_codes DROP COLUMN redeem_count;
ALTER TABLE @badge_codes DROP COLUMN redeem_limit;
DROP TABLE @group;
|]

{- TODO [badges] deferred with the draft in M20260915_user_badges, service only.

ALTER TABLE @payments ADD COLUMN receipt_hash BYTEA;

-- down
ALTER TABLE @payments DROP COLUMN receipt_hash;
-}
