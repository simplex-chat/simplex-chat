{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations, servicePrefix, withPrefix) where

import Data.List (sortOn)
import Data.Text (Text)
import Database.SQLite.Simple (Query (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Store.SQLite.Migrations.M20260915_user_badges (badgeSchema, badgeSchemaDown, withPrefix)
import Simplex.Messaging.Agent.Store.Shared (Migration (..))

badgeServiceSchemaMigrations :: [Migration]
badgeServiceSchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up = fromQuery up, down = fromQuery <$> down}

schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("20260915_badge_service_schema", m20260915_badge_service_schema, Just down_m20260915_badge_service_schema)
  ]

-- | The client tables share this database, so the service tables are the same names behind a prefix.
servicePrefix :: Text
servicePrefix = "sx_badge_service_"

m20260915_badge_service_schema :: Query
m20260915_badge_service_schema =
  badgeSchema servicePrefix
    <> withPrefix
      servicePrefix
      -- The payment columns are added to @payments, which badgeSchema owns. crypto_paid,
      -- crypto_due and paid_in_full record the provider's own figures: it applies a payment
      -- tolerance and adds a network fee after a partial payment, so what is owed and whether
      -- an invoice is settled are its verdicts, not amounts recomputable from what we store.
      [sql|
ALTER TABLE @payments ADD COLUMN crypto_paid TEXT;

ALTER TABLE @payments ADD COLUMN crypto_due TEXT;

ALTER TABLE @payments ADD COLUMN paid_in_full INTEGER NOT NULL DEFAULT 0;

CREATE TABLE @badge_codes(
  badge_code_id INTEGER PRIMARY KEY AUTOINCREMENT,
  code_hash BLOB NOT NULL,
  badge_type TEXT NOT NULL,
  months INTEGER NOT NULL,
  code_payment_status TEXT NOT NULL,
  redeemed_at TEXT,
  expires_at TEXT,
  revoked_at TEXT,
  created_at TEXT NOT NULL,
  UNIQUE(code_hash)
) STRICT;

ALTER TABLE @badge_purchases ADD COLUMN badge_code_id INTEGER REFERENCES @badge_codes;

CREATE UNIQUE INDEX @idx_badge_purchases_code ON @badge_purchases(badge_code_id);

CREATE TABLE @badge_code_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES @invoices ON DELETE CASCADE,
  badge_code_id INTEGER NOT NULL REFERENCES @badge_codes,
  price_id TEXT NOT NULL REFERENCES @badge_prices,
  offer_id TEXT REFERENCES @badge_offers,
  provider_ref TEXT NOT NULL,
  created_at TEXT NOT NULL
) STRICT;

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
      [sql|
CREATE INDEX @idx_invoices_status_expires_at ON @invoices(status, expires_at);

CREATE INDEX @idx_invoices_created ON @invoices(created_at);
|]

down_m20260915_badge_service_schema :: Query
down_m20260915_badge_service_schema =
  withPrefix
    servicePrefix
    [sql|
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
      [sql|
DROP TABLE @badge_codes;
|]
