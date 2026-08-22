{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations) where

import Data.List (sortOn)
import Data.Text (Text)
import Database.SQLite.Simple (Query (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Store.SQLite.Migrations.M20261001_user_badges (badgeSchema, badgeSchemaDown, withPrefix)
import Simplex.Messaging.Agent.Store.Shared (Migration (..))

badgeServiceSchemaMigrations :: [Migration]
badgeServiceSchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up = fromQuery up, down = fromQuery <$> down}

schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("20260806_badge_service_schema", m20260806_badge_service_schema, Just down_m20260806_badge_service_schema),
    ("20260821_badge_service_web", m20260821_badge_service_web, Just down_m20260821_badge_service_web)
  ]

-- the client tables are in the same database, so the service tables are the same names with this prefix
servicePrefix :: Text
servicePrefix = "sx_badge_service_"

m20260806_badge_service_schema :: Query
m20260806_badge_service_schema =
  badgeSchema servicePrefix
    <> withPrefix
      servicePrefix
      [sql|
ALTER TABLE @payments ADD COLUMN receipt_hash BLOB;
|]

down_m20260806_badge_service_schema :: Query
down_m20260806_badge_service_schema = badgeSchemaDown servicePrefix

m20260821_badge_service_web :: Query
m20260821_badge_service_web =
  withPrefix
    servicePrefix
    [sql|
CREATE TABLE @web_orders(
  order_id TEXT NOT NULL PRIMARY KEY,   -- 128-bit random, base64url; a bearer capability, see E4
  invoice_id TEXT REFERENCES @invoices, -- money side reuses the existing invoices table
  -- provider invoice / session / payment-intent id; order-side only, never a code or purchase
  -- reference. Unique across providers: a collision must fail loudly rather than resolve a
  -- charge to the wrong order.
  provider_ref TEXT,
  method TEXT NOT NULL CHECK (method IN ('card','btc','xmr')),
  short_ref TEXT NOT NULL,              -- 5 Crockford chars, generated per order (D6); the reference
                                        -- support resolves by, shown on card statements (F1) and on
                                        -- the crypto payment and result screens (E5, E6)
  badge_type TEXT NOT NULL CHECK (badge_type IN ('supporter','legend')),
  price_id TEXT REFERENCES @badge_prices,
  offer_id TEXT REFERENCES @badge_offers,
  months INTEGER NOT NULL,
  -- invoiced (created, unpaid) -> pending (partial or unconfirmed) -> paid (terminal).
  -- invoiced|pending -> expired|failed; both remain recoverable to paid (E3).
  status TEXT NOT NULL CHECK (status IN ('invoiced','pending','paid','expired','failed')),
  -- amount received, in minor units of the invoice currency, at the rate the provider locked
  amount_paid INTEGER,
  settled_at TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE INDEX @idx_web_orders_invoice ON @web_orders(invoice_id);

CREATE UNIQUE INDEX @idx_web_orders_provider_ref ON @web_orders(provider_ref);

CREATE UNIQUE INDEX @idx_web_orders_short_ref ON @web_orders(short_ref);

-- No order reference, and no code_hash on @web_orders: no row may hold both an order
-- reference and a purchase reference (§3 Linkage). An order's code row is found by
-- deriving the code from orderId and hashing it.
CREATE TABLE @codes(
  code_hash BLOB NOT NULL PRIMARY KEY,
  badge_type TEXT NOT NULL CHECK (badge_type IN ('supporter','legend')),
  months INTEGER NOT NULL CHECK (months > 0), -- lifetime codes are out of scope, see §6
  batch TEXT NOT NULL,                  -- 'web' for web orders, else an operator batch name
  expires_at TEXT NOT NULL,             -- redemption deadline -> code_expired
  redeemed_purchase_id INTEGER REFERENCES @badge_purchases,
  redeemed_at TEXT,
  unredeemed_at TEXT,                   -- set by B1's unredeemCode; reopens E4's window
  revoked_at TEXT,
  created_at TEXT NOT NULL
) STRICT;

CREATE INDEX @idx_codes_batch ON @codes(batch);

CREATE TABLE @provider_events(
  provider TEXT NOT NULL,
  event_id TEXT NOT NULL,
  received_at TEXT NOT NULL,
  processed_at TEXT,                    -- NULL means the previous attempt did not complete (E3)
  PRIMARY KEY(provider, event_id)
) STRICT;
|]

down_m20260821_badge_service_web :: Query
down_m20260821_badge_service_web =
  withPrefix
    servicePrefix
    [sql|
DROP TABLE @provider_events;
DROP TABLE @codes;
DROP TABLE @web_orders;
|]
