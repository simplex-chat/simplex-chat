{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module StoreService.Store.Postgres.Migrations (storeServiceSchemaMigrations, servicePrefix, withPrefix) where

import Data.List (sortOn)
import Data.Text (Text)
import Simplex.Chat.Store.Postgres.Migrations.M20260915_user_badges (badgeSchema, badgeSchemaDown, withPrefix)
import Simplex.Messaging.Agent.Store.Shared (Migration (..))
import Text.RawString.QQ (r)

storeServiceSchemaMigrations :: [Migration]
storeServiceSchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20260915_badge_service_schema", m20260915_badge_service_schema, Just down_m20260915_badge_service_schema),
    ("20260918_badge_group_ops", m20260918_badge_group_ops, Just down_m20260918_badge_group_ops),
    ("20261010_names", m20261010_names, Just down_m20261010_names),
    ("20261022_drop_taken_codes", m20261022_drop_taken_codes, Just down_m20261022_drop_taken_codes)
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

-- A registration is live as Names.Store's orderLive decides.
-- reserved_by is added after the registrations table, which references the codes table.
m20261010_names :: Text
m20261010_names =
  withPrefix
    servicePrefix
    [r|
CREATE TABLE @name_prepaid_codes(
  name_prepaid_code_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code_hash BYTEA NOT NULL,
  min_length INTEGER NOT NULL,
  years INTEGER NOT NULL,
  code_payment_status TEXT NOT NULL,
  reserved_at TIMESTAMPTZ,
  spent_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE(code_hash)
);

CREATE TABLE @name_registrations(
  registration_id TEXT NOT NULL PRIMARY KEY,
  version BIGINT NOT NULL,
  label_hash BYTEA NOT NULL,
  length INTEGER NOT NULL,
  duration BIGINT NOT NULL,
  owner BYTEA NOT NULL,
  commitment BYTEA NOT NULL,
  sealed TEXT NOT NULL,
  phase TEXT NOT NULL,
  years INTEGER NOT NULL,
  price INTEGER NOT NULL,
  method TEXT,
  name_prepaid_code_id BIGINT REFERENCES @name_prepaid_codes,
  label TEXT,
  secret BYTEA,
  contact_link TEXT,
  channel_link TEXT,
  intent_nonce BIGINT,
  intent_deadline BIGINT,
  intent_signature BYTEA,
  commit_tx BYTEA,
  commit_block BIGINT,
  register_tx BYTEA,
  register_block BIGINT,
  reveal_after BIGINT,
  reveal_before BIGINT,
  expires_at BIGINT,
  issued_code_id BIGINT REFERENCES @name_prepaid_codes,
  registered_at TIMESTAMPTZ,
  failed_at TIMESTAMPTZ,
  error TEXT,
  live SMALLINT NOT NULL,
  order_hash BYTEA NOT NULL,
  moves INTEGER NOT NULL DEFAULT 0,
  links_refused_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

ALTER TABLE @name_prepaid_codes ADD COLUMN reserved_by TEXT REFERENCES @name_registrations;

CREATE UNIQUE INDEX @idx_name_prepaid_codes_reserved_by ON @name_prepaid_codes(reserved_by);

CREATE UNIQUE INDEX @idx_name_registrations_live_commitment ON @name_registrations(commitment) WHERE live = 1;

CREATE UNIQUE INDEX @idx_name_registrations_live_owner_label ON @name_registrations(owner, label_hash) WHERE live = 1;

CREATE INDEX @idx_name_registrations_label_phase ON @name_registrations(label_hash, phase);

CREATE INDEX @idx_name_registrations_owner ON @name_registrations(owner, created_at);

CREATE INDEX @idx_name_registrations_phase ON @name_registrations(phase, created_at);

CREATE INDEX @idx_name_registrations_order_hash ON @name_registrations(order_hash);

CREATE INDEX @idx_name_registrations_prepaid_code ON @name_registrations(name_prepaid_code_id);

CREATE INDEX @idx_name_registrations_issued_code ON @name_registrations(issued_code_id);

CREATE TABLE @name_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES @invoices ON DELETE CASCADE,
  registration_id TEXT NOT NULL REFERENCES @name_registrations ON DELETE CASCADE,
  provider_ref TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX @idx_name_invoices_registration ON @name_invoices(registration_id);

CREATE UNIQUE INDEX @idx_name_invoices_provider_ref ON @name_invoices(provider_ref);

CREATE TABLE @name_record_edits(
  name_record_edit_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  label_hash BYTEA NOT NULL,
  owner BYTEA NOT NULL,
  nonce BIGINT NOT NULL,
  record_key TEXT,
  record_value TEXT,
  deadline BIGINT NOT NULL,
  signature BYTEA NOT NULL,
  body_hash BYTEA NOT NULL,
  state TEXT NOT NULL,
  revert TEXT,
  mined_tx BYTEA,
  created_at TIMESTAMPTZ NOT NULL
);

-- A reverted edit keeps its row for its repeats while a new body reuses the nonce.
CREATE UNIQUE INDEX @idx_name_record_edits_owner_nonce ON @name_record_edits(owner, nonce) WHERE state IN ('pending', 'mined');

CREATE INDEX @idx_name_record_edits_body_hash ON @name_record_edits(body_hash);

CREATE INDEX @idx_name_record_edits_label_created ON @name_record_edits(label_hash, created_at);

CREATE TABLE @name_transactions(
  name_transaction_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  registration_id TEXT REFERENCES @name_registrations,
  name_record_edit_id BIGINT REFERENCES @name_record_edits,
  kind TEXT NOT NULL,
  raw BYTEA NOT NULL,
  tx_hash BYTEA NOT NULL,
  registrar_nonce BIGINT NOT NULL,
  sent_at TIMESTAMPTZ NOT NULL,
  mined_block BIGINT,
  status TEXT NOT NULL,
  CHECK ((registration_id IS NULL) <> (name_record_edit_id IS NULL)),
  UNIQUE(tx_hash)
);

CREATE INDEX @idx_name_transactions_registration ON @name_transactions(registration_id);

CREATE INDEX @idx_name_transactions_edit ON @name_transactions(name_record_edit_id);

CREATE INDEX @idx_name_transactions_status ON @name_transactions(status, registrar_nonce);
|]

-- A table another table's foreign key references cannot be dropped, so reserved_by goes first.
-- The name orders' invoices go with them, except those with a payment, which stay for the accounts.
down_m20261010_names :: Text
down_m20261010_names =
  withPrefix
    servicePrefix
    [r|
DROP INDEX @idx_name_transactions_status;
DROP INDEX @idx_name_transactions_edit;
DROP INDEX @idx_name_transactions_registration;
DROP TABLE @name_transactions;

DROP INDEX @idx_name_record_edits_label_created;
DROP INDEX @idx_name_record_edits_body_hash;
DROP INDEX @idx_name_record_edits_owner_nonce;
DROP TABLE @name_record_edits;

DELETE FROM @invoices WHERE invoice_id IN (
  SELECT ni.invoice_id FROM @name_invoices ni
  WHERE NOT EXISTS (SELECT 1 FROM @payments p WHERE p.invoice_id = ni.invoice_id)
);
DROP INDEX @idx_name_invoices_provider_ref;
DROP INDEX @idx_name_invoices_registration;
DROP TABLE @name_invoices;

DROP INDEX @idx_name_registrations_issued_code;
DROP INDEX @idx_name_registrations_prepaid_code;
DROP INDEX @idx_name_registrations_order_hash;
DROP INDEX @idx_name_registrations_phase;
DROP INDEX @idx_name_registrations_owner;
DROP INDEX @idx_name_registrations_label_phase;
DROP INDEX @idx_name_registrations_live_owner_label;
DROP INDEX @idx_name_registrations_live_commitment;
DROP INDEX @idx_name_prepaid_codes_reserved_by;
ALTER TABLE @name_prepaid_codes DROP COLUMN reserved_by;
DROP TABLE @name_registrations;

DROP TABLE @name_prepaid_codes;
|]

-- A code_issued order's worth went into its code, which stays a valid prepaid code, so it ends taken with nothing left.
m20261022_drop_taken_codes :: Text
m20261022_drop_taken_codes =
  withPrefix
    servicePrefix
    [r|
UPDATE @name_registrations SET phase = 'taken', price = 0, live = 0 WHERE phase = 'code_issued';

DROP INDEX @idx_name_registrations_issued_code;

ALTER TABLE @name_registrations DROP COLUMN issued_code_id;
|]

down_m20261022_drop_taken_codes :: Text
down_m20261022_drop_taken_codes =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @name_registrations ADD COLUMN issued_code_id BIGINT REFERENCES @name_prepaid_codes;

CREATE INDEX @idx_name_registrations_issued_code ON @name_registrations(issued_code_id);
|]

{- TODO [badges] deferred with the draft in M20260915_user_badges, service only.

ALTER TABLE @payments ADD COLUMN receipt_hash BYTEA;

-- down
ALTER TABLE @payments DROP COLUMN receipt_hash;
-}
