CREATE TABLE products(
  product_id TEXT PRIMARY KEY,
  product_type TEXT NOT NULL,
  badge_type TEXT NOT NULL,
  active BOOLEAN NOT NULL DEFAULT true,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE offers(
  offer_id TEXT PRIMARY KEY,
  product_id TEXT NOT NULL REFERENCES products,
  plan TEXT NOT NULL CHECK (plan IN ('one_time', 'monthly', 'annual')),
  months INT,
  apple_product_id TEXT,
  google_product_id TEXT,
  price INT,
  currency TEXT,
  state TEXT NOT NULL CHECK (state IN ('active', 'deprecated', 'disabled')),
  created_at TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX idx_offers_apple ON offers(apple_product_id) WHERE apple_product_id IS NOT NULL;

CREATE UNIQUE INDEX idx_offers_google ON offers(google_product_id) WHERE google_product_id IS NOT NULL;

CREATE TABLE orders(
  order_key BYTEA PRIMARY KEY,
  product_id TEXT NOT NULL REFERENCES products,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE payments(
  payment_ref TEXT PRIMARY KEY,
  order_key BYTEA NOT NULL REFERENCES orders,
  offer_id TEXT REFERENCES offers,
  provider TEXT NOT NULL CHECK (provider IN ('apple', 'google', 'stripe', 'btc', 'xmr', 'code')),
  provider_ref TEXT,
  months INT,
  amount INT,
  currency TEXT,
  status TEXT NOT NULL CHECK (status IN ('invoiced', 'pending', 'settled', 'failed', 'expired')),
  exception TEXT,
  receipt_hash BYTEA,
  renews_at TIMESTAMPTZ,
  grace_until TIMESTAMPTZ,
  cancelled BOOLEAN NOT NULL DEFAULT false,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX idx_payments_order ON payments(order_key);

CREATE INDEX idx_payments_provider_ref ON payments(provider, provider_ref);

CREATE TABLE charges(
  charge_id BIGSERIAL PRIMARY KEY,
  payment_ref TEXT NOT NULL REFERENCES payments,
  provider_charge_ref TEXT NOT NULL,
  period_start TIMESTAMPTZ NOT NULL,
  period_end TIMESTAMPTZ NOT NULL,
  amount INT NOT NULL,
  currency TEXT NOT NULL,
  charged_at TIMESTAMPTZ NOT NULL,
  UNIQUE(payment_ref, provider_charge_ref)
);

CREATE TABLE badge_ledger(
  entry_id BIGSERIAL PRIMARY KEY,
  order_key BYTEA NOT NULL REFERENCES orders,
  op TEXT NOT NULL CHECK (op IN (
    'grant_payment', 'grant_charge', 'grant_goodwill', 'grant_transfer_in',
    'debit_refund', 'debit_conversion', 'debit_transfer_out', 'debit_correction',
    'consume', 'lapse', 'resume'
  )),
  delta INT NOT NULL,
  months INT NOT NULL CHECK (months >= 0),
  start TIMESTAMPTZ NOT NULL,
  payment_ref TEXT,
  charge_id BIGINT,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX idx_badge_ledger_order ON badge_ledger(order_key, entry_id DESC);

CREATE TABLE issuances(
  issuance_id BIGSERIAL PRIMARY KEY,
  order_key BYTEA NOT NULL REFERENCES orders,
  period_start TIMESTAMPTZ,
  period_end TIMESTAMPTZ,
  expiry TIMESTAMPTZ,
  entry_id BIGINT REFERENCES badge_ledger,
  credential BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE(order_key, period_start)
);

CREATE TABLE codes(
  code_hash BYTEA PRIMARY KEY,
  badge_type TEXT NOT NULL,
  months INT,
  batch TEXT NOT NULL,
  redeemed_order BYTEA REFERENCES orders,
  redeemed_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE provider_events(
  provider TEXT NOT NULL,
  event_id TEXT NOT NULL,
  received_at TIMESTAMPTZ NOT NULL,
  processed_at TIMESTAMPTZ,
  PRIMARY KEY(provider, event_id)
);
