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

CREATE TABLE store_skus(
  store_sku_id INTEGER PRIMARY KEY AUTOINCREMENT,
  provider TEXT NOT NULL,
  sku TEXT NOT NULL,
  badge_type TEXT NOT NULL,
  plan TEXT NOT NULL,
  months INTEGER,
  created_at TEXT NOT NULL
);

CREATE UNIQUE INDEX idx_store_skus_provider_sku ON store_skus(provider, sku);

CREATE TABLE badge_purchases(
  badge_purchase_id INTEGER PRIMARY KEY AUTOINCREMENT,
  purchase_key BLOB NOT NULL,
  badge_type TEXT NOT NULL,
  price_id TEXT REFERENCES badge_prices,
  offer_id TEXT REFERENCES badge_offers,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  UNIQUE(purchase_key)
);

CREATE TABLE payments(
  payment_id INTEGER PRIMARY KEY AUTOINCREMENT,
  badge_purchase_id INTEGER NOT NULL REFERENCES badge_purchases,
  badge_type TEXT NOT NULL,
  price_id TEXT REFERENCES badge_prices,
  offer_id TEXT REFERENCES badge_offers,
  invoice_uuid TEXT,
  provider TEXT NOT NULL,
  provider_ref TEXT,
  months INTEGER,
  amount INTEGER,
  currency TEXT,
  status TEXT NOT NULL,
  exception TEXT,
  receipt_hash BLOB,
  renews_at TEXT,
  grace_until TEXT,
  cancelled INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE UNIQUE INDEX idx_payments_invoice_uuid ON payments(invoice_uuid);

CREATE INDEX idx_payments_purchase ON payments(badge_purchase_id);

CREATE INDEX idx_payments_provider_ref ON payments(provider, provider_ref);

CREATE TABLE charges(
  charge_id INTEGER PRIMARY KEY,
  payment_id INTEGER NOT NULL REFERENCES payments,
  provider_charge_ref TEXT NOT NULL,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  amount INTEGER NOT NULL,
  currency TEXT NOT NULL,
  charged_at TEXT NOT NULL,
  UNIQUE(payment_id, provider_charge_ref)
);

-- badgeLedgerTable constant (M20261001_user_badges), re-used literally
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

CREATE UNIQUE INDEX idx_badge_ledger_uuid ON badge_ledger(entry_uuid);

CREATE INDEX idx_badge_ledger_purchase ON badge_ledger(badge_purchase_id, entry_id);

CREATE INDEX idx_badge_ledger_invoice ON badge_ledger(invoice_id);

CREATE INDEX idx_badge_ledger_charge ON badge_ledger(charge_id);

CREATE INDEX idx_badge_ledger_from_purchase ON badge_ledger(from_purchase_id);

CREATE INDEX idx_badge_ledger_to_purchase ON badge_ledger(to_purchase_id);

CREATE TABLE issuances(
  issuance_id INTEGER PRIMARY KEY,
  badge_purchase_id INTEGER NOT NULL REFERENCES badge_purchases,
  period_start TEXT,
  period_end TEXT,
  expiry TEXT,
  entry_id INTEGER REFERENCES badge_ledger,
  credential BLOB NOT NULL,
  created_at TEXT NOT NULL,
  UNIQUE(badge_purchase_id, period_start)
);

CREATE TABLE codes(
  code_hash BLOB NOT NULL PRIMARY KEY,
  badge_type TEXT NOT NULL,
  months INTEGER,
  batch TEXT NOT NULL,
  redeemed_purchase_id INTEGER REFERENCES badge_purchases,
  redeemed_at TEXT,
  revoked_at TEXT,
  created_at TEXT NOT NULL
);

CREATE TABLE provider_events(
  provider TEXT NOT NULL,
  event_id TEXT NOT NULL,
  received_at TEXT NOT NULL,
  processed_at TEXT,
  PRIMARY KEY(provider, event_id)
);
