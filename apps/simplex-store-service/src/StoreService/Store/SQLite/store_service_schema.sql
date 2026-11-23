CREATE TABLE migrations(
  name TEXT NOT NULL PRIMARY KEY,
  ts TEXT NOT NULL,
  down TEXT
);
CREATE TABLE sx_badge_service_invoices(
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
  ,
  product TEXT NOT NULL DEFAULT 'badge',
  provider_ref TEXT NOT NULL DEFAULT ''
) STRICT;
CREATE TABLE sx_badge_service_payments(
  payment_id TEXT NOT NULL PRIMARY KEY,
  invoice_id TEXT REFERENCES sx_badge_service_invoices,
  provider TEXT NOT NULL,
  provider_ref TEXT,
  amount INTEGER,
  currency TEXT,
  status TEXT NOT NULL,
  exception TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
  ,
  crypto_paid TEXT,
  crypto_due TEXT,
  paid_in_full INTEGER NOT NULL DEFAULT 0,
  subscription_state TEXT,
  subscription_checked_at TEXT,
  provider_token TEXT,
  rebound_at TEXT,
  confirmed_amount INTEGER NOT NULL DEFAULT 0,
  received_units INTEGER,
  confirmed_units INTEGER,
  invoiced_units INTEGER,
  invoice_rate TEXT
) STRICT;
CREATE TABLE sx_badge_service_badge_prices(
  price_id TEXT NOT NULL PRIMARY KEY,
  badge_type TEXT NOT NULL,
  month_price INTEGER NOT NULL,
  currency TEXT NOT NULL,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_badge_offers(
  offer_id TEXT NOT NULL PRIMARY KEY,
  price_id TEXT REFERENCES sx_badge_service_badge_prices,
  months INTEGER NOT NULL,
  free_months INTEGER,
  discount INTEGER,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_badge_purchases(
  badge_purchase_id INTEGER PRIMARY KEY AUTOINCREMENT,
  purchase_key BLOB NOT NULL,
  master_key BLOB NOT NULL,
  initial_badge_type TEXT NOT NULL,
  current_badge_type TEXT NOT NULL,
  status TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  badge_code_id INTEGER REFERENCES sx_badge_service_badge_codes,
  payment_id TEXT REFERENCES sx_badge_service_payments,
  UNIQUE(purchase_key)
) STRICT;
CREATE TABLE sqlite_sequence(name,seq);
CREATE TABLE sx_badge_service_badge_ledger(
  entry_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entry_uuid TEXT NOT NULL,
  badge_purchase_id INTEGER NOT NULL REFERENCES sx_badge_service_badge_purchases ON DELETE CASCADE,
  change_months INTEGER NOT NULL,
  balance_months INTEGER NOT NULL,
  balance_start_ts TEXT NOT NULL,
  balance_anchor_ts TEXT NOT NULL,
  balance_badge_type TEXT NOT NULL,
  was_paused_since TEXT,
  service_created_at TEXT NOT NULL,
  created_at TEXT NOT NULL,
  entry_type TEXT NOT NULL,
  entry_credit_type TEXT,
  entry_debit_type TEXT
  ,
  payment_id TEXT REFERENCES sx_badge_service_payments,
  charge_id TEXT REFERENCES sx_badge_service_subscription_charges,
  from_purchase_id INTEGER REFERENCES sx_badge_service_badge_purchases,
  to_purchase_id INTEGER REFERENCES sx_badge_service_badge_purchases
) STRICT;
CREATE TABLE sx_badge_service_badge_issuances(
  issuance_id TEXT NOT NULL PRIMARY KEY,
  badge_purchase_id INTEGER NOT NULL REFERENCES sx_badge_service_badge_purchases ON DELETE CASCADE,
  entry_id INTEGER REFERENCES sx_badge_service_badge_ledger,
  badge_type TEXT NOT NULL,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  expiry TEXT NOT NULL,
  credential BLOB NOT NULL,
  created_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_badge_codes(
  badge_code_id INTEGER PRIMARY KEY AUTOINCREMENT,
  code_hash BLOB NOT NULL,
  badge_type TEXT NOT NULL,
  months INTEGER NOT NULL,
  code_payment_status TEXT NOT NULL,
  redeemed_at TEXT,
  expires_at TEXT,
  revoked_at TEXT,
  created_at TEXT NOT NULL,
  redeem_limit INTEGER NOT NULL DEFAULT 1,
  redeem_count INTEGER NOT NULL DEFAULT 0,
  group_item_id INTEGER,
  group_item_sent_at TEXT,
  UNIQUE(code_hash)
) STRICT;
CREATE TABLE sx_badge_service_badge_code_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES sx_badge_service_invoices ON DELETE CASCADE,
  badge_code_id INTEGER NOT NULL REFERENCES sx_badge_service_badge_codes,
  price_id TEXT NOT NULL REFERENCES sx_badge_service_badge_prices,
  offer_id TEXT REFERENCES sx_badge_service_badge_offers,
  provider_ref TEXT NOT NULL,
  created_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_group(
  group_id INTEGER NOT NULL PRIMARY KEY,
  group_link TEXT NOT NULL,
  owner_bootstrapped INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_subscription_charges(
  charge_id TEXT NOT NULL PRIMARY KEY,
  payment_id TEXT NOT NULL REFERENCES sx_badge_service_payments ON DELETE CASCADE,
  provider_charge_ref TEXT NOT NULL,
  plan_id TEXT NOT NULL,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  amount INTEGER,
  currency TEXT,
  created_at TEXT NOT NULL,
  UNIQUE(payment_id, provider_charge_ref)
) STRICT;
CREATE TABLE sx_badge_service_name_prepaid_codes(
  name_prepaid_code_id INTEGER PRIMARY KEY AUTOINCREMENT,
  code_hash BLOB NOT NULL,
  min_length INTEGER NOT NULL,
  years INTEGER NOT NULL,
  code_payment_status TEXT NOT NULL,
  reserved_at TEXT,
  spent_at TEXT,
  revoked_at TEXT,
  expires_at TEXT,
  created_at TEXT NOT NULL,
  issued_in_group INTEGER NOT NULL DEFAULT 0,
  reserved_by TEXT REFERENCES sx_badge_service_name_registrations(registration_id),
  UNIQUE(code_hash)
) STRICT;
CREATE TABLE sx_badge_service_name_registrations(
  name_registration_id INTEGER PRIMARY KEY,
  registration_id TEXT NOT NULL,
  version INTEGER NOT NULL,
  label_hash BLOB NOT NULL,
  length INTEGER NOT NULL,
  duration INTEGER NOT NULL,
  owner BLOB NOT NULL,
  commitment BLOB NOT NULL,
  sealed TEXT NOT NULL,
  agreed_terms TEXT NOT NULL,
  phase TEXT NOT NULL,
  years INTEGER NOT NULL,
  price INTEGER NOT NULL,
  method TEXT,
  name_prepaid_code_id INTEGER REFERENCES sx_badge_service_name_prepaid_codes(name_prepaid_code_id),
  dispute_days INTEGER NOT NULL,
  label TEXT,
  secret BLOB,
  contact_link TEXT,
  channel_link TEXT,
  intent_nonce INTEGER,
  intent_deadline INTEGER,
  intent_signature BLOB,
  accept_terms BLOB,
  accept_nonce INTEGER,
  accept_signature BLOB,
  commit_tx BLOB,
  commit_block INTEGER,
  register_tx BLOB,
  reveal_after INTEGER,
  reveal_before INTEGER,
  expires_at INTEGER,
  dispute_until INTEGER,
  registered_at TEXT,
  taken_at TEXT,
  failed_at TEXT,
  fail_reason TEXT,
  closed_by TEXT,
  live INTEGER NOT NULL,
  order_hash BLOB NOT NULL,
  settled_at TEXT,
  selected_at TEXT,
  unregistered_alerted_at TEXT,
  failed_dry_runs INTEGER NOT NULL DEFAULT 0,
  failed_sends INTEGER NOT NULL DEFAULT 0,
  last_attempt_tx TEXT,
  last_attempt_stage TEXT,
  last_attempt_reason TEXT,
  last_attempt_at TEXT,
  read_at TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  UNIQUE(registration_id)
) STRICT;
CREATE TABLE sx_badge_service_name_balances(
  registration_id TEXT NOT NULL PRIMARY KEY REFERENCES sx_badge_service_name_registrations(registration_id),
  amount INTEGER NOT NULL CHECK(amount >= 0),
  credited INTEGER NOT NULL DEFAULT 0 CHECK(credited >= 0),
  refunded INTEGER NOT NULL DEFAULT 0 CHECK(refunded >= 0),
  refunded_at TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_name_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES sx_badge_service_invoices(invoice_id) ON DELETE CASCADE,
  registration_id TEXT NOT NULL REFERENCES sx_badge_service_name_registrations(registration_id) ON DELETE CASCADE,
  late_alerted_at TEXT,
  late_alerted_units INTEGER,
  late_support TEXT,
  reversal_count INTEGER NOT NULL DEFAULT 0,
  last_reversal_event TEXT,
  last_reversal_cents INTEGER,
  reversed_cents INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_name_late_credits(
  name_late_credit_id INTEGER PRIMARY KEY AUTOINCREMENT,
  invoice_id TEXT NOT NULL REFERENCES sx_badge_service_invoices(invoice_id),
  credit_no INTEGER NOT NULL,
  registration_id TEXT NOT NULL REFERENCES sx_badge_service_name_registrations(registration_id),
  units INTEGER,
  invoice_rate TEXT,
  current_rate TEXT,
  rate_used TEXT,
  cents INTEGER NOT NULL CHECK(cents >= 0),
  created_at TEXT NOT NULL,
  UNIQUE(invoice_id, credit_no)
) STRICT;
CREATE TABLE sx_badge_service_name_record_edits(
  name_record_edit_id INTEGER PRIMARY KEY AUTOINCREMENT,
  label_hash BLOB NOT NULL,
  owner BLOB NOT NULL,
  nonce INTEGER NOT NULL,
  body_hash BLOB NOT NULL,
  state TEXT NOT NULL,
  revert TEXT,
  mined_tx BLOB,
  created_at TEXT NOT NULL
) STRICT;
CREATE TABLE sx_badge_service_name_record_changes(
  name_record_edit_id INTEGER NOT NULL REFERENCES sx_badge_service_name_record_edits(name_record_edit_id) ON DELETE CASCADE,
  nonce INTEGER NOT NULL,
  record_key TEXT NOT NULL,
  record_value TEXT NOT NULL,
  deadline INTEGER NOT NULL,
  signature BLOB NOT NULL,
  PRIMARY KEY(name_record_edit_id, nonce)
) STRICT;
CREATE TABLE sx_badge_service_name_transactions(
  name_transaction_id INTEGER PRIMARY KEY AUTOINCREMENT,
  registration_id TEXT REFERENCES sx_badge_service_name_registrations(registration_id),
  name_record_edit_id INTEGER REFERENCES sx_badge_service_name_record_edits(name_record_edit_id),
  kind TEXT NOT NULL,
  raw BLOB NOT NULL,
  tx_hash BLOB NOT NULL,
  registrar_nonce INTEGER NOT NULL,
  sent_at TEXT NOT NULL,
  mined_block INTEGER,
  status TEXT NOT NULL,
  CHECK((registration_id IS NULL) <>(name_record_edit_id IS NULL)),
  UNIQUE(tx_hash)
) STRICT;
CREATE TABLE sx_badge_service_name_terms(
  terms_hash BLOB NOT NULL PRIMARY KEY,
  terms_text BLOB NOT NULL
) STRICT;
CREATE INDEX sx_badge_service_idx_payments_invoice ON sx_badge_service_payments(
  invoice_id
);
CREATE INDEX sx_badge_service_idx_badge_offers_price ON sx_badge_service_badge_offers(
  price_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_badge_ledger_uuid ON sx_badge_service_badge_ledger(
  entry_uuid
);
CREATE INDEX sx_badge_service_idx_badge_ledger_purchase ON sx_badge_service_badge_ledger(
  badge_purchase_id,
  entry_id
);
CREATE INDEX sx_badge_service_idx_badge_issuances_purchase ON sx_badge_service_badge_issuances(
  badge_purchase_id,
  issuance_id
);
CREATE INDEX sx_badge_service_idx_badge_issuances_entry ON sx_badge_service_badge_issuances(
  entry_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_badge_issuances_purchase_entry ON sx_badge_service_badge_issuances(
  badge_purchase_id,
  entry_id
);
CREATE INDEX sx_badge_service_idx_badge_code_invoices_offer ON sx_badge_service_badge_code_invoices(
  offer_id
);
CREATE INDEX sx_badge_service_idx_badge_code_invoices_price ON sx_badge_service_badge_code_invoices(
  price_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_badge_code_invoices_provider_ref ON sx_badge_service_badge_code_invoices(
  provider_ref
);
CREATE INDEX sx_badge_service_idx_invoices_status_expires_at ON sx_badge_service_invoices(
  status,
  expires_at
);
CREATE INDEX sx_badge_service_idx_invoices_created ON sx_badge_service_invoices(
  created_at
);
CREATE INDEX sx_badge_service_idx_badge_purchases_code ON sx_badge_service_badge_purchases(
  badge_code_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_payments_provider_ref ON sx_badge_service_payments(
  provider,
  provider_ref
);
CREATE UNIQUE INDEX sx_badge_service_idx_badge_purchases_payment ON sx_badge_service_badge_purchases(
  payment_id
);
CREATE INDEX sx_badge_service_idx_badge_ledger_payment ON sx_badge_service_badge_ledger(
  payment_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_badge_ledger_charge ON sx_badge_service_badge_ledger(
  charge_id,
  badge_purchase_id
);
CREATE INDEX sx_badge_service_idx_badge_ledger_from_purchase ON sx_badge_service_badge_ledger(
  from_purchase_id
);
CREATE INDEX sx_badge_service_idx_badge_ledger_to_purchase ON sx_badge_service_badge_ledger(
  to_purchase_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_name_prepaid_codes_reserved_by ON sx_badge_service_name_prepaid_codes(
  reserved_by
);
CREATE UNIQUE INDEX sx_badge_service_idx_name_registrations_live_commitment ON sx_badge_service_name_registrations(
  commitment
) WHERE live = 1;
CREATE UNIQUE INDEX sx_badge_service_idx_name_registrations_live_owner_label ON sx_badge_service_name_registrations(
  owner,
  label_hash
) WHERE live = 1;
CREATE UNIQUE INDEX sx_badge_service_idx_name_registrations_held_label ON sx_badge_service_name_registrations(
  label_hash
) WHERE live = 1 AND phase <> 'unpaid';
CREATE INDEX sx_badge_service_idx_name_registrations_label_phase ON sx_badge_service_name_registrations(
  label_hash,
  phase
);
CREATE INDEX sx_badge_service_idx_name_registrations_owner ON sx_badge_service_name_registrations(
  owner,
  created_at
);
CREATE INDEX sx_badge_service_idx_name_registrations_phase ON sx_badge_service_name_registrations(
  phase,
  created_at
);
CREATE INDEX sx_badge_service_idx_name_registrations_order_hash ON sx_badge_service_name_registrations(
  order_hash
);
CREATE INDEX sx_badge_service_idx_name_registrations_prepaid_code ON sx_badge_service_name_registrations(
  name_prepaid_code_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_name_invoices_registration ON sx_badge_service_name_invoices(
  registration_id
);
CREATE INDEX sx_badge_service_idx_payments_overpaid ON sx_badge_service_payments(
  invoice_id
) WHERE received_units > invoiced_units;
CREATE INDEX sx_badge_service_idx_name_late_credits_registration ON sx_badge_service_name_late_credits(
  registration_id
);
CREATE UNIQUE INDEX sx_badge_service_idx_name_record_edits_owner_nonce ON sx_badge_service_name_record_edits(
  owner,
  nonce
) WHERE state IN(
  'pending',
  'mined'
);
CREATE INDEX sx_badge_service_idx_name_record_edits_body_hash ON sx_badge_service_name_record_edits(
  body_hash
);
CREATE INDEX sx_badge_service_idx_name_record_edits_label_created ON sx_badge_service_name_record_edits(
  label_hash,
  created_at
);
CREATE INDEX sx_badge_service_idx_name_transactions_registration ON sx_badge_service_name_transactions(
  registration_id
);
CREATE INDEX sx_badge_service_idx_name_transactions_edit ON sx_badge_service_name_transactions(
  name_record_edit_id
);
CREATE INDEX sx_badge_service_idx_name_transactions_status ON sx_badge_service_name_transactions(
  status,
  registrar_nonce
);
CREATE UNIQUE INDEX sx_badge_service_idx_invoices_provider_ref ON sx_badge_service_invoices(
  provider_ref
);
