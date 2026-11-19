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
  issued_in_group SMALLINT NOT NULL DEFAULT 0,
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
  agreed_terms TEXT NOT NULL,
  phase TEXT NOT NULL,
  years INTEGER NOT NULL,
  price INTEGER NOT NULL,
  method TEXT,
  name_prepaid_code_id BIGINT REFERENCES @name_prepaid_codes,
  balance_source TEXT REFERENCES @name_registrations,
  balance_used INTEGER NOT NULL,
  label TEXT,
  secret BYTEA,
  contact_link TEXT,
  channel_link TEXT,
  intent_nonce BIGINT,
  intent_deadline BIGINT,
  intent_signature BYTEA,
  accept_terms BYTEA,
  accept_nonce BIGINT,
  accept_signature BYTEA,
  commit_tx BYTEA,
  commit_block BIGINT,
  register_tx BYTEA,
  register_block BIGINT,
  reveal_after BIGINT,
  reveal_before BIGINT,
  expires_at BIGINT,
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

CREATE INDEX @idx_name_registrations_balance_source ON @name_registrations(balance_source);

CREATE TABLE @name_balances(
  registration_id TEXT NOT NULL PRIMARY KEY REFERENCES @name_registrations,
  amount INTEGER NOT NULL CHECK (amount >= 0),
  credited INTEGER NOT NULL DEFAULT 0 CHECK (credited >= 0),
  refunded INTEGER NOT NULL DEFAULT 0 CHECK (refunded >= 0),
  reserved_by TEXT REFERENCES @name_registrations,
  refunded_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX @idx_name_balances_reserved_by ON @name_balances(reserved_by);

-- late_support says why a closed order's late money will not be credited by itself:
-- high_speed, units, rate_floor, unconfirmed or no_confirmation. It is never cleared.
CREATE TABLE @name_invoices(
  invoice_id TEXT NOT NULL PRIMARY KEY REFERENCES @invoices ON DELETE CASCADE,
  registration_id TEXT NOT NULL REFERENCES @name_registrations ON DELETE CASCADE,
  provider_ref TEXT NOT NULL,
  late_alerted_at TIMESTAMPTZ,
  late_alerted_units BIGINT,
  late_support TEXT,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX @idx_name_invoices_registration ON @name_invoices(registration_id);

CREATE UNIQUE INDEX @idx_name_invoices_provider_ref ON @name_invoices(provider_ref);

ALTER TABLE @payments ADD COLUMN confirmed_amount INTEGER NOT NULL DEFAULT 0;

ALTER TABLE @payments ADD COLUMN received_units BIGINT;

ALTER TABLE @payments ADD COLUMN confirmed_units BIGINT;

ALTER TABLE @payments ADD COLUMN invoiced_units BIGINT;

ALTER TABLE @payments ADD COLUMN invoice_rate TEXT;

-- The poller's credit pass reads the overpaid payments on every pass.
CREATE INDEX @idx_payments_overpaid ON @payments(invoice_id) WHERE received_units > invoiced_units;

CREATE TABLE @name_late_credits(
  name_late_credit_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  invoice_id TEXT NOT NULL REFERENCES @invoices,
  credit_no INTEGER NOT NULL,
  registration_id TEXT NOT NULL REFERENCES @name_registrations,
  units BIGINT,
  invoice_rate TEXT,
  current_rate TEXT,
  rate_used TEXT,
  cents INTEGER NOT NULL CHECK (cents >= 0),
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE(invoice_id, credit_no)
);

CREATE INDEX @idx_name_late_credits_registration ON @name_late_credits(registration_id);

CREATE TABLE @name_record_edits(
  name_record_edit_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  label_hash BYTEA NOT NULL,
  owner BYTEA NOT NULL,
  nonce BIGINT NOT NULL,
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

-- An edit is one request of the page, relayed as one transaction; its signed changes are its rows here, one per nonce.
CREATE TABLE @name_record_changes(
  name_record_edit_id BIGINT NOT NULL REFERENCES @name_record_edits ON DELETE CASCADE,
  nonce BIGINT NOT NULL,
  record_key TEXT,
  record_value TEXT,
  deadline BIGINT NOT NULL,
  signature BYTEA NOT NULL,
  PRIMARY KEY (name_record_edit_id, nonce)
);

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

CREATE TABLE @name_terms(
  terms_hash BYTEA NOT NULL PRIMARY KEY,
  terms_text BYTEA NOT NULL
);
