


SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


CREATE SCHEMA test_store_service_schema;


SET default_table_access_method = heap;


CREATE TABLE test_store_service_schema.migrations (
    name text NOT NULL,
    ts timestamp without time zone NOT NULL,
    down text
);



CREATE TABLE test_store_service_schema.sx_badge_service_badge_code_invoices (
    invoice_id text NOT NULL,
    badge_code_id bigint NOT NULL,
    price_id text NOT NULL,
    offer_id text,
    provider_ref text NOT NULL,
    created_at timestamp with time zone NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_badge_codes (
    badge_code_id bigint NOT NULL,
    code_hash bytea NOT NULL,
    badge_type text NOT NULL,
    months smallint NOT NULL,
    code_payment_status text NOT NULL,
    redeemed_at timestamp with time zone,
    expires_at timestamp with time zone,
    revoked_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    redeem_limit integer DEFAULT 1 NOT NULL,
    redeem_count integer DEFAULT 0 NOT NULL,
    group_item_id bigint,
    group_item_sent_at timestamp with time zone
);



ALTER TABLE test_store_service_schema.sx_badge_service_badge_codes ALTER COLUMN badge_code_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_badge_codes_badge_code_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_badge_issuances (
    issuance_id text NOT NULL,
    badge_purchase_id bigint NOT NULL,
    entry_id bigint,
    badge_type text NOT NULL,
    period_start timestamp with time zone NOT NULL,
    period_end timestamp with time zone NOT NULL,
    expiry timestamp with time zone NOT NULL,
    credential bytea NOT NULL,
    created_at timestamp with time zone NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_badge_ledger (
    entry_id bigint NOT NULL,
    entry_uuid text NOT NULL,
    badge_purchase_id bigint NOT NULL,
    change_months smallint NOT NULL,
    balance_months smallint NOT NULL,
    balance_start_ts timestamp with time zone NOT NULL,
    balance_anchor_ts timestamp with time zone NOT NULL,
    balance_badge_type text NOT NULL,
    was_paused_since timestamp with time zone,
    service_created_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL,
    entry_type text NOT NULL,
    entry_credit_type text,
    entry_debit_type text,
    payment_id text,
    charge_id text,
    from_purchase_id bigint,
    to_purchase_id bigint
);



ALTER TABLE test_store_service_schema.sx_badge_service_badge_ledger ALTER COLUMN entry_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_badge_ledger_entry_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_badge_offers (
    offer_id text NOT NULL,
    price_id text,
    months smallint NOT NULL,
    free_months smallint,
    discount smallint,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_badge_prices (
    price_id text NOT NULL,
    badge_type text NOT NULL,
    month_price bigint NOT NULL,
    currency text NOT NULL,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_badge_purchases (
    badge_purchase_id bigint NOT NULL,
    purchase_key bytea NOT NULL,
    master_key bytea NOT NULL,
    initial_badge_type text NOT NULL,
    current_badge_type text NOT NULL,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    badge_code_id bigint,
    payment_id text
);



ALTER TABLE test_store_service_schema.sx_badge_service_badge_purchases ALTER COLUMN badge_purchase_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_badge_purchases_badge_purchase_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_group (
    group_id bigint NOT NULL,
    group_link text NOT NULL,
    owner_bootstrapped smallint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_invoices (
    invoice_id text NOT NULL,
    provider text NOT NULL,
    price bigint NOT NULL,
    discount_amount bigint,
    credit_amount bigint,
    amount bigint NOT NULL,
    currency text NOT NULL,
    payment_url text,
    payment_address text,
    payment_crypto_currency text,
    payment_crypto_amount text,
    expires_at timestamp with time zone NOT NULL,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    product text DEFAULT 'badge'::text NOT NULL,
    provider_ref text NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_balances (
    registration_id text NOT NULL,
    amount integer NOT NULL,
    credited integer DEFAULT 0 NOT NULL,
    refunded integer DEFAULT 0 NOT NULL,
    reserved_by text,
    refunded_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    CONSTRAINT sx_badge_service_name_balances_amount_check CHECK ((amount >= 0)),
    CONSTRAINT sx_badge_service_name_balances_credited_check CHECK ((credited >= 0)),
    CONSTRAINT sx_badge_service_name_balances_refunded_check CHECK ((refunded >= 0))
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_invoices (
    invoice_id text NOT NULL,
    registration_id text NOT NULL,
    late_alerted_at timestamp with time zone,
    late_alerted_units bigint,
    late_support text,
    reversal_count integer DEFAULT 0 NOT NULL,
    last_reversal_event text,
    last_reversal_kind text,
    last_reversal_at timestamp with time zone,
    last_reversal_cents integer,
    reversed_cents integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_late_credits (
    name_late_credit_id bigint NOT NULL,
    invoice_id text NOT NULL,
    credit_no integer NOT NULL,
    registration_id text NOT NULL,
    units bigint,
    invoice_rate text,
    current_rate text,
    rate_used text,
    cents integer NOT NULL,
    created_at timestamp with time zone NOT NULL,
    CONSTRAINT sx_badge_service_name_late_credits_cents_check CHECK ((cents >= 0))
);



ALTER TABLE test_store_service_schema.sx_badge_service_name_late_credits ALTER COLUMN name_late_credit_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_name_late_credits_name_late_credit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_prepaid_codes (
    name_prepaid_code_id bigint NOT NULL,
    code_hash bytea NOT NULL,
    min_length integer NOT NULL,
    years integer NOT NULL,
    code_payment_status text NOT NULL,
    reserved_at timestamp with time zone,
    spent_at timestamp with time zone,
    revoked_at timestamp with time zone,
    expires_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    issued_in_group smallint DEFAULT 0 NOT NULL,
    reserved_by text
);



ALTER TABLE test_store_service_schema.sx_badge_service_name_prepaid_codes ALTER COLUMN name_prepaid_code_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_name_prepaid_codes_name_prepaid_code_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_record_changes (
    name_record_edit_id bigint NOT NULL,
    nonce bigint NOT NULL,
    record_key text NOT NULL,
    record_value text NOT NULL,
    deadline bigint NOT NULL,
    signature bytea NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_record_edits (
    name_record_edit_id bigint NOT NULL,
    label_hash bytea NOT NULL,
    owner bytea NOT NULL,
    nonce bigint NOT NULL,
    body_hash bytea NOT NULL,
    state text NOT NULL,
    revert text,
    mined_tx bytea,
    created_at timestamp with time zone NOT NULL
);



ALTER TABLE test_store_service_schema.sx_badge_service_name_record_edits ALTER COLUMN name_record_edit_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_name_record_edits_name_record_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_registrations (
    registration_id text NOT NULL,
    version bigint NOT NULL,
    label_hash bytea NOT NULL,
    length integer NOT NULL,
    duration bigint NOT NULL,
    owner bytea NOT NULL,
    commitment bytea NOT NULL,
    sealed text NOT NULL,
    agreed_terms text NOT NULL,
    phase text NOT NULL,
    years integer NOT NULL,
    price integer NOT NULL,
    method text,
    name_prepaid_code_id bigint,
    balance_source text,
    balance_used integer NOT NULL,
    label text,
    secret bytea,
    contact_link text,
    channel_link text,
    intent_nonce bigint,
    intent_deadline bigint,
    intent_signature bytea,
    accept_terms bytea,
    accept_nonce bigint,
    accept_signature bytea,
    commit_tx bytea,
    commit_block bigint,
    register_tx bytea,
    reveal_after bigint,
    reveal_before bigint,
    expires_at bigint,
    registered_at timestamp with time zone,
    failed_at timestamp with time zone,
    fail_reason text,
    closed_by text,
    live smallint NOT NULL,
    order_hash bytea NOT NULL,
    moves integer DEFAULT 0 NOT NULL,
    links_refused_at timestamp with time zone,
    links_refused_kind text,
    paid_at timestamp with time zone,
    unregistered_alerted_at timestamp with time zone,
    failed_dry_runs integer DEFAULT 0 NOT NULL,
    failed_sends integer DEFAULT 0 NOT NULL,
    last_attempt_tx text,
    last_attempt_stage text,
    last_attempt_reason text,
    last_attempt_at timestamp with time zone,
    read_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    created_seq bigint NOT NULL
);



ALTER TABLE test_store_service_schema.sx_badge_service_name_registrations ALTER COLUMN created_seq ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_name_registrations_created_seq_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_terms (
    terms_hash bytea NOT NULL,
    terms_text bytea NOT NULL
);



CREATE TABLE test_store_service_schema.sx_badge_service_name_transactions (
    name_transaction_id bigint NOT NULL,
    registration_id text,
    name_record_edit_id bigint,
    kind text NOT NULL,
    raw bytea NOT NULL,
    tx_hash bytea NOT NULL,
    registrar_nonce bigint NOT NULL,
    sent_at timestamp with time zone NOT NULL,
    mined_block bigint,
    status text NOT NULL,
    CONSTRAINT sx_badge_service_name_transactions_check CHECK (((registration_id IS NULL) <> (name_record_edit_id IS NULL)))
);



ALTER TABLE test_store_service_schema.sx_badge_service_name_transactions ALTER COLUMN name_transaction_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_store_service_schema.sx_badge_service_name_transactions_name_transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_store_service_schema.sx_badge_service_payments (
    payment_id text NOT NULL,
    invoice_id text,
    provider text NOT NULL,
    provider_ref text,
    amount bigint,
    currency text,
    status text NOT NULL,
    exception text,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    crypto_paid text,
    crypto_due text,
    paid_in_full smallint DEFAULT 0 NOT NULL,
    subscription_state text,
    subscription_checked_at timestamp with time zone,
    provider_token text,
    rebound_at timestamp with time zone,
    confirmed_amount integer DEFAULT 0 NOT NULL,
    received_units bigint,
    confirmed_units bigint,
    invoiced_units bigint,
    invoice_rate text
);



CREATE TABLE test_store_service_schema.sx_badge_service_subscription_charges (
    charge_id text NOT NULL,
    payment_id text NOT NULL,
    provider_charge_ref text NOT NULL,
    plan_id text NOT NULL,
    period_start timestamp with time zone NOT NULL,
    period_end timestamp with time zone NOT NULL,
    amount bigint,
    currency text,
    created_at timestamp with time zone NOT NULL
);



ALTER TABLE ONLY test_store_service_schema.migrations
    ADD CONSTRAINT migrations_pkey PRIMARY KEY (name);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_code_invoices
    ADD CONSTRAINT sx_badge_service_badge_code_invoices_pkey PRIMARY KEY (invoice_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_codes
    ADD CONSTRAINT sx_badge_service_badge_codes_code_hash_key UNIQUE (code_hash);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_codes
    ADD CONSTRAINT sx_badge_service_badge_codes_pkey PRIMARY KEY (badge_code_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_issuances
    ADD CONSTRAINT sx_badge_service_badge_issuances_pkey PRIMARY KEY (issuance_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_ledger
    ADD CONSTRAINT sx_badge_service_badge_ledger_pkey PRIMARY KEY (entry_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_offers
    ADD CONSTRAINT sx_badge_service_badge_offers_pkey PRIMARY KEY (offer_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_prices
    ADD CONSTRAINT sx_badge_service_badge_prices_pkey PRIMARY KEY (price_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_purchases
    ADD CONSTRAINT sx_badge_service_badge_purchases_pkey PRIMARY KEY (badge_purchase_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_purchases
    ADD CONSTRAINT sx_badge_service_badge_purchases_purchase_key_key UNIQUE (purchase_key);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_group
    ADD CONSTRAINT sx_badge_service_group_pkey PRIMARY KEY (group_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_invoices
    ADD CONSTRAINT sx_badge_service_invoices_pkey PRIMARY KEY (invoice_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_balances
    ADD CONSTRAINT sx_badge_service_name_balances_pkey PRIMARY KEY (registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_invoices
    ADD CONSTRAINT sx_badge_service_name_invoices_pkey PRIMARY KEY (invoice_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_late_credits
    ADD CONSTRAINT sx_badge_service_name_late_credits_invoice_id_credit_no_key UNIQUE (invoice_id, credit_no);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_late_credits
    ADD CONSTRAINT sx_badge_service_name_late_credits_pkey PRIMARY KEY (name_late_credit_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_prepaid_codes
    ADD CONSTRAINT sx_badge_service_name_prepaid_codes_code_hash_key UNIQUE (code_hash);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_prepaid_codes
    ADD CONSTRAINT sx_badge_service_name_prepaid_codes_pkey PRIMARY KEY (name_prepaid_code_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_record_changes
    ADD CONSTRAINT sx_badge_service_name_record_changes_pkey PRIMARY KEY (name_record_edit_id, nonce);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_record_edits
    ADD CONSTRAINT sx_badge_service_name_record_edits_pkey PRIMARY KEY (name_record_edit_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_registrations
    ADD CONSTRAINT sx_badge_service_name_registrations_pkey PRIMARY KEY (registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_terms
    ADD CONSTRAINT sx_badge_service_name_terms_pkey PRIMARY KEY (terms_hash);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_transactions
    ADD CONSTRAINT sx_badge_service_name_transactions_pkey PRIMARY KEY (name_transaction_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_transactions
    ADD CONSTRAINT sx_badge_service_name_transactions_tx_hash_key UNIQUE (tx_hash);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_payments
    ADD CONSTRAINT sx_badge_service_payments_pkey PRIMARY KEY (payment_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_subscription_charges
    ADD CONSTRAINT sx_badge_service_subscription_charges_pkey PRIMARY KEY (charge_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_subscription_charges
    ADD CONSTRAINT sx_badge_service_subscription_payment_id_provider_charge_re_key UNIQUE (payment_id, provider_charge_ref);



CREATE INDEX sx_badge_service_idx_badge_code_invoices_offer ON test_store_service_schema.sx_badge_service_badge_code_invoices USING btree (offer_id);



CREATE INDEX sx_badge_service_idx_badge_code_invoices_price ON test_store_service_schema.sx_badge_service_badge_code_invoices USING btree (price_id);



CREATE UNIQUE INDEX sx_badge_service_idx_badge_code_invoices_provider_ref ON test_store_service_schema.sx_badge_service_badge_code_invoices USING btree (provider_ref);



CREATE INDEX sx_badge_service_idx_badge_issuances_entry ON test_store_service_schema.sx_badge_service_badge_issuances USING btree (entry_id);



CREATE INDEX sx_badge_service_idx_badge_issuances_purchase ON test_store_service_schema.sx_badge_service_badge_issuances USING btree (badge_purchase_id, issuance_id);



CREATE UNIQUE INDEX sx_badge_service_idx_badge_issuances_purchase_entry ON test_store_service_schema.sx_badge_service_badge_issuances USING btree (badge_purchase_id, entry_id);



CREATE UNIQUE INDEX sx_badge_service_idx_badge_ledger_charge ON test_store_service_schema.sx_badge_service_badge_ledger USING btree (charge_id, badge_purchase_id);



CREATE INDEX sx_badge_service_idx_badge_ledger_from_purchase ON test_store_service_schema.sx_badge_service_badge_ledger USING btree (from_purchase_id);



CREATE INDEX sx_badge_service_idx_badge_ledger_payment ON test_store_service_schema.sx_badge_service_badge_ledger USING btree (payment_id);



CREATE INDEX sx_badge_service_idx_badge_ledger_purchase ON test_store_service_schema.sx_badge_service_badge_ledger USING btree (badge_purchase_id, entry_id);



CREATE INDEX sx_badge_service_idx_badge_ledger_to_purchase ON test_store_service_schema.sx_badge_service_badge_ledger USING btree (to_purchase_id);



CREATE UNIQUE INDEX sx_badge_service_idx_badge_ledger_uuid ON test_store_service_schema.sx_badge_service_badge_ledger USING btree (entry_uuid);



CREATE INDEX sx_badge_service_idx_badge_offers_price ON test_store_service_schema.sx_badge_service_badge_offers USING btree (price_id);



CREATE INDEX sx_badge_service_idx_badge_purchases_code ON test_store_service_schema.sx_badge_service_badge_purchases USING btree (badge_code_id);



CREATE UNIQUE INDEX sx_badge_service_idx_badge_purchases_payment ON test_store_service_schema.sx_badge_service_badge_purchases USING btree (payment_id);



CREATE INDEX sx_badge_service_idx_invoices_created ON test_store_service_schema.sx_badge_service_invoices USING btree (created_at);



CREATE UNIQUE INDEX sx_badge_service_idx_invoices_provider_ref ON test_store_service_schema.sx_badge_service_invoices USING btree (provider_ref);



CREATE INDEX sx_badge_service_idx_invoices_status_expires_at ON test_store_service_schema.sx_badge_service_invoices USING btree (status, expires_at);



CREATE UNIQUE INDEX sx_badge_service_idx_name_balances_reserved_by ON test_store_service_schema.sx_badge_service_name_balances USING btree (reserved_by);



CREATE UNIQUE INDEX sx_badge_service_idx_name_invoices_registration ON test_store_service_schema.sx_badge_service_name_invoices USING btree (registration_id);



CREATE INDEX sx_badge_service_idx_name_late_credits_registration ON test_store_service_schema.sx_badge_service_name_late_credits USING btree (registration_id);



CREATE UNIQUE INDEX sx_badge_service_idx_name_prepaid_codes_reserved_by ON test_store_service_schema.sx_badge_service_name_prepaid_codes USING btree (reserved_by);



CREATE INDEX sx_badge_service_idx_name_record_edits_body_hash ON test_store_service_schema.sx_badge_service_name_record_edits USING btree (body_hash);



CREATE INDEX sx_badge_service_idx_name_record_edits_label_created ON test_store_service_schema.sx_badge_service_name_record_edits USING btree (label_hash, created_at);



CREATE UNIQUE INDEX sx_badge_service_idx_name_record_edits_owner_nonce ON test_store_service_schema.sx_badge_service_name_record_edits USING btree (owner, nonce) WHERE (state = ANY (ARRAY['pending'::text, 'mined'::text]));



CREATE INDEX sx_badge_service_idx_name_registrations_balance_source ON test_store_service_schema.sx_badge_service_name_registrations USING btree (balance_source);



CREATE UNIQUE INDEX sx_badge_service_idx_name_registrations_held_label ON test_store_service_schema.sx_badge_service_name_registrations USING btree (label_hash) WHERE ((live = 1) AND (phase <> 'unpaid'::text));



CREATE INDEX sx_badge_service_idx_name_registrations_label_phase ON test_store_service_schema.sx_badge_service_name_registrations USING btree (label_hash, phase);



CREATE UNIQUE INDEX sx_badge_service_idx_name_registrations_live_commitment ON test_store_service_schema.sx_badge_service_name_registrations USING btree (commitment) WHERE (live = 1);



CREATE UNIQUE INDEX sx_badge_service_idx_name_registrations_live_owner_label ON test_store_service_schema.sx_badge_service_name_registrations USING btree (owner, label_hash) WHERE (live = 1);



CREATE INDEX sx_badge_service_idx_name_registrations_order_hash ON test_store_service_schema.sx_badge_service_name_registrations USING btree (order_hash);



CREATE INDEX sx_badge_service_idx_name_registrations_owner ON test_store_service_schema.sx_badge_service_name_registrations USING btree (owner, created_at);



CREATE INDEX sx_badge_service_idx_name_registrations_phase ON test_store_service_schema.sx_badge_service_name_registrations USING btree (phase, created_at);



CREATE INDEX sx_badge_service_idx_name_registrations_prepaid_code ON test_store_service_schema.sx_badge_service_name_registrations USING btree (name_prepaid_code_id);



CREATE INDEX sx_badge_service_idx_name_transactions_edit ON test_store_service_schema.sx_badge_service_name_transactions USING btree (name_record_edit_id);



CREATE INDEX sx_badge_service_idx_name_transactions_registration ON test_store_service_schema.sx_badge_service_name_transactions USING btree (registration_id);



CREATE INDEX sx_badge_service_idx_name_transactions_status ON test_store_service_schema.sx_badge_service_name_transactions USING btree (status, registrar_nonce);



CREATE INDEX sx_badge_service_idx_payments_invoice ON test_store_service_schema.sx_badge_service_payments USING btree (invoice_id);



CREATE INDEX sx_badge_service_idx_payments_overpaid ON test_store_service_schema.sx_badge_service_payments USING btree (invoice_id) WHERE (received_units > invoiced_units);



CREATE UNIQUE INDEX sx_badge_service_idx_payments_provider_ref ON test_store_service_schema.sx_badge_service_payments USING btree (provider, provider_ref);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_code_invoices
    ADD CONSTRAINT sx_badge_service_badge_code_invoices_badge_code_id_fkey FOREIGN KEY (badge_code_id) REFERENCES test_store_service_schema.sx_badge_service_badge_codes(badge_code_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_code_invoices
    ADD CONSTRAINT sx_badge_service_badge_code_invoices_invoice_id_fkey FOREIGN KEY (invoice_id) REFERENCES test_store_service_schema.sx_badge_service_invoices(invoice_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_code_invoices
    ADD CONSTRAINT sx_badge_service_badge_code_invoices_offer_id_fkey FOREIGN KEY (offer_id) REFERENCES test_store_service_schema.sx_badge_service_badge_offers(offer_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_code_invoices
    ADD CONSTRAINT sx_badge_service_badge_code_invoices_price_id_fkey FOREIGN KEY (price_id) REFERENCES test_store_service_schema.sx_badge_service_badge_prices(price_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_issuances
    ADD CONSTRAINT sx_badge_service_badge_issuances_badge_purchase_id_fkey FOREIGN KEY (badge_purchase_id) REFERENCES test_store_service_schema.sx_badge_service_badge_purchases(badge_purchase_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_issuances
    ADD CONSTRAINT sx_badge_service_badge_issuances_entry_id_fkey FOREIGN KEY (entry_id) REFERENCES test_store_service_schema.sx_badge_service_badge_ledger(entry_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_ledger
    ADD CONSTRAINT sx_badge_service_badge_ledger_badge_purchase_id_fkey FOREIGN KEY (badge_purchase_id) REFERENCES test_store_service_schema.sx_badge_service_badge_purchases(badge_purchase_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_ledger
    ADD CONSTRAINT sx_badge_service_badge_ledger_charge_id_fkey FOREIGN KEY (charge_id) REFERENCES test_store_service_schema.sx_badge_service_subscription_charges(charge_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_ledger
    ADD CONSTRAINT sx_badge_service_badge_ledger_from_purchase_id_fkey FOREIGN KEY (from_purchase_id) REFERENCES test_store_service_schema.sx_badge_service_badge_purchases(badge_purchase_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_ledger
    ADD CONSTRAINT sx_badge_service_badge_ledger_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES test_store_service_schema.sx_badge_service_payments(payment_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_ledger
    ADD CONSTRAINT sx_badge_service_badge_ledger_to_purchase_id_fkey FOREIGN KEY (to_purchase_id) REFERENCES test_store_service_schema.sx_badge_service_badge_purchases(badge_purchase_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_offers
    ADD CONSTRAINT sx_badge_service_badge_offers_price_id_fkey FOREIGN KEY (price_id) REFERENCES test_store_service_schema.sx_badge_service_badge_prices(price_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_purchases
    ADD CONSTRAINT sx_badge_service_badge_purchases_badge_code_id_fkey FOREIGN KEY (badge_code_id) REFERENCES test_store_service_schema.sx_badge_service_badge_codes(badge_code_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_badge_purchases
    ADD CONSTRAINT sx_badge_service_badge_purchases_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES test_store_service_schema.sx_badge_service_payments(payment_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_balances
    ADD CONSTRAINT sx_badge_service_name_balances_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES test_store_service_schema.sx_badge_service_name_registrations(registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_balances
    ADD CONSTRAINT sx_badge_service_name_balances_reserved_by_fkey FOREIGN KEY (reserved_by) REFERENCES test_store_service_schema.sx_badge_service_name_registrations(registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_invoices
    ADD CONSTRAINT sx_badge_service_name_invoices_invoice_id_fkey FOREIGN KEY (invoice_id) REFERENCES test_store_service_schema.sx_badge_service_invoices(invoice_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_invoices
    ADD CONSTRAINT sx_badge_service_name_invoices_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES test_store_service_schema.sx_badge_service_name_registrations(registration_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_late_credits
    ADD CONSTRAINT sx_badge_service_name_late_credits_invoice_id_fkey FOREIGN KEY (invoice_id) REFERENCES test_store_service_schema.sx_badge_service_invoices(invoice_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_late_credits
    ADD CONSTRAINT sx_badge_service_name_late_credits_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES test_store_service_schema.sx_badge_service_name_registrations(registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_prepaid_codes
    ADD CONSTRAINT sx_badge_service_name_prepaid_codes_reserved_by_fkey FOREIGN KEY (reserved_by) REFERENCES test_store_service_schema.sx_badge_service_name_registrations(registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_record_changes
    ADD CONSTRAINT sx_badge_service_name_record_changes_name_record_edit_id_fkey FOREIGN KEY (name_record_edit_id) REFERENCES test_store_service_schema.sx_badge_service_name_record_edits(name_record_edit_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_registrations
    ADD CONSTRAINT sx_badge_service_name_registrations_balance_source_fkey FOREIGN KEY (balance_source) REFERENCES test_store_service_schema.sx_badge_service_name_registrations(registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_registrations
    ADD CONSTRAINT sx_badge_service_name_registrations_name_prepaid_code_id_fkey FOREIGN KEY (name_prepaid_code_id) REFERENCES test_store_service_schema.sx_badge_service_name_prepaid_codes(name_prepaid_code_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_transactions
    ADD CONSTRAINT sx_badge_service_name_transactions_name_record_edit_id_fkey FOREIGN KEY (name_record_edit_id) REFERENCES test_store_service_schema.sx_badge_service_name_record_edits(name_record_edit_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_name_transactions
    ADD CONSTRAINT sx_badge_service_name_transactions_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES test_store_service_schema.sx_badge_service_name_registrations(registration_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_payments
    ADD CONSTRAINT sx_badge_service_payments_invoice_id_fkey FOREIGN KEY (invoice_id) REFERENCES test_store_service_schema.sx_badge_service_invoices(invoice_id);



ALTER TABLE ONLY test_store_service_schema.sx_badge_service_subscription_charges
    ADD CONSTRAINT sx_badge_service_subscription_charges_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES test_store_service_schema.sx_badge_service_payments(payment_id) ON DELETE CASCADE;




