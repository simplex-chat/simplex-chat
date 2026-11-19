DROP TABLE @name_terms;

DROP INDEX @idx_name_transactions_status;
DROP INDEX @idx_name_transactions_edit;
DROP INDEX @idx_name_transactions_registration;
DROP TABLE @name_transactions;

DROP TABLE @name_record_changes;

DROP INDEX @idx_name_record_edits_label_created;
DROP INDEX @idx_name_record_edits_body_hash;
DROP INDEX @idx_name_record_edits_owner_nonce;
DROP TABLE @name_record_edits;

DROP INDEX @idx_name_late_credits_registration;
DROP TABLE @name_late_credits;

DROP INDEX @idx_payments_overpaid;
ALTER TABLE @payments DROP COLUMN invoice_rate;
ALTER TABLE @payments DROP COLUMN invoiced_units;
ALTER TABLE @payments DROP COLUMN confirmed_units;
ALTER TABLE @payments DROP COLUMN received_units;
ALTER TABLE @payments DROP COLUMN confirmed_amount;

DELETE FROM @invoices WHERE invoice_id IN (
  SELECT ni.invoice_id FROM @name_invoices ni
  WHERE NOT EXISTS (SELECT 1 FROM @payments p WHERE p.invoice_id = ni.invoice_id)
);
DROP INDEX @idx_name_invoices_provider_ref;
DROP INDEX @idx_name_invoices_registration;
DROP TABLE @name_invoices;

DROP INDEX @idx_name_balances_reserved_by;
DROP TABLE @name_balances;

DROP INDEX @idx_name_registrations_balance_source;
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
