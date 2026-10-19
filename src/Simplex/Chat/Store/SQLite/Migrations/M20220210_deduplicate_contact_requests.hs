{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220210_deduplicate_contact_requests where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220210_deduplicate_contact_requests :: Query
m20220210_deduplicate_contact_requests =
  [sql|
-- hash of contact address uri used by contact request sender to connect,
-- null for contact request recipient and for both parties when using one-off invitation
ALTER TABLE connections ADD COLUMN via_contact_uri_hash BLOB;
CREATE INDEX idx_connections_via_contact_uri_hash ON connections (via_contact_uri_hash);

ALTER TABLE connections ADD COLUMN xcontact_id BLOB;

ALTER TABLE contact_requests ADD COLUMN xcontact_id BLOB;
CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests (xcontact_id);

ALTER TABLE contacts ADD COLUMN xcontact_id BLOB;
CREATE INDEX idx_contacts_xcontact_id ON contacts (xcontact_id);

ALTER TABLE user_contact_links ADD column auto_accept INTEGER DEFAULT 0;
|]
