{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220210_deduplicate_contact_requests where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220210_deduplicate_contact_requests :: Query
m20220210_deduplicate_contact_requests =
  [sql|
-- contact address uri used by contact request sender to connect,
-- null for contact request recipient and for both parties when using one-off invitation
ALTER TABLE connections ADD COLUMN via_contact_uri BLOB;

ALTER TABLE connections ADD COLUMN xinfo_identifier BLOB;

ALTER TABLE contact_requests ADD COLUMN xinfo_identifier BLOB;
|]
