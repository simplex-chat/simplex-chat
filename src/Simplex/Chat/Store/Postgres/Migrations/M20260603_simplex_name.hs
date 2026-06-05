{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260603_simplex_name where

import Data.Text (Text)
import Text.RawString.QQ (r)

-- contacts.simplex_name and groups.simplex_name are the source of truth for the
-- entity's current name (updated when XInfo/XGrpInfo arrives).
--
-- connections.simplex_name is a TRANSIENT carrier for the connect-via-plan
-- (connect-by-name) path: when the user initiates a connection by typing
-- #name.simplex, the peer's profile is not yet available, so the name is
-- stashed on the connection row. When XInfo arrives and the Contact row is
-- created, the XInfo handler in Library/Subscriber.hs reads
-- connections.simplex_name and passes it to createDirectContact. After contact
-- creation, contacts.simplex_name is canonical and the connection's value
-- becomes a historical snapshot - it is intentionally never UPDATEd.
m20260603_simplex_name :: Text
m20260603_simplex_name =
  [r|
ALTER TABLE contacts ADD COLUMN simplex_name TEXT;
ALTER TABLE groups ADD COLUMN simplex_name TEXT;
ALTER TABLE connections ADD COLUMN simplex_name TEXT;

CREATE UNIQUE INDEX idx_contacts_simplex_name
  ON contacts(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL AND deleted = 0;

CREATE UNIQUE INDEX idx_groups_simplex_name
  ON groups(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;
|]

down_m20260603_simplex_name :: Text
down_m20260603_simplex_name =
  [r|
DROP INDEX idx_groups_simplex_name;
DROP INDEX idx_contacts_simplex_name;

ALTER TABLE connections DROP COLUMN simplex_name;
ALTER TABLE groups DROP COLUMN simplex_name;
ALTER TABLE contacts DROP COLUMN simplex_name;
|]
