{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250721_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250721_indexes :: Query
m20250721_indexes =
  [sql|
DROP INDEX idx_contact_requests_xcontact_id;

CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests(user_id, xcontact_id);
|]

down_m20250721_indexes :: Query
down_m20250721_indexes =
  [sql|
DROP INDEX idx_contact_requests_xcontact_id;

CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests(xcontact_id);
|]
