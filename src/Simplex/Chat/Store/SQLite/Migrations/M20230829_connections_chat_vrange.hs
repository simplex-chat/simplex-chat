{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230829_connections_chat_vrange where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230829_connections_chat_vrange :: Query
m20230829_connections_chat_vrange =
  [sql|
ALTER TABLE connections ADD COLUMN peer_chat_min_version INTEGER NOT NULL DEFAULT 1;
ALTER TABLE connections ADD COLUMN peer_chat_max_version INTEGER NOT NULL DEFAULT 1;

ALTER TABLE contact_requests ADD COLUMN peer_chat_min_version INTEGER NOT NULL DEFAULT 1;
ALTER TABLE contact_requests ADD COLUMN peer_chat_max_version INTEGER NOT NULL DEFAULT 1;
|]

down_m20230829_connections_chat_vrange :: Query
down_m20230829_connections_chat_vrange =
  [sql|
ALTER TABLE contact_requests DROP COLUMN peer_chat_max_version;
ALTER TABLE contact_requests DROP COLUMN peer_chat_min_version;

ALTER TABLE connections DROP COLUMN peer_chat_max_version;
ALTER TABLE connections DROP COLUMN peer_chat_min_version;
|]
