{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260923_wallet_owned_names where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260923_wallet_owned_names :: Query
m20260923_wallet_owned_names =
  [sql|
CREATE TABLE wallet_owned_names (
  wallet_owned_name_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  account_index INTEGER NOT NULL CHECK (account_index BETWEEN 0 AND 2147483647),
  name TEXT NOT NULL,
  name_response TEXT NOT NULL -- the NameResponse a scan last read for this name
) STRICT;

CREATE UNIQUE INDEX idx_wallet_owned_names_wallet_seed_id_name ON wallet_owned_names(wallet_seed_id, name);
|]

-- No reverse step: wallet_seeds, which the rows hang off, has none either.
