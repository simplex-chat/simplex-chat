{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260921_wallet_seeds where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260921_wallet_seeds :: Query
m20260921_wallet_seeds =
  [sql|
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  entropy BLOB NOT NULL CHECK (length(entropy) = 32), -- BIP-39 entropy, 24 words
  next_account_index INTEGER CHECK (next_account_index BETWEEN 0 AND 2147483648),
  single_seed INTEGER NOT NULL DEFAULT 1
) STRICT;

CREATE TABLE wallet_accounts (
  wallet_account_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  account_index INTEGER CHECK (account_index BETWEEN 0 AND 2147483647),
  user_id INTEGER REFERENCES users ON DELETE SET NULL
) STRICT;

CREATE UNIQUE INDEX idx_wallet_seeds_single_seed ON wallet_seeds(single_seed);
CREATE UNIQUE INDEX idx_wallet_accounts_wallet_seed_id_account_index ON wallet_accounts(wallet_seed_id, account_index);
CREATE INDEX idx_wallet_accounts_user_id ON wallet_accounts(user_id);
|]

-- No reverse step: it would drop the only copy of the master entropy.
