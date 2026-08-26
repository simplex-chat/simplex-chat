{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260826_wallet_name_keys where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- | One key per name, at BIP-44 address index k under the profile's account.
--
-- Which key owns which name is recorded nowhere else: it is not on chain, and a
-- name found by a future recovery scan may sit on a layout that is not ours (a
-- name bought in a dapp is typically at the master key, with no derivation at
-- all). So the path is stored literally rather than as indices.
m20260826_wallet_name_keys :: Query
m20260826_wallet_name_keys =
  [sql|
CREATE TABLE wallet_name_keys (
  wallet_name_key_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE RESTRICT,
  derivation_path TEXT NOT NULL,            -- "m/44'/60'/0'/0/1", or "m" for a root key
  name TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (wallet_seed_id, derivation_path),
  UNIQUE (wallet_seed_id, name)
) STRICT;

-- required: the .lint check enforces an index on every foreign key
CREATE INDEX idx_wallet_name_keys_wallet_seed_id ON wallet_name_keys(wallet_seed_id);

-- High-water mark for k, per profile. Same reason as next_account_index: after
-- recovery from the phrase alone nothing else knows which indices are taken.
ALTER TABLE users ADD COLUMN wallet_next_name_index INTEGER NOT NULL DEFAULT 0;
|]

down_m20260826_wallet_name_keys :: Query
down_m20260826_wallet_name_keys =
  [sql|
ALTER TABLE users DROP COLUMN wallet_next_name_index;

DROP INDEX idx_wallet_name_keys_wallet_seed_id;

DROP TABLE wallet_name_keys;
|]
