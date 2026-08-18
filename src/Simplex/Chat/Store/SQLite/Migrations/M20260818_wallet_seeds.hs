{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260818_wallet_seeds where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- | Wallet seeds: BIP-39 entropy, one or more per database.
--
-- The schema allows several seeds, with each chat profile bound to exactly one
-- of them plus its own BIP-44 account index. Only the single-seed case is
-- reachable today.
m20260818_wallet_seeds :: Query
m20260818_wallet_seeds =
  [sql|
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  seed BLOB NOT NULL,                       -- BIP-39 entropy, 16-32 bytes
  -- High-water mark for account allocation. Deliberately not derived from
  -- MAX(users.wallet_account_index): after recovery from the phrase alone that
  -- table is empty while accounts 0..N already hold names on chain, so a new
  -- profile would silently reuse a recovered account's keys.
  next_account_index INTEGER NOT NULL DEFAULT 0
) STRICT;

ALTER TABLE users ADD COLUMN wallet_seed_id INTEGER REFERENCES wallet_seeds ON DELETE RESTRICT;
ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;

-- required: the .lint check enforces an index on every foreign key
CREATE INDEX idx_users_wallet_seed_id ON users(wallet_seed_id);
|]

down_m20260818_wallet_seeds :: Query
down_m20260818_wallet_seeds =
  [sql|
DROP INDEX idx_users_wallet_seed_id;

ALTER TABLE users DROP COLUMN wallet_account_index;
ALTER TABLE users DROP COLUMN wallet_seed_id;

DROP TABLE wallet_seeds;
|]
