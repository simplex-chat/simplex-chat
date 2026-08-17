{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260806_wallet_seeds where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- | Wallet seeds: BIP-39 entropy, one or more per database.
--
-- The schema allows several seeds per database, with each chat profile bound to
-- exactly one of them plus its own BIP-44 account index. Only the single-seed
-- case is reachable from the UI today — profiles all share one seed and differ
-- by account index — but modelling it this way now means importing a second
-- recovery key later is a UI change, not a migration of live key material.
m20260806_wallet_seeds :: Query
m20260806_wallet_seeds =
  [sql|
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  seed BLOB NOT NULL,                       -- BIP-39 entropy, 16-32 bytes
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  backed_up INTEGER NOT NULL DEFAULT 0,     -- user acknowledged saving the recovery key
  -- High-water mark for account allocation. Deliberately not derived from
  -- MAX(users.wallet_account_index): after recovery from the phrase alone that
  -- table is empty while accounts 0..N already hold names on chain, so a new
  -- profile would silently reuse a recovered account's keys and meta-address.
  -- The recovery probe raises this before any profile is created.
  next_account_index INTEGER NOT NULL DEFAULT 0
) STRICT;

ALTER TABLE users ADD COLUMN wallet_seed_id INTEGER REFERENCES wallet_seeds ON DELETE RESTRICT;
ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;
-- Position of the last recovery scan, so a repeat scan resumes. Not a live
-- watermark: incoming names are learned from a chat message, not by scanning.
ALTER TABLE users ADD COLUMN wallet_scanned_to TEXT;

CREATE INDEX idx_users_wallet_seed_id ON users(wallet_seed_id);

-- Destinations learned from a sender's message, or rediscovered by a recovery
-- scan. Holds no private key: the key is re-derived from the seed and the
-- ephemeral public key on demand, so this table is a cache and losing it costs
-- a rescan rather than an asset.
--
-- 'chain' is carried from the first migration so that adding Bitcoin or Monero
-- later is new rows, not a schema change.
CREATE TABLE wallet_one_time_addresses (
  wallet_one_time_address_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  chain TEXT NOT NULL,                      -- 'eth' today; 'btc', 'xmr' later
  address BLOB NOT NULL,
  ephemeral_pub_key BLOB NOT NULL,          -- compressed secp256k1 point, 33 bytes
  discovered_at TEXT NOT NULL DEFAULT (datetime('now')),
  accepted_at TEXT,                         -- NULL = received but not accepted
  UNIQUE (user_id, chain, address)
) STRICT;

CREATE INDEX idx_wallet_one_time_addresses_user ON wallet_one_time_addresses(user_id, chain);
|]

down_m20260806_wallet_seeds :: Query
down_m20260806_wallet_seeds =
  [sql|
DROP INDEX idx_wallet_one_time_addresses_user;

DROP TABLE wallet_one_time_addresses;

DROP INDEX idx_users_wallet_seed_id;

ALTER TABLE users DROP COLUMN wallet_scanned_to;
ALTER TABLE users DROP COLUMN wallet_account_index;
ALTER TABLE users DROP COLUMN wallet_seed_id;

DROP TABLE wallet_seeds;
|]
