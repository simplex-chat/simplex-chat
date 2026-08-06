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
  backed_up INTEGER NOT NULL DEFAULT 0      -- user acknowledged saving the recovery key
);

ALTER TABLE users ADD COLUMN wallet_seed_id INTEGER REFERENCES wallet_seeds ON DELETE RESTRICT;
ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;

CREATE INDEX idx_users_wallet_seed_id ON users(wallet_seed_id);
|]

down_m20260806_wallet_seeds :: Query
down_m20260806_wallet_seeds =
  [sql|
DROP INDEX idx_users_wallet_seed_id;

ALTER TABLE users DROP COLUMN wallet_account_index;
ALTER TABLE users DROP COLUMN wallet_seed_id;

DROP TABLE wallet_seeds;
|]
