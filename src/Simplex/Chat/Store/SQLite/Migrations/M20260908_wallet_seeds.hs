{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260908_wallet_seeds where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260908_wallet_seeds :: Query
m20260908_wallet_seeds =
  [sql|
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  seed BLOB NOT NULL, -- BIP-39 entropy, 16-32 bytes
  -- known issue: after an import this starts at 0, so /_wallet bind with no
  -- account can hand out one that already owns names
  next_account_index INTEGER NOT NULL DEFAULT 0,
  -- one key per device for now
  single_seed INTEGER NOT NULL DEFAULT 1
) STRICT;

ALTER TABLE users ADD COLUMN wallet_seed_id INTEGER REFERENCES wallet_seeds ON DELETE RESTRICT;
ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;

CREATE UNIQUE INDEX idx_wallet_seeds_single_seed ON wallet_seeds(single_seed);
CREATE INDEX idx_users_wallet_seed_id ON users(wallet_seed_id);
|]

down_m20260908_wallet_seeds :: Query
down_m20260908_wallet_seeds =
  [sql|
DROP INDEX idx_users_wallet_seed_id;

ALTER TABLE users DROP COLUMN wallet_account_index;
ALTER TABLE users DROP COLUMN wallet_seed_id;

DROP TABLE wallet_seeds;
|]
