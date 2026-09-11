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
  -- known issue: after an import this starts at 1, so it can hand out a name
  -- key at a path that already owns a name
  next_name_index INTEGER NOT NULL DEFAULT 1,
  -- one seed per device for now
  single_seed INTEGER NOT NULL DEFAULT 1
) STRICT;

CREATE UNIQUE INDEX idx_wallet_seeds_single_seed ON wallet_seeds(single_seed);
|]

down_m20260908_wallet_seeds :: Query
down_m20260908_wallet_seeds =
  [sql|
DROP INDEX idx_wallet_seeds_single_seed;

DROP TABLE wallet_seeds;
|]
