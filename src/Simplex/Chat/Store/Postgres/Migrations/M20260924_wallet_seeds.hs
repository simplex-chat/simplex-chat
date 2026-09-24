{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260924_wallet_seeds where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260924_wallet_seeds :: Text
m20260924_wallet_seeds =
  [r|
CREATE TABLE wallet_seeds (
  wallet_seed_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  entropy BYTEA NOT NULL CHECK (length(entropy) = 32),
  next_account_index BIGINT CHECK (next_account_index BETWEEN 0 AND 2147483648),
  single_seed SMALLINT NOT NULL DEFAULT 1
);

CREATE TABLE wallet_accounts (
  wallet_account_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  wallet_seed_id BIGINT NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  account_index BIGINT CHECK (account_index BETWEEN 0 AND 2147483647),
  user_id BIGINT REFERENCES users ON DELETE SET NULL
);

CREATE UNIQUE INDEX idx_wallet_seeds_single_seed ON wallet_seeds(single_seed);
CREATE UNIQUE INDEX idx_wallet_accounts_wallet_seed_id_account_index ON wallet_accounts(wallet_seed_id, account_index);
CREATE INDEX idx_wallet_accounts_user_id ON wallet_accounts(user_id);
|]

-- no down migration, see the SQLite migration
