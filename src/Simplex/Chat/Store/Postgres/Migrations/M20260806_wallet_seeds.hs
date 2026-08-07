{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260806_wallet_seeds where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260806_wallet_seeds :: Text
m20260806_wallet_seeds =
  [r|
CREATE TABLE wallet_seeds (
  wallet_seed_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  seed BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  backed_up SMALLINT NOT NULL DEFAULT 0,
  -- High-water mark for account allocation; see the SQLite migration for why
  -- this cannot be derived from MAX(users.wallet_account_index).
  next_account_index BIGINT NOT NULL DEFAULT 0
);

ALTER TABLE users ADD COLUMN wallet_seed_id BIGINT REFERENCES wallet_seeds ON DELETE RESTRICT;
ALTER TABLE users ADD COLUMN wallet_account_index BIGINT;
ALTER TABLE users ADD COLUMN wallet_scanned_to TEXT;

CREATE INDEX idx_users_wallet_seed_id ON users(wallet_seed_id);

CREATE TABLE wallet_one_time_addresses (
  wallet_one_time_address_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  chain TEXT NOT NULL,
  address BYTEA NOT NULL,
  ephemeral_pub_key BYTEA NOT NULL,
  discovered_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  accepted_at TIMESTAMPTZ,
  UNIQUE (user_id, chain, address)
);

CREATE INDEX idx_wallet_one_time_addresses_user ON wallet_one_time_addresses(user_id, chain);
|]

down_m20260806_wallet_seeds :: Text
down_m20260806_wallet_seeds =
  [r|
DROP INDEX idx_wallet_one_time_addresses_user;

DROP TABLE wallet_one_time_addresses;

DROP INDEX idx_users_wallet_seed_id;

ALTER TABLE users DROP COLUMN wallet_scanned_to;
ALTER TABLE users DROP COLUMN wallet_account_index;
ALTER TABLE users DROP COLUMN wallet_seed_id;

DROP TABLE wallet_seeds;
|]
