{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260818_wallet_seeds where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260818_wallet_seeds :: Text
m20260818_wallet_seeds =
  [r|
CREATE TABLE wallet_seeds (
  wallet_seed_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  seed BYTEA NOT NULL,
  -- High-water mark for account allocation; see the SQLite migration for why
  -- this cannot be derived from MAX(users.wallet_account_index).
  next_account_index BIGINT NOT NULL DEFAULT 0
);

ALTER TABLE users ADD COLUMN wallet_seed_id BIGINT REFERENCES wallet_seeds ON DELETE RESTRICT;
ALTER TABLE users ADD COLUMN wallet_account_index BIGINT;

CREATE INDEX idx_users_wallet_seed_id ON users(wallet_seed_id);
|]

down_m20260818_wallet_seeds :: Text
down_m20260818_wallet_seeds =
  [r|
DROP INDEX idx_users_wallet_seed_id;

ALTER TABLE users DROP COLUMN wallet_account_index;
ALTER TABLE users DROP COLUMN wallet_seed_id;

DROP TABLE wallet_seeds;
|]
