{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260826_wallet_name_keys where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260826_wallet_name_keys :: Text
m20260826_wallet_name_keys =
  [r|
CREATE TABLE wallet_name_keys (
  wallet_name_key_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  wallet_seed_id BIGINT NOT NULL REFERENCES wallet_seeds ON DELETE RESTRICT,
  derivation_path TEXT NOT NULL,
  name TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (wallet_seed_id, derivation_path),
  UNIQUE (wallet_seed_id, name)
);

CREATE INDEX idx_wallet_name_keys_wallet_seed_id ON wallet_name_keys(wallet_seed_id);

ALTER TABLE users ADD COLUMN wallet_next_name_index BIGINT NOT NULL DEFAULT 0;
|]

down_m20260826_wallet_name_keys :: Text
down_m20260826_wallet_name_keys =
  [r|
ALTER TABLE users DROP COLUMN wallet_next_name_index;

DROP INDEX idx_wallet_name_keys_wallet_seed_id;

DROP TABLE wallet_name_keys;
|]
