{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260827_wallet_multi_key where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260827_wallet_multi_key :: Text
m20260827_wallet_multi_key =
  [r|
ALTER TABLE wallet_seeds ADD COLUMN backed_up SMALLINT NOT NULL DEFAULT 0;
ALTER TABLE users ADD COLUMN wallet_current_seed_id BIGINT REFERENCES wallet_seeds ON DELETE RESTRICT;
CREATE INDEX idx_users_wallet_current_seed_id ON users(wallet_current_seed_id);

ALTER TABLE wallet_name_keys ADD COLUMN provenance TEXT NOT NULL DEFAULT 'derived';
ALTER TABLE wallet_name_keys ADD COLUMN priv_key BYTEA;
|]

down_m20260827_wallet_multi_key :: Text
down_m20260827_wallet_multi_key =
  [r|
ALTER TABLE wallet_name_keys DROP COLUMN priv_key;
ALTER TABLE wallet_name_keys DROP COLUMN provenance;

DROP INDEX idx_users_wallet_current_seed_id;
ALTER TABLE users DROP COLUMN wallet_current_seed_id;

ALTER TABLE wallet_seeds DROP COLUMN backed_up;
|]
