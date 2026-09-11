{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260908_wallet_seeds where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260908_wallet_seeds :: Text
m20260908_wallet_seeds =
  [r|
CREATE TABLE wallet_seeds (
  wallet_seed_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  seed BYTEA NOT NULL,
  -- see the SQLite migration
  next_name_index BIGINT NOT NULL DEFAULT 1,
  -- one seed per device for now
  single_seed SMALLINT NOT NULL DEFAULT 1
);

CREATE UNIQUE INDEX idx_wallet_seeds_single_seed ON wallet_seeds(single_seed);
|]

down_m20260908_wallet_seeds :: Text
down_m20260908_wallet_seeds =
  [r|
DROP INDEX idx_wallet_seeds_single_seed;

DROP TABLE wallet_seeds;
|]
