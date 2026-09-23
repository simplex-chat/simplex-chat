{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260923_wallet_names where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260923_wallet_names :: Text
m20260923_wallet_names =
  [r|
-- the columns are commented in the SQLite migration
CREATE TABLE wallet_names (
  wallet_name_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  wallet_seed_id BIGINT NOT NULL REFERENCES wallet_seeds ON DELETE CASCADE,
  account_index BIGINT NOT NULL CHECK (account_index BETWEEN 0 AND 2147483647),
  name TEXT NOT NULL,
  name_response TEXT NOT NULL
);

CREATE UNIQUE INDEX idx_wallet_names_wallet_seed_id_name ON wallet_names(wallet_seed_id, name);
|]

-- No reverse step: wallet_seeds, which the rows hang off, has none either.
