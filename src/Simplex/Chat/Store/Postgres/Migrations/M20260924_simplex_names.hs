{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260924_simplex_names where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260924_simplex_names :: Text
m20260924_simplex_names =
  [r|
CREATE TABLE simplex_names(
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  simplex_domain TEXT NOT NULL,
  registration TEXT NOT NULL,
  resolved_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (user_id, simplex_domain)
);
|]

down_m20260924_simplex_names :: Text
down_m20260924_simplex_names =
  [r|
DROP TABLE simplex_names;
|]
