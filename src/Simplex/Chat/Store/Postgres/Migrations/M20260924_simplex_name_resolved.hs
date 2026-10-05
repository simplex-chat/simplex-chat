{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260924_simplex_name_resolved where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260924_simplex_name_resolved :: Text
m20260924_simplex_name_resolved =
  [r|
CREATE TABLE simplex_names(
  simplex_domain TEXT NOT NULL PRIMARY KEY,
  registration TEXT NOT NULL,
  resolved_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX idx_simplex_names_resolved_at ON simplex_names(resolved_at);
|]

down_m20260924_simplex_name_resolved :: Text
down_m20260924_simplex_name_resolved =
  [r|
DROP INDEX idx_simplex_names_resolved_at;
DROP TABLE simplex_names;
|]
