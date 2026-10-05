{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260924_simplex_name_resolved where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260924_simplex_name_resolved :: Query
m20260924_simplex_name_resolved =
  [sql|
CREATE TABLE simplex_names(
  simplex_domain TEXT NOT NULL PRIMARY KEY,
  registration TEXT NOT NULL,
  resolved_at TEXT NOT NULL
) STRICT;

CREATE INDEX idx_simplex_names_resolved_at ON simplex_names(resolved_at);
|]

down_m20260924_simplex_name_resolved :: Query
down_m20260924_simplex_name_resolved =
  [sql|
DROP INDEX idx_simplex_names_resolved_at;
DROP TABLE simplex_names;
|]
