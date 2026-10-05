{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260924_simplex_names where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260924_simplex_names :: Query
m20260924_simplex_names =
  [sql|
CREATE TABLE simplex_names(
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  simplex_domain TEXT NOT NULL,
  registration TEXT NOT NULL,
  resolved_at TEXT NOT NULL,
  PRIMARY KEY (user_id, simplex_domain)
) STRICT;
|]

down_m20260924_simplex_names :: Query
down_m20260924_simplex_names =
  [sql|
DROP TABLE simplex_names;
|]
