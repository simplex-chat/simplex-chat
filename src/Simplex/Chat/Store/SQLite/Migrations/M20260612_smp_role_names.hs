{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260612_smp_role_names where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260612_smp_role_names :: Query
m20260612_smp_role_names =
  [sql|
ALTER TABLE server_operators ADD COLUMN smp_role_names INTEGER NOT NULL DEFAULT 0;

UPDATE server_operators SET smp_role_names = 1 WHERE server_operator_tag = 'simplex';
|]

down_m20260612_smp_role_names :: Query
down_m20260612_smp_role_names =
  [sql|
ALTER TABLE server_operators DROP COLUMN smp_role_names;
|]
