{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240528_quota_err_counter where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240528_quota_err_counter :: Query
m20240528_quota_err_counter =
  [sql|
ALTER TABLE connections ADD COLUMN quota_err_counter INTEGER NOT NULL DEFAULT 0;
|]

down_m20240528_quota_err_counter :: Query
down_m20240528_quota_err_counter =
  [sql|
ALTER TABLE connections DROP COLUMN quota_err_counter;
|]
