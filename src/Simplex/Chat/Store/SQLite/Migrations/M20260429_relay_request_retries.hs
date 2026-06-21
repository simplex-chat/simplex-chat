{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260429_relay_request_retries where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260429_relay_request_retries :: Query
m20260429_relay_request_retries =
  [sql|
ALTER TABLE groups ADD COLUMN relay_request_retries INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN relay_request_delay INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN relay_request_execute_at TEXT NOT NULL DEFAULT '1970-01-01 00:00:00';
|]

down_m20260429_relay_request_retries :: Query
down_m20260429_relay_request_retries =
  [sql|
ALTER TABLE groups DROP COLUMN relay_request_retries;
ALTER TABLE groups DROP COLUMN relay_request_delay;
ALTER TABLE groups DROP COLUMN relay_request_execute_at;
|]
