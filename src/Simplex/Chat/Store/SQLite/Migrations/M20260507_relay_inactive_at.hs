{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260507_relay_inactive_at where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260507_relay_inactive_at :: Query
m20260507_relay_inactive_at =
  [sql|
ALTER TABLE groups ADD COLUMN relay_inactive_at TEXT;
|]

down_m20260507_relay_inactive_at :: Query
down_m20260507_relay_inactive_at =
  [sql|
ALTER TABLE groups DROP COLUMN relay_inactive_at;
|]
