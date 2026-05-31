{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260531_relay_sent_web_url where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260531_relay_sent_web_url :: Query
m20260531_relay_sent_web_url =
  [sql|
ALTER TABLE groups ADD COLUMN relay_sent_web_url TEXT;
|]

down_m20260531_relay_sent_web_url :: Query
down_m20260531_relay_sent_web_url =
  [sql|
ALTER TABLE groups DROP COLUMN relay_sent_web_url;
|]
