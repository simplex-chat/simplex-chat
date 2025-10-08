{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251007_subscriptions_sync where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- should_sync should be set manually when required
m20251007_subscriptions_sync :: Query
m20251007_subscriptions_sync =
  [sql|
CREATE TABLE subscriptions_sync(
  subscriptions_sync_id INTEGER PRIMARY KEY AUTOINCREMENT,
  should_sync INTEGER NOT NULL DEFAULT 0,
  last_sync_ts TEXT,
  result TEXT
);

INSERT INTO subscriptions_sync (subscriptions_sync_id, should_sync, last_sync_ts, result) VALUES (1,0,NULL,NULL);
|]

down_m20251007_subscriptions_sync :: Query
down_m20251007_subscriptions_sync =
  [sql|
DROP TABLE subscriptions_sync;
|]
