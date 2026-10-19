{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261017_badge_subscriptions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261017_badge_subscriptions :: Query
m20261017_badge_subscriptions =
  [sql|
ALTER TABLE badge_purchases ADD COLUMN subscription_status TEXT;

ALTER TABLE badge_purchases ADD COLUMN unrenewed_since TEXT;

ALTER TABLE badge_purchases ADD COLUMN last_unrenewed_at TEXT;
|]

down_m20261017_badge_subscriptions :: Query
down_m20261017_badge_subscriptions =
  [sql|
UPDATE badge_purchases SET alert_acked_kind = NULL, alert_acked_episode = NULL, alert_snooze_until = NULL WHERE alert_acked_kind = 'subscription_cancelled';

ALTER TABLE badge_purchases DROP COLUMN last_unrenewed_at;

ALTER TABLE badge_purchases DROP COLUMN unrenewed_since;

ALTER TABLE badge_purchases DROP COLUMN subscription_status;
|]
