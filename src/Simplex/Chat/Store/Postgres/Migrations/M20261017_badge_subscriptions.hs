{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261017_badge_subscriptions where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261017_badge_subscriptions :: Text
m20261017_badge_subscriptions =
  [r|
ALTER TABLE badge_purchases ADD COLUMN subscription_status TEXT;

ALTER TABLE badge_purchases ADD COLUMN unrenewed_since TIMESTAMPTZ;

ALTER TABLE badge_purchases ADD COLUMN last_unrenewed_at TIMESTAMPTZ;
|]

down_m20261017_badge_subscriptions :: Text
down_m20261017_badge_subscriptions =
  [r|
UPDATE badge_purchases SET alert_acked_kind = NULL, alert_acked_episode = NULL, alert_snooze_until = NULL WHERE alert_acked_kind = 'subscription_cancelled';

ALTER TABLE badge_purchases DROP COLUMN last_unrenewed_at;

ALTER TABLE badge_purchases DROP COLUMN unrenewed_since;

ALTER TABLE badge_purchases DROP COLUMN subscription_status;
|]
