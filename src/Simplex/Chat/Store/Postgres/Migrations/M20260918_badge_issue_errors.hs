{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260918_badge_issue_errors where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260918_badge_issue_errors :: Text
m20260918_badge_issue_errors =
  [r|
ALTER TABLE badge_purchases ADD COLUMN issue_failed_since TIMESTAMPTZ;

ALTER TABLE badge_purchases ADD COLUMN issue_error_at TIMESTAMPTZ;

ALTER TABLE badge_purchases ADD COLUMN issue_error TEXT;

ALTER TABLE badge_purchases ADD COLUMN next_wake_at TIMESTAMPTZ;
|]

down_m20260918_badge_issue_errors :: Text
down_m20260918_badge_issue_errors =
  [r|
ALTER TABLE badge_purchases DROP COLUMN issue_failed_since;

ALTER TABLE badge_purchases DROP COLUMN issue_error_at;

ALTER TABLE badge_purchases DROP COLUMN issue_error;

ALTER TABLE badge_purchases DROP COLUMN next_wake_at;
|]
