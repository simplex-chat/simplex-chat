{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260918_badge_issue_errors where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260918_badge_issue_errors :: Query
m20260918_badge_issue_errors =
  [sql|
ALTER TABLE badge_purchases ADD COLUMN issue_failed_since TEXT;

ALTER TABLE badge_purchases ADD COLUMN issue_error_at TEXT;

ALTER TABLE badge_purchases ADD COLUMN issue_error TEXT;

ALTER TABLE badge_purchases ADD COLUMN next_wake_at TEXT;
|]

down_m20260918_badge_issue_errors :: Query
down_m20260918_badge_issue_errors =
  [sql|
ALTER TABLE badge_purchases DROP COLUMN issue_failed_since;

ALTER TABLE badge_purchases DROP COLUMN issue_error_at;

ALTER TABLE badge_purchases DROP COLUMN issue_error;

ALTER TABLE badge_purchases DROP COLUMN next_wake_at;
|]
