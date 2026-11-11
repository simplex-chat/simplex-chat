{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261012_delivery_job_notification where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261012_delivery_job_notification :: Query
m20261012_delivery_job_notification =
  [sql|
ALTER TABLE delivery_jobs ADD COLUMN notification INTEGER NOT NULL DEFAULT 0;
|]

down_m20261012_delivery_job_notification :: Query
down_m20261012_delivery_job_notification =
  [sql|
ALTER TABLE delivery_jobs DROP COLUMN notification;
|]
