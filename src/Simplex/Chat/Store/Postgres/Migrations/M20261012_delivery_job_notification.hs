{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261012_delivery_job_notification where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261012_delivery_job_notification :: Text
m20261012_delivery_job_notification =
  [r|
ALTER TABLE delivery_jobs ADD COLUMN notification SMALLINT NOT NULL DEFAULT 0;
|]

down_m20261012_delivery_job_notification :: Text
down_m20261012_delivery_job_notification =
  [r|
ALTER TABLE delivery_jobs DROP COLUMN notification;
|]
