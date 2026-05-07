{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260430_subscriber_body where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260430_subscriber_body :: Query
m20260430_subscriber_body =
  [sql|
ALTER TABLE delivery_jobs ADD COLUMN subscriber_body BLOB;
|]

down_m20260430_subscriber_body :: Query
down_m20260430_subscriber_body =
  [sql|
ALTER TABLE delivery_jobs DROP COLUMN subscriber_body;
|]
