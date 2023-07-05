{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230705_delivery_receipts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230705_delivery_receipts :: Query
m20230705_delivery_receipts =
  [sql|
ALTER TABLE users ADD COLUMN delivery_rcpts_enabled INTEGER NOT NULL DEFAULT 0;
|]

down_m20230705_delivery_receipts :: Query
down_m20230705_delivery_receipts =
  [sql|
ALTER TABLE users DROP COLUMN delivery_rcpts_enabled;
|]
