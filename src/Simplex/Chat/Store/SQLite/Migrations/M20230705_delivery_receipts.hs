{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230705_delivery_receipts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230705_delivery_receipts :: Query
m20230705_delivery_receipts =
  [sql|
ALTER TABLE users ADD COLUMN send_rcpts_contacts INTEGER NOT NULL DEFAULT 0;
ALTER TABLE users ADD COLUMN send_rcpts_small_groups INTEGER NOT NULL DEFAULT 0;
ALTER TABLE contacts ADD COLUMN send_rcpts INTEGER;
ALTER TABLE groups ADD COLUMN send_rcpts INTEGER;
|]

down_m20230705_delivery_receipts :: Query
down_m20230705_delivery_receipts =
  [sql|
ALTER TABLE users DROP COLUMN send_rcpts_contacts;
ALTER TABLE users DROP COLUMN send_rcpts_small_groups;
ALTER TABLE contacts DROP COLUMN send_rcpts;
ALTER TABLE groups DROP COLUMN send_rcpts;
|]
