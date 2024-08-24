{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240515_rcv_files_user_approved_relays where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240515_rcv_files_user_approved_relays :: Query
m20240515_rcv_files_user_approved_relays =
  [sql|
ALTER TABLE rcv_files ADD COLUMN user_approved_relays INTEGER NOT NULL DEFAULT 0;
|]

down_m20240515_rcv_files_user_approved_relays :: Query
down_m20240515_rcv_files_user_approved_relays =
  [sql|
ALTER TABLE rcv_files DROP COLUMN user_approved_relays;
|]
