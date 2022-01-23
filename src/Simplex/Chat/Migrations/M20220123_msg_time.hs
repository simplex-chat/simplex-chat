{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220123_msg_time where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220123_msg_time :: Query
m20220123_msg_time =
  [sql|
ALTER TABLE messages ADD msg_time TEXT;
|]