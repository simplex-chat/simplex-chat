{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221222_chat_ts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221222_chat_ts :: Query
m20221222_chat_ts =
  [sql|
ALTER TABLE contacts ADD COLUMN chat_ts TEXT; -- must be not NULL

ALTER TABLE groups ADD COLUMN chat_ts TEXT; -- must be not NULL
|]
