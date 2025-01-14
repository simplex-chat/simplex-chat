{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241205_business_chat_members where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241205_business_chat_members :: Query
m20241205_business_chat_members =
  [sql|
ALTER TABLE groups ADD COLUMN customer_member_id BLOB NULL;
|]

down_m20241205_business_chat_members :: Query
down_m20241205_business_chat_members =
  [sql|
ALTER TABLE groups DROP COLUMN customer_member_id;
|]
