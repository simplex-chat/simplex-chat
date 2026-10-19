{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220818_chat_notifications where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220818_chat_notifications :: Query
m20220818_chat_notifications =
  [sql|
ALTER TABLE contacts ADD COLUMN enable_ntfs INTEGER;

ALTER TABLE groups ADD COLUMN enable_ntfs INTEGER;
|]
