{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221019_chat_unread_status where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221019_chat_unread_status :: Query
m20221019_chat_unread_status =
  [sql|
ALTER TABLE contacts ADD COLUMN chat_unread INTEGER CHECK (chat_unread IN (NULL, 0, 1);
ALTER TABLE groups ADD COLUMN chat_unread INTEGER CHECK (chat_unread IN (NULL, 0, 1);
|]

