{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221003_delete_broken_integrity_error_chat_items where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221003_delete_broken_integrity_error_chat_items :: Query
m20221003_delete_broken_integrity_error_chat_items =
  [sql|
DELETE FROM chat_items WHERE item_content LIKE '%{"rcvIntegrityError":{%';
|]
