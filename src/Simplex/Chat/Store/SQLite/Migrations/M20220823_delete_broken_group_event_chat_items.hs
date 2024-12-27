{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220823_delete_broken_group_event_chat_items where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220823_delete_broken_group_event_chat_items :: Query
m20220823_delete_broken_group_event_chat_items =
  [sql|
DELETE FROM chat_items WHERE item_content LIKE '%{"rcvGroupEvent":{"rcvGroupEvent":{%';
DELETE FROM chat_items WHERE item_content LIKE '%{"sndGroupEvent":{"sndGroupEvent":{%';
|]
