{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221129_delete_group_feature_items where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221129_delete_group_feature_items :: Query
m20221129_delete_group_feature_items =
  [sql|
DELETE FROM chat_items WHERE item_content LIKE '%{"rcvGroupFeature":{%';
DELETE FROM chat_items WHERE item_content LIKE '%{"sndGroupFeature":{%';
|]
