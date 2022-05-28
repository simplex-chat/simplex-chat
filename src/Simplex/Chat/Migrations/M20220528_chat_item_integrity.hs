{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220528_chat_item_integrity where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220514_profiles_user_id :: Query
m20220514_profiles_user_id =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_integrity TEXT DEFAULT NULL;
|]
