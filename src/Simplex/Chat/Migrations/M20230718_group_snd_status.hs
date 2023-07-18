{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230718_group_snd_status where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230718_group_snd_status :: Query
m20230718_group_snd_status =
  [sql|
ALTER TABLE chat_items ADD COLUMN group_snd_status TEXT;
|]

down_m20230718_group_snd_status :: Query
down_m20230718_group_snd_status =
  [sql|
ALTER TABLE chat_items DROP COLUMN group_snd_status;
|]
