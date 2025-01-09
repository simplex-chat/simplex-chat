{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20250109_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250109_indexes :: Query
m20250109_indexes =
  [sql|
CREATE INDEX idx_group_snd_item_statuses_chat_item_id_group_member_id ON group_snd_item_statuses(chat_item_id, group_member_id);
|]

down_m20250109_indexes :: Query
down_m20250109_indexes =
  [sql|
DROP INDEX idx_group_snd_item_statuses_chat_item_id_group_member_id;
|]
