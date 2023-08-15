{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230814_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230814_indexes :: Query
m20230814_indexes =
  [sql|
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(user_id, item_status);

CREATE INDEX idx_chat_items_group_id ON chat_items(group_id);
|]

down_m20230814_indexes :: Query
down_m20230814_indexes =
  [sql|
DROP INDEX idx_chat_items_group_id;

DROP INDEX idx_chat_items_user_id_item_status;
|]
