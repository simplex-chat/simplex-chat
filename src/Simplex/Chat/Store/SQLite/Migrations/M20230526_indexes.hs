{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230526_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230526_indexes :: Query
m20230526_indexes =
  [sql|
CREATE INDEX idx_messages_created_at ON messages(created_at);

CREATE INDEX idx_chat_item_reactions_created_by_msg_id ON chat_item_reactions(created_by_msg_id);
|]

down_m20230526_indexes :: Query
down_m20230526_indexes =
  [sql|
DROP INDEX idx_chat_item_reactions_created_by_msg_id;

DROP INDEX idx_messages_created_at;
|]
