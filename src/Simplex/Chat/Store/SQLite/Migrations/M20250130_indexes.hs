{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250130_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250130_indexes :: Query
m20250130_indexes =
  [sql|
CREATE INDEX idx_chat_items_group_id ON chat_items(group_id);

CREATE INDEX idx_connections_group_member_id ON connections(group_member_id);

DROP INDEX idx_chat_items_shared_msg_id;
CREATE INDEX idx_chat_items_group_id_shared_msg_id ON chat_items(group_id, shared_msg_id);
|]

down_m20250130_indexes :: Query
down_m20250130_indexes =
  [sql|
DROP INDEX idx_chat_items_group_id_shared_msg_id;
CREATE INDEX idx_chat_items_shared_msg_id ON chat_items(shared_msg_id);

DROP INDEX idx_connections_group_member_id;

DROP INDEX idx_chat_items_group_id;
|]
