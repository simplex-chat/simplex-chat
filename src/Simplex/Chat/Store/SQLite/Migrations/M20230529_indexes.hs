{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230529_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230529_indexes :: Query
m20230529_indexes =
  [sql|
DROP INDEX idx_chat_items_timed_delete_at;

CREATE INDEX idx_chat_items_timed_delete_at ON chat_items(user_id, timed_delete_at);

CREATE INDEX idx_group_members_group_id ON group_members(user_id, group_id);

CREATE INDEX idx_msg_deliveries_agent_ack_cmd_id ON msg_deliveries(connection_id, agent_ack_cmd_id);
|]

down_m20230529_indexes :: Query
down_m20230529_indexes =
  [sql|
DROP INDEX idx_msg_deliveries_agent_ack_cmd_id;

DROP INDEX idx_group_members_group_id;

DROP INDEX idx_chat_items_timed_delete_at;

CREATE INDEX idx_chat_items_timed_delete_at ON chat_items(timed_delete_at);
|]
