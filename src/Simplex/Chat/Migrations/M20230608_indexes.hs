{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230608_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230608_indexes :: Query
m20230608_indexes =
  [sql|
CREATE INDEX idx_chat_items_group_id ON chat_items(group_id);

CREATE INDEX connections_group_member_id ON connections(group_member_id);

CREATE INDEX msg_delivery_events_msg_delivery_id ON msg_delivery_events(msg_delivery_id);
|]

down_m20230608_indexes :: Query
down_m20230608_indexes =
  [sql|
DROP INDEX msg_delivery_events_msg_delivery_id;

DROP INDEX connections_group_member_id;

DROP INDEX idx_chat_items_group_id;
|]
