{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221001_shared_msg_id_indices where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221001_shared_msg_id_indices :: Query
m20221001_shared_msg_id_indices =
  [sql|
DROP INDEX idx_messages_group_shared_msg_id;

CREATE UNIQUE INDEX idx_chat_items_direct_shared_msg_id ON chat_items(
  user_id,
  contact_id,
  shared_msg_id
);

CREATE UNIQUE INDEX idx_chat_items_group_shared_msg_id ON chat_items(
  user_id,
  group_id,
  group_member_id,
  shared_msg_id
);
|]
