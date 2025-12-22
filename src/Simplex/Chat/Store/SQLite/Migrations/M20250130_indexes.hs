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


DROP INDEX idx_groups_host_conn_custom_user_profile_id;
ALTER TABLE groups DROP COLUMN host_conn_custom_user_profile_id;
|]

down_m20250130_indexes :: Query
down_m20250130_indexes =
  [sql|
ALTER TABLE groups ADD COLUMN host_conn_custom_user_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL;
CREATE INDEX idx_groups_host_conn_custom_user_profile_id ON groups(host_conn_custom_user_profile_id);


DROP INDEX idx_chat_items_group_id_shared_msg_id;
CREATE INDEX idx_chat_items_shared_msg_id ON chat_items(shared_msg_id);

DROP INDEX idx_connections_group_member_id;

DROP INDEX idx_chat_items_group_id;
|]
