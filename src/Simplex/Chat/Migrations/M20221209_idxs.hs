{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221209_idxs where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221209_idxs :: Query
m20221209_idxs =
  [sql|
-- CREATE INDEX idx_files_contact_id ON files(user_id, contact_id);
-- CREATE INDEX idx_files_group_id ON files(user_id, group_id);

-- CREATE INDEX idx_commands_connection_id ON commands(user_id, connection_id);
-- CREATE INDEX idx_connections_group_member_id ON connections(user_id, group_member_id);
CREATE INDEX idx_messages_connection_id ON messages(connection_id);  -- <-- speeds up 40 sec (DELETE FROM files; contacts)

-- CREATE INDEX idx_group_members_local_display_name ON group_members(user_id, local_display_name);

-- CREATE INDEX idx_connections_group_member_id ON connections(group_member_id);

CREATE INDEX idx_chat_items_group_member_id ON chat_items(group_member_id);  -- <-- speeds up ~5 sec (deleteGroupItemsAndMembers DELETE FROM group_members)

CREATE INDEX idx_chat_items_contact_id ON chat_items(contact_id);
|]
