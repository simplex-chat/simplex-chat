{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231207_chat_list_pagination where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231207_chat_list_pagination :: Query
m20231207_chat_list_pagination =
  [sql|
UPDATE contacts SET contact_used = 1
WHERE contact_id = (
  SELECT contact_id FROM connections
  WHERE conn_level = 0 AND via_group_link = 0
);

UPDATE contacts
SET chat_ts = updated_at
WHERE chat_ts IS NULL;

UPDATE groups
SET chat_ts = updated_at
WHERE chat_ts IS NULL;

CREATE INDEX idx_contacts_chat_ts ON contacts(user_id, chat_ts);
CREATE INDEX idx_groups_chat_ts ON groups(user_id, chat_ts);
CREATE INDEX idx_contact_requests_updated_at ON contact_requests(user_id, updated_at);
CREATE INDEX idx_connections_updated_at ON connections(user_id, updated_at);
|]

down_m20231207_chat_list_pagination :: Query
down_m20231207_chat_list_pagination =
  [sql|
DROP INDEX idx_contacts_chat_ts;
DROP INDEX idx_groups_chat_ts;
DROP INDEX idx_contact_requests_updated_at;
DROP INDEX idx_connections_updated_at;
|]
