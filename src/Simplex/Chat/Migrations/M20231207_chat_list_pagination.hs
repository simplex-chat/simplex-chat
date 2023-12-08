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
WHERE chat_ts IS NULL AND contact_used = 1;

UPDATE groups
SET chat_ts = updated_at
WHERE chat_ts IS NULL;
|]

down_m20231207_chat_list_pagination :: Query
down_m20231207_chat_list_pagination =
  [sql|
|]
