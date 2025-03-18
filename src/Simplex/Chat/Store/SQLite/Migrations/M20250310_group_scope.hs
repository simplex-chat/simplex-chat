{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250310_group_scope where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [knocking] TBC schema
-- TODO            - support_chat_unanswered - don't persist, calculate on the fly?
m20250310_group_scope :: Query
m20250310_group_scope =
  [sql|
ALTER TABLE group_members ADD COLUMN support_chat_exists INTEGER;
ALTER TABLE group_members ADD COLUMN support_chat_ts TEXT;
ALTER TABLE group_members ADD COLUMN support_chat_unanswered INTEGER;

ALTER TABLE chat_items ADD COLUMN support_chat_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE;

CREATE INDEX idx_chat_items_support_chat_group_member_id ON chat_items(support_chat_group_member_id);
|]

down_m20250310_group_scope :: Query
down_m20250310_group_scope =
  [sql|
DROP INDEX idx_chat_items_support_chat_group_member_id;

ALTER TABLE chat_items DROP COLUMN group_scope;
ALTER TABLE chat_items DROP COLUMN group_conversation_id;

ALTER TABLE group_members DROP COLUMN support_chat_exists;
ALTER TABLE group_members DROP COLUMN support_chat_ts;
ALTER TABLE group_members DROP COLUMN support_chat_unanswered;
|]
