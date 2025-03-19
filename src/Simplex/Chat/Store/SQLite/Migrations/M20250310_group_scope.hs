{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250310_group_scope where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- - group_scope_group_member_id points either to member (for chat with member as admin),
--   or to membership (for chat with admins as member); it is used to find chat items for scope,
--   when we know from context what group member id we are looking for;
-- - group_scope_tag allows to distinguish between these two cases when reading chat item
--   without knowing the context - to learn the scope of the chat item;
-- - alternatively we could have a separate filter for "member to admins" scope (e.g. chat_items.support_chat_for_group),
--   but it would require additional index for reading chat items for this scope.
-- TODO [knocking] TBC schema
-- TODO            - group_members.support_chat_unanswered - don't persist, calculate on the fly?
m20250310_group_scope :: Query
m20250310_group_scope =
  [sql|
ALTER TABLE group_members ADD COLUMN support_chat_exists INTEGER;
ALTER TABLE group_members ADD COLUMN support_chat_ts TEXT;
ALTER TABLE group_members ADD COLUMN support_chat_unanswered INTEGER;

ALTER TABLE chat_items ADD COLUMN group_scope_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN group_scope_tag TEXT;

CREATE INDEX idx_chat_items_group_scope_group_member_id ON chat_items(group_scope_group_member_id);

CREATE INDEX idx_chat_items_groups_group_scope_group_member_id_item_ts ON chat_items(
  user_id,
  group_id,
  group_scope_group_member_id,
  item_ts
);
|]

down_m20250310_group_scope :: Query
down_m20250310_group_scope =
  [sql|
DROP INDEX idx_chat_items_groups_group_scope_group_member_id_item_ts;

DROP INDEX idx_chat_items_group_scope_group_member_id;

ALTER TABLE chat_items DROP COLUMN group_scope_group_member_id;
ALTER TABLE chat_items DROP COLUMN group_scope_tag;

ALTER TABLE group_members DROP COLUMN support_chat_exists;
ALTER TABLE group_members DROP COLUMN support_chat_ts;
ALTER TABLE group_members DROP COLUMN support_chat_unanswered;
|]
