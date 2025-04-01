{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250310_group_scope where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- - group_scope_group_member_id points either to member (for chat with member as admin),
--   or to membership (for chat with admins as member); it is used to find chat items for scope,
--   when we know from context what group member id we are looking for;
-- - to learn scope of chat item when context is not known, group member is joined and scope
--   is decided based on whether member is of user member category (membership -> chat with admins), or not.
-- TODO [knocking] TBC schema
-- TODO            - group_members.support_chat_unanswered - don't persist, calculate on the fly?
-- TODO            - review indexes (drop idx_chat_items_groups_item_ts?)
m20250310_group_scope :: Query
m20250310_group_scope =
  [sql|
ALTER TABLE group_members ADD COLUMN support_chat_ts TEXT;
ALTER TABLE group_members ADD COLUMN support_chat_unanswered INTEGER;

ALTER TABLE chat_items ADD COLUMN group_scope_tag TEXT;
ALTER TABLE chat_items ADD COLUMN group_scope_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE;

CREATE INDEX idx_chat_items_group_scope_group_member_id ON chat_items(group_scope_group_member_id);

CREATE INDEX idx_chat_items_group_scope_item_ts ON chat_items(
  user_id,
  group_id,
  group_scope_tag,
  group_scope_group_member_id,
  item_ts
);
|]

down_m20250310_group_scope :: Query
down_m20250310_group_scope =
  [sql|
DROP INDEX idx_chat_items_group_scope_item_ts;

DROP INDEX idx_chat_items_group_scope_group_member_id;

ALTER TABLE chat_items DROP COLUMN group_scope_tag;
ALTER TABLE chat_items DROP COLUMN group_scope_group_member_id;

ALTER TABLE group_members DROP COLUMN support_chat_ts;
ALTER TABLE group_members DROP COLUMN support_chat_unanswered;
|]
