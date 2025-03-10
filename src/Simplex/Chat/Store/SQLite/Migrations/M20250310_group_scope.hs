{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250310_group_scope where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [knocking] TBC schema
-- TODO            - group_scope to include only "tag" or also group_member_id?
-- TODO            - group_scope_group_member_id ON DELETE SET NULL?
m20250310_group_scope :: Query
m20250310_group_scope =
  [sql|
ALTER TABLE chat_items ADD COLUMN group_scope TEXT;
ALTER TABLE chat_items ADD COLUMN group_scope_group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE;

CREATE INDEX idx_chat_items_group_scope_group_member_id ON chat_items(group_scope_group_member_id);
|]

down_m20250310_group_scope :: Query
down_m20250310_group_scope =
  [sql|
DROP INDEX idx_chat_items_group_scope_group_member_id;

ALTER TABLE chat_items DROP COLUMN group_scope;
ALTER TABLE chat_items DROP COLUMN group_scope_group_member_id;
|]
