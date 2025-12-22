{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250513_group_scope where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250513_group_scope :: Text
m20250513_group_scope =
  T.pack
    [r|
ALTER TABLE group_members ADD COLUMN support_chat_ts TIMESTAMPTZ;
ALTER TABLE group_members ADD COLUMN support_chat_items_unread BIGINT NOT NULL DEFAULT 0;
ALTER TABLE group_members ADD COLUMN support_chat_items_member_attention BIGINT NOT NULL DEFAULT 0;
ALTER TABLE group_members ADD COLUMN support_chat_items_mentions BIGINT NOT NULL DEFAULT 0;
ALTER TABLE group_members ADD COLUMN support_chat_last_msg_from_member_ts TIMESTAMPTZ;

ALTER TABLE groups ADD COLUMN members_require_attention BIGINT NOT NULL DEFAULT 0;

ALTER TABLE chat_items ADD COLUMN group_scope_tag TEXT;
ALTER TABLE chat_items ADD COLUMN group_scope_group_member_id BIGINT REFERENCES group_members(group_member_id) ON DELETE CASCADE;

CREATE INDEX idx_chat_items_group_scope_group_member_id ON chat_items(group_scope_group_member_id);

CREATE INDEX idx_chat_items_group_scope_item_ts ON chat_items(
  user_id,
  group_id,
  group_scope_tag,
  group_scope_group_member_id,
  item_ts
);

CREATE INDEX idx_chat_items_group_scope_item_status ON chat_items(
  user_id,
  group_id,
  group_scope_tag,
  group_scope_group_member_id,
  item_status,
  item_ts
);
|]

down_m20250513_group_scope :: Text
down_m20250513_group_scope =
  T.pack
    [r|
DROP INDEX idx_chat_items_group_scope_item_status;

DROP INDEX idx_chat_items_group_scope_item_ts;

DROP INDEX idx_chat_items_group_scope_group_member_id;

ALTER TABLE chat_items DROP COLUMN group_scope_tag;
ALTER TABLE chat_items DROP COLUMN group_scope_group_member_id;

ALTER TABLE groups DROP COLUMN members_require_attention;

ALTER TABLE group_members DROP COLUMN support_chat_ts;
ALTER TABLE group_members DROP COLUMN support_chat_items_unread;
ALTER TABLE group_members DROP COLUMN support_chat_items_member_attention;
ALTER TABLE group_members DROP COLUMN support_chat_items_mentions;
ALTER TABLE group_members DROP COLUMN support_chat_last_msg_from_member_ts;
|]
