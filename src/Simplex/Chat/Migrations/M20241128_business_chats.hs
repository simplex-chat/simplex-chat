{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241128_business_chats where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241128_business_chats :: Query
m20241128_business_chats =
  [sql|
ALTER TABLE user_contact_links ADD business INTEGER DEFAULT 0;
ALTER TABLE groups ADD COLUMN biz_group TEXT NULL;
ALTER TABLE groups ADD COLUMN biz_group_member_id INTEGER NULL REFERENCES group_members(group_member_id);
ALTER TABLE group_members ADD COLUMN biz_member TEXT NULL;

CREATE INDEX idx_groups_biz_group_member_id ON GROUPS(biz_group_member_id);
|]

down_m20241128_business_chats :: Query
down_m20241128_business_chats =
  [sql|
DROP INDEX idx_groups_biz_group_member_id;

ALTER TABLE user_contact_links DROP COLUMN business;
ALTER TABLE groups DROP COLUMN group_biz;
ALTER TABLE groups DROP COLUMN biz_group_member_id;
ALTER TABLE group_members DROP COLUMN member_biz;
|]
