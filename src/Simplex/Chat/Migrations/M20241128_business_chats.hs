{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241128_business_chats where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241128_business_chats :: Query
m20241128_business_chats =
  [sql|
ALTER TABLE user_contact_links ADD business INTEGER DEFAULT 0;
ALTER TABLE groups ADD COLUMN group_biz TEXT NULL;
ALTER TABLE group_members ADD COLUMN member_biz TEXT NULL;
|]

down_m20241128_business_chats :: Query
down_m20241128_business_chats =
  [sql|
ALTER TABLE user_contact_links DROP COLUMN business;
ALTER TABLE groups DROP COLUMN group_biz;
ALTER TABLE group_members DROP COLUMN member_biz;
|]
