{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241128_business_chats where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241128_business_chats :: Query
m20241128_business_chats =
  [sql|
ALTER TABLE user_contact_links ADD business INTEGER DEFAULT 0;
ALTER TABLE groups ADD COLUMN business_chat TEXT NULL;
ALTER TABLE groups ADD COLUMN business_member_id BLOB NULL;
|]

down_m20241128_business_chats :: Query
down_m20241128_business_chats =
  [sql|
ALTER TABLE user_contact_links DROP COLUMN business;
ALTER TABLE groups DROP COLUMN business_chat;
ALTER TABLE groups DROP COLUMN business_member_id;
|]
