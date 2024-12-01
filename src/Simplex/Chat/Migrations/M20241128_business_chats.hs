{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241128_business_chats where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [business] not sure if connection needs any marker now, probably not
m20241128_business_chats :: Query
m20241128_business_chats =
  [sql|
ALTER TABLE user_contact_links ADD business INTEGER DEFAULT 0;
ALTER TABLE connections ADD COLUMN business INTEGER DEFAULT 0;
ALTER TABLE groups ADD COLUMN business_chat TEXT NULL;
ALTER TABLE groups ADD COLUMN business_member_id BLOB NULL REFERENCES group_members(member_id);

CREATE INDEX idx_groups_business_member_id ON groups(business_member_id);
|]

down_m20241128_business_chats :: Query
down_m20241128_business_chats =
  [sql|
DROP INDEX idx_groups_business_member_id;

ALTER TABLE user_contact_links DROP COLUMN business;
ALTER TABLE connections DROP COLUMN business;
ALTER TABLE groups DROP COLUMN business_chat;
ALTER TABLE groups DROP COLUMN business_member_id;
|]
