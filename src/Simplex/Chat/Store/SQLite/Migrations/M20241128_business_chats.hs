{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241128_business_chats where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241128_business_chats :: Query
m20241128_business_chats =
  [sql|
ALTER TABLE user_contact_links ADD business_address INTEGER DEFAULT 0;
ALTER TABLE groups ADD COLUMN business_member_id BLOB NULL;
ALTER TABLE groups ADD COLUMN business_chat TEXT NULL;
ALTER TABLE groups ADD COLUMN business_xcontact_id BLOB NULL;

CREATE INDEX idx_groups_business_xcontact_id ON groups(business_xcontact_id);
|]

down_m20241128_business_chats :: Query
down_m20241128_business_chats =
  [sql|
DROP INDEX idx_groups_business_xcontact_id;

ALTER TABLE user_contact_links DROP COLUMN business_address;
ALTER TABLE groups DROP COLUMN business_member_id;
ALTER TABLE groups DROP COLUMN business_chat;
ALTER TABLE groups DROP COLUMN business_xcontact_id;
|]
