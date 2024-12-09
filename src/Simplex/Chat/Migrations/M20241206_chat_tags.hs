{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241206_chat_tags where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241205_business_chat_members :: Query
m20241205_business_chat_members =
  [sql|
CREATE TABLE chat_tags (
  chat_tag_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER REFERENCES users;
  chat_tag_text TEXT NOT NULL UNIQUE,
  chat_tag_emoji TEXT NOT NULL UNIQUE
);

ALTER TABLE contacts ADD COLUMN chat_tag_id REFERENCES chat_tags;
ALTER TABLE groups ADD COLUMN chat_tag_id REFERENCES chat_tags;

CREATE INDEX idx_chat_tags_user_id ON chat_tags(user_id);
CREATE INDEX idx_contacts_chat_tag_id ON contacts(chat_tag_id);
CREATE INDEX idx_groups_chat_tag_id ON groups(chat_tag_id);
|]


down_m20241205_business_chat_members :: Query
down_m20241205_business_chat_members =
  [sql|
DROP INDEX idx_contacts_chat_tag_id;
DROP INDEX idx_groups_chat_tag_id;

ALTER TABLE contacts DROP COLUMN;
ALTER TABLE groups DROP COLUMN;

DROP TABLE chat_tags
|]
