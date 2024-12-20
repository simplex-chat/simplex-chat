{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220626_auto_reply where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220626_auto_reply :: Query
m20220626_auto_reply =
  [sql|
ALTER TABLE user_contact_links ADD COLUMN auto_reply_msg_content TEXT DEFAULT NULL;

ALTER TABLE connections ADD COLUMN via_user_contact_link INTEGER DEFAULT NULL
  REFERENCES user_contact_links (user_contact_link_id) ON DELETE SET NULL;
|]
