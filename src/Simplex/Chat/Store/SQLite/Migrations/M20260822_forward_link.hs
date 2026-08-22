{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260822_forward_link where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260822_forward_link :: Query
m20260822_forward_link =
  [sql|
ALTER TABLE chat_items ADD COLUMN fwd_chat_link_shared INTEGER;
ALTER TABLE chat_items ADD COLUMN fwd_from_group_link BLOB;
ALTER TABLE chat_items ADD COLUMN fwd_from_public_group_id BLOB;
ALTER TABLE chat_items ADD COLUMN fwd_from_shared_msg_id BLOB;
|]

down_m20260822_forward_link :: Query
down_m20260822_forward_link =
  [sql|
ALTER TABLE chat_items DROP COLUMN fwd_chat_link_shared;
ALTER TABLE chat_items DROP COLUMN fwd_from_group_link;
ALTER TABLE chat_items DROP COLUMN fwd_from_public_group_id;
ALTER TABLE chat_items DROP COLUMN fwd_from_shared_msg_id;
|]
