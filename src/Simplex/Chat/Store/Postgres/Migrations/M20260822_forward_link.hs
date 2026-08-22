{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260822_forward_link where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260822_forward_link :: Text
m20260822_forward_link =
  [r|
ALTER TABLE chat_items ADD COLUMN fwd_chat_link_shared SMALLINT;
ALTER TABLE chat_items ADD COLUMN fwd_from_group_link BYTEA;
ALTER TABLE chat_items ADD COLUMN fwd_from_public_group_id BYTEA;
ALTER TABLE chat_items ADD COLUMN fwd_from_simplex_name TEXT;
ALTER TABLE chat_items ADD COLUMN fwd_from_shared_msg_id BYTEA;
|]

down_m20260822_forward_link :: Text
down_m20260822_forward_link =
  [r|
ALTER TABLE chat_items DROP COLUMN fwd_chat_link_shared;
ALTER TABLE chat_items DROP COLUMN fwd_from_group_link;
ALTER TABLE chat_items DROP COLUMN fwd_from_public_group_id;
ALTER TABLE chat_items DROP COLUMN fwd_from_simplex_name;
ALTER TABLE chat_items DROP COLUMN fwd_from_shared_msg_id;
|]
