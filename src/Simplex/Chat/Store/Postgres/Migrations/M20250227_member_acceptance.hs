{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250227_member_acceptance where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250227_member_acceptance :: Text
m20250227_member_acceptance =
  T.pack
    [r|
DROP INDEX idx_chat_items_groups_history;
CREATE INDEX idx_chat_items_groups_history ON chat_items(
  user_id,
  group_id,
  include_in_history,
  item_deleted,
  group_member_id,
  item_ts,
  chat_item_id
);
|]

down_m20250227_member_acceptance :: Text
down_m20250227_member_acceptance =
  T.pack
    [r|
DROP INDEX idx_chat_items_groups_history;
CREATE INDEX idx_chat_items_groups_history ON chat_items(
  user_id,
  group_id,
  include_in_history,
  item_deleted,
  item_ts,
  chat_item_id
);
|]
