{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260514_relay_request_group_link_index where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260514_relay_request_group_link_index :: Query
m20260514_relay_request_group_link_index =
  [sql|
CREATE INDEX idx_groups_relay_request_group_link
  ON groups(user_id, relay_request_group_link)
  WHERE relay_request_group_link IS NOT NULL;
|]

down_m20260514_relay_request_group_link_index :: Query
down_m20260514_relay_request_group_link_index =
  [sql|
DROP INDEX idx_groups_relay_request_group_link;
|]
