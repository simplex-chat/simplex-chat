{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250721_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250721_indexes :: Query
m20250721_indexes =
  [sql|
DROP INDEX idx_contact_requests_xcontact_id;

CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests(user_id, xcontact_id);

CREATE INDEX idx_chat_items_group_scope_stats_all ON chat_items (
  user_id,
  group_id,
  group_scope_tag,
  group_scope_group_member_id,
  item_status,
  chat_item_id,
  user_mention
);
|]

down_m20250721_indexes :: Query
down_m20250721_indexes =
  [sql|
DROP INDEX idx_contact_requests_xcontact_id;

CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests(xcontact_id);

DROP INDEX idx_chat_items_group_scope_stats_all;
|]
