{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250205_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250205_indexes :: Query
m20250205_indexes =
  [sql|
DROP INDEX idx_group_members_group_id;
CREATE INDEX idx_group_members_group_id ON group_members(group_id);
CREATE INDEX idx_group_members_group_id_contact_id ON group_members(group_id, contact_id);
|]

down_m20250205_indexes :: Query
down_m20250205_indexes =
  [sql|
DROP INDEX idx_group_members_group_id;
DROP INDEX idx_group_members_group_id_contact_id;
CREATE INDEX idx_group_members_group_id ON group_members(user_id, group_id);
|]
