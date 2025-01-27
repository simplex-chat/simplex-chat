{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20231107_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231107_indexes :: Query
m20231107_indexes =
  [sql|
CREATE INDEX idx_contact_profiles_contact_link ON contact_profiles(user_id, contact_link);
|]

down_m20231107_indexes :: Query
down_m20231107_indexes =
  [sql|
DROP INDEX idx_contact_profiles_contact_link;
|]
