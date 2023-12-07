{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231207_mark_direct_contacts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231207_mark_direct_contacts :: Query
m20231207_mark_direct_contacts =
  [sql|
    UPDATE contacts SET contact_used = 1
    WHERE contact_id = (
      SELECT contact_id FROM connections
      WHERE conn_level = 0 AND via_group_link = 0
    )
  |]
