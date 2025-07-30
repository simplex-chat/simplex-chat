{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Store.Postgres.Migrations (migrations) where

import Data.List (sortOn)
import Data.Text (Text)
import Simplex.Chat.Store.Postgres.Migrations.M20241220_initial
import Simplex.Chat.Store.Postgres.Migrations.M20250402_short_links
import Simplex.Chat.Store.Postgres.Migrations.M20250512_member_admission
import Simplex.Chat.Store.Postgres.Migrations.M20250513_group_scope
import Simplex.Chat.Store.Postgres.Migrations.M20250526_short_links
import Simplex.Chat.Store.Postgres.Migrations.M20250702_contact_requests_remove_cascade_delete
import Simplex.Chat.Store.Postgres.Migrations.M20250704_groups_conn_link_prepared_connection
import Simplex.Chat.Store.Postgres.Migrations.M20250709_profile_short_descr
import Simplex.Chat.Store.Postgres.Migrations.M20250721_indexes
import Simplex.Messaging.Agent.Store.Shared (Migration (..))

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20241220_initial", m20241220_initial, Nothing),
    ("20250402_short_links", m20250402_short_links, Just down_m20250402_short_links),
    ("20250512_member_admission", m20250512_member_admission, Just down_m20250512_member_admission),
    ("20250513_group_scope", m20250513_group_scope, Just down_m20250513_group_scope),
    ("20250526_short_links", m20250526_short_links, Just down_m20250526_short_links),
    ("20250702_contact_requests_remove_cascade_delete", m20250702_contact_requests_remove_cascade_delete, Just down_m20250702_contact_requests_remove_cascade_delete),
    ("20250704_groups_conn_link_prepared_connection", m20250704_groups_conn_link_prepared_connection, Just down_m20250704_groups_conn_link_prepared_connection),
    ("20250709_profile_short_descr", m20250709_profile_short_descr, Just down_m20250709_profile_short_descr),
    ("20250721_indexes", m20250721_indexes, Just down_m20250721_indexes)
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}
