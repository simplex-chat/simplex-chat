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
import Simplex.Chat.Store.Postgres.Migrations.M20250729_member_contact_requests
import Simplex.Chat.Store.Postgres.Migrations.M20250801_via_group_link_uri
import Simplex.Chat.Store.Postgres.Migrations.M20250802_chat_peer_type
import Simplex.Chat.Store.Postgres.Migrations.M20250813_delivery_tasks
import Simplex.Chat.Store.Postgres.Migrations.M20250919_group_summary
import Simplex.Chat.Store.Postgres.Migrations.M20250922_remove_unused_connections
import Simplex.Chat.Store.Postgres.Migrations.M20251007_connections_sync
import Simplex.Chat.Store.Postgres.Migrations.M20251017_chat_tags_cascade
import Simplex.Chat.Store.Postgres.Migrations.M20251117_member_relations_vector
import Simplex.Chat.Store.Postgres.Migrations.M20251128_migrate_member_relations
import Simplex.Chat.Store.Postgres.Migrations.M20251230_strict_tables
import Simplex.Chat.Store.Postgres.Migrations.M20260108_chat_indices
import Simplex.Chat.Store.Postgres.Migrations.M20260122_has_link
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
    ("20250721_indexes", m20250721_indexes, Just down_m20250721_indexes),
    ("20250729_member_contact_requests", m20250729_member_contact_requests, Just down_m20250729_member_contact_requests),
    ("20250801_via_group_link_uri", m20250801_via_group_link_uri, Just down_m20250801_via_group_link_uri),
    ("20250802_chat_peer_type", m20250802_chat_peer_type, Just down_m20250802_chat_peer_type),
    ("20250813_delivery_tasks", m20250813_delivery_tasks, Just down_m20250813_delivery_tasks),
    ("20250919_group_summary", m20250919_group_summary, Just down_m20250919_group_summary),
    ("20250922_remove_unused_connections", m20250922_remove_unused_connections, Just down_m20250922_remove_unused_connections),
    ("20251007_connections_sync", m20251007_connections_sync, Just down_m20251007_connections_sync),
    ("20251017_chat_tags_cascade", m20251017_chat_tags_cascade, Just down_m20251017_chat_tags_cascade),
    ("20251117_member_relations_vector", m20251117_member_relations_vector, Just down_m20251117_member_relations_vector),
    ("20251128_migrate_member_relations", m20251128_migrate_member_relations, Just down_m20251128_migrate_member_relations),
    ("20251230_strict_tables", m20251230_strict_tables, Just down_m20251230_strict_tables),
    ("20260108_chat_indices", m20260108_chat_indices, Just down_m20260108_chat_indices),
    ("20260122_has_link", m20260122_has_link, Just down_m20260122_has_link)
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}
