{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Store.Migrations (migrations) where

import Data.List (sortOn)
import Database.SQLite.Simple (Query (..))
import Simplex.Chat.Migrations.M20220101_initial
import Simplex.Chat.Migrations.M20220122_v1_1
import Simplex.Chat.Migrations.M20220205_chat_item_status
import Simplex.Chat.Migrations.M20220210_deduplicate_contact_requests
import Simplex.Chat.Migrations.M20220224_messages_fks
import Simplex.Chat.Migrations.M20220301_smp_servers
import Simplex.Chat.Migrations.M20220302_profile_images
import Simplex.Chat.Migrations.M20220304_msg_quotes
import Simplex.Chat.Migrations.M20220321_chat_item_edited
import Simplex.Chat.Migrations.M20220404_files_status_fields
import Simplex.Chat.Migrations.M20220514_profiles_user_id
import Simplex.Chat.Migrations.M20220626_auto_reply
import Simplex.Chat.Migrations.M20220702_calls
import Simplex.Chat.Migrations.M20220715_groups_chat_item_id
import Simplex.Chat.Migrations.M20220811_chat_items_indices
import Simplex.Chat.Migrations.M20220812_incognito_profiles
import Simplex.Chat.Migrations.M20220818_chat_notifications
import Simplex.Chat.Migrations.M20220822_groups_host_conn_custom_user_profile_id
import Simplex.Chat.Migrations.M20220823_delete_broken_group_event_chat_items
import Simplex.Chat.Migrations.M20220824_profiles_local_alias
import Simplex.Chat.Migrations.M20220909_commands
import Simplex.Chat.Migrations.M20220926_connection_alias
import Simplex.Chat.Migrations.M20220928_settings
import Simplex.Chat.Migrations.M20221001_shared_msg_id_indices
import Simplex.Chat.Migrations.M20221003_delete_broken_integrity_error_chat_items
import Simplex.Chat.Migrations.M20221004_idx_msg_deliveries_message_id
import Simplex.Chat.Migrations.M20221011_user_contact_links_group_id
import Simplex.Chat.Migrations.M20221012_inline_files
import Simplex.Chat.Migrations.M20221019_unread_chat
import Simplex.Chat.Migrations.M20221021_auto_accept__group_links
import Simplex.Chat.Migrations.M20221024_contact_used
import Simplex.Chat.Migrations.M20221025_chat_settings
import Simplex.Chat.Migrations.M20221029_group_link_id
import Simplex.Chat.Migrations.M20221112_server_password
import Simplex.Chat.Migrations.M20221115_server_cfg
import Simplex.Chat.Migrations.M20221129_delete_group_feature_items
import Simplex.Chat.Migrations.M20221130_delete_item_deleted
import Simplex.Chat.Migrations.M20221209_verified_connection
import Simplex.Chat.Migrations.M20221210_idxs
import Simplex.Chat.Migrations.M20221211_group_description
import Simplex.Chat.Migrations.M20221212_chat_items_timed
import Simplex.Chat.Migrations.M20221214_live_message
import Simplex.Chat.Migrations.M20221222_chat_ts
import Simplex.Chat.Migrations.M20221223_idx_chat_items_item_status
import Simplex.Chat.Migrations.M20221230_idxs
import Simplex.Chat.Migrations.M20230107_connections_auth_err_counter
import Simplex.Chat.Migrations.M20230111_users_agent_user_id
import Simplex.Chat.Migrations.M20230117_fkey_indexes
import Simplex.Chat.Migrations.M20230118_recreate_smp_servers
import Simplex.Chat.Migrations.M20230129_drop_chat_items_group_idx
import Simplex.Chat.Migrations.M20230206_item_deleted_by_group_member_id
import Simplex.Chat.Migrations.M20230303_group_link_role
import Simplex.Chat.Migrations.M20230317_hidden_profiles
import Simplex.Chat.Migrations.M20230318_file_description
import Simplex.Chat.Migrations.M20230321_agent_file_deleted
import Simplex.Chat.Migrations.M20230328_files_protocol
import Simplex.Chat.Migrations.M20230402_protocol_servers
import Simplex.Chat.Migrations.M20230411_extra_xftp_file_descriptions
import Simplex.Chat.Migrations.M20230420_rcv_files_to_receive
import Simplex.Chat.Migrations.M20230422_profile_contact_links
import Simplex.Chat.Migrations.M20230504_recreate_msg_delivery_events_cleanup_messages
import Simplex.Chat.Migrations.M20230505_chat_item_versions
import Simplex.Chat.Migrations.M20230511_reactions
import Simplex.Chat.Migrations.M20230519_item_deleted_ts
import Simplex.Chat.Migrations.M20230526_indexes
import Simplex.Chat.Migrations.M20230529_indexes
import Simplex.Chat.Migrations.M20230608_deleted_contacts
import Simplex.Chat.Migrations.M20230618_favorite_chats
import Simplex.Chat.Migrations.M20230621_chat_item_moderations
import Simplex.Chat.Migrations.M20230705_delivery_receipts
import Simplex.Chat.Migrations.M20230721_group_snd_item_statuses
import Simplex.Chat.Migrations.M20230814_indexes
import Simplex.Chat.Migrations.M20230827_file_encryption
import Simplex.Chat.Migrations.M20230829_connections_chat_vrange
import Simplex.Chat.Migrations.M20230903_connections_to_subscribe
import Simplex.Chat.Migrations.M20230913_member_contacts
import Simplex.Chat.Migrations.M20230914_member_probes
import Simplex.Chat.Migrations.M20230926_contact_status
import Simplex.Chat.Migrations.M20231002_conn_initiated
import Simplex.Chat.Migrations.M20231009_via_group_link_uri_hash
import Simplex.Chat.Migrations.M20231010_member_settings
import Simplex.Chat.Migrations.M20231019_indexes
import Simplex.Chat.Migrations.M20231030_xgrplinkmem_received
import Simplex.Chat.Migrations.M20231107_indexes
import Simplex.Chat.Migrations.M20231113_group_forward
import Simplex.Chat.Migrations.M20231114_remote_control
import Simplex.Chat.Migrations.M20231126_remote_ctrl_address
import Simplex.Chat.Migrations.M20231207_chat_list_pagination
import Simplex.Chat.Migrations.M20231214_item_content_tag
import Simplex.Chat.Migrations.M20231215_recreate_msg_deliveries
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))

schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("20220101_initial", m20220101_initial, Nothing),
    ("20220122_v1_1", m20220122_v1_1, Nothing),
    ("20220205_chat_item_status", m20220205_chat_item_status, Nothing),
    ("20220210_deduplicate_contact_requests", m20220210_deduplicate_contact_requests, Nothing),
    ("20220224_messages_fks", m20220224_messages_fks, Nothing),
    ("20220301_smp_servers", m20220301_smp_servers, Nothing),
    ("20220302_profile_images", m20220302_profile_images, Nothing),
    ("20220304_msg_quotes", m20220304_msg_quotes, Nothing),
    ("20220321_chat_item_edited", m20220321_chat_item_edited, Nothing),
    ("20220404_files_status_fields", m20220404_files_status_fields, Nothing),
    ("20220514_profiles_user_id", m20220514_profiles_user_id, Nothing),
    ("20220626_auto_reply", m20220626_auto_reply, Nothing),
    ("20220702_calls", m20220702_calls, Nothing),
    ("20220715_groups_chat_item_id", m20220715_groups_chat_item_id, Nothing),
    ("20220811_chat_items_indices", m20220811_chat_items_indices, Nothing),
    ("20220812_incognito_profiles", m20220812_incognito_profiles, Nothing),
    ("20220818_chat_notifications", m20220818_chat_notifications, Nothing),
    ("20220822_groups_host_conn_custom_user_profile_id", m20220822_groups_host_conn_custom_user_profile_id, Nothing),
    ("20220823_delete_broken_group_event_chat_items", m20220823_delete_broken_group_event_chat_items, Nothing),
    ("20220824_profiles_local_alias", m20220824_profiles_local_alias, Nothing),
    ("20220909_commands", m20220909_commands, Nothing),
    ("20220926_connection_alias", m20220926_connection_alias, Nothing),
    ("20220928_settings", m20220928_settings, Nothing),
    ("20221001_shared_msg_id_indices", m20221001_shared_msg_id_indices, Nothing),
    ("20221003_delete_broken_integrity_error_chat_items", m20221003_delete_broken_integrity_error_chat_items, Nothing),
    ("20221004_idx_msg_deliveries_message_id", m20221004_idx_msg_deliveries_message_id, Nothing),
    ("20221011_user_contact_links_group_id", m20221011_user_contact_links_group_id, Nothing),
    ("20221012_inline_files", m20221012_inline_files, Nothing),
    ("20221019_unread_chat", m20221019_unread_chat, Nothing),
    ("20221021_auto_accept__group_links", m20221021_auto_accept__group_links, Nothing),
    ("20221024_contact_used", m20221024_contact_used, Nothing),
    ("20221025_chat_settings", m20221025_chat_settings, Nothing),
    ("20221029_group_link_id", m20221029_group_link_id, Nothing),
    ("20221112_server_password", m20221112_server_password, Nothing),
    ("20221115_server_cfg", m20221115_server_cfg, Nothing),
    ("20221129_delete_group_feature_items", m20221129_delete_group_feature_items, Nothing),
    ("20221130_delete_item_deleted", m20221130_delete_item_deleted, Nothing),
    ("20221209_verified_connection", m20221209_verified_connection, Nothing),
    ("20221210_idxs", m20221210_idxs, Nothing),
    ("20221211_group_description", m20221211_group_description, Nothing),
    ("20221212_chat_items_timed", m20221212_chat_items_timed, Nothing),
    ("20221214_live_message", m20221214_live_message, Nothing),
    ("20221222_chat_ts", m20221222_chat_ts, Nothing),
    ("20221223_idx_chat_items_item_status", m20221223_idx_chat_items_item_status, Nothing),
    ("20221230_idxs", m20221230_idxs, Nothing),
    ("20230107_connections_auth_err_counter", m20230107_connections_auth_err_counter, Nothing),
    ("20230111_users_agent_user_id", m20230111_users_agent_user_id, Nothing),
    ("20230117_fkey_indexes", m20230117_fkey_indexes, Nothing),
    ("20230118_recreate_smp_servers", m20230118_recreate_smp_servers, Nothing),
    ("20230129_drop_chat_items_group_idx", m20230129_drop_chat_items_group_idx, Nothing),
    ("20230206_item_deleted_by_group_member_id", m20230206_item_deleted_by_group_member_id, Nothing),
    ("20230303_group_link_role", m20230303_group_link_role, Nothing),
    ("20230317_hidden_profiles", m20230317_hidden_profiles, Just down_m20230317_hidden_profiles),
    ("20230318_file_description", m20230318_file_description, Just down_m20230318_file_description),
    ("20230321_agent_file_deleted", m20230321_agent_file_deleted, Just down_m20230321_agent_file_deleted),
    ("20230328_files_protocol", m20230328_files_protocol, Just down_m20230328_files_protocol),
    ("20230402_protocol_servers", m20230402_protocol_servers, Just down_m20230402_protocol_servers),
    ("20230411_extra_xftp_file_descriptions", m20230411_extra_xftp_file_descriptions, Just down_m20230411_extra_xftp_file_descriptions),
    ("20230420_rcv_files_to_receive", m20230420_rcv_files_to_receive, Just down_m20230420_rcv_files_to_receive),
    ("20230422_profile_contact_links", m20230422_profile_contact_links, Just down_m20230422_profile_contact_links),
    ("20230504_recreate_msg_delivery_events_cleanup_messages", m20230504_recreate_msg_delivery_events_cleanup_messages, Just down_m20230504_recreate_msg_delivery_events_cleanup_messages),
    ("20230505_chat_item_versions", m20230505_chat_item_versions, Just down_m20230505_chat_item_versions),
    ("20230511_reactions", m20230511_reactions, Just down_m20230511_reactions),
    ("20230519_item_deleted_ts", m20230519_item_deleted_ts, Just down_m20230519_item_deleted_ts),
    ("20230526_indexes", m20230526_indexes, Just down_m20230526_indexes),
    ("20230529_indexes", m20230529_indexes, Just down_m20230529_indexes),
    ("20230608_deleted_contacts", m20230608_deleted_contacts, Just down_m20230608_deleted_contacts),
    ("20230618_favorite_chats", m20230618_favorite_chats, Just down_m20230618_favorite_chats),
    ("20230621_chat_item_moderations", m20230621_chat_item_moderations, Just down_m20230621_chat_item_moderations),
    ("20230705_delivery_receipts", m20230705_delivery_receipts, Just down_m20230705_delivery_receipts),
    ("20230721_group_snd_item_statuses", m20230721_group_snd_item_statuses, Just down_m20230721_group_snd_item_statuses),
    ("20230814_indexes", m20230814_indexes, Just down_m20230814_indexes),
    ("20230827_file_encryption", m20230827_file_encryption, Just down_m20230827_file_encryption),
    ("20230829_connections_chat_vrange", m20230829_connections_chat_vrange, Just down_m20230829_connections_chat_vrange),
    ("20230903_connections_to_subscribe", m20230903_connections_to_subscribe, Just down_m20230903_connections_to_subscribe),
    ("20230913_member_contacts", m20230913_member_contacts, Just down_m20230913_member_contacts),
    ("20230914_member_probes", m20230914_member_probes, Just down_m20230914_member_probes),
    ("20230926_contact_status", m20230926_contact_status, Just down_m20230926_contact_status),
    ("20231002_conn_initiated", m20231002_conn_initiated, Just down_m20231002_conn_initiated),
    ("20231009_via_group_link_uri_hash", m20231009_via_group_link_uri_hash, Just down_m20231009_via_group_link_uri_hash),
    ("20231010_member_settings", m20231010_member_settings, Just down_m20231010_member_settings),
    ("20231019_indexes", m20231019_indexes, Just down_m20231019_indexes),
    ("20231030_xgrplinkmem_received", m20231030_xgrplinkmem_received, Just down_m20231030_xgrplinkmem_received),
    ("20231107_indexes", m20231107_indexes, Just down_m20231107_indexes),
    ("20231113_group_forward", m20231113_group_forward, Just down_m20231113_group_forward),
    ("20231114_remote_control", m20231114_remote_control, Just down_m20231114_remote_control),
    ("20231126_remote_ctrl_address", m20231126_remote_ctrl_address, Just down_m20231126_remote_ctrl_address),
    ("20231207_chat_list_pagination", m20231207_chat_list_pagination, Just down_m20231207_chat_list_pagination),
    ("20231214_item_content_tag", m20231214_item_content_tag, Just down_m20231214_item_content_tag),
    ("20231215_recreate_msg_deliveries", m20231215_recreate_msg_deliveries, Just down_m20231215_recreate_msg_deliveries)
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up = fromQuery up, down = fromQuery <$> down}
