{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230117_fkey_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- .lint fkey-indexes
m20230117_fkey_indexes :: Query
m20230117_fkey_indexes =
  [sql|
CREATE INDEX idx_calls_user_id ON calls(user_id);
CREATE INDEX idx_calls_chat_item_id ON calls(chat_item_id);
CREATE INDEX idx_calls_contact_id ON calls(contact_id);
CREATE INDEX idx_chat_items_group_id ON chat_items(group_id);
CREATE INDEX idx_commands_user_id ON commands(user_id);
CREATE INDEX idx_connections_custom_user_profile_id ON connections(custom_user_profile_id);
CREATE INDEX idx_connections_via_user_contact_link ON connections(via_user_contact_link);
CREATE INDEX idx_connections_rcv_file_id ON connections(rcv_file_id);
CREATE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE INDEX idx_connections_user_contact_link_id ON connections(user_contact_link_id);
CREATE INDEX idx_connections_via_contact ON connections(via_contact);
CREATE INDEX idx_contact_profiles_user_id ON contact_profiles(user_id);
CREATE INDEX idx_contact_requests_contact_profile_id ON contact_requests(contact_profile_id);
CREATE INDEX idx_contact_requests_user_contact_link_id ON contact_requests(user_contact_link_id);
CREATE INDEX idx_contacts_via_group ON contacts(via_group);
CREATE INDEX idx_contacts_contact_profile_id ON contacts(contact_profile_id);
CREATE INDEX idx_files_chat_item_id ON files(chat_item_id);
CREATE INDEX idx_files_user_id ON files(user_id);
CREATE INDEX idx_files_group_id ON files(group_id);
CREATE INDEX idx_files_contact_id ON files(contact_id);
CREATE INDEX idx_group_member_intros_to_group_member_id ON group_member_intros(to_group_member_id);
CREATE INDEX idx_group_members_user_id_local_display_name ON group_members(user_id, local_display_name);
CREATE INDEX idx_group_members_member_profile_id ON group_members(member_profile_id);
CREATE INDEX idx_group_members_contact_id ON group_members(contact_id);
CREATE INDEX idx_group_members_contact_profile_id ON group_members(contact_profile_id);
CREATE INDEX idx_group_members_user_id ON group_members(user_id);
CREATE INDEX idx_group_members_invited_by ON group_members(invited_by);
CREATE INDEX idx_group_profiles_user_id ON group_profiles(user_id);
CREATE INDEX idx_groups_host_conn_custom_user_profile_id ON groups(host_conn_custom_user_profile_id);
CREATE INDEX idx_groups_chat_item_id ON groups(chat_item_id);
CREATE INDEX idx_groups_group_profile_id ON groups(group_profile_id);
CREATE INDEX idx_messages_group_id ON messages(group_id);
CREATE INDEX idx_pending_group_messages_group_member_intro_id ON pending_group_messages(group_member_intro_id);
CREATE INDEX idx_pending_group_messages_message_id ON pending_group_messages(message_id);
CREATE INDEX idx_pending_group_messages_group_member_id ON pending_group_messages(group_member_id);
CREATE INDEX idx_rcv_file_chunks_file_id ON rcv_file_chunks(file_id);
CREATE INDEX idx_rcv_files_group_member_id ON rcv_files(group_member_id);
CREATE INDEX idx_received_probes_user_id ON received_probes(user_id);
CREATE INDEX idx_received_probes_contact_id ON received_probes(contact_id);
CREATE INDEX idx_sent_probe_hashes_user_id ON sent_probe_hashes(user_id);
CREATE INDEX idx_sent_probe_hashes_contact_id ON sent_probe_hashes(contact_id);
CREATE INDEX idx_settings_user_id ON settings(user_id);
CREATE INDEX idx_smp_servers_user_id ON smp_servers(user_id);
CREATE INDEX idx_snd_file_chunks_file_id_connection_id ON snd_file_chunks(file_id, connection_id);
CREATE INDEX idx_snd_files_group_member_id ON snd_files(group_member_id);
CREATE INDEX idx_snd_files_connection_id ON snd_files(connection_id);
CREATE INDEX idx_snd_files_file_id ON snd_files(file_id);
|]
