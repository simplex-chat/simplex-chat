{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20241220_initial where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20241220_initial :: Text
m20241220_initial =
  T.pack
    [r|
CREATE TABLE users(
  user_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  contact_id BIGINT NOT NULL UNIQUE,
  local_display_name TEXT NOT NULL UNIQUE,
  active_user SMALLINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  agent_user_id BIGINT NOT NULL,
  view_pwd_hash BYTEA,
  view_pwd_salt BYTEA,
  show_ntfs SMALLINT NOT NULL DEFAULT 1,
  send_rcpts_contacts SMALLINT NOT NULL DEFAULT 0,
  send_rcpts_small_groups SMALLINT NOT NULL DEFAULT 0,
  user_member_profile_updated_at TIMESTAMPTZ,
  ui_themes TEXT,
  active_order BIGINT NOT NULL DEFAULT 0
);
CREATE TABLE contact_profiles(
  contact_profile_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  display_name TEXT NOT NULL,
  full_name TEXT NOT NULL,
  properties TEXT NOT NULL DEFAULT '{}',
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  image TEXT,
  user_id BIGINT DEFAULT NULL REFERENCES users ON DELETE CASCADE,
  incognito SMALLINT,
  local_alias TEXT NOT NULL DEFAULT '',
  preferences TEXT,
  contact_link BYTEA
);
CREATE TABLE display_names(
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  local_display_name TEXT NOT NULL,
  ldn_base TEXT NOT NULL,
  ldn_suffix BIGINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(user_id, local_display_name),
  UNIQUE(user_id, ldn_base, ldn_suffix)
);
ALTER TABLE users
ADD CONSTRAINT fk_users_display_names
  FOREIGN KEY(user_id, local_display_name)
  REFERENCES display_names(user_id, local_display_name)
  ON DELETE RESTRICT
  ON UPDATE CASCADE
  DEFERRABLE INITIALLY DEFERRED;
CREATE TABLE contacts(
  contact_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  contact_profile_id BIGINT REFERENCES contact_profiles ON DELETE SET NULL,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  local_display_name TEXT NOT NULL,
  is_user SMALLINT NOT NULL DEFAULT 0,
  via_group BIGINT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL,
  xcontact_id BYTEA,
  enable_ntfs SMALLINT,
  unread_chat SMALLINT NOT NULL DEFAULT 0,
  contact_used SMALLINT NOT NULL DEFAULT 0,
  user_preferences TEXT NOT NULL DEFAULT '{}',
  chat_ts TIMESTAMPTZ,
  deleted SMALLINT NOT NULL DEFAULT 0,
  favorite SMALLINT NOT NULL DEFAULT 0,
  send_rcpts SMALLINT,
  contact_group_member_id BIGINT,
  contact_grp_inv_sent SMALLINT NOT NULL DEFAULT 0,
  contact_status TEXT NOT NULL DEFAULT 'active',
  custom_data BYTEA,
  ui_themes TEXT,
  chat_deleted SMALLINT NOT NULL DEFAULT 0,
  chat_item_ttl BIGINT,
  FOREIGN KEY(user_id, local_display_name)
  REFERENCES display_names(user_id, local_display_name)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
  UNIQUE(user_id, local_display_name),
  UNIQUE(user_id, contact_profile_id)
);
ALTER TABLE users
ADD CONSTRAINT fk_users_contacts
  FOREIGN KEY(contact_id)
  REFERENCES contacts(contact_id)
  ON DELETE RESTRICT
  DEFERRABLE INITIALLY DEFERRED;
CREATE TABLE known_servers(
  server_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BYTEA,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  UNIQUE(user_id, host, port)
);
CREATE TABLE group_profiles(
  group_profile_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  display_name TEXT NOT NULL,
  full_name TEXT NOT NULL,
  properties TEXT NOT NULL DEFAULT '{}',
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  image TEXT,
  user_id BIGINT DEFAULT NULL REFERENCES users ON DELETE CASCADE,
  preferences TEXT,
  description TEXT NULL
);
CREATE TABLE groups(
  group_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  local_display_name TEXT NOT NULL,
  group_profile_id BIGINT REFERENCES group_profiles ON DELETE SET NULL,
  inv_queue_info BYTEA,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  chat_item_id BIGINT DEFAULT NULL,
  enable_ntfs SMALLINT,
  unread_chat SMALLINT NOT NULL DEFAULT 0,
  chat_ts TIMESTAMPTZ,
  favorite SMALLINT NOT NULL DEFAULT 0,
  send_rcpts SMALLINT,
  via_group_link_uri_hash BYTEA,
  user_member_profile_sent_at TIMESTAMPTZ,
  custom_data BYTEA,
  ui_themes TEXT,
  business_member_id BYTEA NULL,
  business_chat TEXT NULL,
  business_xcontact_id BYTEA NULL,
  customer_member_id BYTEA NULL,
  chat_item_ttl BIGINT,
  local_alias TEXT DEFAULT '',
  FOREIGN KEY(user_id, local_display_name)
  REFERENCES display_names(user_id, local_display_name)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
  UNIQUE(user_id, local_display_name),
  UNIQUE(user_id, group_profile_id)
);
ALTER TABLE contacts
ADD CONSTRAINT fk_contacts_groups
  FOREIGN KEY(via_group)
  REFERENCES groups(group_id) ON DELETE SET NULL;
CREATE TABLE group_members(
  group_member_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_id BIGINT NOT NULL REFERENCES groups ON DELETE CASCADE,
  member_id BYTEA NOT NULL,
  member_role TEXT NOT NULL,
  member_category TEXT NOT NULL,
  member_status TEXT NOT NULL,
  invited_by BIGINT REFERENCES contacts(contact_id) ON DELETE SET NULL,
  sent_inv_queue_info BYTEA,
  group_queue_info BYTEA,
  direct_queue_info BYTEA,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  local_display_name TEXT NOT NULL,
  contact_profile_id BIGINT NOT NULL REFERENCES contact_profiles ON DELETE CASCADE,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  member_profile_id BIGINT REFERENCES contact_profiles ON DELETE SET NULL,
  show_messages SMALLINT NOT NULL DEFAULT 1,
  xgrplinkmem_received SMALLINT NOT NULL DEFAULT 0,
  invited_by_group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL,
  peer_chat_min_version INTEGER NOT NULL DEFAULT 1,
  peer_chat_max_version INTEGER NOT NULL DEFAULT 1,
  member_restriction TEXT,
  FOREIGN KEY(user_id, local_display_name)
  REFERENCES display_names(user_id, local_display_name)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
  UNIQUE(group_id, member_id)
);
ALTER TABLE contacts
ADD CONSTRAINT fk_contacts_group_members
  FOREIGN KEY(contact_group_member_id)
  REFERENCES group_members(group_member_id) ON DELETE SET NULL;
CREATE TABLE group_member_intros(
  group_member_intro_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  re_group_member_id BIGINT NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  to_group_member_id BIGINT NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  group_queue_info BYTEA,
  direct_queue_info BYTEA,
  intro_status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  intro_chat_protocol_version INTEGER NOT NULL DEFAULT 3,
  UNIQUE(re_group_member_id, to_group_member_id)
);
CREATE TABLE files(
  file_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_id BIGINT REFERENCES groups ON DELETE CASCADE,
  file_name TEXT NOT NULL,
  file_path TEXT,
  file_size BIGINT NOT NULL,
  chunk_size BIGINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  chat_item_id BIGINT DEFAULT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  cancelled SMALLINT,
  ci_file_status TEXT,
  file_inline TEXT,
  agent_snd_file_id BYTEA NULL,
  private_snd_file_descr TEXT NULL,
  agent_snd_file_deleted SMALLINT NOT NULL DEFAULT 0,
  protocol TEXT NOT NULL DEFAULT 'smp',
  file_crypto_key BYTEA,
  file_crypto_nonce BYTEA,
  note_folder_id BIGINT DEFAULT NULL,
  redirect_file_id BIGINT REFERENCES files ON DELETE CASCADE
);
CREATE TABLE snd_files(
  file_id BIGINT NOT NULL REFERENCES files ON DELETE CASCADE,
  connection_id BIGINT NOT NULL,
  file_status TEXT NOT NULL,
  group_member_id BIGINT REFERENCES group_members ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  file_inline TEXT,
  last_inline_msg_delivery_id BIGINT,
  file_descr_id BIGINT NULL,
  PRIMARY KEY(file_id, connection_id)
);
CREATE TABLE rcv_files(
  file_id BIGINT PRIMARY KEY REFERENCES files ON DELETE CASCADE,
  file_status TEXT NOT NULL,
  group_member_id BIGINT REFERENCES group_members ON DELETE CASCADE,
  file_queue_info BYTEA,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  rcv_file_inline TEXT,
  file_inline TEXT,
  file_descr_id BIGINT NULL,
  agent_rcv_file_id BYTEA NULL,
  agent_rcv_file_deleted SMALLINT NOT NULL DEFAULT 0,
  to_receive SMALLINT,
  user_approved_relays SMALLINT NOT NULL DEFAULT 0
);
CREATE TABLE snd_file_chunks(
  file_id BIGINT NOT NULL,
  connection_id BIGINT NOT NULL,
  chunk_number BIGINT NOT NULL,
  chunk_agent_msg_id BIGINT,
  chunk_sent SMALLINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  FOREIGN KEY(file_id, connection_id) REFERENCES snd_files ON DELETE CASCADE,
  PRIMARY KEY(file_id, connection_id, chunk_number)
);
CREATE TABLE rcv_file_chunks(
  file_id BIGINT NOT NULL REFERENCES rcv_files ON DELETE CASCADE,
  chunk_number BIGINT NOT NULL,
  chunk_agent_msg_id BIGINT NOT NULL,
  chunk_stored SMALLINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(file_id, chunk_number)
);
CREATE TABLE connections(
  connection_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  agent_conn_id BYTEA NOT NULL UNIQUE,
  conn_level BIGINT NOT NULL DEFAULT 0,
  via_contact BIGINT REFERENCES contacts(contact_id) ON DELETE SET NULL,
  conn_status TEXT NOT NULL,
  conn_type TEXT NOT NULL,
  user_contact_link_id BIGINT,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_member_id BIGINT REFERENCES group_members ON DELETE CASCADE,
  snd_file_id BIGINT,
  rcv_file_id BIGINT REFERENCES rcv_files(file_id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  updated_at TIMESTAMPTZ NOT NULL,
  via_contact_uri_hash BYTEA,
  xcontact_id BYTEA,
  via_user_contact_link BIGINT DEFAULT NULL,
  custom_user_profile_id BIGINT REFERENCES contact_profiles ON DELETE SET NULL,
  conn_req_inv BYTEA,
  local_alias TEXT NOT NULL DEFAULT '',
  via_group_link SMALLINT NOT NULL DEFAULT 0,
  group_link_id BYTEA,
  security_code TEXT NULL,
  security_code_verified_at TIMESTAMPTZ NULL,
  auth_err_counter BIGINT NOT NULL DEFAULT 0,
  peer_chat_min_version INTEGER NOT NULL DEFAULT 1,
  peer_chat_max_version INTEGER NOT NULL DEFAULT 1,
  to_subscribe SMALLINT DEFAULT 0 NOT NULL,
  contact_conn_initiated SMALLINT NOT NULL DEFAULT 0,
  conn_chat_version INTEGER,
  pq_support SMALLINT NOT NULL DEFAULT 0,
  pq_encryption SMALLINT NOT NULL DEFAULT 0,
  pq_snd_enabled SMALLINT,
  pq_rcv_enabled SMALLINT,
  quota_err_counter BIGINT NOT NULL DEFAULT 0,
  FOREIGN KEY(snd_file_id, connection_id)
  REFERENCES snd_files(file_id, connection_id)
  ON DELETE CASCADE
  DEFERRABLE INITIALLY DEFERRED
);
ALTER TABLE snd_files
ADD CONSTRAINT fk_snd_files_connections
  FOREIGN KEY(connection_id)
  REFERENCES connections(connection_id) ON DELETE CASCADE;
CREATE TABLE user_contact_links(
  user_contact_link_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  conn_req_contact BYTEA NOT NULL,
  local_display_name TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  updated_at TIMESTAMPTZ NOT NULL,
  auto_accept SMALLINT DEFAULT 0,
  auto_reply_msg_content TEXT DEFAULT NULL,
  group_id BIGINT REFERENCES groups ON DELETE CASCADE,
  auto_accept_incognito SMALLINT NOT NULL DEFAULT 0,
  group_link_id BYTEA,
  group_link_member_role TEXT NULL,
  business_address SMALLINT DEFAULT 0,
  UNIQUE(user_id, local_display_name)
);
ALTER TABLE connections
ADD CONSTRAINT fk_connections_user_contact_links_user_contact_link_id
  FOREIGN KEY(user_contact_link_id)
  REFERENCES user_contact_links(user_contact_link_id) ON DELETE CASCADE;
ALTER TABLE connections
ADD CONSTRAINT fk_connections_user_contact_links_via_user_contact_link
  FOREIGN KEY(via_user_contact_link)
  REFERENCES user_contact_links(user_contact_link_id) ON DELETE SET NULL;
CREATE TABLE contact_requests(
  contact_request_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_contact_link_id BIGINT NOT NULL REFERENCES user_contact_links
  ON UPDATE CASCADE ON DELETE CASCADE,
  agent_invitation_id BYTEA NOT NULL,
  contact_profile_id BIGINT REFERENCES contact_profiles
  ON DELETE SET NULL
  DEFERRABLE INITIALLY DEFERRED,
  local_display_name TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  updated_at TIMESTAMPTZ NOT NULL,
  xcontact_id BYTEA,
  peer_chat_min_version INTEGER NOT NULL DEFAULT 1,
  peer_chat_max_version INTEGER NOT NULL DEFAULT 1,
  pq_support SMALLINT NOT NULL DEFAULT 0,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  FOREIGN KEY(user_id, local_display_name)
  REFERENCES display_names(user_id, local_display_name)
  ON UPDATE CASCADE
  ON DELETE CASCADE
  DEFERRABLE INITIALLY DEFERRED,
  UNIQUE(user_id, local_display_name),
  UNIQUE(user_id, contact_profile_id)
);
CREATE TABLE messages(
  message_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  msg_sent SMALLINT NOT NULL,
  chat_msg_event TEXT NOT NULL,
  msg_body BYTEA,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL,
  connection_id BIGINT DEFAULT NULL REFERENCES connections ON DELETE CASCADE,
  group_id BIGINT DEFAULT NULL REFERENCES groups ON DELETE CASCADE,
  shared_msg_id BYTEA,
  shared_msg_id_user SMALLINT,
  author_group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL,
  forwarded_by_group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL
);
CREATE TABLE pending_group_messages(
  pending_group_message_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_member_id BIGINT NOT NULL REFERENCES group_members ON DELETE CASCADE,
  message_id BIGINT NOT NULL REFERENCES messages ON DELETE CASCADE,
  group_member_intro_id BIGINT REFERENCES group_member_intros ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE chat_items(
  chat_item_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_id BIGINT REFERENCES groups ON DELETE CASCADE,
  group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL,
  chat_msg_id BIGINT,
  created_by_msg_id BIGINT UNIQUE REFERENCES messages(message_id) ON DELETE SET NULL,
  item_sent SMALLINT NOT NULL,
  item_ts TIMESTAMPTZ NOT NULL,
  item_deleted SMALLINT NOT NULL DEFAULT 0,
  item_content TEXT NOT NULL,
  item_text TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  item_status TEXT NOT NULL,
  shared_msg_id BYTEA,
  quoted_shared_msg_id BYTEA,
  quoted_sent_at TIMESTAMPTZ,
  quoted_content TEXT,
  quoted_sent SMALLINT,
  quoted_member_id BYTEA,
  item_edited SMALLINT,
  timed_ttl BIGINT,
  timed_delete_at TIMESTAMPTZ,
  item_live SMALLINT,
  item_deleted_by_group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL,
  item_deleted_ts TIMESTAMPTZ,
  forwarded_by_group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL,
  item_content_tag TEXT,
  note_folder_id BIGINT DEFAULT NULL,
  fwd_from_tag TEXT,
  fwd_from_chat_name TEXT,
  fwd_from_msg_dir SMALLINT,
  fwd_from_contact_id BIGINT REFERENCES contacts ON DELETE SET NULL,
  fwd_from_group_id BIGINT REFERENCES groups ON DELETE SET NULL,
  fwd_from_chat_item_id BIGINT REFERENCES chat_items ON DELETE SET NULL,
  via_proxy SMALLINT,
  msg_content_tag TEXT,
  include_in_history SMALLINT NOT NULL DEFAULT 0,
  user_mention SMALLINT NOT NULL DEFAULT 0
);
ALTER TABLE groups
ADD CONSTRAINT fk_groups_chat_items
  FOREIGN KEY(chat_item_id)
  REFERENCES chat_items(chat_item_id) ON DELETE SET NULL;
ALTER TABLE files
ADD CONSTRAINT fk_files_chat_items
  FOREIGN KEY(chat_item_id)
  REFERENCES chat_items(chat_item_id) ON DELETE CASCADE;
CREATE TABLE chat_item_messages(
  chat_item_id BIGINT NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  message_id BIGINT NOT NULL UNIQUE REFERENCES messages ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  UNIQUE(chat_item_id, message_id)
);
CREATE TABLE calls(
  call_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  contact_id BIGINT NOT NULL REFERENCES contacts ON DELETE CASCADE,
  shared_call_id BYTEA NOT NULL,
  chat_item_id BIGINT NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  call_state BYTEA NOT NULL,
  call_ts TIMESTAMPTZ NOT NULL,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  call_uuid TEXT NOT NULL DEFAULT ''
);
CREATE TABLE commands(
  command_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  connection_id BIGINT REFERENCES connections ON DELETE CASCADE,
  command_function TEXT NOT NULL,
  command_status TEXT NOT NULL,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE settings(
  settings_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  chat_item_ttl BIGINT,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE protocol_servers(
  smp_server_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BYTEA NOT NULL,
  basic_auth TEXT,
  preset SMALLINT NOT NULL DEFAULT 0,
  tested SMALLINT,
  enabled SMALLINT NOT NULL DEFAULT 1,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  protocol TEXT NOT NULL DEFAULT 'smp',
  UNIQUE(user_id, host, port)
);
CREATE TABLE xftp_file_descriptions(
  file_descr_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  file_descr_text TEXT NOT NULL,
  file_descr_part_no BIGINT NOT NULL DEFAULT(0),
  file_descr_complete SMALLINT NOT NULL DEFAULT(0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
ALTER TABLE snd_files
ADD CONSTRAINT fk_snd_files_xftp_file_descriptions
  FOREIGN KEY(file_descr_id)
  REFERENCES xftp_file_descriptions(file_descr_id) ON DELETE SET NULL;
ALTER TABLE rcv_files
ADD CONSTRAINT fk_rcv_files_xftp_file_descriptions
  FOREIGN KEY(file_descr_id)
  REFERENCES xftp_file_descriptions(file_descr_id) ON DELETE SET NULL;
CREATE TABLE extra_xftp_file_descriptions(
  extra_file_descr_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  file_id BIGINT NOT NULL REFERENCES files ON DELETE CASCADE,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  file_descr_text TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE chat_item_versions(
  chat_item_version_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  chat_item_id BIGINT NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  msg_content TEXT NOT NULL,
  item_version_ts TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE chat_item_reactions(
  chat_item_reaction_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  item_member_id BYTEA,
  shared_msg_id BYTEA NOT NULL,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_id BIGINT REFERENCES groups ON DELETE CASCADE,
  group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL,
  created_by_msg_id BIGINT REFERENCES messages(message_id) ON DELETE SET NULL,
  reaction TEXT NOT NULL,
  reaction_sent SMALLINT NOT NULL,
  reaction_ts TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE chat_item_moderations(
  chat_item_moderation_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_id BIGINT NOT NULL REFERENCES groups ON DELETE CASCADE,
  moderator_member_id BIGINT NOT NULL REFERENCES group_members ON DELETE CASCADE,
  item_member_id BYTEA NOT NULL,
  shared_msg_id BYTEA NOT NULL,
  created_by_msg_id BIGINT REFERENCES messages(message_id) ON DELETE SET NULL,
  moderated_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE group_snd_item_statuses(
  group_snd_item_status_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  chat_item_id BIGINT NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  group_member_id BIGINT NOT NULL REFERENCES group_members ON DELETE CASCADE,
  group_snd_item_status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  via_proxy SMALLINT
);
CREATE TABLE sent_probes(
  sent_probe_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_member_id BIGINT REFERENCES group_members ON DELETE CASCADE,
  probe BYTEA NOT NULL,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  UNIQUE(user_id, probe)
);
CREATE TABLE sent_probe_hashes(
  sent_probe_hash_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  sent_probe_id BIGINT NOT NULL REFERENCES sent_probes ON DELETE CASCADE,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_member_id BIGINT REFERENCES group_members ON DELETE CASCADE,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);
CREATE TABLE received_probes(
  received_probe_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_member_id BIGINT REFERENCES group_members ON DELETE CASCADE,
  probe BYTEA,
  probe_hash BYTEA NOT NULL,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);
CREATE TABLE remote_hosts(
  remote_host_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  host_device_name TEXT NOT NULL,
  store_path TEXT NOT NULL,
  ca_key BYTEA NOT NULL,
  ca_cert BYTEA NOT NULL,
  id_key BYTEA NOT NULL,
  host_fingerprint BYTEA NOT NULL,
  host_dh_pub BYTEA NOT NULL,
  bind_addr TEXT,
  bind_iface TEXT,
  bind_port INTEGER
);
CREATE TABLE remote_controllers(
  remote_ctrl_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  ctrl_device_name TEXT NOT NULL,
  ca_key BYTEA NOT NULL,
  ca_cert BYTEA NOT NULL,
  ctrl_fingerprint BYTEA NOT NULL,
  id_pub BYTEA NOT NULL,
  dh_priv_key BYTEA NOT NULL,
  prev_dh_priv_key BYTEA
);
CREATE TABLE msg_deliveries(
  msg_delivery_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  message_id BIGINT NOT NULL REFERENCES messages ON DELETE CASCADE,
  connection_id BIGINT NOT NULL REFERENCES connections ON DELETE CASCADE,
  agent_msg_id BIGINT,
  agent_msg_meta TEXT,
  chat_ts TIMESTAMPTZ NOT NULL DEFAULT (now()),
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  delivery_status TEXT
);
CREATE TABLE note_folders(
  note_folder_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  chat_ts TIMESTAMPTZ NOT NULL DEFAULT (now()),
  favorite SMALLINT NOT NULL DEFAULT 0,
  unread_chat SMALLINT NOT NULL DEFAULT 0
);
ALTER TABLE files
ADD CONSTRAINT fk_files_note_folders
  FOREIGN KEY(note_folder_id)
  REFERENCES note_folders(note_folder_id) ON DELETE CASCADE;
ALTER TABLE chat_items
ADD CONSTRAINT fk_chat_items_note_folders
  FOREIGN KEY(note_folder_id)
  REFERENCES note_folders(note_folder_id) ON DELETE CASCADE;
CREATE TABLE app_settings(app_settings TEXT NOT NULL);
CREATE TABLE server_operators(
  server_operator_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  server_operator_tag TEXT,
  trade_name TEXT NOT NULL,
  legal_name TEXT,
  server_domains TEXT,
  enabled SMALLINT NOT NULL DEFAULT 1,
  smp_role_storage SMALLINT NOT NULL DEFAULT 1,
  smp_role_proxy SMALLINT NOT NULL DEFAULT 1,
  xftp_role_storage SMALLINT NOT NULL DEFAULT 1,
  xftp_role_proxy SMALLINT NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE usage_conditions(
  usage_conditions_id BIGINT PRIMARY KEY,
  conditions_commit TEXT NOT NULL UNIQUE,
  notified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE TABLE operator_usage_conditions(
  operator_usage_conditions_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  server_operator_id BIGINT REFERENCES server_operators(server_operator_id) ON DELETE SET NULL ON UPDATE CASCADE,
  server_operator_tag TEXT,
  conditions_commit TEXT NOT NULL,
  accepted_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  auto_accepted SMALLINT DEFAULT 0
);
CREATE TABLE chat_tags(
  chat_tag_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_id BIGINT REFERENCES users,
  chat_tag_text TEXT NOT NULL,
  chat_tag_emoji TEXT,
  tag_order BIGINT NOT NULL
);
CREATE TABLE chat_tags_chats(
  contact_id BIGINT REFERENCES contacts ON DELETE CASCADE,
  group_id BIGINT REFERENCES groups ON DELETE CASCADE,
  chat_tag_id BIGINT NOT NULL REFERENCES chat_tags ON DELETE CASCADE
);
CREATE TABLE chat_item_mentions (
  chat_item_mention_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  chat_item_id BIGINT NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  group_id BIGINT NOT NULL REFERENCES groups ON DELETE CASCADE,
  member_id BYTEA NOT NULL,
  display_name TEXT NOT NULL
);
CREATE INDEX contact_profiles_index ON contact_profiles(
  display_name,
  full_name
);
CREATE INDEX idx_groups_inv_queue_info ON groups(inv_queue_info);
CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests(xcontact_id);
CREATE INDEX idx_contacts_xcontact_id ON contacts(xcontact_id);
CREATE INDEX idx_messages_shared_msg_id ON messages(shared_msg_id);
CREATE UNIQUE INDEX idx_chat_items_direct_shared_msg_id ON chat_items(
  user_id,
  contact_id,
  shared_msg_id
);
CREATE UNIQUE INDEX idx_chat_items_group_shared_msg_id ON chat_items(
  user_id,
  group_id,
  group_member_id,
  shared_msg_id
);
CREATE UNIQUE INDEX idx_user_contact_links_group_id ON user_contact_links(
  group_id
);
CREATE UNIQUE INDEX idx_snd_files_last_inline_msg_delivery_id ON snd_files(
  last_inline_msg_delivery_id
);
CREATE INDEX idx_messages_connection_id ON messages(connection_id);
CREATE INDEX idx_chat_items_group_member_id ON chat_items(group_member_id);
CREATE INDEX idx_chat_items_contact_id ON chat_items(contact_id);
CREATE INDEX idx_chat_items_item_status ON chat_items(item_status);
CREATE INDEX idx_connections_group_member ON connections(
  user_id,
  group_member_id
);
CREATE INDEX idx_commands_connection_id ON commands(connection_id);
CREATE INDEX idx_calls_user_id ON calls(user_id);
CREATE INDEX idx_calls_chat_item_id ON calls(chat_item_id);
CREATE INDEX idx_calls_contact_id ON calls(contact_id);
CREATE INDEX idx_commands_user_id ON commands(user_id);
CREATE INDEX idx_connections_custom_user_profile_id ON connections(
  custom_user_profile_id
);
CREATE INDEX idx_connections_via_user_contact_link ON connections(
  via_user_contact_link
);
CREATE INDEX idx_connections_rcv_file_id ON connections(rcv_file_id);
CREATE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE INDEX idx_connections_user_contact_link_id ON connections(
  user_contact_link_id
);
CREATE INDEX idx_connections_via_contact ON connections(via_contact);
CREATE INDEX idx_contact_profiles_user_id ON contact_profiles(user_id);
CREATE INDEX idx_contact_requests_contact_profile_id ON contact_requests(
  contact_profile_id
);
CREATE INDEX idx_contact_requests_user_contact_link_id ON contact_requests(
  user_contact_link_id
);
CREATE INDEX idx_contacts_via_group ON contacts(via_group);
CREATE INDEX idx_contacts_contact_profile_id ON contacts(contact_profile_id);
CREATE INDEX idx_files_chat_item_id ON files(chat_item_id);
CREATE INDEX idx_files_user_id ON files(user_id);
CREATE INDEX idx_files_group_id ON files(group_id);
CREATE INDEX idx_files_contact_id ON files(contact_id);
CREATE INDEX idx_group_member_intros_to_group_member_id ON group_member_intros(
  to_group_member_id
);
CREATE INDEX idx_group_members_user_id_local_display_name ON group_members(
  user_id,
  local_display_name
);
CREATE INDEX idx_group_members_member_profile_id ON group_members(
  member_profile_id
);
CREATE INDEX idx_group_members_contact_id ON group_members(contact_id);
CREATE INDEX idx_group_members_contact_profile_id ON group_members(
  contact_profile_id
);
CREATE INDEX idx_group_members_user_id ON group_members(user_id);
CREATE INDEX idx_group_members_invited_by ON group_members(invited_by);
CREATE INDEX idx_group_profiles_user_id ON group_profiles(user_id);
CREATE INDEX idx_groups_chat_item_id ON groups(chat_item_id);
CREATE INDEX idx_groups_group_profile_id ON groups(group_profile_id);
CREATE INDEX idx_messages_group_id ON messages(group_id);
CREATE INDEX idx_pending_group_messages_group_member_intro_id ON pending_group_messages(
  group_member_intro_id
);
CREATE INDEX idx_pending_group_messages_message_id ON pending_group_messages(
  message_id
);
CREATE INDEX idx_pending_group_messages_group_member_id ON pending_group_messages(
  group_member_id
);
CREATE INDEX idx_rcv_file_chunks_file_id ON rcv_file_chunks(file_id);
CREATE INDEX idx_rcv_files_group_member_id ON rcv_files(group_member_id);
CREATE INDEX idx_settings_user_id ON settings(user_id);
CREATE INDEX idx_snd_file_chunks_file_id_connection_id ON snd_file_chunks(
  file_id,
  connection_id
);
CREATE INDEX idx_snd_files_group_member_id ON snd_files(group_member_id);
CREATE INDEX idx_snd_files_connection_id ON snd_files(connection_id);
CREATE INDEX idx_snd_files_file_id ON snd_files(file_id);
CREATE INDEX idx_smp_servers_user_id ON protocol_servers(user_id);
CREATE INDEX idx_chat_items_item_deleted_by_group_member_id ON chat_items(
  item_deleted_by_group_member_id
);
CREATE INDEX idx_snd_files_file_descr_id ON snd_files(file_descr_id);
CREATE INDEX idx_rcv_files_file_descr_id ON rcv_files(file_descr_id);
CREATE INDEX idx_extra_xftp_file_descriptions_file_id ON extra_xftp_file_descriptions(
  file_id
);
CREATE INDEX idx_extra_xftp_file_descriptions_user_id ON extra_xftp_file_descriptions(
  user_id
);
CREATE INDEX idx_xftp_file_descriptions_user_id ON xftp_file_descriptions(
  user_id
);
CREATE INDEX idx_chat_item_versions_chat_item_id ON chat_item_versions(
  chat_item_id
);
CREATE INDEX idx_chat_item_reactions_shared_msg_id ON chat_item_reactions(
  shared_msg_id
);
CREATE INDEX idx_chat_item_reactions_contact_id ON chat_item_reactions(
  contact_id
);
CREATE INDEX idx_chat_item_reactions_group_id ON chat_item_reactions(group_id);
CREATE INDEX idx_chat_item_reactions_group_member_id ON chat_item_reactions(
  group_member_id
);
CREATE INDEX idx_chat_item_reactions_contact ON chat_item_reactions(
  contact_id,
  shared_msg_id
);
CREATE INDEX idx_chat_item_reactions_group ON chat_item_reactions(
  group_id,
  shared_msg_id
);
CREATE INDEX idx_messages_created_at ON messages(created_at);
CREATE INDEX idx_chat_item_reactions_created_by_msg_id ON chat_item_reactions(
  created_by_msg_id
);
CREATE INDEX idx_chat_items_timed_delete_at ON chat_items(
  user_id,
  timed_delete_at
);
CREATE INDEX idx_group_members_group_id ON group_members(user_id, group_id);
CREATE INDEX idx_chat_item_moderations_group_id ON chat_item_moderations(
  group_id
);
CREATE INDEX idx_chat_item_moderations_moderator_member_id ON chat_item_moderations(
  moderator_member_id
);
CREATE INDEX idx_chat_item_moderations_created_by_msg_id ON chat_item_moderations(
  created_by_msg_id
);
CREATE INDEX idx_chat_item_moderations_group ON chat_item_moderations(
  group_id,
  item_member_id,
  shared_msg_id
);
CREATE INDEX idx_group_snd_item_statuses_chat_item_id ON group_snd_item_statuses(
  chat_item_id
);
CREATE INDEX idx_group_snd_item_statuses_group_member_id ON group_snd_item_statuses(
  group_member_id
);
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(
  user_id,
  item_status
);
CREATE INDEX idx_connections_to_subscribe ON connections(to_subscribe);
CREATE INDEX idx_contacts_contact_group_member_id ON contacts(
  contact_group_member_id
);
CREATE INDEX idx_sent_probes_user_id ON sent_probes(user_id);
CREATE INDEX idx_sent_probes_contact_id ON sent_probes(contact_id);
CREATE INDEX idx_sent_probes_group_member_id ON sent_probes(group_member_id);
CREATE INDEX idx_sent_probe_hashes_user_id ON sent_probe_hashes(user_id);
CREATE INDEX idx_sent_probe_hashes_sent_probe_id ON sent_probe_hashes(
  sent_probe_id
);
CREATE INDEX idx_sent_probe_hashes_contact_id ON sent_probe_hashes(contact_id);
CREATE INDEX idx_sent_probe_hashes_group_member_id ON sent_probe_hashes(
  group_member_id
);
CREATE INDEX idx_received_probes_user_id ON received_probes(user_id);
CREATE INDEX idx_received_probes_contact_id ON received_probes(contact_id);
CREATE INDEX idx_received_probes_probe ON received_probes(probe);
CREATE INDEX idx_received_probes_probe_hash ON received_probes(probe_hash);
CREATE INDEX idx_sent_probes_created_at ON sent_probes(created_at);
CREATE INDEX idx_sent_probe_hashes_created_at ON sent_probe_hashes(created_at);
CREATE INDEX idx_received_probes_created_at ON received_probes(created_at);
CREATE INDEX idx_connections_conn_req_inv ON connections(
  user_id,
  conn_req_inv
);
CREATE INDEX idx_groups_via_group_link_uri_hash ON groups(
  user_id,
  via_group_link_uri_hash
);
CREATE INDEX idx_connections_via_contact_uri_hash ON connections(
  user_id,
  via_contact_uri_hash
);
CREATE INDEX idx_contact_profiles_contact_link ON contact_profiles(
  user_id,
  contact_link
);
CREATE INDEX idx_group_member_intros_re_group_member_id ON group_member_intros(
  re_group_member_id
);
CREATE INDEX idx_group_members_invited_by_group_member_id ON group_members(
  invited_by_group_member_id
);
CREATE INDEX idx_messages_author_group_member_id ON messages(
  author_group_member_id
);
CREATE INDEX idx_messages_forwarded_by_group_member_id ON messages(
  forwarded_by_group_member_id
);
CREATE INDEX idx_messages_group_id_shared_msg_id ON messages(
  group_id,
  shared_msg_id
);
CREATE INDEX idx_chat_items_forwarded_by_group_member_id ON chat_items(
  forwarded_by_group_member_id
);
CREATE UNIQUE INDEX idx_remote_hosts_host_fingerprint ON remote_hosts(
  host_fingerprint
);
CREATE UNIQUE INDEX idx_remote_controllers_ctrl_fingerprint ON remote_controllers(
  ctrl_fingerprint
);
CREATE INDEX idx_contacts_chat_ts ON contacts(user_id, chat_ts);
CREATE INDEX idx_groups_chat_ts ON groups(user_id, chat_ts);
CREATE INDEX idx_contact_requests_updated_at ON contact_requests(
  user_id,
  updated_at
);
CREATE INDEX idx_connections_updated_at ON connections(user_id, updated_at);
CREATE INDEX idx_msg_deliveries_message_id ON msg_deliveries(message_id);
CREATE INDEX idx_msg_deliveries_agent_msg_id ON msg_deliveries(
  connection_id,
  agent_msg_id
);
CREATE INDEX chat_items_note_folder_id ON chat_items(note_folder_id);
CREATE INDEX files_note_folder_id ON files(note_folder_id);
CREATE INDEX note_folders_user_id ON note_folders(user_id);
CREATE INDEX idx_chat_items_contacts_created_at on chat_items(
  user_id,
  contact_id,
  created_at
);
CREATE INDEX idx_chat_items_notes_created_at on chat_items(
  user_id,
  note_folder_id,
  created_at
);
CREATE INDEX idx_files_redirect_file_id on files(redirect_file_id);
CREATE INDEX idx_chat_items_fwd_from_contact_id ON chat_items(
  fwd_from_contact_id
);
CREATE INDEX idx_chat_items_fwd_from_group_id ON chat_items(fwd_from_group_id);
CREATE INDEX idx_chat_items_fwd_from_chat_item_id ON chat_items(
  fwd_from_chat_item_id
);
CREATE INDEX idx_received_probes_group_member_id on received_probes(
  group_member_id
);
CREATE INDEX idx_contact_requests_contact_id ON contact_requests(contact_id);
CREATE INDEX idx_operator_usage_conditions_server_operator_id ON operator_usage_conditions(
  server_operator_id
);
CREATE UNIQUE INDEX idx_operator_usage_conditions_conditions_commit ON operator_usage_conditions(
  conditions_commit,
  server_operator_id
);
CREATE INDEX idx_chat_items_contacts ON chat_items(
  user_id,
  contact_id,
  item_status,
  created_at
);
CREATE INDEX idx_chat_items_groups ON chat_items(
  user_id,
  group_id,
  item_status,
  item_ts
);
CREATE INDEX idx_chat_items_groups_item_ts ON chat_items(
  user_id,
  group_id,
  item_ts
);
CREATE INDEX idx_chat_items_notes ON chat_items(
  user_id,
  note_folder_id,
  item_status,
  created_at
);
CREATE INDEX idx_groups_business_xcontact_id ON groups(business_xcontact_id);
CREATE INDEX idx_chat_tags_user_id ON chat_tags(user_id);
CREATE UNIQUE INDEX idx_chat_tags_user_id_chat_tag_text ON chat_tags(
  user_id,
  chat_tag_text
);
CREATE UNIQUE INDEX idx_chat_tags_user_id_chat_tag_emoji ON chat_tags(
  user_id,
  chat_tag_emoji
);
CREATE INDEX idx_chat_tags_chats_chat_tag_id ON chat_tags_chats(chat_tag_id);
CREATE UNIQUE INDEX idx_chat_tags_chats_chat_tag_id_contact_id ON chat_tags_chats(
  contact_id,
  chat_tag_id
);
CREATE UNIQUE INDEX idx_chat_tags_chats_chat_tag_id_group_id ON chat_tags_chats(
  group_id,
  chat_tag_id
);
CREATE INDEX idx_chat_items_groups_msg_content_tag_item_ts ON chat_items(
  user_id,
  group_id,
  msg_content_tag,
  item_ts
);
CREATE INDEX idx_chat_items_groups_msg_content_tag_deleted ON chat_items(
  user_id,
  group_id,
  msg_content_tag,
  item_deleted,
  item_sent
);
CREATE INDEX idx_chat_items_groups_history ON chat_items(
  user_id,
  group_id,
  include_in_history,
  item_deleted,
  item_ts,
  chat_item_id
);
CREATE INDEX idx_group_snd_item_statuses_chat_item_id_group_member_id ON group_snd_item_statuses(
  chat_item_id,
  group_member_id
);
CREATE INDEX idx_chat_item_mentions_group_id ON chat_item_mentions(group_id);
CREATE INDEX idx_chat_item_mentions_chat_item_id ON chat_item_mentions(
  chat_item_id
);
CREATE UNIQUE INDEX idx_chat_item_mentions_display_name ON chat_item_mentions(
  chat_item_id,
  display_name
);
CREATE UNIQUE INDEX idx_chat_item_mentions_member_id ON chat_item_mentions(
  chat_item_id,
  member_id
);
CREATE INDEX idx_chat_items_groups_user_mention ON chat_items(
  user_id,
  group_id,
  item_status,
  user_mention
);
CREATE INDEX idx_chat_items_group_id ON chat_items(group_id);
CREATE INDEX idx_connections_group_member_id ON connections(group_member_id);
CREATE INDEX idx_chat_items_group_id_shared_msg_id ON chat_items(
  group_id,
  shared_msg_id
);
|]
