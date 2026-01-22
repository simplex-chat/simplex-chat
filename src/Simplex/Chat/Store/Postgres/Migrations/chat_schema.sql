

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


CREATE SCHEMA test_chat_schema;



CREATE FUNCTION test_chat_schema.is_current_member(p_status text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
BEGIN
  RETURN p_status IN (
    'introduced',
    'intro-inv',
    'accepted',
    'announced',
    'connected',
    'complete',
    'creator'
  );
END;
$$;



CREATE FUNCTION test_chat_schema.migrate_relations_vector_step(state bytea, idx bigint, direction integer, intro_status text) RETURNS bytea
    LANGUAGE plpgsql IMMUTABLE
    AS $$
DECLARE
  new_len INT;
  result BYTEA;
  status INT;
  byte_val INT;
BEGIN
  IF idx < 0 THEN
    RETURN state;
  END IF;
  IF intro_status = 're-con' THEN
    IF direction = 0 THEN status := 2; ELSE status := 3; END IF;
  ELSIF intro_status = 'to-con' THEN
    IF direction = 0 THEN status := 3; ELSE status := 2; END IF;
  ELSIF intro_status = 'con' THEN
    status := 4;
  ELSE
    status := 1;
  END IF;
  byte_val := (direction * 8) + status;
  new_len := GREATEST(length(state), idx + 1);
  IF new_len > length(state) THEN
    result := state || (SELECT string_agg('\x00'::BYTEA, ''::BYTEA) FROM generate_series(1, new_len - length(state)));
  ELSE
    result := state;
  END IF;
  result := set_byte(result, idx::INT, byte_val);
  RETURN result;
END;
$$;



CREATE FUNCTION test_chat_schema.on_group_members_delete_update_summary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF is_current_member(OLD.member_status) THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count - 1
    WHERE group_id = OLD.group_id;
  END IF;
  RETURN OLD;
END;
$$;



CREATE FUNCTION test_chat_schema.on_group_members_insert_update_summary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF is_current_member(NEW.member_status) THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count + 1
    WHERE group_id = NEW.group_id;
  END IF;
  RETURN NEW;
END;
$$;



CREATE FUNCTION test_chat_schema.on_group_members_update_update_summary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  was_active BOOLEAN;
  is_active BOOLEAN;
BEGIN
  was_active := is_current_member(OLD.member_status);
  is_active := is_current_member(NEW.member_status);

  IF was_active != is_active THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count +
        (CASE WHEN is_active THEN 1 ELSE -1 END)
    WHERE group_id = NEW.group_id;
  END IF;
  RETURN NEW;
END;
$$;



CREATE FUNCTION test_chat_schema.set_member_vector_new_relation(v bytea, idx bigint, direction integer, status integer) RETURNS bytea
    LANGUAGE plpgsql IMMUTABLE
    AS $$
DECLARE
  new_len INT;
  result BYTEA;
  byte_val INT;
  old_byte INT;
BEGIN
  IF idx < 0 THEN
    RETURN v;
  END IF;
  IF idx < length(v) THEN
    old_byte := get_byte(v, idx::INT);
  ELSE
    old_byte := 0;
  END IF;
  byte_val := (old_byte & x'F0'::INT) | (direction * 8) | status;
  new_len := GREATEST(length(v), idx + 1);
  IF new_len > length(v) THEN
    result := v || (SELECT string_agg('\x00'::BYTEA, ''::BYTEA) FROM generate_series(1, new_len - length(v)));
  ELSE
    result := v;
  END IF;
  result := set_byte(result, idx::INT, byte_val);
  RETURN result;
END;
$$;



CREATE AGGREGATE test_chat_schema.migrate_relations_vector(bigint, integer, text) (
    SFUNC = test_chat_schema.migrate_relations_vector_step,
    STYPE = bytea,
    INITCOND = ''
);


SET default_table_access_method = heap;


CREATE TABLE test_chat_schema.app_settings (
    app_settings text NOT NULL
);



CREATE TABLE test_chat_schema.calls (
    call_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    shared_call_id bytea NOT NULL,
    chat_item_id bigint NOT NULL,
    call_state text NOT NULL,
    call_ts timestamp with time zone NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    call_uuid text DEFAULT ''::text NOT NULL
);



ALTER TABLE test_chat_schema.calls ALTER COLUMN call_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.calls_call_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.chat_item_mentions (
    chat_item_mention_id bigint NOT NULL,
    chat_item_id bigint NOT NULL,
    group_id bigint NOT NULL,
    member_id bytea NOT NULL,
    display_name text NOT NULL
);



ALTER TABLE test_chat_schema.chat_item_mentions ALTER COLUMN chat_item_mention_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.chat_item_mentions_chat_item_mention_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.chat_item_messages (
    chat_item_id bigint NOT NULL,
    message_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



CREATE TABLE test_chat_schema.chat_item_moderations (
    chat_item_moderation_id bigint NOT NULL,
    group_id bigint NOT NULL,
    moderator_member_id bigint NOT NULL,
    item_member_id bytea NOT NULL,
    shared_msg_id bytea NOT NULL,
    created_by_msg_id bigint,
    moderated_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.chat_item_moderations ALTER COLUMN chat_item_moderation_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.chat_item_moderations_chat_item_moderation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.chat_item_reactions (
    chat_item_reaction_id bigint NOT NULL,
    item_member_id bytea,
    shared_msg_id bytea NOT NULL,
    contact_id bigint,
    group_id bigint,
    group_member_id bigint,
    created_by_msg_id bigint,
    reaction text NOT NULL,
    reaction_sent smallint NOT NULL,
    reaction_ts timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.chat_item_reactions ALTER COLUMN chat_item_reaction_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.chat_item_reactions_chat_item_reaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.chat_item_versions (
    chat_item_version_id bigint NOT NULL,
    chat_item_id bigint NOT NULL,
    msg_content text NOT NULL,
    item_version_ts timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.chat_item_versions ALTER COLUMN chat_item_version_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.chat_item_versions_chat_item_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.chat_items (
    chat_item_id bigint NOT NULL,
    user_id bigint NOT NULL,
    contact_id bigint,
    group_id bigint,
    group_member_id bigint,
    chat_msg_id bigint,
    created_by_msg_id bigint,
    item_sent smallint NOT NULL,
    item_ts timestamp with time zone NOT NULL,
    item_deleted smallint DEFAULT 0 NOT NULL,
    item_content text NOT NULL,
    item_text text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    item_status text NOT NULL,
    shared_msg_id bytea,
    quoted_shared_msg_id bytea,
    quoted_sent_at timestamp with time zone,
    quoted_content text,
    quoted_sent smallint,
    quoted_member_id bytea,
    item_edited smallint,
    timed_ttl bigint,
    timed_delete_at timestamp with time zone,
    item_live smallint,
    item_deleted_by_group_member_id bigint,
    item_deleted_ts timestamp with time zone,
    forwarded_by_group_member_id bigint,
    item_content_tag text,
    note_folder_id bigint,
    fwd_from_tag text,
    fwd_from_chat_name text,
    fwd_from_msg_dir smallint,
    fwd_from_contact_id bigint,
    fwd_from_group_id bigint,
    fwd_from_chat_item_id bigint,
    via_proxy smallint,
    msg_content_tag text,
    include_in_history smallint DEFAULT 0 NOT NULL,
    user_mention smallint DEFAULT 0 NOT NULL,
    group_scope_tag text,
    group_scope_group_member_id bigint,
    show_group_as_sender smallint DEFAULT 0 NOT NULL,
    has_link smallint DEFAULT 0 NOT NULL
);



ALTER TABLE test_chat_schema.chat_items ALTER COLUMN chat_item_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.chat_items_chat_item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.chat_tags (
    chat_tag_id bigint NOT NULL,
    user_id bigint,
    chat_tag_text text NOT NULL,
    chat_tag_emoji text,
    tag_order bigint NOT NULL
);



ALTER TABLE test_chat_schema.chat_tags ALTER COLUMN chat_tag_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.chat_tags_chat_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.chat_tags_chats (
    contact_id bigint,
    group_id bigint,
    chat_tag_id bigint NOT NULL
);



CREATE TABLE test_chat_schema.commands (
    command_id bigint NOT NULL,
    connection_id bigint,
    command_function text NOT NULL,
    command_status text NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.commands ALTER COLUMN command_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.commands_command_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.connections (
    connection_id bigint NOT NULL,
    agent_conn_id bytea NOT NULL,
    conn_level bigint DEFAULT 0 NOT NULL,
    via_contact bigint,
    conn_status text NOT NULL,
    conn_type text NOT NULL,
    user_contact_link_id bigint,
    contact_id bigint,
    group_member_id bigint,
    snd_file_id bigint,
    rcv_file_id bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    user_id bigint NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    via_contact_uri_hash bytea,
    xcontact_id bytea,
    via_user_contact_link bigint,
    custom_user_profile_id bigint,
    conn_req_inv bytea,
    local_alias text DEFAULT ''::text NOT NULL,
    via_group_link smallint DEFAULT 0 NOT NULL,
    group_link_id bytea,
    security_code text,
    security_code_verified_at timestamp with time zone,
    auth_err_counter bigint DEFAULT 0 NOT NULL,
    peer_chat_min_version integer DEFAULT 1 NOT NULL,
    peer_chat_max_version integer DEFAULT 1 NOT NULL,
    to_subscribe smallint DEFAULT 0 NOT NULL,
    contact_conn_initiated smallint DEFAULT 0 NOT NULL,
    conn_chat_version integer,
    pq_support smallint DEFAULT 0 NOT NULL,
    pq_encryption smallint DEFAULT 0 NOT NULL,
    pq_snd_enabled smallint,
    pq_rcv_enabled smallint,
    quota_err_counter bigint DEFAULT 0 NOT NULL,
    short_link_inv bytea,
    via_short_link_contact bytea,
    via_contact_uri bytea
);



ALTER TABLE test_chat_schema.connections ALTER COLUMN connection_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.connections_connection_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.connections_sync (
    connections_sync_id bigint NOT NULL,
    should_sync smallint DEFAULT 0 NOT NULL,
    last_sync_ts timestamp with time zone
);



ALTER TABLE test_chat_schema.connections_sync ALTER COLUMN connections_sync_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME test_chat_schema.connections_sync_connections_sync_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.contact_profiles (
    contact_profile_id bigint NOT NULL,
    display_name text NOT NULL,
    full_name text NOT NULL,
    properties text DEFAULT '{}'::text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    image text,
    user_id bigint,
    incognito smallint,
    local_alias text DEFAULT ''::text NOT NULL,
    preferences text,
    contact_link bytea,
    short_descr text,
    chat_peer_type text
);



ALTER TABLE test_chat_schema.contact_profiles ALTER COLUMN contact_profile_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.contact_profiles_contact_profile_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.contact_requests (
    contact_request_id bigint NOT NULL,
    user_contact_link_id bigint,
    agent_invitation_id bytea NOT NULL,
    contact_profile_id bigint,
    local_display_name text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    user_id bigint NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    xcontact_id bytea,
    peer_chat_min_version integer DEFAULT 1 NOT NULL,
    peer_chat_max_version integer DEFAULT 1 NOT NULL,
    pq_support smallint DEFAULT 0 NOT NULL,
    contact_id bigint,
    business_group_id bigint,
    welcome_shared_msg_id bytea,
    request_shared_msg_id bytea
);



ALTER TABLE test_chat_schema.contact_requests ALTER COLUMN contact_request_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.contact_requests_contact_request_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.contacts (
    contact_id bigint NOT NULL,
    contact_profile_id bigint,
    user_id bigint NOT NULL,
    local_display_name text NOT NULL,
    is_user smallint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    xcontact_id bytea,
    enable_ntfs smallint,
    unread_chat smallint DEFAULT 0 NOT NULL,
    contact_used smallint DEFAULT 0 NOT NULL,
    user_preferences text DEFAULT '{}'::text NOT NULL,
    chat_ts timestamp with time zone,
    deleted smallint DEFAULT 0 NOT NULL,
    favorite smallint DEFAULT 0 NOT NULL,
    send_rcpts smallint,
    contact_group_member_id bigint,
    contact_grp_inv_sent smallint DEFAULT 0 NOT NULL,
    contact_status text DEFAULT 'active'::text NOT NULL,
    custom_data bytea,
    ui_themes text,
    chat_deleted smallint DEFAULT 0 NOT NULL,
    chat_item_ttl bigint,
    conn_full_link_to_connect bytea,
    conn_short_link_to_connect bytea,
    welcome_shared_msg_id bytea,
    request_shared_msg_id bytea,
    contact_request_id bigint,
    grp_direct_inv_link bytea,
    grp_direct_inv_from_group_id bigint,
    grp_direct_inv_from_group_member_id bigint,
    grp_direct_inv_from_member_conn_id bigint,
    grp_direct_inv_started_connection smallint DEFAULT 0 NOT NULL
);



ALTER TABLE test_chat_schema.contacts ALTER COLUMN contact_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.contacts_contact_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.delivery_jobs (
    delivery_job_id bigint NOT NULL,
    group_id bigint NOT NULL,
    worker_scope text NOT NULL,
    job_scope_spec_tag text,
    job_scope_include_pending smallint,
    job_scope_support_gm_id bigint,
    single_sender_group_member_id bigint,
    body bytea,
    cursor_group_member_id bigint,
    job_status text NOT NULL,
    job_err_reason text,
    failed smallint DEFAULT 0,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.delivery_jobs ALTER COLUMN delivery_job_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.delivery_jobs_delivery_job_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.delivery_tasks (
    delivery_task_id bigint NOT NULL,
    group_id bigint NOT NULL,
    worker_scope text NOT NULL,
    job_scope_spec_tag text,
    job_scope_include_pending smallint,
    job_scope_support_gm_id bigint,
    sender_group_member_id bigint NOT NULL,
    message_id bigint,
    message_from_channel smallint DEFAULT 0 NOT NULL,
    task_status text NOT NULL,
    task_err_reason text,
    failed smallint DEFAULT 0,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.delivery_tasks ALTER COLUMN delivery_task_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.delivery_tasks_delivery_task_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.display_names (
    user_id bigint NOT NULL,
    local_display_name text NOT NULL,
    ldn_base text NOT NULL,
    ldn_suffix bigint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);



CREATE TABLE test_chat_schema.extra_xftp_file_descriptions (
    extra_file_descr_id bigint NOT NULL,
    file_id bigint NOT NULL,
    user_id bigint NOT NULL,
    file_descr_text text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.extra_xftp_file_descriptions ALTER COLUMN extra_file_descr_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.extra_xftp_file_descriptions_extra_file_descr_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.files (
    file_id bigint NOT NULL,
    contact_id bigint,
    group_id bigint,
    file_name text NOT NULL,
    file_path text,
    file_size bigint NOT NULL,
    chunk_size bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    user_id bigint NOT NULL,
    chat_item_id bigint,
    updated_at timestamp with time zone NOT NULL,
    cancelled smallint,
    ci_file_status text,
    file_inline text,
    agent_snd_file_id bytea,
    private_snd_file_descr text,
    agent_snd_file_deleted smallint DEFAULT 0 NOT NULL,
    protocol text DEFAULT 'smp'::text NOT NULL,
    file_crypto_key bytea,
    file_crypto_nonce bytea,
    note_folder_id bigint,
    redirect_file_id bigint
);



ALTER TABLE test_chat_schema.files ALTER COLUMN file_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.files_file_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.group_member_intros (
    group_member_intro_id bigint NOT NULL,
    re_group_member_id bigint NOT NULL,
    to_group_member_id bigint NOT NULL,
    group_queue_info bytea,
    direct_queue_info bytea,
    intro_status text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    intro_chat_protocol_version integer DEFAULT 3 NOT NULL
);



ALTER TABLE test_chat_schema.group_member_intros ALTER COLUMN group_member_intro_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.group_member_intros_group_member_intro_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.group_members (
    group_member_id bigint NOT NULL,
    group_id bigint NOT NULL,
    member_id bytea NOT NULL,
    member_role text NOT NULL,
    member_category text NOT NULL,
    member_status text NOT NULL,
    invited_by bigint,
    sent_inv_queue_info bytea,
    group_queue_info bytea,
    direct_queue_info bytea,
    user_id bigint NOT NULL,
    local_display_name text NOT NULL,
    contact_profile_id bigint NOT NULL,
    contact_id bigint,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    member_profile_id bigint,
    show_messages smallint DEFAULT 1 NOT NULL,
    xgrplinkmem_received smallint DEFAULT 0 NOT NULL,
    invited_by_group_member_id bigint,
    peer_chat_min_version integer DEFAULT 1 NOT NULL,
    peer_chat_max_version integer DEFAULT 1 NOT NULL,
    member_restriction text,
    support_chat_ts timestamp with time zone,
    support_chat_items_unread bigint DEFAULT 0 NOT NULL,
    support_chat_items_member_attention bigint DEFAULT 0 NOT NULL,
    support_chat_items_mentions bigint DEFAULT 0 NOT NULL,
    support_chat_last_msg_from_member_ts timestamp with time zone,
    member_xcontact_id bytea,
    member_welcome_shared_msg_id bytea,
    index_in_group bigint DEFAULT 0 NOT NULL,
    member_relations_vector bytea
);



ALTER TABLE test_chat_schema.group_members ALTER COLUMN group_member_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.group_members_group_member_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.group_profiles (
    group_profile_id bigint NOT NULL,
    display_name text NOT NULL,
    full_name text NOT NULL,
    properties text DEFAULT '{}'::text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    image text,
    user_id bigint,
    preferences text,
    description text,
    member_admission text,
    short_descr text
);



ALTER TABLE test_chat_schema.group_profiles ALTER COLUMN group_profile_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.group_profiles_group_profile_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.group_snd_item_statuses (
    group_snd_item_status_id bigint NOT NULL,
    chat_item_id bigint NOT NULL,
    group_member_id bigint NOT NULL,
    group_snd_item_status text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    via_proxy smallint
);



ALTER TABLE test_chat_schema.group_snd_item_statuses ALTER COLUMN group_snd_item_status_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.group_snd_item_statuses_group_snd_item_status_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.groups (
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    local_display_name text NOT NULL,
    group_profile_id bigint,
    inv_queue_info bytea,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    chat_item_id bigint,
    enable_ntfs smallint,
    unread_chat smallint DEFAULT 0 NOT NULL,
    chat_ts timestamp with time zone,
    favorite smallint DEFAULT 0 NOT NULL,
    send_rcpts smallint,
    via_group_link_uri_hash bytea,
    user_member_profile_sent_at timestamp with time zone,
    custom_data bytea,
    ui_themes text,
    business_member_id bytea,
    business_chat text,
    business_xcontact_id bytea,
    customer_member_id bytea,
    chat_item_ttl bigint,
    local_alias text DEFAULT ''::text,
    members_require_attention bigint DEFAULT 0 NOT NULL,
    conn_full_link_to_connect bytea,
    conn_short_link_to_connect bytea,
    conn_link_started_connection smallint DEFAULT 0 NOT NULL,
    welcome_shared_msg_id bytea,
    request_shared_msg_id bytea,
    conn_link_prepared_connection smallint DEFAULT 0 NOT NULL,
    via_group_link_uri bytea,
    summary_current_members_count bigint DEFAULT 0 NOT NULL,
    member_index bigint DEFAULT 0 NOT NULL
);



ALTER TABLE test_chat_schema.groups ALTER COLUMN group_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.groups_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.known_servers (
    server_id bigint NOT NULL,
    host text NOT NULL,
    port text NOT NULL,
    key_hash bytea,
    user_id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);



ALTER TABLE test_chat_schema.known_servers ALTER COLUMN server_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.known_servers_server_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.messages (
    message_id bigint NOT NULL,
    msg_sent smallint NOT NULL,
    chat_msg_event text NOT NULL,
    msg_body bytea,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    connection_id bigint,
    group_id bigint,
    shared_msg_id bytea,
    shared_msg_id_user smallint,
    author_group_member_id bigint,
    forwarded_by_group_member_id bigint,
    broker_ts timestamp with time zone
);



ALTER TABLE test_chat_schema.messages ALTER COLUMN message_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.messages_message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.migrations (
    name text NOT NULL,
    ts timestamp without time zone NOT NULL,
    down text
);



CREATE TABLE test_chat_schema.msg_deliveries (
    msg_delivery_id bigint NOT NULL,
    message_id bigint NOT NULL,
    connection_id bigint NOT NULL,
    agent_msg_id bigint,
    agent_msg_meta text,
    chat_ts timestamp with time zone DEFAULT now() NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    delivery_status text
);



ALTER TABLE test_chat_schema.msg_deliveries ALTER COLUMN msg_delivery_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.msg_deliveries_msg_delivery_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.note_folders (
    note_folder_id bigint NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    chat_ts timestamp with time zone DEFAULT now() NOT NULL,
    favorite smallint DEFAULT 0 NOT NULL,
    unread_chat smallint DEFAULT 0 NOT NULL
);



ALTER TABLE test_chat_schema.note_folders ALTER COLUMN note_folder_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.note_folders_note_folder_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.operator_usage_conditions (
    operator_usage_conditions_id bigint NOT NULL,
    server_operator_id bigint,
    server_operator_tag text,
    conditions_commit text NOT NULL,
    accepted_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    auto_accepted smallint DEFAULT 0
);



ALTER TABLE test_chat_schema.operator_usage_conditions ALTER COLUMN operator_usage_conditions_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.operator_usage_conditions_operator_usage_conditions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.pending_group_messages (
    pending_group_message_id bigint NOT NULL,
    group_member_id bigint NOT NULL,
    message_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.pending_group_messages ALTER COLUMN pending_group_message_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.pending_group_messages_pending_group_message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.protocol_servers (
    smp_server_id bigint NOT NULL,
    host text NOT NULL,
    port text NOT NULL,
    key_hash bytea NOT NULL,
    basic_auth text,
    preset smallint DEFAULT 0 NOT NULL,
    tested smallint,
    enabled smallint DEFAULT 1 NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    protocol text DEFAULT 'smp'::text NOT NULL
);



ALTER TABLE test_chat_schema.protocol_servers ALTER COLUMN smp_server_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.protocol_servers_smp_server_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.rcv_file_chunks (
    file_id bigint NOT NULL,
    chunk_number bigint NOT NULL,
    chunk_agent_msg_id bigint NOT NULL,
    chunk_stored smallint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);



CREATE TABLE test_chat_schema.rcv_files (
    file_id bigint NOT NULL,
    file_status text NOT NULL,
    group_member_id bigint,
    file_queue_info bytea,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    rcv_file_inline text,
    file_inline text,
    file_descr_id bigint,
    agent_rcv_file_id bytea,
    agent_rcv_file_deleted smallint DEFAULT 0 NOT NULL,
    to_receive smallint,
    user_approved_relays smallint DEFAULT 0 NOT NULL
);



CREATE TABLE test_chat_schema.received_probes (
    received_probe_id bigint NOT NULL,
    contact_id bigint,
    group_member_id bigint,
    probe bytea,
    probe_hash bytea NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);



ALTER TABLE test_chat_schema.received_probes ALTER COLUMN received_probe_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.received_probes_received_probe_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.remote_controllers (
    remote_ctrl_id bigint NOT NULL,
    ctrl_device_name text NOT NULL,
    ca_key bytea NOT NULL,
    ca_cert bytea NOT NULL,
    ctrl_fingerprint bytea NOT NULL,
    id_pub bytea NOT NULL,
    dh_priv_key bytea NOT NULL,
    prev_dh_priv_key bytea
);



ALTER TABLE test_chat_schema.remote_controllers ALTER COLUMN remote_ctrl_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.remote_controllers_remote_ctrl_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.remote_hosts (
    remote_host_id bigint NOT NULL,
    host_device_name text NOT NULL,
    store_path text NOT NULL,
    ca_key bytea NOT NULL,
    ca_cert bytea NOT NULL,
    id_key bytea NOT NULL,
    host_fingerprint bytea NOT NULL,
    host_dh_pub bytea NOT NULL,
    bind_addr text,
    bind_iface text,
    bind_port integer
);



ALTER TABLE test_chat_schema.remote_hosts ALTER COLUMN remote_host_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.remote_hosts_remote_host_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.sent_probe_hashes (
    sent_probe_hash_id bigint NOT NULL,
    sent_probe_id bigint NOT NULL,
    contact_id bigint,
    group_member_id bigint,
    user_id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);



ALTER TABLE test_chat_schema.sent_probe_hashes ALTER COLUMN sent_probe_hash_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.sent_probe_hashes_sent_probe_hash_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.sent_probes (
    sent_probe_id bigint NOT NULL,
    contact_id bigint,
    group_member_id bigint,
    probe bytea NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);



ALTER TABLE test_chat_schema.sent_probes ALTER COLUMN sent_probe_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.sent_probes_sent_probe_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.server_operators (
    server_operator_id bigint NOT NULL,
    server_operator_tag text,
    trade_name text NOT NULL,
    legal_name text,
    server_domains text,
    enabled smallint DEFAULT 1 NOT NULL,
    smp_role_storage smallint DEFAULT 1 NOT NULL,
    smp_role_proxy smallint DEFAULT 1 NOT NULL,
    xftp_role_storage smallint DEFAULT 1 NOT NULL,
    xftp_role_proxy smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.server_operators ALTER COLUMN server_operator_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.server_operators_server_operator_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.settings (
    settings_id bigint NOT NULL,
    chat_item_ttl bigint,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.settings ALTER COLUMN settings_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.settings_settings_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.snd_files (
    file_id bigint NOT NULL,
    connection_id bigint NOT NULL,
    file_status text NOT NULL,
    group_member_id bigint,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    file_inline text,
    last_inline_msg_delivery_id bigint,
    file_descr_id bigint
);



CREATE TABLE test_chat_schema.usage_conditions (
    usage_conditions_id bigint NOT NULL,
    conditions_commit text NOT NULL,
    notified_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



CREATE TABLE test_chat_schema.user_contact_links (
    user_contact_link_id bigint NOT NULL,
    conn_req_contact bytea NOT NULL,
    local_display_name text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    user_id bigint NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    auto_accept smallint DEFAULT 0,
    auto_reply_msg_content text,
    group_id bigint,
    auto_accept_incognito smallint DEFAULT 0 NOT NULL,
    group_link_id bytea,
    group_link_member_role text,
    business_address smallint DEFAULT 0,
    short_link_contact bytea,
    short_link_data_set smallint DEFAULT 0 NOT NULL,
    short_link_large_data_set smallint DEFAULT 0 NOT NULL
);



ALTER TABLE test_chat_schema.user_contact_links ALTER COLUMN user_contact_link_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.user_contact_links_user_contact_link_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.users (
    user_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    local_display_name text NOT NULL,
    active_user smallint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    agent_user_id bigint NOT NULL,
    view_pwd_hash bytea,
    view_pwd_salt bytea,
    show_ntfs smallint DEFAULT 1 NOT NULL,
    send_rcpts_contacts smallint DEFAULT 0 NOT NULL,
    send_rcpts_small_groups smallint DEFAULT 0 NOT NULL,
    user_member_profile_updated_at timestamp with time zone,
    ui_themes text,
    active_order bigint DEFAULT 0 NOT NULL,
    auto_accept_member_contacts smallint DEFAULT 0 NOT NULL
);



ALTER TABLE test_chat_schema.users ALTER COLUMN user_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.users_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE test_chat_schema.xftp_file_descriptions (
    file_descr_id bigint NOT NULL,
    user_id bigint NOT NULL,
    file_descr_text text NOT NULL,
    file_descr_part_no bigint DEFAULT 0 NOT NULL,
    file_descr_complete smallint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);



ALTER TABLE test_chat_schema.xftp_file_descriptions ALTER COLUMN file_descr_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME test_chat_schema.xftp_file_descriptions_file_descr_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



ALTER TABLE ONLY test_chat_schema.calls
    ADD CONSTRAINT calls_pkey PRIMARY KEY (call_id);



ALTER TABLE ONLY test_chat_schema.chat_item_mentions
    ADD CONSTRAINT chat_item_mentions_pkey PRIMARY KEY (chat_item_mention_id);



ALTER TABLE ONLY test_chat_schema.chat_item_messages
    ADD CONSTRAINT chat_item_messages_chat_item_id_message_id_key UNIQUE (chat_item_id, message_id);



ALTER TABLE ONLY test_chat_schema.chat_item_messages
    ADD CONSTRAINT chat_item_messages_message_id_key UNIQUE (message_id);



ALTER TABLE ONLY test_chat_schema.chat_item_moderations
    ADD CONSTRAINT chat_item_moderations_pkey PRIMARY KEY (chat_item_moderation_id);



ALTER TABLE ONLY test_chat_schema.chat_item_reactions
    ADD CONSTRAINT chat_item_reactions_pkey PRIMARY KEY (chat_item_reaction_id);



ALTER TABLE ONLY test_chat_schema.chat_item_versions
    ADD CONSTRAINT chat_item_versions_pkey PRIMARY KEY (chat_item_version_id);



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_created_by_msg_id_key UNIQUE (created_by_msg_id);



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_pkey PRIMARY KEY (chat_item_id);



ALTER TABLE ONLY test_chat_schema.chat_tags
    ADD CONSTRAINT chat_tags_pkey PRIMARY KEY (chat_tag_id);



ALTER TABLE ONLY test_chat_schema.commands
    ADD CONSTRAINT commands_pkey PRIMARY KEY (command_id);



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_agent_conn_id_key UNIQUE (agent_conn_id);



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_pkey PRIMARY KEY (connection_id);



ALTER TABLE ONLY test_chat_schema.connections_sync
    ADD CONSTRAINT connections_sync_pkey PRIMARY KEY (connections_sync_id);



ALTER TABLE ONLY test_chat_schema.contact_profiles
    ADD CONSTRAINT contact_profiles_pkey PRIMARY KEY (contact_profile_id);



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_pkey PRIMARY KEY (contact_request_id);



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_user_id_contact_profile_id_key UNIQUE (user_id, contact_profile_id);



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_user_id_local_display_name_key UNIQUE (user_id, local_display_name);



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (contact_id);



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_user_id_contact_profile_id_key UNIQUE (user_id, contact_profile_id);



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_user_id_local_display_name_key UNIQUE (user_id, local_display_name);



ALTER TABLE ONLY test_chat_schema.delivery_jobs
    ADD CONSTRAINT delivery_jobs_pkey PRIMARY KEY (delivery_job_id);



ALTER TABLE ONLY test_chat_schema.delivery_tasks
    ADD CONSTRAINT delivery_tasks_pkey PRIMARY KEY (delivery_task_id);



ALTER TABLE ONLY test_chat_schema.display_names
    ADD CONSTRAINT display_names_pkey PRIMARY KEY (user_id, local_display_name);



ALTER TABLE ONLY test_chat_schema.display_names
    ADD CONSTRAINT display_names_user_id_ldn_base_ldn_suffix_key UNIQUE (user_id, ldn_base, ldn_suffix);



ALTER TABLE ONLY test_chat_schema.extra_xftp_file_descriptions
    ADD CONSTRAINT extra_xftp_file_descriptions_pkey PRIMARY KEY (extra_file_descr_id);



ALTER TABLE ONLY test_chat_schema.files
    ADD CONSTRAINT files_pkey PRIMARY KEY (file_id);



ALTER TABLE ONLY test_chat_schema.group_member_intros
    ADD CONSTRAINT group_member_intros_pkey PRIMARY KEY (group_member_intro_id);



ALTER TABLE ONLY test_chat_schema.group_member_intros
    ADD CONSTRAINT group_member_intros_re_group_member_id_to_group_member_id_key UNIQUE (re_group_member_id, to_group_member_id);



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_group_id_member_id_key UNIQUE (group_id, member_id);



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_pkey PRIMARY KEY (group_member_id);



ALTER TABLE ONLY test_chat_schema.group_profiles
    ADD CONSTRAINT group_profiles_pkey PRIMARY KEY (group_profile_id);



ALTER TABLE ONLY test_chat_schema.group_snd_item_statuses
    ADD CONSTRAINT group_snd_item_statuses_pkey PRIMARY KEY (group_snd_item_status_id);



ALTER TABLE ONLY test_chat_schema.groups
    ADD CONSTRAINT groups_pkey PRIMARY KEY (group_id);



ALTER TABLE ONLY test_chat_schema.groups
    ADD CONSTRAINT groups_user_id_group_profile_id_key UNIQUE (user_id, group_profile_id);



ALTER TABLE ONLY test_chat_schema.groups
    ADD CONSTRAINT groups_user_id_local_display_name_key UNIQUE (user_id, local_display_name);



ALTER TABLE ONLY test_chat_schema.known_servers
    ADD CONSTRAINT known_servers_pkey PRIMARY KEY (server_id);



ALTER TABLE ONLY test_chat_schema.known_servers
    ADD CONSTRAINT known_servers_user_id_host_port_key UNIQUE (user_id, host, port);



ALTER TABLE ONLY test_chat_schema.messages
    ADD CONSTRAINT messages_pkey PRIMARY KEY (message_id);



ALTER TABLE ONLY test_chat_schema.migrations
    ADD CONSTRAINT migrations_pkey PRIMARY KEY (name);



ALTER TABLE ONLY test_chat_schema.msg_deliveries
    ADD CONSTRAINT msg_deliveries_pkey PRIMARY KEY (msg_delivery_id);



ALTER TABLE ONLY test_chat_schema.note_folders
    ADD CONSTRAINT note_folders_pkey PRIMARY KEY (note_folder_id);



ALTER TABLE ONLY test_chat_schema.operator_usage_conditions
    ADD CONSTRAINT operator_usage_conditions_pkey PRIMARY KEY (operator_usage_conditions_id);



ALTER TABLE ONLY test_chat_schema.pending_group_messages
    ADD CONSTRAINT pending_group_messages_pkey PRIMARY KEY (pending_group_message_id);



ALTER TABLE ONLY test_chat_schema.protocol_servers
    ADD CONSTRAINT protocol_servers_pkey PRIMARY KEY (smp_server_id);



ALTER TABLE ONLY test_chat_schema.protocol_servers
    ADD CONSTRAINT protocol_servers_user_id_host_port_key UNIQUE (user_id, host, port);



ALTER TABLE ONLY test_chat_schema.rcv_file_chunks
    ADD CONSTRAINT rcv_file_chunks_pkey PRIMARY KEY (file_id, chunk_number);



ALTER TABLE ONLY test_chat_schema.rcv_files
    ADD CONSTRAINT rcv_files_pkey PRIMARY KEY (file_id);



ALTER TABLE ONLY test_chat_schema.received_probes
    ADD CONSTRAINT received_probes_pkey PRIMARY KEY (received_probe_id);



ALTER TABLE ONLY test_chat_schema.remote_controllers
    ADD CONSTRAINT remote_controllers_pkey PRIMARY KEY (remote_ctrl_id);



ALTER TABLE ONLY test_chat_schema.remote_hosts
    ADD CONSTRAINT remote_hosts_pkey PRIMARY KEY (remote_host_id);



ALTER TABLE ONLY test_chat_schema.sent_probe_hashes
    ADD CONSTRAINT sent_probe_hashes_pkey PRIMARY KEY (sent_probe_hash_id);



ALTER TABLE ONLY test_chat_schema.sent_probes
    ADD CONSTRAINT sent_probes_pkey PRIMARY KEY (sent_probe_id);



ALTER TABLE ONLY test_chat_schema.sent_probes
    ADD CONSTRAINT sent_probes_user_id_probe_key UNIQUE (user_id, probe);



ALTER TABLE ONLY test_chat_schema.server_operators
    ADD CONSTRAINT server_operators_pkey PRIMARY KEY (server_operator_id);



ALTER TABLE ONLY test_chat_schema.settings
    ADD CONSTRAINT settings_pkey PRIMARY KEY (settings_id);



ALTER TABLE ONLY test_chat_schema.snd_files
    ADD CONSTRAINT snd_files_pkey PRIMARY KEY (file_id, connection_id);



ALTER TABLE ONLY test_chat_schema.usage_conditions
    ADD CONSTRAINT usage_conditions_conditions_commit_key UNIQUE (conditions_commit);



ALTER TABLE ONLY test_chat_schema.usage_conditions
    ADD CONSTRAINT usage_conditions_pkey PRIMARY KEY (usage_conditions_id);



ALTER TABLE ONLY test_chat_schema.user_contact_links
    ADD CONSTRAINT user_contact_links_pkey PRIMARY KEY (user_contact_link_id);



ALTER TABLE ONLY test_chat_schema.user_contact_links
    ADD CONSTRAINT user_contact_links_user_id_local_display_name_key UNIQUE (user_id, local_display_name);



ALTER TABLE ONLY test_chat_schema.users
    ADD CONSTRAINT users_contact_id_key UNIQUE (contact_id);



ALTER TABLE ONLY test_chat_schema.users
    ADD CONSTRAINT users_local_display_name_key UNIQUE (local_display_name);



ALTER TABLE ONLY test_chat_schema.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (user_id);



ALTER TABLE ONLY test_chat_schema.xftp_file_descriptions
    ADD CONSTRAINT xftp_file_descriptions_pkey PRIMARY KEY (file_descr_id);



CREATE INDEX chat_items_note_folder_id ON test_chat_schema.chat_items USING btree (note_folder_id);



CREATE INDEX contact_profiles_index ON test_chat_schema.contact_profiles USING btree (display_name, full_name);



CREATE INDEX files_note_folder_id ON test_chat_schema.files USING btree (note_folder_id);



CREATE INDEX idx_calls_chat_item_id ON test_chat_schema.calls USING btree (chat_item_id);



CREATE INDEX idx_calls_contact_id ON test_chat_schema.calls USING btree (contact_id);



CREATE INDEX idx_calls_user_id ON test_chat_schema.calls USING btree (user_id);



CREATE INDEX idx_chat_item_mentions_chat_item_id ON test_chat_schema.chat_item_mentions USING btree (chat_item_id);



CREATE UNIQUE INDEX idx_chat_item_mentions_display_name ON test_chat_schema.chat_item_mentions USING btree (chat_item_id, display_name);



CREATE INDEX idx_chat_item_mentions_group_id ON test_chat_schema.chat_item_mentions USING btree (group_id);



CREATE UNIQUE INDEX idx_chat_item_mentions_member_id ON test_chat_schema.chat_item_mentions USING btree (chat_item_id, member_id);



CREATE INDEX idx_chat_item_moderations_created_by_msg_id ON test_chat_schema.chat_item_moderations USING btree (created_by_msg_id);



CREATE INDEX idx_chat_item_moderations_group ON test_chat_schema.chat_item_moderations USING btree (group_id, item_member_id, shared_msg_id);



CREATE INDEX idx_chat_item_moderations_group_id ON test_chat_schema.chat_item_moderations USING btree (group_id);



CREATE INDEX idx_chat_item_moderations_moderator_member_id ON test_chat_schema.chat_item_moderations USING btree (moderator_member_id);



CREATE INDEX idx_chat_item_reactions_contact ON test_chat_schema.chat_item_reactions USING btree (contact_id, shared_msg_id);



CREATE INDEX idx_chat_item_reactions_contact_id ON test_chat_schema.chat_item_reactions USING btree (contact_id);



CREATE INDEX idx_chat_item_reactions_created_by_msg_id ON test_chat_schema.chat_item_reactions USING btree (created_by_msg_id);



CREATE INDEX idx_chat_item_reactions_group ON test_chat_schema.chat_item_reactions USING btree (group_id, shared_msg_id);



CREATE INDEX idx_chat_item_reactions_group_id ON test_chat_schema.chat_item_reactions USING btree (group_id);



CREATE INDEX idx_chat_item_reactions_group_member_id ON test_chat_schema.chat_item_reactions USING btree (group_member_id);



CREATE INDEX idx_chat_item_reactions_shared_msg_id ON test_chat_schema.chat_item_reactions USING btree (shared_msg_id);



CREATE INDEX idx_chat_item_versions_chat_item_id ON test_chat_schema.chat_item_versions USING btree (chat_item_id);



CREATE INDEX idx_chat_items_contact_id ON test_chat_schema.chat_items USING btree (contact_id);



CREATE INDEX idx_chat_items_contacts ON test_chat_schema.chat_items USING btree (user_id, contact_id, item_status, created_at);



CREATE INDEX idx_chat_items_contacts_created_at ON test_chat_schema.chat_items USING btree (user_id, contact_id, created_at);



CREATE INDEX idx_chat_items_contacts_msg_content_tag_created_at ON test_chat_schema.chat_items USING btree (user_id, contact_id, msg_content_tag, created_at);



CREATE UNIQUE INDEX idx_chat_items_direct_shared_msg_id ON test_chat_schema.chat_items USING btree (user_id, contact_id, shared_msg_id);



CREATE INDEX idx_chat_items_forwarded_by_group_member_id ON test_chat_schema.chat_items USING btree (forwarded_by_group_member_id);



CREATE INDEX idx_chat_items_fwd_from_chat_item_id ON test_chat_schema.chat_items USING btree (fwd_from_chat_item_id);



CREATE INDEX idx_chat_items_fwd_from_contact_id ON test_chat_schema.chat_items USING btree (fwd_from_contact_id);



CREATE INDEX idx_chat_items_fwd_from_group_id ON test_chat_schema.chat_items USING btree (fwd_from_group_id);



CREATE INDEX idx_chat_items_group_id ON test_chat_schema.chat_items USING btree (group_id);



CREATE INDEX idx_chat_items_group_id_shared_msg_id ON test_chat_schema.chat_items USING btree (group_id, shared_msg_id);



CREATE INDEX idx_chat_items_group_member_id ON test_chat_schema.chat_items USING btree (group_member_id);



CREATE INDEX idx_chat_items_group_scope_group_member_id ON test_chat_schema.chat_items USING btree (group_scope_group_member_id);



CREATE INDEX idx_chat_items_group_scope_item_status ON test_chat_schema.chat_items USING btree (user_id, group_id, group_scope_tag, group_scope_group_member_id, item_status, item_ts);



CREATE INDEX idx_chat_items_group_scope_item_ts ON test_chat_schema.chat_items USING btree (user_id, group_id, group_scope_tag, group_scope_group_member_id, item_ts);



CREATE INDEX idx_chat_items_group_scope_stats_all ON test_chat_schema.chat_items USING btree (user_id, group_id, group_scope_tag, group_scope_group_member_id, item_status, chat_item_id, user_mention);



CREATE UNIQUE INDEX idx_chat_items_group_shared_msg_id ON test_chat_schema.chat_items USING btree (user_id, group_id, group_member_id, shared_msg_id);



CREATE INDEX idx_chat_items_groups ON test_chat_schema.chat_items USING btree (user_id, group_id, item_status, item_ts);



CREATE INDEX idx_chat_items_groups_history ON test_chat_schema.chat_items USING btree (user_id, group_id, include_in_history, item_deleted, item_ts, chat_item_id);



CREATE INDEX idx_chat_items_groups_item_ts ON test_chat_schema.chat_items USING btree (user_id, group_id, item_ts);



CREATE INDEX idx_chat_items_groups_msg_content_tag_deleted ON test_chat_schema.chat_items USING btree (user_id, group_id, msg_content_tag, item_deleted, item_sent);



CREATE INDEX idx_chat_items_groups_msg_content_tag_item_ts ON test_chat_schema.chat_items USING btree (user_id, group_id, msg_content_tag, item_ts);



CREATE INDEX idx_chat_items_groups_user_mention ON test_chat_schema.chat_items USING btree (user_id, group_id, item_status, user_mention);



CREATE INDEX idx_chat_items_item_deleted_by_group_member_id ON test_chat_schema.chat_items USING btree (item_deleted_by_group_member_id);



CREATE INDEX idx_chat_items_item_status ON test_chat_schema.chat_items USING btree (item_status);



CREATE INDEX idx_chat_items_note_folder_msg_content_tag_created_at ON test_chat_schema.chat_items USING btree (user_id, note_folder_id, msg_content_tag, created_at);



CREATE INDEX idx_chat_items_groups_has_link_item_ts ON test_chat_schema.chat_items USING btree (user_id, group_id, has_link, item_ts);



CREATE INDEX idx_chat_items_contacts_has_link_created_at ON test_chat_schema.chat_items USING btree (user_id, contact_id, has_link, created_at);



CREATE INDEX idx_chat_items_note_folder_has_link_created_at ON test_chat_schema.chat_items USING btree (user_id, note_folder_id, has_link, created_at);



CREATE INDEX idx_chat_items_notes ON test_chat_schema.chat_items USING btree (user_id, note_folder_id, item_status, created_at);



CREATE INDEX idx_chat_items_notes_created_at ON test_chat_schema.chat_items USING btree (user_id, note_folder_id, created_at);



CREATE INDEX idx_chat_items_timed_delete_at ON test_chat_schema.chat_items USING btree (user_id, timed_delete_at);



CREATE INDEX idx_chat_items_user_id_item_status ON test_chat_schema.chat_items USING btree (user_id, item_status);



CREATE INDEX idx_chat_tags_chats_chat_tag_id ON test_chat_schema.chat_tags_chats USING btree (chat_tag_id);



CREATE UNIQUE INDEX idx_chat_tags_chats_chat_tag_id_contact_id ON test_chat_schema.chat_tags_chats USING btree (contact_id, chat_tag_id);



CREATE UNIQUE INDEX idx_chat_tags_chats_chat_tag_id_group_id ON test_chat_schema.chat_tags_chats USING btree (group_id, chat_tag_id);



CREATE INDEX idx_chat_tags_user_id ON test_chat_schema.chat_tags USING btree (user_id);



CREATE UNIQUE INDEX idx_chat_tags_user_id_chat_tag_emoji ON test_chat_schema.chat_tags USING btree (user_id, chat_tag_emoji);



CREATE UNIQUE INDEX idx_chat_tags_user_id_chat_tag_text ON test_chat_schema.chat_tags USING btree (user_id, chat_tag_text);



CREATE INDEX idx_commands_connection_id ON test_chat_schema.commands USING btree (connection_id);



CREATE INDEX idx_commands_user_id ON test_chat_schema.commands USING btree (user_id);



CREATE INDEX idx_connections_conn_req_inv ON test_chat_schema.connections USING btree (user_id, conn_req_inv);



CREATE UNIQUE INDEX idx_connections_contact_id ON test_chat_schema.connections USING btree (contact_id);



CREATE INDEX idx_connections_custom_user_profile_id ON test_chat_schema.connections USING btree (custom_user_profile_id);



CREATE INDEX idx_connections_group_member ON test_chat_schema.connections USING btree (user_id, group_member_id);



CREATE UNIQUE INDEX idx_connections_group_member_id ON test_chat_schema.connections USING btree (group_member_id);



CREATE INDEX idx_connections_rcv_file_id ON test_chat_schema.connections USING btree (rcv_file_id);



CREATE INDEX idx_connections_to_subscribe ON test_chat_schema.connections USING btree (user_id, to_subscribe);



CREATE INDEX idx_connections_updated_at ON test_chat_schema.connections USING btree (user_id, updated_at);



CREATE INDEX idx_connections_user_contact_link_id ON test_chat_schema.connections USING btree (user_contact_link_id);



CREATE INDEX idx_connections_via_contact ON test_chat_schema.connections USING btree (via_contact);



CREATE INDEX idx_connections_via_contact_uri_hash ON test_chat_schema.connections USING btree (user_id, via_contact_uri_hash);



CREATE INDEX idx_connections_via_user_contact_link ON test_chat_schema.connections USING btree (via_user_contact_link);



CREATE INDEX idx_contact_profiles_contact_link ON test_chat_schema.contact_profiles USING btree (user_id, contact_link);



CREATE INDEX idx_contact_profiles_user_id ON test_chat_schema.contact_profiles USING btree (user_id);



CREATE INDEX idx_contact_requests_business_group_id ON test_chat_schema.contact_requests USING btree (business_group_id);



CREATE INDEX idx_contact_requests_contact_id ON test_chat_schema.contact_requests USING btree (contact_id);



CREATE INDEX idx_contact_requests_contact_profile_id ON test_chat_schema.contact_requests USING btree (contact_profile_id);



CREATE INDEX idx_contact_requests_updated_at ON test_chat_schema.contact_requests USING btree (user_id, updated_at);



CREATE INDEX idx_contact_requests_user_contact_link_id ON test_chat_schema.contact_requests USING btree (user_contact_link_id);



CREATE INDEX idx_contact_requests_xcontact_id ON test_chat_schema.contact_requests USING btree (user_id, xcontact_id);



CREATE INDEX idx_contacts_chat_ts ON test_chat_schema.contacts USING btree (user_id, chat_ts);



CREATE INDEX idx_contacts_contact_group_member_id ON test_chat_schema.contacts USING btree (contact_group_member_id);



CREATE INDEX idx_contacts_contact_profile_id ON test_chat_schema.contacts USING btree (contact_profile_id);



CREATE INDEX idx_contacts_contact_request_id ON test_chat_schema.contacts USING btree (contact_request_id);



CREATE INDEX idx_contacts_grp_direct_inv_from_group_id ON test_chat_schema.contacts USING btree (grp_direct_inv_from_group_id);



CREATE INDEX idx_contacts_grp_direct_inv_from_group_member_id ON test_chat_schema.contacts USING btree (grp_direct_inv_from_group_member_id);



CREATE INDEX idx_contacts_grp_direct_inv_from_member_conn_id ON test_chat_schema.contacts USING btree (grp_direct_inv_from_member_conn_id);



CREATE INDEX idx_contacts_xcontact_id ON test_chat_schema.contacts USING btree (xcontact_id);



CREATE INDEX idx_delivery_jobs_created_at ON test_chat_schema.delivery_jobs USING btree (created_at);



CREATE INDEX idx_delivery_jobs_group_id ON test_chat_schema.delivery_jobs USING btree (group_id);



CREATE INDEX idx_delivery_jobs_job_scope_support_gm_id ON test_chat_schema.delivery_jobs USING btree (job_scope_support_gm_id);



CREATE INDEX idx_delivery_jobs_next ON test_chat_schema.delivery_jobs USING btree (group_id, worker_scope, failed, job_status);



CREATE INDEX idx_delivery_jobs_single_sender_group_member_id ON test_chat_schema.delivery_jobs USING btree (single_sender_group_member_id);



CREATE INDEX idx_delivery_tasks_created_at ON test_chat_schema.delivery_tasks USING btree (created_at);



CREATE INDEX idx_delivery_tasks_group_id ON test_chat_schema.delivery_tasks USING btree (group_id);



CREATE INDEX idx_delivery_tasks_job_scope_support_gm_id ON test_chat_schema.delivery_tasks USING btree (job_scope_support_gm_id);



CREATE INDEX idx_delivery_tasks_message_id ON test_chat_schema.delivery_tasks USING btree (message_id);



CREATE INDEX idx_delivery_tasks_next ON test_chat_schema.delivery_tasks USING btree (group_id, worker_scope, failed, task_status);



CREATE INDEX idx_delivery_tasks_next_for_job_scope ON test_chat_schema.delivery_tasks USING btree (group_id, worker_scope, job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id, failed, task_status);



CREATE INDEX idx_delivery_tasks_next_for_job_scope_sender ON test_chat_schema.delivery_tasks USING btree (group_id, worker_scope, job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id, sender_group_member_id, failed, task_status);



CREATE INDEX idx_delivery_tasks_sender_group_member_id ON test_chat_schema.delivery_tasks USING btree (sender_group_member_id);



CREATE INDEX idx_extra_xftp_file_descriptions_file_id ON test_chat_schema.extra_xftp_file_descriptions USING btree (file_id);



CREATE INDEX idx_extra_xftp_file_descriptions_user_id ON test_chat_schema.extra_xftp_file_descriptions USING btree (user_id);



CREATE INDEX idx_files_chat_item_id ON test_chat_schema.files USING btree (chat_item_id);



CREATE INDEX idx_files_contact_id ON test_chat_schema.files USING btree (contact_id);



CREATE INDEX idx_files_group_id ON test_chat_schema.files USING btree (group_id);



CREATE INDEX idx_files_redirect_file_id ON test_chat_schema.files USING btree (redirect_file_id);



CREATE INDEX idx_files_user_id ON test_chat_schema.files USING btree (user_id);



CREATE INDEX idx_group_member_intros_re_group_member_id ON test_chat_schema.group_member_intros USING btree (re_group_member_id);



CREATE INDEX idx_group_member_intros_to_group_member_id ON test_chat_schema.group_member_intros USING btree (to_group_member_id);



CREATE INDEX idx_group_members_contact_id ON test_chat_schema.group_members USING btree (contact_id);



CREATE INDEX idx_group_members_contact_profile_id ON test_chat_schema.group_members USING btree (contact_profile_id);



CREATE INDEX idx_group_members_group_id ON test_chat_schema.group_members USING btree (user_id, group_id);



CREATE UNIQUE INDEX idx_group_members_group_id_index_in_group ON test_chat_schema.group_members USING btree (group_id, index_in_group);



CREATE INDEX idx_group_members_invited_by ON test_chat_schema.group_members USING btree (invited_by);



CREATE INDEX idx_group_members_invited_by_group_member_id ON test_chat_schema.group_members USING btree (invited_by_group_member_id);



CREATE INDEX idx_group_members_member_profile_id ON test_chat_schema.group_members USING btree (member_profile_id);



CREATE INDEX idx_group_members_user_id ON test_chat_schema.group_members USING btree (user_id);



CREATE INDEX idx_group_members_user_id_local_display_name ON test_chat_schema.group_members USING btree (user_id, local_display_name);



CREATE INDEX idx_group_profiles_user_id ON test_chat_schema.group_profiles USING btree (user_id);



CREATE INDEX idx_group_snd_item_statuses_chat_item_id ON test_chat_schema.group_snd_item_statuses USING btree (chat_item_id);



CREATE INDEX idx_group_snd_item_statuses_chat_item_id_group_member_id ON test_chat_schema.group_snd_item_statuses USING btree (chat_item_id, group_member_id);



CREATE INDEX idx_group_snd_item_statuses_group_member_id ON test_chat_schema.group_snd_item_statuses USING btree (group_member_id);



CREATE INDEX idx_groups_business_xcontact_id ON test_chat_schema.groups USING btree (business_xcontact_id);



CREATE INDEX idx_groups_chat_item_id ON test_chat_schema.groups USING btree (chat_item_id);



CREATE INDEX idx_groups_chat_ts ON test_chat_schema.groups USING btree (user_id, chat_ts);



CREATE INDEX idx_groups_group_profile_id ON test_chat_schema.groups USING btree (group_profile_id);



CREATE INDEX idx_groups_inv_queue_info ON test_chat_schema.groups USING btree (inv_queue_info);



CREATE INDEX idx_groups_summary_current_members_count ON test_chat_schema.groups USING btree (summary_current_members_count);



CREATE INDEX idx_groups_via_group_link_uri_hash ON test_chat_schema.groups USING btree (user_id, via_group_link_uri_hash);



CREATE INDEX idx_messages_author_group_member_id ON test_chat_schema.messages USING btree (author_group_member_id);



CREATE INDEX idx_messages_connection_id ON test_chat_schema.messages USING btree (connection_id);



CREATE INDEX idx_messages_created_at ON test_chat_schema.messages USING btree (created_at);



CREATE INDEX idx_messages_forwarded_by_group_member_id ON test_chat_schema.messages USING btree (forwarded_by_group_member_id);



CREATE INDEX idx_messages_group_id ON test_chat_schema.messages USING btree (group_id);



CREATE INDEX idx_messages_group_id_shared_msg_id ON test_chat_schema.messages USING btree (group_id, shared_msg_id);



CREATE INDEX idx_messages_shared_msg_id ON test_chat_schema.messages USING btree (shared_msg_id);



CREATE INDEX idx_msg_deliveries_agent_msg_id ON test_chat_schema.msg_deliveries USING btree (connection_id, agent_msg_id);



CREATE INDEX idx_msg_deliveries_message_id ON test_chat_schema.msg_deliveries USING btree (message_id);



CREATE UNIQUE INDEX idx_operator_usage_conditions_conditions_commit ON test_chat_schema.operator_usage_conditions USING btree (conditions_commit, server_operator_id);



CREATE INDEX idx_operator_usage_conditions_server_operator_id ON test_chat_schema.operator_usage_conditions USING btree (server_operator_id);



CREATE INDEX idx_pending_group_messages_group_member_id ON test_chat_schema.pending_group_messages USING btree (group_member_id);



CREATE INDEX idx_pending_group_messages_message_id ON test_chat_schema.pending_group_messages USING btree (message_id);



CREATE INDEX idx_rcv_file_chunks_file_id ON test_chat_schema.rcv_file_chunks USING btree (file_id);



CREATE INDEX idx_rcv_files_file_descr_id ON test_chat_schema.rcv_files USING btree (file_descr_id);



CREATE INDEX idx_rcv_files_group_member_id ON test_chat_schema.rcv_files USING btree (group_member_id);



CREATE INDEX idx_received_probes_contact_id ON test_chat_schema.received_probes USING btree (contact_id);



CREATE INDEX idx_received_probes_created_at ON test_chat_schema.received_probes USING btree (created_at);



CREATE INDEX idx_received_probes_group_member_id ON test_chat_schema.received_probes USING btree (group_member_id);



CREATE INDEX idx_received_probes_probe ON test_chat_schema.received_probes USING btree (probe);



CREATE INDEX idx_received_probes_probe_hash ON test_chat_schema.received_probes USING btree (probe_hash);



CREATE INDEX idx_received_probes_user_id ON test_chat_schema.received_probes USING btree (user_id);



CREATE UNIQUE INDEX idx_remote_controllers_ctrl_fingerprint ON test_chat_schema.remote_controllers USING btree (ctrl_fingerprint);



CREATE UNIQUE INDEX idx_remote_hosts_host_fingerprint ON test_chat_schema.remote_hosts USING btree (host_fingerprint);



CREATE INDEX idx_sent_probe_hashes_contact_id ON test_chat_schema.sent_probe_hashes USING btree (contact_id);



CREATE INDEX idx_sent_probe_hashes_created_at ON test_chat_schema.sent_probe_hashes USING btree (created_at);



CREATE INDEX idx_sent_probe_hashes_group_member_id ON test_chat_schema.sent_probe_hashes USING btree (group_member_id);



CREATE INDEX idx_sent_probe_hashes_sent_probe_id ON test_chat_schema.sent_probe_hashes USING btree (sent_probe_id);



CREATE INDEX idx_sent_probe_hashes_user_id ON test_chat_schema.sent_probe_hashes USING btree (user_id);



CREATE INDEX idx_sent_probes_contact_id ON test_chat_schema.sent_probes USING btree (contact_id);



CREATE INDEX idx_sent_probes_created_at ON test_chat_schema.sent_probes USING btree (created_at);



CREATE INDEX idx_sent_probes_group_member_id ON test_chat_schema.sent_probes USING btree (group_member_id);



CREATE INDEX idx_sent_probes_user_id ON test_chat_schema.sent_probes USING btree (user_id);



CREATE INDEX idx_settings_user_id ON test_chat_schema.settings USING btree (user_id);



CREATE INDEX idx_smp_servers_user_id ON test_chat_schema.protocol_servers USING btree (user_id);



CREATE INDEX idx_snd_files_connection_id ON test_chat_schema.snd_files USING btree (connection_id);



CREATE INDEX idx_snd_files_file_descr_id ON test_chat_schema.snd_files USING btree (file_descr_id);



CREATE INDEX idx_snd_files_file_id ON test_chat_schema.snd_files USING btree (file_id);



CREATE INDEX idx_snd_files_group_member_id ON test_chat_schema.snd_files USING btree (group_member_id);



CREATE UNIQUE INDEX idx_snd_files_last_inline_msg_delivery_id ON test_chat_schema.snd_files USING btree (last_inline_msg_delivery_id);



CREATE UNIQUE INDEX idx_user_contact_links_group_id ON test_chat_schema.user_contact_links USING btree (group_id);



CREATE INDEX idx_xftp_file_descriptions_user_id ON test_chat_schema.xftp_file_descriptions USING btree (user_id);



CREATE INDEX note_folders_user_id ON test_chat_schema.note_folders USING btree (user_id);



CREATE TRIGGER tr_group_members_delete_update_summary AFTER DELETE ON test_chat_schema.group_members FOR EACH ROW EXECUTE FUNCTION test_chat_schema.on_group_members_delete_update_summary();



CREATE TRIGGER tr_group_members_insert_update_summary AFTER INSERT ON test_chat_schema.group_members FOR EACH ROW EXECUTE FUNCTION test_chat_schema.on_group_members_insert_update_summary();



CREATE TRIGGER tr_group_members_update_update_summary AFTER UPDATE ON test_chat_schema.group_members FOR EACH ROW EXECUTE FUNCTION test_chat_schema.on_group_members_update_update_summary();



ALTER TABLE ONLY test_chat_schema.calls
    ADD CONSTRAINT calls_chat_item_id_fkey FOREIGN KEY (chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.calls
    ADD CONSTRAINT calls_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.calls
    ADD CONSTRAINT calls_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_mentions
    ADD CONSTRAINT chat_item_mentions_chat_item_id_fkey FOREIGN KEY (chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_mentions
    ADD CONSTRAINT chat_item_mentions_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_messages
    ADD CONSTRAINT chat_item_messages_chat_item_id_fkey FOREIGN KEY (chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_messages
    ADD CONSTRAINT chat_item_messages_message_id_fkey FOREIGN KEY (message_id) REFERENCES test_chat_schema.messages(message_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_moderations
    ADD CONSTRAINT chat_item_moderations_created_by_msg_id_fkey FOREIGN KEY (created_by_msg_id) REFERENCES test_chat_schema.messages(message_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_item_moderations
    ADD CONSTRAINT chat_item_moderations_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_moderations
    ADD CONSTRAINT chat_item_moderations_moderator_member_id_fkey FOREIGN KEY (moderator_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_reactions
    ADD CONSTRAINT chat_item_reactions_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_reactions
    ADD CONSTRAINT chat_item_reactions_created_by_msg_id_fkey FOREIGN KEY (created_by_msg_id) REFERENCES test_chat_schema.messages(message_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_item_reactions
    ADD CONSTRAINT chat_item_reactions_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_item_reactions
    ADD CONSTRAINT chat_item_reactions_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_item_versions
    ADD CONSTRAINT chat_item_versions_chat_item_id_fkey FOREIGN KEY (chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_created_by_msg_id_fkey FOREIGN KEY (created_by_msg_id) REFERENCES test_chat_schema.messages(message_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_forwarded_by_group_member_id_fkey FOREIGN KEY (forwarded_by_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_fwd_from_chat_item_id_fkey FOREIGN KEY (fwd_from_chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_fwd_from_contact_id_fkey FOREIGN KEY (fwd_from_contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_fwd_from_group_id_fkey FOREIGN KEY (fwd_from_group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_group_scope_group_member_id_fkey FOREIGN KEY (group_scope_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_item_deleted_by_group_member_id_fkey FOREIGN KEY (item_deleted_by_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT chat_items_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_tags_chats
    ADD CONSTRAINT chat_tags_chats_chat_tag_id_fkey FOREIGN KEY (chat_tag_id) REFERENCES test_chat_schema.chat_tags(chat_tag_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_tags_chats
    ADD CONSTRAINT chat_tags_chats_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_tags_chats
    ADD CONSTRAINT chat_tags_chats_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_tags
    ADD CONSTRAINT chat_tags_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.commands
    ADD CONSTRAINT commands_connection_id_fkey FOREIGN KEY (connection_id) REFERENCES test_chat_schema.connections(connection_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.commands
    ADD CONSTRAINT commands_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_custom_user_profile_id_fkey FOREIGN KEY (custom_user_profile_id) REFERENCES test_chat_schema.contact_profiles(contact_profile_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_rcv_file_id_fkey FOREIGN KEY (rcv_file_id) REFERENCES test_chat_schema.rcv_files(file_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_snd_file_id_connection_id_fkey FOREIGN KEY (snd_file_id, connection_id) REFERENCES test_chat_schema.snd_files(file_id, connection_id) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT connections_via_contact_fkey FOREIGN KEY (via_contact) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contact_profiles
    ADD CONSTRAINT contact_profiles_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_business_group_id_fkey FOREIGN KEY (business_group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_contact_profile_id_fkey FOREIGN KEY (contact_profile_id) REFERENCES test_chat_schema.contact_profiles(contact_profile_id) ON DELETE SET NULL DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_user_contact_link_id_fkey FOREIGN KEY (user_contact_link_id) REFERENCES test_chat_schema.user_contact_links(user_contact_link_id) ON UPDATE CASCADE ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.contact_requests
    ADD CONSTRAINT contact_requests_user_id_local_display_name_fkey FOREIGN KEY (user_id, local_display_name) REFERENCES test_chat_schema.display_names(user_id, local_display_name) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_contact_profile_id_fkey FOREIGN KEY (contact_profile_id) REFERENCES test_chat_schema.contact_profiles(contact_profile_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_contact_request_id_fkey FOREIGN KEY (contact_request_id) REFERENCES test_chat_schema.contact_requests(contact_request_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_grp_direct_inv_from_group_id_fkey FOREIGN KEY (grp_direct_inv_from_group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_grp_direct_inv_from_group_member_id_fkey FOREIGN KEY (grp_direct_inv_from_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_grp_direct_inv_from_member_conn_id_fkey FOREIGN KEY (grp_direct_inv_from_member_conn_id) REFERENCES test_chat_schema.connections(connection_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT contacts_user_id_local_display_name_fkey FOREIGN KEY (user_id, local_display_name) REFERENCES test_chat_schema.display_names(user_id, local_display_name) ON UPDATE CASCADE ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.delivery_jobs
    ADD CONSTRAINT delivery_jobs_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.delivery_jobs
    ADD CONSTRAINT delivery_jobs_job_scope_support_gm_id_fkey FOREIGN KEY (job_scope_support_gm_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.delivery_jobs
    ADD CONSTRAINT delivery_jobs_single_sender_group_member_id_fkey FOREIGN KEY (single_sender_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.delivery_tasks
    ADD CONSTRAINT delivery_tasks_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.delivery_tasks
    ADD CONSTRAINT delivery_tasks_job_scope_support_gm_id_fkey FOREIGN KEY (job_scope_support_gm_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.delivery_tasks
    ADD CONSTRAINT delivery_tasks_message_id_fkey FOREIGN KEY (message_id) REFERENCES test_chat_schema.messages(message_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.delivery_tasks
    ADD CONSTRAINT delivery_tasks_sender_group_member_id_fkey FOREIGN KEY (sender_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.display_names
    ADD CONSTRAINT display_names_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.extra_xftp_file_descriptions
    ADD CONSTRAINT extra_xftp_file_descriptions_file_id_fkey FOREIGN KEY (file_id) REFERENCES test_chat_schema.files(file_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.extra_xftp_file_descriptions
    ADD CONSTRAINT extra_xftp_file_descriptions_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.files
    ADD CONSTRAINT files_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.files
    ADD CONSTRAINT files_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.files
    ADD CONSTRAINT files_redirect_file_id_fkey FOREIGN KEY (redirect_file_id) REFERENCES test_chat_schema.files(file_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.files
    ADD CONSTRAINT files_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.chat_items
    ADD CONSTRAINT fk_chat_items_note_folders FOREIGN KEY (note_folder_id) REFERENCES test_chat_schema.note_folders(note_folder_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT fk_connections_user_contact_links_user_contact_link_id FOREIGN KEY (user_contact_link_id) REFERENCES test_chat_schema.user_contact_links(user_contact_link_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.connections
    ADD CONSTRAINT fk_connections_user_contact_links_via_user_contact_link FOREIGN KEY (via_user_contact_link) REFERENCES test_chat_schema.user_contact_links(user_contact_link_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.contacts
    ADD CONSTRAINT fk_contacts_group_members FOREIGN KEY (contact_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.files
    ADD CONSTRAINT fk_files_chat_items FOREIGN KEY (chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.files
    ADD CONSTRAINT fk_files_note_folders FOREIGN KEY (note_folder_id) REFERENCES test_chat_schema.note_folders(note_folder_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.groups
    ADD CONSTRAINT fk_groups_chat_items FOREIGN KEY (chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.rcv_files
    ADD CONSTRAINT fk_rcv_files_xftp_file_descriptions FOREIGN KEY (file_descr_id) REFERENCES test_chat_schema.xftp_file_descriptions(file_descr_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.snd_files
    ADD CONSTRAINT fk_snd_files_connections FOREIGN KEY (connection_id) REFERENCES test_chat_schema.connections(connection_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.snd_files
    ADD CONSTRAINT fk_snd_files_xftp_file_descriptions FOREIGN KEY (file_descr_id) REFERENCES test_chat_schema.xftp_file_descriptions(file_descr_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.users
    ADD CONSTRAINT fk_users_contacts FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY test_chat_schema.users
    ADD CONSTRAINT fk_users_display_names FOREIGN KEY (user_id, local_display_name) REFERENCES test_chat_schema.display_names(user_id, local_display_name) ON UPDATE CASCADE ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY test_chat_schema.group_member_intros
    ADD CONSTRAINT group_member_intros_re_group_member_id_fkey FOREIGN KEY (re_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_member_intros
    ADD CONSTRAINT group_member_intros_to_group_member_id_fkey FOREIGN KEY (to_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_contact_profile_id_fkey FOREIGN KEY (contact_profile_id) REFERENCES test_chat_schema.contact_profiles(contact_profile_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_invited_by_fkey FOREIGN KEY (invited_by) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_invited_by_group_member_id_fkey FOREIGN KEY (invited_by_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_member_profile_id_fkey FOREIGN KEY (member_profile_id) REFERENCES test_chat_schema.contact_profiles(contact_profile_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_members
    ADD CONSTRAINT group_members_user_id_local_display_name_fkey FOREIGN KEY (user_id, local_display_name) REFERENCES test_chat_schema.display_names(user_id, local_display_name) ON UPDATE CASCADE ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_profiles
    ADD CONSTRAINT group_profiles_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_snd_item_statuses
    ADD CONSTRAINT group_snd_item_statuses_chat_item_id_fkey FOREIGN KEY (chat_item_id) REFERENCES test_chat_schema.chat_items(chat_item_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.group_snd_item_statuses
    ADD CONSTRAINT group_snd_item_statuses_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.groups
    ADD CONSTRAINT groups_group_profile_id_fkey FOREIGN KEY (group_profile_id) REFERENCES test_chat_schema.group_profiles(group_profile_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.groups
    ADD CONSTRAINT groups_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.groups
    ADD CONSTRAINT groups_user_id_local_display_name_fkey FOREIGN KEY (user_id, local_display_name) REFERENCES test_chat_schema.display_names(user_id, local_display_name) ON UPDATE CASCADE ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.known_servers
    ADD CONSTRAINT known_servers_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.messages
    ADD CONSTRAINT messages_author_group_member_id_fkey FOREIGN KEY (author_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.messages
    ADD CONSTRAINT messages_connection_id_fkey FOREIGN KEY (connection_id) REFERENCES test_chat_schema.connections(connection_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.messages
    ADD CONSTRAINT messages_forwarded_by_group_member_id_fkey FOREIGN KEY (forwarded_by_group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.messages
    ADD CONSTRAINT messages_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.msg_deliveries
    ADD CONSTRAINT msg_deliveries_connection_id_fkey FOREIGN KEY (connection_id) REFERENCES test_chat_schema.connections(connection_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.msg_deliveries
    ADD CONSTRAINT msg_deliveries_message_id_fkey FOREIGN KEY (message_id) REFERENCES test_chat_schema.messages(message_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.note_folders
    ADD CONSTRAINT note_folders_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.operator_usage_conditions
    ADD CONSTRAINT operator_usage_conditions_server_operator_id_fkey FOREIGN KEY (server_operator_id) REFERENCES test_chat_schema.server_operators(server_operator_id) ON UPDATE CASCADE ON DELETE SET NULL;



ALTER TABLE ONLY test_chat_schema.pending_group_messages
    ADD CONSTRAINT pending_group_messages_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.pending_group_messages
    ADD CONSTRAINT pending_group_messages_message_id_fkey FOREIGN KEY (message_id) REFERENCES test_chat_schema.messages(message_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.protocol_servers
    ADD CONSTRAINT protocol_servers_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.rcv_file_chunks
    ADD CONSTRAINT rcv_file_chunks_file_id_fkey FOREIGN KEY (file_id) REFERENCES test_chat_schema.rcv_files(file_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.rcv_files
    ADD CONSTRAINT rcv_files_file_id_fkey FOREIGN KEY (file_id) REFERENCES test_chat_schema.files(file_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.rcv_files
    ADD CONSTRAINT rcv_files_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.received_probes
    ADD CONSTRAINT received_probes_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.received_probes
    ADD CONSTRAINT received_probes_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.received_probes
    ADD CONSTRAINT received_probes_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.sent_probe_hashes
    ADD CONSTRAINT sent_probe_hashes_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.sent_probe_hashes
    ADD CONSTRAINT sent_probe_hashes_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.sent_probe_hashes
    ADD CONSTRAINT sent_probe_hashes_sent_probe_id_fkey FOREIGN KEY (sent_probe_id) REFERENCES test_chat_schema.sent_probes(sent_probe_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.sent_probe_hashes
    ADD CONSTRAINT sent_probe_hashes_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.sent_probes
    ADD CONSTRAINT sent_probes_contact_id_fkey FOREIGN KEY (contact_id) REFERENCES test_chat_schema.contacts(contact_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.sent_probes
    ADD CONSTRAINT sent_probes_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.sent_probes
    ADD CONSTRAINT sent_probes_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.settings
    ADD CONSTRAINT settings_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.snd_files
    ADD CONSTRAINT snd_files_file_id_fkey FOREIGN KEY (file_id) REFERENCES test_chat_schema.files(file_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.snd_files
    ADD CONSTRAINT snd_files_group_member_id_fkey FOREIGN KEY (group_member_id) REFERENCES test_chat_schema.group_members(group_member_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.user_contact_links
    ADD CONSTRAINT user_contact_links_group_id_fkey FOREIGN KEY (group_id) REFERENCES test_chat_schema.groups(group_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.user_contact_links
    ADD CONSTRAINT user_contact_links_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



ALTER TABLE ONLY test_chat_schema.xftp_file_descriptions
    ADD CONSTRAINT xftp_file_descriptions_user_id_fkey FOREIGN KEY (user_id) REFERENCES test_chat_schema.users(user_id) ON DELETE CASCADE;



