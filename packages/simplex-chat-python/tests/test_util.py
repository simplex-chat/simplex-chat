import pytest

from simplex_chat import util


def test_chat_info_ref_direct():
    ci = {"type": "direct", "contact": {"contactId": 7}}
    assert util.chat_info_ref(ci) == {"chatType": "direct", "chatId": 7}


def test_chat_info_ref_group():
    ci = {"type": "group", "groupInfo": {"groupId": 42}}
    assert util.chat_info_ref(ci) == {"chatType": "group", "chatId": 42}


def test_chat_info_ref_group_with_member_support_scope():
    ci = {
        "type": "group",
        "groupInfo": {"groupId": 42},
        "groupChatScope": {"type": "memberSupport", "groupMember_": {"groupMemberId": 99}},
    }
    ref = util.chat_info_ref(ci)
    assert ref == {
        "chatType": "group",
        "chatId": 42,
        "chatScope": {"type": "memberSupport", "groupMemberId_": 99},
    }


def test_chat_info_ref_group_with_member_support_scope_no_member():
    ci = {
        "type": "group",
        "groupInfo": {"groupId": 42},
        "groupChatScope": {"type": "memberSupport"},
    }
    ref = util.chat_info_ref(ci)
    # No groupMember_ → no groupMemberId_ in the wire scope.
    assert ref == {
        "chatType": "group",
        "chatId": 42,
        "chatScope": {"type": "memberSupport"},
    }


def test_chat_info_ref_returns_none_for_non_targets():
    assert util.chat_info_ref({"type": "contactRequest"}) is None
    assert util.chat_info_ref({"type": "contactConnection"}) is None


def test_chat_info_name_direct():
    ci = {"type": "direct", "contact": {"profile": {"displayName": "Alice"}}}
    assert util.chat_info_name(ci) == "@Alice"


def test_chat_info_name_group():
    ci = {"type": "group", "groupInfo": {"groupProfile": {"displayName": "MyGroup"}}}
    assert util.chat_info_name(ci) == "#MyGroup"


def test_chat_info_name_group_with_member_support():
    ci = {
        "type": "group",
        "groupInfo": {"groupProfile": {"displayName": "MyGroup"}},
        "groupChatScope": {
            "type": "memberSupport",
            "groupMember_": {"memberProfile": {"displayName": "Carol"}},
        },
    }
    assert util.chat_info_name(ci) == "#MyGroup(support Carol)"


def test_chat_info_name_local():
    assert util.chat_info_name({"type": "local"}) == "private notes"


def test_chat_info_name_contact_request():
    ci = {"type": "contactRequest", "contactRequest": {"profile": {"displayName": "Eve"}}}
    assert util.chat_info_name(ci) == "request from @Eve"


def test_chat_info_name_contact_connection():
    assert util.chat_info_name({"type": "contactConnection", "contactConnection": {}}) == (
        "pending connection"
    )
    assert (
        util.chat_info_name({"type": "contactConnection", "contactConnection": {"localAlias": "X"}})
        == "pending connection (X)"
    )


def test_sender_name_direct_uses_chat_name():
    ci = {"type": "direct", "contact": {"profile": {"displayName": "Alice"}}}
    chat_dir = {"type": "directRcv"}
    assert util.sender_name(ci, chat_dir) == "@Alice"


def test_sender_name_group_appends_member():
    ci = {"type": "group", "groupInfo": {"groupProfile": {"displayName": "MyGroup"}}}
    chat_dir = {"type": "groupRcv", "groupMember": {"memberProfile": {"displayName": "Bob"}}}
    assert util.sender_name(ci, chat_dir) == "#MyGroup @Bob"


def test_contact_address_str_prefers_short():
    assert util.contact_address_str({"connFullLink": "full", "connShortLink": "short"}) == "short"


def test_contact_address_str_falls_back_to_full():
    assert util.contact_address_str({"connFullLink": "full"}) == "full"


def test_from_local_profile_strips_extras_and_undefined():
    local = {
        "displayName": "x",
        "fullName": "X Y",
        "shortDescr": None,
        "image": "data:image/png;base64,...",
        "contactLink": None,
        "preferences": {},
        "peerType": "bot",
        "profileId": 99,  # extra LocalProfile field
        "localAlias": "alias",  # extra LocalProfile field
    }
    p = util.from_local_profile(local)
    assert p == {
        "displayName": "x",
        "fullName": "X Y",
        "image": "data:image/png;base64,...",
        "preferences": {},
        "peerType": "bot",
    }


def test_ci_content_text_rcv():
    ci = {"content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "hello"}}}
    assert util.ci_content_text(ci) == "hello"


def test_ci_content_text_snd():
    ci = {"content": {"type": "sndMsgContent", "msgContent": {"type": "text", "text": "world"}}}
    assert util.ci_content_text(ci) == "world"


def test_ci_content_text_other():
    ci = {"content": {"type": "rcvGroupEvent"}}
    assert util.ci_content_text(ci) is None


def test_ci_bot_command_match():
    ci = {"content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "/ping"}}}
    assert util.ci_bot_command(ci) == ("ping", "")


def test_ci_bot_command_with_args():
    ci = {
        "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "/echo  hi  "}}
    }
    assert util.ci_bot_command(ci) == ("echo", "hi")


def test_ci_bot_command_not_a_command():
    ci = {"content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "hello"}}}
    assert util.ci_bot_command(ci) is None


def test_ci_bot_command_no_text():
    ci = {"content": {"type": "rcvGroupEvent"}}
    assert util.ci_bot_command(ci) is None


def test_reaction_text_emoji():
    r = {"chatReaction": {"reaction": {"type": "emoji", "emoji": "🎉"}}}
    assert util.reaction_text(r) == "🎉"


def test_reaction_text_tag():
    r = {"chatReaction": {"reaction": {"type": "unknown", "tag": "thumbs_up"}}}
    assert util.reaction_text(r) == "thumbs_up"


def test_merged_custom_data_adds_a_key_keeping_the_others():
    data = {"other": {"kept": True}}
    assert util.merged_custom_data(data, "mine", {"roster": "active"}) == {
        "other": {"kept": True},
        "mine": {"roster": "active"},
    }


def test_merged_custom_data_does_not_mutate_the_original():
    data = {"other": 1}
    util.merged_custom_data(data, "mine", 2)
    assert data == {"other": 1}


def test_merged_custom_data_replaces_an_existing_key():
    assert util.merged_custom_data({"mine": "old"}, "mine", "new") == {"mine": "new"}


def test_merged_custom_data_on_an_empty_column():
    assert util.merged_custom_data(None, "mine", 1) == {"mine": 1}


def test_merged_custom_data_removes_a_key():
    assert util.merged_custom_data({"mine": 1, "other": 2}, "mine", None) == {"other": 2}


def test_merged_custom_data_clears_the_column_when_nothing_is_left():
    # None is what the set commands read as "clear"; {} would be a wasted write
    # of an empty object.
    assert util.merged_custom_data({"mine": 1}, "mine", None) is None


def test_merged_custom_data_removing_a_key_that_is_not_there():
    assert util.merged_custom_data({"other": 2}, "mine", None) == {"other": 2}


def test_conn_status_reads_the_tag():
    contact = {"activeConn": {"connStatus": {"type": "ready"}}}
    assert util.conn_status(contact) == "ready"


def test_conn_status_without_a_connection():
    # api_create_member_contact produces exactly this: a contact row before
    # any connection exists.
    assert util.conn_status({"contactId": 3}) is None


def test_conn_status_with_a_null_connection():
    assert util.conn_status({"activeConn": None}) is None


def test_check_profile_image_accepts_what_the_apps_decode():
    png = "data:image/png;base64,AAA"
    jpg = "data:image/jpg;base64,AAA"
    assert util.check_profile_image(png) == png
    assert util.check_profile_image(jpg) == jpg


def test_check_profile_image_rejects_another_media_type():
    # image/jpeg is the easy mistake: the file extension is .jpeg, and the
    # core stores it, but no client strips that prefix before decoding.
    with pytest.raises(ValueError, match="must start with"):
        util.check_profile_image("data:image/jpeg;base64,AAA")


def test_check_profile_image_rejects_a_remote_url():
    with pytest.raises(ValueError, match="must start with"):
        util.check_profile_image("https://simplex.chat/logo.png")
