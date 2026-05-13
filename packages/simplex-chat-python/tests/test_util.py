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
