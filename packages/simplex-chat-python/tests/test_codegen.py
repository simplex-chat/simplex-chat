"""Sanity checks on auto-generated wire types — catches generator regressions."""

import typing

from simplex_chat.types import CC, CR, CEvt, T


def test_types_module_imports():
    """Every generated module imports cleanly with no SyntaxError."""
    assert T is not None and CC is not None and CR is not None and CEvt is not None


def test_chat_type_is_literal_enum():
    """ChatType should be a Literal of expected member set."""
    args = typing.get_args(T.ChatType)
    assert "direct" in args
    assert "group" in args
    assert "local" in args


def test_known_command_has_cmd_string():
    s = CC.APICreateMyAddress_cmd_string({"userId": 1})
    assert s == "/_address 1"


def test_chat_response_tag_alias_present():
    """ChatResponse_Tag union of literals exists."""
    assert hasattr(CR, "ChatResponse_Tag")


def test_chat_event_tag_alias_present():
    """ChatEvent_Tag exists; covers the on_event Literal annotation."""
    assert hasattr(CEvt, "ChatEvent_Tag")
    args = typing.get_args(CEvt.ChatEvent_Tag)
    assert "newChatItems" in args


def test_chat_ref_cmd_string_direct():
    """Sanity check the codegen fix for ChatRef-bearing commands."""
    assert T.ChatRef_cmd_string({"chatType": "direct", "chatId": 7}) == "@7"
    assert T.ChatRef_cmd_string({"chatType": "group", "chatId": 42}) == "#42"


def test_api_connect_cmd_string_renders_incognito():
    link = {"connFullLink": "L"}
    assert CC.APIConnect_cmd_string({"userId": 1, "incognito": True, "preparedLink_": link}) == "/_connect 1 incognito=on L"
    assert CC.APIConnect_cmd_string({"userId": 1, "incognito": False, "preparedLink_": link}) == "/_connect 1 L"


def test_chat_pagination_cmd_string():
    assert T.ChatPagination_cmd_string({"type": "last", "count": 0}) == "count=0"
    assert T.ChatPagination_cmd_string({"type": "initial", "count": 1}) == "initial=1"
    assert (
        T.ChatPagination_cmd_string({"type": "around", "chatItemId": 5, "count": 0})
        == "around=5 count=0"
    )
    assert (
        T.ChatPagination_cmd_string({"type": "after", "chatItemId": 5, "count": 100})
        == "after=5 count=100"
    )
    assert (
        T.ChatPagination_cmd_string({"type": "before", "chatItemId": 5, "count": 10})
        == "before=5 count=10"
    )


def test_api_get_chat_cmd_string():
    ref = {"chatType": "direct", "chatId": 2}
    last = {"type": "last", "count": 10}
    assert (
        CC.APIGetChat_cmd_string(
            {"chatRef": ref, "contentTag": "image", "chatPagination": last, "search": "k"}
        )
        == "/_get chat @2 content=image count=10 search=k"
    )
    assert (
        CC.APIGetChat_cmd_string(
            {"chatRef": ref, "chatPagination": {"type": "initial", "count": 1}}
        )
        == "/_get chat @2 initial=1"
    )


def test_api_chat_items_read_cmd_string():
    ref = {"chatType": "direct", "chatId": 2}
    assert (
        CC.APIChatItemsRead_cmd_string({"chatRef": ref, "chatItemIds": [3, 4]})
        == "/_read chat items @2 3,4"
    )


def test_api_connect_plan_cmd_string_omits_default_resolve_mode():
    assert T.PlanResolveMode_cmd_string("unknown") == ""
    assert T.PlanResolveMode_cmd_string("allGroups") == " resolve=allGroups"
    assert T.PlanResolveMode_cmd_string("never") == " resolve=never"
    assert (
        CC.APIConnectPlan_cmd_string({"userId": 1, "connectTarget": "L", "resolveMode": "unknown"})
        == "/_connect plan 1 L"
    )
    assert (
        CC.APIConnectPlan_cmd_string({"userId": 1, "connectTarget": "L", "resolveMode": "never"})
        == "/_connect plan 1 L resolve=never"
    )


def test_api_connect_prepared_group_cmd_string():
    owner = {"contactId": 1, "memberId": "abc"}
    msg = {"type": "text", "text": "hi"}
    assert T.GroupOwnerContact_cmd_string(owner) == "contact=1 owner=abc"
    assert (
        CC.APIConnectPreparedGroup_cmd_string(
            {"groupId": 3, "incognito": False, "ownerContact": owner}
        )
        == "/_connect group #3 contact=1 owner=abc"
    )
    assert (
        CC.APIConnectPreparedGroup_cmd_string({"groupId": 3, "incognito": True, "msgContent_": msg})
        == '/_connect group #3 incognito=on json {"type": "text", "text": "hi"}'
    )


def test_member_contact_cmd_string():
    assert (
        CC.APICreateMemberContact_cmd_string({"groupId": 1, "groupMemberId": 2})
        == "/_create member contact #1 2"
    )
    assert (
        CC.APISendMemberContactInvitation_cmd_string({"contactId": 2})
        == "/_invite member contact @2"
    )


def test_api_get_network_config_cmd_string():
    assert CC.APIGetNetworkConfig_cmd_string({}) == "/network"
