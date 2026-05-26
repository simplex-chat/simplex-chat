"""Sanity checks on auto-generated wire types — catches generator regressions."""

import typing

from simplex_chat.types import CC, CEvt, CR, T


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
