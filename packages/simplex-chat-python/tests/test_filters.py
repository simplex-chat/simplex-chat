import re

from simplex_chat.filters import compile_message_filter


def _msg(content_type="text", text=None, chat_type="direct", group_id=None):
    """Build a minimal mock Message-like object for filter testing."""

    class M:
        pass

    m = M()
    m.content = {"type": content_type, "text": text} if text is not None else {"type": content_type}
    m.chat_item = {
        "chatInfo": {
            "type": chat_type,
            **({"groupInfo": {"groupId": group_id}} if chat_type == "group" else {}),
        }
    }
    return m


def test_no_filters_matches_all():
    f = compile_message_filter({})
    assert f(_msg(content_type="text"))
    assert f(_msg(content_type="image"))


def test_content_type_singular():
    f = compile_message_filter({"content_type": "text"})
    assert f(_msg(content_type="text"))
    assert not f(_msg(content_type="image"))


def test_content_type_tuple_or():
    f = compile_message_filter({"content_type": ("text", "image")})
    assert f(_msg(content_type="text"))
    assert f(_msg(content_type="image"))
    assert not f(_msg(content_type="voice"))


def test_text_exact():
    f = compile_message_filter({"text": "hello"})
    assert f(_msg(text="hello"))
    assert not f(_msg(text="world"))


def test_text_regex():
    f = compile_message_filter({"text": re.compile(r"^\d+$")})
    assert f(_msg(text="123"))
    assert not f(_msg(text="abc"))


def test_when_callable():
    f = compile_message_filter({"when": lambda m: m.content["type"] == "voice"})
    assert f(_msg(content_type="voice"))
    assert not f(_msg(content_type="text"))


def test_combined_and():
    f = compile_message_filter({"content_type": "text", "text": re.compile(r"\d")})
    assert f(_msg(content_type="text", text="abc123"))
    assert not f(_msg(content_type="text", text="abc"))
    assert not f(_msg(content_type="image"))


def test_chat_type_filter():
    f = compile_message_filter({"chat_type": "group"})
    assert f(_msg(chat_type="group", group_id=1))
    assert not f(_msg(chat_type="direct"))


def test_group_id_filter():
    f = compile_message_filter({"group_id": 42})
    assert f(_msg(chat_type="group", group_id=42))
    assert not f(_msg(chat_type="group", group_id=99))
    assert not f(_msg(chat_type="direct"))


def test_group_id_tuple_or():
    f = compile_message_filter({"group_id": (1, 2, 3)})
    assert f(_msg(chat_type="group", group_id=2))
    assert not f(_msg(chat_type="group", group_id=99))
