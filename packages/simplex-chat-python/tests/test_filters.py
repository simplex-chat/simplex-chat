import re

from simplex_chat.filters import compile_message_filter


def _msg(content_type="text", text=None, chat_type="direct", group_id=None, contact_id=None):
    """Build a minimal mock Message-like object for filter testing."""

    class M:
        pass

    m = M()
    m.content = {"type": content_type, "text": text} if text is not None else {"type": content_type}
    chat_info: dict = {"type": chat_type}
    if chat_type == "group":
        chat_info["groupInfo"] = {"groupId": group_id}
    elif chat_type == "direct" and contact_id is not None:
        chat_info["contact"] = {"contactId": contact_id}
    m.chat_item = {"chatInfo": chat_info}
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


def test_contact_id_filter():
    f = compile_message_filter({"contact_id": 7})
    assert f(_msg(chat_type="direct", contact_id=7))
    assert not f(_msg(chat_type="direct", contact_id=99))
    assert not f(_msg(chat_type="group", group_id=7))


def test_contact_id_tuple_or():
    f = compile_message_filter({"contact_id": (1, 2, 3)})
    assert f(_msg(chat_type="direct", contact_id=2))
    assert not f(_msg(chat_type="direct", contact_id=99))


def test_contact_id_combined_with_content_type():
    f = compile_message_filter({"content_type": "text", "contact_id": 5})
    assert f(_msg(content_type="text", chat_type="direct", contact_id=5))
    assert not f(_msg(content_type="image", chat_type="direct", contact_id=5))
    assert not f(_msg(content_type="text", chat_type="direct", contact_id=99))
