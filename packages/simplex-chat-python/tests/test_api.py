"""ChatApi commands and error classification, without the native controller.

`ChatApi` only touches the FFI through `send_chat_cmd`, so replacing that one
method exercises every wrapper: the command string it builds and the response
shape it accepts.
"""

from __future__ import annotations

from typing import Any

import pytest

from simplex_chat import ChatApi, ChatAPIError, ChatCommandError, ChatError


class FakeCtrl(ChatApi):
    """ChatApi with the FFI call replaced by a scripted response."""

    def __init__(self, response: Any = None, raises: Exception | None = None) -> None:
        super().__init__(ctrl=1)
        self.response = response
        self.raises = raises
        self.sent: list[str] = []

    async def send_chat_cmd(self, cmd: str) -> Any:
        self.sent.append(cmd)
        if self.raises is not None:
            raise self.raises
        return self.response


# ---------------------------------------------------------------------- #
# Error hierarchy
# ---------------------------------------------------------------------- #


def test_both_command_failures_share_one_base():
    # The two are raised from different layers for the same kind of failure;
    # callers should not have to name both.
    assert issubclass(ChatAPIError, ChatError)
    assert issubclass(ChatCommandError, ChatError)


def test_store_error_type_reads_the_nested_tag():
    e = ChatAPIError("x", {"type": "errorStore", "storeError": {"type": "duplicateName"}})
    assert e.store_error_type == "duplicateName"
    assert e.error_type is None


def test_error_type_reads_the_nested_tag():
    e = ChatAPIError("x", {"type": "error", "errorType": {"type": "noActiveUser"}})
    assert e.error_type == "noActiveUser"
    assert e.store_error_type is None


def test_command_error_carries_the_message():
    # The tag is always "commandError"; the message is the whole content.
    e = ChatAPIError(
        "x", {"type": "error", "errorType": {"type": "commandError", "message": "name too long"}}
    )
    assert e.command_error == "name too long"


def test_command_error_of_another_failure():
    e = ChatAPIError("x", {"type": "errorStore", "storeError": {"type": "duplicateName"}})
    assert e.command_error is None


def test_error_tags_of_an_unrelated_error():
    e = ChatAPIError("x", {"type": "errorAgent", "agentError": {"type": "CRITICAL"}})
    assert e.error_type is None
    assert e.store_error_type is None


def test_error_tags_without_a_chat_error():
    # Raised when the controller returns something that is not valid JSON-RPC.
    e = ChatAPIError("invalid chat command result")
    assert e.error_type is None
    assert e.store_error_type is None


# ---------------------------------------------------------------------- #
# Errors surfaced as absence
# ---------------------------------------------------------------------- #


async def test_missing_address_reads_as_none():
    api = FakeCtrl(
        raises=ChatAPIError(
            "x", {"type": "errorStore", "storeError": {"type": "userContactLinkNotFound"}}
        )
    )
    assert await api.api_get_user_address(1) is None


async def test_another_store_error_still_raises():
    api = FakeCtrl(
        raises=ChatAPIError("x", {"type": "errorStore", "storeError": {"type": "dBBusyError"}})
    )
    with pytest.raises(ChatAPIError):
        await api.api_get_user_address(1)


async def test_no_active_user_reads_as_none():
    api = FakeCtrl(
        raises=ChatAPIError("x", {"type": "error", "errorType": {"type": "noActiveUser"}})
    )
    assert await api.api_get_active_user() is None


async def test_another_error_from_the_user_query_still_raises():
    api = FakeCtrl(
        raises=ChatAPIError("x", {"type": "error", "errorType": {"type": "invalidConnReq"}})
    )
    with pytest.raises(ChatAPIError):
        await api.api_get_active_user()


# ---------------------------------------------------------------------- #
# Member contacts
# ---------------------------------------------------------------------- #


async def test_accept_member_contact():
    contact = {"contactId": 7}
    api = FakeCtrl({"type": "memberContactAccepted", "contact": contact})
    assert await api.api_accept_member_contact(7) is contact
    assert api.sent == ["/_accept member contact @7"]


async def test_accept_member_contact_rejected():
    # The core answers a second accept with a command error, not a contact.
    api = FakeCtrl({"type": "chatCmdError"})
    with pytest.raises(ChatCommandError):
        await api.api_accept_member_contact(7)


# ---------------------------------------------------------------------- #
# Custom data
# ---------------------------------------------------------------------- #


async def test_merge_contact_custom_data_keeps_other_keys():
    api = FakeCtrl({"type": "cmdOk"})
    contact = {"contactId": 4, "customData": {"other": 1}}
    await api.api_merge_contact_custom_data(contact, "mine", {"roster": "active"})
    assert api.sent == ['/_set custom @4 {"other": 1, "mine": {"roster": "active"}}']


async def test_merge_contact_custom_data_removing_the_last_key_clears_the_column():
    api = FakeCtrl({"type": "cmdOk"})
    contact = {"contactId": 4, "customData": {"mine": 1}}
    await api.api_merge_contact_custom_data(contact, "mine", None)
    assert api.sent == ["/_set custom @4"]


async def test_merge_group_custom_data_keeps_other_keys():
    api = FakeCtrl({"type": "cmdOk"})
    group = {"groupId": 9, "customData": {"other": 1}}
    await api.api_merge_group_custom_data(group, "mine", {"rostered": True})
    assert api.sent == ['/_set custom #9 {"other": 1, "mine": {"rostered": true}}']


async def test_merge_group_custom_data_on_a_group_with_no_custom_data():
    api = FakeCtrl({"type": "cmdOk"})
    await api.api_merge_group_custom_data({"groupId": 9}, "mine", 1)
    assert api.sent == ['/_set custom #9 {"mine": 1}']


async def test_a_failed_custom_data_write_raises():
    api = FakeCtrl({"type": "chatCmdError"})
    with pytest.raises(ChatCommandError):
        await api.api_merge_group_custom_data({"groupId": 9}, "mine", 1)
