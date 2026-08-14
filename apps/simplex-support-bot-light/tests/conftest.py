"""Fake ChatApi and wire-object factories. No libsimplex, no I/O."""

from __future__ import annotations

import copy
from types import SimpleNamespace
from typing import Any

import pytest
from simplex_chat import Message, util
from simplex_chat.core import ChatAPIError

USER_ID = 1
ROSTER_GROUP_ID = 10


class FakeChatApi:
    """Records calls; returns canned wire dicts.

    `fail_on` is a set of method names that raise `ChatAPIError` when called,
    used to drive the partial-failure paths.
    """

    def __init__(self, contacts: list[dict] | None = None, fail_on: set[str] | None = None):
        self.contacts = contacts or []
        self.groups: list[dict] = []
        self.members: dict[int, list[dict]] = {}
        self.fail_on = fail_on or set()
        self.custom_data: list[tuple[int, dict | None]] = []
        self.group_custom_data: list[tuple[int, dict | None]] = []
        self.replies: list[str] = []
        self.sent: list[tuple[Any, str]] = []
        self.added: list[tuple[int, int, str]] = []
        self.created_member_contacts: list[tuple[int, int]] = []
        self.invitations: list[tuple[int, str]] = []
        self.new_groups: list[dict] = []
        self.profile_updates: list[tuple[int, dict]] = []
        self.links: list[int] = []
        self.group_links: dict[int, str] = {}
        self._next_contact_id = 100
        self._next_item_id = 1000
        self._member_contacts_created: set[tuple[int, int]] = set()
        self.accepted_member_contacts: list[int] = []

    def _check(self, name: str) -> None:
        if name in self.fail_on:
            raise ChatAPIError(f"fake failure in {name}", {"type": "chatCmdError"})

    async def api_list_contacts(self, user_id: int) -> list[dict]:
        self._check("api_list_contacts")
        # Copies, like the core: a caller holding a contact does not see a later
        # write to it, so stale reads show up in tests instead of in production.
        return copy.deepcopy(self.contacts)

    async def api_set_contact_custom_data(self, contact_id: int, custom_data=None) -> None:
        self._check("api_set_contact_custom_data")
        self.custom_data.append((contact_id, custom_data))
        for c in self.contacts:
            if c["contactId"] == contact_id:
                if custom_data is None:
                    c.pop("customData", None)
                else:
                    c["customData"] = custom_data

    async def api_merge_contact_custom_data(self, contact: dict, key: str, value) -> None:
        # Mirrors ChatApi: the column is replaced wholesale, so a merge is a
        # read-modify-write through the same set command.
        await self.api_set_contact_custom_data(
            contact["contactId"], util.merged_custom_data(contact.get("customData"), key, value)
        )

    async def api_merge_group_custom_data(self, group: dict, key: str, value) -> None:
        await self.api_set_group_custom_data(
            group["groupId"], util.merged_custom_data(group.get("customData"), key, value)
        )

    async def api_create_member_contact(self, group_id: int, group_member_id: int) -> dict:
        self._check("api_create_member_contact")
        key = (group_id, group_member_id)
        if key in self._member_contacts_created:
            raise ChatAPIError("member contact already exists", {"type": "chatCmdError"})
        self._member_contacts_created.add(key)
        self.created_member_contacts.append((group_id, group_member_id))
        contact = make_contact(self._next_contact_id, f"member{group_member_id}")
        self._next_contact_id += 1
        self.contacts.append(contact)
        return contact

    async def api_send_member_contact_invitation(self, contact_id: int, message=None) -> dict:
        self._check("api_send_member_contact_invitation")
        for c in self.contacts:
            if c["contactId"] == contact_id and c.get("contactGrpInvSent"):
                raise ChatAPIError("x.grp.direct.inv already sent", {"type": "chatCmdError"})
        self.invitations.append((contact_id, message))
        for c in self.contacts:
            if c["contactId"] == contact_id:
                c["contactGrpInvSent"] = True
        return make_contact(contact_id, "invited", grp_inv_sent=True)

    async def api_accept_member_contact(self, contact_id: int) -> dict:
        self._check("api_accept_member_contact")
        self.accepted_member_contacts.append(contact_id)
        for c in self.contacts:
            if c["contactId"] == contact_id:
                c.setdefault("groupDirectInv", {})["groupDirectInvStartedConnection"] = True
                return c
        return make_contact(contact_id, "accepted")

    async def api_list_members(self, group_id: int) -> list[dict]:
        self._check("api_list_members")
        return list(self.members.get(group_id, []))

    async def api_add_member(self, group_id: int, contact_id: int, member_role: str) -> dict:
        self._check("api_add_member")
        self.added.append((group_id, contact_id, member_role))
        # Distinct id spaces; keep them apart so a mix-up shows up.
        return make_member(group_member_id=contact_id + 1000, contact_id=contact_id)

    async def api_send_text_message(self, chat, text: str, in_reply_to=None) -> list:
        self._check("api_send_text_message")
        self.sent.append((chat, text))
        return []

    async def api_send_text_reply(self, chat_item, text: str) -> list:
        self._check("api_send_text_reply")
        self.replies.append(text)
        # Message.reply indexes items[0], so this cannot return [].
        self._next_item_id += 1
        sent_item = {
            "chatInfo": chat_item["chatInfo"],
            "chatItem": {
                "chatDir": {"type": "direct"},
                "meta": {"itemId": self._next_item_id},
                "content": {"type": "sndMsgContent", "msgContent": {"type": "text", "text": text}},
            },
        }
        return [sent_item]

    async def api_list_groups(self, user_id: int, contact_id=None, search=None) -> list[dict]:
        self._check("api_list_groups")
        return list(self.groups)

    async def api_new_group(self, user_id: int, group_profile: dict) -> dict:
        self._check("api_new_group")
        self.new_groups.append(group_profile)
        group = make_group(ROSTER_GROUP_ID, group_profile)
        self.groups.append(group)
        return group

    async def api_set_group_custom_data(self, group_id: int, custom_data=None) -> None:
        self._check("api_set_group_custom_data")
        self.group_custom_data.append((group_id, custom_data))
        for g in self.groups:
            if g["groupId"] == group_id:
                g["customData"] = custom_data

    async def api_update_group_profile(self, group_id: int, group_profile: dict) -> dict:
        self._check("api_update_group_profile")
        self.profile_updates.append((group_id, group_profile))
        return make_group(group_id, group_profile)

    async def api_create_group_link(self, group_id: int, member_role: str) -> str:
        self._check("api_create_group_link")
        self.links.append(group_id)
        link = f"https://simplex.chat/contact#/?v=2&group={group_id}"
        self.group_links[group_id] = link
        return link

    async def api_get_group_link_str(self, group_id: int) -> str:
        self._check("api_get_group_link_str")
        try:
            return self.group_links[group_id]
        except KeyError:
            raise ChatAPIError("no group link", {"type": "chatCmdError"}) from None


def make_contact(
    contact_id: int,
    name: str,
    custom_data: dict | None = None,
    connected: bool = False,
    grp_inv_sent: bool = False,
    grp_member_id: int | None = -1,
    conn_status: str | None = None,
) -> dict:
    contact: dict = {
        "contactId": contact_id,
        "localDisplayName": name,
        "profile": {"profileId": contact_id, "displayName": name, "fullName": ""},
        "contactGrpInvSent": grp_inv_sent,
    }
    if custom_data is not None:
        contact["customData"] = custom_data
    if conn_status is not None:
        contact["activeConn"] = {"connStatus": {"type": conn_status}}
    elif connected:
        contact["activeConn"] = {"connStatus": {"type": "ready"}}
    # The core sets contactGroupMemberId when a member contact is created and
    # clears it once that contact connects (resetMemberContactFields). -1 means
    # "use whichever of those matches `connected`".
    if grp_member_id == -1:
        grp_member_id = None if connected else contact_id
    if grp_member_id is not None:
        contact["contactGroupMemberId"] = grp_member_id
    return contact


def make_member(
    group_member_id: int = 1,
    contact_id: int | None = None,
    name: str = "someone",
    status: str = "complete",
) -> dict:
    member: dict = {
        "groupMemberId": group_member_id,
        "localDisplayName": name,
        "memberProfile": {"displayName": name, "fullName": ""},
        "memberStatus": status,
    }
    if contact_id is not None:
        member["memberContactId"] = contact_id
    return member


def make_group(
    group_id: int,
    profile: dict,
    custom_data: dict | None = None,
    membership_status: str = "creator",
) -> dict:
    # The core always sends membership; discovery reads it to skip groups the
    # bot has left.
    group: dict = {
        "groupId": group_id,
        "groupProfile": profile,
        "localDisplayName": "g",
        "membership": make_member(1, name="bot", status=membership_status),
    }
    if custom_data is not None:
        group["customData"] = custom_data
    return group


def join_roster_group(api: FakeChatApi) -> None:
    """Put every contact in the roster group.

    Being on the roster means being in that group; the bot re-checks it before
    adding anyone to a customer's chat, so tests have to model it.
    """
    api.members[ROSTER_GROUP_ID] = [
        make_member(1000 + c["contactId"], contact_id=c["contactId"], name=c["localDisplayName"])
        for c in api.contacts
    ]


def make_group_message(api: FakeChatApi, member: dict, text: str, group_id: int = ROSTER_GROUP_ID):
    """A `Message` as delivered from a group, wired to the fake api."""
    chat_item = {
        "chatInfo": {"type": "group", "groupInfo": make_group(group_id, {"displayName": "r"})},
        "chatItem": {
            "chatDir": {"type": "groupRcv", "groupMember": member},
            "meta": {"itemId": 1},
            "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": text}},
        },
    }
    return Message(
        chat_item=chat_item,
        content={"type": "text", "text": text},
        client=SimpleNamespace(api=api),
    )


@pytest.fixture
def api() -> FakeChatApi:
    return FakeChatApi()
