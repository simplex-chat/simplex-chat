"""Roster membership, stored in contact custom data."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import UTC, datetime
from typing import Literal

from simplex_chat import ChatApi
from simplex_chat.types import T

from .text import safe_name

NAMESPACE = "supportBotLight"
ACTIVE = "active"
PENDING = "pending"

RosterState = Literal["active", "pending"]

READY_STATUSES = frozenset({"ready", "sndReady"})

# Mirrors isInGroup in apps/simplex-support-bot/src/bot.ts.
TERMINAL_STATUSES = frozenset({"rejected", "removed", "left", "deleted", "unknown"})

# The connection is gone; nothing about it is still in progress.
DEAD_STATUSES = frozenset({"deleted", "failed"})


def in_group(member: T.GroupMember) -> bool:
    return member["memberStatus"] not in TERMINAL_STATUSES


async def contact_ids_in_group(api: ChatApi, group_id: int) -> set[int]:
    """Contact ids of everyone currently in a group.

    api_list_members keeps rows for people who left or were removed, so the
    status filter is what makes this a membership test rather than a history of
    everyone who was ever in the group.
    """
    members = await api.api_list_members(group_id)
    return {cid for m in members if (cid := m.get("memberContactId")) is not None and in_group(m)}


def connecting(contact: T.Contact) -> bool:
    """Whether a connection is still on its way up.

    Between accepting and `ready` the core parks a member contact in `accepted`
    with `contactGroupMemberId` cleared, which is indistinguishable by shape
    from a connection the peer deleted. Only the status separates them.
    """
    status = (contact.get("activeConn") or {}).get("connStatus") or {}
    tag = status.get("type")
    return tag is not None and tag not in DEAD_STATUSES


def awaiting_accept(contact: T.Contact) -> bool:
    """Whether the peer opened a direct connection we have not accepted.

    A member who taps "connect directly" on the bot's profile in the roster
    group produces this: a contact in `prepared` state with no
    `contactGroupMemberId`, otherwise indistinguishable from one the peer
    deleted.
    """
    inv = contact.get("groupDirectInv")
    if inv is not None:
        # The record survives acceptance; only this flag moves, and the core
        # rejects a second accept with "connection already started".
        return not inv.get("groupDirectInvStartedConnection", False)
    status = (contact.get("activeConn") or {}).get("connStatus") or {}
    return status.get("type") == "prepared"


def accept_started(contact: T.Contact) -> bool:
    """Whether we accepted and the connection is still completing.

    UPSTREAM BUG: `groupDirectInv` outlives the connection it describes. Nothing
    clears the record when that connection dies, so the started flag alone
    reports progress on a contact the peer deleted long ago.

    Workaround: the connection status decides, and the flag only distinguishes
    accepted from not yet accepted.
    """
    inv = contact.get("groupDirectInv")
    if inv is None or not inv.get("groupDirectInvStartedConnection", False):
        return False
    status = (contact.get("activeConn") or {}).get("connStatus") or {}
    return status.get("type") not in DEAD_STATUSES


def contact_usable(contact: T.Contact) -> bool:
    """Whether the bot can actually add this contact to a group.

    `api_create_member_contact` sets the member's contact id before the person
    has accepted anything, so the contact merely existing proves nothing — only
    a connected connection does.
    """
    status = (contact.get("activeConn") or {}).get("connStatus") or {}
    return status.get("type") in READY_STATUSES


@dataclass(frozen=True, slots=True)
class RosterEntry:
    """One person on the roster, as recorded in their contact's custom data."""

    contact_id: int
    name: str
    state: RosterState
    since: str
    reachable: bool


def utc_now() -> str:
    """Current UTC time as an ISO-8601 string, second precision."""
    return datetime.now(UTC).isoformat(timespec="seconds")


def contact_name(contact: T.Contact) -> str:
    """A contact's display name, sanitised for rendering."""
    return safe_name(
        contact.get("localDisplayName") or (contact.get("profile") or {}).get("displayName") or ""
    )


def entry_of(contact: T.Contact) -> RosterEntry | None:
    """The roster entry for a contact, or None if it carries no roster mark."""
    mark = (contact.get("customData") or {}).get(NAMESPACE)
    if not isinstance(mark, dict):
        return None
    state = mark.get("roster")
    if state != ACTIVE and state != PENDING:
        return None
    return RosterEntry(
        contact_id=contact["contactId"],
        name=contact_name(contact),
        state=state,
        since=str(mark.get("since", "")),
        reachable=contact_usable(contact),
    )


async def mark(api: ChatApi, contact: T.Contact, state: RosterState, since: str) -> None:
    """Write the roster mark, preserving any other keys in the blob.

    `api_set_contact_custom_data` replaces the whole column, so this is
    read-modify-write even though this bot is the only expected writer.
    """
    data = dict(contact.get("customData") or {})
    data[NAMESPACE] = {"roster": state, "since": since}
    await api.api_set_contact_custom_data(contact["contactId"], data)


async def unmark(api: ChatApi, contact: T.Contact) -> None:
    """Remove the roster mark, leaving any other keys and the contact intact."""
    data = dict(contact.get("customData") or {})
    if NAMESPACE not in data:
        return
    del data[NAMESPACE]
    await api.api_set_contact_custom_data(contact["contactId"], data or None)


async def load(api: ChatApi, user_id: int) -> list[RosterEntry]:
    """Every marked contact, sorted by display name."""
    contacts = await api.api_list_contacts(user_id)
    entries = [e for c in contacts if (e := entry_of(c)) is not None]
    return sorted(entries, key=lambda e: e.name.lower())


async def active(api: ChatApi, user_id: int) -> list[RosterEntry]:
    """Marked active and still reachable — the ones added to business chats.

    A contact marked active can stop being usable later, for instance when the
    person deletes the bot. `api_add_member` always fails for such a contact, so
    it is excluded here rather than failing once per business chat forever.
    `/list` reports the same distinction under "Not reachable".
    """
    return [e for e in await load(api, user_id) if e.state == ACTIVE and e.reachable]


async def find_contact(api: ChatApi, user_id: int, contact_id: int) -> T.Contact | None:
    """The contact with this id, or None if it no longer exists."""
    for c in await api.api_list_contacts(user_id):
        if c["contactId"] == contact_id:
            return c
    return None
