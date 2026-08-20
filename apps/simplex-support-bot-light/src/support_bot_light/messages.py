"""Every user-visible string, and roster rendering."""

from __future__ import annotations

from collections.abc import Sequence

from .roster import ACTIVE, RosterEntry

ADDED = "You are on the roster. You will be added to new chats."
ALREADY_ACTIVE = "You are already on the roster."
INVITATION_SENT = "Contact request sent. Accept it to join the roster."
STILL_PENDING = (
    "Contact request not accepted yet. If you declined it, leave this group, "
    "join it again with the link, then run /dm."
)
INVITATION_FAILED = "Contact request could not be sent. Run /dm again."
ACCEPTING = "Accepting the connection you started. You will be on the roster shortly."
CONNECTING = "The connection is still completing. You will be on the roster shortly."
CONNECTION_LOST = (
    "The direct connection is gone, so I cannot add you to chats. Open my "
    "profile in this group, connect directly, then run /dm."
)
INVITATION_TEXT = "Accept this contact request to be added to incoming chats. Keep the contact."
NOW_ACTIVE = "Now on the roster: {name}"
REMOVED_FROM_GROUP = "Off the roster: {name} left the roster group."
LEFT = "You are off the roster. Chats you have already joined are unchanged."
NOT_ON_ROSTER = "You are not on the roster."
ROSTER_EMPTY = "The roster is empty."

# Keeps /list under the core's per-message size limit.
MAX_LISTED = 40

# Below the core's maxEncodedMsgLength (Protocol.hs).
MAX_REPLY_BYTES = 12000
TRUNCATED = "\n… truncated"
EMPTY_ROSTER_LOG = "Connected: {customer} → nobody on the roster to add"
NOBODY_NEW_LOG = "Connected: {customer} → everyone on the roster was already in the chat"
COMMAND_FAILED = "The command failed. Try again."
REVOKE_FAILED = "Could not take {name} off the roster — retrying on the next restart."
BUSINESS_FAILED_LOG = "Connected: {customer} → could not set up the chat, nobody added"

HELP = (
    "I add roster members to chats started by anyone who connects to my address.\n\n"
    "/dm — join the roster. Without a direct contact I send a contact request; "
    "you join the roster once you accept it.\n"
    "/list — roster members, and contact requests not yet accepted.\n"
    "/leave — leave the roster. Chats you have already joined are unchanged."
)


def _since(label: str, since: str) -> str:
    """` — since 2026-08-13`, or empty when the entry has no timestamp."""
    day = since[:10]
    return f" — {label} {day}" if day else ""


def _section(title: str, label: str, entries: Sequence[RosterEntry]) -> list[str]:
    """A `/list` section, capped so the whole reply stays sendable.

    A long enough roster would push `/list` past the core's wire limit
    (maxEncodedMsgLength), so it is capped here and what is omitted is stated
    rather than silently dropped.
    """
    lines = [f"{title} ({len(entries)}):"]
    lines += [f"  • {e.name}{_since(label, e.since)}" for e in entries[:MAX_LISTED]]
    if len(entries) > MAX_LISTED:
        lines.append(f"  … and {len(entries) - MAX_LISTED} more")
    return lines


def render_roster(entries: Sequence[RosterEntry]) -> str:
    """Format the roster for `/list`, with a section per state."""
    active = [e for e in entries if e.state == ACTIVE and e.reachable]
    unreachable = [e for e in entries if e.state == ACTIVE and not e.reachable]
    pending = [e for e in entries if e.state != ACTIVE]

    lines: list[str] = []
    if active:
        lines += _section("On the roster", "since", active)
    else:
        lines.append(ROSTER_EMPTY)
    if unreachable:
        lines.append("")
        lines += _section("Not reachable, not being added", "since", unreachable)
    if pending:
        lines.append("")
        lines += _section("Contact request not accepted", "asked", pending)

    return _bounded("\n".join(lines))


def _bounded(out: str) -> str:
    """Keep a message inside what the core will send."""
    encoded = out.encode()
    if len(encoded) > MAX_REPLY_BYTES:
        # A last resort: names are capped in characters, so a section of CJK
        # names can still overrun what the core will send. The suffix is inside
        # the budget, so the result never exceeds MAX_REPLY_BYTES.
        room = MAX_REPLY_BYTES - len(TRUNCATED.encode())
        return encoded[:room].decode(errors="ignore") + TRUNCATED
    return out


def invite_log(customer: str, added: Sequence[str], failed: Sequence[str]) -> str:
    """One line for the roster group recording who was pulled into a business chat."""
    line = (
        f"Connected: {customer} → added {', '.join(added)}"
        if added
        else f"Connected: {customer} → nobody added"
    )
    if failed:
        line += f" (failed: {', '.join(failed)})"
    return _bounded(line)
