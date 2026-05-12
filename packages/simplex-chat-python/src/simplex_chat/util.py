"""Reusable helpers for working with chat events, types, and message content.

Mirrors the Node `util.ts` exports — provides the same primitives bot
authors typically reach for: command parsing, sender display strings,
message-content extraction, profile field cleanup, and ChatRef extraction
from a ChatInfo (handy when echoing into a different chat).
"""

from __future__ import annotations

import re
from typing import Any

from .types import T


def chat_info_ref(c_info: T.ChatInfo) -> T.ChatRef | None:
    """Extract a wire-format `ChatRef` from a `ChatInfo`.

    Returns `None` for non-chat infos (contactRequest, contactConnection)
    that can't be the target of `api_send_messages`. For groups, the
    `memberSupport` scope is forwarded so messages land in the right
    thread; other scopes are dropped (matches Node `util.chatInfoRef`).
    """
    t = c_info["type"]
    if t == "direct":
        return {"chatType": "direct", "chatId": c_info["contact"]["contactId"]}  # type: ignore[index]
    if t == "group":
        ref: T.ChatRef = {"chatType": "group", "chatId": c_info["groupInfo"]["groupId"]}  # type: ignore[index]
        scope = c_info.get("groupChatScope")  # type: ignore[union-attr]
        if scope and scope.get("type") == "memberSupport":
            member = scope.get("groupMember_")
            ms_scope: T.GroupChatScope_memberSupport = {"type": "memberSupport"}
            if member is not None:
                ms_scope["groupMemberId_"] = member["groupMemberId"]
            ref["chatScope"] = ms_scope
        return ref
    return None


def chat_info_name(c_info: T.ChatInfo) -> str:
    """Display string for a chat: `@Alice`, `#GroupName`, `private notes`, etc."""
    t = c_info["type"]
    if t == "direct":
        return f"@{c_info['contact']['profile']['displayName']}"  # type: ignore[index]
    if t == "group":
        scope = c_info.get("groupChatScope")  # type: ignore[union-attr]
        if scope and scope.get("type") == "memberSupport":
            member = scope.get("groupMember_")
            scope_name = f" {member['memberProfile']['displayName']}" if member else ""
            return f"#{c_info['groupInfo']['groupProfile']['displayName']}(support{scope_name})"  # type: ignore[index]
        return f"#{c_info['groupInfo']['groupProfile']['displayName']}"  # type: ignore[index]
    if t == "local":
        return "private notes"
    if t == "contactRequest":
        return f"request from @{c_info['contactRequest']['profile']['displayName']}"  # type: ignore[index]
    if t == "contactConnection":
        alias = c_info["contactConnection"].get("localAlias")  # type: ignore[index]
        return f"pending connection ({alias})" if alias else "pending connection"
    return f"<{t}>"


def sender_name(c_info: T.ChatInfo, chat_dir: T.CIDirection) -> str:
    """Sender display: chat name plus group sender suffix when applicable."""
    base = chat_info_name(c_info)
    if chat_dir["type"] == "groupRcv":
        sender = chat_dir["groupMember"]["memberProfile"]["displayName"]  # type: ignore[index]
        return f"{base} @{sender}"
    return base


def contact_address_str(link: T.CreatedConnLink) -> str:
    """Prefer the short link, fall back to the full link."""
    return link.get("connShortLink") or link["connFullLink"]


def from_local_profile(local: T.LocalProfile) -> T.Profile:
    """Strip extra LocalProfile fields (profileId, localAlias) and undefined values."""
    p: dict[str, Any] = {}
    for key in (
        "displayName",
        "fullName",
        "shortDescr",
        "image",
        "contactLink",
        "preferences",
        "peerType",
    ):
        v = local.get(key)  # type: ignore[misc]
        if v is not None:
            p[key] = v
    return p  # type: ignore[return-value]


def ci_content_text(chat_item: T.ChatItem) -> str | None:
    """Extract the message text from a sent or received message item, if any."""
    content = chat_item["content"]
    if content["type"] in ("sndMsgContent", "rcvMsgContent"):
        msg = content.get("msgContent", {})  # type: ignore[union-attr]
        return msg.get("text")
    return None


_BOT_COMMAND_RE = re.compile(r"^/([^\s]+)(.*)$")


def ci_bot_command(chat_item: T.ChatItem) -> tuple[str, str] | None:
    """Parse a `/keyword args...` slash-command from a chat item.

    Returns `(keyword, trimmed_params)` or `None` if the message isn't a
    slash command. Mirrors Node `util.ciBotCommand` semantics.
    """
    text = ci_content_text(chat_item)
    if not text:
        return None
    text = text.strip()
    m = _BOT_COMMAND_RE.match(text)
    if not m:
        return None
    return m.group(1), m.group(2).strip()


def reaction_text(reaction: T.ACIReaction) -> str:
    """Format an `ACIReaction` as the emoji character or tag string."""
    r = reaction["chatReaction"]["reaction"]  # type: ignore[index]
    if r["type"] == "emoji":
        return r["emoji"]  # type: ignore[index]
    return r.get("tag", "")  # type: ignore[union-attr]
