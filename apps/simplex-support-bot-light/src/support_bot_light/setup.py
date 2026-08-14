"""Find or create the roster group, and keep its command menu in sync."""

from __future__ import annotations

import asyncio
import logging

from simplex_chat import ChatApi, ChatError
from simplex_chat.types import T

from . import commands, roster
from .config import Config

log = logging.getLogger(__name__)

GROUP_MARKER = "roster"
JOIN_ROLE: T.GroupMemberRole = "member"

# api_update_group_profile broadcasts, and the core's view queue is bounded
# (tbqSize in Mobile.hs) with a blocking write. Nothing drains that queue until
# the bot serves, so after enough downtime this call cannot return. The bot must
# start anyway: the write completes once the queue drains, and a stale menu is a
# cosmetic problem next to a process that never gets there.
PROFILE_PUSH_TIMEOUT = 30.0


def is_roster_group(group: T.GroupInfo) -> bool:
    """Whether this is a roster group the bot is still in.

    api_list_groups keeps groups the bot has left or been removed from. Without
    the membership test the marker on a dead group would be chosen on every
    start: no command would ever arrive, nothing could be posted, and the marker
    would keep a replacement from being created.
    """
    mark = (group.get("customData") or {}).get(roster.NAMESPACE)
    if not isinstance(mark, dict) or mark.get("group") != GROUP_MARKER:
        return False
    return roster.in_group(group["membership"])


def _preferences() -> T.GroupPreferences:
    return {
        "directMessages": {"enable": "on"},
        "commands": commands.to_wire(commands.COMMANDS),
    }


async def _get_or_create_group_link(api: ChatApi, group_id: int) -> str | None:
    """The group's join link, creating one if it doesn't exist yet.

    A link can be missing if the process died between marking the group and
    creating the link on a previous run — that must not leave the group
    permanently unjoinable.

    `api_get_group_link_str` also fails for reasons other than "no link
    exists" — if that happens while a link is actually present, the fallback
    create hits the group's unique link index and raises too. A missing link
    must never block startup, so that failure is logged and swallowed rather
    than left to propagate out of `ensure_roster_group`.
    """
    try:
        return await api.api_get_group_link_str(group_id)
    except ChatError:
        pass
    try:
        return await api.api_create_group_link(group_id, JOIN_ROLE)
    except ChatError:
        log.warning(
            "Could not get or create a join link for roster group %s", group_id, exc_info=True
        )
        return None


async def ensure_roster_group(api: ChatApi, user_id: int, config: Config) -> int:
    """Return the roster group id, creating the group on first run.

    The group is identified by a marker in its custom data, not by name, so an
    operator renaming it in the client doesn't cause a second group to appear.
    """
    marked = [g for g in await api.api_list_groups(user_id) if is_roster_group(g)]
    if len(marked) > 1:
        # Reachable when two instances share a database, or after a database is
        # restored. Members of the group not chosen here are talking to a bot
        # that ignores them, so say which one won.
        log.warning(
            "%d groups carry the roster marker (%s); using %s",
            len(marked),
            ", ".join(str(g["groupId"]) for g in marked),
            marked[0]["groupId"],
        )
    if marked:
        group = marked[0]
        try:
            await _sync_preferences(api, group)
        except ChatError:
            # The menu is a convenience; the commands work when typed. The core
            # requires owner rights to update the profile, so an operator who
            # demotes the bot would otherwise brick every later start.
            log.warning("could not update the command menu", exc_info=True)
        group_id = group["groupId"]
        log.info("Roster group: %s:%s", group_id, group["localDisplayName"])
    else:
        profile: T.GroupProfile = {
            "displayName": config.group_name,
            "fullName": "",
            "groupPreferences": _preferences(),
        }
        group = await api.api_new_group(user_id, profile)
        group_id = group["groupId"]
        await api.api_set_group_custom_data(group_id, {roster.NAMESPACE: {"group": GROUP_MARKER}})
        log.info("Roster group created: %s", group_id)

    link = await _get_or_create_group_link(api, group_id)
    if link is not None:
        log.info("Roster group link (share with the people who should answer):\n%s", link)
    return group_id


async def _sync_preferences(api: ChatApi, group: T.GroupInfo) -> None:
    """Restore the preferences the roster group needs, only when they differ.

    Both matter: without `commands` there is no menu, and without
    `directMessages` the core refuses to create a member contact, so `/dm`
    fails with nothing to explain it. An owner can switch either off in a
    client, so neither can be assumed to survive from creation.

    `api_update_group_profile` broadcasts to every member, so a no-op update is
    traffic for everyone in the group.
    """
    profile = group.get("groupProfile") or {}
    prefs = profile.get("groupPreferences") or {}
    desired = _preferences()
    if all(prefs.get(key) == value for key, value in desired.items()):
        return
    updated: T.GroupProfile = {**profile, "groupPreferences": {**prefs, **desired}}
    try:
        await asyncio.wait_for(
            api.api_update_group_profile(group["groupId"], updated), PROFILE_PUSH_TIMEOUT
        )
    except TimeoutError:
        log.warning(
            "Roster group preferences are still being written after %ss; continuing",
            PROFILE_PUSH_TIMEOUT,
        )
        return
    log.info("Restored roster group preferences on %s", group["groupId"])
