"""Choosing a display name the core will accept.

UPSTREAM BUG: a rejected rename half-commits. `updateUserProfile`
(src/Simplex/Chat/Store/Profiles.hs:339-350) writes the new name to the user row
before the `display_names` insert that violates the unique constraint, and
`checkConstraint` (Store/Shared.hs:193) returns the failure as a value rather
than raising, so the transaction commits. The user row is left holding a name
that belongs to somebody else, the old name stays allocated, and neither can be
applied afterwards — the database cannot be repaired through the API.

Workaround: do not attempt a rename the core is likely to refuse, and never
attempt one at all once the database already carries a half-applied rename —
that second attempt is what destroys data. When it succeeds, the core frees the
name the user row *used* to hold, and `display_names` cascades to
`contacts.local_display_name` and `group_members.local_display_name`, deleting
whoever held it.

The check cannot be complete. `api_list_contacts` omits contacts with no
connection — `Ask SimpleX Team` is seeded into every database and is one — and a
business chat allocates two names, `<Customer>` for the group and `<Customer>_1`
for the customer's member row, of which only the first is listed. Both are left
to the half-applied-rename guard rather than paid for with a call per customer
on the path before the bot serves.
"""

from __future__ import annotations

import logging

from simplex_chat import ChatApi, ChatError
from simplex_chat.types import T

from . import setup

log = logging.getLogger(__name__)


async def taken_by(api: ChatApi, user_id: int, name: str, groups: list[T.GroupInfo]) -> str | None:
    """What holds `name`, or None if nothing does.

    Names the bot cannot see are the one gap: a `display_names` row orphaned by
    an earlier half-commit belongs to nothing and is invisible here.
    """
    for contact in await api.api_list_contacts(user_id):
        if contact.get("localDisplayName") == name:
            return "a contact"

    roster_groups = []
    for group in groups:
        if group.get("localDisplayName") == name:
            return "a group"
        if setup.is_roster_group(group):
            roster_groups.append(group)

    # Members of the roster group only. Everyone there holds a name whether or
    # not they ever became a contact, and there is normally one such group. A
    # business chat's members are not scanned: that would be a call per customer
    # on the path before the bot serves, and it would only add the `_1` suffixed
    # form of a name whose group is already listed above.
    for group in roster_groups:
        for member in await api.api_list_members(group["groupId"]):
            if member.get("localDisplayName") == name:
                return "a member of the roster group"
    return None


def half_applied(groups: list[T.GroupInfo], current: str) -> bool:
    """Whether a rename was already refused against this database.

    A refused rename leaves the user row holding a name that belongs to somebody
    else, while everything the successful path would have renamed alongside it —
    the bot's own membership in every group — keeps the old one.
    `api_get_active_user` cannot show this on its own: `userQuery`
    (src/Simplex/Chat/Store/Shared.hs:559) selects `u.local_display_name` and
    binds it to both the user's name and its profile's, so the two are equal by
    construction. The memberships are the only copy that lags.
    """
    for group in groups:
        own = (group.get("membership") or {}).get("localDisplayName")
        if own:
            return own != current
    return False


async def usable(api: ChatApi, user_id: int, wanted: str) -> str:
    """`wanted`, or the name in use when the core would refuse to change to it.

    A refused rename cannot be undone, so a name that is already taken is
    declined here and the bot keeps the one it has. Answering customers matters
    more than a name, and the operator is told who holds it.
    """
    user = await api.api_get_active_user() or {}
    current = user.get("localDisplayName", "")
    if not current or wanted == current:
        return wanted

    try:
        # One listing for both checks: it grows with every customer the bot has
        # ever had, and this runs on every start until the name is resolved.
        groups = await api.api_list_groups(user_id)
        if half_applied(groups, current):
            # UPSTREAM BUG, second act: renaming from here succeeds, frees the
            # name the user row is still holding, and `display_names` cascades
            # to `contacts` and `group_members` — deleting the contact or the
            # live business-chat participant that owns it.
            log.error(
                "This database carries a half-applied rename: the user row says %r while "
                "the bot's own group membership still says otherwise. Renaming now would "
                "delete whoever holds %r, so the name is left alone. Repair the database "
                "before changing bot.display_name.",
                current,
                current,
            )
            return current
        holder = await taken_by(api, user_id, wanted, groups)
    except ChatError:
        # Reading the names failed, so nothing is known about the collision.
        # Attempting the rename risks the half-commit; keeping the current name
        # risks nothing.
        log.warning("could not check whether %r is in use; keeping %r", wanted, current)
        return current

    if holder is None:
        return wanted
    log.error(
        "bot.display_name %r is already used by %s in this database, and the core "
        "keeps every display name unique. Keeping %r. Choose another name.",
        wanted,
        holder,
        current,
    )
    return current
