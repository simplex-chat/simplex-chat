"""Declining a rename the core would refuse, without attempting it."""

from simplex_chat.core import ChatAPIError

from support_bot_light import names
from tests.conftest import USER_ID, make_contact, make_group, make_member


class NameApi:
    """The three listings that between them hold every allocated name."""

    def __init__(self, current: str, contacts=(), groups=(), members=None, membership=None):
        self.current = current
        # What the bot's own group membership still says. It differs from the
        # user row only after a refused rename; None means it agrees.
        self.membership = membership
        self.contacts = list(contacts)
        self.groups = list(groups)
        self.members = members or {}
        self.calls: list[str] = []

    async def api_get_active_user(self) -> dict:
        self.calls.append("user")
        # Both names come from one column in the core; they cannot disagree.
        return {
            "userId": USER_ID,
            "localDisplayName": self.current,
            "profile": {"displayName": self.current},
        }

    async def api_list_contacts(self, user_id: int) -> list[dict]:
        self.calls.append("contacts")
        return self.contacts

    async def api_list_groups(self, user_id: int) -> list[dict]:
        self.calls.append("groups")
        for group in self.groups:
            group["membership"] = make_member(
                1, name=self.membership if self.membership is not None else self.current
            )
        return self.groups

    async def api_list_members(self, group_id: int) -> list[dict]:
        self.calls.append(f"members:{group_id}")
        return self.members.get(group_id, [])


async def test_an_unused_name_is_accepted():
    api = NameApi("Support", contacts=[make_contact(1, "Alex")])
    assert await names.usable(api, USER_ID, "Helpdesk") == "Helpdesk"


async def test_a_name_held_by_a_contact_is_declined(caplog):
    api = NameApi("Support", contacts=[make_contact(1, "Alex")])
    assert await names.usable(api, USER_ID, "Alex") == "Support"
    assert "a contact" in caplog.text


async def test_a_name_held_by_a_group_is_declined():
    group = make_group(5, {"displayName": "Invite roster", "fullName": ""})
    group["localDisplayName"] = "Invite roster"
    api = NameApi("Support", groups=[group])
    assert await names.usable(api, USER_ID, "Invite roster") == "Support"


async def test_a_name_held_by_a_roster_group_member_is_declined():
    # Someone who joined the roster group by link holds a name whether or not
    # they ever ran /dm, and they appear in no other listing.
    group = make_group(
        5,
        {"displayName": "Invite roster", "fullName": ""},
        {"supportBotLight": {"group": "roster"}},
    )
    api = NameApi("Support", groups=[group], members={5: [make_member(9, name="Sh")]})
    assert await names.usable(api, USER_ID, "Sh") == "Support"
    assert "members:5" in api.calls


async def test_a_business_chat_customer_is_seen_through_the_group_name():
    # A customer never becomes a contact; the core names their business chat
    # after them, which is what makes the per-group member scan unnecessary.
    group = make_group(5, {"displayName": "Alex", "fullName": ""})
    group["localDisplayName"] = "Alex"
    api = NameApi("Support", groups=[group], members={5: [make_member(9, name="Alex")]})
    assert await names.usable(api, USER_ID, "Alex") == "Support"
    assert "members:5" not in api.calls


async def test_the_name_already_in_use_needs_no_check():
    # Renaming to the name already held is not a rename.
    api = NameApi("Support")
    assert await names.usable(api, USER_ID, "Support") == "Support"
    assert api.calls == ["user"]


async def test_members_are_listed_only_when_nothing_else_matches():
    # One call per group; not worth making when the answer is already known.
    group = make_group(5, {"displayName": "Alex", "fullName": ""})
    api = NameApi("Support", contacts=[make_contact(1, "Alex")], groups=[group])
    await names.usable(api, USER_ID, "Alex")
    assert "members:5" not in api.calls


async def test_a_failed_check_keeps_the_current_name(caplog):
    class Failing(NameApi):
        async def api_list_contacts(self, user_id: int) -> list[dict]:
            raise ChatAPIError("no", {"type": "chatCmdError"})

    api = Failing("Support")
    # Nothing is known about the collision, and only the rename can corrupt.
    assert await names.usable(api, USER_ID, "Helpdesk") == "Support"
    assert "could not check" in caplog.text


async def test_a_half_applied_rename_stops_every_later_attempt(caplog):
    # The user row holds a name that belongs to somebody else while the bot's
    # own membership still says the old one. Renaming from here frees the stale
    # name, and display_names cascades to whoever holds it.
    group = make_group(5, {"displayName": "Invite roster", "fullName": ""})
    api = NameApi("Ask SimpleX Team", groups=[group], membership="Support")
    assert await names.usable(api, USER_ID, "Helpdesk") == "Ask SimpleX Team"
    assert "half-applied rename" in caplog.text


async def test_a_matching_membership_is_not_treated_as_half_applied():
    group = make_group(5, {"displayName": "Invite roster", "fullName": ""})
    api = NameApi("Support", groups=[group])
    assert await names.usable(api, USER_ID, "Helpdesk") == "Helpdesk"


async def test_a_database_with_no_groups_cannot_be_checked():
    # Nothing to compare against on a first run, and nothing to cascade to.
    api = NameApi("Support")
    assert await names.usable(api, USER_ID, "Helpdesk") == "Helpdesk"


async def test_the_name_already_in_use_is_returned_unchanged():
    api = NameApi("Support")
    assert await names.usable(api, USER_ID, "Support") == "Support"
    assert api.calls == ["user"]


async def test_the_group_listing_is_read_once():
    # It grows with every customer the bot has ever had, and this runs on every
    # start until the operator resolves the name.
    group = make_group(5, {"displayName": "Invite roster", "fullName": ""})
    api = NameApi("Support", groups=[group])
    await names.usable(api, USER_ID, "Helpdesk")
    assert api.calls.count("groups") == 1
