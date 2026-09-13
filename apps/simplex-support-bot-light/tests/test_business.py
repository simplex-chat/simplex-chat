import pytest
from simplex_chat import ChatCommandError

from support_bot_light import business, messages
from support_bot_light.config import Config
from support_bot_light.context import BotContext
from tests.conftest import (
    ROSTER_GROUP_ID,
    USER_ID,
    join_roster_group,
    make_contact,
    make_group,
    make_member,
)

BUSINESS_GROUP_ID = 42
CONFIG = Config("Support", "./x", "hi", "Invite roster", "owner")


@pytest.fixture
def ctx(api):
    return BotContext(api=api, user_id=USER_ID, roster_group_id=ROSTER_GROUP_ID, config=CONFIG)


def event(name="Alex"):
    return {
        "type": "acceptingBusinessRequest",
        "groupInfo": make_group(BUSINESS_GROUP_ID, {"displayName": name, "fullName": ""}),
    }


async def test_adds_active_roster_members(ctx, api):
    api.contacts += [
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(
            2, "Narasimha", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(3, "Alex", {"supportBotLight": {"roster": "pending", "since": "x"}}),
    ]
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert api.added == [
        (BUSINESS_GROUP_ID, 2, "owner"),
        (BUSINESS_GROUP_ID, 1, "owner"),
    ]
    assert api.sent == [(["group", ROSTER_GROUP_ID], "Connected: Alex → added Narasimha, sh")]


@pytest.mark.parametrize("status", ["rejected", "removed", "left", "deleted", "unknown"])
async def test_does_not_add_someone_who_has_left_the_roster_group(ctx, api, status):
    # The departure event and a queued business request arrive in whatever order
    # the core dispatches them, so an active mark is not authority on its own:
    # this is what stops a departed member reading a conversation started after
    # they went.
    api.contacts += [
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(
            2, "gone", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
    ]
    join_roster_group(api)
    api.members[ROSTER_GROUP_ID][1]["memberStatus"] = status
    await business.on_business_request(ctx, event())
    assert api.added == [(BUSINESS_GROUP_ID, 1, "owner")]
    assert api.sent == [(["group", ROSTER_GROUP_ID], "Connected: Alex → added sh")]


async def test_reconcile_does_not_add_someone_who_has_left_the_roster_group(ctx, api):
    api.contacts.append(
        make_contact(
            1, "gone", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    join_roster_group(api)
    api.members[ROSTER_GROUP_ID][0]["memberStatus"] = "removed"
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    api.members[BUSINESS_GROUP_ID] = []
    await business.reconcile_chats(ctx)
    assert api.added == []


async def test_skips_members_already_in_the_group(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.members[BUSINESS_GROUP_ID] = [make_member(5, contact_id=1, status="invited")]
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert api.added == []
    assert api.sent[-1][1] == messages.NOBODY_NEW_LOG.format(customer="Alex")


async def test_does_not_skip_members_who_left(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.members[BUSINESS_GROUP_ID] = [make_member(5, contact_id=1, status="left")]
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert api.added == [(BUSINESS_GROUP_ID, 1, "owner")]


async def test_empty_roster_logs_and_adds_nobody(ctx, api):
    await business.on_business_request(ctx, event())
    assert api.added == []
    assert api.sent[-1][1] == messages.EMPTY_ROSTER_LOG.format(customer="Alex")


async def test_pending_only_roster_counts_as_empty(ctx, api):
    api.contacts.append(
        make_contact(1, "Alex", {"supportBotLight": {"roster": "pending", "since": "x"}})
    )
    await business.on_business_request(ctx, event())
    assert api.added == []
    assert api.sent[-1][1] == messages.EMPTY_ROSTER_LOG.format(customer="Alex")


async def test_one_failure_does_not_block_the_rest(ctx, api):
    api.contacts += [
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(
            2, "Narasimha", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
    ]
    calls: list[int] = []
    original = api.api_add_member

    async def flaky(group_id, contact_id, member_role):
        calls.append(contact_id)
        if contact_id == 2:
            raise ChatCommandError("nope", {"type": "chatCmdError"})
        return await original(group_id, contact_id, member_role)

    api.api_add_member = flaky
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert sorted(calls) == [1, 2]  # both attempted
    assert api.sent[-1][1] == "Connected: Alex → added sh (failed: Narasimha)"


async def test_roster_read_failure_logs_and_adds_nobody(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.fail_on.add("api_list_contacts")
    await business.on_business_request(ctx, event())
    assert api.added == []
    assert api.sent[-1][1] == messages.BUSINESS_FAILED_LOG.format(customer="Alex")


async def test_member_list_failure_logs_and_adds_nobody(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.fail_on.add("api_list_members")
    await business.on_business_request(ctx, event())
    assert api.added == []
    assert api.sent[-1][1] == messages.BUSINESS_FAILED_LOG.format(customer="Alex")


async def test_uses_configured_member_role(api):
    ctx = BotContext(
        api=api,
        user_id=USER_ID,
        roster_group_id=ROSTER_GROUP_ID,
        config=Config("S", "./x", "hi", "R", "admin"),
    )
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert api.added == [(BUSINESS_GROUP_ID, 1, "admin")]


async def test_reconcile_repairs_a_chat_left_half_added_by_a_crash(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    api.members[BUSINESS_GROUP_ID] = []
    join_roster_group(api)
    await business.reconcile_chats(ctx)
    assert api.added == [(BUSINESS_GROUP_ID, 1, "owner")]
    assert api.sent[-1][1] == "Connected: Alex → added sh"


async def test_reconcile_is_idempotent_when_everyone_is_present(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    api.members[BUSINESS_GROUP_ID] = [make_member(5, contact_id=1, status="complete")]
    join_roster_group(api)
    await business.reconcile_chats(ctx)
    assert api.added == []
    # The chat was left unmarked, so a crash took the roster group's record of
    # this customer with it; the repair puts it back even with nothing to add.
    assert api.sent[-1][1] == messages.NOBODY_NEW_LOG.format(customer="Alex")


async def test_reconcile_skips_non_business_groups(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": ROSTER_GROUP_ID,
            "groupProfile": {"displayName": "roster", "fullName": ""},
            "localDisplayName": "roster",
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    join_roster_group(api)
    await business.reconcile_chats(ctx)
    assert api.added == []


async def test_reconcile_skips_a_chat_the_bot_has_left(ctx, api):
    # The core keeps the group row after removal; adding into it would fail on
    # every start, and the customer is no longer the bot's to serve.
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="removed"),
        }
    )
    api.members[BUSINESS_GROUP_ID] = []
    join_roster_group(api)
    await business.reconcile_chats(ctx)
    assert api.added == []
    assert api.group_custom_data == []


async def test_reconcile_failure_does_not_stop_startup(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.fail_on.add("api_list_groups")
    join_roster_group(api)
    await business.reconcile_chats(ctx)  # must not raise


async def test_reconcile_skips_a_chat_whose_roster_pass_already_ran(ctx, api):
    # Someone who joins the roster later must not be back-filled into every
    # conversation the bot has ever handled.
    api.contacts.append(
        make_contact(
            1, "newbie", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
            "customData": {"supportBotLight": {"rostered": True}},
        }
    )
    api.members[BUSINESS_GROUP_ID] = []
    join_roster_group(api)
    await business.reconcile_chats(ctx)
    assert api.added == []
    assert api.sent == []


async def test_reconcile_does_not_re_invite_someone_who_left_a_chat(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
            "customData": {"supportBotLight": {"rostered": True}},
        }
    )
    api.members[BUSINESS_GROUP_ID] = [make_member(5, contact_id=1, status="left")]
    join_roster_group(api)
    await business.reconcile_chats(ctx)
    assert api.added == []


async def test_on_business_request_marks_the_chat_as_rostered(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert api.group_custom_data[-1] == (
        BUSINESS_GROUP_ID,
        {"supportBotLight": {"rostered": True}},
    )


async def test_reconcile_marks_chats_even_with_an_empty_roster(ctx, api):
    # Otherwise the chat stays unmarked and a later restart back-fills whoever
    # joined the roster in the meantime.
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    await business.reconcile_chats(ctx)
    assert api.group_custom_data[-1] == (
        BUSINESS_GROUP_ID,
        {"supportBotLight": {"rostered": True}},
    )
    assert api.added == []


async def test_a_failed_mark_does_not_report_nobody_added(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.fail_on.add("api_set_group_custom_data")
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert api.added == [(BUSINESS_GROUP_ID, 1, "owner")]
    assert api.sent[-1][1] == "Connected: Alex → added sh"


async def test_a_chat_where_every_add_failed_is_retried_next_start(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.fail_on.add("api_add_member")
    join_roster_group(api)
    await business.on_business_request(ctx, event())
    assert api.group_custom_data == []  # not marked, so repair will revisit it


async def test_a_chat_left_unmarked_is_not_back_filled_with_a_later_roster(ctx, api):
    # The one bit that keeps a new roster member out of old conversations.
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    join_roster_group(api)
    api.fail_on.add("api_send_text_message")
    await business.on_business_request(ctx, event())
    api.fail_on.clear()

    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
            "customData": api.group_custom_data[-1][1],
        }
    )
    api.contacts.append(
        make_contact(
            2, "newbie", {"supportBotLight": {"roster": "active", "since": "y"}}, connected=True
        )
    )
    join_roster_group(api)
    api.added.clear()
    await business.reconcile_chats(ctx)
    assert api.added == []


async def test_a_chat_is_marked_even_when_its_line_never_went_out(ctx, api):
    # An unmarked chat is repaired by every later start with the roster of the
    # day, so withholding the marker to preserve a log line would hand this
    # customer's conversation to whoever joins the roster next.
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    join_roster_group(api)
    api.fail_on.add("api_send_text_message")
    await business.on_business_request(ctx, event())
    assert api.added == [(BUSINESS_GROUP_ID, 1, "owner")]
    assert api.group_custom_data[-1][1] == {"supportBotLight": {"rostered": True}}


async def test_a_failed_mark_still_reports_the_repaired_chat(ctx, api):
    # The marker is re-derived on the next start; the report is not, because
    # the event that would have produced it was consumed before the crash.
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    api.members[BUSINESS_GROUP_ID] = []
    join_roster_group(api)
    api.fail_on.add("api_set_group_custom_data")
    await business.reconcile_chats(ctx)
    assert api.added == [(BUSINESS_GROUP_ID, 1, "owner")]
    assert api.sent[-1][1] == "Connected: Alex → added sh"


async def test_an_unfinished_repair_is_retried_by_the_queued_event(ctx, api):
    # Nothing was added and the chat was left unmarked, so the event that the
    # startup pass raced is the only remaining chance to finish it.
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    api.members[BUSINESS_GROUP_ID] = []
    join_roster_group(api)
    api.fail_on.add("api_add_member")
    await business.reconcile_chats(ctx)
    assert api.group_custom_data == []  # not marked: the repair failed

    api.fail_on.clear()
    await business.on_business_request(ctx, event())
    assert api.added == [(BUSINESS_GROUP_ID, 1, "owner")]


async def test_repair_does_not_re_report_a_chat_to_the_event_handler(ctx, api):
    api.contacts.append(
        make_contact(
            1, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    api.groups.append(
        {
            "groupId": BUSINESS_GROUP_ID,
            "groupProfile": {"displayName": "Alex", "fullName": ""},
            "localDisplayName": "Alex",
            "businessChat": {"chatType": "business", "businessId": "b", "customerId": "c"},
            "membership": make_member(99, name="bot", status="complete"),
        }
    )
    api.members[BUSINESS_GROUP_ID] = []
    join_roster_group(api)
    await business.reconcile_chats(ctx)
    posts_after_repair = len(api.sent)
    await business.on_business_request(ctx, event())
    assert len(api.sent) == posts_after_repair  # the queued event adds no line

    # Only that one event is swallowed: the same customer coming back later
    # must be handled like anyone else.
    api.members[BUSINESS_GROUP_ID] = []
    await business.on_business_request(ctx, event())
    assert len(api.sent) == posts_after_repair + 1
