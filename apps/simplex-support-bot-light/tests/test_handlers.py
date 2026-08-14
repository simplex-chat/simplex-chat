import pytest
from simplex_chat import ChatCommandError

from support_bot_light import handlers, messages, roster
from support_bot_light.config import Config
from support_bot_light.context import BotContext
from tests.conftest import (
    ROSTER_GROUP_ID,
    USER_ID,
    make_contact,
    make_group,
    make_group_message,
    make_member,
)

CONFIG = Config(
    display_name="Support",
    db_prefix="./x",
    welcome="hi",
    group_name="Invite roster",
    member_role="owner",
)


@pytest.fixture
def ctx(api):
    # The bot always has its own marked roster group; reconcile checks for it.
    api.groups.append(
        make_group(
            ROSTER_GROUP_ID,
            {"displayName": "Invite roster", "fullName": ""},
            custom_data={"supportBotLight": {"group": "roster"}},
        )
    )
    return BotContext(api=api, user_id=USER_ID, roster_group_id=ROSTER_GROUP_ID, config=CONFIG)


async def test_dm_with_existing_contact_marks_active(ctx, api):
    api.contacts.append(make_contact(7, "sh", connected=True))
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "active"
    assert api.replies == [messages.ADDED]
    assert api.created_member_contacts == []


async def test_dm_promotes_pending_contact_keeps_original_since(ctx, api):
    # Self-heal after a missed contactConnected; the ask date must survive.
    api.contacts.append(
        make_contact(
            7,
            "Alex",
            {"supportBotLight": {"roster": "pending", "since": "2026-08-01T00:00:00+00:00"}},
            connected=True,
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="Alex"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data[-1][1]["supportBotLight"] == {
        "roster": "active",
        "since": "2026-08-01T00:00:00+00:00",
    }
    assert api.replies == [messages.ADDED]


async def test_dm_promotes_usable_contact_even_if_invitation_was_sent(ctx, api):
    # The invitation is what made the contact usable, so both flags are set.
    api.contacts.append(
        make_contact(
            7,
            "Alex",
            {"supportBotLight": {"roster": "pending", "since": "x"}},
            connected=True,
            grp_inv_sent=True,
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="Alex"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "active"
    assert api.replies == [messages.ADDED]


async def test_dm_without_contact_creates_and_invites(ctx, api):
    msg = make_group_message(api, make_member(1, name="Alex"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.created_member_contacts == [(ROSTER_GROUP_ID, 1)]
    assert api.invitations == [(100, messages.INVITATION_TEXT)]
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "pending"
    assert api.replies == [messages.INVITATION_SENT]


async def test_dm_replies_invitation_failed_when_send_fails(ctx, api):
    api.fail_on.add("api_send_member_contact_invitation")
    msg = make_group_message(api, make_member(1, name="Alex"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "pending"
    assert api.replies == [messages.INVITATION_FAILED]


async def test_dm_while_pending_and_invitation_sent_is_a_noop(ctx, api):
    # The core rejects a second invitation.
    api.contacts.append(
        make_contact(
            7, "Alex", {"supportBotLight": {"roster": "pending", "since": "x"}}, grp_inv_sent=True
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="Alex"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.invitations == []
    assert api.custom_data == []  # already pending, mark untouched
    assert api.replies == [messages.STILL_PENDING]


async def test_dm_while_pending_and_invitation_never_sent_resends_it(ctx, api):
    api.contacts.append(
        make_contact(7, "Alex", {"supportBotLight": {"roster": "pending", "since": "x"}})
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="Alex"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.invitations == [(7, messages.INVITATION_TEXT)]
    assert api.custom_data == []  # already pending, mark untouched
    assert api.replies == [messages.INVITATION_SENT]


async def test_dm_when_already_active_is_a_noop(ctx, api):
    api.contacts.append(
        make_contact(
            7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data == []
    assert api.replies == [messages.ALREADY_ACTIVE]


async def test_dm_ignores_non_group_message(ctx, api):
    msg = make_group_message(api, make_member(1), "/dm")
    msg.chat_item["chatItem"]["chatDir"] = {"type": "directRcv"}
    await handlers.dm(ctx, msg)
    assert api.replies == [] and api.custom_data == []


async def test_dm_recovers_when_contact_vanished(ctx, api):
    # memberContactId points at a contact that no longer exists.
    msg = make_group_message(api, make_member(1, contact_id=404, name="ghost"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.created_member_contacts == [(ROSTER_GROUP_ID, 1)]


async def test_dm_replies_command_failed_when_api_fails(ctx, api):
    # api_create_member_contact has no try/except of its own.
    api.fail_on.add("api_create_member_contact")
    msg = make_group_message(api, make_member(1, name="Alex"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.replies == [messages.COMMAND_FAILED]


async def test_dm_lets_unexpected_errors_propagate(ctx, api):
    async def boom(contact_id, message=None):
        raise RuntimeError("network on fire")

    api.api_send_member_contact_invitation = boom
    msg = make_group_message(api, make_member(1, name="Alex"), "/dm")
    with pytest.raises(RuntimeError):
        await handlers.dm(ctx, msg)


async def test_dm_after_leave_does_not_promote_unconnected_contact(ctx, api):
    member = make_member(1, name="Alex")
    await handlers.dm(ctx, make_group_message(api, member, "/dm"))
    assert api.created_member_contacts == [(ROSTER_GROUP_ID, 1)]
    created_contact_id = api.contacts[-1]["contactId"]
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "pending"

    # The core sets memberContactId as soon as the contact exists.
    member["memberContactId"] = created_contact_id
    await handlers.leave(ctx, make_group_message(api, member, "/leave"))
    api.custom_data.clear()

    await handlers.dm(ctx, make_group_message(api, member, "/dm"))
    written = api.custom_data[-1][1]["supportBotLight"]["roster"] if api.custom_data else None
    assert written != "active", "unconnected contact must never be marked active"


async def test_dm_after_leave_still_promotes_on_accept(ctx, api):
    """/dm -> /leave -> /dm -> accept must end up active.

    /leave clears our roster mark, but the core's contactGrpInvSent survives and
    cannot be unset, so the second /dm must re-establish the pending mark
    or the eventual acceptance has nothing to promote.
    """
    member = make_member(1, name="Alex")
    await handlers.dm(ctx, make_group_message(api, member, "/dm"))
    contact_id = api.contacts[-1]["contactId"]
    # The fake names the contact after the group member id, not the member.
    contact_name = api.contacts[-1]["profile"]["displayName"]
    member["memberContactId"] = contact_id

    await handlers.leave(ctx, make_group_message(api, member, "/leave"))
    await handlers.dm(ctx, make_group_message(api, member, "/dm"))
    assert api.replies[-1] == messages.STILL_PENDING

    for c in api.contacts:
        if c["contactId"] == contact_id:
            c["activeConn"] = {"connStatus": {"type": "ready"}}
    await handlers.contact_ready(ctx, contact_id)
    assert [e.name for e in await roster.active(api, USER_ID)] == [contact_name]


async def test_contact_connected_promotes_pending(ctx, api):
    api.contacts.append(
        make_contact(
            7,
            "Alex",
            {"supportBotLight": {"roster": "pending", "since": "2026-08-13"}},
            connected=True,
        )
    )
    await handlers.contact_ready(ctx, 7)
    assert api.custom_data[-1][1]["supportBotLight"] == {
        "roster": "active",
        "since": "2026-08-13",  # original ask time preserved
    }
    assert api.sent == [(["group", ROSTER_GROUP_ID], "Now on the roster: Alex")]


async def test_contact_connected_ignores_unmarked_contact(ctx, api):
    api.contacts.append(make_contact(7, "stranger"))
    await handlers.contact_ready(ctx, 7)
    assert api.custom_data == [] and api.sent == []


async def test_contact_connected_ignores_already_active(ctx, api):
    api.contacts.append(
        make_contact(7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}})
    )
    await handlers.contact_ready(ctx, 7)
    assert api.custom_data == [] and api.sent == []


async def test_contact_connected_does_not_promote_an_unusable_connection(ctx, api):
    # The event says the connection is up; the contact record says otherwise.
    # active() consults the record, so promoting here would list somebody the
    # bot cannot reach.
    api.contacts.append(
        make_contact(
            7,
            "Alex",
            {"supportBotLight": {"roster": "pending", "since": "x"}},
            conn_status="deleted",
        )
    )
    await handlers.contact_ready(ctx, 7)
    assert api.custom_data == [] and api.sent == []


async def test_contact_connected_for_unknown_contact_is_a_noop(ctx, api):
    await handlers.contact_ready(ctx, 999)
    assert api.custom_data == [] and api.sent == []


async def test_a_failed_revocation_is_reported_to_the_roster_group(ctx, api):
    # Revocation is the access-control path; a silent failure would leave the
    # operator reading /list as the truth.
    api.contacts.append(
        make_contact(7, "Alex", {"supportBotLight": {"roster": "active", "since": "x"}})
    )
    api.fail_on.add("api_set_contact_custom_data")
    await handlers.member_gone(ctx, ROSTER_GROUP_ID, make_member(1, contact_id=7, name="Alex"))
    assert api.sent[-1][1] == messages.REVOKE_FAILED.format(name="Alex")


async def test_list_renders_both_states(ctx, api):
    api.contacts += [
        make_contact(
            1,
            "sh",
            {"supportBotLight": {"roster": "active", "since": "2026-08-13"}},
            connected=True,
        ),
        make_contact(2, "Alex", {"supportBotLight": {"roster": "pending", "since": "2026-08-13"}}),
    ]
    await handlers.list_roster(ctx, make_group_message(api, make_member(1), "/list"))
    assert "On the roster (1):" in api.replies[0]
    assert "Contact request not accepted (1):" in api.replies[0]


async def test_list_when_empty(ctx, api):
    await handlers.list_roster(ctx, make_group_message(api, make_member(1), "/list"))
    assert api.replies == [messages.ROSTER_EMPTY]


async def test_list_replies_command_failed_when_api_fails(ctx, api):
    api.fail_on.add("api_list_contacts")
    await handlers.list_roster(ctx, make_group_message(api, make_member(1), "/list"))
    assert api.replies == [messages.COMMAND_FAILED]


async def test_leave_clears_the_mark(ctx, api):
    api.contacts.append(
        make_contact(7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}})
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/leave")
    await handlers.leave(ctx, msg)
    assert api.custom_data[-1] == (7, None)
    assert api.replies == [messages.LEFT]


async def test_leave_when_not_on_roster(ctx, api):
    api.contacts.append(make_contact(7, "sh"))
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/leave")
    await handlers.leave(ctx, msg)
    assert api.custom_data == []
    assert api.replies == [messages.NOT_ON_ROSTER]


async def test_leave_without_any_contact(ctx, api):
    msg = make_group_message(api, make_member(1, name="stranger"), "/leave")
    await handlers.leave(ctx, msg)
    assert api.replies == [messages.NOT_ON_ROSTER]
    assert api.created_member_contacts == []  # /leave never creates a contact


async def test_leave_ignores_non_group_message(ctx, api):
    msg = make_group_message(api, make_member(1, contact_id=7), "/leave")
    msg.chat_item["chatItem"]["chatDir"] = {"type": "directRcv"}
    await handlers.leave(ctx, msg)
    assert api.replies == [] and api.custom_data == []


async def test_leave_replies_command_failed_when_api_fails(ctx, api):
    api.contacts.append(
        make_contact(7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}})
    )
    api.fail_on.add("api_list_contacts")
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/leave")
    await handlers.leave(ctx, msg)
    assert api.replies == [messages.COMMAND_FAILED]


async def test_help_replies_with_help_text(ctx, api):
    await handlers.help_cmd(ctx, make_group_message(api, make_member(1), "/help"))
    assert api.replies == [messages.HELP]


async def test_help_replies_command_failed_when_send_fails(ctx, api):
    # help_cmd's only action is the reply, so the first send must fail alone.
    calls = 0
    original = api.api_send_text_reply

    async def flaky_once(chat_item, text):
        nonlocal calls
        calls += 1
        if calls == 1:
            raise ChatCommandError("boom", {"type": "chatCmdError"})
        return await original(chat_item, text)

    api.api_send_text_reply = flaky_once
    await handlers.help_cmd(ctx, make_group_message(api, make_member(1), "/help"))
    assert api.replies == [messages.COMMAND_FAILED]


async def test_dm_redrives_a_contact_that_never_connected(ctx, api):
    # Marked active but never usable: the member contact still exists, so the
    # invitation can be re-sent.
    api.contacts.append(
        make_contact(7, "sh", {"supportBotLight": {"roster": "active", "since": "2026-01-01"}})
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "pending"
    assert api.invitations == [(7, messages.INVITATION_TEXT)]
    assert api.replies == [messages.INVITATION_SENT]


async def test_dm_reports_a_connection_that_is_gone_for_good(ctx, api):
    # The person deleted the bot after connecting. The core cleared
    # contactGroupMemberId, so no invitation can be sent and telling them to
    # retry would be false.
    api.contacts.append(
        make_contact(
            7,
            "sh",
            {"supportBotLight": {"roster": "active", "since": "2026-01-01"}},
            grp_member_id=None,
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.invitations == []
    assert api.replies == [messages.CONNECTION_LOST]


async def test_dm_on_a_dead_contact_with_an_invitation_outstanding_waits(ctx, api):
    api.contacts.append(
        make_contact(
            7,
            "sh",
            {"supportBotLight": {"roster": "active", "since": "2026-01-01"}},
            grp_inv_sent=True,
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "pending"
    assert api.invitations == []
    assert api.replies == [messages.STILL_PENDING]


async def test_dm_finds_a_contact_created_since_the_message_was_built(ctx, api):
    # Two commands sent in quick succession both carry the pre-/dm snapshot, in
    # which memberContactId is still None.
    api.contacts.append(
        make_contact(
            7, "sh", {"supportBotLight": {"roster": "pending", "since": "x"}}, grp_inv_sent=True
        )
    )
    api.members[ROSTER_GROUP_ID] = [make_member(1, contact_id=7, name="sh")]
    stale = make_member(1, name="sh")  # no memberContactId
    await handlers.dm(ctx, make_group_message(api, stale, "/dm"))
    assert api.created_member_contacts == []
    assert api.replies == [messages.STILL_PENDING]


async def test_leave_finds_a_contact_created_since_the_message_was_built(ctx, api):
    api.contacts.append(
        make_contact(7, "sh", {"supportBotLight": {"roster": "pending", "since": "x"}})
    )
    api.members[ROSTER_GROUP_ID] = [make_member(1, contact_id=7, name="sh")]
    stale = make_member(1, name="sh")
    await handlers.leave(ctx, make_group_message(api, stale, "/leave"))
    assert api.custom_data[-1] == (7, None)
    assert api.replies == [messages.LEFT]


async def test_member_gone_takes_them_off_the_roster(ctx, api):
    api.contacts.append(
        make_contact(
            7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    await handlers.member_gone(ctx, ROSTER_GROUP_ID, make_member(1, contact_id=7, name="sh"))
    assert api.custom_data[-1] == (7, None)
    assert api.sent[-1][1] == messages.REMOVED_FROM_GROUP.format(name="sh")


async def test_member_gone_ignores_other_groups(ctx, api):
    api.contacts.append(
        make_contact(
            7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    await handlers.member_gone(ctx, 999, make_member(1, contact_id=7, name="sh"))
    assert api.custom_data == [] and api.sent == []


async def test_member_gone_ignores_someone_not_on_the_roster(ctx, api):
    api.contacts.append(make_contact(7, "sh", connected=True))
    await handlers.member_gone(ctx, ROSTER_GROUP_ID, make_member(1, contact_id=7, name="sh"))
    assert api.custom_data == [] and api.sent == []


async def test_reconcile_promotes_an_acceptance_missed_while_stopped(ctx, api):
    api.members[ROSTER_GROUP_ID] = [
        make_member(1, contact_id=1),
        make_member(2, contact_id=2),
        make_member(3, contact_id=3),
    ]
    api.contacts += [
        make_contact(
            1,
            "accepted",
            {"supportBotLight": {"roster": "pending", "since": "2026-01-01"}},
            connected=True,
        ),
        make_contact(
            2, "waiting", {"supportBotLight": {"roster": "pending", "since": "2026-01-01"}}
        ),
        make_contact(
            3,
            "already",
            {"supportBotLight": {"roster": "active", "since": "2026-01-01"}},
            connected=True,
        ),
    ]
    await handlers.reconcile_roster(ctx)
    assert api.custom_data == [
        (1, {"supportBotLight": {"roster": "active", "since": "2026-01-01"}})
    ]
    assert api.sent[-1][1] == messages.NOW_ACTIVE.format(name="accepted")


async def test_contact_ready_failure_does_not_escape(ctx, api):
    api.contacts.append(
        make_contact(
            7, "sh", {"supportBotLight": {"roster": "pending", "since": "x"}}, connected=True
        )
    )
    api.fail_on.add("api_set_contact_custom_data")
    await handlers.contact_ready(ctx, 7)  # must not raise
    assert api.sent == []


async def test_reconcile_removes_someone_who_left_while_stopped(ctx, api):
    # api_list_members keeps the row and only changes its status.
    api.members[ROSTER_GROUP_ID] = [
        make_member(1, contact_id=5, name="stays"),
        make_member(2, contact_id=7, name="gone", status="left"),
    ]
    api.contacts += [
        make_contact(
            5, "stays", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(
            7, "gone", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
    ]
    await handlers.reconcile_roster(ctx)
    assert api.custom_data[-1] == (7, None)
    assert api.sent[-1][1] == messages.REMOVED_FROM_GROUP.format(name="gone")


async def test_reconcile_failure_does_not_stop_startup(ctx, api):
    api.fail_on.add("api_list_members")
    await handlers.reconcile_roster(ctx)  # must not raise


async def test_reconcile_continues_past_a_failing_contact(ctx, api):
    api.members[ROSTER_GROUP_ID] = [make_member(1, contact_id=5, name="stays")]
    api.contacts += [
        make_contact(
            7, "a", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(
            8, "b", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
    ]
    attempts: list[int] = []

    async def flaky(contact_id, custom_data=None):
        attempts.append(contact_id)
        raise ChatCommandError("nope", {"type": "chatCmdError"})

    api.api_set_contact_custom_data = flaky
    await handlers.reconcile_roster(ctx)
    assert attempts == [7, 8], "a failure on one contact must not abandon the rest"


async def test_dm_on_a_dead_contact_leaves_the_mark_alone(ctx, api):
    api.contacts.append(
        make_contact(
            7,
            "sh",
            {"supportBotLight": {"roster": "active", "since": "2026-01-01"}},
            grp_member_id=None,
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.custom_data == []
    assert api.replies == [messages.CONNECTION_LOST]


async def test_reconcile_skips_revocation_when_the_marker_is_ambiguous(ctx, api):
    # A second marked group means ensure_roster_group may have picked the wrong
    # one; deleting every mark on that basis is not recoverable.
    api.groups.append(
        make_group(
            99,
            {"displayName": "Invite roster", "fullName": ""},
            custom_data={"supportBotLight": {"group": "roster"}},
        )
    )
    api.members[ROSTER_GROUP_ID] = []
    api.contacts.append(
        make_contact(
            7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    await handlers.reconcile_roster(ctx)
    assert api.custom_data == []


async def test_reconcile_revokes_even_when_the_last_member_leaves(ctx, api):
    api.members[ROSTER_GROUP_ID] = [make_member(1, contact_id=7, name="gone", status="left")]
    api.contacts.append(
        make_contact(
            7, "gone", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    await handlers.reconcile_roster(ctx)
    assert api.custom_data[-1] == (7, None)


async def test_dm_accepts_a_connection_the_member_started(ctx, api):
    # Tapping "connect directly" on the bot's profile leaves a prepared contact
    # with no contactGroupMemberId, which looks identical to a dead one.
    api.contacts.append(make_contact(7, "Kit", grp_member_id=None, conn_status="prepared"))
    msg = make_group_message(api, make_member(1, contact_id=7, name="Kit"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.accepted_member_contacts == [7]
    assert api.custom_data[-1][1]["supportBotLight"]["roster"] == "pending"
    assert api.replies == [messages.ACCEPTING]


async def test_dm_still_reports_a_genuinely_dead_contact(ctx, api):
    # No prepared connection and no groupDirectInv: nothing to accept.
    api.contacts.append(make_contact(7, "sh", grp_member_id=None, conn_status="deleted"))
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.accepted_member_contacts == []
    assert api.replies == [messages.CONNECTION_LOST]


async def test_dm_does_not_re_accept_a_connection_already_started(ctx, api):
    # The core keeps groupDirectInv after acceptance and rejects a second
    # accept with "connection already started".
    contact = make_contact(7, "Kit", grp_member_id=None, conn_status="prepared")
    contact["groupDirectInv"] = {
        "groupDirectInvLink": "x",
        "groupDirectInvStartedConnection": True,
    }
    api.contacts.append(contact)
    msg = make_group_message(api, make_member(1, contact_id=7, name="Kit"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.accepted_member_contacts == []
    assert api.replies == [messages.ACCEPTING]


async def test_dm_accepts_an_invitation_not_yet_started(ctx, api):
    contact = make_contact(7, "Kit", grp_member_id=None, conn_status="prepared")
    contact["groupDirectInv"] = {
        "groupDirectInvLink": "x",
        "groupDirectInvStartedConnection": False,
    }
    api.contacts.append(contact)
    msg = make_group_message(api, make_member(1, contact_id=7, name="Kit"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.accepted_member_contacts == [7]


async def test_dm_reports_a_handshake_in_progress_as_connecting(ctx, api):
    # The core clears contactGroupMemberId when the peer accepts and only later
    # reports ready. Calling that gone would send the member to advice that
    # tears the completing connection down.
    api.contacts.append(
        make_contact(
            7,
            "Kit",
            {"supportBotLight": {"roster": "pending", "since": "x"}},
            grp_member_id=None,
            conn_status="accepted",
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="Kit"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.replies == [messages.CONNECTING]
    assert api.accepted_member_contacts == []


async def test_dm_marks_an_unmarked_member_whose_connection_is_completing(ctx, api):
    # ACCEPTING and CONNECTING both promise a roster place, and contact_ready
    # delivers it only for a pending mark.
    api.contacts.append(make_contact(7, "Kit", grp_member_id=None, conn_status="accepted"))
    msg = make_group_message(api, make_member(1, contact_id=7, name="Kit"), "/dm")
    await handlers.dm(ctx, msg)
    assert roster.entry_of(api.contacts[0]) is not None

    for c in api.contacts:
        c["activeConn"] = {"connStatus": {"type": "ready"}}
    await handlers.contact_ready(ctx, 7)
    assert [e.name for e in await roster.active(api, USER_ID)] == ["Kit"]


async def test_dm_marks_an_unmarked_member_whose_accept_already_started(ctx, api):
    contact = make_contact(7, "Kit", grp_member_id=None, conn_status="joined")
    contact["groupDirectInv"] = {
        "groupDirectInvLink": "x",
        "groupDirectInvStartedConnection": True,
    }
    api.contacts.append(contact)
    msg = make_group_message(api, make_member(1, contact_id=7, name="Kit"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.replies == [messages.ACCEPTING]
    assert roster.entry_of(api.contacts[0]) is not None


async def test_dm_reports_a_dead_connection_even_after_we_accepted(ctx, api):
    # The core never clears groupDirectInv, so the started flag alone would
    # promise progress on a connection the peer has since deleted.
    contact = make_contact(7, "Alice", grp_member_id=None, conn_status="deleted")
    contact["groupDirectInv"] = {
        "groupDirectInvLink": "x",
        "groupDirectInvStartedConnection": True,
    }
    api.contacts.append(contact)
    msg = make_group_message(api, make_member(1, contact_id=7, name="Alice"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.accepted_member_contacts == []
    assert api.replies == [messages.CONNECTION_LOST]


async def test_dm_fast_path_announces_the_arrival(ctx, api):
    api.contacts.append(make_contact(7, "sh", connected=True))
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.replies == [messages.ADDED]
    assert api.sent[-1][1] == messages.NOW_ACTIVE.format(name="sh")


async def test_dm_on_an_already_active_member_announces_nothing(ctx, api):
    api.contacts.append(
        make_contact(
            7, "sh", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        )
    )
    msg = make_group_message(api, make_member(1, contact_id=7, name="sh"), "/dm")
    await handlers.dm(ctx, msg)
    assert api.sent == []
