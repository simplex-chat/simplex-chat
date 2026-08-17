import asyncio
import logging

from support_bot_light import commands, setup
from support_bot_light.config import Config
from tests.conftest import ROSTER_GROUP_ID, USER_ID, make_group

CONFIG = Config("Support", "./x", "hi", "Invite roster", "owner")
MARKER = {"supportBotLight": {"group": "roster"}}


async def test_creates_group_when_none_marked(api, caplog):
    caplog.set_level(logging.INFO)
    group_id = await setup.ensure_roster_group(api, USER_ID, CONFIG)
    assert group_id == ROSTER_GROUP_ID
    profile = api.new_groups[0]
    assert profile["displayName"] == "Invite roster"
    assert profile["groupPreferences"]["directMessages"] == {"enable": "on"}
    assert profile["groupPreferences"]["commands"] == commands.to_wire(commands.COMMANDS)
    assert api.group_custom_data == [(ROSTER_GROUP_ID, MARKER)]
    assert api.links == [ROSTER_GROUP_ID]  # created exactly once
    assert api.group_links[ROSTER_GROUP_ID] in caplog.text


async def test_finds_existing_group_by_marker(api):
    profile = {
        "displayName": "renamed by a human",
        "fullName": "",
        "groupPreferences": {
            "directMessages": {"enable": "on"},
            "commands": commands.to_wire(commands.COMMANDS),
        },
    }
    api.groups.append(make_group(77, profile, custom_data=MARKER))
    api.group_links[77] = "https://simplex.chat/contact#/?v=2&group=77"
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == 77
    assert api.new_groups == []  # not recreated
    assert api.profile_updates == []  # commands already match — no broadcast


async def test_found_group_with_link_logs_it_without_recreating(api, caplog):
    caplog.set_level(logging.INFO)
    profile = {
        "displayName": "Invite roster",
        "fullName": "",
        "groupPreferences": {
            "directMessages": {"enable": "on"},
            "commands": commands.to_wire(commands.COMMANDS),
        },
    }
    api.groups.append(make_group(77, profile, custom_data=MARKER))
    api.group_links[77] = "https://simplex.chat/contact#/?v=2&group=77"
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == 77
    assert api.links == []  # fetched, not recreated
    assert "https://simplex.chat/contact#/?v=2&group=77" in caplog.text


async def test_found_group_without_link_recreates_and_logs_it(api, caplog):
    # Crash window between marking the group and creating its link.
    caplog.set_level(logging.INFO)
    profile = {
        "displayName": "Invite roster",
        "fullName": "",
        "groupPreferences": {
            "directMessages": {"enable": "on"},
            "commands": commands.to_wire(commands.COMMANDS),
        },
    }
    api.groups.append(make_group(77, profile, custom_data=MARKER))
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == 77
    assert api.links == [77]  # recovered by creating a new link
    assert api.group_links[77] in caplog.text


async def test_group_link_get_failure_with_link_present_does_not_block_startup(api, caplog):
    # The create fallback hits the unique link index. A missing link must not
    # stop the bot starting.
    caplog.set_level(logging.WARNING)
    profile = {
        "displayName": "Invite roster",
        "fullName": "",
        "groupPreferences": {
            "directMessages": {"enable": "on"},
            "commands": commands.to_wire(commands.COMMANDS),
        },
    }
    api.groups.append(make_group(77, profile, custom_data=MARKER))
    api.group_links[77] = "https://simplex.chat/contact#/?v=2&group=77"
    api.fail_on.add("api_get_group_link_str")
    api.fail_on.add("api_create_group_link")
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == 77


async def test_pushes_commands_when_they_differ(api):
    profile = {
        "displayName": "Invite roster",
        "fullName": "",
        "groupPreferences": {"directMessages": {"enable": "on"}, "commands": []},
    }
    api.groups.append(make_group(77, profile, custom_data=MARKER))
    await setup.ensure_roster_group(api, USER_ID, CONFIG)
    assert len(api.profile_updates) == 1
    group_id, sent = api.profile_updates[0]
    assert group_id == 77
    assert sent["groupPreferences"]["commands"] == commands.to_wire(commands.COMMANDS)


async def test_pushed_profile_keeps_existing_display_name(api):
    profile = {
        "displayName": "renamed by a human",
        "fullName": "",
        "groupPreferences": {"directMessages": {"enable": "on"}, "commands": []},
    }
    api.groups.append(make_group(77, profile, custom_data=MARKER))
    await setup.ensure_roster_group(api, USER_ID, CONFIG)
    _, sent = api.profile_updates[0]
    # Syncing commands must not silently rename a group the operator renamed.
    assert sent["displayName"] == "renamed by a human"


async def test_ignores_groups_without_the_marker(api):
    api.groups.append(make_group(88, {"displayName": "Invite roster", "fullName": ""}))
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == ROSTER_GROUP_ID
    assert api.new_groups != []  # name match alone must not be trusted


async def test_ignores_groups_with_a_foreign_marker(api):
    api.groups.append(
        make_group(
            88, {"displayName": "x", "fullName": ""}, custom_data={"otherBot": {"group": "roster"}}
        )
    )
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == ROSTER_GROUP_ID
    assert api.new_groups != []


async def test_ignores_groups_with_the_wrong_marker_value(api):
    # Right namespace, wrong marker.
    api.groups.append(
        make_group(
            88,
            {"displayName": "x", "fullName": ""},
            custom_data={"supportBotLight": {"group": "archive"}},
        )
    )
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == ROSTER_GROUP_ID
    assert api.new_groups != []


async def test_ignores_groups_with_a_non_dict_marker(api):
    api.groups.append(
        make_group(
            88, {"displayName": "x", "fullName": ""}, custom_data={"supportBotLight": "roster"}
        )
    )
    assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == ROSTER_GROUP_ID


async def test_warns_and_picks_one_when_two_groups_are_marked(api, caplog):
    profile = {
        "displayName": "Invite roster",
        "fullName": "",
        "groupPreferences": {
            "directMessages": {"enable": "on"},
            "commands": commands.to_wire(commands.COMMANDS),
        },
    }
    api.groups += [
        make_group(20, profile, custom_data=MARKER),
        make_group(21, profile, custom_data=MARKER),
    ]
    api.group_links[20] = "https://simplex.chat/#g20"
    with caplog.at_level("WARNING"):
        assert await setup.ensure_roster_group(api, USER_ID, CONFIG) == 20
    assert "2 groups carry the roster marker" in caplog.text
    assert api.new_groups == []


async def test_a_group_the_bot_has_left_is_not_reused(api):
    # Nothing would ever be delivered there, and the marker would keep a
    # replacement from being created.
    api.groups.append(
        make_group(
            77,
            {"displayName": "old roster", "fullName": ""},
            {"supportBotLight": {"group": "roster"}},
            membership_status="removed",
        )
    )
    group_id = await setup.ensure_roster_group(api, USER_ID, CONFIG)
    assert group_id != 77
    assert api.new_groups  # a live roster group was created instead


async def test_direct_messages_is_restored_when_an_owner_switches_it_off(api):
    # api_create_member_contact fails without it, so /dm would fail forever with
    # nothing to explain it.
    profile = {
        "displayName": "Invite roster",
        "fullName": "",
        "groupPreferences": {
            "directMessages": {"enable": "off"},
            "commands": commands.to_wire(commands.COMMANDS),
        },
    }
    api.groups.append(make_group(77, profile, {"supportBotLight": {"group": "roster"}}))
    await setup.ensure_roster_group(api, USER_ID, CONFIG)
    pushed = api.profile_updates[-1][1]["groupPreferences"]
    assert pushed["directMessages"] == {"enable": "on"}
    assert pushed["commands"] == commands.to_wire(commands.COMMANDS)


async def test_a_profile_push_that_cannot_return_does_not_hang_startup(api, monkeypatch):
    # The core's view queue is bounded and nothing drains it until the bot
    # serves, so this write can block until it does.
    monkeypatch.setattr(setup, "PROFILE_PUSH_TIMEOUT", 0.05)

    async def never_returns(group_id, profile):
        await asyncio.sleep(10)

    monkeypatch.setattr(api, "api_update_group_profile", never_returns)
    api.groups.append(make_group(77, {"displayName": "r", "fullName": ""}, MARKER))
    group_id = await asyncio.wait_for(setup.ensure_roster_group(api, USER_ID, CONFIG), 2)
    assert group_id == 77
