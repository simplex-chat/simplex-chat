"""Constants and boundaries pinned at their exact edge.

Each of these was a surviving mutant: the value could be moved by one, or a
member of a set removed, with the whole suite still green.
"""

import pytest

from support_bot_light import commands, config, messages, roster, setup, text
from support_bot_light.config import ConfigError, load_config
from tests.conftest import make_contact, make_member
from tests.test_config import VALID, write


def entry(name: str, state: str = "active", reachable: bool = True) -> roster.RosterEntry:
    return roster.RosterEntry(
        contact_id=1, name=name, state=state, since="2026-08-13", reachable=reachable
    )


def test_the_roster_group_link_hands_out_the_member_role():
    # An owner could remove the bot from its own roster group.
    assert setup.JOIN_ROLE == "member"


def test_both_ready_statuses_make_a_contact_usable():
    # contactSndReady is a distinct event from contactConnected, and a member
    # promoted by one must not be treated as unreachable by the other.
    for status in ("ready", "sndReady"):
        assert roster.contact_usable(make_contact(1, "sh", conn_status=status)) is True
    assert roster.contact_usable(make_contact(1, "sh", conn_status="accepted")) is False


@pytest.mark.parametrize("status", ["deleted", "failed"])
def test_a_dead_connection_is_not_accepted_or_connecting(status):
    contact = make_contact(1, "sh", conn_status=status)
    contact["groupDirectInv"] = {"groupDirectInvLink": "x", "groupDirectInvStartedConnection": True}
    assert roster.accept_started(contact) is False
    assert roster.connecting(contact) is False


def test_a_member_of_the_roster_group_is_in_it_until_a_terminal_status():
    assert roster.in_group(make_member(1, status="pending_approval")) is True
    assert roster.in_group(make_member(1, status="invited")) is True
    assert roster.in_group(make_member(1, status="left")) is False


def test_list_shows_forty_before_it_summarises():
    # 40 keeps the reply inside the core's wire limit with room for two more
    # sections; the literal is the point, so a change has to be deliberate.
    assert messages.MAX_LISTED == 40
    at_cap = messages.render_roster([entry(f"n{i}") for i in range(40)])
    assert at_cap.count("•") == 40
    assert "more" not in at_cap

    over_cap = messages.render_roster([entry(f"n{i}") for i in range(41)])
    assert over_cap.count("•") == 40
    assert "… and 1 more" in over_cap


def test_a_reply_at_the_byte_cap_is_not_truncated():
    room = messages.MAX_REPLY_BYTES - len("On the roster (1):\n  • ") - len(" — since 2026-08-13")
    assert messages.render_roster([entry("a" * min(room, text.MAX_NAME))]).endswith("2026-08-13")

    over = [entry("漢" * text.MAX_NAME) for _ in range(messages.MAX_LISTED)]
    over += [entry("漢" * text.MAX_NAME, state="pending") for _ in range(messages.MAX_LISTED)]
    rendered = messages.render_roster(over)
    assert len(rendered.encode()) <= messages.MAX_REPLY_BYTES
    assert rendered.endswith(messages.TRUNCATED)


def test_a_name_of_fifty_is_kept_whole():
    # mkValidName caps a locally entered name at 50; inbound profiles are not
    # capped at all, which is why this exists.
    assert text.MAX_NAME == 50
    assert text.safe_name("a" * 50) == "a" * 50
    over = text.safe_name("a" * 51)
    assert len(over) == 50 and over.endswith("…")


def test_a_welcome_of_twelve_thousand_bytes_is_accepted(tmp_path):
    assert config.MAX_WELCOME_BYTES == 12000
    at_cap = "w" * 12000
    text_at = VALID.replace('welcome = "Hi! Someone will join shortly."', f'welcome = "{at_cap}"')
    assert load_config(write(tmp_path, text_at)).welcome == at_cap

    over = "w" * 12001
    text_over = VALID.replace('welcome = "Hi! Someone will join shortly."', f'welcome = "{over}"')
    with pytest.raises(ConfigError, match="too long"):
        load_config(write(tmp_path, text_over))


def test_an_image_of_9357_bytes_is_accepted(tmp_path):
    # 9357 raw bytes is what a 12500-character data URI holds once base64 and
    # the "data:image/png;base64," prefix are added. The pre-read check must
    # admit everything the encoded cap can hold, and no more.
    assert config.MAX_IMAGE_BYTES == 9357
    at_cap = tmp_path / "a.png"
    at_cap.write_bytes(b"\x89PNG" + b"x" * (9357 - 4))
    conf = VALID.replace("[roster]", f'image = "{at_cap}"\n\n[roster]')
    assert load_config(write(tmp_path, conf)).image is not None

    over = tmp_path / "b.png"
    over.write_bytes(b"\x89PNG" + b"x" * (9358 - 4))
    conf_over = VALID.replace("[roster]", f'image = "{over}"\n\n[roster]')
    with pytest.raises(ConfigError, match="too large"):
        load_config(write(tmp_path, conf_over))


@pytest.mark.parametrize("port", [1, 65535])
def test_the_port_range_ends_are_accepted(tmp_path, port):
    assert config.MAX_PORT == 65535
    conf = load_config(write(tmp_path, VALID + f"\n[health]\nport = {port}\n"))
    assert conf.health is not None and conf.health.port == port


def test_the_command_menu_carries_every_command():
    wire = commands.to_wire(commands.COMMANDS)
    assert [c["keyword"] for c in wire] == [
        commands.DM,
        commands.LIST,
        commands.LEAVE,
        commands.HELP,
    ]
