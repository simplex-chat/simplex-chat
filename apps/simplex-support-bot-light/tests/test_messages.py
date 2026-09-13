from support_bot_light import messages
from support_bot_light.roster import RosterEntry


def entry(name, state, since="2026-08-13T09:00:00+00:00", reachable=True):
    return RosterEntry(contact_id=1, name=name, state=state, since=since, reachable=reachable)


def test_render_roster_lists_active_and_pending():
    out = messages.render_roster([entry("sh", "active"), entry("Alex", "pending")])
    assert "On the roster (1):" in out
    assert "• sh — since 2026-08-13" in out
    assert "Contact request not accepted (1):" in out
    assert "• Alex — asked 2026-08-13" in out


def test_render_roster_empty():
    assert messages.render_roster([]) == messages.ROSTER_EMPTY


def test_render_roster_omits_pending_section_when_none():
    out = messages.render_roster([entry("sh", "active")])
    assert "Waiting" not in out


def test_render_roster_omits_date_suffix_when_since_is_empty():
    out = messages.render_roster([entry("sh", "active", since="")])
    assert out == "On the roster (1):\n  • sh"
    assert "since" not in out


def test_render_roster_formats_date_suffix():
    out = messages.render_roster([entry("sh", "active")])
    assert out == "On the roster (1):\n  • sh — since 2026-08-13"


def test_invite_log_lists_added_names():
    assert messages.invite_log("Alex", ["sh", "Narasimha"], []) == (
        "Connected: Alex → added sh, Narasimha"
    )


def test_invite_log_reports_failures():
    line = messages.invite_log("Alex", ["sh"], ["Narasimha"])
    assert line == "Connected: Alex → added sh (failed: Narasimha)"


def test_help_mentions_every_command():
    for keyword in ("dm", "list", "leave"):
        assert f"/{keyword}" in messages.HELP


def test_render_roster_separates_unreachable_members():
    out = messages.render_roster(
        [entry("live", "active"), entry("dead", "active", reachable=False)]
    )
    assert "On the roster (1):" in out
    assert "Not reachable, not being added (1):" in out
    assert "• dead" in out


def test_render_roster_caps_long_sections():
    entries = [entry(f"n{i}", "active") for i in range(messages.MAX_LISTED + 12)]
    out = messages.render_roster(entries)
    assert f"On the roster ({messages.MAX_LISTED + 12}):" in out
    assert "… and 12 more" in out
    assert out.count("•") == messages.MAX_LISTED
    assert len(out.encode()) < 15000


def test_render_roster_bounds_the_whole_reply_in_bytes():
    # Names are capped in characters, so CJK can overrun a byte limit even with
    # every section capped.
    entries = [entry("漢" * 50, "active") for _ in range(messages.MAX_LISTED)]
    entries += [entry("漢" * 50, "pending") for _ in range(messages.MAX_LISTED)]
    out = messages.render_roster(entries)
    assert len(out.encode()) <= messages.MAX_REPLY_BYTES
    assert out.endswith(messages.TRUNCATED)


def test_invite_log_is_bounded_in_bytes():
    names = ["漢" * 50 for _ in range(100)]
    out = messages.invite_log("Alex", names, [])
    assert len(out.encode()) <= messages.MAX_REPLY_BYTES
    assert out.endswith(messages.TRUNCATED)
