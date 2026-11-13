import sys

import pytest
from simplex_chat import MigrationConfirmation
from simplex_chat.core import ChatAPIError, ChatInitError

from support_bot_light import dry_run as dr
from support_bot_light import roster, setup
from support_bot_light.__main__ import main
from support_bot_light.config import Config
from tests.conftest import FakeChatApi, make_group
from tests.test_config import VALID

CONFIG = Config("Support", "./x", "hi", "Invite roster", "owner")

UPGRADE = ChatInitError(
    "migration error",
    {
        "type": "errorMigration",
        "dbFile": "x",
        "migrationError": {
            "type": "upgrade",
            "upMigrations": [{"upName": "20261019_chat_layout", "withDown": True}],
        },
    },
)
DOWNGRADE = ChatInitError(
    "migration error",
    {
        "type": "errorMigration",
        "dbFile": "x",
        "migrationError": {"type": "downgrade", "downMigrations": ["20261111_feeds"]},
    },
)
DIFFERENT = ChatInitError(
    "migration error",
    {
        "type": "errorMigration",
        "dbFile": "x",
        "migrationError": {
            "type": "migrationError",
            "mtrError": {"type": "different", "appMigration": "a", "dbMigration": "b"},
        },
    },
)
NOT_A_DATABASE = ChatInitError(
    "Database or migration error (see db_migration_error)",
    {"type": "errorNotADatabase", "dbFile": "x"},
)


CHAT_NOT_STARTED = {"type": "error", "errorType": {"type": "chatNotStarted"}}


class DryRunApi(FakeChatApi):
    """The core with chat stopped: only chat previews list groups."""

    def __init__(self, user: dict | None = None):
        super().__init__()
        self.user = user
        self.closed = False

    async def api_get_active_user(self):
        return self.user

    async def api_list_groups(self, user_id: int, contact_id=None, search=None) -> list[dict]:
        raise ChatAPIError("chat command error", CHAT_NOT_STARTED)

    async def api_list_contacts(self, user_id: int) -> list[dict]:
        raise ChatAPIError("chat command error", CHAT_NOT_STARTED)

    async def api_get_chats(self, user_id: int, pagination: dict, query: dict) -> list[dict]:
        return [
            {"chatInfo": {"type": "group", "groupInfo": g}}
            for g in self.groups
            if g["groupProfile"]["displayName"] == query["search"]
        ]

    async def close(self) -> None:
        self.closed = True


USER = {"userId": 1, "profile": {"displayName": "Support"}}


def roster_group(group_id: int = 10, name: str = "Invite roster") -> dict:
    return make_group(
        group_id,
        {"displayName": name},
        {roster.NAMESPACE: {"group": setup.GROUP_MARKER}},
    )


def test_probe_reports_pending_and_newer_migrations_by_name():
    assert dr.probe_of(UPGRADE) == dr.Probe("upgrade", ("20261019_chat_layout",))
    assert dr.probe_of(DOWNGRADE) == dr.Probe("downgrade", ("20261111_feeds",))


def test_probe_reports_other_errors_as_failed_with_their_text():
    different = dr.probe_of(DIFFERENT)
    assert different.kind == "failed"
    assert different.text.startswith("migration state error: ")
    assert dr.probe_of(NOT_A_DATABASE) == dr.Probe(
        "failed", text='{"type": "errorNotADatabase", "dbFile": "x"}'
    )


def test_plan_for_a_current_schema_opens_without_migrating():
    plan = dr.migration_plan(dr.Probe("current"), False, False)
    assert plan == dr.Plan(MigrationConfirmation.ERROR, "schema up to date")


def test_plan_applies_pending_migrations_only_when_allowed():
    probe = dr.Probe("upgrade", ("a", "b"))
    assert dr.migration_plan(probe, False, True).confirm is None
    assert "--allow-migrations applies them now" in dr.migration_plan(probe, False, True).message
    assert dr.migration_plan(probe, True, False) == dr.Plan(
        MigrationConfirmation.YES_UP, "applied 2 migration(s): a, b"
    )


def test_plan_rolls_back_only_when_allowed():
    probe = dr.Probe("downgrade", ("20261111_feeds",))
    refused = dr.migration_plan(probe, True, False)
    assert refused.confirm is None
    assert "--allow-down-migrations rolls them back" in refused.message
    assert dr.migration_plan(probe, False, True) == dr.Plan(
        MigrationConfirmation.YES_UP_DOWN, "rolled back 1 migration(s): 20261111_feeds"
    )


def test_plan_for_a_failed_probe_stops_whatever_is_allowed():
    assert dr.migration_plan(dr.Probe("failed", text="boom"), True, True) == dr.Plan(None, "boom")


async def test_state_without_an_active_user_fails():
    report = dr.Report()
    await dr.check_state(DryRunApi(), CONFIG, report)
    assert report.failed
    assert report.lines == ["FAIL  no active user in database, a real start would create one"]


async def test_state_finds_the_marked_roster_group_by_name_with_chat_stopped():
    api = DryRunApi(USER)
    api.groups = [make_group(9, {"displayName": "Invite roster"}), roster_group()]
    report = dr.Report()
    await dr.check_state(api, CONFIG, report)
    assert not report.failed
    assert report.lines == [
        "ok    active user: 1:Support",
        "ok    roster group 10: Invite roster",
    ]


@pytest.mark.parametrize(
    "groups",
    [[], [roster_group(name="Renamed roster")], [make_group(9, {"displayName": "Invite roster"})]],
)
async def test_state_without_a_marked_group_of_that_name_is_unverified(groups):
    api = DryRunApi(USER)
    api.groups = groups
    report = dr.Report()
    await dr.check_state(api, CONFIG, report)
    assert not report.failed
    assert report.lines[1] == (
        '?     no roster group named "Invite roster" in chat previews; '
        "the bot finds a renamed one by its marker at startup, or creates it"
    )


def fake_init(monkeypatch, probe_error: ChatInitError | None, api: DryRunApi):
    calls: list[MigrationConfirmation] = []

    async def init(_db, confirm):
        calls.append(confirm)
        if confirm == MigrationConfirmation.ERROR and probe_error is not None:
            raise probe_error
        return api

    monkeypatch.setattr(dr.ChatApi, "init", init)
    return calls


async def test_dry_run_rolls_back_with_yes_up_down_and_checks_after(monkeypatch):
    api = DryRunApi(USER)
    api.groups = [roster_group()]
    calls = fake_init(monkeypatch, DOWNGRADE, api)
    report = await dr.dry_run(CONFIG, False, True)
    assert calls == [MigrationConfirmation.ERROR, MigrationConfirmation.YES_UP_DOWN]
    assert report.lines[0] == "ok    database opened, rolled back 1 migration(s): 20261111_feeds"
    assert not report.failed
    assert api.closed


async def test_dry_run_reports_pending_migrations_without_opening_again(monkeypatch):
    calls = fake_init(monkeypatch, UPGRADE, DryRunApi(USER))
    report = await dr.dry_run(CONFIG, False, False)
    assert calls == [MigrationConfirmation.ERROR]
    assert report.failed
    assert report.lines[0].startswith(
        "FAIL  database: pending 1 migration(s): 20261019_chat_layout"
    )


async def test_dry_run_on_a_current_schema_closes_both_opens(monkeypatch):
    api = DryRunApi(USER)
    api.groups = [roster_group()]
    calls = fake_init(monkeypatch, None, api)
    report = await dr.dry_run(CONFIG, False, False)
    assert calls == [MigrationConfirmation.ERROR, MigrationConfirmation.ERROR]
    assert report.lines[0] == "ok    database opened, schema up to date"
    assert api.closed


@pytest.mark.parametrize("flag", ["--allow-migrations", "--allow-down-migrations"])
def test_migration_flags_require_dry_run(monkeypatch, flag):
    monkeypatch.setattr(sys, "argv", ["support-bot-light", flag])
    with pytest.raises(SystemExit) as exc:
        main()
    assert exc.value.code == 2


@pytest.mark.parametrize("failed,code", [(False, 0), (True, 1)])
def test_main_prints_the_report_and_exits_by_its_result(
    tmp_path, monkeypatch, capsys, failed, code
):
    (tmp_path / "config.toml").write_text(VALID, encoding="utf-8")
    seen: list[tuple[bool, bool]] = []

    async def fake_dry_run(_config, allow_up, allow_down):
        seen.append((allow_up, allow_down))
        return dr.Report(["ok    database opened, schema up to date"], failed)

    monkeypatch.setattr("support_bot_light.__main__.dry_run", fake_dry_run)
    monkeypatch.setattr("support_bot_light.__main__.os.umask", lambda _mask: 0)
    argv = ["support-bot-light", "--config", str(tmp_path / "config.toml"), "--dry-run"]
    monkeypatch.setattr(sys, "argv", [*argv, "--allow-down-migrations"])
    assert main() == code
    assert seen == [(False, True)]
    assert capsys.readouterr().out == "ok    database opened, schema up to date\n"
