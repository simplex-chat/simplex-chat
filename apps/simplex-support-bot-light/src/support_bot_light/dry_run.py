"""Check the database, migrations and roster group that the next start would use, then exit."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from typing import Literal

from simplex_chat import ChatApi, MigrationConfirmation, SqliteDb
from simplex_chat.core import ChatInitError
from simplex_chat.types import T

from . import setup
from .config import Config

ProbeKind = Literal["current", "upgrade", "downgrade", "failed"]

# Chat previews are the group list the core serves with chat stopped.
SEARCH_LIMIT = 100


@dataclass(frozen=True, slots=True)
class Probe:
    kind: ProbeKind
    names: tuple[str, ...] = ()
    text: str = ""


@dataclass(frozen=True, slots=True)
class Plan:
    confirm: MigrationConfirmation | None
    message: str


@dataclass(slots=True)
class Report:
    lines: list[str] = field(default_factory=list)
    failed: bool = False

    def ok(self, text: str) -> None:
        self.lines.append(f"ok    {text}")

    def fail(self, text: str) -> None:
        self.failed = True
        self.lines.append(f"FAIL  {text}")

    def unverified(self, text: str) -> None:
        self.lines.append(f"?     {text}")


def migration_error_text(e: ChatInitError) -> str:
    err = e.db_migration_error
    if err.get("type") == "errorMigration":
        me = err.get("migrationError", {})
        if me.get("type") == "upgrade":
            names = [m["upName"] for m in me.get("upMigrations", [])]
            return f"{len(names)} migration(s) pending: {', '.join(names)}"
        if me.get("type") == "downgrade":
            names = me.get("downMigrations", [])
            return f"database is newer than this build, to roll back: {', '.join(names)}"
        return f"migration state error: {json.dumps(me)}"
    return str(e)


def probe_of(e: ChatInitError) -> Probe:
    err = e.db_migration_error
    me = err.get("migrationError", {}) if err.get("type") == "errorMigration" else {}
    if me.get("type") == "upgrade":
        return Probe("upgrade", tuple(m["upName"] for m in me.get("upMigrations", [])))
    if me.get("type") == "downgrade":
        return Probe("downgrade", tuple(me.get("downMigrations", [])))
    return Probe("failed", text=migration_error_text(e))


def migration_plan(probe: Probe, allow_up: bool, allow_down: bool) -> Plan:
    listed = f"{len(probe.names)} migration(s): {', '.join(probe.names)}"
    if probe.kind == "current":
        return Plan(MigrationConfirmation.ERROR, "schema up to date")
    if probe.kind == "upgrade":
        if allow_up:
            return Plan(MigrationConfirmation.YES_UP, f"applied {listed}")
        return Plan(
            None,
            f"pending {listed}; a real start would apply them, --allow-migrations applies them now",
        )
    if probe.kind == "downgrade":
        if allow_down:
            return Plan(MigrationConfirmation.YES_UP_DOWN, f"rolled back {listed}")
        return Plan(
            None,
            f"database is newer than this build, to roll back {listed}; "
            "a real start would refuse, --allow-down-migrations rolls them back",
        )
    return Plan(None, probe.text)


async def probe_database(db: SqliteDb) -> Probe:
    try:
        api = await ChatApi.init(db, MigrationConfirmation.ERROR)
    except ChatInitError as e:
        return probe_of(e)
    await api.close()
    return Probe("current")


async def check_state(api: ChatApi, config: Config, report: Report) -> None:
    user = await api.api_get_active_user()
    if user is None:
        report.fail("no active user in database, a real start would create one")
        return
    user_id = user["userId"]
    report.ok(f"active user: {user_id}:{user['profile']['displayName']}")

    search: T.ChatListQuery = {"type": "search", "search": config.group_name}
    chats = await api.api_get_chats(user_id, {"type": "last", "count": SEARCH_LIMIT}, search)
    groups = [c["chatInfo"]["groupInfo"] for c in chats if c["chatInfo"]["type"] == "group"]
    marked = [g for g in groups if setup.is_roster_group(g)]
    if marked:
        group = marked[0]
        report.ok(f"roster group {group['groupId']}: {group['groupProfile']['displayName']}")
    else:
        report.unverified(
            f'no roster group named "{config.group_name}" in chat previews; '
            "the bot finds a renamed one by its marker at startup, or creates it"
        )


async def dry_run(config: Config, allow_up: bool, allow_down: bool) -> Report:
    report = Report()
    db = SqliteDb(file_prefix=config.db_prefix)
    plan = migration_plan(await probe_database(db), allow_up, allow_down)
    if plan.confirm is None:
        report.fail(f"database: {plan.message}")
        return report
    try:
        api = await ChatApi.init(db, plan.confirm)
    except ChatInitError as e:
        report.fail(f"database: {migration_error_text(e)}")
        return report
    report.ok(f"database opened, {plan.message}")
    try:
        await check_state(api, config, report)
    finally:
        await api.close()
    return report
