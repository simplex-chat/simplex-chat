import asyncio
import os
import signal
from types import SimpleNamespace

import pytest
from simplex_chat import Bot, BotProfile, SqliteDb
from simplex_chat.core import ChatAPIError

from support_bot_light import handlers, health
from support_bot_light.__main__ import (
    _install_signal_handlers,
    _register,
    _run,
    _serve,
    _usable_name,
    bot_profile,
    build_bot,
    name_taken,
    startup_error,
)
from support_bot_light.config import Config, Health
from support_bot_light.context import BotContext
from tests.conftest import (
    ROSTER_GROUP_ID,
    USER_ID,
    make_group,
    make_group_message,
    make_member,
)

CONFIG = Config("Support", "./x", "hi", "Invite roster", "owner")
OTHER_GROUP_ID = 99


def plain_bot() -> Bot:
    return Bot(
        profile=BotProfile(display_name="Support"),
        db=SqliteDb(file_prefix="./unused"),
        welcome="hi",
    )


def registered(api) -> Bot:
    bot = plain_bot()
    ctx = BotContext(api=api, user_id=USER_ID, roster_group_id=ROSTER_GROUP_ID, config=CONFIG)
    _register(bot, ctx)
    return bot


def test_registers_all_four_commands(api):
    bot = registered(api)
    keywords = [names for names, _predicate, _handler in bot._command_handlers]
    assert keywords == [("dm",), ("list",), ("leave",), ("help",)]


def test_registers_connection_and_business_events(api):
    bot = registered(api)
    assert set(bot._event_handlers) == {
        "acceptingBusinessRequest",
        "contactConnected",
        "contactSndReady",
        "deletedMember",
        "leftMember",
    }


def test_commands_match_in_the_roster_group(api):
    bot = registered(api)
    msg = make_group_message(api, make_member(1), "/dm", group_id=ROSTER_GROUP_ID)
    _names, predicate, _handler = bot._command_handlers[0]
    assert predicate(msg) is True


def test_commands_do_not_match_in_other_groups(api):
    # A /dm typed inside a business chat must not be acted on.
    bot = registered(api)
    msg = make_group_message(api, make_member(1), "/dm", group_id=OTHER_GROUP_ID)
    _names, predicate, _handler = bot._command_handlers[0]
    assert predicate(msg) is False


def test_bot_profile_carries_display_name_and_image():
    profile = bot_profile(
        Config("Support", "./x", "hi", "R", "owner", image="data:image/png;base64,AAA")
    )
    assert profile.display_name == "Support"
    assert profile.image == "data:image/png;base64,AAA"


def test_bot_profile_without_image():
    assert bot_profile(CONFIG).image is None


@pytest.mark.parametrize("index,keyword", [(0, "dm"), (1, "list"), (2, "leave"), (3, "help")])
def test_every_command_is_scoped_to_the_roster_group(api, index, keyword):
    # A /list answered in a business chat would show the roster to a customer.
    bot = registered(api)
    names, predicate, _handler = bot._command_handlers[index]
    assert names == (keyword,)
    inside = make_group_message(api, make_member(1), f"/{keyword}", group_id=ROSTER_GROUP_ID)
    outside = make_group_message(api, make_member(1), f"/{keyword}", group_id=OTHER_GROUP_ID)
    assert predicate(inside) is True
    assert predicate(outside) is False


async def test_registered_handlers_call_the_matching_handler(api, monkeypatch):
    # Registration bookkeeping alone would not catch /dm being wired to leave().
    bot = registered(api)
    called: list[str] = []

    def spy(name):
        async def handler(_ctx, _msg):
            called.append(name)

        return handler

    for name in ("dm", "list_roster", "leave", "help_cmd"):
        monkeypatch.setattr(handlers, name, spy(name))
    for names, _predicate, handler in bot._command_handlers:
        await handler(make_group_message(api, make_member(1), f"/{names[0]}"), None)
    assert called == ["dm", "list_roster", "leave", "help_cmd"]


class StopSpy:
    """A Bot stand-in that records stop() calls."""

    def __init__(self):
        self.stops = 0

    def stop(self):
        self.stops += 1


async def stopper_for() -> tuple:
    """The SIGINT and SIGTERM callbacks, plus the Stopper they belong to."""
    installed = {}
    loop = asyncio.get_running_loop()
    original = loop.add_signal_handler
    loop.add_signal_handler = lambda sig, cb: installed.__setitem__(sig, cb)
    try:
        stopper = _install_signal_handlers()
    finally:
        loop.add_signal_handler = original
    return installed[signal.SIGINT], installed[signal.SIGTERM], stopper


async def test_no_signal_means_no_stop_requested():
    _interrupt, _terminate, stopper = await stopper_for()
    assert stopper.requested is False


async def test_a_signal_before_any_client_is_started_is_not_lost():
    # The first client start runs migrations and address creation; a signal
    # there must not hit the default disposition.
    _interrupt, terminate, stopper = await stopper_for()
    terminate()
    assert stopper.requested is True

    bot = StopSpy()
    stopper.attach(bot)
    assert bot.stops == 0  # nothing to stop when the signal arrived


async def test_sigterm_asks_the_attached_bot_to_stop():
    _interrupt, terminate, stopper = await stopper_for()
    bot = StopSpy()
    stopper.attach(bot)
    terminate()
    assert bot.stops == 1 and stopper.requested is True


async def test_a_replacement_bot_receives_the_stop():
    # The name probe and the serving bot are different clients.
    _interrupt, terminate, stopper = await stopper_for()
    probe, serving = StopSpy(), StopSpy()
    stopper.attach(probe)
    stopper.attach(serving)
    terminate()
    assert (probe.stops, serving.stops) == (0, 1)


async def test_first_sigint_stops_gracefully_and_the_second_forces_exit(monkeypatch):
    interrupt, _terminate, stopper = await stopper_for()
    bot = StopSpy()
    stopper.attach(bot)
    interrupt()
    assert bot.stops == 1 and stopper.requested is True

    exits: list[int] = []
    monkeypatch.setattr(os, "_exit", exits.append)
    interrupt()
    assert exits == [130] and bot.stops == 1


def test_a_taken_display_name_is_explained():
    # The core reports it as a bare errorStore; the cause is in chat_error.
    e = ChatAPIError("chat command error: errorStore", {"storeError": {"type": "duplicateName"}})
    assert "bot.display_name" in startup_error(e)


def test_any_other_chat_error_keeps_its_detail():
    e = ChatAPIError("chat command error: errorStore", {"storeError": {"type": "userNotFound"}})
    assert "userNotFound" in startup_error(e)


def test_an_error_without_detail_is_rendered_plainly():
    assert startup_error(ValueError("no active user after start")) == "no active user after start"


def test_the_bot_opens_a_business_address():
    # Without these two the address yields direct chats that nothing handles:
    # acceptingBusinessRequest never fires and no roster is ever added.
    bot = build_bot(CONFIG)
    assert bot._business_address is True
    assert bot._auto_accept is True
    assert bot._welcome == "hi"


TAKEN = ChatAPIError("chat command error: errorStore", {"storeError": {"type": "duplicateName"}})


async def fake_usable_name(config):
    return config.display_name


def test_name_taken_only_matches_the_core_s_duplicate_name():
    assert name_taken(TAKEN) is True
    assert name_taken(ChatAPIError("x", {"storeError": {"type": "userNotFound"}})) is False
    assert name_taken(ValueError("no active user after start")) is False


def run_with(monkeypatch, *, serve, requested=False, built=None):
    """Drive _run with the client starts replaced."""

    def fake_build(config, *, update_profile=True, display_name=None):
        if built is not None:
            built.append(update_profile)
        return update_profile

    class FakeStopper:
        requested = False

        def attach(self, bot):
            return bot

    stopper = FakeStopper()
    stopper.requested = requested

    async def fake_usable_name(config, _stopper):
        return config.display_name

    monkeypatch.setattr("support_bot_light.__main__.build_bot", fake_build)
    monkeypatch.setattr("support_bot_light.__main__._serve", serve)
    monkeypatch.setattr("support_bot_light.__main__._usable_name", fake_usable_name)
    monkeypatch.setattr("support_bot_light.__main__._install_signal_handlers", lambda: stopper)
    return stopper


def test_the_bot_serves_without_the_profile_when_the_name_is_refused(monkeypatch):
    # The rename half-lands in the core and cannot be undone from here, so the
    # choice is between answering customers and applying a name.
    built: list[bool] = []
    served: list[bool] = []

    async def fake_serve(config, bot, stopper):
        served.append(bot)
        if bot is True:  # the attempt that applies the profile
            raise TAKEN

    run_with(monkeypatch, serve=fake_serve, built=built)
    asyncio.run(_run(CONFIG))
    assert built == [True, False]
    assert served == [True, False]


def test_a_signal_during_the_refused_attempt_is_not_retried(monkeypatch):
    served: list[bool] = []

    async def fake_serve(config, bot, stopper):
        served.append(bot)
        raise TAKEN

    run_with(monkeypatch, serve=fake_serve, requested=True)
    asyncio.run(_run(CONFIG))
    assert served == []  # the signal arrived before the first serve


def test_any_other_startup_error_still_stops_the_bot(monkeypatch):
    async def fake_serve(config, bot, stopper):
        raise ChatAPIError("x", {"storeError": {"type": "userNotFound"}})

    run_with(monkeypatch, serve=fake_serve)
    with pytest.raises(ChatAPIError):
        asyncio.run(_run(CONFIG))


def test_a_signal_during_the_name_probe_stops_before_serving(monkeypatch):
    served: list[bool] = []

    async def fake_serve(config, bot, stopper):
        served.append(bot)

    run_with(monkeypatch, serve=fake_serve, requested=True)
    asyncio.run(_run(CONFIG))
    assert served == []


class FakeBot:
    """A Bot stand-in for _serve: an async context manager with an api."""

    def __init__(self, api):
        self.api = api
        self.served = 0
        self._command_handlers = []
        self._event_handlers = {}
        self.stopped = False

    async def __aenter__(self):
        return self

    async def __aexit__(self, *_exc):
        return False

    def on_command(self, *_names, **_kw):
        def register(handler):
            self._command_handlers.append(handler)
            return handler

        return register

    def on_event(self, tag):
        def register(handler):
            self._event_handlers.setdefault(tag, []).append(handler)
            return handler

        return register

    async def serve_forever(self):
        self.served += 1

    def stop(self):
        self.stopped = True


class RunStopper:
    requested = False

    def attach(self, bot):
        return bot


def serve_api(api):
    """The fake api with the calls _serve makes before serving."""

    async def api_get_active_user():
        return {"userId": USER_ID, "localDisplayName": "Support"}

    api.api_get_active_user = api_get_active_user
    api.group_links[ROSTER_GROUP_ID] = "https://example.invalid/g#x"
    api.groups.append(
        make_group(
            ROSTER_GROUP_ID,
            {"displayName": "Invite roster", "fullName": ""},
            {"supportBotLight": {"group": "roster"}},
        )
    )
    return api


async def test_serve_wires_the_handlers_and_serves(api):
    bot = FakeBot(serve_api(api))
    await _serve(CONFIG, bot, RunStopper())
    assert bot.served == 1
    assert len(bot._command_handlers) == 4  # nothing is delivered without these
    assert set(bot._event_handlers) == {
        "acceptingBusinessRequest",
        "contactConnected",
        "contactSndReady",
        "deletedMember",
        "leftMember",
    }


async def test_serve_reads_the_group_listing_once(api):
    # It is the largest thing startup marshals and grows with every customer.
    bot = FakeBot(serve_api(api))
    calls = {"n": 0}
    original = api.api_list_groups

    async def counted(user_id, **kw):
        calls["n"] += 1
        return await original(user_id, **kw)

    api.api_list_groups = counted
    await _serve(CONFIG, bot, RunStopper())
    assert calls["n"] == 2  # one for discovery, one shared by both passes


async def test_serve_does_not_begin_serving_after_a_signal(api):
    stopper = RunStopper()
    stopper.requested = True
    bot = FakeBot(serve_api(api))
    await _serve(CONFIG, bot, stopper)
    assert bot.served == 0


async def test_serve_closes_the_health_endpoint_afterwards(api):
    config = Config("Support", "./x", "hi", "Invite roster", "owner", health=Health("127.0.0.1", 0))
    bot = FakeBot(serve_api(api))
    servers: list = []
    original = health.serve

    async def spy(ctx, cfg):
        server = await original(ctx, cfg)
        servers.append(server)
        return server

    health.serve = spy
    try:
        await _serve(config, bot, RunStopper())
    finally:
        health.serve = original
    assert servers and not servers[0].is_serving()


def test_the_name_probe_never_applies_the_profile(monkeypatch):
    # Applying it is the write that can half-commit, which is what the probe
    # exists to decide about.
    kwargs: list[dict] = []

    class Probe:
        api = None

        async def __aenter__(self):
            return self

        async def __aexit__(self, *_exc):
            return False

    def fake_build(config, **kw):
        kwargs.append(kw)
        return Probe()

    async def fake_active_user():
        return None

    Probe.api = SimpleNamespace(api_get_active_user=fake_active_user)
    monkeypatch.setattr("support_bot_light.__main__.build_bot", fake_build)

    class Stopper:
        def attach(self, bot):
            return bot

    assert asyncio.run(_usable_name(CONFIG, Stopper())) == CONFIG.display_name
    assert kwargs == [{"update_profile": False}]
