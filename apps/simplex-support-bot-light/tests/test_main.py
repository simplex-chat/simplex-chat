import pytest
from simplex_chat import Bot, BotProfile, SqliteDb
from simplex_chat.core import ChatAPIError

from support_bot_light import handlers, health
from support_bot_light.__main__ import (
    _register,
    _run,
    _serve,
    bot_profile,
    build_bot,
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
    for keywords, _predicate, handler in bot._command_handlers:
        await handler(make_group_message(api, make_member(1), f"/{keywords[0]}"), None)
    assert called == ["dm", "list_roster", "leave", "help_cmd"]


def test_a_taken_display_name_is_explained():
    # The core reports it as a bare errorStore; the cause is in the store error.
    e = ChatAPIError("chat command error: errorStore", {"storeError": {"type": "duplicateName"}})
    assert "bot.display_name" in startup_error(e)


def test_any_other_chat_error_keeps_its_detail():
    e = ChatAPIError("chat command error: errorStore", {"storeError": {"type": "userNotFound"}})
    assert "userNotFound" in startup_error(e)


def test_a_rejected_command_is_quoted_as_the_core_wrote_it():
    # The core puts what the caller did wrong in the message, and the tag says
    # nothing; printing the raw dict instead would bury it.
    e = ChatAPIError(
        "chat command error: error",
        {"type": "error", "errorType": {"type": "commandError", "message": "Profile image"}},
    )
    assert startup_error(e) == "Profile image"


def test_an_error_without_detail_is_rendered_plainly():
    assert startup_error(ValueError("no active user after start")) == "no active user after start"


def test_the_bot_opens_a_business_address():
    # Without these two the address yields direct chats that nothing handles:
    # acceptingBusinessRequest never fires and no roster is ever added.
    bot = build_bot(CONFIG)
    assert bot._business_address is True
    assert bot._auto_accept is True
    assert bot._welcome == "hi"


def test_the_bot_does_not_apply_its_profile_while_starting():
    # The name the core will accept is only knowable from the database, which
    # nothing can read until the client has started. _apply_profile does it.
    assert build_bot(CONFIG)._update_profile is False


class FakeBot:
    """A Bot stand-in for _serve: an async context manager with an api."""

    def __init__(self, api, sync_error: Exception | None = None):
        self.api = api
        self.profile = BotProfile(display_name="Support")
        self.served = 0
        self.syncs = 0
        self.sync_error = sync_error
        self.signal_handlers = 0
        self._command_handlers = []
        self._event_handlers = {}
        self.stop_requested = False
        self.stopped = False

    async def __aenter__(self):
        return self

    async def __aexit__(self, *_exc):
        return False

    def install_signal_handlers(self):
        self.signal_handlers += 1

    async def sync_profile(self) -> bool:
        self.syncs += 1
        if self.sync_error is not None:
            raise self.sync_error
        return True

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
    await _serve(CONFIG, bot)
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
    await _serve(CONFIG, bot)
    assert calls["n"] == 2  # one for discovery, one shared by both passes


async def test_serve_does_not_begin_serving_after_a_signal(api):
    bot = FakeBot(serve_api(api))
    bot.stop_requested = True
    await _serve(CONFIG, bot)
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
        await _serve(config, bot)
    finally:
        health.serve = original
    assert servers and not servers[0].is_serving()


async def test_the_bot_serves_after_a_refused_rename(api, caplog):
    # The core keeps display names unique; a refused one is not a reason to
    # leave customers unanswered.
    refused = ChatAPIError("x", {"storeError": {"type": "duplicateName"}})
    bot = FakeBot(serve_api(api), sync_error=refused)
    await _serve(CONFIG, bot)
    assert bot.served == 1
    assert "bot.display_name" in caplog.text


async def test_the_profile_is_applied_after_start(api, monkeypatch):
    bot = FakeBot(serve_api(api))
    await _serve(CONFIG, bot)
    assert bot.syncs == 1


async def test_run_installs_signal_handlers_before_starting(monkeypatch):
    # Startup runs migrations and address creation; a signal there would
    # otherwise kill the process mid-write.
    order: list[str] = []
    bot = FakeBot(None)

    def build(_config):
        return bot

    async def serve(_config, b):
        order.append(f"serve:{b.signal_handlers}")

    monkeypatch.setattr("support_bot_light.__main__.build_bot", build)
    monkeypatch.setattr("support_bot_light.__main__._serve", serve)
    await _run(CONFIG)
    assert order == ["serve:1"]
