import pytest

from simplex_chat import Bot, BotCommand, BotProfile, Client, Middleware, Profile, SqliteDb
from simplex_chat.api import ChatApi


def _bot() -> Bot:
    return Bot(profile=BotProfile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))


def test_decorator_registers_message_handler():
    bot = _bot()

    @bot.on_message(content_type="text")
    async def h(msg):
        pass

    assert len(bot._message_handlers) == 1


def test_decorator_registers_command_handler():
    bot = _bot()

    @bot.on_command("ping")
    async def h(msg, cmd):
        pass

    assert len(bot._command_handlers) == 1
    assert bot._command_handlers[0][0] == ("ping",)


def test_decorator_registers_event_handler():
    bot = _bot()

    @bot.on_event("newChatItems")
    async def h(evt):
        pass

    assert "newChatItems" in bot._event_handlers
    assert len(bot._event_handlers["newChatItems"]) == 1


def test_api_property_raises_before_init():
    bot = _bot()
    with pytest.raises(RuntimeError, match="not initialized"):
        _ = bot.api


def test_command_keyword_tuple():
    bot = _bot()

    @bot.on_command(("p", "ping"))
    async def h(msg, cmd):
        pass

    assert bot._command_handlers[0][0] == ("p", "ping")


def test_bot_profile_to_wire_default():
    """Bot's profile wire-form sets peerType=bot and disables calls/voice."""
    bot = _bot()
    p = bot._profile_to_wire()
    assert p["displayName"] == "x"
    assert p.get("peerType") == "bot"
    prefs = p.get("preferences") or {}
    assert prefs.get("calls", {}).get("allow") == "no"
    assert prefs.get("voice", {}).get("allow") == "no"
    assert prefs.get("files", {}).get("allow") == "no"  # allow_files defaults to False


def test_bot_profile_to_wire_allow_files():
    bot = Bot(
        profile=BotProfile(display_name="x"),
        db=SqliteDb(file_prefix="/tmp/test"),
        allow_files=True,
    )
    prefs = bot._profile_to_wire().get("preferences") or {}
    assert prefs.get("files", {}).get("allow") == "yes"


def test_bot_profile_to_wire_with_commands():
    bot = Bot(
        profile=BotProfile(display_name="x"),
        db=SqliteDb(file_prefix="/tmp/test"),
        commands=[BotCommand(keyword="ping", label="Ping bot"), BotCommand("help", "Show help")],
    )
    cmds = bot._profile_to_wire().get("preferences", {}).get("commands") or []
    assert len(cmds) == 2
    assert cmds[0] == {"type": "command", "keyword": "ping", "label": "Ping bot"}
    assert cmds[1] == {"type": "command", "keyword": "help", "label": "Show help"}


def test_client_profile_to_wire_has_no_bot_extras():
    """Client's wire profile has no peerType=bot, no command list, no calls/voice prefs.
    That's the whole point of having Client as a separate class."""
    c = Client(profile=Profile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))
    p = c._profile_to_wire()
    assert p["displayName"] == "x"
    assert "peerType" not in p
    assert "preferences" not in p


def test_bot_profile_alias_is_profile():
    """`BotProfile` is kept as an alias for backwards compatibility."""
    assert BotProfile is Profile
    assert BotProfile(display_name="x") == Profile(display_name="x")


def test_dispatch_message_first_match_wins():
    """Two matching message handlers — only the first registered fires."""
    import asyncio
    import re

    bot = _bot()
    calls: list[str] = []

    @bot.on_message(content_type="text", text=re.compile(r"^\d+$"))
    async def number(_msg):
        calls.append("number")

    @bot.on_message(content_type="text")
    async def fallback(_msg):
        calls.append("fallback")

    class M:
        pass

    m = M()
    m.content = {"type": "text", "text": "42"}
    m.chat_item = {
        "chatItem": {
            "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "42"}}
        },
        "chatInfo": {"type": "direct"},
    }
    m.text = "42"

    asyncio.run(bot._dispatch_message(m))  # type: ignore[arg-type]
    assert calls == ["number"], f"expected only 'number' for '42', got {calls}"


def test_dispatch_message_falls_to_second_when_first_doesnt_match():
    """If the first handler's filter doesn't match, the second one fires."""
    import asyncio
    import re

    bot = _bot()
    calls: list[str] = []

    @bot.on_message(content_type="text", text=re.compile(r"^\d+$"))
    async def number(_msg):
        calls.append("number")

    @bot.on_message(content_type="text")
    async def fallback(_msg):
        calls.append("fallback")

    class M:
        pass

    m = M()
    m.content = {"type": "text", "text": "hello"}
    m.chat_item = {
        "chatItem": {
            "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "hello"}}
        },
        "chatInfo": {"type": "direct"},
    }
    m.text = "hello"

    asyncio.run(bot._dispatch_message(m))  # type: ignore[arg-type]
    assert calls == ["fallback"], f"expected 'fallback' for 'hello', got {calls}"


def test_register_log_handlers_idempotent():
    """Calling _register_log_handlers twice doesn't duplicate handlers."""
    bot = Bot(
        profile=BotProfile(display_name="x"),
        db=SqliteDb(file_prefix="/tmp/test"),
        log_contacts=True,
        log_network=True,
    )
    bot._register_log_handlers()
    counts1 = {tag: len(hs) for tag, hs in bot._event_handlers.items()}
    bot._register_log_handlers()
    counts2 = {tag: len(hs) for tag, hs in bot._event_handlers.items()}
    assert counts1 == counts2, f"handler count changed across calls: {counts1} -> {counts2}"


def test_default_error_handlers_always_registered():
    """messageError/chatError/chatErrors get default loggers regardless of opts."""
    bot = Bot(
        profile=BotProfile(display_name="x"),
        db=SqliteDb(file_prefix="/tmp/test"),
        log_contacts=False,
        log_network=False,
    )
    bot._register_log_handlers()
    assert "messageError" in bot._event_handlers
    assert "chatError" in bot._event_handlers
    assert "chatErrors" in bot._event_handlers


def test_dispatch_command_suppresses_matching_message_handlers():
    """A `/help` message routed to a command handler must NOT also fire the
    generic on_message text handler."""
    import asyncio

    bot = _bot()
    calls: list[str] = []

    @bot.on_message(content_type="text")
    async def fallback(_msg):
        calls.append("message")

    @bot.on_command("help")
    async def help_cmd(_msg, _cmd):
        calls.append("command")

    # Build a minimal Message-shaped object (handlers only inspect chat_item / text).
    class M:
        pass

    m = M()
    m.content = {"type": "text", "text": "/help"}
    m.chat_item = {
        "chatItem": {
            "content": {
                "type": "rcvMsgContent",
                "msgContent": {"type": "text", "text": "/help"},
            }
        },
        "chatInfo": {"type": "direct"},
    }
    m.text = "/help"

    asyncio.run(bot._dispatch_message(m))  # type: ignore[arg-type]
    assert calls == ["command"], f"expected only 'command' to fire for /help, got {calls}"


def test_dispatch_unknown_command_falls_through_to_message_handlers():
    """A `/unknown` slash-command with no handler should still fire on_message."""
    import asyncio

    bot = _bot()
    calls: list[str] = []

    @bot.on_message(content_type="text")
    async def fallback(_msg):
        calls.append("message")

    @bot.on_command("help")
    async def help_cmd(_msg, _cmd):
        calls.append("command")

    class M:
        pass

    m = M()
    m.content = {"type": "text", "text": "/unknown"}
    m.chat_item = {
        "chatItem": {
            "content": {
                "type": "rcvMsgContent",
                "msgContent": {"type": "text", "text": "/unknown"},
            }
        },
        "chatInfo": {"type": "direct"},
    }
    m.text = "/unknown"

    asyncio.run(bot._dispatch_message(m))  # type: ignore[arg-type]
    assert calls == ["message"], f"expected message fallback to fire for /unknown, got {calls}"


def test_chat_api_status_properties():
    """`initialized` and `started` reflect lifecycle state without invoking the FFI."""
    api = ChatApi(ctrl=12345)
    assert api.initialized is True
    assert api.started is False
    assert api.ctrl == 12345
    # Simulate close: ctrl wiped, both properties false.
    api._ctrl = None
    api._started = False
    assert api.initialized is False
    assert api.started is False
    with pytest.raises(RuntimeError, match="not initialized"):
        _ = api.ctrl


def test_log_contacts_registers_handlers():
    bot = Bot(
        profile=BotProfile(display_name="x"),
        db=SqliteDb(file_prefix="/tmp/test"),
        log_contacts=True,
        log_network=False,
    )
    bot._register_log_handlers()
    assert "contactConnected" in bot._event_handlers
    assert "contactDeletedByContact" in bot._event_handlers
    assert "hostConnected" not in bot._event_handlers


def test_log_network_registers_handlers():
    bot = Bot(
        profile=BotProfile(display_name="x"),
        db=SqliteDb(file_prefix="/tmp/test"),
        log_contacts=False,
        log_network=True,
    )
    bot._register_log_handlers()
    assert "hostConnected" in bot._event_handlers
    assert "hostDisconnected" in bot._event_handlers
    assert "subscriptionStatus" in bot._event_handlers
    assert "contactConnected" not in bot._event_handlers


def test_middleware_registration_and_invocation_order():
    """Middleware registered first wraps middleware registered later (outer first)."""
    bot = _bot()
    calls: list[str] = []

    class Outer(Middleware):
        async def __call__(self, handler, message, data):
            calls.append("outer-before")
            await handler(message, data)
            calls.append("outer-after")

    class Inner(Middleware):
        async def __call__(self, handler, message, data):
            calls.append("inner-before")
            await handler(message, data)
            calls.append("inner-after")

    bot.use(Outer())
    bot.use(Inner())
    assert len(bot._middleware) == 2

    async def handler(msg):
        calls.append("handler")

    import asyncio

    asyncio.run(bot._invoke_with_middleware(handler, message=object()))  # type: ignore[arg-type]
    assert calls == [
        "outer-before",
        "inner-before",
        "handler",
        "inner-after",
        "outer-after",
    ]
