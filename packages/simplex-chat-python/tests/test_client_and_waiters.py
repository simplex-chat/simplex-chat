"""Tests for Client class + connect_to / send_and_wait / events plumbing.

Stubs out ChatApi so we exercise the dispatch and waiter logic without
spinning up the native libsimplex controller.
"""

from __future__ import annotations

import asyncio
from typing import Any

import pytest

from simplex_chat import (
    Bot,
    BotProfile,
    Client,
    ContactAlreadyExistsError,
    Profile,
    SqliteDb,
)


class FakeApi:
    """Drop-in replacement for ChatApi for tests that don't need the FFI.

    Records api_send_text_message calls; supports scripting api_connect_plan
    and api_connect_active_user behaviour.
    """

    def __init__(self) -> None:
        self.sent: list[tuple[Any, str]] = []
        self.connect_plan_result: Any = ("error", None)  # default: no known contact
        self.connect_should_raise: Exception | None = None
        self.active_user: dict[str, Any] = {"userId": 1, "profile": {"displayName": "x"}}

    async def api_send_text_message(self, chat, text, in_reply_to=None):
        self.sent.append((chat, text))
        return []

    async def api_connect_plan(self, _user_id, _link):
        kind = self.connect_plan_result[0]
        if kind == "known_contact_address":
            return (
                {
                    "type": "contactAddress",
                    "contactAddressPlan": {"type": "known", "contact": self.connect_plan_result[1]},
                },
                {},
            )
        if kind == "known_invitation":
            return (
                {
                    "type": "invitationLink",
                    "invitationLinkPlan": {"type": "known", "contact": self.connect_plan_result[1]},
                },
                {},
            )
        if kind == "ok":
            return (
                {
                    "type": "contactAddress",
                    "contactAddressPlan": {"type": "ok"},
                },
                {},
            )
        # default "error"
        return ({"type": "error", "chatError": {}}, {})

    async def api_connect_active_user(self, _link):
        if self.connect_should_raise is not None:
            raise self.connect_should_raise
        return "contact"

    async def api_get_active_user(self):
        return self.active_user


def _bot_with_fake_api() -> tuple[Bot, FakeApi]:
    bot = Bot(profile=BotProfile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))
    api = FakeApi()
    bot._api = api  # type: ignore[assignment]
    bot._serving = True  # pretend receive loop is up
    return bot, api


# ---------------------------------------------------------------------------
# Client class
# ---------------------------------------------------------------------------


def test_client_has_no_address_or_bot_profile_attributes():
    """Client should not carry bot-side state (address creation, auto-accept,
    welcome, commands). That's the whole point of separating Client from Bot."""
    c = Client(profile=Profile(display_name="monitor"), db=SqliteDb(file_prefix="/tmp/test"))
    for attr in ("_create_address", "_update_address", "_auto_accept", "_welcome", "_commands"):
        assert not hasattr(c, attr), f"Client unexpectedly has Bot-only attribute {attr}"
    # And the wire profile has no bot peerType
    p = c._profile_to_wire()
    assert "peerType" not in p
    assert "preferences" not in p


def test_bot_is_a_client_subclass():
    """Bot should extend Client, so anywhere a Client is accepted, a Bot fits too."""
    assert issubclass(Bot, Client)


def test_client_exposes_messaging_methods():
    c = Client(profile=Profile(display_name="m"), db=SqliteDb(file_prefix="/tmp/test"))
    assert hasattr(c, "connect_to")
    assert hasattr(c, "send_and_wait")
    assert hasattr(c, "events")
    assert hasattr(c, "on_message")  # decorators available on Client too


# ---------------------------------------------------------------------------
# send_and_wait
# ---------------------------------------------------------------------------


def test_send_and_wait_requires_serving():
    """Without the receive loop running, send_and_wait must raise — otherwise
    callers would silently hang waiting for a reply that's never dispatched."""
    bot = Bot(profile=BotProfile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))
    bot._api = FakeApi()  # type: ignore[assignment]
    # _serving is False by default
    with pytest.raises(RuntimeError, match="receive loop"):
        asyncio.run(bot.send_and_wait(1, "hi"))


def test_send_and_wait_resolves_on_matching_reply():
    """A reply from the awaited contact should resolve the Future and skip
    regular message dispatch."""
    bot, api = _bot_with_fake_api()
    fallback_calls: list[str] = []

    @bot.on_message(content_type="text")
    async def fallback(_msg):
        fallback_calls.append("fallback")

    async def go() -> str:
        send_task = asyncio.create_task(bot.send_and_wait(42, "ping", timeout=2.0))
        # Yield so the task gets to register its waiter.
        await asyncio.sleep(0)
        evt = {"type": "newChatItems", "chatItems": [
            {
                "chatInfo": {"type": "direct", "contact": {"contactId": 42}},
                "chatItem": {
                    "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "pong"}},
                },
            }
        ]}
        await bot._dispatch_event(evt)  # type: ignore[arg-type]
        reply = await send_task
        return reply.text or ""

    result = asyncio.run(go())
    assert result == "pong"
    assert api.sent == [(["direct", 42], "ping")]
    assert fallback_calls == [], "fallback handler should NOT fire when a waiter consumed the reply"


def test_send_and_wait_ignores_other_contacts():
    """Replies from a different contact must not resolve the waiter — that
    would mis-correlate responses and is the bug send_and_wait exists to
    prevent users from writing themselves."""
    bot, _api = _bot_with_fake_api()

    async def go():
        send_task = asyncio.create_task(bot.send_and_wait(42, "ping", timeout=0.5))
        await asyncio.sleep(0)
        evt = {"type": "newChatItems", "chatItems": [
            {
                "chatInfo": {"type": "direct", "contact": {"contactId": 99}},
                "chatItem": {
                    "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "not for you"}},
                },
            }
        ]}
        await bot._dispatch_event(evt)  # type: ignore[arg-type]
        with pytest.raises(asyncio.TimeoutError):
            await send_task

    asyncio.run(go())


def test_send_and_wait_fifo_within_contact():
    """Two concurrent waiters on the same contact should resolve in send order."""
    bot, _api = _bot_with_fake_api()

    async def go() -> tuple[str, str]:
        first = asyncio.create_task(bot.send_and_wait(42, "first", timeout=2.0))
        await asyncio.sleep(0)
        second = asyncio.create_task(bot.send_and_wait(42, "second", timeout=2.0))
        await asyncio.sleep(0)
        for text in ("reply1", "reply2"):
            evt = {"type": "newChatItems", "chatItems": [
                {
                    "chatInfo": {"type": "direct", "contact": {"contactId": 42}},
                    "chatItem": {
                        "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": text}},
                    },
                }
            ]}
            await bot._dispatch_event(evt)  # type: ignore[arg-type]
        return (await first).text or "", (await second).text or ""

    a, b = asyncio.run(go())
    assert (a, b) == ("reply1", "reply2")


def test_send_and_wait_cleans_up_state_on_timeout():
    """Timed-out waiters must be removed so they don't accidentally consume
    later replies."""
    bot, _api = _bot_with_fake_api()

    async def go():
        with pytest.raises(asyncio.TimeoutError):
            await bot.send_and_wait(42, "ping", timeout=0.05)
        assert 42 not in bot._reply_waiters, f"leaked waiters: {bot._reply_waiters}"

    asyncio.run(go())


def test_dispatch_skips_cancelled_waiters_and_falls_through_to_handlers():
    """Race fix: if a waiter is cancelled (wait_for timed out) but still in
    the FIFO when a reply arrives, the dispatcher must skip it and either
    resolve a live waiter OR fall through to user message handlers — not
    silently drop the message."""
    bot, _api = _bot_with_fake_api()
    fallback_calls: list[str] = []

    @bot.on_message(content_type="text")
    async def fallback(msg):
        fallback_calls.append(msg.text or "")

    async def go():
        # Manually inject a cancelled waiter (simulating wait_for timeout
        # cleanup losing the race with the inbound message).
        loop = asyncio.get_running_loop()
        stale: asyncio.Future = loop.create_future()
        stale.cancel()
        bot._reply_waiters[42] = [stale]

        evt = {"type": "newChatItems", "chatItems": [
            {
                "chatInfo": {"type": "direct", "contact": {"contactId": 42}},
                "chatItem": {
                    "content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "racing reply"}},
                },
            }
        ]}
        await bot._dispatch_event(evt)  # type: ignore[arg-type]

    asyncio.run(go())
    assert fallback_calls == ["racing reply"], (
        "dispatcher dropped the message instead of falling through to user handlers; "
        f"got {fallback_calls}"
    )
    assert 42 not in bot._reply_waiters, "cancelled waiter wasn't cleaned up"


def test_send_and_wait_parallel_different_contacts():
    """Concurrent send_and_wait to different contacts must not block each other.

    The library docstring promises this; this test pins the behaviour so a
    future refactor (e.g., adding a single lock) can't quietly break it."""
    bot, _api = _bot_with_fake_api()

    async def go() -> tuple[str, str]:
        t_a = asyncio.create_task(bot.send_and_wait(10, "a", timeout=2.0))
        await asyncio.sleep(0)
        t_b = asyncio.create_task(bot.send_and_wait(20, "b", timeout=2.0))
        await asyncio.sleep(0)
        # Deliver reply for B first — order shouldn't matter.
        await bot._dispatch_event({"type": "newChatItems", "chatItems": [  # type: ignore[arg-type]
            {
                "chatInfo": {"type": "direct", "contact": {"contactId": 20}},
                "chatItem": {"content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "B"}}},
            }
        ]})
        await bot._dispatch_event({"type": "newChatItems", "chatItems": [  # type: ignore[arg-type]
            {
                "chatInfo": {"type": "direct", "contact": {"contactId": 10}},
                "chatItem": {"content": {"type": "rcvMsgContent", "msgContent": {"type": "text", "text": "A"}}},
            }
        ]})
        return (await t_a).text or "", (await t_b).text or ""

    a, b = asyncio.run(go())
    assert (a, b) == ("A", "B")


# ---------------------------------------------------------------------------
# connect_to
# ---------------------------------------------------------------------------


def test_connect_to_returns_known_contact_without_handshake():
    """If the link is already known, connect_to skips api_connect entirely."""
    bot, api = _bot_with_fake_api()
    existing = {"contactId": 7, "profile": {"displayName": "SimpleX Directory"}}
    api.connect_plan_result = ("known_contact_address", existing)

    contact = asyncio.run(bot.connect_to("link", timeout=2.0))
    assert contact["contactId"] == 7
    # No connect issued: send buffer untouched.
    assert api.sent == []


def test_connect_to_waits_for_contactConnected():
    """For unknown links, connect_to issues the handshake and waits for the
    contactConnected event before returning."""
    bot, api = _bot_with_fake_api()
    api.connect_plan_result = ("ok", None)
    new_contact = {"contactId": 11, "profile": {"displayName": "Friend"}}

    async def go():
        connect_task = asyncio.create_task(bot.connect_to("link", timeout=2.0))
        await asyncio.sleep(0)
        await bot._dispatch_event({"type": "contactConnected", "contact": new_contact})  # type: ignore[arg-type]
        return await connect_task

    contact = asyncio.run(go())
    assert contact["contactId"] == 11


def test_connect_to_tolerates_contact_already_exists():
    """ContactAlreadyExistsError must NOT abort connect_to — a previous
    incomplete attempt may have left the connection mid-handshake; the
    contactConnected event will still arrive."""
    bot, api = _bot_with_fake_api()
    api.connect_plan_result = ("ok", None)
    api.connect_should_raise = ContactAlreadyExistsError(
        "exists", {"type": "contactAlreadyExists"}  # type: ignore[arg-type]
    )

    async def go():
        connect_task = asyncio.create_task(bot.connect_to("link", timeout=2.0))
        await asyncio.sleep(0)
        await bot._dispatch_event({"type": "contactConnected", "contact": {"contactId": 5, "profile": {"displayName": "Friend"}}})  # type: ignore[arg-type]
        return await connect_task

    contact = asyncio.run(go())
    assert contact["contactId"] == 5


def test_connect_to_requires_serving():
    bot = Bot(profile=BotProfile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))
    bot._api = FakeApi()  # type: ignore[assignment]
    with pytest.raises(RuntimeError, match="receive loop"):
        asyncio.run(bot.connect_to("link"))


def test_connect_to_timeout_cleans_up_waiter():
    bot, api = _bot_with_fake_api()
    api.connect_plan_result = ("ok", None)

    async def go():
        with pytest.raises(asyncio.TimeoutError):
            await bot.connect_to("link", timeout=0.05)
        assert bot._connect_waiters == [], "leaked connect waiter"

    asyncio.run(go())


def test_connect_to_rejects_non_positive_timeout():
    """timeout<=0 must fail upfront — otherwise wait_for raises after the
    handshake side-effect has already gone over the wire."""
    bot, _api = _bot_with_fake_api()

    async def go():
        for bad in (0, -1, -0.001):
            with pytest.raises(ValueError, match="timeout must be positive"):
                await bot.connect_to("link", timeout=bad)

    asyncio.run(go())


def test_send_and_wait_rejects_non_positive_timeout():
    """Same as connect_to: timeout<=0 would surprise the caller with a sent
    message and no Future to await."""
    bot, api = _bot_with_fake_api()

    async def go():
        for bad in (0, -1, -0.5):
            with pytest.raises(ValueError, match="timeout must be positive"):
                await bot.send_and_wait(42, "ping", timeout=bad)
        # And nothing was sent.
        assert api.sent == []

    asyncio.run(go())


def test_stop_before_serve_forever_is_preserved(monkeypatch):
    """If stop() is called between __aenter__ and serve_forever (e.g. a
    signal handler fires during the window where run() wires SIGINT), the
    pre-set _stop_event must NOT be cleared by serve_forever — otherwise
    the signal is silently lost and the loop runs indefinitely."""
    import simplex_chat.client as client_mod

    class _FakeApi:
        @classmethod
        async def init(cls, *_a, **_kw):
            return cls()

        @property
        def started(self):
            return False

        async def start_chat(self):
            pass

        async def stop_chat(self):
            pass

        async def close(self):
            pass

        async def api_get_active_user(self):
            return {"userId": 1, "profile": {"displayName": "x"}}

        async def recv_chat_event(self, wait_us=0):
            # Should NOT be reached — the loop should exit on the pre-set
            # stop event before it ever polls for an event.
            raise AssertionError("receive loop should have exited immediately")

        # _ensure_active_user / _maybe_sync_profile pokes
        async def send_chat_cmd(self, _cmd):
            return {"type": "cmdOk"}

    monkeypatch.setattr(client_mod, "ChatApi", _FakeApi)

    c = Client(profile=Profile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))

    async def go():
        async with c:
            c.stop()  # signal fires before serve_forever
            await c.serve_forever()  # must not block

    asyncio.run(go())


def test_aexit_nulls_api_even_if_close_raises(monkeypatch):
    """If `close()` raises inside __aexit__, the Client must still appear
    closed — `client.api` should refuse to hand back the half-shutdown
    controller, and re-entering the context manager should re-init cleanly."""
    import simplex_chat.client as client_mod

    init_count = [0]

    class _BoomCloseApi:
        @classmethod
        async def init(cls, *_a, **_kw):
            init_count[0] += 1
            return cls()

        @property
        def started(self):
            return False

        async def start_chat(self):
            pass

        async def stop_chat(self):
            pass

        async def close(self):
            raise RuntimeError("close failed")

        async def api_get_active_user(self):
            return {"userId": 1, "profile": {"displayName": "x"}}

        async def send_chat_cmd(self, _cmd):
            return {"type": "cmdOk"}

    monkeypatch.setattr(client_mod, "ChatApi", _BoomCloseApi)

    c = Client(profile=Profile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))

    async def go():
        with pytest.raises(RuntimeError, match="close failed"):
            async with c:
                pass
        # _api must be None despite close() raising
        assert c._api is None, "Client._api leaked after __aexit__ close() raised"
        with pytest.raises(RuntimeError, match="not initialized"):
            _ = c.api
        # Re-enter must work
        try:
            async with c:
                pass
        except RuntimeError:
            pass  # close raises again, fine
        assert init_count[0] == 2, "re-entry didn't re-init the controller"

    asyncio.run(go())


def test_aenter_rolls_back_partial_init_on_post_start_failure(monkeypatch):
    """If anything in __aenter__ raises after ChatApi.init succeeded — including
    _post_start — the controller must be closed. Otherwise the with-block isn't
    entered, __aexit__ never runs, and the FFI handle leaks."""
    import simplex_chat.client as client_mod

    closed: list[str] = []
    started: list[bool] = [False]

    class FakeChatApi:
        @classmethod
        async def init(cls, *_args, **_kwargs):
            return cls()

        @property
        def started(self) -> bool:
            return started[0]

        async def start_chat(self):
            started[0] = True

        async def stop_chat(self):
            started[0] = False
            closed.append("stop")

        async def close(self):
            closed.append("close")

        # Stub the bits _ensure_active_user / _maybe_sync_profile reach for.
        async def api_get_active_user(self):
            return {"userId": 1, "profile": {"displayName": "x"}}

        async def send_chat_cmd(self, _cmd):
            return {"type": "cmdOk"}

    monkeypatch.setattr(client_mod, "ChatApi", FakeChatApi)

    class Boom(RuntimeError):
        pass

    class BoomClient(Client):
        async def _post_start(self, user):
            raise Boom("kaboom")

    c = BoomClient(profile=Profile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))

    async def go():
        with pytest.raises(Boom):
            async with c:
                pytest.fail("should not enter the with-block")

    asyncio.run(go())
    assert closed == ["stop", "close"], f"controller not cleaned up: {closed}"
    assert c._api is None, "Client._api should be reset to None after rollback"


def test_lookup_known_contact_propagates_non_command_errors():
    """_lookup_known_contact must NOT mask transport / FFI errors as 'unknown
    link' — only ChatCommandError (malformed link, etc.) should fall through
    to the handshake path. Bare Exception catch would hide real bugs."""
    bot, api = _bot_with_fake_api()

    class BoomError(RuntimeError):
        pass

    async def boom(_user_id, _link):
        raise BoomError("FFI wedged")

    api.api_connect_plan = boom  # type: ignore[assignment]

    async def go():
        with pytest.raises(BoomError):
            await bot._lookup_known_contact(1, "link")

    asyncio.run(go())


# ---------------------------------------------------------------------------
# Exception subclasses
# ---------------------------------------------------------------------------


def test_contact_already_exists_is_chat_command_error_subclass():
    """Callers should be able to catch the base class to handle all command
    errors uniformly, and the specific subclass for targeted handling."""
    from simplex_chat import ChatCommandError, ContactAlreadyExistsError

    assert issubclass(ContactAlreadyExistsError, ChatCommandError)

    e = ContactAlreadyExistsError("x", {"type": "contactAlreadyExists"})  # type: ignore[arg-type]
    assert isinstance(e, ChatCommandError)
    assert e.response_type == "contactAlreadyExists"


def test_chat_command_error_response_type_property():
    from simplex_chat import ChatCommandError

    e = ChatCommandError("x", {"type": "someError"})  # type: ignore[arg-type]
    assert e.response_type == "someError"


# ---------------------------------------------------------------------------
# events() mutual exclusion with serve_forever
# ---------------------------------------------------------------------------


def test_events_raises_if_already_serving():
    bot, _api = _bot_with_fake_api()
    # _serving=True is set by _bot_with_fake_api

    async def go():
        with pytest.raises(RuntimeError, match="mutually exclusive"):
            async for _ in bot.events():
                pass

    asyncio.run(go())
