"""Base `Client` API: lifecycle, dispatch, decorators, connect_to / send_and_wait / events.

Bot extends Client to add server-side features (address, auto-accept, welcome,
commands). Client by itself is suitable for monitors, probes, automated
participants — anything that talks TO services rather than accepting incoming
connections.
"""

from __future__ import annotations

import asyncio
import logging
import os
import signal as _signal
from collections.abc import AsyncIterator, Awaitable, Callable
from dataclasses import dataclass
from typing import Any, Generic, Literal, TypeVar, overload

from . import util
from .api import ChatApi, ChatCommandError, ContactAlreadyExistsError, Db
from .core import ChatAPIError, MigrationConfirmation
from .filters import compile_message_filter
from .types import CEvt, T

log = logging.getLogger("simplex_chat")

C = TypeVar("C", bound="T.MsgContent")


@dataclass(slots=True)
class Profile:
    """SimpleX user profile fields: display name, optional full name, descr, avatar.

    Universal — used by both `Client` and `Bot`. The bot-specific extensions
    (peerType=bot, command list, calls/voice preferences) are added at
    wire-conversion time by `Bot`, not stored here.
    """

    display_name: str
    full_name: str = ""
    short_descr: str | None = None
    image: str | None = None


# Backwards-compatibility alias — the dataclass was named `BotProfile` before
# the Client/Bot hierarchy was introduced. Keep the old name working so
# `from simplex_chat import BotProfile` doesn't break existing code.
BotProfile = Profile


@dataclass(slots=True, frozen=True)
class ParsedCommand:
    keyword: str
    args: str


@dataclass(slots=True, frozen=True)
class Message(Generic[C]):
    chat_item: T.AChatItem
    content: C
    client: "Client"

    @property
    def chat_info(self) -> T.ChatInfo:
        return self.chat_item["chatInfo"]

    @property
    def text(self) -> str | None:
        c = self.content
        if isinstance(c, dict):
            return c.get("text")  # type: ignore[return-value]
        return None

    async def reply(self, text: str) -> "Message[T.MsgContent]":
        items = await self.client.api.api_send_text_reply(self.chat_item, text)
        ci = items[0]
        content = ci["chatItem"]["content"]
        # content is CIContent — snd variant has msgContent; cast for type safety.
        msg_content: T.MsgContent = content["msgContent"]  # type: ignore[index]
        return Message(chat_item=ci, content=msg_content, client=self.client)

    async def reply_content(self, content: T.MsgContent) -> "Message[T.MsgContent]":
        items = await self.client.api.api_send_messages(
            self.chat_info, [{"msgContent": content, "mentions": {}}]
        )
        ci = items[0]
        ci_content = ci["chatItem"]["content"]
        msg_content: T.MsgContent = ci_content["msgContent"]  # type: ignore[index]
        return Message(chat_item=ci, content=msg_content, client=self.client)


# Concrete narrowed aliases — one per MsgContent_<tag> variant in _types.py.
TextMessage = Message[T.MsgContent_text]
LinkMessage = Message[T.MsgContent_link]
ImageMessage = Message[T.MsgContent_image]
VideoMessage = Message[T.MsgContent_video]
VoiceMessage = Message[T.MsgContent_voice]
FileMessage = Message[T.MsgContent_file]
ReportMessage = Message[T.MsgContent_report]
ChatMessage = Message[T.MsgContent_chat]
UnknownMessage = Message[T.MsgContent_unknown]

MessageHandler = Callable[[Message[Any]], Awaitable[None]]
CommandHandler = Callable[[Message[Any], ParsedCommand], Awaitable[None]]
EventHandler = Callable[[CEvt.ChatEvent], Awaitable[None]]


class Middleware:
    """Override `__call__` to wrap message handlers with cross-cutting logic.

    `handler` is the next stage in the chain — call it with `(message, data)`
    to continue, or skip the call to short-circuit. `data` is a per-dispatch
    dict that middleware can use to pass values down the chain.
    """

    async def __call__(
        self,
        handler: Callable[[Message[Any], dict[str, object]], Awaitable[None]],
        message: Message[Any],
        data: dict[str, object],
    ) -> None:
        await handler(message, data)


class Client:
    """SimpleX participant — has an identity, sends and receives messages.

    No address, no auto-accept of incoming requests, no bot profile prefs. Use
    this for monitors, probes, automated participants — anything that talks
    TO services rather than accepting incoming connections. Use `Bot` for the
    server-side flavour.

    Typical pattern:

        async with Client(profile=Profile(display_name="m"), db=...) as c:
            serve = asyncio.create_task(c.serve_forever())
            contact = await c.connect_to(link)
            reply = await c.send_and_wait(contact["contactId"], "/help")
            c.stop()
            await serve

    The decorator-style handlers (`@on_message`, `@on_command`, `@on_event`)
    work too if you want callback-style dispatch instead of async-await.
    """

    def __init__(
        self,
        *,
        profile: Profile,
        db: Db,
        confirm_migrations: MigrationConfirmation = MigrationConfirmation.YES_UP,
        update_profile: bool = True,
        log_contacts: bool = False,
        log_network: bool = False,
    ) -> None:
        self._profile = profile
        self._db = db
        self._confirm_migrations = confirm_migrations
        self._update_profile = update_profile
        self._log_contacts = log_contacts
        self._log_network = log_network
        self._api: ChatApi | None = None
        self._serving = False
        self._stop_event = asyncio.Event()
        self._message_handlers: list[tuple[Callable[[Message[Any]], bool], MessageHandler]] = []
        self._command_handlers: list[
            tuple[tuple[str, ...], Callable[[Message[Any]], bool], CommandHandler]
        ] = []
        self._event_handlers: dict[str, list[EventHandler]] = {}
        self._middleware: list[Middleware] = []
        # Track default-handler registration so __aenter__ on a re-used client
        # doesn't accumulate duplicate log/error handlers.
        self._defaults_registered = False
        # Internal waiters used by `send_and_wait` (keyed by contact_id, FIFO
        # within a contact) and `connect_to` (one-shot, resolved on the next
        # contactConnected event). Populated by user-async-callers, drained
        # in `_dispatch_event` before user handlers run.
        self._reply_waiters: dict[int, list[asyncio.Future[Message[Any]]]] = {}
        self._connect_waiters: list[asyncio.Future[T.Contact]] = []

    @property
    def api(self) -> ChatApi:
        if self._api is None:
            raise RuntimeError("Client not initialized — call run() or use `async with client:`")
        return self._api

    # ------------------------------------------------------------------ #
    # Decorators
    # ------------------------------------------------------------------ #

    @overload
    def on_message(
        self, *, content_type: Literal["text"], **rest: Any
    ) -> Callable[
        [Callable[[TextMessage], Awaitable[None]]],
        Callable[[TextMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["link"], **rest: Any
    ) -> Callable[
        [Callable[[LinkMessage], Awaitable[None]]],
        Callable[[LinkMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["image"], **rest: Any
    ) -> Callable[
        [Callable[[ImageMessage], Awaitable[None]]],
        Callable[[ImageMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["video"], **rest: Any
    ) -> Callable[
        [Callable[[VideoMessage], Awaitable[None]]],
        Callable[[VideoMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["voice"], **rest: Any
    ) -> Callable[
        [Callable[[VoiceMessage], Awaitable[None]]],
        Callable[[VoiceMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["file"], **rest: Any
    ) -> Callable[
        [Callable[[FileMessage], Awaitable[None]]],
        Callable[[FileMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["report"], **rest: Any
    ) -> Callable[
        [Callable[[ReportMessage], Awaitable[None]]],
        Callable[[ReportMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["chat"], **rest: Any
    ) -> Callable[
        [Callable[[ChatMessage], Awaitable[None]]],
        Callable[[ChatMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(
        self, *, content_type: Literal["unknown"], **rest: Any
    ) -> Callable[
        [Callable[[UnknownMessage], Awaitable[None]]],
        Callable[[UnknownMessage], Awaitable[None]],
    ]: ...

    @overload
    def on_message(self, **rest: Any) -> Callable[[MessageHandler], MessageHandler]: ...

    def on_message(self, **filter_kw: Any) -> Callable[[MessageHandler], MessageHandler]:
        predicate = compile_message_filter(filter_kw)

        def deco(fn: MessageHandler) -> MessageHandler:
            self._message_handlers.append((predicate, fn))
            return fn

        return deco

    def on_command(
        self, name: str | tuple[str, ...], **filter_kw: Any
    ) -> Callable[[CommandHandler], CommandHandler]:
        names = (name,) if isinstance(name, str) else tuple(name)
        predicate = compile_message_filter(filter_kw)

        def deco(fn: CommandHandler) -> CommandHandler:
            self._command_handlers.append((names, predicate, fn))
            return fn

        return deco

    # `on_event` is exposed as a property typed as the generated
    # `OnEventDecorator` Protocol so per-tag narrowing applies — e.g.
    # `@client.on_event("contactConnected")` types the handler's event
    # parameter as `CEvt.ContactConnected`, not the unnarrowed
    # `CEvt.ChatEvent` union. The Protocol's overload chain lives in
    # generated code (one entry per event tag) so it stays in sync with
    # the wire schema automatically. The runtime implementation is the
    # plain handler-registration below.
    @property
    def on_event(self) -> CEvt.OnEventDecorator:
        return self._register_event_handler  # type: ignore[return-value]

    def _register_event_handler(
        self, event: str, /
    ) -> Callable[[EventHandler], EventHandler]:
        def deco(fn: EventHandler) -> EventHandler:
            self._event_handlers.setdefault(event, []).append(fn)
            return fn

        return deco

    def use(self, middleware: Middleware) -> None:
        self._middleware.append(middleware)

    # ------------------------------------------------------------------ #
    # Lifecycle
    # ------------------------------------------------------------------ #

    async def __aenter__(self) -> "Client":
        # Order matters: libsimplex `/_start` requires an active user, so
        # ensure (or create) the user first, THEN start the chat, THEN
        # do post-start setup (profile sync; Bot adds address sync).
        # Clear `_stop_event` here (not in `serve_forever`/`events`) so that
        # a `stop()` call landing between `__aenter__` and the receive loop
        # — e.g. a signal handler firing while signal handlers are being
        # wired up — is preserved and causes the loop to exit immediately
        # on entry.
        self._stop_event.clear()
        self._api = await ChatApi.init(self._db, self._confirm_migrations)
        try:
            user = await self._ensure_active_user()
            await self._api.start_chat()
            await self._post_start(user)
            self._register_log_handlers()
            return self
        except BaseException:
            # __aexit__ is only called when __aenter__ returns successfully —
            # roll back the open chat controller here so a failure during
            # init doesn't leak the FFI resource.
            await self._shutdown_partial_init()
            raise

    async def _shutdown_partial_init(self) -> None:
        """Best-effort teardown for an `__aenter__` that didn't reach return."""
        api = self._api
        if api is None:
            return
        if api.started:
            try:
                await api.stop_chat()
            except Exception:
                log.exception("stop_chat failed during init rollback")
        try:
            await api.close()
        except Exception:
            log.exception("close failed during init rollback")
        self._api = None

    async def __aexit__(self, *exc_info: object) -> None:
        self.stop()
        api = self._api
        if api is None:
            return
        # Null out the reference up-front so the Client appears closed even
        # if stop_chat / close raise — otherwise `client.api` would still
        # hand back a half-shutdown controller after `async with` exits.
        self._api = None
        try:
            await api.stop_chat()
        finally:
            await api.close()

    async def _post_start(self, user: T.User) -> None:
        """Hook for subclasses to add work between `start_chat` and serving.

        Default (Client): sync profile only. Bot overrides to also sync its
        address and embed the connection link in the profile.
        """
        await self._maybe_sync_profile(user, contact_link=None)

    def run(self) -> None:
        """Blocking entry: runs serve_forever() with SIGINT/SIGTERM handlers installed.

        Configures `logging.basicConfig(level=INFO)` if the root logger has no
        handlers yet, so startup messages and the announced address are
        visible without callers having to set up logging. Embedders that
        manage logging themselves are unaffected (basicConfig is a no-op when
        handlers already exist).
        """
        if not logging.getLogger().handlers:
            logging.basicConfig(
                level=logging.INFO,
                format="%(asctime)s %(levelname)s %(name)s %(message)s",
            )

        async def _main() -> None:
            async with self:
                loop = asyncio.get_running_loop()
                # First Ctrl+C → graceful stop (~500ms, bounded by the
                # receive-loop poll interval). Second Ctrl+C → force-exit
                # immediately (in case stop_chat / close hang on a wedged
                # FFI call). Standard CLI UX (jupyter, ipython, …).
                sigint_count = 0

                def on_interrupt() -> None:
                    nonlocal sigint_count
                    sigint_count += 1
                    if sigint_count == 1:
                        log.info("stopping... (press Ctrl+C again to force exit)")
                        self.stop()
                    else:
                        os._exit(130)  # 128 + SIGINT

                if hasattr(_signal, "SIGINT"):
                    try:
                        loop.add_signal_handler(_signal.SIGINT, on_interrupt)
                        loop.add_signal_handler(_signal.SIGTERM, self.stop)
                    except NotImplementedError:  # Windows
                        _signal.signal(_signal.SIGINT, lambda *_: on_interrupt())
                await self.serve_forever()

        asyncio.run(_main())

    async def serve_forever(self) -> None:
        if self._serving:
            raise RuntimeError("already serving")
        self._serving = True
        try:
            await self._receive_loop()
        finally:
            self._serving = False

    def stop(self) -> None:
        self._stop_event.set()

    async def events(self) -> AsyncIterator[CEvt.ChatEvent]:
        """Yield chat events one at a time — alternative to `serve_forever`.

        Runs the full dispatch pipeline on each event (internal waiters,
        user `@on_event`/`@on_message`/`@on_command` handlers), then yields
        the raw event for inspection. Use this when you want direct control
        over the receive loop, e.g. to surface errors that `serve_forever`
        would otherwise swallow, or to compose with other async iterators.

        Mutually exclusive with `serve_forever`. Stops when `stop()` is
        called or when the consumer exits the `async for` loop (which
        triggers the generator's `aclose`). Async-generator GC alone is
        not reliable for cleanup — exit the loop explicitly.
        """
        if self._serving:
            raise RuntimeError(
                "already serving — events() and serve_forever() are mutually exclusive"
            )
        self._serving = True
        try:
            while not self._stop_event.is_set():
                try:
                    event = await self.api.recv_chat_event(wait_us=500_000)
                except asyncio.CancelledError:
                    raise
                if event is None:
                    continue
                try:
                    await self._dispatch_event(event)
                except asyncio.CancelledError:
                    raise
                except Exception:
                    log.exception("dispatch_event failed for tag=%s", event.get("type"))
                yield event
        finally:
            self._serving = False

    async def connect_to(self, link: str, *, timeout: float = 120.0) -> T.Contact:
        """Connect to a SimpleX contact link, returning the resulting Contact.

        Idempotent: if the link is already known (via `api_connect_plan`)
        the existing Contact is returned without re-handshaking. Otherwise
        initiates the handshake and waits for the `contactConnected` event.

        Requires the receive loop to be running (`serve_forever` or
        `events()`), since the handshake completes asynchronously.

        Concurrency caveat: pending `connect_to` waiters are a single FIFO
        with no link↔waiter correlation. If you call `connect_to` for two
        different links concurrently, or if a third party connects to your
        address (Bot subclass with `auto_accept=True`) while a `connect_to`
        is in flight, the returned Contact may not be the one you asked
        for. Sequence concurrent connects, or call them one at a time.

        Raises:
            asyncio.TimeoutError: handshake didn't complete within `timeout`
            ValueError: timeout is not positive
            RuntimeError: no active user, or receive loop not running
        """
        if timeout <= 0:
            # Reject upfront — otherwise wait_for raises TimeoutError after
            # the handshake side-effect (api_connect_active_user) has
            # already gone over the wire, leaving the caller with no
            # Contact reference and a half-initiated connection.
            raise ValueError(f"timeout must be positive, got {timeout!r}")
        if not self._serving:
            raise RuntimeError(
                "connect_to requires the receive loop to be running — "
                "call serve_forever() (in a task) or iterate events() first"
            )
        api = self.api
        user = await api.api_get_active_user()
        if user is None:
            raise RuntimeError("no active user")

        existing = await self._lookup_known_contact(user["userId"], link)
        if existing is not None:
            return existing

        loop = asyncio.get_running_loop()
        waiter: asyncio.Future[T.Contact] = loop.create_future()
        self._connect_waiters.append(waiter)
        try:
            try:
                await api.api_connect_active_user(link)
            except ContactAlreadyExistsError:
                # Handshake mid-flight, or a previous incomplete attempt
                # left the connection in a known-but-not-connected state.
                # Either way: wait for the contactConnected event.
                pass
            return await asyncio.wait_for(waiter, timeout=timeout)
        finally:
            if waiter in self._connect_waiters:
                self._connect_waiters.remove(waiter)

    async def _lookup_known_contact(self, user_id: int, link: str) -> T.Contact | None:
        """Resolve a link to an existing Contact via api_connect_plan, or None.

        Only ChatCommandError is swallowed (malformed link, etc.) — the
        connect_to caller will fall back to the full handshake path.
        Transport/FFI errors propagate so the caller sees the real cause.
        """
        try:
            plan, _ = await self.api.api_connect_plan(user_id, link)
        except ChatCommandError:
            return None
        if plan["type"] == "contactAddress":
            cap = plan["contactAddressPlan"]
            if cap["type"] == "known":
                return cap["contact"]
        if plan["type"] == "invitationLink":
            ilp = plan["invitationLinkPlan"]
            if ilp["type"] == "known":
                return ilp["contact"]
        return None

    async def send_and_wait(
        self,
        contact_id: int,
        text: str,
        *,
        timeout: float = 30.0,
    ) -> "Message[T.MsgContent]":
        """Send text to a direct contact and wait for the next reply from them.

        Waiters are FIFO per contact_id: two concurrent calls to the same
        contact get two replies in send order. Concurrent calls to *different*
        contacts run in parallel. Once a reply matches a waiter, user
        message handlers do NOT fire for that message — the awaiter owns it.

        Correlation caveat: matching is by sender contact_id only — there
        is no message-id correlation. ANY direct message from `contact_id`
        arriving while a waiter is pending will resolve that waiter, even
        if it was an unsolicited message (e.g. an auto-reply from a bot's
        address settings, a delayed reply from a previous send, a push
        notification). For strict request/response semantics, ensure the
        peer is otherwise quiet, or use the `@on_message` callback model.

        Requires the receive loop to be running. Raises asyncio.TimeoutError
        on timeout, ValueError if timeout is not positive.
        """
        if timeout <= 0:
            # Reject upfront — otherwise wait_for raises TimeoutError after
            # api_send_text_message already went over the wire, surprising
            # the caller with a sent message and no Future to await.
            raise ValueError(f"timeout must be positive, got {timeout!r}")
        if not self._serving:
            raise RuntimeError(
                "send_and_wait requires the receive loop to be running — "
                "call serve_forever() (in a task) or iterate events() first"
            )
        loop = asyncio.get_running_loop()
        waiter: asyncio.Future[Message[Any]] = loop.create_future()
        waiters = self._reply_waiters.setdefault(contact_id, [])
        waiters.append(waiter)
        try:
            await self.api.api_send_text_message(["direct", contact_id], text)
            return await asyncio.wait_for(waiter, timeout=timeout)
        finally:
            # Always clean up our slot, even on send error or timeout. Leaving
            # an unresolved Future in the dict would make the next incoming
            # message resolve a future no one is waiting on.
            if waiter in waiters:
                waiters.remove(waiter)
            if not waiters:
                self._reply_waiters.pop(contact_id, None)

    async def _receive_loop(self) -> None:
        # Catch broad Exception so a single malformed event or transient
        # native error doesn't crash the whole client. CancelledError must
        # always re-raise so `stop()` and asyncio cancellation work.
        # `wait_us=500_000` (500ms) bounds the worst-case Ctrl+C latency:
        # the C call blocks the worker thread until timeout, and the loop
        # only checks `_stop_event` between polls.
        while not self._stop_event.is_set():
            try:
                event = await self.api.recv_chat_event(wait_us=500_000)
            except asyncio.CancelledError:
                raise
            except ChatAPIError as e:
                # Async chat errors emitted via the Haskell `eToView` path —
                # routine soft errors (stale connections after a peer deletes
                # a chat, file cleanup failures, etc.) intermixed with
                # CRITICAL agent failures the operator must see. Mirror the
                # desktop policy in SimpleXAPI.kt:3332-3340: escalate
                # CRITICAL agent errors, keep everything else at debug.
                chat_err: Any = e.chat_error or {}
                agent_err: Any = (
                    chat_err.get("agentError", {}) if chat_err.get("type") == "errorAgent" else {}
                )
                if agent_err.get("type") == "CRITICAL":
                    log.error(
                        "chat agent CRITICAL: %s (offerRestart=%s)",
                        agent_err.get("criticalErr"),
                        agent_err.get("offerRestart"),
                    )
                else:
                    log.debug("chat event error: %s", chat_err.get("type"))
                continue
            except Exception:
                log.exception("recv_chat_event failed")
                # Bound the spin rate when the FFI is wedged on a persistent
                # error (vs the timeout path, which already paces itself).
                await asyncio.sleep(0.5)
                continue
            if event is None:
                continue
            try:
                await self._dispatch_event(event)
            except asyncio.CancelledError:
                raise
            except Exception:
                log.exception("dispatch_event failed for tag=%s", event.get("type"))

    # ------------------------------------------------------------------ #
    # Dispatch
    # ------------------------------------------------------------------ #

    async def _dispatch_event(self, event: CEvt.ChatEvent) -> None:
        tag = event["type"]
        # Resolve internal waiters BEFORE user handlers. A pending
        # `connect_to` consumes the contactConnected; a pending
        # `send_and_wait` consumes the matching incoming message — user
        # handlers don't fire for that message. This matches the mental
        # model: the awaiter explicitly asked for this event.
        if tag == "contactConnected" and self._connect_waiters:
            contact: T.Contact = event["contact"]  # type: ignore[typeddict-item]
            waiter = self._connect_waiters.pop(0)
            if not waiter.done():
                waiter.set_result(contact)
        for h in self._event_handlers.get(tag, []):
            try:
                await h(event)
            except Exception:
                log.exception("on_event handler failed")
        if tag == "newChatItems":
            evt: CEvt.NewChatItems = event  # type: ignore[assignment]
            for ci in evt["chatItems"]:
                content = ci["chatItem"]["content"]
                if content["type"] != "rcvMsgContent":
                    continue
                msg_content = content["msgContent"]  # type: ignore[index]
                msg: Message[T.MsgContent] = Message(chat_item=ci, content=msg_content, client=self)
                # If a send_and_wait is pending for this sender, fulfil it
                # and skip the user dispatch chain — the awaiter "owns" this
                # reply. FIFO within a contact_id.
                if self._maybe_resolve_reply_waiter(msg):
                    continue
                await self._dispatch_message(msg)

    def _maybe_resolve_reply_waiter(self, msg: Message[T.MsgContent]) -> bool:
        chat_info = msg.chat_info
        if chat_info.get("type") != "direct":
            return False
        contact_id = chat_info.get("contact", {}).get("contactId")  # type: ignore[union-attr]
        if contact_id is None:
            return False
        waiters = self._reply_waiters.get(contact_id)
        if not waiters:
            return False
        # Skip waiters whose callers have already given up (cancelled by
        # wait_for timing out at the same loop tick). Without this skip,
        # a reply arriving in the narrow timeout-race window would be
        # silently dropped because the FIFO would pop a done waiter and
        # neither resolve it nor dispatch to user handlers.
        while waiters:
            waiter = waiters.pop(0)
            if not waiter.done():
                if not waiters:
                    self._reply_waiters.pop(contact_id, None)
                waiter.set_result(msg)
                return True
        self._reply_waiters.pop(contact_id, None)
        return False

    async def _dispatch_message(self, msg: Message[Any]) -> None:
        # First-match-wins. The squaring bot's `@on_message(text=NUMBER_RE)`
        # and catch-all `@on_message(content_type="text")` both match a number
        # like "1"; we want only the first to fire. Registration order is the
        # priority order — register the most-specific filters first.
        #
        # Slash-commands are tried first against command handlers; if no
        # command handler matches, fall through to message handlers (so
        # `@on_message` can still catch unknown slash-commands).
        cmd = self._parse_command(msg)
        if cmd is not None:
            for names, predicate, handler in self._command_handlers:
                if cmd.keyword in names and predicate(msg):
                    await self._invoke_command_with_middleware(handler, msg, cmd)
                    return
        for predicate, handler in self._message_handlers:
            if predicate(msg):
                await self._invoke_with_middleware(handler, msg)
                return

    async def _invoke_with_middleware(self, handler: MessageHandler, message: Message[Any]) -> None:
        # Fast path: most clients register no middleware. Skip the closure-chain
        # construction and the empty-data dict on every dispatch.
        if not self._middleware:
            try:
                await handler(message)
            except Exception:
                log.exception("message handler failed")
            return

        async def call(m: Message[Any], _data: dict[str, object]) -> None:
            await handler(m)

        chain: Callable[[Message[Any], dict[str, object]], Awaitable[None]] = call
        for mw in reversed(self._middleware):
            inner = chain

            async def _wrapped(
                m: Message[Any],
                d: dict[str, object],
                mw: Middleware = mw,
                inner: Callable[[Message[Any], dict[str, object]], Awaitable[None]] = inner,
            ) -> None:
                await mw(inner, m, d)

            chain = _wrapped

        try:
            await chain(message, {})
        except Exception:
            log.exception("message handler failed")

    async def _invoke_command_with_middleware(
        self, handler: CommandHandler, message: Message[Any], cmd: ParsedCommand
    ) -> None:
        if not self._middleware:
            try:
                await handler(message, cmd)
            except Exception:
                log.exception("command handler failed")
            return

        async def call(m: Message[Any], _data: dict[str, object]) -> None:
            await handler(m, cmd)

        chain: Callable[[Message[Any], dict[str, object]], Awaitable[None]] = call
        for mw in reversed(self._middleware):
            inner = chain

            async def _wrapped(
                m: Message[Any],
                d: dict[str, object],
                mw: Middleware = mw,
                inner: Callable[[Message[Any], dict[str, object]], Awaitable[None]] = inner,
            ) -> None:
                await mw(inner, m, d)

            chain = _wrapped

        try:
            await chain(message, {})
        except Exception:
            log.exception("command handler failed")

    @staticmethod
    def _parse_command(msg: Message[Any]) -> ParsedCommand | None:
        parsed = util.ci_bot_command(msg.chat_item["chatItem"])
        if parsed is None:
            return None
        keyword, args = parsed
        return ParsedCommand(keyword=keyword, args=args)

    # ------------------------------------------------------------------ #
    # Profile sync
    # ------------------------------------------------------------------ #

    async def _ensure_active_user(self) -> T.User:
        """Get or create the active user. Must run before `start_chat`.

        Mirrors Node `createBotUser` (bot.ts:158-166). The chat controller
        won't accept `/_start` without a user, so this phase has to land
        before lifecycle proceeds.
        """
        api = self.api
        user = await api.api_get_active_user()
        if user is None:
            log.info("No active user in database, creating...")
            user = await api.api_create_active_user(self._profile_to_wire())
        log.info("user: %s", user["profile"]["displayName"])
        return user

    async def _maybe_sync_profile(self, user: T.User, *, contact_link: str | None) -> None:
        """Update the user profile on the wire if its fields changed.

        `contact_link` is only set by Bot (to embed its address). Mirrors
        Node `updateBotUserProfile` (bot.ts:199-214). Field-by-field
        comparison because user["profile"] is LocalProfile (has extra
        fields profileId, localAlias, preferences, peerType) so a full
        dict equality would always differ.
        """
        if not self._update_profile:
            return
        new_profile = self._profile_to_wire()
        if contact_link is not None:
            new_profile["contactLink"] = contact_link
        cur = user["profile"]
        changed = (
            cur["displayName"] != new_profile["displayName"]
            or cur.get("fullName", "") != new_profile.get("fullName", "")
            or cur.get("shortDescr") != new_profile.get("shortDescr")
            or cur.get("image") != new_profile.get("image")
            or cur.get("preferences") != new_profile.get("preferences")
            or cur.get("peerType") != new_profile.get("peerType")
            or cur.get("contactLink") != new_profile.get("contactLink")
        )
        if changed:
            log.info("profile changed, updating...")
            await self.api.api_update_profile(user["userId"], new_profile)

    def _profile_to_wire(self) -> T.Profile:
        """Convert the user-facing Profile dataclass to wire format.

        Base version produces a plain user profile. Bot overrides this to
        add the bot-specific extensions (peerType=bot, command list,
        calls/voice/files prefs).
        """
        p: T.Profile = {
            "displayName": self._profile.display_name,
            "fullName": self._profile.full_name,
        }
        if self._profile.short_descr is not None:
            p["shortDescr"] = self._profile.short_descr
        if self._profile.image is not None:
            p["image"] = self._profile.image
        return p

    # ------------------------------------------------------------------ #
    # Log subscriptions (mirror Node subscribeLogEvents bot.ts:142-156)
    # ------------------------------------------------------------------ #

    def _register_log_handlers(self) -> None:
        # Idempotent: re-entering the async context must not stack duplicate
        # log handlers. Always-on error handlers run regardless of
        # log_contacts/log_network so messageError/chatError/chatErrors
        # don't disappear into the void.
        if self._defaults_registered:
            return
        self._defaults_registered = True
        self._event_handlers.setdefault("messageError", []).append(self._log_message_error)
        self._event_handlers.setdefault("chatError", []).append(self._log_chat_error)
        self._event_handlers.setdefault("chatErrors", []).append(self._log_chat_errors)
        if self._log_contacts:
            self._event_handlers.setdefault("contactConnected", []).append(
                self._log_contact_connected
            )
            self._event_handlers.setdefault("contactDeletedByContact", []).append(
                self._log_contact_deleted
            )
        if self._log_network:
            self._event_handlers.setdefault("hostConnected", []).append(self._log_host_connected)
            self._event_handlers.setdefault("hostDisconnected", []).append(
                self._log_host_disconnected
            )
            self._event_handlers.setdefault("subscriptionStatus", []).append(
                self._log_subscription_status
            )

    @staticmethod
    async def _log_contact_connected(evt: CEvt.ChatEvent) -> None:
        log.info("%s connected", evt["contact"]["profile"]["displayName"])  # type: ignore[index]

    @staticmethod
    async def _log_contact_deleted(evt: CEvt.ChatEvent) -> None:
        log.info(
            "%s deleted connection",
            evt["contact"]["profile"]["displayName"],  # type: ignore[index]
        )

    @staticmethod
    async def _log_host_connected(evt: CEvt.ChatEvent) -> None:
        log.info("connected server %s", evt["transportHost"])  # type: ignore[index]

    @staticmethod
    async def _log_host_disconnected(evt: CEvt.ChatEvent) -> None:
        log.info("disconnected server %s", evt["transportHost"])  # type: ignore[index]

    @staticmethod
    async def _log_subscription_status(evt: CEvt.ChatEvent) -> None:
        log.info(
            "%d subscription(s) %s",
            len(evt["connections"]),  # type: ignore[index]
            evt["subscriptionStatus"]["type"],  # type: ignore[index]
        )

    @staticmethod
    async def _log_message_error(evt: CEvt.ChatEvent) -> None:
        log.warning("messageError: %s", evt.get("severity", "?"))  # type: ignore[union-attr]

    @staticmethod
    async def _log_chat_error(evt: CEvt.ChatEvent) -> None:
        err = evt.get("chatError")  # type: ignore[union-attr]
        log.error("chatError: %s", err.get("type") if isinstance(err, dict) else err)

    @staticmethod
    async def _log_chat_errors(evt: CEvt.ChatEvent) -> None:
        errs = evt.get("chatErrors") or []  # type: ignore[union-attr]
        log.error("chatErrors: %d errors", len(errs))


__all__ = [
    "BotProfile",  # backwards-compat alias for Profile
    "ChatMessage",
    "Client",
    "CommandHandler",
    "EventHandler",
    "FileMessage",
    "ImageMessage",
    "LinkMessage",
    "Message",
    "MessageHandler",
    "Middleware",
    "ParsedCommand",
    "Profile",
    "ReportMessage",
    "TextMessage",
    "UnknownMessage",
    "VideoMessage",
    "VoiceMessage",
]
