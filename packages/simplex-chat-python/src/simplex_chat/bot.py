"""User-facing `Bot` API: decorators, filters, Message wrapper, lifecycle."""

from __future__ import annotations

import asyncio
import logging
import os
import signal as _signal
from collections.abc import Awaitable, Callable
from dataclasses import dataclass
from typing import Any, Generic, Literal, TypeVar, overload

from . import util
from .api import ChatApi, Db
from .core import MigrationConfirmation
from .filters import compile_message_filter
from .types import CEvt, T

log = logging.getLogger("simplex_chat")

C = TypeVar("C", bound="T.MsgContent")


@dataclass(slots=True)
class BotProfile:
    display_name: str
    full_name: str = ""
    short_descr: str | None = None
    image: str | None = None


@dataclass(slots=True)
class BotCommand:
    keyword: str
    label: str


@dataclass(slots=True, frozen=True)
class ParsedCommand:
    keyword: str
    args: str


@dataclass(slots=True, frozen=True)
class Message(Generic[C]):
    chat_item: T.AChatItem
    content: C
    bot: "Bot"

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
        items = await self.bot.api.api_send_text_reply(self.chat_item, text)
        ci = items[0]
        content = ci["chatItem"]["content"]
        # content is CIContent — snd variant has msgContent; cast for type safety.
        msg_content: T.MsgContent = content["msgContent"]  # type: ignore[index]
        return Message(chat_item=ci, content=msg_content, bot=self.bot)

    async def reply_content(self, content: T.MsgContent) -> "Message[T.MsgContent]":
        items = await self.bot.api.api_send_messages(
            self.chat_info, [{"msgContent": content, "mentions": {}}]
        )
        ci = items[0]
        ci_content = ci["chatItem"]["content"]
        msg_content: T.MsgContent = ci_content["msgContent"]  # type: ignore[index]
        return Message(chat_item=ci, content=msg_content, bot=self.bot)


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


class Bot:
    def __init__(
        self,
        *,
        profile: BotProfile,
        db: Db,
        welcome: str | T.MsgContent | None = None,
        commands: list[BotCommand] | None = None,
        confirm_migrations: MigrationConfirmation = MigrationConfirmation.YES_UP,
        create_address: bool = True,
        update_address: bool = True,
        update_profile: bool = True,
        auto_accept: bool = True,
        business_address: bool = False,
        allow_files: bool = False,
        use_bot_profile: bool = True,
        log_contacts: bool = True,
        log_network: bool = False,
    ) -> None:
        self._profile = profile
        self._db = db
        self._welcome = welcome
        self._commands = commands or []
        self._confirm_migrations = confirm_migrations
        self._opts = {
            "create_address": create_address,
            "update_address": update_address,
            "update_profile": update_profile,
            "auto_accept": auto_accept,
            "business_address": business_address,
            "allow_files": allow_files,
            "use_bot_profile": use_bot_profile,
            "log_contacts": log_contacts,
            "log_network": log_network,
        }
        self._api: ChatApi | None = None
        self._serving = False
        self._stop_event = asyncio.Event()
        self._message_handlers: list[tuple[Callable[[Message[Any]], bool], MessageHandler]] = []
        self._command_handlers: list[
            tuple[tuple[str, ...], Callable[[Message[Any]], bool], CommandHandler]
        ] = []
        self._event_handlers: dict[str, list[EventHandler]] = {}
        self._middleware: list[Middleware] = []
        # Track default-handler registration so __aenter__ on a re-used bot
        # doesn't accumulate duplicate log/error handlers.
        self._defaults_registered = False

    @property
    def api(self) -> ChatApi:
        if self._api is None:
            raise RuntimeError("Bot not initialized — call bot.run() or use `async with bot:`")
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

    def on_event(self, event: CEvt.ChatEvent_Tag, /) -> Callable[[EventHandler], EventHandler]:
        def deco(fn: EventHandler) -> EventHandler:
            self._event_handlers.setdefault(event, []).append(fn)
            return fn

        return deco

    def use(self, middleware: Middleware) -> None:
        self._middleware.append(middleware)

    # ------------------------------------------------------------------ #
    # Lifecycle
    # ------------------------------------------------------------------ #

    async def __aenter__(self) -> "Bot":
        # Order matters: libsimplex `/_start` requires an active user, so
        # ensure (or create) the user first, THEN start the chat, THEN
        # do address + profile sync. Mirrors Node bot.ts:48-64.
        self._api = await ChatApi.init(self._db, self._confirm_migrations)
        user = await self._ensure_active_user()
        await self._api.start_chat()
        await self._sync_address_and_profile(user)
        self._register_log_handlers()
        return self

    async def __aexit__(self, *exc_info: object) -> None:
        self.stop()
        if self._api is not None:
            try:
                await self._api.stop_chat()
            finally:
                await self._api.close()
                self._api = None

    def run(self) -> None:
        """Blocking entry: runs serve_forever() with SIGINT/SIGTERM handlers installed.

        Configures `logging.basicConfig(level=INFO)` if the root logger has no
        handlers yet, so the bot's startup messages and the announced address
        are visible without callers having to set up logging. Embedders that
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
                        log.info("stopping bot... (press Ctrl+C again to force exit)")
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
        self._stop_event.clear()
        try:
            await self._receive_loop()
        finally:
            self._serving = False

    def stop(self) -> None:
        self._stop_event.set()

    async def _receive_loop(self) -> None:
        # Catch broad Exception so a single malformed event or transient
        # native error doesn't crash the whole bot. CancelledError must
        # always re-raise so `bot.stop()` and asyncio cancellation work.
        # `wait_us=500_000` (500ms) bounds the worst-case Ctrl+C latency:
        # the C call blocks the worker thread until timeout, and the loop
        # only checks `_stop_event` between polls.
        while not self._stop_event.is_set():
            try:
                event = await self.api.recv_chat_event(wait_us=500_000)
            except asyncio.CancelledError:
                raise
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
                msg: Message[T.MsgContent] = Message(chat_item=ci, content=msg_content, bot=self)
                await self._dispatch_message(msg)

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
        # Fast path: most bots register no middleware. Skip the closure-chain
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
    # Profile + address sync
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
            user = await api.api_create_active_user(self._bot_profile_to_wire())
        log.info("Bot user: %s", user["profile"]["displayName"])
        return user

    async def _sync_address_and_profile(self, user: T.User) -> None:
        """Address + profile sync. Runs after `start_chat` (mirrors bot.ts:57-63)."""
        api = self.api
        user_id = user["userId"]

        # 2. Address (numbered to match bot.ts comments — phase 1 was user creation).
        address = await api.api_get_user_address(user_id)
        if address is None:
            if self._opts["create_address"]:
                log.info("Bot has no address, creating...")
                await api.api_create_user_address(user_id)
                address = await api.api_get_user_address(user_id)
                if address is None:
                    raise RuntimeError("Failed reading newly created user address")
            else:
                log.warning("Bot has no address")

        # Always announce the address — matches Node bot.ts:60.
        link: str | None = None
        if address is not None:
            link = util.contact_address_str(address["connLinkContact"])
            log.info("Bot address: %s", link)

        # 3. Address settings (auto-accept + welcome message). Mirrors bot.ts:185-194.
        # autoAccept present → accept; absent → no auto-accept (mirrors Node bot.ts).
        if address is not None and self._opts["update_address"]:
            desired: T.AddressSettings = {"businessAddress": self._opts["business_address"]}
            if self._opts["auto_accept"]:
                desired["autoAccept"] = {"acceptIncognito": False}
            if self._welcome is not None:
                desired["autoReply"] = (
                    {"type": "text", "text": self._welcome}
                    if isinstance(self._welcome, str)
                    else self._welcome
                )
            if address["addressSettings"] != desired:
                log.info("Bot address settings changed, updating...")
                await api.api_set_address_settings(user_id, desired)

        # 4. Profile update. Mirrors Node `updateBotUserProfile` (bot.ts:199-214).
        # Field-by-field comparison: user["profile"] is LocalProfile (has extra
        # fields profileId, localAlias, preferences, peerType) so a full-dict
        # equality would always differ.
        new_profile = self._bot_profile_to_wire()
        if link is not None and self._opts["use_bot_profile"]:
            # Mirrors bot.ts:62 — embed the connection link in the bot's profile
            # so contacts that resolve the bot via stored profile data see the
            # current address.
            new_profile["contactLink"] = link
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
        if changed and self._opts["update_profile"]:
            log.info("Bot profile changed, updating...")
            await api.api_update_profile(user_id, new_profile)

    def _bot_profile_to_wire(self) -> T.Profile:
        """Construct wire-format Profile, applying bot conventions when use_bot_profile=True.

        Mirrors Node mkBotProfile (bot.ts:88-102): bots get peerType="bot",
        calls/voice prefs disabled, files gated on `allow_files`, and any
        registered `commands` embedded in the profile preferences.
        """
        p: T.Profile = {
            "displayName": self._profile.display_name,
            "fullName": self._profile.full_name,
        }
        if self._profile.short_descr is not None:
            p["shortDescr"] = self._profile.short_descr
        if self._profile.image is not None:
            p["image"] = self._profile.image
        if self._opts["use_bot_profile"]:
            prefs: T.Preferences = {
                "calls": {"allow": "no"},
                "voice": {"allow": "no"},
                "files": {"allow": "yes" if self._opts["allow_files"] else "no"},
            }
            if self._commands:
                prefs["commands"] = [
                    {"type": "command", "keyword": c.keyword, "label": c.label}
                    for c in self._commands
                ]
            p["preferences"] = prefs
            p["peerType"] = "bot"
        elif self._commands:
            raise ValueError(
                "use_bot_profile=False but commands were passed; commands are "
                "only sent when use_bot_profile=True (they're embedded in the "
                "user profile preferences)."
            )
        return p

    # ------------------------------------------------------------------ #
    # Log subscriptions (mirror Node subscribeLogEvents bot.ts:142-156)
    # ------------------------------------------------------------------ #

    def _register_log_handlers(self) -> None:
        # Idempotent: a Bot reused across multiple `__aenter__` cycles must
        # not stack duplicate log handlers. Always-on error handlers run
        # regardless of log_contacts/log_network so messageError/chatError/
        # chatErrors don't disappear into the void.
        if self._defaults_registered:
            return
        self._defaults_registered = True
        self._event_handlers.setdefault("messageError", []).append(self._log_message_error)
        self._event_handlers.setdefault("chatError", []).append(self._log_chat_error)
        self._event_handlers.setdefault("chatErrors", []).append(self._log_chat_errors)
        if self._opts["log_contacts"]:
            self._event_handlers.setdefault("contactConnected", []).append(
                self._log_contact_connected
            )
            self._event_handlers.setdefault("contactDeletedByContact", []).append(
                self._log_contact_deleted
            )
        if self._opts["log_network"]:
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
            "%s deleted connection with bot",
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


# Suppress unused-import warnings for re-exported names used only at type-check time.
__all__ = [
    "Bot",
    "BotCommand",
    "BotProfile",
    "ChatMessage",
    "FileMessage",
    "ImageMessage",
    "LinkMessage",
    "Message",
    "MessageHandler",
    "CommandHandler",
    "EventHandler",
    "Middleware",
    "ParsedCommand",
    "ReportMessage",
    "TextMessage",
    "UnknownMessage",
    "VideoMessage",
    "VoiceMessage",
]
