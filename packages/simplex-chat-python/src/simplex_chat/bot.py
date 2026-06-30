"""`Bot` — Client extended with server-side features (address, auto-accept, commands)."""

from __future__ import annotations

from dataclasses import dataclass

from . import util
from .api import Db
from .client import (
    BotProfile,
    ChatMessage,
    Client,
    CommandHandler,
    EventHandler,
    FileMessage,
    ImageMessage,
    LinkMessage,
    Message,
    MessageHandler,
    Middleware,
    ParsedCommand,
    Profile,
    ReportMessage,
    TextMessage,
    UnknownMessage,
    VideoMessage,
    VoiceMessage,
    log,
)
from .core import MigrationConfirmation
from .types import T


@dataclass(slots=True)
class BotCommand:
    keyword: str
    label: str
    params: str = ""


class Bot(Client):
    """SimpleX bot — Client extended with server-side features.

    On top of `Client` (identity + messaging + connect_to/send_and_wait/events),
    a Bot:
      - creates and announces its own contact address
      - auto-accepts incoming contact requests (configurable)
      - advertises a list of slash-commands in its profile preferences
      - sets `peerType=bot` and disables calls/voice in profile prefs
      - sends a `welcome` message to new contacts via the auto-reply address setting

    If you want just identity + messaging without any of that, use `Client`
    directly.
    """

    def __init__(
        self,
        *,
        profile: Profile,
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
        log_contacts: bool = True,
        log_network: bool = False,
    ) -> None:
        super().__init__(
            profile=profile,
            db=db,
            confirm_migrations=confirm_migrations,
            update_profile=update_profile,
            log_contacts=log_contacts,
            log_network=log_network,
        )
        self._welcome = welcome
        self._commands = commands or []
        self._create_address = create_address
        self._update_address = update_address
        self._auto_accept = auto_accept
        self._business_address = business_address
        self._allow_files = allow_files

    # ------------------------------------------------------------------ #
    # Profile + address sync (overrides hooks in Client)
    # ------------------------------------------------------------------ #

    async def _post_start(self, user: T.User) -> None:
        """Bots sync address first, then embed the link in the profile."""
        link = await self._sync_address(user)
        await self._maybe_sync_profile(user, contact_link=link)

    async def _sync_address(self, user: T.User) -> str | None:
        """Address sync. Returns the public link if any, for embedding in the profile."""
        api = self.api
        user_id = user["userId"]

        address = await api.api_get_user_address(user_id)
        if address is None:
            if self._create_address:
                log.info("Bot has no address, creating...")
                await api.api_create_user_address(user_id)
                address = await api.api_get_user_address(user_id)
                if address is None:
                    raise RuntimeError("Failed reading newly created user address")
            else:
                log.warning("Bot has no address")

        link: str | None = None
        if address is not None:
            link = util.contact_address_str(address["connLinkContact"])
            log.info("Bot address: %s", link)

        # Address settings (auto-accept + welcome message). Mirrors bot.ts:185-194.
        # autoAccept present → accept; absent → no auto-accept (mirrors Node bot.ts).
        if address is not None and self._update_address:
            desired: T.AddressSettings = {"businessAddress": self._business_address}
            if self._auto_accept:
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

        return link

    def _profile_to_wire(self) -> T.Profile:
        """Bot profile: base profile + peerType=bot, command list, calls/voice prefs disabled.

        Mirrors Node `mkBotProfile` (bot.ts:88-102).
        """
        p = super()._profile_to_wire()
        prefs: T.Preferences = {
            "calls": {"allow": "no"},
            "voice": {"allow": "no"},
            "files": {"allow": "yes" if self._allow_files else "no"},
        }
        if self._commands:
            prefs["commands"] = [
                {"type": "command", "keyword": c.keyword, "label": c.label, "params": c.params}
                if c.params else
                {"type": "command", "keyword": c.keyword, "label": c.label}
                for c in self._commands
            ]
        p["preferences"] = prefs
        p["peerType"] = "bot"
        return p


__all__ = [
    "Bot",
    "BotCommand",
    "BotProfile",
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
