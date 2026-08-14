"""Entry point: load config, start the bot, wire handlers, serve."""

from __future__ import annotations

import argparse
import asyncio
import logging
import os
import signal
import stat
import sys
from pathlib import Path

from simplex_chat import Bot, BotProfile, SqliteDb
from simplex_chat.types import CEvt

from . import business, commands, handlers, health, names, setup
from .config import Config, ConfigError, load_config
from .context import BotContext
from .errors import STARTUP_ERRORS

log = logging.getLogger("support_bot_light")

# storeError tag the core returns when a display name is already in use.
DUPLICATE_NAME = "duplicateName"


def _register(bot: Bot, ctx: BotContext) -> None:
    """Register handlers. Commands are scoped to the roster group, so the same
    keyword typed in a business chat falls through and is ignored."""
    group_id = ctx.roster_group_id

    @bot.on_command(commands.DM, group_id=group_id)
    async def _dm(msg, _cmd):
        await handlers.dm(ctx, msg)

    @bot.on_command(commands.LIST, group_id=group_id)
    async def _list(msg, _cmd):
        await handlers.list_roster(ctx, msg)

    @bot.on_command(commands.LEAVE, group_id=group_id)
    async def _leave(msg, _cmd):
        await handlers.leave(ctx, msg)

    @bot.on_command(commands.HELP, group_id=group_id)
    async def _help(msg, _cmd):
        await handlers.help_cmd(ctx, msg)

    @bot.on_event("acceptingBusinessRequest")
    async def _business(evt: CEvt.AcceptingBusinessRequest):
        await business.on_business_request(ctx, evt)

    @bot.on_event("contactConnected")
    async def _connected(evt: CEvt.ContactConnected):
        await handlers.contact_ready(ctx, evt["contact"]["contactId"])

    @bot.on_event("contactSndReady")
    async def _snd_ready(evt: CEvt.ContactSndReady):
        await handlers.contact_ready(ctx, evt["contact"]["contactId"])

    @bot.on_event("deletedMember")
    async def _deleted_member(evt: CEvt.DeletedMember):
        await handlers.member_gone(ctx, evt["groupInfo"]["groupId"], evt["deletedMember"])

    @bot.on_event("leftMember")
    async def _left_member(evt: CEvt.LeftMember):
        await handlers.member_gone(ctx, evt["groupInfo"]["groupId"], evt["member"])


def startup_error(e: Exception) -> str:
    """What the operator can act on, from an exception that names only a tag.

    The core reports a display name already taken by a contact or group as a
    bare `errorStore`, and the detail the bot needs is in `chat_error`.
    """
    chat_error = getattr(e, "chat_error", None) or {}
    if (chat_error.get("storeError") or {}).get("type") == DUPLICATE_NAME:
        return (
            "bot.display_name is already taken in this database by a contact, a "
            "group or a past customer; the core keeps every display name unique. "
            "Choose another name."
        )
    return f"{e} {chat_error}" if chat_error else str(e)


def bot_profile(config: Config, display_name: str | None = None) -> BotProfile:
    return BotProfile(display_name=display_name or config.display_name, image=config.image)


class Stopper:
    """Stop intent, tracked across every client this process starts.

    Installed before any client is started, because starting one runs database
    migrations, address creation and profile sync; a signal arriving there would
    otherwise hit the default disposition and kill the process mid-write. The
    bot is attached afterwards, and replaced for a second start, so no window
    between the two is left uncovered. `Client.__aenter__` clears its own stop
    event, so the intent is tracked here and acted on once startup has unwound.
    """

    def __init__(self) -> None:
        self.requested = False
        self._bot: Bot | None = None
        self._interrupts = 0

    def attach(self, bot: Bot) -> Bot:
        self._bot = bot
        return bot

    def _stop_bot(self) -> None:
        if self._bot is not None:
            self._bot.stop()

    def on_interrupt(self) -> None:
        self._interrupts += 1
        self.requested = True
        if self._interrupts == 1:
            log.info("stopping... (press Ctrl+C again to force exit)")
            self._stop_bot()
        else:
            os._exit(130)  # 128 + SIGINT

    def on_terminate(self) -> None:
        self.requested = True
        self._stop_bot()


def _install_signal_handlers() -> Stopper:
    """Track SIGINT/SIGTERM from before the first client start."""
    stopper = Stopper()
    try:
        loop = asyncio.get_running_loop()
        loop.add_signal_handler(signal.SIGINT, stopper.on_interrupt)
        loop.add_signal_handler(signal.SIGTERM, stopper.on_terminate)
    except NotImplementedError:  # Windows
        signal.signal(signal.SIGINT, lambda *_: stopper.on_interrupt())
    return stopper


def build_bot(
    config: Config, *, update_profile: bool = True, display_name: str | None = None
) -> Bot:
    """The bot's identity and address settings.

    business_address is what makes a connection open a group the roster can be
    added to; without it every customer would get a plain direct chat and the
    bot would have nothing to do.
    """
    return Bot(
        profile=bot_profile(config, display_name),
        db=SqliteDb(file_prefix=config.db_prefix),
        welcome=config.welcome,
        business_address=True,
        auto_accept=True,
        update_profile=update_profile,
        # The library logs peer display names verbatim; the bot sanitises every
        # name it renders itself, and this is the one path that bypasses it.
        log_contacts=False,
    )


def name_taken(e: Exception) -> bool:
    """Whether the core refused a profile update because the name is in use."""
    chat_error = getattr(e, "chat_error", None) or {}
    return (chat_error.get("storeError") or {}).get("type") == DUPLICATE_NAME


async def _run(config: Config) -> None:
    """Resolve the display name, then serve.

    Two starts, because the name the core will accept is only knowable from the
    database and the profile is applied while the client starts. The first start
    reads; the second is the one that serves. See `names` for why a rename is
    not simply attempted.

    The fallback remains for the one collision the check cannot see: a
    `display_names` row orphaned by an earlier half-commit belongs to no contact,
    group or member.
    """
    stopper = _install_signal_handlers()
    name = await _usable_name(config, stopper)
    if stopper.requested:
        log.info("stopped during startup")
        return
    refused = await _start(config, build_bot(config, display_name=name), stopper)
    if not refused:
        return
    log.warning("Serving without applying the profile change.")
    await _start(config, build_bot(config, update_profile=False), stopper, retry=False)


async def _usable_name(config: Config, stopper: Stopper) -> str:
    """The display name to start with, read from the database."""
    async with stopper.attach(build_bot(config, update_profile=False)) as probe:
        user = await probe.api.api_get_active_user()
        if user is None:
            return config.display_name
        return await names.usable(probe.api, user["userId"], config.display_name)


async def _start(config: Config, bot: Bot, stopper: Stopper, retry: bool = True) -> bool:
    """Serve until stopped. True if the core refused the display name."""
    # Not bot.run(): handlers are scoped with group_id=, which is unknown until
    # the roster group is resolved after start.
    stopper.attach(bot)
    try:
        await _serve(config, bot, stopper)
    except STARTUP_ERRORS as e:
        if not retry or not name_taken(e):
            raise
        log.error("%s", startup_error(e))
        # A signal during the failed attempt is a stop, not a reason to retry.
        return not stopper.requested
    return False


async def _serve(config: Config, bot: Bot, stopper: Stopper) -> None:
    async with bot:
        user = await bot.api.api_get_active_user()
        if user is None:
            raise RuntimeError("no active user after start")
        user_id = user["userId"]
        roster_group_id = await setup.ensure_roster_group(bot.api, user_id, config)
        ctx = BotContext(
            api=bot.api,
            user_id=user_id,
            roster_group_id=roster_group_id,
            config=config,
        )
        _register(bot, ctx)
        groups = await bot.api.api_list_groups(user_id)
        await handlers.reconcile_roster(ctx, groups)
        await business.reconcile_chats(ctx, groups)

        if stopper.requested:
            # A signal arrived during startup; unwind rather than begin serving.
            log.info("stopped during startup")
            return

        server = await health.serve(ctx, config.health) if config.health else None
        try:
            await bot.serve_forever()
        finally:
            if server is not None:
                server.close()
                await server.wait_closed()


def main() -> int:
    parser = argparse.ArgumentParser(prog="support-bot-light")
    parser.add_argument("--config", type=Path, default=Path("config.toml"))
    args = parser.parse_args()

    if not logging.getLogger().handlers:
        logging.basicConfig(
            level=logging.INFO, format="%(asctime)s %(levelname)s %(name)s %(message)s"
        )
    # The core creates its databases with the process umask, and they hold the
    # bot's identity keys. Docker mounts a 0700 directory; a manual install
    # would otherwise put them in the working directory at 0644.
    os.umask(stat.S_IRWXG | stat.S_IRWXO)

    try:
        config = load_config(args.config)
    except ConfigError as e:
        log.error("%s", e)
        return 2
    try:
        asyncio.run(_run(config))
    except ConfigError as e:
        # Raised past load_config only by the health endpoint, which cannot know
        # its port is taken until it binds.
        log.error("%s", e)
        return 2
    except STARTUP_ERRORS as e:
        # Startup rejections the core only reports at first use, such as a
        # display name it will not accept.
        log.error("%s", startup_error(e))
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
