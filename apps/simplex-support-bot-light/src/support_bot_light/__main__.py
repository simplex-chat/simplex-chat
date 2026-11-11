"""Entry point: load config, start the bot, wire handlers, serve."""

from __future__ import annotations

import argparse
import asyncio
import dataclasses
import logging
import os
import stat
import sys
from pathlib import Path

from simplex_chat import Bot, BotProfile, ChatApi, ChatError, SqliteDb
from simplex_chat.core import ChatInitError
from simplex_chat.types import CEvt

from . import business, commands, handlers, health, setup
from .config import Config, ConfigError, IdName, load_config, parse_id_names
from .context import BotContext
from .text import safe_name

log = logging.getLogger("support_bot_light")

# storeError tag the core returns when a display name is already in use.
DUPLICATE_NAME = "duplicateName"

# ChatInitError is raised only while opening the database, which is why it
# belongs here rather than in the per-command guards elsewhere.
STARTUP_ERRORS = (ChatError, ChatInitError)


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

    @bot.on_command(commands.FEED, group_id=group_id)
    async def _feed(msg, _cmd):
        await handlers.feed(ctx, msg)

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

    @bot.on_event("chatItemsStatusesUpdated")
    async def _statuses(evt: CEvt.ChatItemsStatusesUpdated):
        await handlers.feed_status(ctx, evt)


def startup_error(e: Exception) -> str:
    """What the operator can act on, from an exception that names only a tag.

    The core reports a display name already taken by a contact or group as a
    bare `errorStore`, and the detail the bot needs is in the store error.
    """
    if getattr(e, "store_error_type", None) == DUPLICATE_NAME:
        return (
            "bot.display_name is already taken in this database by a contact, a "
            "group or a past customer; the core keeps every display name unique. "
            "Choose another name."
        )
    command_error = getattr(e, "command_error", None)
    if command_error is not None:
        return command_error
    chat_error = getattr(e, "chat_error", None)
    return f"{e} {chat_error}" if chat_error else str(e)


async def check_broadcasters(api: ChatApi, user_id: int, broadcasters: tuple[IdName, ...]) -> None:
    if not broadcasters:
        return
    contacts = {c["contactId"]: c for c in await api.api_list_contacts(user_id)}
    for b in broadcasters:
        contact = contacts.get(b.id)
        if contact is None:
            raise ConfigError(f"broadcaster not found: ID={b.id}")
        name = contact["profile"]["displayName"]
        if name != b.name:
            raise ConfigError(
                f'broadcaster name mismatch: expected "{b.name}", '
                f'got "{safe_name(name)}" (ID={b.id})'
            )
        log.info("broadcaster validated: %d:%s", b.id, safe_name(b.name))


def bot_profile(config: Config) -> BotProfile:
    return BotProfile(display_name=config.display_name, image=config.image)


def build_bot(config: Config) -> Bot:
    """The bot's identity and address settings.

    business_address is what makes a connection open a group the roster can be
    added to; without it every customer would get a plain direct chat and the
    bot would have nothing to do.

    The profile is applied after the client starts, not by the startup sync, so
    that a name the core refuses does not stop the bot. See `_apply_profile`.
    """
    return Bot(
        profile=bot_profile(config),
        db=SqliteDb(file_prefix=config.db_prefix),
        welcome=config.welcome,
        business_address=True,
        auto_accept=True,
        update_profile=False,
        # The library logs peer display names verbatim; the bot sanitises every
        # name it renders itself, and this is the one path that bypasses it.
        log_contacts=False,
    )


async def _run(config: Config) -> None:
    bot = build_bot(config)
    # Before the client starts: a signal during migrations would otherwise hit
    # the default disposition and kill the process mid-write.
    bot.install_signal_handlers()
    await _serve(config, bot)


async def _apply_profile(bot: Bot) -> None:
    """Apply the configured profile once the database can be reached.

    The core refuses a display name another contact or group holds, and the
    profile update broadcasts to every contact, so it is the startup step most
    likely to fail. Answering customers matters more than a name or an avatar.
    """
    try:
        await bot.sync_profile()
    except ChatError as e:
        log.error("%s", startup_error(e))
        log.warning("Serving without applying the profile change.")


async def _serve(config: Config, bot: Bot) -> None:
    # Not bot.run(): handlers are scoped with group_id=, which is unknown until
    # the roster group is resolved after start.
    async with bot:
        user = await bot.api.api_get_active_user()
        if user is None:
            raise RuntimeError("no active user after start")
        user_id = user["userId"]
        await check_broadcasters(bot.api, user_id, config.broadcasters)
        await _apply_profile(bot)
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

        if bot.stop_requested:
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
    parser.add_argument(
        "--broadcasters",
        type=parse_id_names,
        default=(),
        help="comma-separated ID:name pairs of contacts allowed to use /feed in the roster group",
    )
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
        config = dataclasses.replace(load_config(args.config), broadcasters=args.broadcasters)
    except ConfigError as e:
        log.error("%s", e)
        return 2
    try:
        asyncio.run(_run(config))
    except ConfigError as e:
        # Raised past load_config by the health endpoint, which cannot know its
        # port is taken until it binds, and by the broadcaster check, which needs
        # the database.
        log.error("%s", e)
        return 2
    except STARTUP_ERRORS as e:
        # Startup rejections the core only reports at first use, such as a
        # database it will not open.
        log.error("%s", startup_error(e))
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
