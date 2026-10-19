"""Squaring bot — replies to every number with its square.

Run with the simplex-chat package installed:

    python examples/squaring_bot.py

Sends `n * n = ...` for any text message that parses as a number; falls
back to a hint for non-number messages; responds to `/help` with usage.
"""

from __future__ import annotations

import re

from simplex_chat import (
    Bot,
    BotCommand,
    BotProfile,
    Message,
    ParsedCommand,
    SqliteDb,
    TextMessage,
)

bot = Bot(
    profile=BotProfile(display_name="Squaring bot"),
    db=SqliteDb(file_prefix="./squaring_bot"),
    welcome="Send me a number, I'll square it.",
    commands=[
        # `params=None` (default): the client SENDS `/help` immediately
        # when the user taps it in the commands menu.
        BotCommand(keyword="help", label="Show help"),
        # `params="<number>"`: the client PASTES `/square <number>`
        # into the input box and positions the cursor at the end. The
        # user replaces `<number>` with the actual number and sends.
        BotCommand(keyword="square", label="Square a number", params="<number>"),
    ],
)

NUMBER_RE = re.compile(r"^-?\d+(\.\d+)?$")


@bot.on_message(content_type="text", text=NUMBER_RE)
async def square(msg: TextMessage) -> None:
    n = float(msg.text or "0")
    await msg.reply(f"{n} * {n} = {n * n}")


@bot.on_message(content_type="text")
async def fallback(msg: Message) -> None:
    await msg.reply("Send me a number, like 7 or 3.14.")


@bot.on_command("help")
async def help_cmd(msg: Message, _cmd: ParsedCommand) -> None:
    await msg.reply("Send a number, I'll square it.")


@bot.on_command("square")
async def square_cmd(msg: Message, cmd: ParsedCommand) -> None:
    """Demonstrates the `params` flow: `cmd.args` is the trimmed text
    AFTER `/square`. When the user tapped the menu entry above, the
    client pasted `/square <number>` and the user replaced
    `<number>` with the actual value before sending."""
    try:
        n = float(cmd.args)
    except ValueError:
        await msg.reply(f"Usage: /square <number> (got {cmd.args!r})")
        return
    await msg.reply(f"{n} * {n} = {n * n}")


if __name__ == "__main__":
    bot.run()
