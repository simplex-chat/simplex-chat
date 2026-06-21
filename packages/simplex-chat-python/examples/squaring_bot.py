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
    commands=[BotCommand(keyword="help", label="Show help")],
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


if __name__ == "__main__":
    bot.run()
