"""The bot's command menu: declarations plus conversion to group-preference wire dicts."""

from __future__ import annotations

from collections.abc import Sequence

from simplex_chat import BotCommand
from simplex_chat.types import T

DM = "dm"
LIST = "list"
LEAVE = "leave"
HELP = "help"

COMMANDS: tuple[BotCommand, ...] = (
    BotCommand(keyword=DM, label="Add me to incoming chats"),
    BotCommand(keyword=LIST, label="Who gets invited"),
    BotCommand(keyword=LEAVE, label="Stop adding me"),
    BotCommand(keyword=HELP, label="How this works"),
)


def to_wire(commands: Sequence[BotCommand]) -> list[T.ChatBotCommand]:
    """Convert declarations to `groupPreferences.commands` entries."""
    wire: list[T.ChatBotCommand] = []
    for c in commands:
        entry: T.ChatBotCommand_command = {
            "type": "command",
            "keyword": c.keyword,
            "label": c.label,
        }
        # Omitted rather than empty: the client sends on tap for Nothing, but
        # pastes for Just "".
        if c.params is not None:
            entry["params"] = c.params
        wire.append(entry)
    return wire
