"""SimpleX Chat — Python client library for chat bots."""

from ._version import __version__
from .api import ChatApi, ChatCommandError, ConnReqType, Db, PostgresDb, SqliteDb
from .bot import (
    Bot,
    BotCommand,
    BotProfile,
    ChatMessage,
    CommandHandler,
    EventHandler,
    FileMessage,
    ImageMessage,
    LinkMessage,
    Message,
    MessageHandler,
    Middleware,
    ParsedCommand,
    ReportMessage,
    TextMessage,
    UnknownMessage,
    VideoMessage,
    VoiceMessage,
)
from .core import ChatAPIError, ChatInitError, CryptoArgs, MigrationConfirmation
from . import util as util  # re-export the util namespace

__all__ = [
    "__version__",
    "Bot",
    "BotCommand",
    "BotProfile",
    "ChatAPIError",
    "ChatApi",
    "ChatCommandError",
    "ChatInitError",
    "ChatMessage",
    "CommandHandler",
    "ConnReqType",
    "CryptoArgs",
    "Db",
    "EventHandler",
    "FileMessage",
    "ImageMessage",
    "LinkMessage",
    "Message",
    "MessageHandler",
    "Middleware",
    "MigrationConfirmation",
    "ParsedCommand",
    "PostgresDb",
    "ReportMessage",
    "SqliteDb",
    "TextMessage",
    "UnknownMessage",
    "VideoMessage",
    "VoiceMessage",
    "util",
]
