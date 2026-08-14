"""The exceptions a chat command can raise."""

from __future__ import annotations

from simplex_chat import ChatCommandError
from simplex_chat.core import ChatAPIError, ChatInitError

# Both must be named. `core.chat_send_cmd` raises ChatAPIError for anything the
# core rejects, which is nearly every real failure; `api.py` raises
# ChatCommandError only when a command succeeds with an unexpected response
# shape. They are siblings, not subclasses, so catching one misses the other.
CHAT_ERRORS = (ChatCommandError, ChatAPIError)

# Raised only while opening the database, so it belongs at the entry point
# rather than in the per-command guards.
STARTUP_ERRORS = (*CHAT_ERRORS, ChatInitError)
