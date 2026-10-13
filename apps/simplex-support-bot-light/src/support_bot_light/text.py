"""Sanitising peer-controlled text before it is rendered."""

from __future__ import annotations

import unicodedata

# mkValidName in src/Simplex/Chat/Library/Commands.hs caps a locally entered
# name at 50 characters. It is not applied to inbound profiles.
MAX_NAME = 50

UNNAMED = "(unnamed)"

# Characters that render as nothing but are neither whitespace nor a control
# category, so `str.split` and `str.isprintable` both let them through. A stock
# client accepts them in a profile name, which makes "ㅤㅤAlice" a
# working impersonation of "Alice".
# Separators the bot's own messages use. A customer chooses their display name,
# and the roster group is the operator's only record of who was added.
SEPARATORS = frozenset("→")

INVISIBLE = frozenset(
    "ᅟᅠㅤﾠ"  # Hangul fillers
    "⠀"  # Braille pattern blank
    "឴឵"  # Khmer inherent vowels
    "⁠﻿"  # word joiner, zero-width no-break space
)


def safe_name(name: str) -> str:
    """Collapse and truncate a display name for rendering.

    The core does not sanitise inbound profiles: a peer's display name reaches
    us verbatim and may contain newlines or run to kilobytes. Rendered as-is it
    forges lines in the roster group and in the log, and can push a message past
    the size the core will send.
    """
    # NFKC folds compatibility forms, so a name cannot hide behind an exotic
    # encoding of an ordinary character.
    collapsed = " ".join(unicodedata.normalize("NFKC", name).split())
    printable = "".join(
        c for c in collapsed if c.isprintable() and c not in INVISIBLE and c not in SEPARATORS
    )
    stripped = printable.strip()
    if not stripped:
        return UNNAMED
    if len(stripped) > MAX_NAME:
        return stripped[: MAX_NAME - 1] + "…"
    return stripped
