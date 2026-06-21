"""SimpleX Chat wire types — auto-generated from Haskell.

Re-exports the four generated modules as namespaces:

- ``T``    — :mod:`._types`     (records, enums, discriminated unions)
- ``CC``   — :mod:`._commands`  (command TypedDicts + ``<Cmd>_cmd_string`` helpers)
- ``CR``   — :mod:`._responses` (``ChatResponse`` and member TypedDicts)
- ``CEvt`` — :mod:`._events`    (``ChatEvent`` and member TypedDicts)
"""

from . import _commands as CC
from . import _events as CEvt
from . import _responses as CR
from . import _types as T

__all__ = ["T", "CC", "CR", "CEvt"]
