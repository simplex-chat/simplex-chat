"""Single source of truth for both the Python package version and the
simplex-chat-libs release tag we depend on.

Bump both together for normal releases. For wrapper-only fixes use a PEP 440
post-release: __version__ = "6.5.2.post1", LIBS_VERSION unchanged.
"""

__version__ = "6.5.6"  # PEP 440 — read by hatchling for wheel metadata
LIBS_VERSION = "6.5.6"  # simplex-chat-libs release tag (no 'v' prefix)
