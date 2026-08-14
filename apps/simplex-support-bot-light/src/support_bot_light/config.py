"""Load and validate `config.toml`."""

from __future__ import annotations

import base64
import stat
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Any, get_args

from simplex_chat.types import T

DEFAULT_MEMBER_ROLE: T.GroupMemberRole = "owner"
MEMBER_ROLES: tuple[str, ...] = get_args(T.GroupMemberRole)

# maxProfileImageSize in src/Simplex/Chat/Library/Commands.hs. Measured against
# the whole data URI, not the raw file.
MAX_PROFILE_IMAGE_SIZE = 12500

# Raw bytes that still fit once base64 and the "data:image/png;base64," prefix
# are added. Checked before the file is read.
# The welcome is sent as a chat message, so it is held below the core's wire
# limit (maxEncodedMsgLength) with room to spare rather than at it.
MAX_WELCOME_BYTES = 12000

MAX_IMAGE_BYTES = (MAX_PROFILE_IMAGE_SIZE - 22) // 4 * 3

# On unless switched off, so a deployment is monitorable without being
# configured for it. Loopback, because the endpoint has no authentication.
DEFAULT_HEALTH_HOST = "127.0.0.1"
DEFAULT_HEALTH_PORT = 8080
MAX_PORT = 65535

# Tag in the data:image/<tag>;base64, prefix. The core recognises jpg, not jpeg.
IMAGE_EXTENSION_TAGS = {".png": "png", ".jpg": "jpg", ".jpeg": "jpg"}


class ConfigError(ValueError):
    """`config.toml` is missing, malformed, or has an invalid value."""


@dataclass(frozen=True, slots=True)
class Health:
    """Where the monitoring endpoint listens."""

    host: str
    port: int
    # True when the config names the port. A port the operator chose has to
    # work; the default must never be what keeps the bot from starting.
    configured: bool = False


@dataclass(frozen=True, slots=True)
class Config:
    """Validated settings loaded from `config.toml`."""

    display_name: str
    db_prefix: str
    welcome: str
    group_name: str
    member_role: T.GroupMemberRole
    image: str | None = None
    health: Health | None = None


def load_config(path: Path) -> Config:
    """Read and validate `config.toml` at `path`, raising `ConfigError` on any problem."""
    try:
        raw = tomllib.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as e:
        # Names, not a command: under Docker this directory is mounted
        # read-only, so the copy is made on the host.
        template = path.with_name(path.name + ".example")
        hint = f" — copy {template.name} to {path.name} and edit it" if template.exists() else ""
        raise ConfigError(f"config file not found: {path}{hint}") from e
    except tomllib.TOMLDecodeError as e:
        raise ConfigError(f"invalid TOML in {path}: {e}") from e
    except UnicodeDecodeError as e:
        raise ConfigError(f"config file is not UTF-8: {path}") from e
    except OSError as e:
        raise ConfigError(f"config file could not be read ({path}): {e}") from e

    bot = _section(raw, "bot")
    roster = _section(raw, "roster")
    role = roster.get("member_role", DEFAULT_MEMBER_ROLE)
    if role not in MEMBER_ROLES:
        raise ConfigError(
            f"roster.member_role must be one of {', '.join(MEMBER_ROLES)}, got {role!r}"
        )
    return Config(
        display_name=_text(bot, "bot", "display_name"),
        db_prefix=_text(bot, "bot", "db_prefix"),
        welcome=_bounded_text(bot, "bot", "welcome", MAX_WELCOME_BYTES),
        group_name=_text(roster, "roster", "group_name"),
        member_role=role,
        image=_image(bot, path.parent),
        health=_health(raw),
    )


def _health(raw: dict[str, Any]) -> Health | None:
    """Where the endpoint listens, or None when `health.enabled` switches it off."""
    health = raw.get("health", {})
    if not isinstance(health, dict):
        raise ConfigError("[health] must be a section")
    enabled = health.get("enabled", True)
    if not isinstance(enabled, bool):
        raise ConfigError(f"health.enabled must be true or false, got {enabled!r}")
    if not enabled:
        return None
    port = health.get("port", DEFAULT_HEALTH_PORT)
    # bool is an int, and TOML has booleans.
    if not isinstance(port, int) or isinstance(port, bool) or not 1 <= port <= MAX_PORT:
        raise ConfigError(f"health.port must be an integer between 1 and {MAX_PORT}, got {port!r}")
    host = health.get("host", DEFAULT_HEALTH_HOST)
    if not isinstance(host, str) or not host.strip():
        raise ConfigError("health.host must be a non-empty string")
    # Either key means the operator chose where it listens, and a bind failure
    # there is a misconfiguration rather than a coincidence.
    return Health(host=host, port=port, configured=bool({"host", "port"} & health.keys()))


def _section(raw: dict[str, Any], name: str) -> dict[str, Any]:
    section = raw.get(name)
    if not isinstance(section, dict):
        raise ConfigError(f"missing [{name}] section")
    return section


def _text(section: dict[str, Any], section_name: str, key: str) -> str:
    value = section.get(key)
    if not isinstance(value, str) or not value.strip():
        raise ConfigError(f"{section_name}.{key} must be a non-empty string")
    return value


def _bounded_text(section: dict[str, Any], section_name: str, key: str, max_bytes: int) -> str:
    value = _text(section, section_name, key)
    if len(value.encode()) > max_bytes:
        raise ConfigError(
            f"{section_name}.{key} is too long: {len(value.encode())} bytes exceeds "
            f"the {max_bytes} the core will send; shorten it"
        )
    return value


def _image(bot: dict[str, Any], config_dir: Path) -> str | None:
    """Encode `bot.image` (a file path) as a profile-image data URI, or `None`
    if the key is absent. Relative paths resolve against `config_dir` — the
    directory containing `config.toml` — not the process's working directory.
    """
    if "image" not in bot:
        return None
    value = _text(bot, "bot", "image")

    image_path = Path(value)
    if not image_path.is_absolute():
        image_path = config_dir / image_path

    extension = image_path.suffix.lower()
    tag = IMAGE_EXTENSION_TAGS.get(extension)
    if tag is None:
        supported = ", ".join(sorted(IMAGE_EXTENSION_TAGS))
        raise ConfigError(
            f"bot.image has unsupported extension {extension!r} ({image_path}); "
            f"supported extensions: {supported}"
        )

    # Inspect before reading: a FIFO would block startup indefinitely and a
    # character device such as /dev/zero would exhaust memory.
    try:
        info = image_path.stat()
    except FileNotFoundError as e:
        raise ConfigError(f"bot.image file not found: {image_path}") from e
    except OSError as e:
        raise ConfigError(f"bot.image could not be read ({image_path}): {e}") from e

    if not stat.S_ISREG(info.st_mode):
        raise ConfigError(f"bot.image is not a regular file: {image_path}")
    if info.st_size > MAX_IMAGE_BYTES:
        raise ConfigError(
            f"bot.image is too large: {info.st_size} bytes exceeds the {MAX_IMAGE_BYTES} "
            f"a {MAX_PROFILE_IMAGE_SIZE}-character data URI can hold; shrink the image "
            "(a 128x128 avatar) and try again"
        )

    try:
        data = image_path.read_bytes()
    except OSError as e:
        raise ConfigError(f"bot.image could not be read ({image_path}): {e}") from e

    # The core rejects an empty image file rather than broadcasting a profile
    # with an undecodable data URI.
    if not data:
        raise ConfigError(f"bot.image file is empty: {image_path}")

    encoded = base64.b64encode(data).decode("ascii")
    data_uri = f"data:image/{tag};base64,{encoded}"
    if len(data_uri) > MAX_PROFILE_IMAGE_SIZE:
        raise ConfigError(
            f"bot.image is too large: encoded size {len(data_uri)} exceeds the "
            f"{MAX_PROFILE_IMAGE_SIZE}-character limit the core enforces on profile "
            "images; shrink the image (e.g. to a 96x96 or 128x128 avatar) and try again"
        )
    return data_uri
