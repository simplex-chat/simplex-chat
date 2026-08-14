"""Optional HTTP endpoint reporting whether the core still answers."""

from __future__ import annotations

import asyncio
import contextlib
import logging

from .config import ConfigError, Health
from .context import BotContext
from .errors import CHAT_ERRORS

log = logging.getLogger(__name__)

PATH = "/health"

# The probe issues a real command, so it has to give up before the monitor does.
PROBE_TIMEOUT = 5.0

# Larger than any request a monitor sends, and the cap on what is read.
MAX_REQUEST_BYTES = 4096
READ_TIMEOUT = 5.0


def _head(status: str, length: int) -> bytes:
    return (
        f"HTTP/1.1 {status}\r\n"
        "Content-Type: application/json\r\n"
        f"Content-Length: {length}\r\n"
        "Connection: close\r\n\r\n"
    ).encode()


def _response(status: str, payload: str) -> tuple[bytes, bytes]:
    """(head, body). HEAD answers with the head alone, as HTTP requires."""
    body = f'{{"status":"{payload}"}}\n'.encode()
    return _head(status, len(body)), body


OK = _response("200 OK", "ok")
UNAVAILABLE = _response("503 Service Unavailable", "unavailable")
NOT_FOUND = _response("404 Not Found", "not found")
NOT_ALLOWED = _response("405 Method Not Allowed", "method not allowed")
BAD_REQUEST = _response("400 Bad Request", "bad request")


class Probe:
    """One outstanding query at a time, however often the endpoint is polled.

    `asyncio.wait_for` bounds the wait, not the work: the FFI call it abandons
    keeps a worker thread in the loop's default executor until the core answers.
    Starting a fresh one per poll would exhaust that executor — as few as six
    threads on a small container — and the receive loop reads events through the
    same executor, so a stalled core would take the bot's own traffic down with
    it. The task is therefore reused rather than replaced, and never cancelled.
    """

    def __init__(self, ctx: BotContext) -> None:
        self._ctx = ctx
        self._task: asyncio.Task[bool] | None = None

    async def check(self) -> bool:
        """Whether the core answered within PROBE_TIMEOUT."""
        task = self._task
        if task is None or task.done():
            task = asyncio.create_task(self._query())
            self._task = task
        done, _pending = await asyncio.wait({task}, timeout=PROBE_TIMEOUT)
        if not done:
            log.warning("health probe still waiting after %ss", PROBE_TIMEOUT)
            return False
        return task.result()

    async def _query(self) -> bool:
        """Query the roster group. Never raises, whatever the core does.

        Reaching the process proves only that the event loop runs. This reads
        the database, so it also waits on the store lock every other operation
        takes — unlike `/u`, which the core answers from memory and which would
        report healthy while a transaction was wedged. It stays small: the
        roster group holds the people who answer, not customers.
        """
        try:
            await self._ctx.api.api_list_members(self._ctx.roster_group_id)
        except CHAT_ERRORS:
            log.warning("health probe failed", exc_info=True)
            return False
        except Exception:
            # A malformed reply or a controller that is gone are exactly what
            # this endpoint exists to report, and both arrive as something other
            # than a chat error.
            log.warning("health probe could not reach the core", exc_info=True)
            return False
        return True


async def _handle(
    probe: Probe,
    reader: asyncio.StreamReader,
    writer: asyncio.StreamWriter,
) -> None:
    try:
        try:
            line = await asyncio.wait_for(reader.readline(), READ_TIMEOUT)
        except ValueError:
            # Longer than MAX_REQUEST_BYTES: answered rather than dropped, so a
            # monitor sees a reason.
            _write(writer, BAD_REQUEST, body=True)
            await writer.drain()
            return

        request = line.decode("latin-1").split()
        method = request[0] if request else ""
        if len(request) < 2 or request[1].split("?")[0] != PATH:
            _write(writer, NOT_FOUND, body=True)
        elif method not in ("GET", "HEAD"):
            _write(writer, NOT_ALLOWED, body=True)
        else:
            _write(writer, OK if await probe.check() else UNAVAILABLE, body=method == "GET")
        await writer.drain()
    except (TimeoutError, OSError):
        # A client that stopped sending, or went away mid-response.
        log.debug("health request dropped", exc_info=True)
    finally:
        writer.close()
        with contextlib.suppress(OSError):
            await writer.wait_closed()


def _write(writer: asyncio.StreamWriter, response: tuple[bytes, bytes], body: bool) -> None:
    head, payload = response
    writer.write(head + payload if body else head)


async def serve(ctx: BotContext, config: Health) -> asyncio.Server | None:
    """Start the endpoint, or None when the default port is already taken.

    A port the config names has to work: monitoring that silently failed to
    listen reads as health. The default port is different — nothing about it was
    asked for, so an unrelated service on it must not keep the bot from running.
    """
    probe = Probe(ctx)

    async def handle(reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> None:
        await _handle(probe, reader, writer)

    try:
        server = await asyncio.start_server(
            handle, config.host, config.port, limit=MAX_REQUEST_BYTES
        )
    except OSError as e:
        if config.configured:
            raise ConfigError(
                f"health endpoint cannot listen on {config.host}:{config.port}: {e}"
            ) from e
        log.warning(
            "No health endpoint: the default %s:%s could not be bound (%s). Set "
            "health.host or health.port, or health.enabled = false.",
            config.host,
            config.port,
            e,
        )
        return None
    log.info("Health endpoint: http://%s:%s%s", config.host, config.port, PATH)
    return server
