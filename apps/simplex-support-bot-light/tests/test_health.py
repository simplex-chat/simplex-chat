"""The monitoring endpoint: real sockets, no libsimplex."""

import asyncio

import pytest
from simplex_chat.core import ChatAPIError

from support_bot_light import health
from support_bot_light.config import Config, ConfigError, Health
from support_bot_light.context import BotContext
from tests.conftest import ROSTER_GROUP_ID, USER_ID

CONFIG = Config("Support", "./x", "hi", "Invite roster", "owner")


class ProbeApi:
    """The one call the probe makes, with the outcomes it has to distinguish."""

    def __init__(self, error: bool = False, delay: float = 0.0):
        self.error = error
        self.delay = delay
        self.calls = 0

    async def api_list_members(self, group_id: int) -> list[dict]:
        self.calls += 1
        assert group_id == ROSTER_GROUP_ID
        if self.delay:
            await asyncio.sleep(self.delay)
        if self.error:
            raise ChatAPIError("core is unhappy", {"type": "chatCmdError"})
        return []


def context(api) -> BotContext:
    return BotContext(api=api, user_id=USER_ID, roster_group_id=ROSTER_GROUP_ID, config=CONFIG)


async def request(server: asyncio.Server, line: str) -> str:
    """Send one request line to a running endpoint and read the whole reply."""
    port = server.sockets[0].getsockname()[1]
    reader, writer = await asyncio.open_connection("127.0.0.1", port)
    try:
        writer.write(f"{line}\r\nHost: localhost\r\n\r\n".encode())
        await writer.drain()
        return (await reader.read()).decode("latin-1")
    finally:
        writer.close()
        await writer.wait_closed()


async def endpoint(api) -> asyncio.Server:
    # Port 0: the OS picks a free one, so tests never collide.
    return await health.serve(context(api), Health(host="127.0.0.1", port=0))


async def test_reports_ok_while_the_core_answers():
    api = ProbeApi()
    server = await endpoint(api)
    try:
        reply = await request(server, "GET /health HTTP/1.1")
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith("HTTP/1.1 200 OK")
    assert reply.endswith('{"status":"ok"}\n')
    assert api.calls == 1


async def test_reports_unavailable_when_the_core_errors():
    server = await endpoint(ProbeApi(error=True))
    try:
        reply = await request(server, "GET /health HTTP/1.1")
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith("HTTP/1.1 503 Service Unavailable")


async def test_a_slow_core_times_out_rather_than_hanging(monkeypatch):
    monkeypatch.setattr(health, "PROBE_TIMEOUT", 0.05)
    api = ProbeApi(delay=5)
    server = await endpoint(api)
    try:
        reply = await asyncio.wait_for(request(server, "GET /health HTTP/1.1"), 2)
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith("HTTP/1.1 503")


async def test_a_concurrent_request_does_not_start_a_second_probe(monkeypatch):
    monkeypatch.setattr(health, "PROBE_TIMEOUT", 0.5)
    api = ProbeApi(delay=0.3)
    server = await endpoint(api)
    try:
        first = asyncio.create_task(request(server, "GET /health HTTP/1.1"))
        await asyncio.sleep(0.05)
        second = await request(server, "GET /health HTTP/1.1")
        assert (await first).startswith("HTTP/1.1 200 OK")
    finally:
        server.close()
        await server.wait_closed()
    assert second.startswith("HTTP/1.1 200 OK")  # it waits on the same probe
    assert api.calls == 1


async def test_polling_a_stalled_core_never_starts_a_second_query(monkeypatch):
    # Each abandoned query keeps a worker in the loop's default executor, which
    # the receive loop also uses: a query per poll would take the bot's own
    # traffic down with the core.
    monkeypatch.setattr(health, "PROBE_TIMEOUT", 0.05)
    api = ProbeApi(delay=3)
    server = await endpoint(api)
    try:
        for _ in range(5):
            reply = await request(server, "GET /health HTTP/1.1")
            assert reply.startswith("HTTP/1.1 503")
        assert api.calls == 1  # one query outstanding, not five
    finally:
        server.close()
        await server.wait_closed()


async def test_the_next_poll_after_recovery_starts_a_fresh_query(monkeypatch):
    monkeypatch.setattr(health, "PROBE_TIMEOUT", 0.05)
    api = ProbeApi(delay=0.2)
    server = await endpoint(api)
    try:
        assert (await request(server, "GET /health HTTP/1.1")).startswith("HTTP/1.1 503")
        await asyncio.sleep(0.3)  # the abandoned query completes
        api.delay = 0  # the core recovers
        assert (await request(server, "GET /health HTTP/1.1")).startswith("HTTP/1.1 200")
    finally:
        server.close()
        await server.wait_closed()
    assert api.calls == 2


async def test_a_core_that_raises_anything_reports_unavailable():
    # A malformed reply or a missing controller is what this exists to report,
    # and neither arrives as a chat error.
    class Broken(ProbeApi):
        async def api_list_members(self, group_id: int) -> list[dict]:
            self.calls += 1
            raise RuntimeError("controller not initialized")

    api = Broken()
    server = await endpoint(api)
    try:
        reply = await request(server, "GET /health HTTP/1.1")
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith("HTTP/1.1 503")


async def test_an_oversized_request_line_is_answered():
    api = ProbeApi()
    server = await endpoint(api)
    try:
        reply = await request(server, "GET /" + "x" * (health.MAX_REQUEST_BYTES + 10))
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith("HTTP/1.1 400")
    assert api.calls == 0


async def test_head_is_answered_without_a_body():
    api = ProbeApi()
    server = await endpoint(api)
    try:
        reply = await request(server, "HEAD /health HTTP/1.1")
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith("HTTP/1.1 200 OK")
    assert "{" not in reply
    assert api.calls == 1


@pytest.mark.parametrize(
    ("line", "status"),
    [
        ("GET / HTTP/1.1", "404"),
        ("GET /healthz HTTP/1.1", "404"),
        ("POST /health HTTP/1.1", "405"),
        ("nonsense", "404"),
    ],
)
async def test_only_get_on_the_health_path_is_answered(line, status):
    api = ProbeApi()
    server = await endpoint(api)
    try:
        reply = await request(server, line)
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith(f"HTTP/1.1 {status}")
    assert api.calls == 0


async def test_a_query_string_still_matches_the_path():
    api = ProbeApi()
    server = await endpoint(api)
    try:
        reply = await request(server, "GET /health?from=monitor HTTP/1.1")
    finally:
        server.close()
        await server.wait_closed()
    assert reply.startswith("HTTP/1.1 200 OK")


async def test_a_configured_port_already_in_use_stops_the_bot():
    api = ProbeApi()
    taken = await endpoint(api)
    port = taken.sockets[0].getsockname()[1]
    try:
        with pytest.raises(ConfigError, match="cannot listen"):
            await health.serve(context(api), Health("127.0.0.1", port, configured=True))
    finally:
        taken.close()
        await taken.wait_closed()


async def test_the_default_port_being_in_use_does_not_stop_the_bot():
    # Nothing asked for port 8080; an unrelated service on it is not a reason to
    # refuse to answer chats.
    api = ProbeApi()
    taken = await endpoint(api)
    port = taken.sockets[0].getsockname()[1]
    try:
        assert await health.serve(context(api), Health("127.0.0.1", port)) is None
    finally:
        taken.close()
        await taken.wait_closed()
