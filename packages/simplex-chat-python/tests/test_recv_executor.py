"""ChatApi receives run on a dedicated per-instance thread, not the default pool.

Uses a fake libsimplex (see tests/test_core_migrate_init.py for the pattern):
`core._native.lib` and `core._read_and_free` are monkeypatched so `chat_recv_msg_wait`
sleeps for a controlled time and returns a scripted result.
"""

from __future__ import annotations

import asyncio
import json
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from typing import Any

import pytest

from simplex_chat import ChatApi

RECV_SLEEP = 0.3


class FakeRecvLib:
    """Fake chat_recv_msg_wait: blocks for `sleep` seconds, then returns a scripted result."""

    def __init__(self, sleep: float = RECV_SLEEP, results: list[str] | None = None) -> None:
        self.sleep = sleep
        self._results = iter(results or [])
        self.calls: list[tuple[int, int]] = []  # (ctrl, thread ident)
        self._lock = threading.Lock()

    def chat_recv_msg_wait(self, ctrl: int, wait_us: int) -> str:
        time.sleep(self.sleep)
        with self._lock:
            self.calls.append((ctrl, threading.get_ident()))
        return next(self._results, "")

    def chat_close_store(self, ctrl: int) -> str:
        return ""


@pytest.fixture
def fake_lib(monkeypatch: pytest.MonkeyPatch):
    def install(sleep: float = RECV_SLEEP, results: list[str] | None = None) -> FakeRecvLib:
        lib = FakeRecvLib(sleep=sleep, results=results)
        monkeypatch.setattr("simplex_chat.core._native.lib", lambda: lib)
        monkeypatch.setattr("simplex_chat.core._read_and_free", lambda ptr: ptr)
        return lib

    return install


def _recv_thread_names() -> list[str]:
    return [t.name for t in threading.enumerate() if t.name.startswith("simplex-recv")]


async def test_receives_do_not_use_the_default_executor(fake_lib):
    fake_lib(sleep=RECV_SLEEP)
    loop = asyncio.get_running_loop()
    loop.set_default_executor(ThreadPoolExecutor(max_workers=1))

    apis = [ChatApi(ctrl=i) for i in range(3)]
    recv_tasks = [asyncio.create_task(api.recv_chat_event()) for api in apis]
    await asyncio.sleep(0.05)  # let all three receives claim their own thread

    start = time.monotonic()
    await asyncio.to_thread(lambda: None)
    elapsed = time.monotonic() - start

    await asyncio.gather(*recv_tasks)
    for api in apis:
        await api.close()

    assert elapsed < 0.1


async def test_one_receive_thread_per_chatapi_reused(fake_lib):
    lib = fake_lib(sleep=0.02)
    api = ChatApi(ctrl=1)
    for _ in range(3):
        await api.recv_chat_event()

    idents = {ident for ctrl, ident in lib.calls if ctrl == 1}
    assert len(idents) == 1
    recv_ident = idents.pop()
    thread = next(t for t in threading.enumerate() if t.ident == recv_ident)
    assert thread.name.startswith("simplex-recv")
    assert thread.ident != threading.get_ident()

    other_api = ChatApi(ctrl=2)
    await other_api.recv_chat_event()
    other_idents = {ident for ctrl, ident in lib.calls if ctrl == 2}
    assert other_idents and other_idents != {thread.ident}

    await api.close()
    await other_api.close()


def test_no_thread_until_first_receive():
    api = ChatApi(ctrl=1)
    assert not hasattr(api, "_recv_executor")
    assert _recv_thread_names() == []


async def test_close_shuts_down_the_executor_without_blocking_the_loop(fake_lib):
    fake_lib(sleep=RECV_SLEEP)
    api = ChatApi(ctrl=1)
    recv_task = asyncio.create_task(api.recv_chat_event())
    await asyncio.sleep(0.05)  # let the receive claim its executor thread
    assert _recv_thread_names() != []

    sleep_task = asyncio.create_task(asyncio.sleep(0.01))
    close_task = asyncio.create_task(api.close())

    await asyncio.wait_for(sleep_task, timeout=0.2)
    assert not close_task.done()  # shutdown still waiting on the in-flight receive

    await close_task
    await recv_task

    assert _recv_thread_names() == []


async def test_recv_chat_event_after_close_raises_before_touching_executor(fake_lib):
    fake_lib()
    api = ChatApi(ctrl=1)
    await api.close()
    with pytest.raises(RuntimeError, match="controller not initialized"):
        await api.recv_chat_event()
    assert not hasattr(api, "_recv_executor")


async def test_receive_parses_event_json_and_none_on_timeout(fake_lib):
    event: dict[str, Any] = {"type": "chatItemUpdated", "chatItem": {}}
    fake_lib(sleep=0.01, results=[json.dumps({"result": event}), ""])
    api = ChatApi(ctrl=1)
    try:
        assert await api.recv_chat_event() == event
        assert await api.recv_chat_event() is None
    finally:
        await api.close()
