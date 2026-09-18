"""core.chat_migrate_init picks the FFI export by queue_size, with a fake libsimplex."""

from __future__ import annotations

import asyncio
import json
from typing import Any

import pytest

from simplex_chat import core
from simplex_chat.core import ChatInitError, MigrationConfirmation

CTRL = 42


class FakeLib:
    """Records calls; each export writes CTRL to the out-param and returns the JSON result."""

    def __init__(self, result: dict[str, Any]) -> None:
        self.result = json.dumps(result)
        self.calls: list[tuple[str, tuple[Any, ...]]] = []

    def _init(self, name: str, args: tuple[Any, ...]) -> str:
        *call_args, ctrl_ref = args
        self.calls.append((name, tuple(call_args)))
        ctrl_ref._obj.value = CTRL
        return self.result

    def chat_migrate_init(self, *args: Any) -> str:
        return self._init("chat_migrate_init", args)

    def chat_migrate_init_queue(self, *args: Any) -> str:
        return self._init("chat_migrate_init_queue", args)


@pytest.fixture
def fake_lib(monkeypatch: pytest.MonkeyPatch):
    def install(result: dict[str, Any]) -> FakeLib:
        lib = FakeLib(result)
        monkeypatch.setattr(core._native, "lib", lambda: lib)
        monkeypatch.setattr(core, "_read_and_free", lambda ptr: ptr)
        return lib

    return install


def migrate_init(queue_size: int | None = None) -> int:
    return asyncio.run(
        core.chat_migrate_init("/tmp/db", "key", MigrationConfirmation.YES_UP, queue_size)
    )


def test_without_queue_size_uses_chat_migrate_init(fake_lib):
    lib = fake_lib({"type": "ok"})
    assert migrate_init() == CTRL
    assert lib.calls == [("chat_migrate_init", (b"/tmp/db", b"key", b"yesUp"))]


def test_with_queue_size_uses_chat_migrate_init_queue(fake_lib):
    lib = fake_lib({"type": "ok"})
    assert migrate_init(65536) == CTRL
    assert lib.calls == [("chat_migrate_init_queue", (b"/tmp/db", b"key", b"yesUp", 65536))]


def test_invalid_queue_size_result_raises_init_error(fake_lib):
    fake_lib({"type": "invalidQueueSize"})
    with pytest.raises(ChatInitError) as e:
        migrate_init(0)
    assert e.value.db_migration_error == {"type": "invalidQueueSize"}


@pytest.mark.parametrize("queue_size", [2**31, -(2**31) - 1])
def test_queue_size_outside_c_int_is_rejected_before_ffi(fake_lib, queue_size):
    lib = fake_lib({"type": "ok"})
    with pytest.raises(ValueError, match="does not fit C int"):
        migrate_init(queue_size)
    assert lib.calls == []
