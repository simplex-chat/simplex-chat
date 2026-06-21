"""Internal typed async wrapper around libsimplex's 8 C ABI functions.

Users interact with `Bot` / `ChatApi`. This module is exposed as
`simplex_chat.core` for tests and the api.ChatApi class only.
"""

from __future__ import annotations

import asyncio
import ctypes
import json
from enum import StrEnum
from typing import Any, TypedDict

from . import _native
from .types import T, CR, CEvt


class ChatAPIError(Exception):
    """Raised when chat_send_cmd / chat_recv_msg_wait returns a chat error."""

    def __init__(self, message: str, chat_error: T.ChatError | None = None):
        super().__init__(message)
        self.chat_error = chat_error


class ChatInitError(Exception):
    """Raised when chat_migrate_init returns a DBMigrationResult error."""

    def __init__(self, message: str, db_migration_error: dict[str, Any]):
        super().__init__(message)
        self.db_migration_error = db_migration_error


class MigrationConfirmation(StrEnum):
    YES_UP = "yesUp"
    YES_UP_DOWN = "yesUpDown"
    CONSOLE = "console"
    ERROR = "error"


class CryptoArgs(TypedDict):  # wire-format JSON; camelCase fields
    fileKey: str
    fileNonce: str


def _read_and_free(ptr: int | None) -> str:
    """Copy a Haskell-allocated null-terminated UTF-8 string and free its buffer.

    Mirrors HandleCResult in packages/simplex-chat-nodejs/cpp/simplex.cc:157-165.
    """
    if not ptr:
        raise RuntimeError("null pointer returned from libsimplex")
    try:
        return ctypes.string_at(ptr).decode("utf-8")
    finally:
        _native.libc().free(ctypes.c_void_p(ptr))


async def chat_send_cmd(ctrl: int, cmd: str) -> CR.ChatResponse:
    def _call() -> str:
        ptr = _native.lib().chat_send_cmd(ctrl, cmd.encode("utf-8"))
        return _read_and_free(ptr)

    raw = await asyncio.to_thread(_call)
    parsed = json.loads(raw)
    if "result" in parsed and isinstance(parsed["result"], dict):
        return parsed["result"]  # type: ignore[return-value]
    err = parsed.get("error")
    if isinstance(err, dict):
        raise ChatAPIError(f"chat command error: {err.get('type')}", err)  # type: ignore[arg-type]
    raise ChatAPIError(f"invalid chat command result: {raw[:200]}")


async def chat_recv_msg_wait(ctrl: int, wait_us: int = 500_000) -> CEvt.ChatEvent | None:
    def _call() -> str:
        # On timeout, the C side returns a non-NULL pointer to a single NUL byte
        # (see Mobile.hs `fromMaybe ""`), so `_read_and_free` returns "" — no
        # NULL-pointer guard is needed here.
        ptr = _native.lib().chat_recv_msg_wait(ctrl, wait_us)
        return _read_and_free(ptr)

    raw = await asyncio.to_thread(_call)
    if not raw:
        return None
    parsed = json.loads(raw)
    if "result" in parsed and isinstance(parsed["result"], dict):
        return parsed["result"]  # type: ignore[return-value]
    err = parsed.get("error")
    if isinstance(err, dict):
        raise ChatAPIError(f"chat event error: {err.get('type')}", err)  # type: ignore[arg-type]
    raise ChatAPIError(f"invalid chat event: {raw[:200]}")


async def chat_migrate_init(db_path: str, db_key: str, confirm: MigrationConfirmation) -> int:
    """Initialize chat controller. Returns opaque ctrl pointer as Python int."""

    def _call() -> tuple[int, str]:
        ctrl = ctypes.c_void_p()
        ptr = _native.lib().chat_migrate_init(
            db_path.encode("utf-8"),
            db_key.encode("utf-8"),
            confirm.encode("utf-8"),
            ctypes.byref(ctrl),
        )
        return (ctrl.value or 0, _read_and_free(ptr))

    ctrl_val, raw = await asyncio.to_thread(_call)
    parsed = json.loads(raw)
    if parsed.get("type") == "ok":
        if not ctrl_val:
            # ABI invariant: type=="ok" → out-param written. Defensive guard so a
            # broken libsimplex doesn't hand us a NULL controller that would only
            # crash on first use much later.
            raise RuntimeError("chat_migrate_init returned ok but did not set ctrl pointer")
        return ctrl_val
    raise ChatInitError(
        "Database or migration error (see db_migration_error)",
        parsed,
    )


async def chat_close_store(ctrl: int) -> None:
    def _call() -> str:
        ptr = _native.lib().chat_close_store(ctrl)
        return _read_and_free(ptr)

    res = await asyncio.to_thread(_call)
    if res:
        raise RuntimeError(res)


async def chat_write_file(ctrl: int, path: str, data: bytes) -> CryptoArgs:
    def _call() -> str:
        ptr = _native.lib().chat_write_file(ctrl, path.encode("utf-8"), data, len(data))
        return _read_and_free(ptr)

    raw = await asyncio.to_thread(_call)
    return _crypto_args_result(raw)


async def chat_read_file(path: str, args: CryptoArgs) -> bytes:
    def _call() -> bytes:
        ptr = _native.lib().chat_read_file(
            path.encode("utf-8"),
            args["fileKey"].encode("utf-8"),
            args["fileNonce"].encode("utf-8"),
        )
        if not ptr:
            raise RuntimeError("chat_read_file returned null")
        addr = ctypes.cast(ptr, ctypes.c_void_p).value
        assert addr is not None  # `if not ptr` above already filtered NULL
        try:
            status = ctypes.cast(addr, ctypes.POINTER(ctypes.c_uint8))[0]
            if status == 1:
                msg = ctypes.string_at(addr + 1).decode("utf-8")
                raise RuntimeError(msg)
            if status != 0:
                raise RuntimeError(f"unexpected status {status} from chat_read_file")
            # `addr + 1` is unaligned for a uint32 read. On the supported platforms
            # (linux-x86_64, linux-aarch64, macos-aarch64, windows-x86_64) this is
            # silently handled; matches the Node.js binding (cpp/simplex.cc:344).
            length = ctypes.cast(addr + 1, ctypes.POINTER(ctypes.c_uint32))[0]
            return ctypes.string_at(addr + 5, length)
        finally:
            _native.libc().free(ctypes.c_void_p(addr))

    return await asyncio.to_thread(_call)


async def chat_encrypt_file(ctrl: int, src: str, dst: str) -> CryptoArgs:
    def _call() -> str:
        ptr = _native.lib().chat_encrypt_file(ctrl, src.encode("utf-8"), dst.encode("utf-8"))
        return _read_and_free(ptr)

    return _crypto_args_result(await asyncio.to_thread(_call))


async def chat_decrypt_file(src: str, args: CryptoArgs, dst: str) -> None:
    def _call() -> str:
        ptr = _native.lib().chat_decrypt_file(
            src.encode("utf-8"),
            args["fileKey"].encode("utf-8"),
            args["fileNonce"].encode("utf-8"),
            dst.encode("utf-8"),
        )
        return _read_and_free(ptr)

    res = await asyncio.to_thread(_call)
    if res:
        raise RuntimeError(res)


def _crypto_args_result(raw: str) -> CryptoArgs:
    parsed = json.loads(raw)
    if parsed.get("type") == "result":
        return parsed["cryptoArgs"]
    if parsed.get("type") == "error":
        raise RuntimeError(parsed.get("writeError", "unknown write error"))
    raise RuntimeError(f"unexpected result: {raw[:200]}")
