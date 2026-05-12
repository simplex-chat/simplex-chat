"""Native libsimplex loader: platform detection, lazy download, ctypes setup.

Internal — users interact with `Bot` / `ChatApi`, never with this module.
"""

from __future__ import annotations

import ctypes
import errno
import os
import platform
import sys
import tempfile
import threading
import urllib.request
import zipfile
from ctypes import POINTER, c_char_p, c_int, c_uint8, c_void_p
from pathlib import Path
from typing import Literal

from ._version import LIBS_VERSION

Backend = Literal["sqlite", "postgres"]

_GITHUB_REPO = "simplex-chat/simplex-chat-libs"

_PLATFORM_MAP = {
    "linux": ("linux", {"x86_64": "x86_64", "aarch64": "aarch64"}),
    "darwin": ("macos", {"x86_64": "x86_64", "arm64": "aarch64"}),
    "win32": ("windows", {"AMD64": "x86_64", "x86_64": "x86_64"}),
}

_LIBNAME = {"linux": "libsimplex.so", "darwin": "libsimplex.dylib", "win32": "libsimplex.dll"}

SUPPORTED = (
    "linux-x86_64",
    "linux-aarch64",
    "macos-x86_64",
    "macos-aarch64",
    "windows-x86_64",
)


def _platform_tag() -> str:
    info = _PLATFORM_MAP.get(sys.platform)
    if not info:
        raise RuntimeError(f"Unsupported platform: {sys.platform}")
    sysname, archs = info
    arch = archs.get(platform.machine())
    if not arch:
        raise RuntimeError(f"Unsupported architecture: {sys.platform}/{platform.machine()}")
    tag = f"{sysname}-{arch}"
    if tag not in SUPPORTED:
        raise RuntimeError(f"Unsupported combination: {tag}; supported: {SUPPORTED}")
    return tag


def _libname() -> str:
    return _LIBNAME[sys.platform]


def _libs_url(backend: Backend) -> str:
    suffix = "-postgres" if backend == "postgres" else ""
    return (
        f"https://github.com/{_GITHUB_REPO}/releases/download/"
        f"v{LIBS_VERSION}/simplex-chat-libs-{_platform_tag()}{suffix}.zip"
    )


def _cache_root() -> Path:
    if sys.platform == "darwin":
        return Path.home() / "Library" / "Caches" / "simplex-chat"
    if sys.platform == "win32":
        return Path(os.environ["LOCALAPPDATA"]) / "simplex-chat"
    base = os.environ.get("XDG_CACHE_HOME") or str(Path.home() / ".cache")
    return Path(base) / "simplex-chat"


def _resolve_libs_dir(backend: Backend) -> Path:
    if override := os.environ.get("SIMPLEX_LIBS_DIR"):
        return Path(override)
    if backend == "postgres" and _platform_tag() != "linux-x86_64":
        raise RuntimeError(
            "postgres backend is only supported on linux-x86_64; "
            f"current platform is {_platform_tag()}"
        )
    target = _cache_root() / f"v{LIBS_VERSION}" / backend
    if not (target / _libname()).exists():
        _download(target, backend)
    return target


_DOWNLOAD_CHUNK = 1 << 16  # 64 KiB


def _stream_to_file(url: str, dest: Path, *, timeout: float = 60.0) -> None:
    """Stream `url` → `dest`, printing a carriage-return progress bar.

    `timeout` is per-request; we don't touch `socket.setdefaulttimeout`
    so other socket users in the same process aren't affected.
    """
    with urllib.request.urlopen(url, timeout=timeout) as resp:  # noqa: S310 - https://github.com/...
        total = int(resp.headers.get("Content-Length") or 0)
        received = 0
        with dest.open("wb") as out:
            while chunk := resp.read(_DOWNLOAD_CHUNK):
                out.write(chunk)
                received += len(chunk)
                if total > 0:
                    pct = min(100, received * 100 // total)
                    msg = f"\r  download: {received >> 20} / {total >> 20} MiB ({pct}%)"
                else:
                    msg = f"\r  download: {received >> 20} MiB"
                print(msg, end="", file=sys.stderr, flush=True)
        print("", file=sys.stderr, flush=True)  # newline after final progress line


def _download(target: Path, backend: Backend) -> None:
    """Download libs zip → atomic rename into `target`. Concurrent processes safe.

    Atomicity strategy: each process extracts to its own sibling tempdir on the same
    filesystem, then `os.rename` the `libs/` subdir to `target`. POSIX `os.rename`
    onto a NON-EXISTENT path is atomic; if the target exists (another process won
    the race), `os.rename` fails on most platforms — we then verify the winner has
    what we need and proceed. NEVER rmtree the target: that creates a TOCTOU
    window where another process is reading/loading the file we're deleting.
    """
    target.parent.mkdir(parents=True, exist_ok=True)
    url = _libs_url(backend)
    print(
        f"Downloading libsimplex ({_platform_tag()}, {backend}) v{LIBS_VERSION} from {url} ...",
        file=sys.stderr,
        flush=True,
    )
    with tempfile.TemporaryDirectory(dir=target.parent) as tmp:
        zip_path = Path(tmp) / "libs.zip"
        _stream_to_file(url, zip_path, timeout=60.0)
        with zipfile.ZipFile(zip_path) as zf:
            zf.extractall(tmp)
        # zip layout: <tmp>/libs/libsimplex.* + libHS*.*
        extracted_libs = Path(tmp) / "libs"
        if not extracted_libs.is_dir():
            raise RuntimeError(f"libs/ missing from {_libs_url(backend)}")
        try:
            os.rename(extracted_libs, target)
        except OSError as e:
            # EEXIST / ENOTEMPTY mean another process won the race — fall through
            # and check that the winner left a usable libsimplex behind. Anything
            # else (ENOSPC, EACCES, EROFS, Windows codes mapped to None) is a real
            # failure and must propagate. Same VERSION cached → same content →
            # safe to proceed once we've confirmed the file is there.
            if e.errno not in (errno.EEXIST, errno.ENOTEMPTY):
                raise
            if not (target / _libname()).exists():
                raise RuntimeError(
                    f"another process partially populated {target} but libsimplex "
                    f"is missing; remove the directory manually and retry"
                ) from e


_lock = threading.Lock()
_lib: ctypes.CDLL | None = None
_libc: ctypes.CDLL | None = None
_backend: Backend | None = None


def _load_libc() -> ctypes.CDLL:
    if sys.platform == "win32":
        return ctypes.CDLL("msvcrt")
    return ctypes.CDLL(None)  # libc on POSIX is the process's own symbol table


def _setup_signatures(lib: ctypes.CDLL) -> None:
    """Declare argtypes/restype for the 8 chat_* functions exported by libsimplex.

    All result strings come back as raw c_void_p so the caller can free them
    after copying — matches HandleCResult in cpp/simplex.cc:157-165.
    """
    lib.chat_migrate_init.argtypes = [c_char_p, c_char_p, c_char_p, POINTER(c_void_p)]
    lib.chat_migrate_init.restype = c_void_p
    lib.chat_close_store.argtypes = [c_void_p]
    lib.chat_close_store.restype = c_void_p
    lib.chat_send_cmd.argtypes = [c_void_p, c_char_p]
    lib.chat_send_cmd.restype = c_void_p
    lib.chat_recv_msg_wait.argtypes = [c_void_p, c_int]
    lib.chat_recv_msg_wait.restype = c_void_p
    # chat_write_file's payload is treated read-only by libsimplex; passing
    # `bytes` via c_char_p avoids the from_buffer_copy doubling. ctypes pins
    # the bytes buffer for the duration of the call.
    lib.chat_write_file.argtypes = [c_void_p, c_char_p, c_char_p, c_int]
    lib.chat_write_file.restype = c_void_p
    lib.chat_read_file.argtypes = [c_char_p, c_char_p, c_char_p]
    lib.chat_read_file.restype = POINTER(c_uint8)
    lib.chat_encrypt_file.argtypes = [c_void_p, c_char_p, c_char_p]
    lib.chat_encrypt_file.restype = c_void_p
    lib.chat_decrypt_file.argtypes = [c_char_p, c_char_p, c_char_p, c_char_p]
    lib.chat_decrypt_file.restype = c_void_p


def _hs_init(lib: ctypes.CDLL) -> None:
    """Initialize the Haskell runtime exactly once. Mirrors cpp/simplex.cc:13-32."""
    if sys.platform == "win32":
        argv_strs = [b"simplex", b"+RTS", b"-A64m", b"-H64m", b"--install-signal-handlers=no"]
    else:
        argv_strs = [
            b"simplex",
            b"+RTS",
            b"-A64m",
            b"-H64m",
            b"-xn",
            b"--install-signal-handlers=no",
        ]
    argc = c_int(len(argv_strs))
    arr = (c_char_p * (len(argv_strs) + 1))(*argv_strs, None)
    arr_ptr = ctypes.byref(ctypes.cast(arr, POINTER(c_char_p)))
    lib.hs_init_with_rtsopts.argtypes = [POINTER(c_int), POINTER(POINTER(c_char_p))]
    lib.hs_init_with_rtsopts.restype = None
    lib.hs_init_with_rtsopts(ctypes.byref(argc), arr_ptr)


def lib_for(backend: Backend) -> ctypes.CDLL:
    """Resolve, load, and initialize libsimplex for the given backend.

    Idempotent for the same backend; raises if called with a different backend.
    Concurrent calls serialize on the module-level lock.
    """
    global _lib, _libc, _backend
    with _lock:
        if _lib is not None:
            if _backend != backend:
                raise RuntimeError(
                    f"libsimplex already loaded with backend={_backend!r}; "
                    f"cannot switch to {backend!r} in the same process"
                )
            return _lib
        libs_dir = _resolve_libs_dir(backend)
        lib = ctypes.CDLL(str(libs_dir / _libname()))
        _setup_signatures(lib)
        _hs_init(lib)
        _libc = _load_libc()
        _lib = lib
        _backend = backend
        return lib


def libc() -> ctypes.CDLL:
    """libc — needed by `core` to free Haskell-allocated result strings."""
    if _libc is None:
        raise RuntimeError("lib_for() must be called before libc()")
    return _libc


def lib() -> ctypes.CDLL:
    """Loaded libsimplex handle. Raises if `lib_for()` has not been called."""
    if _lib is None:
        raise RuntimeError("lib_for() must be called before lib()")
    return _lib
