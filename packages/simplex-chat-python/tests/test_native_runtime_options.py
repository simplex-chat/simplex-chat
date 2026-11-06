from types import SimpleNamespace
from typing import Any

import pytest

from simplex_chat import RuntimeInitializedError, _native, set_runtime_options


class FakeHsInit:
    """Stands in for `hs_init_with_rtsopts` and records the argv it receives."""

    def __init__(self) -> None:
        self.argv: list[bytes] | None = None

    def __call__(self, argc: Any, argv: Any) -> None:
        # Both arguments arrive as ctypes.byref() wrappers; `_obj` is the referenced object.
        self.argv = [argv._obj[i] for i in range(argc._obj.value)]


@pytest.fixture(autouse=True)
def runtime_not_started(monkeypatch):
    monkeypatch.setattr(_native, "_lib", None)
    monkeypatch.setattr(_native, "_runtime_options", None)


def hs_init_argv() -> list[bytes] | None:
    hs_init = FakeHsInit()
    _native._hs_init(SimpleNamespace(hs_init_with_rtsopts=hs_init))  # type: ignore[arg-type]
    return hs_init.argv


def test_default_argv_on_posix(monkeypatch):
    monkeypatch.setattr("sys.platform", "linux")
    assert hs_init_argv() == [
        b"simplex",
        b"+RTS",
        b"-A64m",
        b"-H64m",
        b"-xn",
        b"--install-signal-handlers=no",
    ]


def test_default_argv_on_windows(monkeypatch):
    monkeypatch.setattr("sys.platform", "win32")
    assert hs_init_argv() == [
        b"simplex",
        b"+RTS",
        b"-A64m",
        b"-H64m",
        b"--install-signal-handlers=no",
    ]


def test_options_replace_defaults_and_keep_signal_handlers_off(monkeypatch):
    monkeypatch.setattr("sys.platform", "linux")
    set_runtime_options(["-A4m", "-H4m"])
    assert hs_init_argv() == [
        b"simplex",
        b"+RTS",
        b"-A4m",
        b"-H4m",
        b"--install-signal-handlers=no",
    ]


def test_empty_options_leave_runtime_defaults(monkeypatch):
    monkeypatch.setattr("sys.platform", "linux")
    set_runtime_options([])
    assert hs_init_argv() == [b"simplex", b"+RTS", b"--install-signal-handlers=no"]


def test_options_rejected_after_runtime_started(monkeypatch):
    monkeypatch.setattr(_native, "_lib", object())
    with pytest.raises(RuntimeInitializedError) as exc:
        set_runtime_options(["-A4m"])
    assert str(exc.value) == (
        "the Haskell runtime is already initialized; "
        "set its options before the first ChatApi, Client or Bot starts"
    )
    assert _native._runtime_options is None


def test_options_reject_a_single_string():
    with pytest.raises(TypeError, match="sequence of separate options"):
        set_runtime_options("-A4m")
    assert _native._runtime_options is None
