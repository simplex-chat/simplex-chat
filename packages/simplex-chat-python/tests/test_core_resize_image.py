from __future__ import annotations

import asyncio
import base64
from typing import Any

import pytest

from simplex_chat import core

JPEG = b"\xff\xd8\xff\xe0"


class FakeLib:
    def __init__(self, result: str) -> None:
        self.result = result
        self.calls: list[tuple[str, tuple[Any, ...]]] = []

    def _export(self, name: str) -> Any:
        lib = self

        class Fn:
            argtypes: Any = None
            restype: Any = None

            def __call__(self, *args: Any) -> str:
                lib.calls.append((name, args))
                return lib.result

        return Fn()

    @property
    def chat_resize_image(self) -> Any:
        return self._export("chat_resize_image")

    @property
    def chat_resize_image_uri(self) -> Any:
        return self._export("chat_resize_image_uri")


@pytest.fixture
def fake_lib(monkeypatch: pytest.MonkeyPatch):
    def install(result: str) -> FakeLib:
        lib = FakeLib(result)
        monkeypatch.setattr(core._native, "lib", lambda: lib)
        monkeypatch.setattr(core, "_read_and_free", lambda ptr: ptr)
        return lib

    return install


def test_resize_image_uri_returns_the_data_uri(fake_lib):
    lib = fake_lib("data:image/jpg;base64,AAAA")
    assert asyncio.run(core.chat_resize_image_uri(b"image", 14000)) == "data:image/jpg;base64,AAAA"
    assert lib.calls == [("chat_resize_image_uri", (b"image", 5, 14000))]


@pytest.mark.parametrize(("image_format", "png"), [("jpeg", 0), ("png", 1)])
def test_resize_image_passes_the_format_and_decodes_base64(fake_lib, image_format, png):
    lib = fake_lib(base64.b64encode(JPEG).decode())
    assert asyncio.run(core.chat_resize_image(b"image", 261120, image_format)) == JPEG
    assert lib.calls == [("chat_resize_image", (b"image", 5, 261120, png))]


@pytest.mark.parametrize(
    "resize",
    [
        lambda: core.chat_resize_image_uri(b"image", 14000),
        lambda: core.chat_resize_image(b"image", 261120, "jpeg"),
    ],
)
def test_empty_result_raises(fake_lib, resize):
    fake_lib("")
    with pytest.raises(RuntimeError, match="image cannot be decoded or does not fit max_size"):
        asyncio.run(resize())


@pytest.mark.parametrize(
    ("name", "resize"),
    [
        ("chat_resize_image_uri", lambda: core.chat_resize_image_uri(b"image", 14000)),
        ("chat_resize_image", lambda: core.chat_resize_image(b"image", 261120, "png")),
    ],
)
def test_old_lib_raises_clear_error(monkeypatch, name, resize):
    monkeypatch.setattr(core._native, "lib", lambda: object())
    message = f"loaded libsimplex does not export {name}; image resizing needs a newer libsimplex"
    with pytest.raises(RuntimeError, match=message):
        asyncio.run(resize())


@pytest.mark.parametrize("max_size", [2**31, -(2**31) - 1])
def test_max_size_outside_c_int_is_rejected_before_ffi(fake_lib, max_size):
    lib = fake_lib("data:image/jpg;base64,AAAA")
    with pytest.raises(ValueError, match="does not fit C int"):
        asyncio.run(core.chat_resize_image_uri(b"image", max_size))
    with pytest.raises(ValueError, match="does not fit C int"):
        asyncio.run(core.chat_resize_image(b"image", max_size, "jpeg"))
    assert lib.calls == []
