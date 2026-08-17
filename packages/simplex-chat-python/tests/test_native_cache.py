import zipfile
from pathlib import Path

import pytest

from simplex_chat._native import _cache_root, _resolve_libs_dir, _download
from simplex_chat._version import LIBS_VERSION


def test_cache_root_linux(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    assert _cache_root() == tmp_path / "simplex-chat"


def test_cache_root_macos(tmp_path, monkeypatch):
    monkeypatch.setattr("sys.platform", "darwin")
    monkeypatch.setattr("pathlib.Path.home", lambda: tmp_path)
    assert _cache_root() == tmp_path / "Library" / "Caches" / "simplex-chat"


def test_override_via_env(tmp_path, monkeypatch):
    # _resolve_libs_dir intentionally does not validate the override directory —
    # it returns it verbatim; the eventual ctypes.CDLL call surfaces any mistake.
    monkeypatch.setenv("SIMPLEX_LIBS_DIR", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    assert _resolve_libs_dir("sqlite") == tmp_path


def test_resolve_downloads_when_missing(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    monkeypatch.setattr("simplex_chat._native._platform_tag", lambda: "linux-x86_64")

    called = {}

    def fake_download(target_root: Path, backend: str) -> None:
        called["target"] = target_root
        called["backend"] = backend
        target_root.mkdir(parents=True, exist_ok=True)
        (target_root / "libsimplex.so").touch()

    monkeypatch.setattr("simplex_chat._native._download", fake_download)
    libs_dir = _resolve_libs_dir("sqlite")
    assert libs_dir == tmp_path / "simplex-chat" / f"v{LIBS_VERSION}" / "sqlite"
    assert called["backend"] == "sqlite"
    assert (libs_dir / "libsimplex.so").exists()


def test_resolve_uses_cache_on_second_call(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    cached = tmp_path / "simplex-chat" / f"v{LIBS_VERSION}" / "sqlite"
    cached.mkdir(parents=True)
    (cached / "libsimplex.so").touch()
    # Should NOT call _download — use the cached file.
    monkeypatch.setattr(
        "simplex_chat._native._download", lambda *a: pytest.fail("download should not be called")
    )
    assert _resolve_libs_dir("sqlite") == cached


def test_postgres_on_macos_rejected(monkeypatch):
    monkeypatch.setattr("sys.platform", "darwin")
    monkeypatch.setattr("simplex_chat._native._platform_tag", lambda: "macos-aarch64")
    with pytest.raises(RuntimeError, match="postgres.*linux-x86_64"):
        _resolve_libs_dir("postgres")


def test_atomic_install(tmp_path, monkeypatch):
    """Build a fake libs zip, mock _stream_to_file, verify extraction + atomic rename."""
    # Build zip: libs/libsimplex.so + libs/libHS-stub.so
    src = tmp_path / "src" / "libs"
    src.mkdir(parents=True)
    (src / "libsimplex.so").write_text("fake-so")
    (src / "libHS-stub.so").write_text("fake-hs")
    zip_path = tmp_path / "fake-libs.zip"
    with zipfile.ZipFile(zip_path, "w") as zf:
        for f in src.iterdir():
            zf.write(f, f"libs/{f.name}")

    def fake_stream(url, dest, *, timeout=60.0):
        import shutil

        shutil.copy(zip_path, dest)

    monkeypatch.setattr("simplex_chat._native._stream_to_file", fake_stream)
    monkeypatch.setattr("simplex_chat._native._platform_tag", lambda: "linux-x86_64")

    target = tmp_path / "out"
    _download(target, "sqlite")
    assert (target / "libsimplex.so").read_text() == "fake-so"
    assert (target / "libHS-stub.so").read_text() == "fake-hs"
