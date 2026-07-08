from unittest.mock import patch
import pytest
from simplex_chat._native import _platform_tag, _libs_url, _libname
from simplex_chat._version import LIBS_VERSION


@patch("sys.platform", "linux")
@patch("platform.machine", return_value="x86_64")
def test_platform_linux_x64(_):
    assert _platform_tag() == "linux-x86_64"


@patch("sys.platform", "darwin")
@patch("platform.machine", return_value="arm64")
def test_platform_macos_arm64(_):
    assert _platform_tag() == "macos-aarch64"


@patch("sys.platform", "win32")
@patch("platform.machine", return_value="AMD64")
def test_platform_windows_x64(_):
    assert _platform_tag() == "windows-x86_64"


@patch("sys.platform", "freebsd")
@patch("platform.machine", return_value="x86_64")
def test_platform_unsupported(_):
    with pytest.raises(RuntimeError, match="Unsupported"):
        _platform_tag()


def test_libname_per_platform():
    with patch("sys.platform", "linux"):
        assert _libname() == "libsimplex.so"
    with patch("sys.platform", "darwin"):
        assert _libname() == "libsimplex.dylib"
    with patch("sys.platform", "win32"):
        assert _libname() == "libsimplex.dll"


@patch("simplex_chat._native._platform_tag", return_value="linux-x86_64")
def test_url_sqlite(_):
    assert (
        _libs_url("sqlite")
        == "https://github.com/simplex-chat/simplex-chat-libs/releases/download/"
        f"v{LIBS_VERSION}/simplex-chat-libs-linux-x86_64.zip"
    )


@patch("simplex_chat._native._platform_tag", return_value="linux-x86_64")
def test_url_postgres(_):
    assert (
        _libs_url("postgres")
        == "https://github.com/simplex-chat/simplex-chat-libs/releases/download/"
        f"v{LIBS_VERSION}/simplex-chat-libs-linux-x86_64-postgres.zip"
    )
