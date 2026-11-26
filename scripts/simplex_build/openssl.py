"""The OpenSSL that the apps bundle, pinned to one version and hash."""

import os
import shutil
import tarfile
from dataclasses import dataclass
from pathlib import Path

from .common import BuildError, Platform, fetch_pinned, host_platform, remove_tree, run
from .desktop import SOURCE_DATE_EPOCH, WINDOWS_DESKTOP, macos_desktop

# The OpenSSL that the macOS and Windows releases link, the version of the releases before this script
OPENSSL_VERSION = "3.0.15"


OPENSSL_URL = f"https://github.com/openssl/openssl/releases/download/openssl-{OPENSSL_VERSION}/openssl-{OPENSSL_VERSION}.tar.gz"


OPENSSL_SHA256 = "23c666d0edf20f14249b3d8f0368acaee9ab585b09e1de82107c66e1f3ec9533"


# OpenSSL loads openssl.cnf from OPENSSLDIR and modules from the prefix, and the file can load any module: only
# administrators can write to these directories on users' machines
OPENSSL_UNIX_PREFIX = "/usr/local"


OPENSSL_UNIX_DIR = "/etc/ssl"


OPENSSL_WINDOWS_PREFIX = "C:/Program Files/OpenSSL"


OPENSSL_WINDOWS_DIR = "C:/Program Files/Common Files/SSL"


# Release builds keep OpenSSL at one path in the checkout: cabal hashes its directories into the package ids that the
# binaries contain, and the macOS build records its prefix there
OPENSSL_BUILD_DIR = "dist-openssl"


def fetch_openssl(work: Path) -> Path:
    """The OpenSSL source tree, from the tarball checked against OPENSSL_SHA256."""
    tarball = fetch_pinned(OPENSSL_URL, OPENSSL_SHA256, work / f"openssl-{OPENSSL_VERSION}.tar.gz")
    with tarfile.open(tarball) as tar:
        tar.extractall(work, filter="data")
    return work / f"openssl-{OPENSSL_VERSION}"


def openssl_build(target: str, prefix: str, openssldir: str, install: bool) -> list[tuple[str, ...]]:
    """The commands that build OpenSSL in its source tree, then install it to prefix when install is set."""
    commands = [
        ("perl", "Configure", target, f"--prefix={prefix}", f"--openssldir={openssldir}", "--libdir=lib", "no-tests"),
        ("make", f"-j{os.cpu_count() or 1}"),
    ]
    if install:
        commands.append(("make", "install_sw"))
    return commands


def openssl_cabal_local(include: str, lib: str, simplex_chat_ghc_options: str = "") -> str:
    """cabal.project.local that builds simplexmq and direct-sqlcipher with the OpenSSL in include and lib."""
    text = "ignore-project: False\n"
    for package, flags in (("simplexmq", ""), ("direct-sqlcipher", "    flags: +openssl\n")):
        text += f"package {package}\n    extra-include-dirs: {include}\n    extra-lib-dirs: {lib}\n{flags}"
    if simplex_chat_ghc_options:
        text += f"package simplex-chat\n    ghc-options: {simplex_chat_ghc_options}\n"
    return text


def build_openssl(source: Path, target: str, prefix: str, openssldir: str, install: bool) -> None:
    for argv in openssl_build(target, prefix, openssldir, install):
        run(argv, cwd=source, env={"SOURCE_DATE_EPOCH": SOURCE_DATE_EPOCH})


@dataclass(frozen=True, slots=True)
class OpenSsl:
    """A build of the pinned OpenSSL: its headers, and the directory with its libraries."""

    include: Path
    lib: Path

    def cabal_local(self, simplex_chat_ghc_options: str = "") -> str:
        return openssl_cabal_local(self.include.as_posix(), self.lib.as_posix(), simplex_chat_ghc_options)


def openssl_work(source: Path) -> Path:
    work = source / OPENSSL_BUILD_DIR
    remove_tree(work)
    work.mkdir()
    return work


def host_openssl(work: Path) -> OpenSsl:
    """The pinned OpenSSL built in work for this Mac, installed, or for Windows, in its source tree."""
    match host_platform():
        case Platform.LINUX:
            raise BuildError("Linux builds use the system's OpenSSL")
        case Platform.WINDOWS:
            source = fetch_openssl(work)
            build_openssl(source, WINDOWS_DESKTOP.openssl_target, OPENSSL_WINDOWS_PREFIX, OPENSSL_WINDOWS_DIR, install=False)
            return OpenSsl(source / "include", source)
    source = fetch_openssl(work)
    prefix = work / "openssl"
    build_openssl(source, macos_desktop().openssl_target, str(prefix), OPENSSL_UNIX_DIR, install=True)
    return OpenSsl(prefix / "include", prefix / "lib")


def static_libcrypto(lib: Path, work: Path) -> Path:
    """A directory with only the static libcrypto, so that the linker can't choose the shared one."""
    static = work / "openssl-static"
    static.mkdir()
    shutil.copy(lib / "libcrypto.a", static)
    return static


def check_static_openssl(binary: Path, dependencies: str) -> None:
    """dependencies: what otool -L or objdump -p prints of the binary."""
    if "libcrypto" in dependencies:
        raise BuildError(f"{binary} loads libcrypto, instead of linking the pinned OpenSSL statically")


def build_host_openssl(source: Path, out: Path) -> None:
    """The pinned OpenSSL in out, and cabal.project.local that links it, for CI's test builds on macOS and Windows."""
    (source / "cabal.project.local").write_text(host_openssl(out).cabal_local())
