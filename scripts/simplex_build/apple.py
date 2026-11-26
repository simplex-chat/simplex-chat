"""What the macOS and iOS builds share: the pinned Xcode, and the directory and settings that they build with, so that
the release files don't depend on the machine's paths and settings."""

import contextlib
import ctypes
import os
import re
import sys
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path

from .buildroot import MIN_MACHINE_PATH_PARTS, BuildRoot, fixed_build_root
from .common import BuildError, add_summary, log, log_group, run


@dataclass(frozen=True, slots=True)
class Xcode:
    version: str
    build: str

    def __str__(self) -> str:
        return f"Xcode {self.version} ({self.build})"


# The Xcode of the macOS and iOS release builds, whose compilers, linker and SDKs write the binaries; of GitHub's images,
# macos-26 and macos-26-intel have it
XCODE = Xcode("26.6", "17F113")


XCODE_VERSION_OUTPUT = re.compile(r"^Xcode (?P<version>\S+)\nBuild version (?P<build>\S+)$", re.MULTILINE)


def xcode_version(output: str) -> Xcode:
    """The Xcode that xcodebuild -version describes in output."""
    match = XCODE_VERSION_OUTPUT.search(output)
    if match is None:
        raise BuildError(f"no Xcode version in what xcodebuild -version prints: {output!r}")
    return Xcode(match["version"], match["build"])


def require_xcode(pinned: Xcode) -> Path:
    """The developer directory of the selected Xcode, which must be pinned."""
    selected = xcode_version(run(["xcodebuild", "-version"], capture=True).stdout)
    if selected != pinned:
        raise BuildError(f"{selected} is selected, not {pinned} of this release build: set DEVELOPER_DIR to its Contents/Developer")
    developer = Path(run(["xcrun", "--find", "xcodebuild"], capture=True).stdout.strip()).resolve().parents[2]
    log(f"{pinned} in {developer}")
    add_summary(f"Built with {pinned}.\n\n")
    return developer


# The release builds run in a copy of the checkout here, with cabal's directory and the temporary directory beside it:
# the binaries contain the paths of the checkout, of OpenSSL's build in it and of cabal's package store, and the iOS
# debug information those of the checkout and of the macro expansions that Swift writes in TMPDIR
BUILD_ROOT = Path("/private/tmp/simplex-chat-build")


# the settings of GitHub's macOS runners
BUILD_LOCALE = "en_US.UTF-8"


BUILD_TIMEZONE = "UTC"


def build_env(root: BuildRoot) -> dict[str, str]:
    return {"CABAL_DIR": str(BUILD_ROOT / "cabal"), "TMPDIR": str(root.tmp), "LC_ALL": BUILD_LOCALE, "TZ": BUILD_TIMEZONE}


def build_root(source: Path, env: Mapping[str, str]) -> contextlib.AbstractContextManager[BuildRoot]:
    """source copied into BUILD_ROOT, with the variables of build_env and env set while it is used."""
    root = BuildRoot.at(BUILD_ROOT)
    return fixed_build_root(source, root, {**build_env(root), **env})


# Xcode's usual place, where the iOS build needs it, and which the builds don't report as a path of the machine
CANONICAL_DEVELOPER_DIR = Path("/Applications/Xcode.app/Contents/Developer")


# the user's temporary and cache directories, which the system names per machine and user
DARWIN_USER_DIRS = ("DARWIN_USER_TEMP_DIR", "DARWIN_USER_CACHE_DIR")


def machine_paths(source: Path, developer: Path) -> list[str]:
    """The paths of this machine and user, which release files built on other machines can't contain: the checkout, the
    home and temporary directories, and the Xcode of developer unless it is in CANONICAL_DEVELOPER_DIR."""
    paths = {str(source), str(Path.home()), os.environ.get("TMPDIR", "")}
    paths |= {run(["getconf", name], capture=True).stdout.strip() for name in DARWIN_USER_DIRS}
    if developer != CANONICAL_DEVELOPER_DIR:
        paths.add(str(developer.parents[1]))
    return sorted({path.rstrip("/") for path in paths if len(Path(path).parts) >= MIN_MACHINE_PATH_PARTS})


# _POSIX_SPAWN_DISABLE_ASLR of XNU's bsd/sys/spawn.h, which lldb sets to start programs without ASLR; their children
# inherit it. The Swift compiler's optimizer orders some code by memory addresses, and with ASLR SwiftyGif compiled
# differently in about one build in ten.
POSIX_SPAWN_DISABLE_ASLR = 0x0100


# room for posix_spawnattr_t and posix_spawn_file_actions_t, pointers on macOS and structs of up to 336 bytes in glibc
SPAWN_STRUCT_SIZE = 512


# prints the address of the program's main image, which is the same in each run when ASLR is off
LOAD_ADDRESS_PROBE = """
import ctypes, sys
libc = ctypes.CDLL(None)
libc._dyld_get_image_header.restype = ctypes.c_void_p
with open(sys.argv[1], "w") as f:
    f.write(hex(libc._dyld_get_image_header(0)))
"""


def spawn(args: Sequence[str | Path], cwd: Path, flags: int) -> int:
    """Runs args in cwd with posix_spawnp and these spawn flags, and returns its exit code."""
    libc = ctypes.CDLL(None, use_errno=True)
    attr = ctypes.create_string_buffer(SPAWN_STRUCT_SIZE)
    actions = ctypes.create_string_buffer(SPAWN_STRUCT_SIZE)
    argv = [os.fsencode(str(arg)) for arg in args]
    env = [os.fsencode(f"{name}={value}") for name, value in os.environ.items()]
    c_argv = (ctypes.c_char_p * (len(argv) + 1))(*argv, None)
    c_env = (ctypes.c_char_p * (len(env) + 1))(*env, None)
    pid = ctypes.c_int()

    def check(call: str, result: int) -> None:
        if result != 0:
            raise BuildError(f"{call} failed for {argv[0].decode()}: {os.strerror(result)}")

    check("posix_spawnattr_init", libc.posix_spawnattr_init(attr))
    check("posix_spawn_file_actions_init", libc.posix_spawn_file_actions_init(actions))
    try:
        check("posix_spawnattr_setflags", libc.posix_spawnattr_setflags(attr, ctypes.c_short(flags)))
        check("posix_spawn_file_actions_addchdir_np", libc.posix_spawn_file_actions_addchdir_np(actions, os.fsencode(str(cwd))))
        check("posix_spawnp", libc.posix_spawnp(ctypes.byref(pid), c_argv[0], actions, attr, c_argv, c_env))
    finally:
        libc.posix_spawn_file_actions_destroy(actions)
        libc.posix_spawnattr_destroy(attr)
    _, status = os.waitpid(pid.value, 0)
    return os.waitstatus_to_exitcode(status)


def check_aslr_off(work: Path) -> None:
    """Fails unless two programs started without ASLR load their main image at the same address."""
    addresses = []
    for run_number in range(2):
        output = work / f"load-address-{run_number}"
        if spawn([sys.executable, "-c", LOAD_ADDRESS_PROBE, output], work, POSIX_SPAWN_DISABLE_ASLR) != 0:
            raise BuildError("the program that reports its load address failed")
        addresses.append(output.read_text())
    if addresses[0] != addresses[1]:
        raise BuildError(f"programs started without ASLR load at different addresses, {' and '.join(addresses)}: ASLR is on")
    log(f"ASLR is off: the main image loads at {addresses[0]} in each run")


def run_without_aslr(args: Sequence[str | Path], cwd: Path) -> None:
    """args run in cwd, and the programs they start, without ASLR, after check_aslr_off has confirmed it works."""
    with log_group(" ".join(map(str, args))):
        code = spawn(args, cwd, POSIX_SPAWN_DISABLE_ASLR)
    if code != 0:
        raise BuildError(f"failed ({code}): {' '.join(map(str, args))}")
