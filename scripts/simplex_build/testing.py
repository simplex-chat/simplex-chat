"""Stand-ins and helpers that the tests of simplex_build share."""

import contextlib
import io
import pathlib
import struct
import zipfile
from unittest import mock

from . import (
    android,
    apk_zip,
    apple,
    buildroot,
    cli,
    common,
    desktop,
    dmg,
    docker,
    ios,
    link_without_rpaths,
    linux,
    macho,
    macos,
    nix,
    openssl,
    smoke,
    windows,
)

MODULES = (
    common,
    apk_zip,
    buildroot,
    apple,
    docker,
    nix,
    desktop,
    dmg,
    openssl,
    android,
    link_without_rpaths,
    linux,
    macho,
    macos,
    windows,
    ios,
    smoke,
    cli,
)
REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]


class Names:
    """The names of all simplex_build modules, as the single script had them."""

    def __getattr__(self, name):
        for module in MODULES:
            if hasattr(module, name):
                return getattr(module, name)
        raise AttributeError(name)


build_script = Names()


@contextlib.contextmanager
def patch_all(name, new=mock.DEFAULT, **kwargs):
    """Patches name, with one object, in every module that defines or imports it."""
    value = mock.MagicMock(**kwargs) if new is mock.DEFAULT else new
    with contextlib.ExitStack() as stack:
        for module in MODULES:
            if hasattr(module, name):
                stack.enter_context(mock.patch.object(module, name, value))
        yield value


Arch, Step, Target = build_script.Arch, build_script.Step, build_script.Target


JAMMY, NOBLE, JAMMY_ARM, NOBLE_ARM = build_script.DESKTOP_IMAGES


UTF8_FLAG = 0x800


GITHUB = {"GITHUB_ACTIONS": "true"}


XCODE_DEVELOPER = pathlib.Path("/Applications/Xcode_26.6.app/Contents/Developer")


APKSIGNER_PADDING = struct.pack("<HHH", 0xD935, 2, 4)


def make_zip(entries):
    """entries: (name, data, flags, extra) tuples"""
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, "w") as z:
        for name, data, _, extra in entries:
            info = zipfile.ZipInfo(name, date_time=(2025, 12, 1, 0, 0, 0))
            info.extra = extra
            z.writestr(info, data)
    # zipfile resets flag_bits when it writes an entry, so the flags are set in the written headers
    data = bytearray(buffer.getvalue())
    flags = {name: f for name, _, f, _ in entries}
    for entry in build_script.central_directory(bytes(data)):
        packed = struct.pack("<H", flags[entry.name])
        data[entry.central_offset + 8 : entry.central_offset + 10] = packed
        data[entry.local_offset + 6 : entry.local_offset + 8] = packed
    return bytes(data)


def local_flags(data, name):
    entry = next(e for e in build_script.central_directory(data) if e.name == name)
    return struct.unpack("<H", data[entry.local_offset + 6 : entry.local_offset + 8])[0]


def central_flags(data, name):
    return next(e.flags for e in build_script.central_directory(data) if e.name == name)


SIGNED = make_zip(
    [
        ("classes.dex", b"dex", 0, b""),
        ("META-INF/KEY0.SF", b"sf", UTF8_FLAG, b""),
        ("META-INF/KEY0.RSA", b"rsa", UTF8_FLAG, b""),
        ("META-INF/MANIFEST.MF", b"mf", UTF8_FLAG, b""),
        ("META-INF/versions/9/OSGI-INF/MANIFEST.MF", b"osgi", 0, b""),
    ]
)


COPIED = make_zip(
    [
        ("classes.dex", b"dex", 0, b""),
        ("META-INF/KEY0.SF", b"sf", 0, b""),
        ("META-INF/KEY0.RSA", b"rsa", 0, b""),
        ("META-INF/MANIFEST.MF", b"mf", 0, b""),
        ("META-INF/versions/9/OSGI-INF/MANIFEST.MF", b"osgi", 0, b""),
    ]
)


class Recorder:
    """Stands in for run(): records commands, answers captured ones, fails the ones matching fail_once the first time."""

    def __init__(self, fail_once=(), answers=None):
        self.commands = []
        self.inputs = []
        self.envs = []
        self.fail_once = list(fail_once)
        self.answers = answers or {}

    def __call__(self, args, cwd=None, env=None, input=None, check=True, capture=False, title=None):
        args = [str(a) for a in args]
        self.commands.append(args)
        self.inputs.append(input)
        self.envs.append(env)
        joined = " ".join(args)
        for pattern in self.fail_once:
            if pattern in joined:
                self.fail_once.remove(pattern)
                if check:
                    raise build_script.BuildError(f"failed: {joined}")
        stdout = next((answer for pattern, answer in self.answers.items() if pattern in joined), "")
        return mock.Mock(returncode=0, stdout=stdout)

    def steps(self):
        """argv of the docker exec commands, without docker's own options"""
        result = []
        for command in self.commands:
            if command[:2] == ["docker", "exec"]:
                i = 2
                while command[i].startswith("-"):
                    i += 1 if command[i] == "-i" else 2
                result.append(command[i + 1 :])
        return result


SIGNING_ENV = {
    "ANDROID_KEYSTORE_PASSWORD": "store-secret",
    "ANDROID_KEY_ALIAS": "simplex",
    "ANDROID_KEY_PASSWORD": "key-secret",
}


class FakeRunner:
    """Stands in for a Container or HostRunner: paths are host paths, and actions[argv[0]] runs for each step."""

    def __init__(self, actions=None, answers=None):
        self.steps = []
        self.actions = actions or {}
        self.answers = answers or {}

    def run(self, step, capture=False, secrets=None):
        self.steps.append(step)
        action = self.actions.get(pathlib.Path(step.argv[0]).name)
        if action:
            action(step)
        return next((answer for pattern, answer in self.answers.items() if pattern in " ".join(step.argv)), "")

    def path(self, host):
        return str(host)

    def argvs(self):
        return [step.argv for step in self.steps]


def otool_l(rpaths=(), loads=()):
    lines = ["lib.dylib:", "Load command 0", "      cmd LC_ID_DYLIB"]
    for path in loads:
        lines += ["          cmd LC_LOAD_DYLIB", f"         name {path} (offset 24)"]
    for path in rpaths:
        lines += ["          cmd LC_RPATH", f"         path {path} (offset 12)"]
    return "\n".join(lines) + "\n"


def otool_big_l(own, deps):
    lines = ["lib.dylib:", f"\t{own} (compatibility version 0.0.0, current version 0.0.0)"]
    return "\n".join(lines + [f"\t{d} (compatibility version 0.0.0)" for d in deps]) + "\n"


class FakeOtool:
    """Stands in for run(): answers otool for each library from tables that install_name_tool edits, records commands."""

    def __init__(self, big_l=None, small_l=None, answers=None):
        self.big_l, self.small_l = big_l or {}, small_l or {}
        self.answers = answers or {}
        self.commands = []

    def __call__(self, args, cwd=None, env=None, input=None, check=True, capture=False, title=None):
        args = [str(a) for a in args]
        self.commands.append(args)
        if args[0] == "otool":
            table = self.big_l if args[1] == "-L" else self.small_l
            return mock.Mock(returncode=0, stdout=table.get(pathlib.Path(args[2]).name, ""))
        if args[:2] == ["install_name_tool", "-delete_rpath"]:
            name = pathlib.Path(args[3]).name
            self.small_l[name] = "".join(line for line in self.small_l[name].splitlines(keepends=True) if f"path {args[2]} " not in line)
        if args[:2] == ["install_name_tool", "-change"]:
            name = pathlib.Path(args[4]).name
            self.small_l[name] = self.small_l[name].replace(args[2], args[3])
        stdout = next((answer for pattern, answer in self.answers.items() if pattern in " ".join(args)), "")
        return mock.Mock(returncode=0, stdout=stdout)


OTOOL = """Archive : libffi.a(closures.o)
Load command 1
      cmd LC_VERSION_MIN_IPHONEOS
  cmdsize 16
  version 11.0
      sdk 11.0
Load command 2
      cmd LC_SYMTAB
Archive : libffi.a(ffi.o)
Load command 1
      cmd LC_BUILD_VERSION
  cmdsize 32
 platform 2
    minos 14.2
      sdk 14.2
"""


ENTITLEMENTS = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>keychain-access-groups</key>
	<array>
		<string>$(AppIdentifierPrefix)chat.simplex.app</string>
	</array>
</dict>
</plist>
"""


class FakeApple:
    """Stands in for run() with Apple's tools: codesign stores and returns entitlements, otool returns OTOOL."""

    def __init__(self, tamper=False):
        self.commands = []
        self.signed = {}
        self.tamper = tamper

    def __call__(self, args, cwd=None, env=None, input=None, check=True, capture=False, title=None):
        args = [str(a) for a in args]
        self.commands.append(args)
        stdout = ""
        if args[0] == "codesign" and "--entitlements" in args and "-d" not in args:
            text = pathlib.Path(args[args.index("--entitlements") + 1]).read_text()
            self.signed[args[-1]] = text.replace("5NN7GUYB6T", "XXXX") if self.tamper else text
        elif args[0] == "codesign" and "-d" in args:
            stdout = self.signed[args[-1]]
        elif args[0] == "otool":
            stdout = OTOOL
        return mock.Mock(returncode=0, stdout=stdout)


PBXPROJ = """
    A1 /* libHSsimplex-chat-6.5.0.0-Old.a */ = {isa = PBXBuildFile; fileRef = B1 /* libHSsimplex-chat-6.5.0.0-Old.a */; };
    A2 /* libHSsimplex-chat-6.5.0.0-Old-ghc9.6.3.a */ = {isa = PBXBuildFile; fileRef = B2 /* libHSsimplex-chat-6.5.0.0-Old-ghc9.6.3.a */; };
    B1 /* libHSsimplex-chat-6.5.0.0-Old.a */ = {path = "libHSsimplex-chat-6.5.0.0-Old.a"; };
    B2 /* libHSsimplex-chat-6.5.0.0-Old-ghc9.6.3.a */ = {path = "libHSsimplex-chat-6.5.0.0-Old-ghc9.6.3.a"; };
    C1 /* libHSsimplex-chat-6.5.0.0-Old.a in Frameworks */,
    C2 /* libHSsimplex-chat-6.5.0.0-Old-ghc9.6.3.a in Frameworks */,
    D1 /* libHSsimplex-chat-6.5.0.0-Old.a */,
    D2 /* libHSsimplex-chat-6.5.0.0-Old-ghc9.6.3.a */,
"""
