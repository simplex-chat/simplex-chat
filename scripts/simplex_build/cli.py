"""Builds SimpleX Chat release files and reproduces releases.

Reproduce a release on the platform of each target, Linux for desktop and android (desktop is the default target):
    simplex-chat-build.py reproduce <tag> [desktop] [android] [macos] [windows] [ios] [--arches aarch64,armv7a] [--nix-cache]
        [--art <simplex-chat-art>] [--install-nix] [--repo <owner/name>]

Compare a released file with the one that build made, allowing only its signature to differ:
    simplex-chat-build.py compare <released file> <built file> [--repo <owner/name>]

Build one commit's release files, as CI does; desktop, libs and openssl build for the machine they run on:
    simplex-chat-build.py build desktop linux <22.04|24.04> --version <x.y.z> --out <dir>
    simplex-chat-build.py build desktop macos [--require-signing] --out <dir>
    simplex-chat-build.py build desktop windows --out <dir>
    simplex-chat-build.py build libs linux [--ubuntu <22.04|24.04>] [--database <sqlite|postgres>] [--dist-dir <dir>]
        [--host | --container <name>] --out <dir>
    simplex-chat-build.py build libs macos [--database <sqlite|postgres>] [--require-signing] --out <dir>
    simplex-chat-build.py build libs windows --out <dir>
    simplex-chat-build.py build libs android [--arch <aarch64|armv7a>] [--install-nix] --out <dir>
    simplex-chat-build.py build libs ios-sim --out <dir>
    simplex-chat-build.py build openssl <macos|windows> --out <dir>
    simplex-chat-build.py build apk <aarch64|armv7a> [--nix-cache] --out <dir>
    simplex-chat-build.py build aab --libs <dir> --out <dir>
    simplex-chat-build.py build xcarchive [--art <simplex-chat-art>] [--install-nix] --out <dir>
Each build takes --source <dir>, the simplex-chat checkout, by default the current directory.

Run release files as their users do, with screenshots of the apps; apk needs a running emulator:
    simplex-chat-build.py test cli <binary> --out <dir>
    simplex-chat-build.py test desktop <.deb|.AppImage|.dmg|.msi> --out <dir>
    simplex-chat-build.py test desktop <.deb|.AppImage|.dmg|.msi> --out <dir> --upgrade-from-previous --cli <cli>
    simplex-chat-build.py test apk <apk> --out <dir> [--upgrade-from-previous] [--cli <cli>]

Install prebuilt core libraries where the apps' builds take them, as the test workflows do:
    simplex-chat-build.py install libs linux <libs zip>
    simplex-chat-build.py install libs android <dir> [--arch <aarch64|armv7a>]
    simplex-chat-build.py install libs ios <pkg-ios-aarch64-swift-json.zip>

Needs Python 3.12+; the Linux and Android builds need Docker, xcarchive macOS with Xcode and Nix, and reproduce git.
reproduce writes its results to ./<tag>-<repository name>.
The Android builds are signed by Gradle when ANDROID_KEYSTORE, ANDROID_KEYSTORE_PASSWORD, ANDROID_KEY_ALIAS and
ANDROID_KEY_PASSWORD are set, and unsigned otherwise.
"""

import argparse
import enum
import io
import os
import platform
import re
import sys
import tempfile
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from .android import (
    Arch,
    android_container,
    android_signing,
    build_android_libs,
    build_apk,
    build_bundle,
    install_android_libs,
    reproduce_android,
)
from .common import (
    SIMPLEX_REPO,
    BuildError,
    Platform,
    add_summary,
    clone,
    error,
    host_platform,
    log,
    require_platform,
    require_tools,
    write_sha256sums,
)
from .desktop import Database
from .ios import IOS_TOOLS, build_ios_archive, build_ios_sim_libs, install_ios_libs, reproduce_ios
from .linux import (
    APPIMAGE_IMAGE,
    DESKTOP_IMAGES,
    LibsBuilder,
    UbuntuImage,
    build_desktop_linux,
    build_linux_libs,
    install_linux_libs,
    linux_image,
    reproduce_desktop,
)
from .macos import build_desktop_macos, build_macos_libs, macos_differences, reproduce_macos
from .nix import NIX_PROFILE
from .openssl import build_host_openssl
from .smoke import OLD_RELEASE, make_old_database, test_apk, test_cli, test_desktop, test_desktop_upgrade
from .windows import CFB_SIGNATURE, PE_DOS_MAGIC, build_desktop_windows, build_windows_libs, reproduce_windows, windows_differences

GITHUB_REPO = re.compile(r"[A-Za-z0-9-]+/[A-Za-z0-9._-]+")


class Target(enum.StrEnum):
    DESKTOP = "desktop"
    ANDROID = "android"
    MACOS = "macos"
    WINDOWS = "windows"
    IOS = "ios"


MACOS_DESKTOP_TOOLS = ("cabal", "ghc", "perl", "make", "otool", "cmake", "codesign", "hdiutil", "fsck_hfs", "xcodebuild", "xcrun")


WINDOWS_DESKTOP_TOOLS = ("cabal", "ghc", "perl", "make", "gcc", "objdump", "dlltool", "powershell")


TARGET_PLATFORMS = {
    Target.DESKTOP: Platform.LINUX,
    Target.ANDROID: Platform.LINUX,
    Target.MACOS: Platform.MACOS,
    Target.WINDOWS: Platform.WINDOWS,
    Target.IOS: Platform.MACOS,
}


TARGET_TOOLS = {
    Target.DESKTOP: ("docker",),
    Target.ANDROID: ("docker",),
    Target.MACOS: MACOS_DESKTOP_TOOLS,
    Target.WINDOWS: WINDOWS_DESKTOP_TOOLS,
    Target.IOS: IOS_TOOLS,
}


@dataclass(frozen=True, slots=True)
class Reproduce:
    tag: str
    repo: str
    targets: tuple[Target, ...]
    arches: tuple[Arch, ...]
    nix_cache: bool
    art: Path | None = None
    install_nix: bool = False


@dataclass(frozen=True, slots=True)
class Compare:
    released: Path
    built: Path
    repo: str


@dataclass(frozen=True, slots=True)
class BuildApk:
    arch: Arch
    out: Path
    source: Path
    nix_cache: bool


@dataclass(frozen=True, slots=True)
class BuildBundle:
    libs: Path
    out: Path
    source: Path


@dataclass(frozen=True, slots=True)
class BuildDesktopLinux:
    image: UbuntuImage
    version: str
    out: Path
    source: Path


@dataclass(frozen=True, slots=True)
class BuildLinuxLibs:
    image: UbuntuImage
    database: Database
    dist_dir: str
    builder: LibsBuilder
    out: Path
    source: Path


@dataclass(frozen=True, slots=True)
class BuildMacosLibs:
    database: Database
    out: Path
    source: Path
    require_signing: bool


@dataclass(frozen=True, slots=True)
class BuildWindowsLibs:
    out: Path
    source: Path


@dataclass(frozen=True, slots=True)
class BuildOpenssl:
    platform: Platform
    out: Path
    source: Path


@dataclass(frozen=True, slots=True)
class BuildAndroidLibs:
    arch: Arch
    out: Path
    source: Path
    install_nix: bool


@dataclass(frozen=True, slots=True)
class BuildIosSimLibs:
    out: Path
    source: Path


@dataclass(frozen=True, slots=True)
class InstallLibs:
    """Prebuilt core libraries put where the apps' builds take them."""

    platform: str
    package: Path
    arch: Arch
    source: Path


@dataclass(frozen=True, slots=True)
class BuildDesktopMacos:
    out: Path
    source: Path
    require_signing: bool


@dataclass(frozen=True, slots=True)
class BuildDesktopWindows:
    out: Path
    source: Path


@dataclass(frozen=True, slots=True)
class BuildIosArchive:
    art: Path | None
    out: Path
    source: Path
    install_nix: bool


type Build = (
    BuildApk
    | BuildBundle
    | BuildDesktopLinux
    | BuildLinuxLibs
    | BuildMacosLibs
    | BuildWindowsLibs
    | BuildOpenssl
    | BuildAndroidLibs
    | BuildIosSimLibs
    | BuildDesktopMacos
    | BuildDesktopWindows
    | BuildIosArchive
)


@dataclass(frozen=True, slots=True)
class TestRelease:
    """A smoke test of a release file: what it is, the file, and the previous release's database it must open, with
    the CLI that checks it after a desktop app or connects with an Android app as its peer; or a package installed over
    the previous release's app."""

    kind: str
    file: Path
    out: Path
    database: Path | None = None
    cli: Path | None = None
    upgrade: bool = False


@dataclass(frozen=True, slots=True)
class MakeOldDatabase:
    out: Path


type Command = Reproduce | Compare | Build | InstallLibs | TestRelease | MakeOldDatabase


def require_nix(install: bool) -> None:
    if not install and not (NIX_PROFILE / "bin/nix").exists():
        raise BuildError(f"Nix is not installed ({NIX_PROFILE / 'bin/nix'} not found): install it, or pass --install-nix")


def reproduce(command: Reproduce) -> None:
    for target in command.targets:
        require_platform(TARGET_PLATFORMS[target])
    require_tools(list(dict.fromkeys(["git", *(tool for target in command.targets for tool in TARGET_TOOLS[target])])))
    if Target.IOS in command.targets:
        require_nix(command.install_nix)
    results = Path.cwd() / f"{command.tag}-{command.repo.split('/')[1]}"
    failed: list[str] = []
    with tempfile.TemporaryDirectory() as tmp:
        source = clone(Path(tmp), command.repo)
        if Target.DESKTOP in command.targets:
            failed += reproduce_desktop(command.tag, command.repo, source, results)
        if Target.ANDROID in command.targets:
            failed += reproduce_android(command.tag, command.repo, command.arches, command.nix_cache, source, results)
        if Target.MACOS in command.targets:
            failed += reproduce_macos(command.tag, command.repo, source, results)
        if Target.WINDOWS in command.targets:
            failed += reproduce_windows(command.tag, command.repo, source, results)
        if Target.IOS in command.targets:
            art = command.art.resolve() if command.art else None
            failed += reproduce_ios(command.tag, command.repo, art, command.install_nix, source, results)
    if failed:
        add_summary(f"{command.tag} is not reproduced: {', '.join(f'`{name}`' for name in failed)}\n")
        raise BuildError(f"{command.tag} is not reproduced: {', '.join(failed)}")
    add_summary(f"{command.tag} is reproduced: {', '.join(command.targets)}\n")
    log(f"{command.tag} is reproduced: {', '.join(command.targets)}")


def release_file_differences(released: Path, built: Path, repo: str, work: Path) -> list[str]:
    """What differs other than the signature, for the format of the file: a dmg, an msi, a PE or Mach-O file, or
    another file, which must be the same byte for byte."""
    with released.open("rb") as f:
        head = f.read(len(CFB_SIGNATURE))
    if released.suffix == ".dmg":
        require_platform(Platform.MACOS)
        require_tools(["hdiutil", "codesign", "cmake"])
    elif head.startswith((CFB_SIGNATURE, PE_DOS_MAGIC)):
        return windows_differences(released, built)
    return macos_differences(released, built, repo, work)


def compare(command: Compare) -> None:
    released, built = command.released.resolve(), command.built.resolve()
    with tempfile.TemporaryDirectory() as tmp:
        differences = release_file_differences(released, built, command.repo, Path(tmp).resolve() / "work")
    for difference in differences:
        log(f"{released.name}: {difference}")
    if differences:
        raise BuildError(f"{released} differs from {built} other than by its signature")
    log(f"{released} is {built} with its signature")


def prepare_out(out: Path) -> Path:
    out = out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    return out


def build(command: Build) -> None:
    out = prepare_out(command.out)
    source = command.source.resolve()
    match command:
        case BuildApk(arch=arch, nix_cache=nix_cache):
            require_tools(["docker"])
            signing = android_signing()
            with android_container(source, out, arch == Arch.ARMV7A, signing) as container:
                build_apk(container, source, arch, out, f"simplex-{arch}.apk", nix_cache, keep_libs=True, signing=signing)
        case BuildBundle(libs=libs):
            require_tools(["docker"])
            signing = android_signing()
            with android_container(source, out, set_pid_max=False, signing=signing) as container:
                build_bundle(container, source, libs.resolve(), out, signing)
        case BuildDesktopLinux(image=image, version=version):
            require_platform(Platform.LINUX)
            require_tools(["docker"])
            build_desktop_linux(source, image, version, out)
        case BuildLinuxLibs(image=image, database=database, dist_dir=dist_dir, builder=builder):
            require_platform(Platform.LINUX)
            require_tools(["cabal", "ghc", "patchelf", "ldd"] if builder.on_host else ["docker"])
            build_linux_libs(source, image, database, dist_dir, builder, out)
        case BuildMacosLibs(database=database, require_signing=require_signing):
            require_platform(Platform.MACOS)
            require_tools(["cabal", "ghc", "perl", "make", "otool", "install_name_tool", "xcodebuild", "xcrun"])
            build_macos_libs(source, database, out, require_signing)
        case BuildWindowsLibs():
            require_platform(Platform.WINDOWS)
            require_tools(["cabal", "ghc", "perl", "make", "gcc", "dlltool"])
            build_windows_libs(source, out)
        case BuildAndroidLibs(arch=arch, install_nix=install):
            require_platform(Platform.LINUX)
            if not install:
                require_tools(["nix"])
            build_android_libs(source, arch, out, install)
        case BuildIosSimLibs():
            require_platform(Platform.MACOS)
            require_tools(["cabal", "ghc", "brew", "perl", "make", "strip"])
            build_ios_sim_libs(source, out)
        case BuildOpenssl(platform=openssl_platform):
            require_platform(openssl_platform)
            require_tools(["perl", "make"])
            build_host_openssl(source, out)
        case BuildDesktopMacos(require_signing=require_signing):
            require_platform(Platform.MACOS)
            require_tools(MACOS_DESKTOP_TOOLS)
            build_desktop_macos(source, out, require_signing)
        case BuildDesktopWindows():
            require_platform(Platform.WINDOWS)
            require_tools(WINDOWS_DESKTOP_TOOLS)
            build_desktop_windows(source, out)
        case BuildIosArchive(art=art, install_nix=install):
            require_platform(Platform.MACOS)
            require_tools(IOS_TOOLS)
            require_nix(install)
            build_ios_archive(source, art.resolve() if art else None, out, install)
    write_sha256sums(out)


def install(command: InstallLibs) -> None:
    source = command.source.resolve()
    package = command.package.resolve()
    match command.platform:
        case "linux":
            install_linux_libs(source, package)
        case "android":
            install_android_libs(source, command.arch, package)
        case _:
            require_platform(Platform.MACOS)
            require_tools(["cc", "otool"])
            with tempfile.TemporaryDirectory() as tmp:
                install_ios_libs(source, package, Path(tmp))


def test(command: TestRelease) -> None:
    out = prepare_out(command.out)
    file = command.file.resolve()
    database = command.database.resolve() if command.database else None
    cli = command.cli.resolve() if command.cli else None
    match command.kind:
        case "cli":
            test_cli(file, out, database)
        case "desktop":
            tools = {
                Platform.LINUX: ["Xvfb", "openbox", "xdotool", "import", "identify"],
                Platform.MACOS: ["hdiutil", "screencapture"],
                Platform.WINDOWS: ["msiexec", "powershell"],
            }
            require_tools(tools[host_platform()])
            if command.upgrade:
                test_desktop_upgrade(file, out, cli)
            else:
                test_desktop(file, out, database, cli)
        case _:
            test_apk(file, out, database, command.upgrade, cli)


def execute(command: Command) -> None:
    match command:
        case Reproduce():
            reproduce(command)
        case Compare():
            compare(command)
        case InstallLibs():
            install(command)
        case TestRelease():
            test(command)
        case MakeOldDatabase():
            require_platform(Platform.LINUX)
            make_old_database(command.out.resolve())
        case _:
            build(command)


def parse_build_args(argv: Sequence[str]) -> Command:
    parser = argparse.ArgumentParser(prog="simplex-chat-build.py build", description="Build one commit's release files.")
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--out", type=Path, required=True)
    common.add_argument("--source", type=Path, default=Path("."), help="the simplex-chat checkout (default: current directory)")
    kinds = parser.add_subparsers(dest="kind", required=True)
    ubuntu_versions = sorted({image.version for image in DESKTOP_IMAGES})

    desktop = kinds.add_parser("desktop", help="all release files of this machine's platform").add_subparsers(
        dest="platform", required=True
    )
    desktop_linux = desktop.add_parser("linux", parents=[common], help="the CLI, its deb, the desktop deb, the AppImage and libraries")
    desktop_linux.add_argument("image", choices=ubuntu_versions, help="the Ubuntu version")
    desktop_linux.add_argument("--version", required=True, help="the release version of the CLI deb, e.g. 7.1.0")
    desktop_macos = desktop.add_parser("macos", parents=[common], help="the CLI, dmg and libraries, signed when APPLE_SIMPLEX_* are set")
    desktop.add_parser("windows", parents=[common], help="the CLI, msi and libraries, in an MSYS2 UCRT64 shell")

    libs = kinds.add_parser("libs", help="the core library release of this machine's platform").add_subparsers(
        dest="platform", required=True
    )
    libs_linux = libs.add_parser("linux", parents=[common], help="without VLC, in Docker unless --host or --container")
    libs_linux.add_argument("--ubuntu", choices=ubuntu_versions, default=APPIMAGE_IMAGE)
    libs_linux.add_argument("--dist-dir", default="dist-newstyle", help="cabal's build directory")
    where = libs_linux.add_mutually_exclusive_group()
    where.add_argument("--host", action="store_true", help="build on this machine, with its GHC, rather than in Docker")
    where.add_argument("--container", help="build in this running container, which has the source at /project")
    libs_macos = libs.add_parser("macos", parents=[common], help="signed when APPLE_SIMPLEX_* are set, and linked for the app")
    for platform_libs in (libs_linux, libs_macos):
        platform_libs.add_argument("--database", type=Database, choices=list(Database), default=Database.SQLITE)
    libs.add_parser("windows", parents=[common], help="in an MSYS2 UCRT64 shell")
    libs_android = libs.add_parser("android", parents=[common], help="the APK's core libraries, built with Nix on this machine")
    libs_android.add_argument("--arch", type=Arch, choices=list(Arch), default=Arch.AARCH64)
    libs_android.add_argument("--install-nix", action="store_true", help="install Nix first, as on a CI runner")
    libs.add_parser("ios-sim", parents=[common], help="the iOS core package built with cabal, for the simulator tests")
    for signed in (desktop_macos, libs_macos):
        signed.add_argument("--require-signing", action="store_true", help="fail rather than build unsigned")

    openssl = kinds.add_parser("openssl", help="the pinned OpenSSL, and cabal.project.local linking it").add_subparsers(
        dest="platform", required=True
    )
    for name in (Platform.MACOS, Platform.WINDOWS):
        openssl.add_parser(name, parents=[common])

    apk = kinds.add_parser("apk", parents=[common], help="the deterministic APK and its core libraries, signed when ANDROID_* are set")
    apk.add_argument("arch", type=Arch, choices=list(Arch))
    apk.add_argument("--nix-cache", action="store_true", help="substitute from zw3rk's and IOG's caches")
    aab = kinds.add_parser("aab", parents=[common], help="the Play bundle, from both arches' core libraries")
    aab.add_argument("--libs", type=Path, required=True, help="directory with the pkg-<arch>-android-*.zip files")
    xcarchive = kinds.add_parser("xcarchive", parents=[common], help="the iOS archive, its bundles signed ad hoc with their entitlements")
    xcarchive.add_argument("--art", type=Path, help="checkout of simplex-chat-art, with the brand assets (default: none, with a warning)")
    xcarchive.add_argument("--install-nix", action="store_true", help="install Nix first, as on a CI runner")

    args = parser.parse_args(argv)
    match args.kind, getattr(args, "platform", None):
        case "desktop", Platform.LINUX:
            return BuildDesktopLinux(linux_image(args.image, platform.machine()), args.version, args.out, args.source)
        case "desktop", Platform.MACOS:
            return BuildDesktopMacos(args.out, args.source, args.require_signing)
        case "desktop", Platform.WINDOWS:
            return BuildDesktopWindows(args.out, args.source)
        case "libs", Platform.LINUX:
            image = linux_image(args.ubuntu, platform.machine())
            builder = LibsBuilder(args.host, args.container)
            return BuildLinuxLibs(image, args.database, args.dist_dir, builder, args.out, args.source)
        case "libs", Platform.MACOS:
            return BuildMacosLibs(args.database, args.out, args.source, args.require_signing)
        case "libs", Platform.WINDOWS:
            return BuildWindowsLibs(args.out, args.source)
        case "libs", "android":
            return BuildAndroidLibs(args.arch, args.out, args.source, args.install_nix)
        case "libs", "ios-sim":
            return BuildIosSimLibs(args.out, args.source)
        case "openssl", str(openssl_platform):
            return BuildOpenssl(Platform(openssl_platform), args.out, args.source)
        case "apk", _:
            return BuildApk(args.arch, args.out, args.source, args.nix_cache)
        case "aab", _:
            return BuildBundle(args.libs, args.out, args.source)
        case _:
            return BuildIosArchive(args.art, args.out, args.source, args.install_nix)


def parse_install_args(argv: Sequence[str]) -> InstallLibs:
    parser = argparse.ArgumentParser(prog="simplex-chat-build.py install", description="Install prebuilt core libraries.")
    kinds = parser.add_subparsers(dest="kind", required=True)
    libs = kinds.add_parser("libs", help="the core libraries, where the apps' builds take them").add_subparsers(
        dest="platform", required=True
    )
    for name, package, help in (
        ("linux", "the library zip of build libs linux", "the desktop app's libraries, linked for its Gradle build"),
        ("android", "the directory with the pkg-<arch>-android-*.zip of build libs android", "the APK's libraries"),
        ("ios", "the iOS core package", "for macOS, iOS and the simulator, with project.pbxproj pointing at them"),
    ):
        sub = libs.add_parser(name, help=help)
        sub.add_argument("package", type=Path, help=package)
        sub.add_argument("--source", type=Path, default=Path("."), help="the simplex-chat checkout (default: current directory)")
        if name == "android":
            sub.add_argument("--arch", type=Arch, choices=list(Arch), default=Arch.AARCH64)
    args = parser.parse_args(argv)
    return InstallLibs(args.platform, args.package, getattr(args, "arch", Arch.AARCH64), args.source)


def parse_test_args(argv: Sequence[str]) -> TestRelease | MakeOldDatabase:
    parser = argparse.ArgumentParser(prog="simplex-chat-build.py test", description="Run a release file as its users do.")
    kinds = parser.add_subparsers(dest="kind", required=True)
    for name, file, help in (
        ("cli", "the CLI binary", "the CLI starts and prints its version"),
        ("desktop", "the .deb, .AppImage, .dmg or .msi", "the app installs, opens its window and draws it, with a screenshot"),
        ("apk", "the APK, signed with a throwaway key when unsigned", "the app starts in the running emulator, with a screenshot"),
    ):
        sub = kinds.add_parser(name, help=help)
        sub.add_argument("file", type=Path, help=file)
        sub.add_argument("--out", type=Path, required=True, help="where the screenshots and logs go")
        sub.add_argument("--database", type=Path, help=f"the directory of test old-database: the file must open {OLD_RELEASE}'s database")
        if name in ("desktop", "apk"):
            peer = "checks that the app migrated the database" if name == "desktop" else "connects with the app as its peer"
            sub.add_argument("--cli", type=Path, help=f"the new CLI, which {peer}")
            sub.add_argument(
                "--upgrade-from-previous",
                action="store_true",
                help=f"install the file over {OLD_RELEASE}'s app, which makes its database first; no --database",
            )
    old = kinds.add_parser("old-database", help=f"an encrypted database made by {OLD_RELEASE}'s CLI, on Linux")
    old.add_argument("--out", type=Path, required=True, help="where the database goes")
    args = parser.parse_args(argv)
    if args.kind == "old-database":
        return MakeOldDatabase(args.out)
    upgrade = getattr(args, "upgrade_from_previous", False)
    if upgrade and args.database:
        parser.error("--upgrade-from-previous: the app makes its own database instead of --database")
    if upgrade and args.kind == "desktop" and not args.cli:
        parser.error("--upgrade-from-previous needs --cli, which checks that the desktop app migrated its database")
    return TestRelease(args.kind, args.file, args.out, args.database, getattr(args, "cli", None), upgrade)


def parse_arches(value: str) -> tuple[Arch, ...]:
    try:
        return tuple(Arch(arch) for arch in value.split(","))
    except ValueError as e:
        raise argparse.ArgumentTypeError(f"{e}, choose from {', '.join(Arch)}") from e


def parse_repo(value: str) -> str:
    if not GITHUB_REPO.fullmatch(value):
        raise argparse.ArgumentTypeError(f"{value} is not a GitHub repository as owner/name")
    return value


def parse_args(argv: Sequence[str]) -> Command:
    match argv[:1]:
        case ["build"]:
            return parse_build_args(argv[1:])
        case ["reproduce"]:
            return parse_reproduce_args(argv[1:])
        case ["install"]:
            return parse_install_args(argv[1:])
        case ["test"]:
            return parse_test_args(argv[1:])
        case ["compare"]:
            return parse_compare_args(argv[1:])
    parser = argparse.ArgumentParser(usage="simplex-chat-build.py {build,install,test,reproduce,compare} ...", description=__doc__)
    parser.error("the first argument is build, install, test, reproduce or compare")


def parse_reproduce_args(argv: Sequence[str]) -> Reproduce:
    parser = argparse.ArgumentParser(
        prog="simplex-chat-build.py reproduce", description="Reproduce a SimpleX Chat release and compare it with the released files."
    )
    parser.add_argument("tag", help="release tag, e.g. v7.1.0")
    parser.add_argument("targets", nargs="*", type=Target, choices=list(Target), help="default: desktop")
    parser.add_argument("--repo", type=parse_repo, default=SIMPLEX_REPO, help=f"the release's GitHub repository (default: {SIMPLEX_REPO})")
    parser.add_argument("--arches", type=parse_arches, default=tuple(Arch), help="Android arches, e.g. aarch64,armv7a")
    parser.add_argument("--nix-cache", action="store_true", help="substitute the Android toolchains from zw3rk's and IOG's caches")
    parser.add_argument("--art", type=Path, help="ios: checkout of simplex-chat-art, with the brand assets of the release's archive")
    parser.add_argument("--install-nix", action="store_true", help="ios: install Nix first, as on a CI runner")
    args = parser.parse_args(argv)
    targets = tuple(args.targets) or (Target.DESKTOP,)
    return Reproduce(args.tag, args.repo, targets, args.arches, args.nix_cache, args.art, args.install_nix)


def parse_compare_args(argv: Sequence[str]) -> Compare:
    parser = argparse.ArgumentParser(
        prog="simplex-chat-build.py compare", description="Compare a released file with a built one, allowing only its signature to differ."
    )
    parser.add_argument("released", type=Path, help="the release's file: a CLI, dmg, msi, or a file of another release")
    parser.add_argument("built", type=Path, help="the same file, made by build")
    parser.add_argument("--repo", type=parse_repo, default=SIMPLEX_REPO, help="the release's repository, whose signer a dmg must have")
    args = parser.parse_args(argv)
    return Compare(args.released, args.built, args.repo)


def main(argv: Sequence[str]) -> int:
    # Windows consoles print in a code page without the characters of msi stream names, which compare logs
    for stream in (sys.stdout, sys.stderr):
        if isinstance(stream, io.TextIOWrapper):
            stream.reconfigure(errors="backslashreplace")
    command = parse_args(argv)
    os.environ["DOCKER_BUILDKIT"] = "1"
    try:
        execute(command)
    except BuildError as e:
        error(str(e))
        return 1
    return 0
