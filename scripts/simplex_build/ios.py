"""The iOS archive and core libraries."""

import os
import plistlib
import re
import shutil
import stat
import tempfile
import time
import zipfile
from collections.abc import Sequence
from pathlib import Path

from .apple import (
    CANONICAL_DEVELOPER_DIR,
    XCODE,
    build_root,
    check_aslr_off,
    machine_paths,
    require_xcode,
    run_without_aslr,
)
from .buildroot import check_machine_paths, find_machine_paths
from .common import (
    BuildError,
    byte_differences,
    checkout,
    compare_release,
    download_release,
    fetch_pinned,
    log,
    run,
    touch_tree,
    warn,
)
from .macho import simulator_archive
from .nix import install_nix, nix_env
from .openssl import host_openssl

IOS_CORE_JOB = ".#hydraJobs.aarch64-darwin.aarch64-darwin-ios:lib:simplex-chat"


IOS_CORE_ZIP = "pkg-ios-aarch64-swift-json.zip"


IOS_ARCHIVE_ZIP = "SimpleX.xcarchive.zip"


# The unsigned archive that the App Store build is exported from, and the core it links, which a release publishes
IOS_RELEASE_FILES = (IOS_ARCHIVE_ZIP, IOS_CORE_ZIP)


# The app's Info.plist in the archive, where Xcode records its own build as DTXcodeBuild
IOS_ARCHIVE_APP_INFO = "SimpleX.xcarchive/Products/Applications/SimpleX.app/Info.plist"


IOS_PROJECT = "apps/ios/SimpleX.xcodeproj"


IOS_SCHEME = "SimpleX (iOS)"


IOS_TOOLS = ("xcodebuild", "xcrun", "codesign", "otool", "plutil", "git")


IOS_LIBRARIES = "apps/ios/Libraries"


IOS_PBXPROJ = "apps/ios/SimpleX.xcodeproj/project.pbxproj"


# LC_BUILD_VERSION platforms of the iOS simulator, as otool prints them
IOS_SIMULATOR_PLATFORMS = ("7", "IOSSIMULATOR")


MAC2IOS_SOURCE = "https://raw.githubusercontent.com/zw3rk/mobile-core-tools/4dcb77d5ea896d749381806dfab5358851b08951"


MAC2IOS_SHA256 = {
    "mac2ios.c": "2e55f956eb255ac2b523698d69cb9822ace4df357182e70df97107075b27f934",
    "ar.h": "fe0451cc7c5b92c59662db81cf6519576f5bc7b29c0cc34077e540cd293c5a3f",
    "macho.h": "1d49fd7f0c8d40f4cf3d26d83b7f391ae8f270173fba06e43c2cd55ed09860fd",
}


# The iOS core as flake.nix's aarch64-darwin-ios:lib:simplex-chat builds it, built with cabal for the simulator tests
IOS_SIM_CABAL_LOCAL = """ignore-project: False

package simplex-chat
    flags: +swift +client_library
    static: True
    shared: False
    ghc-options: -threaded -DIOS

package simplexmq
    flags: +swift +client_library +commoncrypto
    extra-include-dirs: {include}
    extra-lib-dirs: {lib}

package direct-sqlcipher
    flags: +commoncrypto

package entropy
    flags: +DoNotGetEntropy
"""


# where the build copies the simplex-chat-art checkout, in its work directory
IOS_ART_COPY = "simplex-chat-art"


IOS_LOCAL_XCCONFIG = "SIMPLEX_ASSETS_DIR = {art}\nSWIFT_ACTIVE_COMPILATION_CONDITIONS = $(inherited) SIMPLEX_ASSETS\n"


PLUTIL_DATE_FORMAT = "%Y-%m-%dT%H:%M:%SZ"


# Xcode writes the build of the macOS that it runs on into each bundle's Info.plist; the archives name this one, of
# macOS 26.6.2, whichever macOS builds them
BUILD_MACHINE_OS_BUILD = "25G83"


BINARY_PLIST = b"bplist00"


def version_tuple(version: str) -> tuple[int, ...]:
    return tuple(int(part) for part in version.split("."))


def minimum_versions(otool_output: str) -> list[str]:
    """Minimum OS versions of the objects in otool -l output: LC_BUILD_VERSION's minos, LC_VERSION_MIN_*'s version."""
    versions = []
    in_version_min = False
    for line in otool_output.splitlines():
        match line.split():
            case ["minos", version, *_]:
                versions.append(version)
            case ["cmd", command, *_]:
                in_version_min = command.startswith("LC_VERSION_MIN_")
            case ["version", version, *_] if in_version_min:
                versions.append(version)
                in_version_min = False
    return versions


def build_settings(output: str, names: Sequence[str]) -> dict[str, str]:
    settings: dict[str, str] = {}
    for line in output.splitlines():
        key, sep, value = line.strip().partition(" = ")
        if sep and key in names and key not in settings:
            settings[key] = value
    missing = [name for name in names if not settings.get(name)]
    if missing:
        raise BuildError(f"build settings not found: {', '.join(missing)}")
    return settings


def resolve_entitlements(text: str, team: str) -> str:
    # $(AppIdentifierPrefix) is the team id: simplex.chat's apple-app-site-association lists 5NN7GUYB6T.chat.simplex.app
    return text.replace("$(AppIdentifierPrefix)", f"{team}.")


def check_minimum_versions(source: Path, target: str) -> None:
    # mac2ios relabels macOS objects as iOS and keeps their minimum version, which the deployment target must cover
    for lib in sorted((source / "apps/ios/Libraries/ios").glob("*.a")):
        versions = minimum_versions(run(["otool", "-l", lib], capture=True).stdout)
        newest = max(versions, key=version_tuple, default=None)
        print(f"{lib.name}: newest minimum version {newest or 'none'}")
        if newest and version_tuple(newest) > version_tuple(target):
            raise BuildError(f"{lib.name} has objects for iOS {newest}, newer than the deployment target {target}")


def sign_bundle(bundle: Path, entitlements: Path, team: str, work: Path) -> None:
    """Ad-hoc signs a bundle with its entitlements: the export keeps only the entitlements a signature carries."""
    if not entitlements.is_file():
        raise BuildError(f"no entitlements for {bundle.name}: {entitlements}")
    resolved = work / f"{bundle.name}.entitlements"
    resolved.write_text(resolve_entitlements(entitlements.read_text(), team))
    frameworks = bundle / "Frameworks"
    if frameworks.is_dir():
        for code in sorted(frameworks.iterdir()):
            if code.suffix in (".framework", ".dylib"):
                run(["codesign", "--force", "--sign", "-", "--timestamp=none", code])
    run(["codesign", "--force", "--sign", "-", "--timestamp=none", "--entitlements", resolved, bundle])
    signed = run(["codesign", "-d", "--entitlements", "-", "--xml", bundle], capture=True).stdout
    if plistlib.loads(signed.encode()) != plistlib.loads(resolved.read_bytes()):
        raise BuildError(f"the signature of {bundle.name} carries other entitlements than {entitlements}")
    print(f"== {bundle.name}\n{resolved.read_text()}")


def build_mac2ios(work: Path) -> Path:
    """zw3rk's mac2ios, which relabels the objects of macOS libraries as iOS or iOS simulator ones."""
    for name, digest in MAC2IOS_SHA256.items():
        fetch_pinned(f"{MAC2IOS_SOURCE}/{name}", digest, work / name)
    run(["cc", "-O2", "-o", work / "mac2ios", work / "mac2ios.c"])
    return work / "mac2ios"


def check_simulator_libs(sim: Path) -> None:
    for lib in sorted(sim.glob("*.a")):
        output = run(["otool", "-l", lib], capture=True, check=False).stdout
        platforms = {line.split()[1] for line in output.splitlines() if line.split()[:1] == ["platform"]}
        if platforms - set(IOS_SIMULATOR_PLATFORMS):
            raise BuildError(f"{lib.name} has objects for platforms {sorted(platforms)}, not only the iOS simulator")
        if "LC_VERSION_MIN_IPHONEOS" in output:
            raise BuildError(f"{lib.name} has objects marked only with LC_VERSION_MIN_IPHONEOS, which are for iOS devices")


def library_names(names: Sequence[str]) -> tuple[str, str] | None:
    """The plain and the -ghc library of libHSsimplex-chat among names."""
    plain = sorted({name for name in names if "-ghc" not in name})
    ghc = sorted({name for name in names if "-ghc" in name})
    return (plain[-1], ghc[-1]) if plain and ghc else None


def update_pbxproj(source: Path) -> None:
    """Points project.pbxproj, which names the libHSsimplex-chat libraries with their versions, at the installed ones."""
    pbxproj = source / IOS_PBXPROJ
    text = pbxproj.read_text()
    new = library_names([lib.name for lib in (source / IOS_LIBRARIES / "ios").glob("libHSsimplex-chat-*.a")])
    old = library_names(re.findall(r'libHSsimplex-chat-[^ "/]+\.a', text))
    if new is None or old is None:
        raise BuildError(f"no libHSsimplex-chat libraries in {IOS_LIBRARIES}/ios or in {IOS_PBXPROJ}")
    if old == new:
        return

    def references(name: str, text: str) -> int:
        return sum(name in line for line in text.splitlines())

    if [references(name, text) for name in old] != [4, 4]:
        raise BuildError(f"expected 4 references to each of {', '.join(old)} in {IOS_PBXPROJ}")
    # the -ghc name contains the plain one's version, so it is replaced first
    text = text.replace(old[1], new[1]).replace(old[0], new[0])
    if [references(name, text) for name in new] != [4, 4]:
        raise BuildError(f"expected 4 references to each of {', '.join(new)} after updating {IOS_PBXPROJ}")
    pbxproj.write_text(text)
    log(f"{IOS_PBXPROJ}: {old[0]} -> {new[0]}")


def install_ios_libs(source: Path, package: Path, work: Path) -> None:
    """The libraries of an iOS core package for macOS, iOS and the iOS simulator, where the Xcode project links them."""
    libraries = source / IOS_LIBRARIES
    mac2ios = build_mac2ios(work)
    mac, ios, sim = (libraries / name for name in ("mac", "ios", "sim"))
    for directory in (mac, ios, sim):
        shutil.rmtree(directory, ignore_errors=True)
        directory.mkdir(parents=True)
    with zipfile.ZipFile(package) as z:
        z.extractall(mac)
    for lib in sorted(mac.iterdir()):
        lib.chmod(lib.stat().st_mode | stat.S_IWUSR)
        shutil.copy(lib, ios)
        shutil.copy(lib, sim)
    for lib in sorted(ios.iterdir()):
        run([mac2ios, lib], capture=True)
    for lib in sorted(sim.iterdir()):
        run([mac2ios, "-s", lib], capture=True)
        # mac2ios relabels only LC_BUILD_VERSION, while the Nix build's objects carry LC_VERSION_MIN_IPHONEOS
        if simulator_archive(lib):
            run(["ranlib", lib])
    check_simulator_libs(sim)
    update_pbxproj(source)


def cabal_version(source: Path) -> str:
    match = re.search(r"^version:\s*(\S+)", (source / "simplex-chat.cabal").read_text(), re.MULTILINE)
    if match is None:
        raise BuildError("no version in simplex-chat.cabal")
    return match.group(1)


def build_ios_sim_libs(source: Path, out: Path) -> None:
    """The iOS core package built with cabal, which the simulator tests use instead of the slower Nix build."""
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp).resolve()
        openssl = host_openssl(work)
        cabal_local = IOS_SIM_CABAL_LOCAL.format(include=openssl.include, lib=openssl.lib)
        (source / "cabal.project.local").write_text(cabal_local)
        run(["cabal", "build", "-j", "lib:simplex-chat"], cwd=source)
        ghc = run(["ghc", "--numeric-version"], capture=True).stdout.strip()
        build = source / f"dist-newstyle/build/aarch64-osx/ghc-{ghc}/simplex-chat-{cabal_version(source)}/build"
        ffi, gmp = (Path(run(["brew", "--prefix", name], capture=True).stdout.strip()) / "lib" for name in ("libffi", "gmp"))
        package = work / "package"
        package.mkdir()
        for lib in [*build.glob("libHSsimplex-chat-*.a"), ffi / "libffi.a", gmp / "libgmp.a", gmp / "libgmpxx.a"]:
            shutil.copy(lib, package)
            (package / lib.name).chmod(0o644)
        for lib in package.glob("libHSsimplex-chat-*.a"):
            run(["strip", "-x", lib])
        with zipfile.ZipFile(out / IOS_CORE_ZIP, "w", zipfile.ZIP_DEFLATED, compresslevel=9) as z:
            for lib in sorted(package.iterdir()):
                z.write(lib, lib.name)


def commit_epoch(source: Path) -> str:
    return run(["git", "-C", source, "log", "-1", "--format=%ct"], capture=True).stdout.strip()


def zip_tree(root: Path, target: Path, epoch: str) -> None:
    """root under its own name, as ditto --keepParent zips it, in name order, with epoch's date and normalized modes."""
    date_time = time.gmtime(int(epoch))[:6]
    paths = [root]
    for directory, dirs, files in os.walk(root):
        paths += [Path(directory, name) for name in dirs + files]
    with zipfile.ZipFile(target, "w", zipfile.ZIP_DEFLATED) as z:
        for path in sorted(paths):
            name = path.relative_to(root.parent).as_posix()
            mode = path.lstat().st_mode
            kind = stat.S_IFMT(mode)
            permissions = 0o755 if kind == stat.S_IFDIR or mode & 0o111 else 0o644
            info = zipfile.ZipInfo(f"{name}/" if kind == stat.S_IFDIR else name, date_time)
            info.external_attr = (kind | permissions) << 16
            if kind == stat.S_IFREG:
                info.compress_type = zipfile.ZIP_DEFLATED
                info.file_size = path.stat().st_size
                with path.open("rb") as source, z.open(info, "w") as entry:
                    shutil.copyfileobj(source, entry)
            else:
                z.writestr(info, os.readlink(path) if kind == stat.S_IFLNK else b"")


def pin_build_machine(products: Path) -> None:
    """Sets BuildMachineOSBuild to BUILD_MACHINE_OS_BUILD in each Info.plist in products that has it."""
    for info in sorted(products.rglob("Info.plist")):
        raw = info.read_bytes()
        data = plistlib.loads(raw)
        if isinstance(data, dict) and "BuildMachineOSBuild" in data:
            data["BuildMachineOSBuild"] = BUILD_MACHINE_OS_BUILD
            fmt = plistlib.FMT_BINARY if raw.startswith(BINARY_PLIST) else plistlib.FMT_XML
            info.write_bytes(plistlib.dumps(data, fmt=fmt))


def build_ios_archive(source: Path, art: Path | None, out: Path, install: bool) -> None:
    """The archive and the Nix core package it links, with the brand assets of art, the simplex-chat-art checkout, or
    else with the default icons."""
    if art is None:
        warn("no simplex-chat-art checkout (--art): the archive has the default icons, not the release's brand assets")
    elif not (art / "ios/Assets.xcassets").is_dir():
        raise BuildError(f"no ios/Assets.xcassets in {art}")
    developer = require_xcode(XCODE)
    # the dSYMs name Xcode's paths in the Swift modules that they embed and in the API notes of Clang's modules, which
    # -debug-prefix-map doesn't change
    if developer != CANONICAL_DEVELOPER_DIR:
        app = CANONICAL_DEVELOPER_DIR.parents[1]
        raise BuildError(f"Xcode is in {developer.parents[1]}: the iOS archive is built with Xcode in {app}, as its dSYMs name the path")
    paths = machine_paths(source, developer)
    with build_root(source, {}) as root:
        work = root.work
        if install:
            install_nix(work)
        run(["nix", "build", "-L", "--out-link", work / "core", IOS_CORE_JOB], cwd=root.source, env=nix_env())
        install_ios_libs(root.source, work / "core" / IOS_CORE_ZIP, work)
        # the simulator test builds the app with this core, relabelled for the simulator by install libs ios
        shutil.copy(work / "core" / IOS_CORE_ZIP, out / IOS_CORE_ZIP)

        if art is not None:
            # The brand assets, as release-mobile.py adds them: Release.xcconfig includes Local.xcconfig, which turns on
            # SIMPLEX_ASSETS. copy-assets.sh runs first, as its build phase runs after the extensions' catalogs are built.
            assets = work / IOS_ART_COPY
            shutil.copytree(art, assets, symlinks=True)
            (root.source / "apps/ios/Local.xcconfig").write_text(IOS_LOCAL_XCCONFIG.format(art=assets))
            run(["scripts/ios/copy-assets.sh", assets], cwd=root.source)
            if not list((root.source / "apps/ios/Shared/SimpleXAssets.xcassets").glob("*.imageset")):
                raise BuildError(f"copy-assets.sh copied no imagesets from {art}")

        xcode = ["-project", IOS_PROJECT, "-scheme", IOS_SCHEME, "-configuration", "Release"]
        settings_output = run(["xcodebuild", "-showBuildSettings", *xcode], cwd=root.source, capture=True).stdout
        settings = build_settings(settings_output, ["DEVELOPMENT_TEAM", "IPHONEOS_DEPLOYMENT_TARGET"])
        team = settings["DEVELOPMENT_TEAM"]
        check_minimum_versions(root.source, settings["IPHONEOS_DEPLOYMENT_TARGET"])
        epoch = commit_epoch(root.source)
        # the modules that Swift embeds in the dSYMs record the modification time of the bridging header
        touch_tree(root.source / "apps/ios", epoch)

        # Xcode refuses ad-hoc signing for iOS devices ("Ad Hoc code signing is not allowed with SDK"), so the archive
        # is built unsigned and its bundles are signed after.
        archive = work / "SimpleX.xcarchive"
        check_aslr_off(work)
        run_without_aslr(
            [
                "xcodebuild", "archive", "-quiet", *xcode,
                "-destination", "generic/platform=iOS",
                "-archivePath", archive,
                "-derivedDataPath", work / "DerivedData",
                "-onlyUsePackageVersionsFromResolvedFile",
                "-skipPackagePluginValidation",
                "-skipMacroValidation",
                "CODE_SIGNING_ALLOWED=NO",
            ],
            root.source,
        )
        pin_build_machine(archive / "Products")
        app = archive / "Products/Applications/SimpleX.app"
        for appex in sorted((app / "PlugIns").glob("*.appex")):
            sign_bundle(appex, root.source / f"apps/ios/{appex.stem}/{appex.stem}.entitlements", team, work)
        sign_bundle(app, root.source / f"apps/ios/{IOS_SCHEME}.entitlements", team, work)
        run(["codesign", "--verify", "--deep", "--strict", app])
        # the team Xcode records when it signs an archive, which Organizer shows and distributes for
        run(["plutil", "-replace", "ApplicationProperties.Team", "-string", team, archive / "Info.plist"])
        creation_date = time.strftime(PLUTIL_DATE_FORMAT, time.gmtime(int(epoch)))
        run(["plutil", "-replace", "CreationDate", "-date", creation_date, archive / "Info.plist"])
        found = find_machine_paths([archive], paths)
        zip_tree(archive, out / IOS_ARCHIVE_ZIP, epoch)
    check_machine_paths(found)


def archive_xcode(archive: Path) -> str:
    with zipfile.ZipFile(archive) as z:
        info = plistlib.loads(z.read(IOS_ARCHIVE_APP_INFO))
    return str(info["DTXcodeBuild"])


def reproduce_ios(tag: str, repo: str, art: Path | None, install: bool, source: Path, results: Path) -> list[str]:
    """The release's archive and core package, built with the pinned Xcode, which must have built them, and compared
    byte for byte."""
    out = results / "ios"
    download_release(tag, repo, IOS_RELEASE_FILES, out / "prebuilt")
    released = archive_xcode(out / "prebuilt" / IOS_ARCHIVE_ZIP)
    if released != XCODE.build:
        raise BuildError(f"{tag} is archived with Xcode build {released}, and this script builds with {XCODE}")
    checkout(source, tag)
    (out / "from-source").mkdir(exist_ok=True)
    build_ios_archive(source, art, out / "from-source", install)
    return compare_release(IOS_RELEASE_FILES, out, byte_differences)
