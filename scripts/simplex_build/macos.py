"""The macOS CLI, dmg and core libraries, signed and notarized with Apple's keys when they are given."""

import base64
import contextlib
import hashlib
import json
import os
import plistlib
import re
import shutil
import stat
import struct
import sys
import tarfile
import tempfile
import time
import zipfile
from collections.abc import Iterator, Sequence
from pathlib import Path

from .apk_zip import LOCAL_HEADER, central_directory
from .apple import XCODE, build_root, machine_paths, require_xcode
from .buildroot import check_machine_paths, find_machine_paths
from .common import (
    GHC,
    SIMPLEX_REPO,
    BuildError,
    checkout,
    compare_release,
    download_release,
    edit_lines,
    fetch_pinned,
    remove_tree,
    run,
    single_dir,
    single_file,
    touch_tree,
    warn,
)
from .desktop import (
    COMPOSE_ARCHES,
    DESKTOP_ENV,
    DESKTOP_RELEASE_DIR,
    SOURCE_DATE_EPOCH,
    VLC_RELEASE,
    VLC_SHA256,
    Database,
    build_host_cli,
    check_exports,
    client_library_constraints,
    desktop_libs,
    link_desktop_libs,
    macos_desktop,
    zip_desktop_libs,
)
from .dmg import build_dmg, build_libdmg, copy_dmg_signature, dmg_signature
from .ios import minimum_versions, version_tuple
from .macho import CODE_DIFFERS, same_code
from .openssl import OpenSsl, check_static_openssl, host_openssl, openssl_work, static_libcrypto

APPLE_TEAM = "5NN7GUYB6T"


APPLE_IDENTITY = f"Developer ID Application: SimpleX Chat Ltd ({APPLE_TEAM})"


LIBFFI_URL = "https://github.com/libffi/libffi/releases/download/v3.5.2/libffi-3.5.2.tar.gz"


LIBFFI_SHA256 = "f3a3082a23b37c293a4fcd1053147b371f2ff91fa7ea1b2a52e335676bac82dc"


# Cabal and GHC link each library with an rpath to the directory of each library it depends on, GHC's own and the SDK's
# among them, and GHC adds them again after the link; the libraries find each other at @loader_path
LINKER = """#!{python}
import sys
sys.path.insert(0, {scripts!r})
from simplex_build.link_without_rpaths import link
link(sys.argv[1:])
"""


LINKER_NAME = "cc-without-rpaths"


# GHC's libraries list the Command Line Tools' SDK, where GHC's configure found libiconv, in their library directories,
# which the linker searches before SDKROOT
COMMAND_LINE_TOOLS_SDK = Path("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk")


# The minimum macOS of the CLI and libraries: that of the launcher and the JDK runtime, the newest of the app's parts that
# are not built here, on both arches (VLC's, skiko's and GHC's libraries need 10.8 to 11.0)
MACOS_DEPLOYMENT_TARGET = "11.0"


# where Compose copies the libraries directory into the app; VLC's libraries there have the paths of the machine that
# built them, which is GitHub's, as this one can be
COMPOSE_RESOURCES = "Contents/app/resources"


# the jars that Compose copies into the app, named with a hash of the jar it copied, and the classpath that lists them
COMPOSE_JAR_NAME = re.compile(r"(?P<name>.+)-[0-9a-f]{1,32}\.jar")


CLASSPATH_PREFIX = "app.classpath=$APPDIR/"


DOS_EPOCH_YEAR = 1980


# the partition of a dmg's volume, in hdiutil attach's list of its partitions
HFS_PARTITION = "Apple_HFS"


# rpaths relative to the library or the app, the only ones that the libraries keep: dyld searches an absolute one, which
# another user can create, before @loader_path, and the app's entitlements disable library validation
RELATIVE_RPATHS = ("@loader_path", "@executable_path")


# directories of the build machine that the libraries must not load from
LOCAL_DIRS = re.compile("/Users|/opt/|/usr/local|/Applications/|/Library/Developer/")


# the metadata of Compose's signature that the app keeps when it is signed again
PRESERVED_SIGNATURE = "--preserve-metadata=entitlements,flags,runtime"


APPLE_SIGNING_ENV = ("APPLE_SIMPLEX_SIGNING_KEYCHAIN", "APPLE_SIMPLEX_NOTARIZATION_APPLE_ID", "APPLE_SIMPLEX_NOTARIZATION_PASSWORD")


def apple_signing(require: bool) -> bool:
    """Whether the APPLE_SIGNING_ENV variables, which the keychain, signing and notarization read, are all set."""
    present = [name for name in APPLE_SIGNING_ENV if os.environ.get(name)]
    if len(present) == len(APPLE_SIGNING_ENV):
        return True
    names = ", ".join(APPLE_SIGNING_ENV)
    if present:
        raise BuildError(f"set all of {names} to sign, or none; set: {', '.join(present)}")
    if require:
        raise BuildError(f"{names} are not set, and --require-signing is given")
    warn(f"{names} are not set: building an unsigned, not notarized dmg and libraries")
    return False


def otool(flag: str, lib: Path) -> list[str]:
    """What otool prints about lib, without its first line, which is lib's path."""
    return run(["otool", flag, lib], capture=True).stdout.splitlines()[1:]


def rpath_dependencies(lib: Path) -> list[str]:
    """The names of the libraries that lib loads from its rpaths; the first @rpath line is lib's own id."""
    return [line.strip().split(" ")[0].split("/")[1] for line in otool("-L", lib) if "@rpath" in line][1:]


def rpaths(lib: Path) -> list[str]:
    return [line.split()[1] for line in otool("-l", lib) if "path " in line]


def libcrypto_path(lib: Path) -> str | None:
    return next((line.split()[1] for line in otool("-l", lib) if "libcrypto" in line), None)


def build_libffi(work: Path) -> Path:
    """libffi.dylib, which GHC's libraries load but macOS doesn't ship."""
    with tarfile.open(fetch_pinned(LIBFFI_URL, LIBFFI_SHA256, work / "libffi.tar.gz")) as tar:
        tar.extractall(work, filter="data")
    source = work / "libffi-3.5.2"
    run(["./configure"], cwd=source)
    run(["make"], cwd=source)
    return single_file(source, "*-apple-darwin*/.libs/libffi.dylib")


def cabal_store_libraries() -> Path:
    """The directory where cabal links the libraries of its package store, in the CABAL_DIR of build_root."""
    cabal_dir = os.environ.get("CABAL_DIR")
    if not cabal_dir:
        raise BuildError("CABAL_DIR is not set: the libraries are built in build_root")
    return Path(cabal_dir) / f"store/ghc-{GHC}/lib"


def copy_mac_dependencies(lib: Path, deps: Path, ghc_libdir: Path, libraries: Sequence[Path], copied: set[Path]) -> None:
    """lib and the libraries it loads at @rpath, recursively, from libraries or from the directories relative to GHC's
    libdir in its rpaths; GHC's libraries find each other at @loader_path."""
    if lib in copied:
        return
    copied.add(lib)
    own_rpaths = rpaths(lib)
    shutil.copy(lib, deps)
    if any("@loader_path/.." in rpath for rpath in own_rpaths):
        run(["install_name_tool", "-add_rpath", "@loader_path", deps / lib.name])
    relative = [Path(rpath.replace("@loader_path/..", str(ghc_libdir), 1)) for rpath in own_rpaths if "@loader_path/.." in rpath]
    search = [*libraries, *relative]
    for name in rpath_dependencies(lib):
        dependency = next((directory / name for directory in search if (directory / name).is_file()), None)
        if dependency is None:
            if not (deps / name).is_file():
                raise BuildError(f"{name}, which {lib.name} loads, is in none of {', '.join(map(str, search))}")
            continue
        if not (deps / name).is_file():
            shutil.copy(dependency, deps)
        copy_mac_dependencies(dependency, deps, ghc_libdir, libraries, copied)


def check_relative_rpaths(libraries: Sequence[Path]) -> None:
    absolute = [f"{lib.name} ({rpath})" for lib in libraries for rpath in rpaths(lib) if not rpath.startswith(RELATIVE_RPATHS)]
    if absolute:
        raise BuildError(f"these libraries have rpaths that are not relative to them: {', '.join(absolute)}")


def bundle_mac_libcrypto(libs: Path, database: Database) -> None:
    """Copies the libcrypto that the libraries link into libs, and points them at the copy and their rpaths at libs."""
    smplxmq = single_file(libs, "libHSsmplxmq-*.dylib")
    libcrypto = libcrypto_path(smplxmq)
    if libcrypto is None:
        raise BuildError(f"{smplxmq.name} doesn't load libcrypto")
    bundled = "libcrypto.3.0.dylib"
    shutil.copy(libcrypto, libs / bundled)
    (libs / bundled).chmod(0o755)
    run(["install_name_tool", "-id", bundled, libs / bundled])
    run(["install_name_tool", "-id", "libffi.8.dylib", libs / "libffi.dylib"])
    patterns = ["libHSsmplxmq-*.dylib", "libsimplex.dylib"]
    if database == Database.SQLITE:
        patterns += ["libHSdrct-*.dylib", "libHSsqlcphr-*.dylib"]
    for lib in (lib for pattern in patterns for lib in libs.glob(pattern)):
        linked = libcrypto_path(lib)
        if linked:
            run(["install_name_tool", "-change", linked, f"@rpath/{bundled}", lib])
    dylibs = sorted(p for p in libs.rglob("*.dylib") if p.is_file())
    check_relative_rpaths(dylibs)
    local = [lib.name for lib in dylibs if any(LOCAL_DIRS.search(line) for line in otool("-l", lib))]
    if local:
        message = f"these libraries still point to local directories: {', '.join(local)}"
        # the postgres libraries are not distributed
        if database == Database.SQLITE:
            raise BuildError(message)
        warn(message)


def build_mac_lib(source: Path, arch: str, database: Database, work: Path) -> Path:
    """libsimplex.dylib and the libraries it loads, in the desktop app's libraries directory, which it returns."""
    check_exports(source)
    ghc_libdir = Path(run(["ghc", "--print-libdir"], capture=True).stdout.strip())
    rts_dir = ghc_libdir / f"{arch}-osx-ghc-{GHC}"
    for old in source.glob(f"dist-newstyle/build/{arch}-*/ghc-*/simplex-chat-*"):
        shutil.rmtree(old)
    options = (
        f"-optl-Wl,-rpath,@loader_path -optl-Wl,-install_name,@rpath/libsimplex.dylib -optl-Wl,-L{rts_dir} "
        f"-optl-lHSrts_thr-ghc{GHC} -optl-lffi"
    )
    run(["cabal", "build", "lib:simplex-chat", f"--ghc-options={options}", *client_library_constraints(database)], cwd=source)
    build = single_dir(source, f"dist-newstyle/build/{arch}-*/ghc-*/simplex-chat-*/build")
    for lib in build.glob("libHSsimplex-chat-*-inplace-ghc*.dylib"):
        lib.rename(build / "libsimplex.dylib")
    deps = build / "deps"
    deps.mkdir(exist_ok=True)
    shutil.copy(build_libffi(work), deps / "libffi.dylib")
    copy_mac_dependencies(build / "libsimplex.dylib", deps, ghc_libdir, [cabal_store_libraries(), rts_dir], set())
    shutil.copy(rts_dir / f"libHSghc-boot-th-{GHC}-ghc{GHC}.dylib", deps)
    (deps / "libsimplex.dylib").unlink()
    libs = desktop_libs(source, f"mac-{arch}")
    shutil.rmtree(libs, ignore_errors=True)
    shutil.rmtree(source / "apps/multiplatform/desktop/build/cmake", ignore_errors=True)
    libs.mkdir(parents=True)
    for path in [*deps.iterdir(), build / "libsimplex.dylib"]:
        shutil.copy(path, libs)
    bundle_mac_libcrypto(libs, database)
    return libs


def add_vlc_mac(libs: Path, arch: str, work: Path) -> None:
    vlc = libs / "vlc"
    (vlc / "vlc").mkdir(parents=True, exist_ok=True)
    name = f"vlc-macos-{arch}.zip"
    extracted = work / "vlc"
    extracted.mkdir()
    run(["unzip", "-oqq", fetch_pinned(f"{VLC_RELEASE}/{name}", VLC_SHA256[name], work / name)], cwd=extracted)
    run(["install_name_tool", "-add_rpath", "@loader_path/VLC.app/Contents/MacOS/lib", "vlc-cache-gen"], cwd=extracted)
    macos = extracted / "VLC.app/Contents/MacOS"
    for lib in sorted((macos / "lib").glob("*.dylib")):
        run(["install_name_tool", "-add_rpath", "@loader_path", lib], check=False, capture=True)
    for lib in sorted((macos / "plugins").glob("*.dylib")):
        run(["install_name_tool", "-add_rpath", "@loader_path/../../", lib], check=False, capture=True)
    # the plugin cache records each plugin's modification time
    touch_tree(macos / "plugins", SOURCE_DATE_EPOCH)
    run([extracted / "vlc-cache-gen", "plugins"], cwd=macos)
    for path in (macos / "lib").iterdir():
        shutil.copy(path, vlc)
    shutil.copytree(macos / "plugins", vlc / "vlc/plugins", symlinks=True, dirs_exist_ok=True)


@contextlib.contextmanager
def apple_keychain(work: Path) -> Iterator[Path]:
    """The keychain of APPLE_SIMPLEX_SIGNING_KEYCHAIN, unlocked and in the search list, removed on exit."""
    keychain = work / "simplex.keychain"
    keychain.write_bytes(base64.b64decode(os.environ["APPLE_SIMPLEX_SIGNING_KEYCHAIN"]))
    searched = [line.strip().strip('"') for line in run(["security", "list-keychains"], capture=True).stdout.splitlines()]
    run(["security", "unlock-keychain", "-p", "", keychain])
    # codesign finds the certificate in a keychain outside the search list, then fails to sign with it
    run(["security", "list-keychains", "-s", *searched, keychain])
    try:
        yield keychain
    finally:
        run(["security", "list-keychains", "-s", *searched])
        keychain.unlink()


def notarize(path: Path, work: Path) -> None:
    """Submits a dmg, or a zip of another path, to Apple's notary service, fails unless it is accepted, and staples the
    ticket to a dmg."""
    dmg = path.suffix == ".dmg"
    archive = path if dmg else work / f"{path.name}.zip"
    if not dmg:
        run(["ditto", "-c", "-k", "--keepParent", path, archive])
    credentials = [
        "--apple-id",
        os.environ["APPLE_SIMPLEX_NOTARIZATION_APPLE_ID"],
        "--password",
        os.environ["APPLE_SIMPLEX_NOTARIZATION_PASSWORD"],
        "--team-id",
        APPLE_TEAM,
    ]
    submit: list[str | Path] = ["xcrun", "notarytool", "submit", archive, "--wait", "--timeout", "1h", "--output-format", "json"]
    result = json.loads(run([*submit, *credentials], capture=True, check=False, title=" ".join(map(str, submit))).stdout)
    run(["xcrun", "notarytool", "log", result["id"], *credentials], title=f"xcrun notarytool log {result['id']}")
    if result["status"] != "Accepted":
        raise BuildError(f"notarization of {path.name} is {result['status']}")
    if dmg:
        run(["xcrun", "stapler", "staple", path])


def sign_mac_libs(libs: Path, keychain: Path, work: Path) -> None:
    for lib in sorted(libs.glob("*.dylib")):
        run(["codesign", "--remove-signature", lib])
        run(["codesign", "--timestamp", "--options", "runtime", "--force", "--sign", APPLE_IDENTITY, "--keychain", keychain, lib])
    notarize(libs, work)


def build_app(source: Path, keychain: Path | None) -> Path:
    """The desktop app, SimpleX.app, signed by Compose with the keychain or else as Compose signs unsigned apps."""
    multiplatform = source / "apps/multiplatform"
    properties = multiplatform / "local.properties"
    if keychain:
        # Compose signs only when the notarization properties are also set
        properties.write_text(
            f"desktop.mac.signing.identity={APPLE_IDENTITY}\n"
            f"desktop.mac.signing.keychain={keychain}\n"
            f"desktop.mac.notarization.apple_id={os.environ['APPLE_SIMPLEX_NOTARIZATION_APPLE_ID']}\n"
            f"desktop.mac.notarization.password={os.environ['APPLE_SIMPLEX_NOTARIZATION_PASSWORD']}\n"
            f"desktop.mac.notarization.team_id={APPLE_TEAM}\n"
        )
    try:
        run(["./gradlew", f"-Psimplex.assets.dir={DESKTOP_ENV['ASSETS_DIR']}", "createDistributable"], cwd=multiplatform)
    finally:
        properties.unlink(missing_ok=True)
    return single_dir(source / DESKTOP_RELEASE_DIR / "app", "*.app")


def dos_date_time(epoch: str) -> bytes:
    t = time.gmtime(int(epoch))
    dos_time = (t.tm_hour << 11) | (t.tm_min << 5) | (t.tm_sec // 2)
    dos_date = ((t.tm_year - DOS_EPOCH_YEAR) << 9) | (t.tm_mon << 5) | t.tm_mday
    return struct.pack("<HH", dos_time, dos_date)


def normalize_jar(jar: Path, epoch: str) -> None:
    """Dates each entry of jar with epoch, in its local and central headers."""
    original = jar.read_bytes()
    data = bytearray(original)
    date_time = dos_date_time(epoch)
    for entry in central_directory(original):
        if data[entry.local_offset : entry.local_offset + 4] != LOCAL_HEADER:
            raise BuildError(f"no local header for {entry.name} in {jar.name}")
        data[entry.central_offset + 12 : entry.central_offset + 16] = date_time
        data[entry.local_offset + 10 : entry.local_offset + 14] = date_time
    jar.write_bytes(data)


def normalize_app(app: Path) -> None:
    """Dates the entries of the jars, which Compose packs again with the current time, with SOURCE_DATE_EPOCH, and names
    each jar, in the app and its classpath, with a hash of its content in place of Compose's hash."""
    jars = app / "Contents/app"
    renamed = {}
    for jar in sorted(jars.glob("*.jar")):
        normalize_jar(jar, SOURCE_DATE_EPOCH)
        match = COMPOSE_JAR_NAME.fullmatch(jar.name)
        if match:
            with jar.open("rb") as f:
                digest = hashlib.file_digest(f, lambda: hashlib.md5(usedforsecurity=False)).hexdigest()
            renamed[jar.name] = f"{match['name']}-{digest}.jar"
            jar.rename(jars / renamed[jar.name])

    listed = dict.fromkeys(renamed, 0)

    def rename_classpath(line: str) -> str:
        name = line.removeprefix(CLASSPATH_PREFIX)
        if not line.startswith(CLASSPATH_PREFIX) or name not in renamed:
            return line
        listed[name] += 1
        return CLASSPATH_PREFIX + renamed[name]

    cfg = jars / f"{app.stem}.cfg"
    edit_lines(cfg, rename_classpath)
    unlisted = sorted(name for name, count in listed.items() if count != 1)
    if unlisted:
        raise BuildError(f"{cfg.name} lists these jars other than once: {', '.join(unlisted)}")


def sign_app(app: Path, keychain: Path | None) -> None:
    """Signs the bundle again after normalize_app, with the keychain's identity or, when Compose signed it ad hoc, ad hoc,
    keeping the entitlements and options of Compose's signature, and verifies the signatures of the bundle and its code."""
    if keychain:
        run(["codesign", "--force", PRESERVED_SIGNATURE, "--timestamp", "--sign", APPLE_IDENTITY, "--keychain", keychain, app])
    elif (app / "Contents/_CodeSignature").is_dir():
        run(["codesign", "--force", PRESERVED_SIGNATURE, "--sign", "-", app])
    else:
        return
    run(["codesign", "--verify", "--deep", "--strict", "--verbose=2", app])


def sign_dmg(dmg: Path, keychain: Path, work: Path) -> None:
    """Signs dmg with the keychain's identity, then notarizes it."""
    run(["codesign", "--timestamp", "--sign", APPLE_IDENTITY, "--keychain", keychain, dmg])
    notarize(dmg, work)


def check_dmg(dmg: Path, signed: bool, work: Path) -> None:
    """Fails unless hdiutil verifies dmg, fsck_hfs finds its volume consistent, and codesign verifies the signature of a
    signed dmg, or of an ad hoc signed copy of an unsigned one, as it must sign the release's dmg."""
    run(["hdiutil", "verify", dmg])
    attached = run(["hdiutil", "attach", "-nomount", "-readonly", "-noverify", dmg], capture=True).stdout.splitlines()
    devices = [line.split()[0] for line in attached if line.startswith("/dev/")]
    volumes = [line.split()[0] for line in attached if line.startswith("/dev/") and HFS_PARTITION in line]
    try:
        if len(volumes) != 1:
            raise BuildError(f"expected one {HFS_PARTITION} partition in {dmg.name}, found {len(volumes)}")
        run(["fsck_hfs", "-fn", volumes[0]])
    finally:
        if devices:
            run(["hdiutil", "detach", devices[0]])
    checked = dmg
    if not signed:
        checked = work / f"ad-hoc-{dmg.name}"
        shutil.copy(dmg, checked)
        run(["codesign", "--sign", "-", checked])
    run(["codesign", "--verify", "--strict", "--verbose=2", checked])


def java_home() -> Path:
    home = os.environ.get("JAVA_HOME")
    if not home:
        raise BuildError("JAVA_HOME is not set: set it to the JDK that builds the app")
    return Path(home)


def rpathless_cabal_local(work: Path) -> str:
    """cabal.project.local that links every package without absolute rpaths, with the linker of link_without_rpaths.py
    written to work; the CLI and the libraries use the same, so that they share the packages they depend on."""
    linker = work / LINKER_NAME
    linker.write_text(LINKER.format(python=sys.executable, scripts=str(Path(__file__).resolve().parents[1])))
    linker.chmod(0o755)
    return f"package *\n    ghc-options: -fno-use-rpaths -pgml {linker}\n"


def release_macos_libs(
    source: Path, openssl: OpenSsl, database: Database, keychain: Path | None, work: Path, out: Path
) -> Path:
    """The library release, signed and notarized with the keychain; returns the libraries directory of the app,
    linked for its Gradle build."""
    host = macos_desktop()
    # bundle_mac_libcrypto copies the libcrypto that the libraries link into the app
    (source / "cabal.project.local").write_text(openssl.cabal_local() + rpathless_cabal_local(work))
    libs = build_mac_lib(source, host.arch, database, work)
    link_desktop_libs(source, host.libs_dir, f"macos-{COMPOSE_ARCHES[host.arch]}")
    released = work / "libs"
    shutil.copytree(libs, released)
    if keychain:
        sign_mac_libs(released, keychain, work)
    name = host.libs.replace(".zip", "-postgres.zip") if database == Database.POSTGRES else host.libs
    zip_desktop_libs(released, [], out / name)
    return libs


def check_command_line_tools_sdk(sdk: Path) -> None:
    if COMMAND_LINE_TOOLS_SDK.exists() and COMMAND_LINE_TOOLS_SDK.resolve() != sdk.resolve():
        raise BuildError(
            f"{COMMAND_LINE_TOOLS_SDK} is not the macOS SDK of the selected Xcode, {sdk}, and GHC's links take libraries "
            f"from it: link it there with sudo ln -sfn {sdk} {COMMAND_LINE_TOOLS_SDK}"
        )


def macos_build_env() -> dict[str, str]:
    """The macOS SDK of the selected Xcode, which GHC's links don't find without SDKROOT, and the deployment target, which
    is the build machine's macOS without MACOSX_DEPLOYMENT_TARGET."""
    sdk = run(["xcrun", "--sdk", "macosx", "--show-sdk-path"], capture=True).stdout.strip()
    check_command_line_tools_sdk(Path(sdk))
    return {"SDKROOT": sdk, "MACOSX_DEPLOYMENT_TARGET": MACOS_DEPLOYMENT_TARGET}


def check_minimum_system_version(app: Path) -> None:
    """Fails unless the app's Info.plist declares MACOS_DEPLOYMENT_TARGET, which build.gradle.kts sets."""
    with (app / "Contents/Info.plist").open("rb") as f:
        declared = plistlib.load(f).get("LSMinimumSystemVersion")
    if declared != MACOS_DEPLOYMENT_TARGET:
        raise BuildError(f"{app.name} declares macOS {declared}, not {MACOS_DEPLOYMENT_TARGET}, the minimum of its libraries")


def check_deployment_target(binaries: Sequence[Path]) -> None:
    """Fails unless each of binaries, with each dylib in it when it is a directory, runs on MACOS_DEPLOYMENT_TARGET."""
    files = [path for binary in binaries for path in ([binary] if binary.is_file() else sorted(binary.glob("*.dylib")))]
    newer = []
    for path in files:
        versions = minimum_versions(run(["otool", "-l", path], capture=True).stdout)
        if not versions:
            raise BuildError(f"{path.name} has no minimum macOS version")
        newest = max(versions, key=version_tuple)
        if version_tuple(newest) > version_tuple(MACOS_DEPLOYMENT_TARGET):
            newer.append(f"{path.name} ({newest})")
    if newer:
        raise BuildError(f"these need a newer macOS than {MACOS_DEPLOYMENT_TARGET}: {', '.join(newer)}")


def install_macos_libs(libs: Path, source: Path) -> None:
    """libs in the desktop app's libraries directory of source, linked for its Gradle build."""
    host = macos_desktop()
    installed = desktop_libs(source, host.libs_dir)
    remove_tree(installed)
    shutil.copytree(libs, installed, symlinks=True)
    link_desktop_libs(source, host.libs_dir, f"macos-{COMPOSE_ARCHES[host.arch]}")


def build_macos_libs(source: Path, database: Database, out: Path, require_signing: bool) -> None:
    signed = apple_signing(require_signing)
    paths = machine_paths(source, require_xcode(XCODE))
    with build_root(source, macos_build_env()) as root, contextlib.ExitStack() as stack:
        keychain = stack.enter_context(apple_keychain(root.work)) if signed else None
        openssl = host_openssl(openssl_work(root.source))
        libs = release_macos_libs(root.source, openssl, database, keychain, root.work, out)
        check_deployment_target([libs])
        found = find_machine_paths([libs], paths)
        install_macos_libs(libs, source)
    check_machine_paths(found)


def build_desktop_macos(source: Path, out: Path, require_signing: bool) -> None:
    host = macos_desktop()
    signed = apple_signing(require_signing)
    paths = machine_paths(source, require_xcode(XCODE))
    with build_root(source, macos_build_env()) as root, contextlib.ExitStack() as stack:
        work = root.work
        keychain = stack.enter_context(apple_keychain(work)) if signed else None
        openssl_dir = openssl_work(root.source)
        openssl = host_openssl(openssl_dir)
        static = OpenSsl(openssl.include, static_libcrypto(openssl.lib, openssl_dir))
        cli = build_host_cli(root.source, static.cabal_local() + rpathless_cabal_local(work))
        check_static_openssl(cli, run(["otool", "-L", cli], capture=True).stdout)
        shutil.copy(cli, out / host.cli)
        libs = release_macos_libs(root.source, openssl, Database.SQLITE, keychain, work, out)
        found = find_machine_paths([out / host.cli, libs], paths)
        add_vlc_mac(libs, host.arch, work)
        app = build_app(root.source, keychain)
        normalize_app(app)
        sign_app(app, keychain)
        # the app's resources have the libraries and the app's own JNI library, which Gradle builds
        check_deployment_target([out / host.cli, app / COMPOSE_RESOURCES])
        check_relative_rpaths(sorted((app / COMPOSE_RESOURCES).glob("*.dylib")))
        check_minimum_system_version(app)
        found += find_machine_paths([app], paths, skip=[app / COMPOSE_RESOURCES])
        dmg = work / host.desktop
        build_dmg(build_libdmg(work), app, java_home(), work, dmg)
        if keychain:
            sign_dmg(dmg, keychain, work)
        check_dmg(dmg, keychain is not None, work)
        shutil.copy(dmg, out / host.desktop)
    check_machine_paths(found)


# What codesign and stapler add to a bundle, which the signature of its main executable covers: the hashes of its
# resources in _CodeSignature/CodeResources, and the notarization ticket in Contents/CodeResources
CODE_SIGNATURE_DIR = "_CodeSignature"


CODE_RESOURCES = "CodeResources"


# jpackage's record of the app, which says whether the app is signed
JPACKAGE_STATE = ".jpackage.xml"


JPACKAGE_UNSIGNED, JPACKAGE_SIGNED = b"<signed>false</signed>", b"<signed>true</signed>"


# The designated requirement of Developer ID applications, for the certificate of the SimpleX team
DEVELOPER_ID_REQUIREMENT = (
    "anchor apple generic and certificate 1[field.1.2.840.113635.100.6.2.6] exists and "
    f'certificate leaf[field.1.2.840.113635.100.6.1.13] exists and certificate leaf[subject.OU] = "{APPLE_TEAM}"'
)


@contextlib.contextmanager
def mounted(dmg: Path, mountpoint: Path) -> Iterator[Path]:
    mountpoint.mkdir(parents=True)
    run(["hdiutil", "attach", "-readonly", "-nobrowse", "-noautoopen", "-noverify", "-mountpoint", mountpoint, dmg])
    try:
        yield mountpoint
    finally:
        run(["hdiutil", "detach", mountpoint])


def signature_item(path: Path) -> bool:
    return path.name == CODE_SIGNATURE_DIR or (path.name == CODE_RESOURCES and path.parent.name in (CODE_SIGNATURE_DIR, "Contents"))


def volume_items(root: Path) -> dict[str, Path]:
    """Everything in root by its path, without what codesign and stapler add to bundles, and with each Compose jar
    under its name without the hash that normalize_app names it with: the jars with native code differ when signed."""
    items = {}
    for directory, dirs, files in os.walk(root):
        for name in dirs + files:
            path = Path(directory, name).relative_to(root)
            if signature_item(path):
                continue
            match = COMPOSE_JAR_NAME.fullmatch(name)
            key = path.with_name(f"{match['name']}.jar") if match and path.parent.name == "app" else path
            items[key.as_posix()] = path
    return items


def rename_jars(cfg: str, renamed: dict[str, str]) -> str:
    """The launcher's classpath with the jars of the built app named as the released app names them."""
    lines = cfg.splitlines(keepends=True)
    for i, line in enumerate(lines):
        name = line.rstrip("\n").removeprefix(CLASSPATH_PREFIX)
        if line.startswith(CLASSPATH_PREFIX) and name in renamed:
            lines[i] = line.replace(name, renamed[name], 1)
    return "".join(lines)


def zip_entry(info: zipfile.ZipInfo) -> tuple[object, ...]:
    """What a zip records about an entry, but the sizes and CRC of its data."""
    return info.filename, info.date_time, info.compress_type, info.flag_bits, info.external_attr, info.extra, info.comment


def same_jar(released: Path, built: Path) -> bool:
    """Whether the jars have the same entries, with the same content or the same code but its signature."""
    try:
        with zipfile.ZipFile(released) as theirs, zipfile.ZipFile(built) as ours:
            entries, built_entries = theirs.infolist(), ours.infolist()
            if theirs.comment != ours.comment or list(map(zip_entry, entries)) != list(map(zip_entry, built_entries)):
                return False
            return all(same_code(theirs.read(a), ours.read(b)) for a, b in zip(entries, built_entries, strict=True))
    except zipfile.BadZipFile:
        return False


def same_item(released: Path, built: Path, renamed: dict[str, str]) -> bool:
    theirs, ours = released.lstat(), built.lstat()
    if theirs.st_mode != ours.st_mode:
        return False
    if stat.S_ISLNK(theirs.st_mode):
        return os.readlink(released) == os.readlink(built)
    if not stat.S_ISREG(theirs.st_mode):
        return True
    data, built_data = released.read_bytes(), built.read_bytes()
    if data == built_data:
        return True
    if released.suffix == ".cfg":
        return data == rename_jars(built_data.decode(), renamed).encode()
    if released.name == JPACKAGE_STATE:
        return data == built_data.replace(JPACKAGE_UNSIGNED, JPACKAGE_SIGNED, 1)
    if released.suffix == ".jar":
        return same_jar(released, built)
    return same_code(data, built_data)


def volume_differences(released: Path, built: Path) -> list[str]:
    """The paths in the released volume that differ from the built one other than by code signatures, then the paths
    that only one of them has."""
    theirs, ours = volume_items(released), volume_items(built)
    renamed = {ours[key].name: path.name for key, path in theirs.items() if key in ours and path.suffix == ".jar"}
    shared = theirs.keys() & ours.keys()
    differences = [theirs[key].as_posix() for key in sorted(shared) if not same_item(released / theirs[key], built / ours[key], renamed)]
    return differences + sorted(f"{path.as_posix()} (only one has it)" for key, path in (theirs | ours).items() if key not in shared)


def signature_differences(dmg: Path, app: Path, repo: str) -> list[str]:
    """The signatures of the dmg and of its app that codesign doesn't verify, as the SimpleX team's Developer ID
    signatures for releases of simplex-chat/simplex-chat."""
    requirement = [f"-R={DEVELOPER_ID_REQUIREMENT}"] if repo == SIMPLEX_REPO else []
    differences = []
    for path, deep in ((dmg, []), (app, ["--deep"])):
        if run(["codesign", "--verify", *deep, "--strict", "--verbose=2", *requirement, path], check=False).returncode:
            differences.append(f"codesign doesn't verify the signature of {path.name}")
    return differences


def dmg_differences(released: Path, built: Path, repo: str, work: Path) -> list[str]:
    """What differs other than signatures: the volumes' contents, then the released dmg and the one that the build
    makes of its app, and the signatures that codesign doesn't verify."""
    work.mkdir(parents=True)
    with mounted(released, work / "released") as theirs, mounted(built, work / "built") as ours:
        differences = volume_differences(theirs, ours)
        if differences:
            return differences
        released_app = single_dir(theirs, "*.app")
        app = work / "app" / released_app.name
        shutil.copytree(released_app, app, symlinks=True)
    data = released.read_bytes()
    if dmg_signature(data)[1]:
        differences += signature_differences(released, app, repo)
    elif repo == SIMPLEX_REPO:
        differences.append("it is not signed")
    rebuilt = work / "rebuilt.dmg"
    build_dmg(build_libdmg(work), app, java_home(), work / "rebuild", rebuilt)
    if copy_dmg_signature(data, rebuilt.read_bytes()) != data:
        differences.append("it is not the dmg that the build makes of its app")
    return differences


def macos_differences(released: Path, built: Path, repo: str, work: Path) -> list[str]:
    if released.suffix == ".dmg":
        return dmg_differences(released, built, repo, work)
    return [] if same_code(released.read_bytes(), built.read_bytes()) else [CODE_DIFFERS]


def reproduce_macos(tag: str, repo: str, source: Path, results: Path) -> list[str]:
    host = macos_desktop()
    out = results / "macos"
    names = (host.cli, host.desktop)
    download_release(tag, repo, names, out / "prebuilt")
    checkout(source, tag)
    (out / "from-source").mkdir(exist_ok=True)
    build_desktop_macos(source, out / "from-source", require_signing=False)
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp).resolve()
        return compare_release(names, out, lambda released, built: macos_differences(released, built, repo, work / released.name))
