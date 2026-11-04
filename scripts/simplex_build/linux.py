"""The Linux CLI, debs, AppImage and core libraries, and their reproduction."""

import contextlib
import platform
import re
import shutil
import tempfile
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from .common import (
    GHC,
    BuildError,
    checkout,
    compare_dirs,
    download,
    edit_lines,
    extract_flat,
    fetch_pinned,
    log,
    normalize_permissions,
    release_url,
    single_dir,
    single_file,
    tag_version,
    touch_tree,
    write_sha256sums,
)
from .desktop import (
    CLI_BUILD_STEPS,
    COMPOSE_ARCHES,
    DESKTOP_ENV,
    DESKTOP_RELEASE_DIR,
    SOURCE_DATE_EPOCH,
    VLC_RELEASE,
    VLC_SHA256,
    Database,
    check_exports,
    client_library_constraints,
    desktop_libs,
    link_desktop_libs,
    remove_skiko_runtime,
    zip_desktop_libs,
)
from .docker import OUT, PROJECT, Container, HostRunner, Runner, Step
from .openssl import OPENSSL_UNIX_DIR, OPENSSL_UNIX_PREFIX, fetch_openssl, openssl_build


@dataclass(frozen=True, slots=True)
class UbuntuImage:
    version: str
    arch: str
    digest: str

    @property
    def suffix(self) -> str:
        return self.version.replace(".", "_")


# The Ubuntu images of build.yml's release builds, for each version and machine
DESKTOP_IMAGES = (
    UbuntuImage("22.04", "x86_64", "sha256:5c8b2c0a6c745bc177669abfaa716b4bc57d58e2ea3882fb5da67f4d59e3dda5"),
    UbuntuImage("24.04", "x86_64", "sha256:98ff7968124952e719a8a69bb3cccdd217f5fe758108ac4f21ad22e1df44d237"),
    UbuntuImage("22.04", "aarch64", "sha256:6a62a4157b8775eaf4959cb629e757d32d39d1f4c8ac1b0ddc2510b555cf72f3"),
    UbuntuImage("24.04", "aarch64", "sha256:68434214381cb38287104e629fe8ee720167dd98cbb36ab1cbbab342515fa6ab"),
)


APPIMAGE_IMAGE = "22.04"


DESKTOP_CABAL_LOCAL = "ignore-project: False\npackage direct-sqlcipher\n    flags: +openssl"


CABAL_DIR = "/home/builder/.cabal"


RELEASE_WORKFLOW = ".github/workflows/build.yml"


def install_linux_libs(source: Path, package: Path) -> None:
    arch = platform.machine()
    extract_flat(package, desktop_libs(source, f"linux-{arch}"), "libs/")
    link_desktop_libs(source, f"linux-{arch}", f"linux-{COMPOSE_ARCHES[arch]}")


CLI_DEB_SOURCE_DATE_EPOCH = "1764547200"


DEB_ARCHES = {"x86_64": "amd64", "aarch64": "arm64"}


LINUX_LIB_GHC_OPTIONS = "-optl-Wl,-rpath,$ORIGIN -optl-Wl,-soname,libsimplex.so -flink-rts -threaded"


# The only directory where the released libraries and programs look for libraries: their own. GHC and cabal link each
# library with the absolute directories, in the build container, of the libraries it loads.
ORIGIN = "$ORIGIN"


ELF_MAGIC = b"\x7fELF"


# readelf -d names each file before its dynamic section when it is given more than one
READELF_FILE = "File: "


READELF_SEARCH_PATHS = ("(RPATH)", "(RUNPATH)")


# The builds of the continuous releases of simplex-chat/appimagetool and simplex-chat/type2-runtime of 2025-06-04
APPIMAGE_TOOLS = {
    "x86_64": (
        ("appimagetool", "appimagetool-x86_64.AppImage", "ad30156bd2c0a364e742b13bd205021e2408399a9090bc2d76048dd504b23f66"),
        ("type2-runtime", "runtime-x86_64", "df90eadfa40b83a4183bf7a7f6fdf9c293eebea83a614fe48ccd2f849a75ad58"),
    ),
    "aarch64": (
        ("appimagetool", "appimagetool-aarch64.AppImage", "1401a0557ed979b32ca82fb136b2f328ff2fafbb024b2b61e155980605cb3ccb"),
        ("type2-runtime", "runtime-aarch64", "75ad8a177eef47943328e541a57911a79a88446c3a1a0c7a172f2046eabebb9e"),
    ),
}


# jpackage's desktop entry passes no URL and handles no MIME type; this registers the simplexchat: and simplex: schemes
DESKTOP_MIME_TYPES = "MimeType=x-scheme-handler/simplexchat;x-scheme-handler/simplex;"


def build_linux_lib(runner: Runner, source: Path, arch: str, database: Database, dist_dir: str = "dist-newstyle") -> Path:
    """libsimplex.so and the GHC libraries it loads, in the desktop app's libraries directory, which it returns."""
    check_exports(source)
    cabal = ("cabal", "build", f"--builddir={dist_dir}", "lib:simplex-chat", f"--ghc-options={LINUX_LIB_GHC_OPTIONS}")
    runner.run(Step((*cabal, *client_library_constraints(database)), runner.path(source)))
    build = single_dir(source / dist_dir / "build" / f"{arch}-linux" / f"ghc-{GHC}", "simplex-chat-*") / "build"
    for lib in build.glob(f"libHSsimplex-chat-*-inplace-ghc{GHC}.so"):
        lib.rename(build / "libsimplex.so")
    # Ubuntu 20.04 runners linked libffi.so.7, which users' systems may lack; libffi.so.8 ships with the libraries
    runner.run(Step(("patchelf", "--replace-needed", "libffi.so.7", "libffi.so.8", "libsimplex.so"), runner.path(build)))
    ldd = runner.run(Step(("ldd", "libsimplex.so"), runner.path(build)), capture=True)
    ghc_libs = [line.split()[2] for line in ldd.splitlines() if "ghc" in line]
    deps = build / "deps"
    deps.mkdir(exist_ok=True)
    if ghc_libs:
        runner.run(Step(("cp", *ghc_libs, runner.path(deps)), runner.path(build)))
    libs = desktop_libs(source, f"linux-{arch}")
    shutil.rmtree(libs, ignore_errors=True)
    shutil.rmtree(source / "apps/multiplatform/desktop/build/cmake", ignore_errors=True)
    libs.mkdir(parents=True)
    for path in [*deps.iterdir(), build / "libsimplex.so"]:
        shutil.copy(path, libs)
    set_origin_runpaths(runner, libs)
    return libs


def elf_files(roots: Sequence[Path]) -> list[Path]:
    """The ELF files that are roots or in them, without symbolic links."""
    candidates = []
    for root in roots:
        candidates += [root] if root.is_file() else sorted(p for p in root.rglob("*") if p.is_file() and not p.is_symlink())
    found = []
    for candidate in candidates:
        with candidate.open("rb") as f:
            if f.read(len(ELF_MAGIC)) == ELF_MAGIC:
                found.append(candidate)
    return found


def library_search_paths(runner: Runner, files: Sequence[Path]) -> dict[Path, list[str]]:
    """The RPATH and RUNPATH entries of each of files, as readelf -d prints them."""
    if not files:
        return {}
    named = {runner.path(file): file for file in files}
    found: dict[Path, list[str]] = {file: [] for file in files}
    current = files[0]
    for line in runner.run(Step(("readelf", "-d", *named), runner.path(files[0].parent)), capture=True).splitlines():
        if line.startswith(READELF_FILE):
            current = named[line.removeprefix(READELF_FILE).strip()]
        elif any(kind in line for kind in READELF_SEARCH_PATHS):
            found[current] += [entry for entry in line.split("[", 1)[1].rstrip().rstrip("]").split(":") if entry]
    return found


def set_origin_runpaths(runner: Runner, libs: Path) -> None:
    """Each library in libs that looks for libraries elsewhere than in its own directory looks only there, where the
    release puts the libraries it loads."""
    for lib, entries in library_search_paths(runner, elf_files([libs])).items():
        if entries and entries != [ORIGIN]:
            runner.run(Step(("patchelf", "--set-rpath", ORIGIN, runner.path(lib)), runner.path(libs)))


def check_origin_runpaths(runner: Runner, roots: Sequence[Path]) -> None:
    """Fails unless each ELF file in roots looks for libraries only in directories relative to its own."""
    outside = [
        f"{file} ({entry})"
        for file, entries in library_search_paths(runner, elf_files(roots)).items()
        for entry in entries
        if entry != ORIGIN and not entry.startswith(f"{ORIGIN}/")
    ]
    if outside:
        raise BuildError("these files look for libraries outside their own directory:\n" + "\n".join(outside))


def add_vlc_linux(runner: Runner, libs: Path, arch: str, work: Path) -> None:
    vlc = libs / "vlc"
    if vlc.exists():
        return
    name = f"vlc-linux-{arch}.appimage"
    appimage = fetch_pinned(f"{VLC_RELEASE}/{name}", VLC_SHA256[name], work / name)
    appimage.chmod(0o755)
    runner.run(Step((runner.path(appimage), "--appimage-extract"), runner.path(work)))
    shutil.copytree(work / "squashfs-root/usr/lib", vlc, symlinks=True)


def build_cli_deb(runner: Runner, binary: Path, version: str, arch: str, work: Path) -> Path:
    """The CLI's deb, with the release's control file and fixed timestamps."""
    build = work / "deb-build"
    package = build / "simplex-chat"
    for directory in (build, package, package / "DEBIAN", package / "usr", package / "usr/bin"):
        directory.mkdir(mode=0o755)
        directory.chmod(0o755)
    size = f"{(binary.stat().st_size + 1023) / 1024:.0f}"
    control = package / "DEBIAN/control"
    control.write_text(
        "Package: simplex-chat\n"
        f"Version: {version}\n"
        "Section: Messenger\n"
        "Priority: optional\n"
        f"Architecture: {DEB_ARCHES[arch]}\n"
        "Maintainer: SimpleX Chat <chat@simplex.chat>\n"
        "Description: SimpleX - the first messaging platform that has no user identifiers of any kind - 100% private by design! (CLI)\n"
        f"Installed-Size: {size}\n"
    )
    control.chmod(0o644)
    shutil.copy(binary, package / "usr/bin/simplex-chat")
    (package / "usr/bin/simplex-chat").chmod(0o755)
    touch_tree(package, CLI_DEB_SOURCE_DATE_EPOCH)
    epoch = {"SOURCE_DATE_EPOCH": CLI_DEB_SOURCE_DATE_EPOCH}
    runner.run(Step(("dpkg-deb", "--build", "--root-owner-group", "--uniform-compression", "./simplex-chat"), runner.path(build), epoch))
    runner.run(Step(("strip-nondeterminism", "./simplex-chat.deb"), runner.path(build), epoch))
    return build / "simplex-chat.deb"


def register_url_schemes(line: str) -> str:
    if line.startswith("Exec=") and "%u" not in line and "%U" not in line:
        return f"{line} %u"
    return DESKTOP_MIME_TYPES if line.startswith("MimeType=") else line


def package_desktop_deb(runner: Runner, source: Path, arch: str, codename: str) -> Path:
    """jpackage's deb without the skiko runtime jar, versioned for the Ubuntu release and with fixed timestamps."""
    multiplatform = source / "apps/multiplatform"
    runner.run(Step(("./gradlew", f"-Psimplex.assets.dir={DESKTOP_ENV['ASSETS_DIR']}", "packageDeb"), runner.path(multiplatform)))
    release = multiplatform / "release/main/deb"
    final = release / f"simplex_{arch}.deb"
    jpackage_debs = [p for p in sorted(release.glob("simplex*.deb")) if p != final]
    if len(jpackage_debs) != 1:
        raise BuildError(f"expected one deb of jpackage in {release}, found {len(jpackage_debs)}")
    extracted = multiplatform / "extracted"
    shutil.rmtree(extracted, ignore_errors=True)
    runner.run(Step(("dpkg-deb", "-R", runner.path(jpackage_debs[0]), "./extracted"), runner.path(multiplatform)))
    app = single_dir(extracted / "opt", "*imple*")
    remove_skiko_runtime(app / "lib/app")
    edit_lines(extracted / "DEBIAN/control", lambda line: f"{line}~{codename}" if "Version" in line else line)
    for entry in app.glob("lib/*.desktop"):
        edit_lines(entry, register_url_schemes)
        if DESKTOP_MIME_TYPES not in entry.read_text().splitlines():
            raise BuildError(f"{entry.name} does not register simplexchat: and simplex:")
    check_origin_runpaths(runner, [extracted])
    touch_tree(extracted, SOURCE_DATE_EPOCH)
    epoch = {"SOURCE_DATE_EPOCH": SOURCE_DATE_EPOCH}
    build = ("dpkg-deb", "--build", "--root-owner-group", "--uniform-compression", "./extracted", runner.path(final))
    runner.run(Step(build, runner.path(multiplatform), epoch))
    runner.run(Step(("strip-nondeterminism", runner.path(final)), runner.path(multiplatform), epoch))
    shutil.rmtree(extracted)
    return final


def ubuntu_codename(runner: Runner) -> str:
    release = runner.run(Step(("cat", "/etc/os-release")), capture=True)
    for line in release.splitlines():
        if line.startswith("VERSION_CODENAME="):
            return line.split("=", 1)[1].strip('"')
    raise BuildError("VERSION_CODENAME is not in /etc/os-release")


def package_appimage(runner: Runner, source: Path, arch: str, libcrypto: Path, work: Path) -> Path:
    """The AppImage of the desktop app, with libcrypto bundled and fixed timestamps."""
    multiplatform = source / "apps/multiplatform"
    libs = desktop_libs(source, f"linux-{arch}")
    bundled = libs / libcrypto.name
    shutil.copy(libcrypto, bundled)
    try:
        runner.run(
            Step(("./gradlew", f"-Psimplex.assets.dir={DESKTOP_ENV['ASSETS_DIR']}", "createDistributable"), runner.path(multiplatform))
        )
    finally:
        bundled.unlink()
    app = multiplatform / "release/main/app"
    appdir = app / "AppDir"
    shutil.rmtree(appdir, ignore_errors=True)
    (appdir / "usr").mkdir(parents=True)
    distributable = single_dir(app, "*imple*")
    for part in ("bin", "lib"):
        shutil.copytree(distributable / part, appdir / "usr" / part, symlinks=True)
    shutil.copy(appdir / "usr/lib/simplex.png", appdir)
    for share in ("icons", "metainfo", "applications"):
        (appdir / "usr/share" / share).mkdir(parents=True)
    # AppImageLauncher shows this icon
    shutil.copy(appdir / "usr/lib/simplex.png", appdir / "usr/share/icons")
    (appdir / "AppRun").symlink_to(Path("usr/bin") / single_file(appdir / "usr/bin", "*imple*").name)
    distribute = multiplatform / "desktop/src/jvmMain/resources/distribute"
    entry = appdir / "chat.simplex.app.desktop"
    shutil.copy(single_file(distribute, "*imple*.desktop"), entry)
    edit_lines(entry, lambda line: re.sub(r"^Exec=[^ ]*", "Exec=simplex", re.sub(r"Icon=.*", "Icon=simplex", line)))
    shutil.copy(entry, appdir / "usr/share/applications")
    for appdata in distribute.glob("*.appdata.xml"):
        shutil.copy(appdata, appdir / "usr/share/metainfo")
    tool, runtime = (
        fetch_pinned(f"https://github.com/simplex-chat/{repo}/releases/download/continuous/{name}", digest, work / name)
        for repo, name, digest in APPIMAGE_TOOLS[arch]
    )
    tool.chmod(0o755)
    remove_skiko_runtime(appdir / "usr/lib/app")
    touch_tree(appdir, SOURCE_DATE_EPOCH)
    epoch = {"SOURCE_DATE_EPOCH": SOURCE_DATE_EPOCH}
    check_origin_runpaths(runner, [appdir])
    appimagetool = (runner.path(tool), "--verbose", "--no-appstream", "--runtime-file", runner.path(runtime), ".")
    runner.run(Step(appimagetool, runner.path(appdir), epoch))
    image = shutil.move(single_file(appdir, "*imple*.AppImage"), multiplatform / "release/main")
    runner.run(Step(("strip-nondeterminism", runner.path(Path(image))), runner.path(multiplatform), epoch))
    return Path(image)


# Releases made before this script built their Linux files with these scripts of their own, which reproduce runs
TAG_CLI_DEB_SCRIPT = "scripts/desktop/build-cli-deb.sh"


# a checkout without it is such a release
BUILD_SCRIPT = "scripts/simplex-chat-build.py"


TAG_DESKTOP_DEB_SCRIPT = "scripts/desktop/make-deb-linux.sh"


TAG_APPIMAGE_SCRIPT = "scripts/desktop/make-appimage-linux.sh"


def desktop_files(image: UbuntuImage) -> list[str]:
    cli = f"simplex-chat-ubuntu-{image.suffix}-{image.arch}"
    files = [cli, f"{cli}.deb", f"simplex-desktop-ubuntu-{image.suffix}-{image.arch}.deb"]
    if image.version == APPIMAGE_IMAGE:
        files.append(f"simplex-desktop-{image.arch}.AppImage")
    return files


def linux_libs_zip(arch: str, database: Database) -> str:
    return f"simplex-chat-libs-linux-{arch}{'-postgres' if database == Database.POSTGRES else ''}.zip"


def linux_image(version: str, machine: str) -> UbuntuImage:
    """The image of an Ubuntu version for this machine: Docker runs images of the host's architecture."""
    for image in DESKTOP_IMAGES:
        if (image.version, image.arch) == (version, machine):
            return image
    raise BuildError(f"no Ubuntu {version} image for {machine}")


def cabal_dir_args(source: Path) -> list[str]:
    """The cabal directory of source's release builds: CABAL_DIR since v7.1.0-beta.7, before it cabal's default
    ~/.local/state/cabal; the binaries contain the path of its package store."""
    if f"CABAL_DIR={CABAL_DIR}" in (source / RELEASE_WORKFLOW).read_text():
        return ["-e", f"CABAL_DIR={CABAL_DIR}"]
    return []


def linux_container(source: Path, image: UbuntuImage, work: Path) -> Container:
    return Container(
        source,
        {source: PROJECT, work: OUT},
        build_args={"TAG": image.version, "HASH": image.digest, "GHC": GHC},
        run_args=["--device", "/dev/fuse", "--cap-add", "SYS_ADMIN", "--security-opt", "apparmor:unconfined", *cabal_dir_args(source)],
    )


def build_desktop_linux(source: Path, image: UbuntuImage, version: str, out: Path) -> None:
    """The Linux release files of an Ubuntu image, with their released names, and on APPIMAGE_IMAGE the libraries."""
    (source / "cabal.project.local").write_text(DESKTOP_CABAL_LOCAL)
    normalize_permissions(source)
    log(f"building Ubuntu {image.version} {image.arch} CLI and desktop apps")
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp).resolve()
        with linux_container(source, image, work) as container:
            for step in CLI_BUILD_STEPS:
                container.run(Step(step.argv, container.path(source)))
            cli = container.run(Step(("cabal", "list-bin", "exe:simplex-chat"), container.path(source)), capture=True).strip()
            container.run(Step(("cp", cli, container.path(work / "simplex-chat")), container.path(source)))
            container.run(Step(("strip", container.path(work / "simplex-chat")), container.path(source)))
            check_origin_runpaths(container, [work / "simplex-chat"])
            shutil.copy(work / "simplex-chat", out / desktop_files(image)[0])
            if not (source / BUILD_SCRIPT).exists():
                build_desktop_linux_with_tag_scripts(container, source, image, version, work, out)
                return
            shutil.copy(build_cli_deb(container, work / "simplex-chat", version, image.arch, work), out / desktop_files(image)[1])
            libs = build_linux_lib(container, source, image.arch, Database.SQLITE)
            add_vlc_linux(container, libs, image.arch, work)
            check_origin_runpaths(container, [libs])
            link_desktop_libs(source, f"linux-{image.arch}", f"linux-{COMPOSE_ARCHES[image.arch]}")
            deb = package_desktop_deb(container, source, image.arch, ubuntu_codename(container))
            shutil.copy(deb, out / desktop_files(image)[2])
            if image.version == APPIMAGE_IMAGE:
                zip_desktop_libs(libs, [], out / linux_libs_zip(image.arch, Database.SQLITE))
                openssl = fetch_openssl(work)
                build = openssl_build(f"linux-{image.arch}", OPENSSL_UNIX_PREFIX, OPENSSL_UNIX_DIR, install=False)
                for argv in build:
                    container.run(Step(argv, container.path(openssl), {"SOURCE_DATE_EPOCH": SOURCE_DATE_EPOCH}))
                appimage = package_appimage(container, source, image.arch, openssl / "libcrypto.so.3", work)
                shutil.copy(appimage, out / desktop_files(image)[3])


def build_desktop_linux_with_tag_scripts(
    container: Container, source: Path, image: UbuntuImage, version: str, work: Path, out: Path
) -> None:
    """The CLI deb, the desktop deb and the AppImage of a release made before this script, by its own scripts."""
    files = desktop_files(image)
    deb_build = work / "deb-build"
    cli_deb = (TAG_CLI_DEB_SCRIPT, version, container.path(work / "simplex-chat"), container.path(deb_build))
    container.run(Step(cli_deb, container.path(source)))
    shutil.copy(deb_build / "simplex-chat.deb", out / files[1])
    container.run(Step((TAG_DESKTOP_DEB_SCRIPT,), container.path(source), DESKTOP_ENV))
    release = source / DESKTOP_RELEASE_DIR
    shutil.copy(release / f"deb/simplex_{image.arch}.deb", out / files[2])
    if image.version == APPIMAGE_IMAGE:
        container.run(Step((TAG_APPIMAGE_SCRIPT,), container.path(source), DESKTOP_ENV))
        shutil.copy(single_file(release, "*imple*.AppImage"), out / files[3])


@dataclass(frozen=True, slots=True)
class LibsBuilder:
    """Where the library release is built: this machine, a running container, or else a new one of the image."""

    on_host: bool = False
    container: str | None = None


def build_linux_libs(source: Path, image: UbuntuImage, database: Database, dist_dir: str, builder: LibsBuilder, out: Path) -> None:
    """The library release: the desktop libraries without VLC."""
    (source / "cabal.project.local").write_text(DESKTOP_CABAL_LOCAL)
    with contextlib.ExitStack() as stack:
        runner: Runner
        if builder.on_host:
            runner = HostRunner()
        elif builder.container:
            runner = Container.running(builder.container, source)
        else:
            normalize_permissions(source)
            work = Path(stack.enter_context(tempfile.TemporaryDirectory())).resolve()
            runner = stack.enter_context(linux_container(source, image, work))
        runner.run(Step(("cabal", "update"), runner.path(source)))
        libs = build_linux_lib(runner, source, image.arch, database, dist_dir)
        check_origin_runpaths(runner, [libs])
    zip_desktop_libs(libs, [], out / linux_libs_zip(image.arch, database))


def reproduce_desktop(tag: str, repo: str, source: Path, results: Path) -> list[str]:
    built = results / "from-source"
    released = results / "prebuilt"
    built.mkdir(parents=True, exist_ok=True)
    released.mkdir(parents=True, exist_ok=True)
    for image in (image for image in DESKTOP_IMAGES if image.arch == platform.machine()):
        checkout(source, tag)
        build_desktop_linux(source, image, tag_version(tag), built)
        for name in desktop_files(image):
            download(release_url(tag, name, repo), released / name)
    mismatched = compare_dirs(built, released)
    for name in mismatched:
        log(f"{name}: sha256 doesn't match")
    if not mismatched:
        write_sha256sums(released, results / "_sha256sums")
        log(f"checksums: {results / '_sha256sums'}")
    return mismatched
