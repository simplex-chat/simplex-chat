"""The Windows CLI, msi and core libraries, built in an MSYS2 UCRT64 shell, and their comparison with signed releases."""

import array
import contextlib
import dataclasses
import os
import shutil
import struct
import sys
import uuid
import zipfile
from pathlib import Path, PureWindowsPath

from .buildroot import MIN_MACHINE_PATH_PARTS, BuildRoot, check_machine_paths, find_machine_paths, fixed_build_root
from .common import (
    REPO,
    BuildError,
    checkout,
    compare_release,
    download_release,
    edit_lines,
    fetch_pinned,
    log,
    log_group,
    remove_tree,
    run,
    sha256,
    single_dir,
    single_file,
    touch_tree,
)
from .desktop import (
    DESKTOP_ENV,
    DESKTOP_RELEASE_DIR,
    SOURCE_DATE_EPOCH,
    VLC_RELEASE,
    VLC_SHA256,
    WINDOWS_DESKTOP,
    Database,
    build_host_cli,
    check_exports,
    client_library_constraints,
    desktop_libs,
    link_desktop_libs,
    remove_skiko_runtime,
    run_cabal,
    zip_desktop_libs,
)
from .openssl import OpenSsl, check_static_openssl, host_openssl, openssl_work, static_libcrypto

# The system libraries that the static libcrypto of a MinGW build needs (ex_libs of OpenSSL's mingw-common)
OPENSSL_WINDOWS_STATIC_LINK = "-optl-lws2_32 -optl-lgdi32 -optl-lcrypt32"


# the linker writes the time of the link into each executable and DLL unless told not to
LINKER_NO_TIMESTAMP = "-Wl,--no-insert-timestamp"


WINDOWS_NO_TIMESTAMP = f"-optl{LINKER_NO_TIMESTAMP}"


# Compose's packageMsi makes an app image of its own; this makes it pack the one of createDistributable, as on macOS
MSI_FROM_APP_IMAGE_INIT = """gradle.allprojects { project ->
    project.tasks.configureEach { task ->
        if (task.name == "packageMsi") {
            def app = project.tasks.named("createDistributable")
            task.dependsOn(app)
            task.appImage.set(app.flatMap { it.destinationDir.dir(it.packageName) })
        }
    }
}
"""


# WiX gives each msi a random package code, which the build replaces with one derived from the msi's content
MSI_PLACEHOLDER_PACKAGE_CODE = "{00000000-0000-0000-0000-000000000000}"


MSI_PACKAGE_CODE_NAMESPACE = uuid.uuid5(uuid.NAMESPACE_URL, REPO)


# The compound file header and the root storage's directory entry (MS-CFB 2.2, 2.6.1)
CFB_SIGNATURE = bytes.fromhex("d0cf11e0a1b11ae1")


CFB_HEADER_SIZE = 512


CFB_SECTOR_SHIFT_OFFSET = 0x1E


CFB_FIRST_DIRECTORY_SECTOR_OFFSET = 0x30


CFB_MODIFIED_TIME_OFFSET = 0x6C


# FILETIME counts 100 ns intervals from 1601
FILETIME_TICKS_PER_SECOND = 10_000_000


FILETIME_AT_UNIX_EPOCH = 11_644_473_600 * FILETIME_TICKS_PER_SECOND


# Windows Installer's automation interface has no type information for PowerShell, which reaches it with InvokeMember
MSI_SUMMARY_SCRIPT = """param([string]$Msi, [string]$PackageCode, [long]$Epoch)
$ErrorActionPreference = "Stop"
$RevisionNumber, $CreateTime, $LastSaveTime = 9, 12, 13
$time = [DateTimeOffset]::FromUnixTimeSeconds($Epoch).UtcDateTime
$properties = @(@($RevisionNumber, $PackageCode), @($CreateTime, $time), @($LastSaveTime, $time))
$installer = New-Object -ComObject WindowsInstaller.Installer
$summary = $installer.GetType().InvokeMember("SummaryInformation", "GetProperty", $null, $installer, @($Msi, $properties.Count))
foreach ($property in $properties) {
    [void]$summary.GetType().InvokeMember("Property", "SetProperty", $null, $summary, $property)
}
[void]$summary.GetType().InvokeMember("Persist", "InvokeMethod", $null, $summary, $null)
"""


def disable_ghc_lld() -> None:
    """With GHC's lld, linking libsimplex.dll can't find the exported symbols; a missing linker makes GHC use ld."""
    settings = Path(run(["ghc", "--print-libdir"], capture=True).stdout.strip()) / "settings"
    edit_lines(settings, lambda line: line.replace("ld.lld.exe", "abracadabra.exe", 1))


def build_windows_lib(source: Path, openssl: OpenSsl) -> Path:
    """libsimplex.dll and the libcrypto it loads, in the desktop app's libraries directory, which it returns."""
    check_exports(source)
    for old in source.glob("dist-newstyle/build/x86_64-windows/ghc-*/simplex-chat-*"):
        remove_tree(old)
    options = (
        f"-shared -threaded {WINDOWS_NO_TIMESTAMP} -optl-L{openssl.lib.as_posix()} -optl-lcrypto-3-x64 "
        "-o libsimplex.dll libsimplex.dll.def"
    )
    (source / "cabal.project.local").write_text(openssl.cabal_local(options))
    disable_ghc_lld()
    run_cabal(source, ["build", "lib:simplex-chat", *client_library_constraints(Database.SQLITE)])
    libs = desktop_libs(source, WINDOWS_DESKTOP.libs_dir)
    shutil.rmtree(libs, ignore_errors=True)
    shutil.rmtree(source / "apps/multiplatform/desktop/build/cmake", ignore_errors=True)
    libs.mkdir(parents=True)
    shutil.copy(openssl.lib / "libcrypto-3-x64.dll", libs)
    shutil.copy(source / "libsimplex.dll", libs)
    return libs


def add_vlc_windows(libs: Path, work: Path) -> None:
    vlc = libs / "vlc"
    shutil.rmtree(vlc, ignore_errors=True)
    (vlc / "vlc").mkdir(parents=True)
    name = "vlc-win-x86_64.zip"
    extracted = work / "vlc"
    with zipfile.ZipFile(fetch_pinned(f"{VLC_RELEASE}/{name}", VLC_SHA256[name], work / name)) as z:
        z.extractall(extracted)
    root = single_dir(extracted, "vlc-*")
    # the plugin cache must hold the time the plugins get when the app extracts them from its jar
    for plugin in (p for p in (root / "plugins").rglob("*") if ".dll" in p.name):
        os.utime(plugin, (plugin.stat().st_atime, 0))
    run([root / "vlc-cache-gen.exe", "plugins"], cwd=root)
    for dll in root.glob("*.dll"):
        shutil.copy(dll, vlc)
    shutil.copytree(root / "plugins", vlc / "vlc/plugins", dirs_exist_ok=True)


def release_windows_libs(source: Path, openssl: OpenSsl, out: Path) -> Path:
    """The library release, with the import library of libsimplex.dll; returns the libraries directory of the app."""
    libs = build_windows_lib(source, openssl)
    run(["dlltool", "-d", "libsimplex.dll.def", "-l", "libsimplex.lib", "-D", "libsimplex.dll"], cwd=source)
    zip_desktop_libs(libs, [source / "libsimplex.lib"], out / WINDOWS_DESKTOP.libs)
    return libs


# The release builds run in a copy of the checkout here, with cabal's directory and the temporary directory in it:
# cabal hashes the paths of OpenSSL's build in the checkout into the package ids that the binaries contain. Any user
# can make a folder in the root of the system drive.
WINDOWS_BUILD_ROOT = Path("C:/simplex-chat-build")


# what the paths of the machine look like in the files: written by Windows, by MSYS2 or by GHC, in UTF-16 or not
WINDOWS_PATH_ENCODINGS = ("utf-8", "utf-16-le")


def windows_path(path: Path) -> str:
    # MSYS2's Python writes paths with /, which cabal's hackage-security joins to the drive as a relative path: C:dir
    return str(path).replace("/", "\\")


def windows_build_env(root: BuildRoot) -> dict[str, str]:
    tmp = windows_path(root.tmp)
    return {"CABAL_DIR": windows_path(root.directory / "cabal"), "TMP": tmp, "TEMP": tmp}


def windows_build_root(source: Path) -> contextlib.AbstractContextManager[BuildRoot]:
    root = BuildRoot.at(WINDOWS_BUILD_ROOT)
    return fixed_build_root(source, root, windows_build_env(root))


def windows_machine_paths(source: Path) -> list[str]:
    """The checkout, home and temporary directories, which release files built on other machines can't contain, with
    either slash, either case of the drive letter and as MSYS2 writes them, and with short names expanded."""
    paths = [source, Path.home(), *(Path(os.environ[name]) for name in ("TMP", "TEMP") if os.environ.get(name))]
    forms = set()
    for path in {*paths, *(path.resolve() for path in paths)}:
        windows = PureWindowsPath(path)
        drive, rest = windows.drive, windows.as_posix()[len(windows.drive) :]
        for written in (str(windows), windows.as_posix()):
            forms |= {written, written.replace(drive, drive.swapcase(), 1)}
        forms.add(f"/{drive.rstrip(':').lower()}{rest}")
    return sorted(form for form in forms if len(PureWindowsPath(form).parts) >= MIN_MACHINE_PATH_PARTS)


# a file of the checkout that is committed with LF line endings
LINE_ENDINGS_PROBE = "simplex-chat.cabal"


def check_line_endings(source: Path) -> None:
    """Fails unless source has the line endings it is committed with: the binaries contain text of the source, such as the
    SQL of the database migrations."""
    if b"\r\n" in (source / LINE_ENDINGS_PROBE).read_bytes():
        raise BuildError(f"{source} is checked out with CRLF line endings: check it out with git -c core.autocrlf=false")


def build_windows_libs(source: Path, out: Path) -> None:
    check_line_endings(source)
    paths = windows_machine_paths(source)
    with windows_build_root(source) as root:
        libs = release_windows_libs(root.source, host_openssl(openssl_work(root.source)), out)
        found = find_machine_paths([libs], paths, encodings=WINDOWS_PATH_ENCODINGS)
    check_machine_paths(found)


def set_msi_summary(msi: Path, package_code: str, work: Path) -> None:
    """The msi's package code, and its creation and save times at SOURCE_DATE_EPOCH."""
    script = work / "msi-summary.ps1"
    script.write_text(MSI_SUMMARY_SCRIPT)
    msi_path = str(PureWindowsPath(msi))
    run(["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", script, msi_path, package_code, SOURCE_DATE_EPOCH])
    script.unlink()


def msi_package_code(msi: Path) -> str:
    """Windows Installer treats msi files with one package code as copies of one file, so the code is a UUID of the content."""
    return "{" + str(uuid.uuid5(MSI_PACKAGE_CODE_NAMESPACE, sha256(msi))).upper() + "}"


def set_msi_storage_time(msi: Path) -> None:
    """The modification time of the msi's root storage, which each commit sets to its own time, at SOURCE_DATE_EPOCH."""
    with msi.open("r+b") as f:
        header = f.read(CFB_HEADER_SIZE)
        if not header.startswith(CFB_SIGNATURE):
            raise BuildError(f"{msi} is not a compound file")
        sector_shift = struct.unpack_from("<H", header, CFB_SECTOR_SHIFT_OFFSET)[0]
        directory = struct.unpack_from("<I", header, CFB_FIRST_DIRECTORY_SECTOR_OFFSET)[0]
        # sector n starts after the header, which takes up sector -1
        f.seek(((directory + 1) << sector_shift) + CFB_MODIFIED_TIME_OFFSET)
        f.write(struct.pack("<Q", FILETIME_AT_UNIX_EPOCH + int(SOURCE_DATE_EPOCH) * FILETIME_TICKS_PER_SECOND))


def normalize_msi_summary(msi: Path, work: Path) -> None:
    set_msi_summary(msi, MSI_PLACEHOLDER_PACKAGE_CODE, work)
    set_msi_storage_time(msi)
    set_msi_summary(msi, msi_package_code(msi), work)
    set_msi_storage_time(msi)


MEBIBYTE = 1 << 20


# where Compose keeps what jpackage and WiX print, which Gradle doesn't show when they fail
JPACKAGE_MSI_LOGS = "desktop/build/compose/logs/packageMsi"


def print_jpackage_logs(logs: Path) -> None:
    """What jpackage printed, and the free space of the drive that it wrote to."""
    for log_file in sorted(logs.glob("*.txt")):
        with log_group(log_file.name):
            print(log_file.read_text(errors="replace"), flush=True)
    log(f"{shutil.disk_usage(logs).free // MEBIBYTE} MiB free on the drive of {logs}")


def package_msi(source: Path, work: Path) -> Path:
    """jpackage's msi of the app image without the skiko runtime jar and with fixed times, which the msi's cabinet keeps."""
    multiplatform = source / "apps/multiplatform"
    # gradlew.bat, as Windows finds its WSL bash.exe before MSYS2's; a daemon would keep files of the build directory
    # open after the build, which Windows then can't delete
    gradle: list[str | Path] = [multiplatform / "gradlew.bat", "--no-daemon", f"-Psimplex.assets.dir={DESKTOP_ENV['ASSETS_DIR']}"]
    # CMake links libapp-lib.dll with LDFLAGS
    env = {"LDFLAGS": LINKER_NO_TIMESTAMP}
    run([*gradle, "createDistributable"], cwd=multiplatform, env=env)
    app = single_dir(source / DESKTOP_RELEASE_DIR / "app", "*imple*")
    remove_skiko_runtime(app / "app")
    touch_tree(app, SOURCE_DATE_EPOCH)
    init = work / "msi-from-app-image.gradle"
    init.write_text(MSI_FROM_APP_IMAGE_INIT)
    # packageMsi would otherwise create the edited app image again
    try:
        run([*gradle, "--init-script", init, "packageMsi", "-x", "createDistributable"], cwd=multiplatform, env=env)
    except BuildError:
        print_jpackage_logs(multiplatform / JPACKAGE_MSI_LOGS)
        raise
    init.unlink()
    msi = single_file(source / DESKTOP_RELEASE_DIR / "msi", "*imple*.msi")
    normalize_msi_summary(msi, work)
    return msi


def build_desktop_windows(source: Path, out: Path) -> None:
    host = WINDOWS_DESKTOP
    check_line_endings(source)
    paths = windows_machine_paths(source)
    with windows_build_root(source) as root:
        work = root.work
        openssl_dir = openssl_work(root.source)
        openssl = host_openssl(openssl_dir)
        static = OpenSsl(openssl.include, static_libcrypto(openssl.lib, openssl_dir))
        cli = build_host_cli(root.source, static.cabal_local(f"{OPENSSL_WINDOWS_STATIC_LINK} {WINDOWS_NO_TIMESTAMP}"))
        check_static_openssl(cli, run(["objdump", "-p", cli], capture=True).stdout)
        shutil.copy(cli, out / host.cli)
        libs = release_windows_libs(root.source, openssl, out)
        add_vlc_windows(libs, work)
        link_desktop_libs(root.source, host.libs_dir, "windows-x64")
        msi = package_msi(root.source, work)
        shutil.copy(msi, out / host.desktop)
        app = single_dir(root.source / DESKTOP_RELEASE_DIR / "app", "*imple*")
        found = find_machine_paths([out / host.cli, app, msi], paths, encodings=WINDOWS_PATH_ENCODINGS)
    check_machine_paths(found)


PE_DOS_MAGIC = b"MZ"
PE_POINTER_OFFSET = 0x3C
PE_SIGNATURE = b"PE\0\0"
COFF_HEADER_SIZE = 20
PE32_MAGIC = 0x10B
PE32_PLUS_MAGIC = 0x20B
OPTIONAL_HEADER_CHECKSUM = 64

# the data directories of the optional header, of which the fifth is the certificate table (PE format, "Optional Header
# Data Directories")
DATA_DIRECTORIES = {PE32_MAGIC: 96, PE32_PLUS_MAGIC: 112}
CERTIFICATE_TABLE = 4 * 8

CHECKSUM_FOLD = 0xFFFF


PE_DIFFERS = "it differs by more than its Authenticode signature"


@dataclasses.dataclass(frozen=True, slots=True)
class PeHeader:
    """The offsets of the checksum and of the certificate table's directory entry in a PE file."""

    checksum: int
    certificate_table: int


def pe_header(data: bytes) -> PeHeader | None:
    if len(data) < PE_POINTER_OFFSET + 4 or not data.startswith(PE_DOS_MAGIC):
        return None
    (pe,) = struct.unpack_from("<I", data, PE_POINTER_OFFSET)
    optional = pe + len(PE_SIGNATURE) + COFF_HEADER_SIZE
    if data[pe : pe + len(PE_SIGNATURE)] != PE_SIGNATURE or len(data) < optional + 2:
        return None
    (magic,) = struct.unpack_from("<H", data, optional)
    if magic not in DATA_DIRECTORIES:
        return None
    return PeHeader(optional + OPTIONAL_HEADER_CHECKSUM, optional + DATA_DIRECTORIES[magic] + CERTIFICATE_TABLE)


def pe_checksum(data: bytes, checksum: int) -> int:
    """The checksum of the PE file as CheckSumMappedFile computes it: its 16-bit words, without the checksum field,
    added with end-around carry, plus its length."""
    padded = bytearray(data)
    padded[checksum : checksum + 4] = bytes(4)
    if len(padded) % 2:
        padded.append(0)
    words = array.array("H", padded)
    if sys.byteorder == "big":
        words.byteswap()
    total = sum(words)
    while total > CHECKSUM_FOLD:
        total = (total & CHECKSUM_FOLD) + (total >> 16)
    return total + len(data)


def copy_authenticode(released: bytes, built: bytes) -> bytes:
    """built with released's certificate table appended as signtool appends it, and the directory entry and checksum
    that it updates; built as it is when released isn't a signed PE file or built isn't an unsigned one."""
    theirs, ours = pe_header(released), pe_header(built)
    if theirs is None or ours is None:
        return built
    offset, size = struct.unpack_from("<II", released, theirs.certificate_table)
    if size == 0 or struct.unpack_from("<II", built, ours.certificate_table) != (0, 0):
        return built
    out = bytearray(built) + bytes(max(0, offset - len(built)))
    struct.pack_into("<II", out, ours.certificate_table, offset, size)
    out += released[offset : offset + size]
    struct.pack_into("<I", out, ours.checksum, pe_checksum(bytes(out), ours.checksum))
    return bytes(out)


def same_pe(released: bytes, built: bytes) -> bool:
    """Whether released is built, or built with released's Authenticode signature."""
    if released == built:
        return True
    try:
        return copy_authenticode(released, built) == released
    except struct.error:
        return False


# The compound file binary format (MS-CFB): the header's fields, the directory entries and the special sector numbers
CFB_HEADER = "<8s16sHHHHH6sIIIIIIIII"
CFB_HEADER_DIFAT = 0x4C
CFB_HEADER_DIFAT_COUNT = 109
CFB_DIRECTORY_ENTRY = "<64sHBBIII16sIQQIQ"
CFB_DIRECTORY_ENTRY_SIZE = 128
CFB_NO_STREAM = 0xFFFFFFFF
CFB_END_OF_CHAIN = 0xFFFFFFFE
CFB_STORAGE = 1
CFB_STREAM = 2
CFB_ROOT = 5

# The streams that signtool adds to an msi: the signature, and the hash of the storages' metadata it may cover
MSI_SIGNATURE_STREAMS = ("\x05DigitalSignature", "\x05MsiDigitalSignatureEx")


@dataclasses.dataclass(frozen=True, slots=True)
class CompoundEntry:
    """A storage or stream of a compound file, with what its directory entry records and a stream's data."""

    kind: int
    clsid: bytes
    state: int
    created: int
    modified: int
    data: bytes


class CompoundFileError(Exception):
    pass


def compound_entries(data: bytes) -> dict[str, CompoundEntry]:
    """The storages and streams of a compound file by their paths, "/" for the root storage."""
    if not data.startswith(CFB_SIGNATURE):
        raise CompoundFileError("not a compound file")
    header = struct.unpack_from(CFB_HEADER, data)
    sector_shift, mini_shift = header[5], header[6]
    first_directory, cutoff, first_mini_fat, first_difat, difat_sectors = header[10], header[12], header[13], header[15], header[16]
    sector_size = 1 << sector_shift

    def sector(number: int) -> bytes:
        start = (number + 1) << sector_shift
        if start >= len(data):
            raise CompoundFileError(f"sector {number} is past the end of the file")
        return data[start : start + sector_size]

    def numbers(chunk: bytes) -> list[int]:
        return list(struct.unpack(f"<{len(chunk) // 4}I", chunk))

    fat_sectors = numbers(data[CFB_HEADER_DIFAT : CFB_HEADER_DIFAT + 4 * CFB_HEADER_DIFAT_COUNT])
    difat = first_difat
    for _ in range(difat_sectors):
        listed = numbers(sector(difat))
        fat_sectors += listed[:-1]
        difat = listed[-1]
    fat = [entry for number in fat_sectors if number < CFB_END_OF_CHAIN for entry in numbers(sector(number))]

    def chain(start: int, table: list[int]) -> list[int]:
        found: list[int] = []
        while start < CFB_END_OF_CHAIN:
            if start >= len(table) or len(found) > len(table):
                raise CompoundFileError(f"broken sector chain at {start}")
            found.append(start)
            start = table[start]
        return found

    def read(start: int) -> bytes:
        return b"".join(sector(number) for number in chain(start, fat))

    directory = read(first_directory)
    records = [struct.unpack_from(CFB_DIRECTORY_ENTRY, directory, i) for i in range(0, len(directory), CFB_DIRECTORY_ENTRY_SIZE)]
    root = records[0]
    mini_stream = read(root[11])[: root[12]]
    mini_fat = numbers(read(first_mini_fat)) if first_mini_fat < CFB_END_OF_CHAIN else []
    mini_size = 1 << mini_shift

    def stream(record: tuple[int, ...]) -> bytes:
        start, size = record[11], record[12]
        if size < cutoff:
            return b"".join(mini_stream[n * mini_size : (n + 1) * mini_size] for n in chain(start, mini_fat))[:size]
        return read(start)[:size]

    entries: dict[str, CompoundEntry] = {}

    def visit(number: int, parent: str) -> None:
        if number == CFB_NO_STREAM:
            return
        name_bytes, name_length, kind, _, left, right, child, clsid, state, created, modified, _, _ = records[number]
        name = name_bytes[: max(0, name_length - 2)].decode("utf-16-le")
        path = f"{parent}{name}"
        if path in entries:
            raise CompoundFileError(f"{path} is listed twice")
        entries[path] = CompoundEntry(kind, clsid, state, created, modified, stream(records[number]) if kind == CFB_STREAM else b"")
        visit(left, parent)
        visit(right, parent)
        if kind == CFB_STORAGE:
            visit(child, f"{path}/")

    entries["/"] = CompoundEntry(CFB_ROOT, root[7], root[8], root[9], root[10], b"")
    visit(root[6], "/")
    return entries


def msi_differences(released: bytes, built: bytes) -> list[str]:
    """The storages and streams of the released msi, other than its signature and the time signtool saved it at,
    that the built one doesn't have as they are, and the ones that only the built one has."""
    try:
        theirs, ours = compound_entries(released), compound_entries(built)
    except (CompoundFileError, struct.error, UnicodeDecodeError) as e:
        return [f"not a readable msi: {e}"]
    signed = [theirs.pop(f"/{name}") for name in MSI_SIGNATURE_STREAMS if f"/{name}" in theirs]
    if signed and "/" in ours:
        # signtool saves the root storage with the time it signs at
        theirs["/"] = dataclasses.replace(theirs["/"], modified=ours["/"].modified)
    return sorted(path for path in theirs.keys() | ours.keys() if theirs.get(path) != ours.get(path))


def windows_differences(released: Path, built: Path) -> list[str]:
    """What differs other than the Authenticode signature: the msi's streams and storages, or the PE file's bytes."""
    data, built_data = released.read_bytes(), built.read_bytes()
    if data.startswith(CFB_SIGNATURE):
        return [f"{path} differs" for path in msi_differences(data, built_data)]
    return [] if same_pe(data, built_data) else [PE_DIFFERS]


def reproduce_windows(tag: str, repo: str, source: Path, results: Path) -> list[str]:
    out = results / "windows"
    names = (WINDOWS_DESKTOP.cli, WINDOWS_DESKTOP.desktop)
    download_release(tag, repo, names, out / "prebuilt")
    checkout(source, tag)
    (out / "from-source").mkdir(exist_ok=True)
    build_desktop_windows(source, out / "from-source")
    return compare_release(names, out, windows_differences)
