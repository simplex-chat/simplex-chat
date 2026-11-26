import pathlib
import struct
import tempfile
import unittest
import zipfile
from unittest import mock

from .testing import Recorder, build_script, patch_all

# 2024-01-01 00:00 UTC in 100 ns intervals from 1601
EPOCH_FILETIME = 133_485_408_000_000_000


def compound_file(summary, modified):
    """A compound file with 512-byte sectors, its directory in sector 1 and summary as the data that follows it."""
    header = bytearray(512)
    header[:8] = bytes.fromhex("d0cf11e0a1b11ae1")
    struct.pack_into("<H", header, 0x1E, 9)
    struct.pack_into("<I", header, 0x30, 1)
    root = bytearray(128)
    root[:20] = "Root Entry".encode("utf-16-le")
    struct.pack_into("<Q", root, 0x6C, modified)
    return bytes(header) + bytes(512) + bytes(root) + bytes(384) + summary


class WindowsLibTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.source = self.root / "src"
        (self.source / "src/Simplex/Chat").mkdir(parents=True)
        (self.source / "src/Simplex/Chat/Mobile.hs").write_text('foreign export ccall "chat_send_cmd" c :: X\n')
        (self.source / "libsimplex.dll.def").write_text("chat_send_cmd\n")
        (self.source / "flake.nix").write_text('"chat_send_cmd"\n')

    def tearDown(self):
        self.tmp.cleanup()

    def test_ghc_settings_stop_using_lld(self):
        libdir = self.root / "ghc-lib"
        libdir.mkdir()
        (libdir / "settings").write_text('[("ld command", "ld.lld.exe"), ("x", "ld.lld.exe")]\n')
        with patch_all("run", Recorder(answers={"print-libdir": f"{libdir}\n"})):
            build_script.disable_ghc_lld()
        self.assertEqual((libdir / "settings").read_text(), '[("ld command", "abracadabra.exe"), ("x", "ld.lld.exe")]\n')

    def test_library_links_the_pinned_openssl(self):
        openssl = self.root / "openssl-3.0.15"
        openssl.mkdir()
        (openssl / "libcrypto-3-x64.dll").write_bytes(b"crypto")
        (self.source / "libsimplex.dll").write_bytes(b"simplex")
        recorder = Recorder()
        with patch_all("run", recorder), patch_all("disable_ghc_lld") as disable:
            libs = build_script.build_windows_lib(self.source, build_script.OpenSsl(openssl / "include", openssl))
        disable.assert_called_once()
        cabal_local = (self.source / "cabal.project.local").read_text()
        self.assertIn(f"extra-include-dirs: {openssl.as_posix()}/include", cabal_local)
        options = (
            f"-shared -threaded -optl-Wl,--no-insert-timestamp -optl-L{openssl.as_posix()} -optl-lcrypto-3-x64 "
            "-o libsimplex.dll libsimplex.dll.def"
        )
        self.assertIn(f"ghc-options: {options}", cabal_local)
        self.assertIn("simplex-chat +client_library", recorder.commands[-1])
        self.assertEqual(sorted(p.name for p in libs.iterdir()), ["libcrypto-3-x64.dll", "libsimplex.dll"])

    def test_vlc_plugins_get_the_jar_time_before_the_cache(self):
        libs, work = self.root / "libs", self.root / "work"
        libs.mkdir()
        work.mkdir()
        archive = work / "vlc-win-x86_64.zip"
        with zipfile.ZipFile(archive, "w") as z:
            z.writestr("vlc-3.0.23/libvlc.dll", b"vlc")
            z.writestr("vlc-3.0.23/plugins/audio/libplugin.dll", b"p")
        times = []

        def cache_gen(args, **kwargs):
            times.append((work / "vlc/vlc-3.0.23/plugins/audio/libplugin.dll").stat().st_mtime)
            return mock.Mock(returncode=0, stdout="")

        with patch_all("run", cache_gen), patch_all("fetch_pinned", lambda u, d, t: t):
            build_script.add_vlc_windows(libs, work)
        self.assertEqual(times, [0])
        self.assertTrue((libs / "vlc/libvlc.dll").exists())
        self.assertEqual((libs / "vlc/vlc/plugins/audio/libplugin.dll").stat().st_mtime, 0)


class MsiTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.source = self.root / "src"
        self.work = self.root / "work"
        self.work.mkdir()
        self.msi = self.root / "SimpleX-7.1.7.msi"
        self.msi.write_bytes(b"msi")

    def tearDown(self):
        self.tmp.cleanup()

    def persisting_summary(self, recorder, scripts):
        """Stands in for run(): powershell writes the package code it is given into the msi, as Persist does."""

        def fake_run(args, **kwargs):
            args = [str(a) for a in args]
            if args[0] == "powershell":
                scripts.append(pathlib.Path(args[args.index("-File") + 1]).read_text())
                self.msi.write_bytes(b"msi " + args[-2].encode())
            return recorder(args, **kwargs)

        return fake_run

    def test_summary_script_gets_the_msi_package_code_and_epoch(self):
        recorder, scripts = Recorder(), []
        with patch_all("run", self.persisting_summary(recorder, scripts)):
            build_script.set_msi_summary(self.msi, "{A}", self.work)
        script = str(self.work / "msi-summary.ps1")
        msi = str(pathlib.PureWindowsPath(self.msi))
        self.assertEqual(
            recorder.commands,
            [["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", script, msi, "{A}", "1704067200"]],
        )
        self.assertEqual(scripts, [build_script.MSI_SUMMARY_SCRIPT])
        self.assertIn("$RevisionNumber, $CreateTime, $LastSaveTime = 9, 12, 13", scripts[0])
        self.assertEqual(list(self.work.iterdir()), [])

    def test_package_code_is_a_uuid_of_the_content(self):
        code = build_script.msi_package_code(self.msi)
        self.assertRegex(code, r"^\{[0-9A-F]{8}-[0-9A-F]{4}-5[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}\}$")
        self.assertEqual(build_script.msi_package_code(self.msi), code)
        self.msi.write_bytes(b"msi 2")
        self.assertNotEqual(build_script.msi_package_code(self.msi), code)

    def test_package_code_is_derived_from_the_msi_with_the_placeholder_and_fixed_time(self):
        commit_times = iter([1_000, 2_000])

        def persist(args, **kwargs):
            if args[0] == "powershell":
                self.msi.write_bytes(compound_file(str(args[-2]).encode(), next(commit_times)))
            return mock.Mock(returncode=0, stdout="")

        with patch_all("run", persist):
            build_script.normalize_msi_summary(self.msi, self.work)
        normalized = self.msi.read_bytes()
        self.msi.write_bytes(compound_file(build_script.MSI_PLACEHOLDER_PACKAGE_CODE.encode(), EPOCH_FILETIME))
        expected = build_script.msi_package_code(self.msi)
        self.assertEqual(normalized, compound_file(expected.encode(), EPOCH_FILETIME))
        self.assertIsNone(next(commit_times, None))

    def test_storage_time_is_the_epoch_and_nothing_else_changes(self):
        self.msi.write_bytes(compound_file(b"{CODE}", 1_000))
        build_script.set_msi_storage_time(self.msi)
        self.assertEqual(self.msi.read_bytes(), compound_file(b"{CODE}", EPOCH_FILETIME))

    def test_storage_time_of_a_file_that_is_no_msi_fails(self):
        with self.assertRaises(build_script.BuildError):
            build_script.set_msi_storage_time(self.msi)
        self.assertEqual(self.msi.read_bytes(), b"msi")

    def test_msi_packs_the_app_image_without_skiko_runtime_at_the_epoch(self):
        image = self.source / build_script.DESKTOP_RELEASE_DIR / "app/SimpleX"
        release_msi = self.source / build_script.DESKTOP_RELEASE_DIR / "msi/SimpleX-7.1.7.msi"
        recorder = Recorder()
        seen = {}

        def fake_run(args, **kwargs):
            args = [str(a) for a in args]
            if args[-1] == "createDistributable" and "-x" not in args:
                (image / "app").mkdir(parents=True)
                (image / "SimpleX.exe").write_bytes(b"exe")
                (image / "app/desktop-jvm-7.1.7-854b.jar").write_bytes(b"jar")
                (image / "app/skiko-awt-runtime-windows-x64-0.9.4.2-1f63.jar").write_bytes(b"jar")
                (image / "app/SimpleX.cfg").write_text("app.classpath=$APPDIR\\skiko-awt-runtime-windows-x64-0.9.4.2-1f63.jar\n")
            if "packageMsi" in args:
                files = list(image.rglob("*"))
                seen["names"] = sorted(p.relative_to(image).as_posix() for p in files)
                seen["times"] = {p.stat().st_mtime for p in [image, *files]}
                seen["init"] = pathlib.Path(args[args.index("--init-script") + 1]).read_text()
                release_msi.parent.mkdir(parents=True)
                release_msi.write_bytes(b"msi")
            return recorder(args, **kwargs)

        with patch_all("run", fake_run), patch_all("normalize_msi_summary") as normalize:
            msi = build_script.package_msi(self.source, self.work)
        self.assertEqual(msi, release_msi)
        normalize.assert_called_once_with(release_msi, self.work)
        gradle = [str(self.source / "apps/multiplatform/gradlew.bat"), "--no-daemon", "-Psimplex.assets.dir=../../assets"]
        init = str(self.work / "msi-from-app-image.gradle")
        self.assertEqual(
            recorder.commands,
            [[*gradle, "createDistributable"], [*gradle, "--init-script", init, "packageMsi", "-x", "createDistributable"]],
        )
        self.assertEqual(recorder.envs, [{"LDFLAGS": "-Wl,--no-insert-timestamp"}] * 2)
        self.assertEqual(seen["init"], build_script.MSI_FROM_APP_IMAGE_INIT)
        self.assertIn('task.appImage.set(app.flatMap { it.destinationDir.dir(it.packageName) })', seen["init"])
        self.assertFalse((self.work / "msi-from-app-image.gradle").exists())
        self.assertEqual(seen["names"], ["SimpleX.exe", "app", "app/SimpleX.cfg", "app/desktop-jvm-7.1.7-854b.jar"])
        self.assertEqual(seen["times"], {1704067200})
        self.assertEqual((image / "app/SimpleX.cfg").read_text(), "")

    def test_failed_msi_prints_what_jpackage_and_wix_printed(self):
        logs = self.source / "apps/multiplatform" / build_script.JPACKAGE_MSI_LOGS
        logs.mkdir(parents=True)
        (logs / "jpackage-out.txt").write_text("Running light.exe\n")
        (logs / "jpackage-err.txt").write_text("light.exe : error LGHT0217 : Error executing ICE action 'ICE01'\n")
        image = self.source / build_script.DESKTOP_RELEASE_DIR / "app/SimpleX"
        (image / "app").mkdir(parents=True)
        (image / "app/SimpleX.cfg").write_text("")
        recorder = Recorder(fail_once=["packageMsi"])
        with (
            patch_all("run", recorder),
            mock.patch("builtins.print") as printed,
            self.assertRaises(build_script.BuildError),
        ):
            build_script.package_msi(self.source, self.work)
        output = [call.args[0] for call in printed.call_args_list if call.args]
        self.assertIn("light.exe : error LGHT0217 : Error executing ICE action 'ICE01'\n", output)
        self.assertIn("Running light.exe\n", output)
        self.assertTrue(any(f"MiB free on the drive of {logs}" in line for line in output), output)


SECTOR = 512
FREE, END, FAT_SECTOR = 0xFFFFFFFF, 0xFFFFFFFE, 0xFFFFFFFD
MSI_CLSID = bytes.fromhex("84100c0000000000c000000000000046")


def msi_file(streams, root_modified=0):
    """A version 3 compound file of streams in its root storage, the ones under 4096 bytes in its mini stream, as an
    msi of Windows Installer is; the root's directory entry lists the streams by their right siblings."""
    small = {name: data for name, data in streams.items() if len(data) < 4096}
    mini_stream, mini_fat, mini_starts = b"", [], {}
    for name, data in small.items():
        start, count = len(mini_stream) // 64, -(-len(data) // 64)
        mini_starts[name] = start
        mini_stream += data + bytes(-len(data) % 64)
        mini_fat += [start + i + 1 for i in range(count - 1)] + [END]
    sectors, fat = [], [FAT_SECTOR]

    def allocate(data):
        start = 1 + len(sectors)
        count = max(1, -(-len(data) // SECTOR))
        for i in range(count):
            sectors.append(data[i * SECTOR : (i + 1) * SECTOR].ljust(SECTOR, b"\0"))
            fat.append(start + i + 1 if i < count - 1 else END)
        return start

    entries = [("Root Entry", 5, MSI_CLSID, root_modified)] + [(name, 2, bytes(16), 0) for name in streams]
    directory_size = -(-len(entries) // 4) * SECTOR
    directory = allocate(bytes(directory_size))
    mini_fat_start = allocate(struct.pack(f"<{len(mini_fat)}I", *mini_fat)) if mini_fat else END
    mini_stream_start = allocate(mini_stream) if mini_stream else END
    starts = {name: mini_starts[name] if name in small else allocate(data) for name, data in streams.items()}
    records = b""
    for i, (name, kind, clsid, modified) in enumerate(entries):
        encoded = name.encode("utf-16-le") + b"\0\0"
        right = i + 1 if 0 < i < len(entries) - 1 else FREE
        child = 1 if i == 0 and len(entries) > 1 else FREE
        start, size = (mini_stream_start, len(mini_stream)) if i == 0 else (starts[name], len(streams[name]))
        records += struct.pack("<64sHBBIII16sIQQIQ", encoded, len(encoded), kind, 1, FREE, right, child, clsid, 0, 0, modified, start, size)
    records = records.ljust(directory_size, b"\0")
    for i in range(directory_size // SECTOR):
        sectors[directory - 1 + i] = records[i * SECTOR : (i + 1) * SECTOR]
    mini_fat_sectors = -(-len(mini_fat) * 4 // SECTOR)
    header = struct.pack(
        "<8s16sHHHHH6sIIIIIIIII",
        *(bytes.fromhex("d0cf11e0a1b11ae1"), bytes(16), 0x3E, 3, 0xFFFE, 9, 6, bytes(6)),
        *(0, 1, directory, 0, 4096, mini_fat_start, mini_fat_sectors, END, 0),
    )
    difat = struct.pack("<109I", 0, *[FREE] * 108)
    fat_sector = struct.pack(f"<{SECTOR // 4}I", *(fat + [FREE] * (SECTOR // 4 - len(fat))))
    return header + difat + fat_sector + b"".join(sectors)


OPTIONAL_HEADER = 0x80 + 24


def pe_file(body=b"code" * 300):
    """A PE32+ file as MinGW's linker leaves it: no checksum and no certificate table."""
    data = bytearray(OPTIONAL_HEADER + 240)
    data[:2] = b"MZ"
    struct.pack_into("<I", data, 0x3C, 0x80)
    data[0x80:0x84] = b"PE\0\0"
    struct.pack_into("<H", data, OPTIONAL_HEADER, 0x20B)
    return bytes(data) + body


def reference_checksum(data, at):
    """CheckSumMappedFile's checksum, one word at a time."""
    total = 0
    padded = data + b"\0" * (len(data) % 2)
    for offset in range(0, len(padded), 2):
        total += 0 if at <= offset < at + 4 else struct.unpack_from("<H", padded, offset)[0]
        total = (total & 0xFFFF) + (total >> 16)
    return total + len(data)


def signtool(pe, certificate):
    """pe signed as signtool signs it: padded to 8 bytes, with a WIN_CERTIFICATE after it and the checksum updated."""
    data = bytearray(pe) + bytes(-len(pe) % 8)
    table = struct.pack("<IHH", 8 + len(certificate), 0x200, 2) + certificate
    table += bytes(-len(table) % 8)
    struct.pack_into("<II", data, OPTIONAL_HEADER + 112 + 32, len(data), len(table))
    data += table
    struct.pack_into("<I", data, OPTIONAL_HEADER + 64, reference_checksum(bytes(data), OPTIONAL_HEADER + 64))
    return bytes(data)


class AuthenticodeTest(unittest.TestCase):
    def test_checksum_is_the_one_of_check_sum_mapped_file(self):
        at = OPTIONAL_HEADER + 64
        for data in (pe_file(), pe_file(body=bytes(range(256)) * 9 + b"odd"), pe_file(body=b"\xff" * 4001)):
            self.assertEqual(build_script.pe_checksum(data, at), reference_checksum(data, at))

    def test_signature_of_a_release_is_copied_onto_the_build(self):
        for built in (pe_file(), pe_file(body=b"code" * 300 + b"odd")):
            released = signtool(built, b"pkcs7 signed data and timestamp")
            self.assertEqual(build_script.copy_authenticode(released, built), released)
            self.assertTrue(build_script.same_pe(released, built))

    def test_other_code_or_a_signed_build_does_not_match(self):
        released = signtool(pe_file(), b"pkcs7")
        self.assertFalse(build_script.same_pe(released, pe_file(body=b"code" * 299 + b"edoc")))
        self.assertFalse(build_script.same_pe(released, released[:200]))
        self.assertFalse(build_script.same_pe(released, signtool(pe_file(), b"pkcs7 of another key")))

    def test_unsigned_release_and_other_files_are_compared_as_they_are(self):
        self.assertTrue(build_script.same_pe(pe_file(), pe_file()))
        self.assertFalse(build_script.same_pe(pe_file(), pe_file(body=b"other")))
        self.assertIsNone(build_script.pe_header(b"MZ" + bytes(100)))
        self.assertIsNone(build_script.pe_header(b"text"))


class MsiComparisonTest(unittest.TestCase):
    STREAMS = {"䡀㼿": b"summary" * 10, "䡀䵎": b"t" * 5000, "Binary.icon": b"i" * 4096}

    def signed(self, streams):
        signature, digest = build_script.MSI_SIGNATURE_STREAMS
        return msi_file({**streams, signature: b"pkcs7", digest: b"hash"}, root_modified=134379596991710000)

    def test_streams_of_a_compound_file_are_read_from_sectors_and_the_mini_stream(self):
        entries = build_script.compound_entries(msi_file(self.STREAMS, root_modified=7))
        self.assertEqual((entries["/"].clsid, entries["/"].modified), (MSI_CLSID, 7))
        streams = {path: entry.data for path, entry in entries.items() if path != "/"}
        self.assertEqual(streams, {f"/{name}": data for name, data in self.STREAMS.items()})

    def test_signature_streams_are_the_only_allowed_difference(self):
        self.assertEqual(build_script.msi_differences(self.signed(self.STREAMS), msi_file(self.STREAMS)), [])

    def test_changed_added_or_missing_streams_and_storage_metadata_differ(self):
        built = msi_file(self.STREAMS)
        self.assertEqual(build_script.msi_differences(self.signed({**self.STREAMS, "Binary.icon": b"j" * 4096}), built), ["/Binary.icon"])
        added = msi_file({**self.STREAMS, "Binary.extra": b"x"})
        self.assertEqual(build_script.msi_differences(added, built), ["/Binary.extra"])
        self.assertEqual(build_script.msi_differences(built, added), ["/Binary.extra"])
        self.assertEqual(build_script.msi_differences(msi_file(self.STREAMS, root_modified=1), built), ["/"])

    def test_files_that_are_no_compound_files_differ(self):
        built = msi_file(self.STREAMS)
        self.assertEqual(build_script.msi_differences(b"MZ", built), ["not a readable msi: not a compound file"])
        self.assertTrue(build_script.msi_differences(built[:1100], built)[0].startswith("not a readable msi"))

    def test_release_files_are_compared_by_their_format(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            files = {
                "signed.msi": self.signed(self.STREAMS),
                "built.msi": msi_file(self.STREAMS),
                "other.msi": msi_file({**self.STREAMS, "Binary.icon": b"x"}),
                "signed.exe": signtool(pe_file(), b"pkcs7"),
                "built.exe": pe_file(),
            }
            for name, data in files.items():
                (root / name).write_bytes(data)
            self.assertEqual(build_script.windows_differences(root / "signed.msi", root / "built.msi"), [])
            self.assertEqual(build_script.windows_differences(root / "other.msi", root / "built.msi"), ["/Binary.icon differs"])
            self.assertEqual(build_script.windows_differences(root / "signed.exe", root / "built.exe"), [])
            self.assertEqual(build_script.windows_differences(root / "signed.exe", root / "built.msi"), [build_script.PE_DIFFERS])

    def test_reproduce_downloads_builds_and_compares_the_cli_and_msi(self):
        with tempfile.TemporaryDirectory() as tmp:
            results = pathlib.Path(tmp) / "results"
            urls = []

            def fake_download(url, target):
                urls.append(url)
                target.write_bytes(signtool(pe_file(), b"pkcs7") if target.suffix != ".msi" else self.signed(self.STREAMS))

            def fake_build(source, out):
                (out / "simplex-chat-windows-x86-64").write_bytes(pe_file())
                (out / "simplex-desktop-windows-x86_64.msi").write_bytes(msi_file({**self.STREAMS, "Binary.icon": b"x"}))

            with patch_all("download", fake_download), patch_all("checkout") as checkout, patch_all("build_desktop_windows", fake_build):
                failed = build_script.reproduce_windows("v7.1.0", "owner/fork", pathlib.Path(tmp), results)
            self.assertEqual(failed, ["simplex-desktop-windows-x86_64.msi"])
            checkout.assert_called_once_with(pathlib.Path(tmp), "v7.1.0")
            self.assertEqual(urls[0], "https://github.com/owner/fork/releases/download/v7.1.0/simplex-chat-windows-x86-64")
            self.assertFalse((results / "windows/_sha256sums").exists())


class WindowsMachinePathsTest(unittest.TestCase):
    def test_the_checkout_home_and_temporary_directories_in_each_form(self):
        temp = "C:\\Users\\RUNNER~1\\AppData\\Local\\Temp"
        home = pathlib.Path("C:\\Users\\runneradmin")
        with (
            mock.patch.dict(build_script.os.environ, {"TMP": temp, "TEMP": temp}),
            mock.patch.object(build_script.Path, "home", return_value=home),
        ):
            paths = build_script.windows_machine_paths(pathlib.Path("D:/a/simplex-chat/simplex-chat"))
        expected = {
            "D:\\a\\simplex-chat\\simplex-chat",
            "D:/a/simplex-chat/simplex-chat",
            "d:/a/simplex-chat/simplex-chat",
            "d:\\a\\simplex-chat\\simplex-chat",
            "/d/a/simplex-chat/simplex-chat",
            "C:\\Users\\runneradmin",
            "c:/Users/runneradmin",
            "/c/Users/runneradmin",
            "C:\\Users\\RUNNER~1\\AppData\\Local\\Temp",
            "C:/Users/RUNNER~1/AppData/Local/Temp",
        }
        self.assertLessEqual(expected, set(paths))
        self.assertTrue(all(len(build_script.PureWindowsPath(path).parts) >= 3 for path in paths))


class LineEndingsTest(unittest.TestCase):
    def test_a_checkout_with_crlf_line_endings_is_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = pathlib.Path(tmp)
            (source / "simplex-chat.cabal").write_bytes(b"name: simplex-chat\nversion: 7.1.0\n")
            build_script.check_line_endings(source)
            (source / "simplex-chat.cabal").write_bytes(b"name: simplex-chat\r\nversion: 7.1.0\r\n")
            with self.assertRaisesRegex(build_script.BuildError, "core.autocrlf=false"):
                build_script.check_line_endings(source)
            with patch_all("windows_build_root") as root, self.assertRaises(build_script.BuildError):
                build_script.build_desktop_windows(source, source / "out")
            root.assert_not_called()


class WindowsLibsReleaseTest(unittest.TestCase):
    def build(self, library):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            source, fixed = root / "checkout", root / "fixed"
            source.mkdir()
            (source / "simplex-chat.cabal").write_bytes(b"name: simplex-chat\n")
            sources = []

            def fake_release(copy, openssl, out):
                sources.append(copy)
                libs = copy / "libs"
                libs.mkdir()
                (libs / "libsimplex.dll").write_bytes(library(source))
                return libs

            with (
                patch_all("WINDOWS_BUILD_ROOT", fixed),
                patch_all("host_openssl"),
                patch_all("release_windows_libs", fake_release),
            ):
                try:
                    build_script.build_windows_libs(source, root / "out")
                finally:
                    self.assertEqual(sources, [fixed / "simplex-chat"])
                    self.assertFalse(fixed.exists())

    def test_libraries_are_built_in_the_fixed_directory(self):
        self.build(lambda source: b"code")

    def test_libraries_with_a_path_of_the_checkout_fail(self):
        with self.assertRaisesRegex(build_script.BuildError, "libsimplex.dll: "):
            self.build(lambda source: str(build_script.PureWindowsPath(source)).encode("utf-16-le"))


class WindowsBuildEnvTest(unittest.TestCase):
    def test_the_build_environment_has_absolute_windows_paths(self):
        # as MSYS2's Python writes the paths of the fixed directory
        root = build_script.BuildRoot.at(pathlib.PurePosixPath("C:/simplex-chat-build"))
        env = build_script.windows_build_env(root)
        self.assertEqual(
            env,
            {"CABAL_DIR": "C:\\simplex-chat-build\\cabal", "TMP": "C:\\simplex-chat-build\\tmp", "TEMP": "C:\\simplex-chat-build\\tmp"},
        )
        for value in env.values():
            self.assertTrue(build_script.PureWindowsPath(value).is_absolute(), value)
            self.assertNotIn("/", value)

    def test_the_fixed_directory_is_absolute_on_the_system_drive(self):
        root = build_script.PureWindowsPath(build_script.WINDOWS_BUILD_ROOT)
        self.assertTrue(root.is_absolute())
        self.assertEqual(root.drive, "C:")
        self.assertTrue(build_script.windows_path(build_script.WINDOWS_BUILD_ROOT).startswith("C:\\"))
