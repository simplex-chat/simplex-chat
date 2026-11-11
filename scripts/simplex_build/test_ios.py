import importlib.util
import os
import pathlib
import plistlib
import shutil
import tempfile
import unittest
import zipfile
from unittest import mock

from .testing import (
    ENTITLEMENTS,
    OTOOL,
    PBXPROJ,
    REPO_ROOT,
    XCODE_DEVELOPER,
    FakeApple,
    Recorder,
    build_script,
    patch_all,
)

COMMIT_EPOCH = 1700000000


def load_zero_ar_headers():
    """scripts/nix/zero-ar-headers.py, which flake.nix runs on the iOS core libraries."""
    spec = importlib.util.spec_from_file_location("zero_ar_headers", REPO_ROOT / "scripts/nix/zero-ar-headers.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


zero_ar_headers = load_zero_ar_headers()


def ar_member(name, data, date=b"1761912000", uid=b"351", gid=b"30000", mode=b"100444"):
    """A BSD ar member, its name after the header as Apple's ar writes names longer than 16 bytes."""
    name_field = f"#1/{len(name)}".encode() if len(name) > 16 else name.encode()
    body = (name.encode() if len(name) > 16 else b"") + data
    header = name_field.ljust(16) + date.ljust(12) + uid.ljust(6) + gid.ljust(6) + mode.ljust(8) + str(len(body)).encode().ljust(10)
    return header + b"`\n" + body + b"\n" * (len(body) % 2)


def ar_archive(*members):
    return b"!<arch>\n" + b"".join(members)


class IosDetailsTest(unittest.TestCase):
    def test_minimum_versions_reads_both_load_commands(self):
        self.assertEqual(build_script.minimum_versions(OTOOL), ["11.0", "14.2"])
        self.assertEqual(build_script.minimum_versions("Load command 1\n      cmd LC_SYMTAB\n"), [])

    def test_versions_compare_numerically(self):
        self.assertGreater(build_script.version_tuple("15.10"), build_script.version_tuple("15.9"))
        self.assertEqual(max(["9.0", "15.0", "11.0"], key=build_script.version_tuple), "15.0")

    def test_build_settings_take_the_first_value(self):
        output = (
            "Build settings for target X:\n"
            "    DEVELOPMENT_TEAM = 5NN7GUYB6T\n    IPHONEOS_DEPLOYMENT_TARGET = 15.0\n    DEVELOPMENT_TEAM = OTHER\n"
        )
        settings = build_script.build_settings(output, ["DEVELOPMENT_TEAM", "IPHONEOS_DEPLOYMENT_TARGET"])
        self.assertEqual(settings, {"DEVELOPMENT_TEAM": "5NN7GUYB6T", "IPHONEOS_DEPLOYMENT_TARGET": "15.0"})
        with self.assertRaises(build_script.BuildError):
            build_script.build_settings("    DEVELOPMENT_TEAM = 5NN7GUYB6T\n", ["DEVELOPMENT_TEAM", "IPHONEOS_DEPLOYMENT_TARGET"])

    def test_build_machine_is_pinned_in_each_info_plist_in_its_format(self):
        with tempfile.TemporaryDirectory() as tmp:
            products = pathlib.Path(tmp)
            app = products / "Applications/SimpleX.app"
            (app / "Frameworks/X.framework").mkdir(parents=True)
            info = {"BuildMachineOSBuild": "24H232", "CFBundleName": "SimpleX"}
            (app / "Info.plist").write_bytes(plistlib.dumps(info, fmt=plistlib.FMT_BINARY))
            (app / "Frameworks/X.framework/Info.plist").write_bytes(plistlib.dumps({"BuildMachineOSBuild": "24H232"}))
            (app / "Settings.bundle").mkdir()
            unrelated = plistlib.dumps(["a"])
            (app / "Settings.bundle/Info.plist").write_bytes(unrelated)
            build_script.pin_build_machine(products)
            binary = (app / "Info.plist").read_bytes()
            self.assertTrue(binary.startswith(b"bplist00"))
            self.assertEqual(plistlib.loads(binary), {"BuildMachineOSBuild": "25G83", "CFBundleName": "SimpleX"})
            xml = (app / "Frameworks/X.framework/Info.plist").read_bytes()
            self.assertTrue(xml.startswith(b"<?xml"))
            self.assertEqual(plistlib.loads(xml), {"BuildMachineOSBuild": "25G83"})
            self.assertEqual((app / "Settings.bundle/Info.plist").read_bytes(), unrelated)

    def test_entitlements_get_the_team_prefix(self):
        self.assertIn("<string>5NN7GUYB6T.chat.simplex.app</string>", build_script.resolve_entitlements(ENTITLEMENTS, "5NN7GUYB6T"))

    def test_nix_env_finds_a_certificate_bundle(self):
        with tempfile.TemporaryDirectory() as tmp:
            cert = pathlib.Path(tmp) / "ca-bundle.crt"
            cert.write_text("")
            with (
                mock.patch.dict(os.environ, {"PATH": "/usr/bin"}, clear=True),
                patch_all("NIX_CERT_FILES", (pathlib.Path(tmp) / "missing", cert)),
            ):
                env = build_script.nix_env()
            self.assertEqual(env["NIX_SSL_CERT_FILE"], str(cert))
            self.assertTrue(env["PATH"].endswith(":/usr/bin"))
            with (
                mock.patch.dict(os.environ, {}, clear=True),
                patch_all("NIX_CERT_FILES", ()),
                self.assertRaises(build_script.BuildError),
            ):
                build_script.nix_env()


class IosSigningTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = pathlib.Path(self.tmp.name)
        self.bundle = self.dir / "SimpleX.app"
        (self.bundle / "Frameworks/SimpleXChat.framework").mkdir(parents=True)
        (self.bundle / "Frameworks/Info.plist").write_text("not code")
        self.entitlements = self.dir / "SimpleX.entitlements"
        self.entitlements.write_text(ENTITLEMENTS)

    def tearDown(self):
        self.tmp.cleanup()

    def test_signs_nested_code_first_then_the_bundle_with_resolved_entitlements(self):
        apple = FakeApple()
        with patch_all("run", apple), mock.patch("builtins.print"):
            build_script.sign_bundle(self.bundle, self.entitlements, "5NN7GUYB6T", self.dir)
        signs = [c for c in apple.commands if "--sign" in c]
        self.assertEqual([pathlib.Path(c[-1]).name for c in signs], ["SimpleXChat.framework", "SimpleX.app"])
        self.assertIn("5NN7GUYB6T.chat.simplex.app", apple.signed[str(self.bundle)])

    def test_rejects_a_signature_with_other_entitlements(self):
        with patch_all("run", FakeApple(tamper=True)), self.assertRaises(build_script.BuildError):
            build_script.sign_bundle(self.bundle, self.entitlements, "5NN7GUYB6T", self.dir)

    def test_rejects_a_bundle_without_entitlements(self):
        with self.assertRaises(build_script.BuildError):
            build_script.sign_bundle(self.bundle, self.dir / "missing.entitlements", "5NN7GUYB6T", self.dir)

    def test_minimum_version_guard(self):
        libs = self.dir / "apps/ios/Libraries/ios"
        libs.mkdir(parents=True)
        (libs / "libffi.a").write_bytes(b"")
        with patch_all("run", FakeApple()), mock.patch("builtins.print"):
            build_script.check_minimum_versions(self.dir, "15.0")
            with self.assertRaises(build_script.BuildError):
                build_script.check_minimum_versions(self.dir, "14.0")


class IosArchiveTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = pathlib.Path(self.tmp.name)
        self.source, self.art, self.out = root / "src", root / "art", root / "out"
        self.build_root = root / "fixed"
        (self.art / "ios/Assets.xcassets/logo.imageset").mkdir(parents=True)
        for name in ("SimpleX NSE", "SimpleX SE"):
            (self.source / f"apps/ios/{name}").mkdir(parents=True)
            (self.source / f"apps/ios/{name}/{name}.entitlements").write_text(ENTITLEMENTS)
        (self.source / "apps/ios/SimpleX (iOS).entitlements").write_text(ENTITLEMENTS)
        (self.source / "apps/ios/Shared/SimpleXAssets.xcassets").mkdir(parents=True)
        (self.source / "apps/ios/SimpleXChat").mkdir(parents=True)
        (self.source / "apps/ios/SimpleXChat/SimpleX.h").write_text("")
        (self.source / "apps/ios/Libraries/ios").mkdir(parents=True)
        (self.source / "apps/ios/Libraries/ios/libHSsimplex-chat.a").write_bytes(b"")
        self.out.mkdir()

    def tearDown(self):
        self.tmp.cleanup()

    def build(self, art, binary=b"app"):
        """build_ios_archive with art, against stand-ins of nix, Xcode and codesign, with binary as the app's executable;
        returns their commands and warn."""
        apple = FakeApple()
        source = self.build_root / "simplex-chat"
        self.xcconfig = None

        def fake_run(args, cwd=None, env=None, input=None, check=True, capture=False, title=None):
            args = [str(a) for a in args]
            if args[:2] == ["nix", "build"]:
                self.assertIn("NIX_SSL_CERT_FILE", env)
                link = pathlib.Path(args[args.index("--out-link") + 1])
                link.mkdir()
                (link / build_script.IOS_CORE_ZIP).write_bytes(b"core")
            elif args[0] == "scripts/ios/copy-assets.sh":
                (source / "apps/ios/Shared/SimpleXAssets.xcassets/logo.imageset").mkdir()
            elif args[:2] == ["xcodebuild", "-showBuildSettings"]:
                apple.commands.append(args)
                return mock.Mock(returncode=0, stdout="    DEVELOPMENT_TEAM = 5NN7GUYB6T\n    IPHONEOS_DEPLOYMENT_TARGET = 15.0\n")
            elif args[:2] == ["xcodebuild", "archive"]:
                archive = pathlib.Path(args[args.index("-archivePath") + 1])
                self.bridging_header_mtime = (source / "apps/ios/SimpleXChat/SimpleX.h").stat().st_mtime
                xcconfig = source / "apps/ios/Local.xcconfig"
                self.xcconfig = xcconfig.read_text() if xcconfig.exists() else None
                self.cwd = cwd
                app = archive / "Products/Applications/SimpleX.app"
                for appex in ("SimpleX NSE", "SimpleX SE"):
                    (app / f"PlugIns/{appex}.appex").mkdir(parents=True)
                (app / "SimpleX").write_bytes(binary)
                (app / "Info.plist").write_bytes(plistlib.dumps({"BuildMachineOSBuild": "24H232"}, fmt=plistlib.FMT_BINARY))
                (archive / "Info.plist").write_bytes(b"plist")
            elif args[0] == "git":
                apple.commands.append(args)
                return mock.Mock(returncode=0, stdout=f"{COMMIT_EPOCH}\n")
            return apple(args, cwd, env, input, check, capture)

        with (
            patch_all("run", fake_run),
            mock.patch("builtins.print"),
            patch_all("nix_env", return_value={"NIX_SSL_CERT_FILE": "/c"}),
            patch_all("install_ios_libs") as install_libs,
            patch_all("require_xcode", return_value=build_script.CANONICAL_DEVELOPER_DIR) as xcode,
            patch_all("BUILD_ROOT", self.build_root),
            patch_all("check_aslr_off") as aslr,
            patch_all("run_without_aslr", lambda args, cwd: fake_run(args, cwd=cwd)),
            patch_all("machine_paths", return_value=["/Users/builder"]) as paths,
            patch_all("warn") as warn,
        ):
            build_script.build_ios_archive(self.source, art, self.out, install=False)
        self.assertEqual(install_libs.call_args.args[0], source)
        self.assertEqual(install_libs.call_args.args[1].name, build_script.IOS_CORE_ZIP)
        self.assertEqual(self.cwd, source)
        xcode.assert_called_once_with(build_script.XCODE)
        aslr.assert_called_once_with(self.build_root / "work")
        with zipfile.ZipFile(self.out / "SimpleX.xcarchive.zip") as z:
            info = plistlib.loads(z.read("SimpleX.xcarchive/Products/Applications/SimpleX.app/Info.plist"))
        self.assertEqual(info["BuildMachineOSBuild"], build_script.BUILD_MACHINE_OS_BUILD)
        self.assertEqual(paths.call_args.args, (self.source, build_script.CANONICAL_DEVELOPER_DIR))
        self.assertFalse(self.build_root.exists())
        return apple.commands, warn

    def test_build_flow(self):
        commands, warn = self.build(self.art)
        warn.assert_not_called()
        self.assertIn("SWIFT_ACTIVE_COMPILATION_CONDITIONS = $(inherited) SIMPLEX_ASSETS", self.xcconfig)
        self.assertIn(f"SIMPLEX_ASSETS_DIR = {self.build_root / 'work/simplex-chat-art'}", self.xcconfig)
        self.assertFalse((self.source / "apps/ios/Local.xcconfig").exists())
        self.assertEqual(
            [c[0] for c in commands][:6], ["nix", "scripts/ios/copy-assets.sh", "xcodebuild", "otool", "git", "xcodebuild"]
        )
        archive = next(c for c in commands if c[:2] == ["xcodebuild", "archive"])
        self.assertIn("CODE_SIGNING_ALLOWED=NO", archive)
        self.assertEqual(archive[archive.index("-derivedDataPath") + 1], str(self.build_root / "work/DerivedData"))
        signed = [c[-1] for c in commands if c[0] == "codesign" and "--entitlements" in c and "-d" not in c]
        self.assertEqual([pathlib.Path(p).name for p in signed], ["SimpleX NSE.appex", "SimpleX SE.appex", "SimpleX.app"])
        self.assertIn(["plutil", "-replace", "ApplicationProperties.Team", "-string", "5NN7GUYB6T"], [c[:5] for c in commands])
        self.assertIn(["plutil", "-replace", "CreationDate", "-date", "2023-11-14T22:13:20Z"], [c[:5] for c in commands])
        self.assertEqual(self.bridging_header_mtime, COMMIT_EPOCH)
        self.assert_archive_zip()
        self.assertEqual((self.out / build_script.IOS_CORE_ZIP).read_bytes(), b"core")

    def assert_archive_zip(self):
        with zipfile.ZipFile(self.out / "SimpleX.xcarchive.zip") as z:
            self.assertEqual(z.namelist()[0], "SimpleX.xcarchive/")
            self.assertIn("SimpleX.xcarchive/Products/Applications/SimpleX.app/SimpleX", z.namelist())
            self.assertEqual({i.date_time for i in z.infolist()}, {(2023, 11, 14, 22, 13, 20)})

    def test_without_the_art_checkout_the_archive_has_the_default_icons(self):
        commands, warn = self.build(None)
        self.assertIn("default icons", warn.call_args.args[0])
        self.assertIsNone(self.xcconfig)
        self.assertNotIn("scripts/ios/copy-assets.sh", [c[0] for c in commands])
        self.assert_archive_zip()

    def test_an_archive_with_a_path_of_the_machine_fails_after_it_is_zipped(self):
        with self.assertRaises(build_script.BuildError) as raised:
            self.build(None, binary=b"app /Users/builder/simplex-chat/apps/ios/Shared/AppDelegate.swift")
        self.assertIn("SimpleX.app/SimpleX: /Users/builder", str(raised.exception))
        self.assert_archive_zip()

    def test_xcode_outside_its_usual_place_fails(self):
        with patch_all("require_xcode", return_value=XCODE_DEVELOPER), self.assertRaises(build_script.BuildError) as raised:
            build_script.build_ios_archive(self.source, self.art, self.out, install=False)
        self.assertIn("/Applications/Xcode_26.6.app", str(raised.exception))
        self.assertIn("/Applications/Xcode.app", str(raised.exception))

    def test_art_checkout_without_the_assets_fails(self):
        with self.assertRaises(build_script.BuildError):
            build_script.build_ios_archive(self.source, self.art / "missing", self.out, install=False)


class IosLibsTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.source = self.root / "src"
        (self.source / "apps/ios/SimpleX.xcodeproj").mkdir(parents=True)
        (self.source / build_script.IOS_PBXPROJ).write_text(PBXPROJ)

    def tearDown(self):
        self.tmp.cleanup()

    def install_libraries(self, *names):
        ios = self.source / build_script.IOS_LIBRARIES / "ios"
        ios.mkdir(parents=True, exist_ok=True)
        for name in names:
            (ios / name).write_bytes(b"")

    def test_pbxproj_points_at_the_installed_libraries(self):
        self.install_libraries("libHSsimplex-chat-6.5.1.0-New.a", "libHSsimplex-chat-6.5.1.0-New-ghc9.6.3.a", "libffi.a")
        with patch_all("log"):
            build_script.update_pbxproj(self.source)
        text = (self.source / build_script.IOS_PBXPROJ).read_text()
        self.assertNotIn("Old", text)
        self.assertEqual(text.count("libHSsimplex-chat-6.5.1.0-New-ghc9.6.3.a"), 6)
        self.assertEqual(text.count("libHSsimplex-chat-6.5.1.0-New.a"), 6)

    def test_pbxproj_without_four_references_each_is_not_changed(self):
        (self.source / build_script.IOS_PBXPROJ).write_text(PBXPROJ.replace("    D1 /* libHSsimplex-chat-6.5.0.0-Old.a */,\n", ""))
        self.install_libraries("libHSsimplex-chat-6.5.1.0-New.a", "libHSsimplex-chat-6.5.1.0-New-ghc9.6.3.a")
        with self.assertRaises(build_script.BuildError):
            build_script.update_pbxproj(self.source)
        self.assertIn("Old", (self.source / build_script.IOS_PBXPROJ).read_text())

    def test_pbxproj_needs_both_libraries(self):
        self.install_libraries("libHSsimplex-chat-6.5.1.0-New.a")
        with self.assertRaises(build_script.BuildError):
            build_script.update_pbxproj(self.source)

    def test_simulator_libraries_must_be_marked_for_the_simulator(self):
        sim = self.root / "sim"
        sim.mkdir()
        (sim / "libffi.a").write_bytes(b"")
        for platform_line, rejected in (
            ("platform 7", False),
            ("platform IOSSIMULATOR", False),
            ("platform 2", True),
            ("cmd LC_VERSION_MIN_IPHONEOS", True),
        ):
            with patch_all("run", Recorder(answers={"otool": f"Load command 1\n {platform_line}\n"})):
                if rejected:
                    with self.assertRaises(build_script.BuildError):
                        build_script.check_simulator_libs(sim)
                else:
                    build_script.check_simulator_libs(sim)

    def test_install_relabels_copies_for_ios_and_the_simulator(self):
        package = self.root / build_script.IOS_CORE_ZIP
        with zipfile.ZipFile(package, "w") as z:
            z.writestr("libHSsimplex-chat-6.5.1.0-New.a", b"a")
            z.writestr("libHSsimplex-chat-6.5.1.0-New-ghc9.6.3.a", b"g")
        recorder = Recorder(answers={"otool": " platform 7\n"})
        work = self.root / "work"
        work.mkdir()
        converted = []

        def fake_simulator_archive(lib):
            converted.append(lib.parent.name)
            return 3 if "ghc" in lib.name else 0

        with (
            patch_all("run", recorder),
            patch_all("fetch_pinned", lambda url, digest, target: target),
            patch_all("log"),
            patch_all("simulator_archive", fake_simulator_archive),
        ):
            build_script.install_ios_libs(self.source, package, work)
        self.assertEqual(converted, ["sim", "sim"])
        ranlib = [c for c in recorder.commands if c[0] == "ranlib"]
        self.assertEqual([pathlib.Path(c[1]).name for c in ranlib], ["libHSsimplex-chat-6.5.1.0-New-ghc9.6.3.a"])
        libraries = self.source / build_script.IOS_LIBRARIES
        for name in ("mac", "ios", "sim"):
            self.assertEqual(len(list((libraries / name).iterdir())), 2)
        relabels = [c for c in recorder.commands if c[0].endswith("mac2ios")]
        self.assertEqual(sum(c[1] == "-s" for c in relabels), 2)
        self.assertEqual(len(relabels), 4)
        self.assertIn("libHSsimplex-chat-6.5.1.0-New.a", (self.source / build_script.IOS_PBXPROJ).read_text())

    def test_simulator_package_has_the_libraries_of_the_nix_package(self):
        source = self.root / "src"
        (source / "simplex-chat.cabal").write_text("name: simplex-chat\nversion:        6.5.1.0\n")
        build = source / "dist-newstyle/build/aarch64-osx/ghc-9.6.3/simplex-chat-6.5.1.0/build"
        build.mkdir(parents=True)
        (build / "libHSsimplex-chat-6.5.1.0-New-ghc9.6.3.a").write_bytes(b"lib")
        brew = self.root / "brew"
        (brew / "lib").mkdir(parents=True)
        for name in ("libffi.a", "libgmp.a", "libgmpxx.a"):
            (brew / "lib" / name).write_bytes(b"x")
        recorder = Recorder(answers={"numeric-version": "9.6.3\n", "brew --prefix": f"{brew}\n"})
        openssl = build_script.OpenSsl(self.root / "o/include", self.root / "o/lib")
        with patch_all("run", recorder), patch_all("host_openssl", return_value=openssl):
            build_script.build_ios_sim_libs(source, self.root)
        self.assertIn(f"extra-lib-dirs: {self.root / 'o/lib'}", (source / "cabal.project.local").read_text())
        self.assertIn(["cabal", "build", "-j", "lib:simplex-chat"], recorder.commands)
        with zipfile.ZipFile(self.root / build_script.IOS_CORE_ZIP) as z:
            self.assertEqual(sorted(z.namelist()), ["libHSsimplex-chat-6.5.1.0-New-ghc9.6.3.a", "libffi.a", "libgmp.a", "libgmpxx.a"])


class ReproducibleArchiveTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_commit_epoch_is_the_head_commit_time(self):
        recorder = Recorder(answers={"log -1 --format=%ct": "1700000000\n"})
        with patch_all("run", recorder):
            self.assertEqual(build_script.commit_epoch(self.root), "1700000000")
        self.assertEqual(recorder.commands, [["git", "-C", str(self.root), "log", "-1", "--format=%ct"]])

    def make_archive(self, group_writable):
        archive = self.root / "build" / "SimpleX.xcarchive"
        app = archive / "Products/Applications/SimpleX.app"
        (app / "Frameworks/SimpleXChat.framework").mkdir(parents=True)
        (app / "SimpleX").write_bytes(b"binary" * 100)
        (app / "SimpleX").chmod(0o775 if group_writable else 0o755)
        (app / "Info.plist").write_bytes(b"plist")
        (app / "Info.plist").chmod(0o664 if group_writable else 0o644)
        (app / "Frameworks/SimpleXChat.framework/SimpleXChat").write_bytes(b"framework")
        (app / "Frameworks/Current").symlink_to("SimpleXChat.framework")
        (archive / "Info.plist").write_bytes(b"archive plist")
        return archive

    def test_zip_tree_does_not_depend_on_times_or_umask(self):
        first, second = self.root / "first.zip", self.root / "second.zip"
        build_script.zip_tree(self.make_archive(group_writable=False), first, "1700000000")
        shutil.rmtree(self.root / "build")
        archive = self.make_archive(group_writable=True)
        build_script.touch_tree(archive, "1800000000")
        build_script.zip_tree(archive, second, "1700000000")
        self.assertEqual(first.read_bytes(), second.read_bytes())

    def test_zip_tree_keeps_the_parent_name_modes_and_symlinks(self):
        target = self.root / "SimpleX.xcarchive.zip"
        build_script.zip_tree(self.make_archive(group_writable=True), target, "1700000000")
        app = "SimpleX.xcarchive/Products/Applications/SimpleX.app"
        with zipfile.ZipFile(target) as z:
            infos = {i.filename: i for i in z.infolist()}
            self.assertEqual(z.namelist(), sorted(z.namelist(), key=lambda name: name.rstrip("/").split("/")))
            self.assertEqual(z.namelist()[0], "SimpleX.xcarchive/")
            self.assertEqual({i.date_time for i in infos.values()}, {(2023, 11, 14, 22, 13, 20)})
            modes = {name: oct(info.external_attr >> 16) for name, info in infos.items()}
            self.assertEqual(modes["SimpleX.xcarchive/"], "0o40755")
            self.assertEqual(modes[f"{app}/SimpleX"], "0o100755")
            self.assertEqual(modes[f"{app}/Info.plist"], "0o100644")
            self.assertEqual(modes[f"{app}/Frameworks/Current"], "0o120755")
            self.assertEqual(z.read(f"{app}/Frameworks/Current"), b"SimpleXChat.framework")
            self.assertEqual(z.read(f"{app}/SimpleX"), b"binary" * 100)
            self.assertEqual(infos[f"{app}/SimpleX"].compress_type, zipfile.ZIP_DEFLATED)


class ZeroArHeadersTest(unittest.TestCase):
    def test_zeroes_dates_and_owners_and_keeps_the_members(self):
        long_name = "M20220812_incognito_profiles.o"
        built = ar_archive(ar_member("__.SYMDEF SORTED", b"toc!"), ar_member(long_name, b"odd"), ar_member("a.o", b"even"))
        zero = {"date": b"0", "uid": b"0", "gid": b"0", "mode": b"644"}
        members = (ar_member("__.SYMDEF SORTED", b"toc!", **zero), ar_member(long_name, b"odd", **zero), ar_member("a.o", b"even", **zero))
        self.assertEqual(zero_ar_headers.zero_headers(built), ar_archive(*members))

    def test_rejects_other_files(self):
        cases = {
            "not an ar archive": b"\xcf\xfa\xed\xfe",
            "ends in a member header": ar_archive(ar_member("a.o", b"x"))[:-20],
            "no member header": ar_archive(ar_member("a.o", b"xy")).replace(b"`\n", b"!!"),
        }
        for message, data in cases.items():
            with self.subTest(message), self.assertRaisesRegex(zero_ar_headers.ArchiveError, message):
                zero_ar_headers.zero_headers(data)

    def test_main_rewrites_each_archive_and_exits_on_other_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            libs = [pathlib.Path(tmp, name) for name in ("libffi.a", "libgmp.a")]
            for lib in libs:
                lib.write_bytes(ar_archive(ar_member("a.o", b"x")))
            zero_ar_headers.main([str(lib) for lib in libs])
            expected = ar_archive(ar_member("a.o", b"x", date=b"0", uid=b"0", gid=b"0", mode=b"644"))
            self.assertEqual([lib.read_bytes() for lib in libs], [expected, expected])
            other = pathlib.Path(tmp, "SimpleX.h")
            other.write_bytes(b"#import")
            with self.assertRaises(SystemExit) as exit:
                zero_ar_headers.main([str(other)])
            self.assertIn("SimpleX.h: not an ar archive", str(exit.exception.code))
            self.assertEqual(other.read_bytes(), b"#import")


def archive_zip(path, xcode):
    with zipfile.ZipFile(path, "w") as z:
        info = plistlib.dumps({"DTXcodeBuild": xcode}, fmt=plistlib.FMT_BINARY)
        z.writestr("SimpleX.xcarchive/Products/Applications/SimpleX.app/Info.plist", info)


class IosReproduceTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.results = self.root / "results"

    def tearDown(self):
        self.tmp.cleanup()

    def reproduce(self, released_xcode, built_core=b"core"):
        def fake_download(url, target):
            if target.name == "SimpleX.xcarchive.zip":
                archive_zip(target, released_xcode)
            else:
                target.write_bytes(b"core")

        def fake_build(source, art, out, install):
            archive_zip(out / "SimpleX.xcarchive.zip", released_xcode)
            (out / "pkg-ios-aarch64-swift-json.zip").write_bytes(built_core)

        with (
            patch_all("download", fake_download),
            patch_all("checkout") as checkout,
            patch_all("build_ios_archive", fake_build),
            patch_all("log"),
            mock.patch("builtins.print"),
        ):
            failed = build_script.reproduce_ios("v7.1.0", "owner/fork", self.root / "art", True, self.root, self.results)
        return failed, checkout

    def test_archive_records_the_xcode_that_built_it(self):
        archive_zip(self.root / "a.zip", "17A324")
        self.assertEqual(build_script.archive_xcode(self.root / "a.zip"), "17A324")

    def test_archive_and_core_are_compared_byte_for_byte(self):
        failed, checkout = self.reproduce(build_script.XCODE.build)
        self.assertEqual(failed, [])
        checkout.assert_called_once_with(self.root, "v7.1.0")
        self.assertTrue((self.results / "ios/_sha256sums").is_file())
        failed, _ = self.reproduce(build_script.XCODE.build, built_core=b"other core")
        self.assertEqual(failed, ["pkg-ios-aarch64-swift-json.zip"])

    def test_a_release_of_another_xcode_is_refused_before_the_build(self):
        with self.assertRaisesRegex(build_script.BuildError, "archived with Xcode build 17A324, and this script builds with Xcode 26.6"):
            self.reproduce("17A324")
