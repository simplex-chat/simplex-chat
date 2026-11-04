import base64
import contextlib
import json
import os
import pathlib
import plistlib
import re
import shlex
import struct
import subprocess
import tempfile
import unittest
import zipfile
from unittest import mock

from .test_dmg import udif
from .test_macho import AD_HOC, DATA, DEVELOPER_ID, codesign, executable
from .testing import REPO_ROOT, FakeOtool, Recorder, build_script, otool_big_l, otool_l, patch_all

XCODE_SDK_LIB = "/Applications/Xcode_26.6.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib"

single_file = build_script.single_file


class MacLibTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_otool_parsing(self):
        fake = FakeOtool(
            big_l={"lib.dylib": otool_big_l("@rpath/lib.dylib", ["@rpath/libHSbase.dylib", "/usr/lib/libSystem.B.dylib"])},
            small_l={"lib.dylib": otool_l(["@loader_path/../lib/x", "/Users/runner/ghc"], ["/w/openssl/lib/libcrypto.3.dylib"])},
        )
        lib = pathlib.Path("lib.dylib")
        with patch_all("run", fake):
            self.assertEqual(build_script.rpath_dependencies(lib), ["libHSbase.dylib"])
            self.assertEqual(build_script.rpaths(lib), ["@loader_path/../lib/x", "/Users/runner/ghc"])
            self.assertEqual(build_script.libcrypto_path(lib), "/w/openssl/lib/libcrypto.3.dylib")

    def dependency_tree(self):
        """libsimplex, linked without absolute rpaths, loading a store library, which loads GHC's base, which loads the
        rts next to it, as GHC's libraries do, and libffi, which the build adds to deps."""
        self.ghc = self.root / "ghc"
        (self.ghc / "lib").mkdir(parents=True)
        self.store = self.root / "cabal/store/ghc-9.6.3/lib"
        self.store.mkdir(parents=True)
        self.build = self.root / "build"
        self.deps = self.build / "deps"
        self.deps.mkdir(parents=True)
        (self.build / "libsimplex.dylib").write_bytes(b"simplex")
        (self.deps / "libffi.dylib").write_bytes(b"ffi")
        (self.store / "libHSsmplxmq.dylib").write_bytes(b"smplxmq")
        (self.ghc / "lib/libHSbase.dylib").write_bytes(b"base")
        (self.ghc / "lib/libHSrts.dylib").write_bytes(b"rts")
        return FakeOtool(
            big_l={
                "libsimplex.dylib": otool_big_l("@rpath/libsimplex.dylib", ["@rpath/libHSsmplxmq.dylib", "@rpath/libffi.dylib"]),
                "libHSsmplxmq.dylib": otool_big_l("@rpath/libHSsmplxmq.dylib", ["@rpath/libHSbase.dylib"]),
                "libHSbase.dylib": otool_big_l("@rpath/libHSbase.dylib", ["@rpath/libHSrts.dylib"]),
                "libHSrts.dylib": otool_big_l("@rpath/libHSrts.dylib", []),
            },
            small_l={
                "libsimplex.dylib": otool_l(["@loader_path"]),
                "libHSsmplxmq.dylib": otool_l([]),
                "libHSbase.dylib": otool_l(["@loader_path/../lib"]),
                "libHSrts.dylib": otool_l([]),
            },
        )

    def test_dependencies_are_found_in_the_store_and_ghcs_libdir_without_rpaths(self):
        fake = self.dependency_tree()
        with patch_all("run", fake):
            build_script.copy_mac_dependencies(self.build / "libsimplex.dylib", self.deps, self.ghc, [self.store, self.ghc / "lib"], set())
        names = ["libHSbase.dylib", "libHSrts.dylib", "libHSsmplxmq.dylib", "libffi.dylib", "libsimplex.dylib"]
        self.assertEqual(sorted(p.name for p in self.deps.iterdir()), names)
        add_rpath = [c for c in fake.commands if c[:2] == ["install_name_tool", "-add_rpath"]]
        self.assertEqual(add_rpath, [["install_name_tool", "-add_rpath", "@loader_path", str(self.deps / "libHSbase.dylib")]])

    def test_a_dependency_in_no_library_directory_fails(self):
        fake = self.dependency_tree()
        (self.store / "libHSsmplxmq.dylib").unlink()
        with patch_all("run", fake), self.assertRaises(build_script.BuildError) as raised:
            build_script.copy_mac_dependencies(self.build / "libsimplex.dylib", self.deps, self.ghc, [self.store], set())
        self.assertIn(f"libHSsmplxmq.dylib, which libsimplex.dylib loads, is in none of {self.store}", str(raised.exception))

    def test_the_store_libraries_are_in_cabal_dir(self):
        with mock.patch.dict(os.environ, {"CABAL_DIR": "/private/tmp/simplex-chat-build/cabal"}):
            libraries = build_script.cabal_store_libraries()
        self.assertEqual(libraries, pathlib.Path("/private/tmp/simplex-chat-build/cabal/store/ghc-9.6.3/lib"))
        with mock.patch.dict(os.environ, {}, clear=True), self.assertRaises(build_script.BuildError):
            build_script.cabal_store_libraries()

    def test_libcrypto_is_bundled_and_relative_rpaths_kept(self):
        libs = self.root / "libs"
        libs.mkdir()
        openssl = self.root / "openssl/lib"
        openssl.mkdir(parents=True)
        (openssl / "libcrypto.3.dylib").write_bytes(b"crypto")
        for name in ("libHSsmplxmq-6.5.dylib", "libsimplex.dylib", "libHSdrct-2.3.dylib", "libffi.dylib"):
            (libs / name).write_bytes(b"x")
        linked = str(openssl / "libcrypto.3.dylib")
        fake = FakeOtool(
            small_l={
                "libHSsmplxmq-6.5.dylib": otool_l([], [linked]),
                "libHSdrct-2.3.dylib": otool_l([], [linked]),
                "libsimplex.dylib": otool_l(["@loader_path", "@executable_path/../lib"]),
            }
        )
        with patch_all("run", fake):
            build_script.bundle_mac_libcrypto(libs, build_script.Database.SQLITE)
            kept = build_script.rpaths(libs / "libsimplex.dylib")
        self.assertEqual((libs / "libcrypto.3.0.dylib").read_bytes(), b"crypto")
        changes = [c[-1] for c in fake.commands if c[:2] == ["install_name_tool", "-change"]]
        self.assertEqual(sorted(pathlib.Path(c).name for c in changes), ["libHSdrct-2.3.dylib", "libHSsmplxmq-6.5.dylib"])
        self.assertIn(["install_name_tool", "-id", "libcrypto.3.0.dylib", str(libs / "libcrypto.3.0.dylib")], fake.commands)
        self.assertEqual(kept, ["@loader_path", "@executable_path/../lib"])

    def test_only_rpaths_relative_to_the_library_or_the_app_pass(self):
        libs = ["libapp-lib.dylib", "libsimplex.dylib", "libHSbase.dylib"]
        fake = FakeOtool(
            small_l={
                "libapp-lib.dylib": otool_l(["@loader_path", "/private/tmp/simplex-chat-build/simplex-chat/libs/mac-aarch64"]),
                "libsimplex.dylib": otool_l(["@loader_path", "@executable_path/../Frameworks"]),
                "libHSbase.dylib": otool_l(["@rpath/../lib"]),
            }
        )
        with patch_all("run", fake), self.assertRaises(build_script.BuildError) as raised:
            build_script.check_relative_rpaths([pathlib.Path(name) for name in libs])
        message = str(raised.exception)
        self.assertIn("libapp-lib.dylib (/private/tmp/simplex-chat-build/simplex-chat/libs/mac-aarch64)", message)
        self.assertIn("libHSbase.dylib (@rpath/../lib)", message)
        self.assertNotIn("libsimplex", message)
        with patch_all("run", fake):
            build_script.check_relative_rpaths([pathlib.Path("libsimplex.dylib")])

    def test_libraries_linked_with_absolute_rpaths_fail(self):
        libs = self.root / "libs"
        libs.mkdir()
        crypto = self.root / "libcrypto.3.dylib"
        crypto.write_bytes(b"c")
        for name in ("libHSsmplxmq-6.5.dylib", "libHSdrct-2.3.dylib", "libffi.dylib"):
            (libs / name).write_bytes(b"x")
        fake = FakeOtool(
            small_l={
                "libHSsmplxmq-6.5.dylib": otool_l(["@loader_path", "/Users/runner/.ghcup/ghc/9.6.3/lib"], [str(crypto)]),
                "libHSdrct-2.3.dylib": otool_l([XCODE_SDK_LIB], [str(crypto)]),
            }
        )
        with patch_all("run", fake), self.assertRaises(build_script.BuildError) as raised:
            build_script.bundle_mac_libcrypto(libs, build_script.Database.SQLITE)
        message = str(raised.exception)
        self.assertIn(f"libHSdrct-2.3.dylib ({XCODE_SDK_LIB})", message)
        self.assertIn("libHSsmplxmq-6.5.dylib (/Users/runner/.ghcup/ghc/9.6.3/lib)", message)
        self.assertNotIn("@loader_path", message)
        self.assertFalse(any(c[:2] == ["install_name_tool", "-delete_rpath"] for c in fake.commands))

    def test_the_path_that_otool_prints_first_is_not_a_local_directory(self):
        libs = self.root / "Users/runner/libs"
        libs.mkdir(parents=True)
        (libs / "libHSsmplxmq-6.5.dylib").write_bytes(b"x")
        (libs / "libffi.dylib").write_bytes(b"x")
        crypto = self.root / "libcrypto.3.dylib"
        crypto.write_bytes(b"c")

        def fake_run(args, **kwargs):
            args = [str(a) for a in args]
            if args[0] == "otool":
                lines = [f"{args[2]}:", "Load command 0", "      cmd LC_ID_DYLIB", "         name @rpath/lib.dylib (offset 24)"]
                if "smplxmq" in args[2] and not any("-change" in c for c in changed):
                    lines.append(f"         name {crypto} (offset 24)")
                return mock.Mock(returncode=0, stdout="\n".join(lines) + "\n")
            changed.append(args)
            return mock.Mock(returncode=0, stdout="")

        changed = []
        with patch_all("run", fake_run):
            build_script.bundle_mac_libcrypto(libs, build_script.Database.SQLITE)
        self.assertTrue((libs / "libcrypto.3.0.dylib").exists())

    def test_libraries_still_pointing_to_local_directories_fail(self):
        libs = self.root / "libs"
        libs.mkdir()
        (libs / "libHSsmplxmq-6.5.dylib").write_bytes(b"x")
        (libs / "libffi.dylib").write_bytes(b"x")
        crypto = self.root / "libcrypto.3.dylib"
        crypto.write_bytes(b"c")
        for local in ("/opt/homebrew/lib/libgmp.dylib", f"{XCODE_SDK_LIB}/libz.dylib"):
            fake = FakeOtool(small_l={"libHSsmplxmq-6.5.dylib": otool_l([], [str(crypto), local])})
            with self.subTest(local), patch_all("run", fake), self.assertRaises(build_script.BuildError) as raised:
                build_script.bundle_mac_libcrypto(libs, build_script.Database.SQLITE)
            self.assertIn("libHSsmplxmq-6.5.dylib", str(raised.exception))

    def test_builds_use_the_selected_sdk_and_the_minimum_macos_of_the_launcher(self):
        recorder = Recorder(answers={"--show-sdk-path": f"{XCODE_SDK_LIB.removesuffix('/usr/lib')}\n"})
        with patch_all("run", recorder), patch_all("COMMAND_LINE_TOOLS_SDK", self.root / "missing"):
            env = build_script.macos_build_env()
        self.assertEqual(recorder.commands, [["xcrun", "--sdk", "macosx", "--show-sdk-path"]])
        self.assertEqual(env, {"SDKROOT": XCODE_SDK_LIB.removesuffix("/usr/lib"), "MACOSX_DEPLOYMENT_TARGET": "11.0"})

    def test_the_command_line_tools_sdk_must_be_the_selected_one_when_it_exists(self):
        xcode_sdk = self.root / "Xcode.app/SDKs/MacOSX26.5.sdk"
        other_sdk = self.root / "CommandLineTools/SDKs/MacOSX27.0.sdk"
        xcode_sdk.mkdir(parents=True)
        other_sdk.mkdir(parents=True)
        link = self.root / "CommandLineTools/SDKs/MacOSX.sdk"
        with patch_all("COMMAND_LINE_TOOLS_SDK", link):
            build_script.check_command_line_tools_sdk(xcode_sdk)
            link.symlink_to(other_sdk)
            with self.assertRaises(build_script.BuildError) as raised:
                build_script.check_command_line_tools_sdk(xcode_sdk)
            self.assertIn(f"sudo ln -sfn {xcode_sdk} {link}", str(raised.exception))
            link.unlink()
            link.symlink_to(xcode_sdk)
            build_script.check_command_line_tools_sdk(xcode_sdk)

    def test_the_app_declares_the_deployment_target_as_its_minimum_macos(self):
        app = self.root / "SimpleX.app"
        (app / "Contents").mkdir(parents=True)
        (app / "Contents/Info.plist").write_bytes(plistlib.dumps({"LSMinimumSystemVersion": "11.0"}))
        build_script.check_minimum_system_version(app)
        (app / "Contents/Info.plist").write_bytes(plistlib.dumps({"LSMinimumSystemVersion": "10.13"}))
        with self.assertRaises(build_script.BuildError) as raised:
            build_script.check_minimum_system_version(app)
        self.assertIn("declares macOS 10.13, not 11.0", str(raised.exception))

    def test_the_gradle_build_sets_the_deployment_target_as_the_minimum_macos(self):
        gradle = (REPO_ROOT / "apps/multiplatform/desktop/build.gradle.kts").read_text()
        self.assertEqual(re.findall(r'minimumSystemVersion = "([^"]+)"', gradle), [build_script.MACOS_DEPLOYMENT_TARGET])

    def run_linker(self, args):
        """The arguments that the linker of rpathless_cabal_local passes to cc, a stand-in that records them, with the
        files of response files that cc gets in place of their names."""
        bin_dir = self.root / "bin"
        bin_dir.mkdir(exist_ok=True)
        (bin_dir / "cc").write_text('#!/bin/sh\nprintf "%s\\n" "$@" > "$CC_ARGS"\n')
        (bin_dir / "cc").chmod(0o755)
        local = build_script.rpathless_cabal_local(self.root)
        linker = self.root / build_script.LINKER_NAME
        self.assertEqual(local, f"package *\n    ghc-options: -fno-use-rpaths -pgml {linker}\n")
        env = {**os.environ, "PATH": f"{bin_dir}:{os.environ['PATH']}", "CC_ARGS": str(self.root / "args")}
        subprocess.run([linker, *args], env=env, check=True)
        passed = (self.root / "args").read_text().splitlines()
        responses = {arg: shlex.split(pathlib.Path(arg[1:]).read_text()) for arg in passed if pathlib.Path(arg[1:]).is_file()}
        return [line for arg in passed for line in responses.get(arg, [arg])]

    def test_the_linker_drops_absolute_rpaths_and_runs_cc_with_the_rest(self):
        kept = ["-dynamiclib", "-Wl,-rpath,@loader_path", "-install_name", "@rpath/lib.dylib"]
        self.assertEqual(self.run_linker([*kept[:2], "-Wl,-rpath,/Users/runner/.ghcup/ghc/9.6.3/lib", *kept[2:]]), kept)

    def test_the_linker_drops_absolute_rpaths_from_ghcs_response_file(self):
        response = self.root / "ghc_1.rsp"
        quoted = ["-dynamiclib", "-o", "/tmp/a b/lib.dylib", f"-Wl,-rpath,{XCODE_SDK_LIB}", "-Wl,-headerpad,8000"]
        response.write_text("".join('"' + arg.replace("\\", "\\\\").replace('"', '\\"') + '"\n' for arg in quoted))
        self.assertEqual(self.run_linker([f"@{response}"]), ["-dynamiclib", "-o", "/tmp/a b/lib.dylib", "-Wl,-headerpad,8000"])

    def test_each_form_of_an_absolute_rpath_is_dropped(self):
        args = [
            "-Wl,-rpath", "-Wl,/opt/ghc/lib",
            "-Xlinker", "-rpath", "-Xlinker", "/opt/ghc/lib",
            "-rpath", "/opt/ghc/lib",
            "-Wl,-rpath,/opt/ghc/lib",
            "-Xlinker", "-rpath", "-Xlinker", "@loader_path",
            "-rpath", "@loader_path/..",
            "-Wl,-rpath",
        ]  # fmt: skip
        kept = ["-Xlinker", "-rpath", "-Xlinker", "@loader_path", "-rpath", "@loader_path/..", "-Wl,-rpath"]
        self.assertEqual(build_script.without_absolute_rpaths(args), kept)
        nested = self.root / "nested.rsp"
        nested.write_text("'-Wl,-rpath,/opt' '-L/opt'\n")
        outer = self.root / "outer.rsp"
        outer.write_text(f"-o lib.dylib @{nested}\n")
        self.assertEqual(list(build_script.expand([f"@{outer}", "@missing"])), ["-o", "lib.dylib", "-Wl,-rpath,/opt", "-L/opt", "@missing"])

    def test_binaries_for_a_newer_macos_than_the_deployment_target_fail(self):
        libs = self.root / "libs"
        libs.mkdir()
        for name in ("libsimplex.dylib", "libHSbase-4.18.1.0-ghc9.6.3.dylib", "libcrypto.3.0.dylib"):
            (libs / name).write_bytes(b"")
        (libs / "vlc").mkdir()
        cli = self.root / "simplex-chat"
        cli.write_bytes(b"")

        def otool(minos):
            return f"{minos}:\nLoad command 9\n      cmd LC_BUILD_VERSION\n  cmdsize 32\n platform 1\n    minos {minos}\n      sdk 26.5\n"

        minimums = {"simplex-chat": "11.0", "libsimplex.dylib": "11.0", "libHSbase-4.18.1.0-ghc9.6.3.dylib": "10.10"}
        fake = FakeOtool(small_l={name: otool(minos) for name, minos in {**minimums, "libcrypto.3.0.dylib": "11.0"}.items()})
        with patch_all("run", fake):
            build_script.check_deployment_target([cli, libs])
        self.assertEqual(sorted(c[-1] for c in fake.commands), sorted(str(p) for p in [cli, *libs.glob("*.dylib")]))
        fake.small_l["libcrypto.3.0.dylib"] = otool("26.0")
        with patch_all("run", fake), self.assertRaises(build_script.BuildError) as raised:
            build_script.check_deployment_target([cli, libs])
        self.assertIn("libcrypto.3.0.dylib (26.0)", str(raised.exception))
        fake.small_l["libcrypto.3.0.dylib"] = "libcrypto.3.0.dylib:\nLoad command 0\n      cmd LC_ID_DYLIB\n"
        with patch_all("run", fake), self.assertRaises(build_script.BuildError):
            build_script.check_deployment_target([cli, libs])

    def test_vlc_gets_rpaths_and_its_plugin_cache(self):
        libs, work = self.root / "libs", self.root / "work"
        libs.mkdir()
        work.mkdir()
        macos = work / "vlc/VLC.app/Contents/MacOS"

        def fake_run(args, **kwargs):
            if args[0] == "unzip":
                (macos / "lib").mkdir(parents=True)
                (macos / "plugins").mkdir()
                (macos / "lib/libvlc.dylib").write_bytes(b"vlc")
                (macos / "plugins/libaudio_plugin.dylib").write_bytes(b"p")
            return recorder(args, **kwargs)

        recorder = Recorder()
        cached_times = []

        def cache_gen(args, **kwargs):
            if str(args[0]).endswith("vlc-cache-gen"):
                cached_times.append((macos / "plugins/libaudio_plugin.dylib").stat().st_mtime)
            return fake_run(args, **kwargs)

        with patch_all("run", cache_gen), patch_all("fetch_pinned", lambda u, d, t: t):
            build_script.add_vlc_mac(libs, "aarch64", work)
        plugin = str(macos / "plugins/libaudio_plugin.dylib")
        self.assertIn(["install_name_tool", "-add_rpath", "@loader_path/../../", plugin], recorder.commands)
        self.assertIn([str(work / "vlc/vlc-cache-gen"), "plugins"], recorder.commands)
        self.assertEqual(cached_times, [int(build_script.SOURCE_DATE_EPOCH)])
        self.assertTrue((libs / "vlc/libvlc.dylib").exists())
        self.assertTrue((libs / "vlc/vlc/plugins/libaudio_plugin.dylib").exists())

    def test_keychain_is_searched_while_signing_and_removed_after(self):
        work = self.root
        recorder = Recorder(answers={"list-keychains": '    "/Users/r/login.keychain-db"\n'})
        environ = {"APPLE_SIMPLEX_SIGNING_KEYCHAIN": base64.b64encode(b"keychain").decode()}
        with patch_all("run", recorder), mock.patch.dict(os.environ, environ):
            with build_script.apple_keychain(work) as keychain:
                self.assertEqual(keychain.read_bytes(), b"keychain")
            self.assertFalse(keychain.exists())
        searches = [c for c in recorder.commands if c[:3] == ["security", "list-keychains", "-s"]]
        self.assertEqual(searches, [["security", "list-keychains", "-s", "/Users/r/login.keychain-db", str(keychain)], searches[1]])
        self.assertEqual(searches[1], ["security", "list-keychains", "-s", "/Users/r/login.keychain-db"])

    def notarize(self, status):
        """notarize with the service answering status; returns the log titles of its commands."""
        titles = []

        def fake_run(args, **kwargs):
            titles.append(kwargs.get("title"))
            stdout = json.dumps({"id": "abc", "status": status}) if "submit" in [str(a) for a in args] else ""
            return mock.Mock(returncode=0, stdout=stdout)

        environ = {"APPLE_SIMPLEX_NOTARIZATION_APPLE_ID": "id@x", "APPLE_SIMPLEX_NOTARIZATION_PASSWORD": "secret"}
        with patch_all("run", fake_run), mock.patch.dict(os.environ, environ):
            build_script.notarize(self.root / "libs", self.root)
        return titles

    def test_notarization_must_be_accepted_and_hides_the_password(self):
        titles = self.notarize("Accepted")
        self.assertFalse(any(t and "secret" in t for t in titles))
        with self.assertRaises(build_script.BuildError):
            self.notarize("Invalid")

    def test_app_signing_properties_are_written_and_removed(self):
        source = self.root / "src"
        multiplatform = source / "apps/multiplatform"
        multiplatform.mkdir(parents=True)
        app = source / build_script.DESKTOP_RELEASE_DIR / "app/SimpleX.app"
        seen = []

        def fake_run(args, **kwargs):
            seen.append(([str(a) for a in args], (multiplatform / "local.properties").read_text()))
            app.mkdir(parents=True, exist_ok=True)
            return mock.Mock(returncode=0, stdout="")

        environ = {"APPLE_SIMPLEX_NOTARIZATION_APPLE_ID": "id@x", "APPLE_SIMPLEX_NOTARIZATION_PASSWORD": "secret"}
        with patch_all("run", fake_run), mock.patch.dict(os.environ, environ):
            built = build_script.build_app(source, self.root / "k.keychain")
        self.assertEqual([args[-1] for args, _ in seen], ["createDistributable"])
        self.assertIn("desktop.mac.notarization.team_id=5NN7GUYB6T\n", seen[0][1])
        self.assertIn(f"desktop.mac.signing.keychain={self.root / 'k.keychain'}\n", seen[0][1])
        self.assertFalse((multiplatform / "local.properties").exists())
        self.assertEqual(built, app)

    def test_unsigned_app_is_built_without_properties(self):
        source = self.root / "src"
        (source / "apps/multiplatform").mkdir(parents=True)
        (source / build_script.DESKTOP_RELEASE_DIR / "app/SimpleX.app").mkdir(parents=True)
        recorder = Recorder()
        with patch_all("run", recorder):
            build_script.build_app(source, None)
        self.assertEqual([c[-1] for c in recorder.commands], ["createDistributable"])
        self.assertFalse((source / "apps/multiplatform/local.properties").exists())

    def test_dos_date_of_source_date_epoch(self):
        self.assertEqual(build_script.dos_date_time("1704067200"), struct.pack("<HH", 0, (44 << 9) | (1 << 5) | 1))
        self.assertEqual(build_script.dos_date_time("1704114245"), struct.pack("<HH", (13 << 11) | (4 << 5) | 2, (44 << 9) | (1 << 5) | 1))

    def jar(self, path, date_time):
        with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as z:
            for name, data in (("META-INF/MANIFEST.MF", b"Manifest-Version: 1.0\n"), ("a/B.class", b"class" * 100)):
                z.writestr(zipfile.ZipInfo(name, date_time), data, zipfile.ZIP_DEFLATED)

    def test_jar_entries_get_the_epoch_date_in_both_headers(self):
        jar = self.root / "a.jar"
        self.jar(jar, (2026, 10, 31, 12, 34, 56))
        build_script.normalize_jar(jar, "1704067200")
        data = jar.read_bytes()
        with zipfile.ZipFile(jar) as z:
            self.assertIsNone(z.testzip())
            self.assertEqual({i.date_time for i in z.infolist()}, {(2024, 1, 1, 0, 0, 0)})
            self.assertEqual(z.read("a/B.class"), b"class" * 100)
        for entry in build_script.central_directory(data):
            self.assertEqual(data[entry.local_offset + 10 : entry.local_offset + 14], build_script.dos_date_time("1704067200"))
        broken = bytearray(data)
        broken[0:4] = b"XXXX"
        jar.write_bytes(broken)
        with self.assertRaises(build_script.BuildError):
            build_script.normalize_jar(jar, "1704067200")

    def app_built_at(self, name, date_time, skiko_listed=1):
        app = self.root / name / "SimpleX.app"
        jars = app / "Contents/app"
        jars.mkdir(parents=True)
        self.jar(jars / f"skiko-awt-runtime-macos-arm64-0.9.4.2-{date_time[4]:02}aa.jar", date_time)
        self.jar(jars / "plain.jar", date_time)
        cfg = "[Application]\n" + f"app.classpath=$APPDIR/skiko-awt-runtime-macos-arm64-0.9.4.2-{date_time[4]:02}aa.jar\n" * skiko_listed
        (jars / "SimpleX.cfg").write_text(cfg + "app.classpath=$APPDIR/plain.jar\napp.mainclass=chat.simplex.desktop.MainKt\n")
        build_script.normalize_app(app)
        return {p.name: p.read_bytes() for p in sorted(jars.iterdir())}

    def test_apps_built_at_other_times_get_the_same_jars_and_classpath(self):
        first = self.app_built_at("a", (2026, 10, 31, 12, 34, 56))
        second = self.app_built_at("b", (2026, 10, 31, 13, 45, 2))
        self.assertEqual(first, second)
        skiko = next(name for name in first if name.startswith("skiko"))
        self.assertRegex(skiko, r"^skiko-awt-runtime-macos-arm64-0\.9\.4\.2-[0-9a-f]{32}\.jar$")
        self.assertIn(f"app.classpath=$APPDIR/{skiko}\n".encode(), first["SimpleX.cfg"])
        self.assertIn(b"app.classpath=$APPDIR/plain.jar\n", first["SimpleX.cfg"])

    def test_a_renamed_jar_that_the_classpath_lists_other_than_once_fails(self):
        for listed in (0, 2):
            with self.subTest(listed=listed), self.assertRaises(build_script.BuildError) as raised:
                self.app_built_at(f"listed-{listed}", (2026, 10, 31, 12, 34, 56), skiko_listed=listed)
            self.assertIn("skiko-awt-runtime-macos-arm64-0.9.4.2-34aa.jar", str(raised.exception))

    def test_app_is_signed_again_as_compose_signed_it(self):
        app = self.root / "SimpleX.app"
        app.mkdir()
        recorder = Recorder()
        with patch_all("run", recorder):
            build_script.sign_app(app, None)
            (app / "Contents/_CodeSignature").mkdir(parents=True)
            build_script.sign_app(app, None)
            build_script.sign_app(app, self.root / "k.keychain")
        preserve = "--preserve-metadata=entitlements,flags,runtime"
        verify = ["codesign", "--verify", "--deep", "--strict", "--verbose=2", str(app)]
        self.assertEqual(
            recorder.commands,
            [
                ["codesign", "--force", preserve, "--sign", "-", str(app)],
                verify,
                [
                    "codesign",
                    "--force",
                    preserve,
                    "--timestamp",
                    "--sign",
                    build_script.APPLE_IDENTITY,
                    "--keychain",
                    str(self.root / "k.keychain"),
                    str(app),
                ],
                verify,
            ],
        )

    def check_dmg(self, signed, attached):
        dmg = self.root / "simplex-desktop-macos-aarch64.dmg"
        dmg.write_bytes(b"dmg")
        recorder = Recorder(answers={"hdiutil attach": attached})
        with patch_all("run", recorder):
            build_script.check_dmg(dmg, signed, self.root)
        return dmg, recorder.commands

    ATTACHED = "/dev/disk4          \tApple_partition_scheme\n/dev/disk4s1        \tApple_partition_map\n/dev/disk4s2        \tApple_HFS\n"

    def test_dmg_and_its_volume_are_checked_and_an_ad_hoc_copy_verified(self):
        dmg, commands = self.check_dmg(False, self.ATTACHED)
        copy = str(self.root / f"ad-hoc-{dmg.name}")
        self.assertEqual(
            commands,
            [
                ["hdiutil", "verify", str(dmg)],
                ["hdiutil", "attach", "-nomount", "-readonly", "-noverify", str(dmg)],
                ["fsck_hfs", "-fn", "/dev/disk4s2"],
                ["hdiutil", "detach", "/dev/disk4"],
                ["codesign", "--sign", "-", copy],
                ["codesign", "--verify", "--strict", "--verbose=2", copy],
            ],
        )
        self.assertEqual(pathlib.Path(copy).read_bytes(), b"dmg")

    def test_signed_dmg_is_verified_itself(self):
        dmg, commands = self.check_dmg(True, self.ATTACHED)
        self.assertEqual(commands[-1], ["codesign", "--verify", "--strict", "--verbose=2", str(dmg)])
        self.assertFalse(any("--sign" in c for c in commands))

    def test_dmg_without_one_hfs_partition_fails(self):
        with self.assertRaises(build_script.BuildError):
            self.check_dmg(False, "/dev/disk4          \tApple_partition_scheme\n")

    def test_dmg_whose_volume_fsck_rejects_fails_and_is_detached(self):
        recorder = Recorder(answers={"hdiutil attach": self.ATTACHED}, fail_once=["fsck_hfs"])
        dmg = self.root / "simplex-desktop-macos-aarch64.dmg"
        with patch_all("run", recorder), self.assertRaises(build_script.BuildError):
            build_script.check_dmg(dmg, False, self.root)
        self.assertEqual(recorder.commands[-1], ["hdiutil", "detach", "/dev/disk4"])

    def test_signed_dmg_is_notarized_itself_and_stapled(self):
        dmg = self.root / "simplex-desktop-macos-aarch64.dmg"
        commands = []

        def fake_run(args, **kwargs):
            args = [str(a) for a in args]
            commands.append(args)
            stdout = json.dumps({"id": "abc", "status": "Accepted"}) if "submit" in args else ""
            return mock.Mock(returncode=0, stdout=stdout)

        environ = {"APPLE_SIMPLEX_NOTARIZATION_APPLE_ID": "id@x", "APPLE_SIMPLEX_NOTARIZATION_PASSWORD": "secret"}
        with patch_all("run", fake_run), mock.patch.dict(os.environ, environ):
            build_script.sign_dmg(dmg, self.root / "k.keychain", self.root)
        self.assertEqual(commands[0][:3], ["codesign", "--timestamp", "--sign"])
        self.assertEqual(commands[1][:5], ["xcrun", "notarytool", "submit", str(dmg), "--wait"])
        self.assertFalse(any(c[0] == "ditto" for c in commands))
        self.assertEqual(commands[-1], ["xcrun", "stapler", "staple", str(dmg)])

    def test_java_home_must_be_set(self):
        with mock.patch.dict(os.environ, {"JAVA_HOME": "/jdk"}):
            self.assertEqual(build_script.java_home(), pathlib.Path("/jdk"))
        with mock.patch.dict(os.environ, {}, clear=True), self.assertRaises(build_script.BuildError):
            build_script.java_home()


def jar(path, native):
    with zipfile.ZipFile(path, "w") as z:
        for name, data in (("META-INF/MANIFEST.MF", b"Manifest-Version: 1.0\n"), ("darwin/libjnidispatch.jnilib", native)):
            info = zipfile.ZipInfo(name, date_time=(2024, 1, 1, 0, 0, 0))
            info.compress_type = zipfile.ZIP_DEFLATED
            z.writestr(info, data)


def app_volume(root, signature):
    """A volume of the desktop app as build_dmg stages it, its code signed with signature or, without one, as the
    unsigned build leaves it: the launcher unsigned and the libraries signed ad hoc."""
    app = root / "SimpleX.app/Contents"
    for directory in ("MacOS", "app", "runtime/Contents/Home/lib", "_CodeSignature"):
        (app / directory).mkdir(parents=True)
    (root / "Applications").symlink_to("/Applications")
    (root / ".DS_Store").write_bytes(b"window")
    launcher, library = executable(), executable(code=DATA * 4)
    (app / "MacOS/SimpleX").write_bytes(codesign(launcher, signature) if signature else launcher)
    (app / "MacOS/SimpleX").chmod(0o755)
    (app / "runtime/Contents/Home/lib/libjli.dylib").write_bytes(codesign(library, signature or AD_HOC))
    if signature:
        (app / "_CodeSignature/CodeResources").write_bytes(b"sealed resources")
        (app / "CodeResources").write_bytes(b"notarization ticket")
    name = "jna-5.14.0-" + ("8b3cc65292435ad9f801e6d9b2a3497" if signature else "1a2b3c4d5e6f")
    jar(app / f"app/{name}.jar", codesign(library, signature) if signature else library)
    (app / "app/SimpleX.cfg").write_text(f"[Application]\napp.classpath=$APPDIR/{name}.jar\napp.mainclass=MainKt\n")
    signed = "true" if signature else "false"
    state = f'<jpackage-state version="17.0.21" platform="macOS">\n  <signed>{signed}</signed></jpackage-state>'
    (app / "app/.jpackage.xml").write_text(state)
    return app


class MacReproduceTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.released, self.built = self.root / "released", self.root / "built"
        app_volume(self.released, DEVELOPER_ID)
        app_volume(self.built, None)

    def tearDown(self):
        self.tmp.cleanup()

    def test_signed_volume_matches_the_unsigned_build(self):
        self.assertEqual(build_script.volume_differences(self.released, self.built), [])

    def test_each_kind_of_change_is_found(self):
        app = self.released / "SimpleX.app/Contents"
        (app / "MacOS/SimpleX").write_bytes(codesign(executable(code=DATA * 9), DEVELOPER_ID))
        (app / "MacOS/SimpleX").chmod(0o755)
        (app / "runtime/Contents/Home/lib/libjli.dylib").chmod(0o600)
        cfg = app / "app/SimpleX.cfg"
        cfg.write_text(cfg.read_text().replace("MainKt", "OtherKt"))
        (self.released / "Applications").unlink()
        (self.released / "Applications").symlink_to("/tmp")
        (app / "Resources").mkdir()
        (app / "Resources/extra.icns").write_bytes(b"icon")
        jar(single_file(app / "app", "jna-*.jar"), codesign(executable(code=DATA * 5), DEVELOPER_ID))
        self.assertEqual(
            build_script.volume_differences(self.released, self.built),
            [
                "Applications",
                "SimpleX.app/Contents/MacOS/SimpleX",
                "SimpleX.app/Contents/app/SimpleX.cfg",
                "SimpleX.app/Contents/app/jna-5.14.0-8b3cc65292435ad9f801e6d9b2a3497.jar",
                "SimpleX.app/Contents/runtime/Contents/Home/lib/libjli.dylib",
                "SimpleX.app/Contents/Resources (only one has it)",
                "SimpleX.app/Contents/Resources/extra.icns (only one has it)",
            ],
        )

    def test_code_signature_folders_hold_only_the_hashes_of_the_resources(self):
        (self.released / "SimpleX.app/Contents/_CodeSignature/CodeDirectory").write_bytes(b"extra")
        self.assertEqual(
            build_script.volume_differences(self.released, self.built),
            ["SimpleX.app/Contents/_CodeSignature/CodeDirectory (only one has it)"],
        )

    def test_files_that_are_no_jars_or_text_differ(self):
        app = self.released / "SimpleX.app/Contents/app"
        jar_path = single_file(app, "jna-*.jar")
        jar_path.write_bytes(b"not a zip")
        (app / "SimpleX.cfg").write_bytes(b"\xff\xfe")
        self.assertEqual(
            build_script.volume_differences(self.released, self.built),
            [f"SimpleX.app/Contents/app/{name}" for name in ("SimpleX.cfg", jar_path.name)],
        )

    def test_jpackage_state_differs_only_in_whether_the_app_is_signed(self):
        state = self.released / "SimpleX.app/Contents/app/.jpackage.xml"
        state.write_text(state.read_text().replace('version="17.0.21"', 'version="17.0.22"'))
        self.assertEqual(build_script.volume_differences(self.released, self.built), ["SimpleX.app/Contents/app/.jpackage.xml"])
        state.write_text(state.read_text().replace('version="17.0.22"', 'version="17.0.21"').replace("true", "false"))
        self.assertEqual(build_script.volume_differences(self.released, self.built), [])

    def test_jars_differ_in_their_entries_metadata(self):
        released = single_file(self.released / "SimpleX.app/Contents/app", "jna-*.jar")
        built = single_file(self.built / "SimpleX.app/Contents/app", "jna-*.jar")
        self.assertTrue(build_script.same_jar(released, built))
        with zipfile.ZipFile(built, "a") as z:
            z.writestr("extra.class", b"class")
        self.assertFalse(build_script.same_jar(released, built))

    def test_classpath_names_the_released_jars(self):
        cfg = "[Application]\napp.classpath=$APPDIR/a-12.jar\napp.classpath=$APPDIR/b-34.jar\napp.mainclass=a-12.jar\n"
        self.assertEqual(
            build_script.rename_jars(cfg, {"a-12.jar": "a-56.jar"}),
            "[Application]\napp.classpath=$APPDIR/a-56.jar\napp.classpath=$APPDIR/b-34.jar\napp.mainclass=a-12.jar\n",
        )


class DmgReproduceTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.volumes = {"released.dmg": self.root / "released-volume", "built.dmg": self.root / "built-volume"}
        app_volume(self.volumes["released.dmg"], DEVELOPER_ID)
        app_volume(self.volumes["built.dmg"], None)
        self.released, self.built = self.root / "released.dmg", self.root / "built.dmg"
        self.released.write_bytes(udif(b"volume", b"signature"))
        self.built.write_bytes(udif(b"volume"))

    def tearDown(self):
        self.tmp.cleanup()

    def differences(self, repo="owner/fork", rebuilt=b"volume", codesign=0):
        @contextlib.contextmanager
        def fake_mounted(dmg, mountpoint):
            yield self.volumes[dmg.name]

        def fake_build_dmg(tools, app, java_home, work, target):
            self.assertTrue((app / "Contents/_CodeSignature/CodeResources").is_file())
            target.write_bytes(udif(rebuilt))

        recorder = Recorder()

        def fake_run(args, **kwargs):
            recorder(args, **kwargs)
            return mock.Mock(returncode=codesign)

        with (
            patch_all("mounted", fake_mounted),
            patch_all("build_libdmg"),
            patch_all("build_dmg", fake_build_dmg),
            patch_all("java_home", return_value=pathlib.Path("/jdk")),
            patch_all("run", fake_run),
        ):
            work = pathlib.Path(tempfile.mkdtemp(dir=self.root)) / "work"
            differences = build_script.macos_differences(self.released, self.built, repo, work)
        return differences, recorder.commands

    def test_signed_dmg_of_a_fork_is_verified_without_the_team_requirement(self):
        differences, commands = self.differences()
        self.assertEqual(differences, [])
        self.assertEqual([c[:3] for c in commands], [["codesign", "--verify", "--strict"], ["codesign", "--verify", "--deep"]])
        self.assertFalse(any(arg.startswith("-R=") for command in commands for arg in command))
        self.assertEqual(pathlib.Path(commands[1][-1]).name, "SimpleX.app")

    def test_simplex_chat_dmg_must_have_the_team_developer_id_signature(self):
        differences, commands = self.differences(repo="simplex-chat/simplex-chat")
        self.assertEqual(differences, [])
        self.assertTrue(all(f"-R={build_script.DEVELOPER_ID_REQUIREMENT}" in command for command in commands))
        self.assertIn('leaf[subject.OU] = "5NN7GUYB6T"', build_script.DEVELOPER_ID_REQUIREMENT)
        differences, _ = self.differences(repo="simplex-chat/simplex-chat", codesign=1)
        self.assertEqual(
            differences, [f"codesign doesn't verify the signature of {name}" for name in ("released.dmg", "SimpleX.app")]
        )
        self.released.write_bytes(udif(b"volume"))
        differences, _ = self.differences(repo="simplex-chat/simplex-chat")
        self.assertEqual(differences, ["it is not signed"])

    def test_image_must_be_the_one_the_build_makes_of_the_released_app(self):
        differences, _ = self.differences(rebuilt=b"volumE")
        self.assertEqual(differences, ["it is not the dmg that the build makes of its app"])

    def test_volume_differences_are_reported_without_making_the_image(self):
        (self.volumes["released.dmg"] / ".DS_Store").write_bytes(b"other window")
        differences, commands = self.differences()
        self.assertEqual(differences, [".DS_Store"])
        self.assertEqual(commands, [])

    def test_other_files_are_compared_as_code(self):
        released, built = self.root / "cli", self.root / "cli.built"
        released.write_bytes(codesign(executable(), DEVELOPER_ID))
        built.write_bytes(executable())
        self.assertEqual(build_script.macos_differences(released, built, "a/b", self.root), [])
        built.write_bytes(executable(code=DATA * 3))
        self.assertEqual(build_script.macos_differences(released, built, "a/b", self.root), [build_script.CODE_DIFFERS])

    def test_volume_is_detached_after_a_failure(self):
        recorder = Recorder()
        with patch_all("run", recorder), self.assertRaises(RuntimeError), build_script.mounted(self.released, self.root / "mnt"):
            raise RuntimeError
        self.assertEqual(recorder.commands[0][:3], ["hdiutil", "attach", "-readonly"])
        self.assertEqual(recorder.commands[0][-3:], ["-mountpoint", str(self.root / "mnt"), str(self.released)])
        self.assertEqual(recorder.commands[1], ["hdiutil", "detach", str(self.root / "mnt")])

    def test_reproduce_builds_unsigned_and_compares_the_cli_and_dmg(self):
        results = self.root / "results"
        urls = []

        def fake_download(url, target):
            urls.append(url)
            target.write_bytes(b"cli" if target.suffix != ".dmg" else b"dmg")

        def fake_build(source, out, require_signing):
            self.assertFalse(require_signing)
            (out / "simplex-chat-macos-aarch64").write_bytes(b"cli")
            (out / "simplex-desktop-macos-aarch64.dmg").write_bytes(b"dmg")

        with (
            mock.patch("platform.machine", return_value="arm64"),
            patch_all("download", fake_download),
            patch_all("checkout"),
            patch_all("build_desktop_macos", fake_build),
            patch_all("dmg_differences", return_value=["SimpleX.app/Contents/Info.plist"]) as dmg,
            patch_all("log"),
        ):
            failed = build_script.reproduce_macos("v7.1.0", "owner/fork", self.root, results)
        self.assertEqual(failed, ["simplex-desktop-macos-aarch64.dmg"])
        names = ("simplex-chat-macos-aarch64", "simplex-desktop-macos-aarch64.dmg")
        self.assertEqual(urls, [f"https://github.com/owner/fork/releases/download/v7.1.0/{name}" for name in names])
        built = (results / "macos/prebuilt" / names[1], results / "macos/from-source" / names[1], "owner/fork")
        self.assertEqual(dmg.call_args.args[:3], built)
