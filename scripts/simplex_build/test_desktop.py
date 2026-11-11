import contextlib
import os
import pathlib
import plistlib
import shutil
import tempfile
import unittest
import zipfile
from unittest import mock

from .testing import XCODE_DEVELOPER, Recorder, build_script, patch_all

MINOS_11 = "lib.dylib:\nLoad command 9\n      cmd LC_BUILD_VERSION\n platform 1\n    minos 11.0\n      sdk 26.5\n"


class HostDesktopTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.source, self.out = self.root / "src", self.root / "out"
        self.source.mkdir()
        self.out.mkdir()
        self.build_root = self.root / "fixed"
        (self.source / "simplex-chat.cabal").write_bytes(b"name: simplex-chat\nversion: 7.1.0\n")

    def tearDown(self):
        self.tmp.cleanup()

    @contextlib.contextmanager
    def fixed_build(self):
        """The build directory in the test's directory, on a Mac without the Command Line Tools."""
        with patch_all("BUILD_ROOT", self.build_root), patch_all("COMMAND_LINE_TOOLS_SDK", self.root / "no-command-line-tools"):
            yield

    def fetch(self, work):
        source = work / f"openssl-{build_script.OPENSSL_VERSION}"
        source.mkdir(exist_ok=True)
        (source / "libcrypto.a").write_bytes(b"static")
        return source

    def zip_names(self, name):
        with zipfile.ZipFile(self.out / name) as z:
            return sorted(z.namelist())

    def openssl_on(self, os_name, sys_platform, machine="arm64"):
        recorder = Recorder()
        with (
            patch_all("run", recorder),
            patch_all("fetch_openssl", self.fetch),
            mock.patch.object(build_script.os, "name", os_name),
            mock.patch.object(build_script.sys, "platform", sys_platform),
            mock.patch.object(build_script.platform, "machine", return_value=machine),
        ):
            return build_script.host_openssl(self.root), recorder.commands

    def test_openssl_for_windows_stays_in_its_source_tree(self):
        openssl, commands = self.openssl_on("nt", "win32")
        tree = self.root / f"openssl-{build_script.OPENSSL_VERSION}"
        self.assertEqual(openssl, build_script.OpenSsl(tree / "include", tree))
        self.assertEqual(
            commands[0][2:5], ["mingw64", "--prefix=C:/Program Files/OpenSSL", "--openssldir=C:/Program Files/Common Files/SSL"]
        )
        self.assertNotIn(["make", "install_sw"], commands)

    def test_openssl_for_macos_is_installed(self):
        openssl, commands = self.openssl_on("posix", "darwin", "x86_64")
        self.assertEqual(openssl, build_script.OpenSsl(self.root / "openssl/include", self.root / "openssl/lib"))
        self.assertEqual(commands[0][2:5], ["darwin64-x86_64-cc", f"--prefix={self.root / 'openssl'}", "--openssldir=/etc/ssl"])
        self.assertEqual(commands[-1], ["make", "install_sw"])

    def test_release_openssl_starts_empty_at_one_path_in_the_checkout(self):
        stale = self.source / f"dist-openssl/openssl-{build_script.OPENSSL_VERSION}/libcrypto.a"
        stale.parent.mkdir(parents=True)
        stale.write_bytes(b"earlier build")
        work = build_script.openssl_work(self.source)
        self.assertEqual(work, self.source / "dist-openssl")
        self.assertEqual(list(work.iterdir()), [])

    def test_linux_has_no_pinned_openssl_build(self):
        with self.assertRaises(build_script.BuildError):
            self.openssl_on("posix", "linux")

    def test_openssl_command_writes_cabal_local(self):
        with patch_all("host_openssl", return_value=build_script.OpenSsl(self.root / "o/include", self.root / "o/lib")):
            build_script.build_host_openssl(self.source, self.root)
        self.assertIn(f"extra-lib-dirs: {self.root / 'o/lib'}", (self.source / "cabal.project.local").read_text())

    def test_cabal_local_links_both_packages_with_openssl(self):
        text = build_script.openssl_cabal_local("/o/include", "/o/lib", "-optl-lws2_32")
        self.assertEqual(text.count("extra-include-dirs: /o/include"), 2)
        self.assertIn("package simplexmq\n", text)
        self.assertIn("package direct-sqlcipher\n    extra-include-dirs: /o/include\n    extra-lib-dirs: /o/lib\n    flags: +openssl", text)
        self.assertTrue(text.endswith("package simplex-chat\n    ghc-options: -optl-lws2_32\n"))
        self.assertNotIn("simplex-chat", build_script.openssl_cabal_local("/i", "/l"))

    def test_cli_loading_libcrypto_is_rejected(self):
        with self.assertRaises(build_script.BuildError):
            build_script.check_static_openssl(pathlib.Path("cli"), "\t/opt/homebrew/opt/openssl@3.0/lib/libcrypto.3.dylib\n")
        build_script.check_static_openssl(pathlib.Path("cli"), "\t/usr/lib/libSystem.B.dylib\n")

    def test_apple_signing_needs_all_or_none(self):
        full = {name: "x" for name in build_script.APPLE_SIGNING_ENV}
        with mock.patch.dict(os.environ, full, clear=True):
            self.assertTrue(build_script.apple_signing(require=True))
        partial = {"APPLE_SIMPLEX_SIGNING_KEYCHAIN": "x"}
        with mock.patch.dict(os.environ, partial, clear=True), self.assertRaises(build_script.BuildError):
            build_script.apple_signing(require=False)
        with mock.patch.dict(os.environ, {}, clear=True), self.assertRaises(build_script.BuildError):
            build_script.apple_signing(require=True)
        with mock.patch.dict(os.environ, {}, clear=True), patch_all("warn") as warn:
            self.assertFalse(build_script.apple_signing(require=False))
        self.assertIn("unsigned", warn.call_args.args[0])

    def build_macos(self, signed, cli=b"cli"):
        """build_desktop_macos with its builds standing in, and cli as the CLI; returns its calls, the sign_dmg mock and
        the work directory."""
        libs = self.root / "libs-app"
        libs.mkdir()
        (libs / "libsimplex.dylib").write_bytes(b"lib")
        self.app = self.root / "SimpleX.app"
        self.tools = build_script.DmgTools(self.root / "hfsplus", self.root / "dmg")
        self.keychain = self.root / "k.keychain" if signed else None
        built = self.root / "openssl-build"
        (built / "openssl/lib").mkdir(parents=True)
        (built / "openssl/lib/libcrypto.a").write_bytes(b"static")
        answers = {"list-bin": f"{self.root}/dist/simplex-chat\n", "otool -L": "\t/usr/lib/libSystem.B.dylib\n"}
        recorder = self.recorder = Recorder(answers={**answers, "show-sdk-path": "/X/MacOSX.sdk\n", "otool -l": MINOS_11})
        (self.root / "dist").mkdir()
        (self.root / "dist/simplex-chat").write_bytes(cli)
        calls = []
        self.cabal_locals = []
        self.openssl_dirs = []
        openssl = build_script.OpenSsl(built / "openssl/include", built / "openssl/lib")
        original_cli = build_script.build_host_cli

        def build_cli(source, cabal_local):
            self.cabal_locals.append(cabal_local)
            return original_cli(source, cabal_local)

        def add_vlc(libs, arch, work):
            (libs / "vlc").mkdir()
            (libs / "vlc/libvlccore.dylib").write_bytes(b"/Users/builder/work/vlc")

        def build_app(source, keychain):
            calls.append(("app", keychain))
            (self.app / "Contents").mkdir(parents=True, exist_ok=True)
            (self.app / "Contents/Info.plist").write_bytes(plistlib.dumps({"LSMinimumSystemVersion": "11.0"}))
            shutil.copytree(libs, self.app / build_script.COMPOSE_RESOURCES)
            names = ("CABAL_DIR", "SDKROOT", "MACOSX_DEPLOYMENT_TARGET", "LC_ALL", "TZ")
            self.app_env = {name: os.environ.get(name) for name in names}
            self.app_umask = os.umask(0o077)
            os.umask(self.app_umask)
            self.app_cabal_local = (source / "cabal.project.local").read_text()
            return self.app

        def build_dmg(tools, app, java_home, work, target):
            calls.append(("dmg", tools, app, java_home))
            target.write_bytes(b"dmg")

        with (
            patch_all("build_host_cli", build_cli),
            patch_all("run", recorder),
            patch_all("host_openssl", lambda openssl_dir: self.openssl_dirs.append(openssl_dir) or openssl),
            mock.patch.object(build_script.platform, "machine", return_value="arm64"),
            patch_all("apple_signing", return_value=signed),
            patch_all("require_xcode", return_value=XCODE_DEVELOPER) as xcode,
            patch_all("apple_keychain", lambda work: contextlib.nullcontext(self.keychain)),
            self.fixed_build(),
            patch_all("machine_paths", return_value=["/Users/builder"]),
            patch_all("build_mac_lib", lambda source, arch, database, work: calls.append(("lib", arch)) or libs),
            patch_all("add_vlc_mac", add_vlc),
            patch_all("link_desktop_libs", lambda *args: calls.append(("link", *args[1:]))),
            patch_all("build_app", build_app),
            patch_all("normalize_app", lambda app: calls.append(("normalize", app))),
            patch_all("sign_app", lambda app, keychain: calls.append(("sign", keychain))),
            patch_all("build_libdmg", return_value=self.tools),
            patch_all("java_home", return_value=self.root / "jdk"),
            patch_all("build_dmg", build_dmg),
            patch_all("sign_dmg") as sign_dmg,
            patch_all("check_dmg", lambda dmg, signed, work: calls.append(("check", dmg.name, signed))),
            patch_all("sign_mac_libs") as sign_libs,
        ):
            build_script.build_desktop_macos(self.source, self.out, require_signing=False)
        self.assertEqual(sign_libs.called, signed)
        xcode.assert_called_once_with(build_script.XCODE)
        self.assertFalse(self.build_root.exists())
        return calls, sign_dmg, self.build_root / "work"

    def test_macos_unsigned_build(self):
        calls, sign_dmg, work = self.build_macos(signed=False)
        copy = self.build_root / "simplex-chat"
        self.assertEqual(self.openssl_dirs, [copy / "dist-openssl"])
        self.assertIn(f"extra-lib-dirs: {copy / 'dist-openssl/openssl-static'}", self.cabal_locals[0])
        linker = self.build_root / "work/cc-without-rpaths"
        linking = f"package *\n    ghc-options: -fno-use-rpaths -pgml {linker}\n"
        self.assertTrue(self.cabal_locals[0].endswith(linking))
        self.assertTrue(self.app_cabal_local.endswith(linking))
        self.assertFalse((self.source / "cabal.project.local").exists())
        self.assertEqual(
            calls,
            [
                ("lib", "aarch64"),
                ("link", "mac-aarch64", "macos-arm64"),
                ("app", None),
                ("normalize", self.app),
                ("sign", None),
                ("dmg", self.tools, self.app, self.root / "jdk"),
                ("check", "simplex-desktop-macos-aarch64.dmg", False),
            ],
        )
        sign_dmg.assert_not_called()
        self.assertEqual((self.out / "simplex-desktop-macos-aarch64.dmg").read_bytes(), b"dmg")
        self.assertIn(f"extra-lib-dirs: {self.root / 'openssl-build/openssl/lib'}", self.app_cabal_local)
        cabal_dir = str(self.build_root / "cabal")
        expected = {"CABAL_DIR": cabal_dir, "SDKROOT": "/X/MacOSX.sdk", "MACOSX_DEPLOYMENT_TARGET": "11.0", "LC_ALL": "en_US.UTF-8"}
        self.assertEqual(self.app_env, {**expected, "TZ": "UTC"})
        self.assertEqual(self.app_umask, 0o022)
        self.assertNotIn(cabal_dir, os.environ.values())
        self.assertEqual(
            sorted(p.name for p in self.out.iterdir()),
            ["simplex-chat-libs-macos-aarch64.zip", "simplex-chat-macos-aarch64", "simplex-desktop-macos-aarch64.dmg"],
        )
        self.assertEqual(self.zip_names("simplex-chat-libs-macos-aarch64.zip"), ["libs/libsimplex.dylib"])
        checked = [c[-1] for c in self.recorder.commands if c[:2] == ["otool", "-l"]]
        resources = self.app / build_script.COMPOSE_RESOURCES
        self.assertEqual(checked, [str(self.out / "simplex-chat-macos-aarch64"), *[str(resources / "libsimplex.dylib")] * 2])

    def test_macos_build_with_a_path_of_the_machine_fails_after_its_files_are_written(self):
        with self.assertRaises(build_script.BuildError) as raised:
            self.build_macos(signed=False, cli=b"cli /Users/builder/.cabal/bin")
        self.assertIn("simplex-chat-macos-aarch64: /Users/builder", str(raised.exception))
        self.assertNotIn("vlc", str(raised.exception))
        self.assertEqual((self.out / "simplex-desktop-macos-aarch64.dmg").read_bytes(), b"dmg")
        self.assertFalse(self.build_root.exists())

    def test_macos_signed_build_signs_the_app_and_notarizes_the_dmg(self):
        calls, sign_dmg, work = self.build_macos(signed=True)
        self.assertIn(("app", self.keychain), calls)
        self.assertIn(("sign", self.keychain), calls)
        sign_dmg.assert_called_once_with(work / "simplex-desktop-macos-aarch64.dmg", self.keychain, work)
        self.assertEqual(calls[-1], ("check", "simplex-desktop-macos-aarch64.dmg", True))
        self.assertEqual((self.out / "simplex-desktop-macos-aarch64.dmg").read_bytes(), b"dmg")

    def test_macos_libs_are_built_in_the_copy_and_installed_in_the_checkout(self):
        released = self.root / "libs-app"
        released.mkdir()
        (released / "libsimplex.dylib").write_bytes(b"lib")
        stale = build_script.desktop_libs(self.source, "mac-aarch64")
        stale.mkdir(parents=True)
        (stale / "libold.dylib").write_bytes(b"old")
        calls = []

        def release(source, openssl, database, keychain, work, out):
            calls.append(("release", source, database, keychain, work, out))
            return released

        with (
            patch_all("run", Recorder(answers={"show-sdk-path": "/X/MacOSX.sdk\n", "otool -l": MINOS_11})),
            patch_all("host_openssl", return_value=build_script.OpenSsl(self.root, self.root)),
            mock.patch.object(build_script.platform, "machine", return_value="arm64"),
            patch_all("apple_signing", return_value=False),
            patch_all("require_xcode", return_value=XCODE_DEVELOPER),
            self.fixed_build(),
            patch_all("release_macos_libs", release),
            patch_all("link_desktop_libs", lambda *args: calls.append(("link", *args))),
        ):
            build_script.build_macos_libs(self.source, build_script.Database.POSTGRES, self.out, require_signing=False)
        copy = self.build_root / "simplex-chat"
        self.assertEqual(
            calls,
            [
                ("release", copy, build_script.Database.POSTGRES, None, self.build_root / "work", self.out),
                ("link", self.source, "mac-aarch64", "macos-arm64"),
            ],
        )
        self.assertEqual(sorted(p.name for p in stale.iterdir()), ["libsimplex.dylib"])
        self.assertFalse(self.build_root.exists())

    def test_macos_cli_linking_libcrypto_fails(self):
        recorder = Recorder(answers={"list-bin": f"{self.root}/cli\n", "otool -L": "\t@rpath/libcrypto.3.dylib\n"})
        with (
            patch_all("run", recorder),
            patch_all("host_openssl", return_value=build_script.OpenSsl(self.root, self.root)),
            mock.patch.object(build_script.platform, "machine", return_value="x86_64"),
            patch_all("apple_signing", return_value=False),
            patch_all("require_xcode", return_value=XCODE_DEVELOPER),
            self.fixed_build(),
            patch_all("static_libcrypto", return_value=self.root),
            self.assertRaises(build_script.BuildError),
        ):
            build_script.build_desktop_macos(self.source, self.out, require_signing=False)

    def windows_build(self, msi_data=b"msi"):
        libs = self.root / "libs-app"
        libs.mkdir()
        (libs / "libsimplex.dll").write_bytes(b"lib")
        (self.root / "dist").mkdir()
        (self.root / "dist/simplex-chat.exe").write_bytes(b"cli")
        recorder = Recorder(answers={"list-bin": f"{self.root}/dist/simplex-chat.exe\n", "objdump": "\tDLL Name: KERNEL32.dll\n"})
        seen = {}

        def fake_run(args, **kwargs):
            args = [str(a) for a in args]
            if args[0] == "dlltool":
                (kwargs["cwd"] / "libsimplex.lib").write_bytes(b"import lib")
            return recorder(args, **kwargs)

        calls = []

        def fake_package_msi(source, work):
            calls.append(("msi", source))
            seen["env"] = {name: os.environ.get(name) for name in ("CABAL_DIR", "TMP", "TEMP")}
            app = source / build_script.DESKTOP_RELEASE_DIR / "app/SimpleX"
            app.mkdir(parents=True)
            (app / "SimpleX.exe").write_bytes("launcher".encode("utf-16-le"))
            msi = work / "SimpleX-7.1.0.msi"
            msi.write_bytes(msi_data)
            return msi

        openssl_dirs = []

        def fake_openssl(work):
            openssl_dirs.append(work)
            return build_script.OpenSsl(self.fetch(work) / "include", self.fetch(work))

        with (
            patch_all("WINDOWS_BUILD_ROOT", self.build_root),
            patch_all("run", fake_run),
            patch_all("host_openssl", fake_openssl),
            patch_all("build_windows_lib", lambda source, openssl: calls.append(("lib", openssl.lib.name)) or libs),
            patch_all("add_vlc_windows", lambda libs, work: (libs / "vlc").mkdir()),
            patch_all("link_desktop_libs", lambda *args: calls.append(("link", *args[1:]))),
            patch_all("package_msi", fake_package_msi),
        ):
            build_script.build_desktop_windows(self.source, self.out)
        return calls, openssl_dirs, seen

    def test_windows_build_runs_in_the_fixed_directory(self):
        calls, openssl_dirs, seen = self.windows_build()
        copy = self.build_root / "simplex-chat"
        lib = ("lib", f"openssl-{build_script.OPENSSL_VERSION}")
        self.assertEqual(calls, [lib, ("link", "windows-x86_64", "windows-x64"), ("msi", copy)])
        self.assertEqual(openssl_dirs, [copy / "dist-openssl"])
        self.assertEqual(seen["env"], build_script.windows_build_env(build_script.BuildRoot.at(self.build_root)))
        self.assertFalse(self.build_root.exists())
        self.assertEqual((self.out / "simplex-desktop-windows-x86_64.msi").read_bytes(), b"msi")
        self.assertEqual(
            sorted(p.name for p in self.out.iterdir()),
            ["simplex-chat-libs-windows-x86_64.zip", "simplex-chat-windows-x86-64", "simplex-desktop-windows-x86_64.msi"],
        )
        self.assertEqual(self.zip_names("simplex-chat-libs-windows-x86_64.zip"), ["libs/libsimplex.dll", "libs/libsimplex.lib"])

    def test_windows_build_fails_on_paths_of_the_checkout(self):
        checkout = build_script.PureWindowsPath(self.source).as_posix()
        with self.assertRaisesRegex(build_script.BuildError, "SimpleX-7.1.0.msi: "):
            self.windows_build(msi_data=b"Property " + checkout.encode() + b"/dist-openssl")
        self.assertFalse(self.build_root.exists())

    def test_cli_cabal_local_is_static_on_windows(self):
        cabal_locals = []

        def fake_cli(source, cabal_local):
            cabal_locals.append(cabal_local)
            raise build_script.BuildError("stop")

        with (
            patch_all("run"),
            patch_all("host_openssl", lambda work: build_script.OpenSsl(self.fetch(work) / "include", self.fetch(work))),
            patch_all("build_host_cli", fake_cli),
            self.assertRaises(build_script.BuildError),
        ):
            build_script.build_desktop_windows(self.source, self.out)
        self.assertIn("/openssl-static\n", cabal_locals[0])
        self.assertIn("ghc-options: -optl-lws2_32 -optl-lgdi32 -optl-lcrypt32 -optl-Wl,--no-insert-timestamp\n", cabal_locals[0])

    def test_host_cli_build_removes_dist_newstyle_instead_of_cabal_clean(self):
        pack = self.source / "dist-newstyle/src/zip-8e3e/.git/objects/pack/p.rev"
        pack.parent.mkdir(parents=True)
        pack.write_bytes(b"rev")
        pack.chmod(0o444)
        recorder = Recorder(answers={"list-bin": "/dist/simplex-chat\n"})
        with patch_all("run", recorder):
            cli = build_script.build_host_cli(self.source, "ignore-project: False\n")
        self.assertFalse((self.source / "dist-newstyle").exists())
        self.assertEqual(recorder.commands, [["cabal", "update"], ["cabal", "build", "-j"], ["cabal", "list-bin", "exe:simplex-chat"]])
        self.assertEqual(cli, pathlib.Path("/dist/simplex-chat"))

    def checkouts_at_build(self, host):
        checkout = self.source / "dist-newstyle/src/direct-sq_-fd06"
        seen = []

        def fake_run(args, **kwargs):
            args = [str(a) for a in args]
            if args == ["cabal", "update"]:
                checkout.mkdir(parents=True)
            elif args[:2] in (["cabal", "build"], ["cabal", "list-bin"]):
                seen.append(checkout.exists())
                checkout.mkdir(parents=True, exist_ok=True)
            return mock.Mock(returncode=0, stdout="/dist/simplex-chat\n")

        with patch_all("run", fake_run), patch_all("host_platform", return_value=host):
            build_script.build_host_cli(self.source, "")
        return seen

    def test_windows_cabal_commands_start_without_earlier_checkouts(self):
        self.assertEqual(self.checkouts_at_build(build_script.Platform.WINDOWS), [False, False])
        shutil.rmtree(self.source / "dist-newstyle")
        self.assertEqual(self.checkouts_at_build(build_script.Platform.MACOS), [True, True])

    def test_libs_zip_is_the_same_whenever_the_libraries_were_built(self):
        libs = self.root / "libs"
        libs.mkdir()
        (libs / "libsimplex.so").write_bytes(b"simplex" * 100)
        (libs / "libHSbase.so").write_bytes(b"base")
        extra = self.root / "libsimplex.lib"
        extra.write_bytes(b"import library")
        build_script.zip_desktop_libs(libs, [extra], self.out / "first.zip")
        for path in [*libs.iterdir(), extra]:
            os.utime(path, (1_900_000_000, 1_900_000_000))
        build_script.zip_desktop_libs(libs, [extra], self.out / "second.zip")
        self.assertEqual((self.out / "first.zip").read_bytes(), (self.out / "second.zip").read_bytes())
        self.assertEqual(self.zip_names("first.zip"), ["libs/libHSbase.so", "libs/libsimplex.lib", "libs/libsimplex.so"])
        with zipfile.ZipFile(self.out / "first.zip") as z:
            self.assertEqual(z.read("libs/libsimplex.so"), b"simplex" * 100)
            self.assertEqual({info.date_time for info in z.infolist()}, {(2024, 1, 1, 0, 0, 0)})

    def test_read_only_cabal_checkouts_are_removed(self):
        checkouts = [
            "simplexmq-abc/.git/modules/blst/objects/pack/p.pack",
            "hs-jpeg-t_-d0a4/.git/modules/jpeg-turbo/cbits/mozjpeg/objects/pack/p.pack",
            "JuicyPixe_-e009/.git/modules/cbits/stb/objects/pack/p.pack",
        ]
        for checkout in checkouts:
            pack = self.source / "dist-newstyle/src" / checkout
            pack.parent.mkdir(parents=True)
            pack.write_bytes(b"pack")
            pack.chmod(0o444)
            pack.parent.chmod(0o555)
        keep = self.source / "dist-newstyle/src/aeson-1"
        keep.mkdir()
        tarball = self.source / "dist-newstyle/src/direct-sq_-fd06-direct-sqlcipher-2.3.28.tar.gz"
        tarball.write_bytes(b"tar")
        build_script.remove_cabal_sources(self.source)
        self.assertFalse(tarball.exists())
        self.assertEqual(sorted(p.name for p in (self.source / "dist-newstyle/src").iterdir()), ["aeson-1"])

    def test_skiko_runtime_jar_leaves_the_app_and_its_classpath(self):
        app = self.root / "SimpleX/app"
        app.mkdir(parents=True)
        for jar in ("desktop-jvm-7.1.7-854b.jar", "skiko-awt-runtime-windows-x64-0.9.4.2-1f63.jar", "skiko-awt-0.9.4.2-aa36.jar"):
            (app / jar).write_bytes(b"jar")
        (app / "SimpleX.cfg").write_text(
            "[Application]\r\n"
            "app.classpath=$APPDIR\\desktop-jvm-7.1.7-854b.jar\r\n"
            "app.classpath=$APPDIR\\skiko-awt-runtime-windows-x64-0.9.4.2-1f63.jar\r\n"
            "app.classpath=$APPDIR\\skiko-awt-0.9.4.2-aa36.jar\r\n"
        )
        build_script.remove_skiko_runtime(app)
        self.assertEqual(sorted(p.name for p in app.iterdir()), ["SimpleX.cfg", "desktop-jvm-7.1.7-854b.jar", "skiko-awt-0.9.4.2-aa36.jar"])
        self.assertEqual(
            (app / "SimpleX.cfg").read_text().splitlines(),
            ["[Application]", "app.classpath=$APPDIR\\desktop-jvm-7.1.7-854b.jar", "app.classpath=$APPDIR\\skiko-awt-0.9.4.2-aa36.jar"],
        )
