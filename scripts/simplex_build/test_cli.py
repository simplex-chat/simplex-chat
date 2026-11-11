import io
import os
import pathlib
import platform
import tempfile
import unittest
import zipfile
from unittest import mock

from .testing import Arch, Recorder, Target, build_script, patch_all


class ParseArgsTest(unittest.TestCase):
    def test_reproduce_defaults_to_desktop_and_both_arches(self):
        self.assertEqual(
            build_script.parse_args(["reproduce", "v7.1.0"]),
            build_script.Reproduce("v7.1.0", "simplex-chat/simplex-chat", (Target.DESKTOP,), (Arch.AARCH64, Arch.ARMV7A), False),
        )

    def test_reproduce_android_with_options(self):
        self.assertEqual(
            build_script.parse_args(["reproduce", "v7.1.0", "android", "--arches", "armv7a", "--nix-cache"]),
            build_script.Reproduce("v7.1.0", "simplex-chat/simplex-chat", (Target.ANDROID,), (Arch.ARMV7A,), True),
        )

    def test_reproduce_a_release_of_a_fork(self):
        command = build_script.parse_args(["reproduce", "v7.1.99-test", "--repo", "sh-agents/simplex-chat"])
        self.assertEqual(command.repo, "sh-agents/simplex-chat")
        for repo in ("simplex-chat", "https://github.com/a/b", "a/b/c", "a/b c"):
            with mock.patch("sys.stderr"), self.assertRaises(SystemExit):
                build_script.parse_args(["reproduce", "v7.1.0", "--repo", repo])

    def test_reproduce_rejects_unknown_arch_and_target(self):
        for argv in (["reproduce", "v7.1.0", "android", "--arches", "x86"], ["reproduce", "v7.1.0", "web"], ["v7.1.0"], []):
            with mock.patch("sys.stderr"), self.assertRaises(SystemExit):
                build_script.parse_args(argv)

    def test_build_commands(self):
        out, here = pathlib.Path("o"), pathlib.Path(".")
        self.assertEqual(
            build_script.parse_args(["build", "apk", "armv7a", "--out", "o", "--nix-cache"]),
            build_script.BuildApk(Arch.ARMV7A, out, here, True),
        )
        self.assertEqual(
            build_script.parse_args(["build", "aab", "--libs", "l", "--out", "o", "--source", "s"]),
            build_script.BuildBundle(pathlib.Path("l"), out, pathlib.Path("s")),
        )
        self.assertEqual(
            build_script.parse_args(["build", "desktop", "linux", "24.04", "--version", "7.1.0", "--out", "o"]),
            build_script.BuildDesktopLinux(build_script.linux_image("24.04", platform.machine()), "7.1.0", out, here),
        )
        self.assertEqual(
            build_script.parse_args(["build", "libs", "linux", "--database", "postgres", "--host", "--out", "o"]),
            build_script.BuildLinuxLibs(
                build_script.linux_image("22.04", platform.machine()),
                build_script.Database.POSTGRES,
                "dist-newstyle",
                build_script.LibsBuilder(on_host=True),
                out,
                here,
            ),
        )
        self.assertEqual(
            build_script.parse_args(["build", "libs", "linux", "--container", "builder", "--dist-dir", "dist-lib", "--out", "o"]).builder,
            build_script.LibsBuilder(container="builder"),
        )
        self.assertEqual(
            build_script.parse_args(["build", "desktop", "macos", "--require-signing", "--out", "o"]),
            build_script.BuildDesktopMacos(out, here, True),
        )
        self.assertEqual(
            build_script.parse_args(["build", "desktop", "windows", "--out", "o"]), build_script.BuildDesktopWindows(out, here)
        )
        self.assertEqual(
            build_script.parse_args(["build", "libs", "macos", "--require-signing", "--out", "o"]),
            build_script.BuildMacosLibs(build_script.Database.SQLITE, out, here, True),
        )
        self.assertEqual(build_script.parse_args(["build", "libs", "windows", "--out", "o"]), build_script.BuildWindowsLibs(out, here))
        self.assertEqual(
            build_script.parse_args(["build", "openssl", "windows", "--out", "o"]),
            build_script.BuildOpenssl(build_script.Platform.WINDOWS, out, here),
        )
        self.assertEqual(
            build_script.parse_args(["build", "xcarchive", "--art", "a", "--out", "o", "--install-nix"]),
            build_script.BuildIosArchive(pathlib.Path("a"), out, here, True),
        )

    def test_build_commands_reject_missing_or_unknown_arguments(self):
        self.assertIsNone(build_script.parse_args(["build", "xcarchive", "--out", "o"]).art)
        for argv in (
            ["build", "aab", "--out", "o"],
            ["build", "desktop", "linux", "20.04", "--version", "7.1.0", "--out", "o"],
            ["build", "desktop", "linux", "22.04", "--out", "o"],
            ["build", "desktop", "--out", "o"],
            ["build", "openssl", "linux", "--out", "o"],
            ["build", "libs", "macos", "--host", "--out", "o"],
            ["build", "apk", "x86", "--out", "o"],
        ):
            with mock.patch("sys.stderr"), self.assertRaises(SystemExit):
                build_script.parse_args(argv)


class PrebuiltLibsTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_android_libs_are_built_with_nix_on_this_machine(self):
        def fake_run(args, **kwargs):
            args = [str(a) for a in args]
            link = pathlib.Path(args[args.index("--out-link") + 1])
            link.mkdir()
            lib = "libsimplex" if "lib:simplex-chat" in args[-1] else "libsupport"
            (link / f"pkg-aarch64-android-{lib}.zip").write_bytes(lib.encode())
            return recorder(args, **kwargs)

        recorder = Recorder()
        with patch_all("run", fake_run), patch_all("install_nix") as install:
            build_script.build_android_libs(self.root, Arch.AARCH64, self.root, install=False)
        install.assert_not_called()
        self.assertEqual(
            [c[-1] for c in recorder.commands],
            [".#hydraJobs.x86_64-linux.aarch64-android:lib:simplex-chat", ".#hydraJobs.x86_64-linux.aarch64-android:lib:support"],
        )
        self.assertEqual(
            sorted(p.name for p in self.root.glob("*.zip")), ["pkg-aarch64-android-libsimplex.zip", "pkg-aarch64-android-libsupport.zip"]
        )

    def test_android_libs_are_installed_for_their_abi(self):
        for lib in build_script.ANDROID_LIBS:
            with zipfile.ZipFile(self.root / f"pkg-aarch64-android-{lib}.zip", "w") as z:
                z.writestr(f"{lib}.so", lib)
        build_script.install_android_libs(self.root, Arch.AARCH64, self.root)
        libs = self.root / build_script.ANDROID_LIBS_DIR / "arm64-v8a"
        self.assertEqual(sorted(p.name for p in libs.iterdir()), ["libsimplex.so", "libsupport.so"])

    def test_linux_libs_are_installed_and_linked(self):
        package = self.root / "simplex-chat-libs-linux-x86_64.zip"
        with zipfile.ZipFile(package, "w") as z:
            z.writestr("libs/libsimplex.so", "lib")
            z.writestr("other/readme", "x")
        with mock.patch.object(build_script.platform, "machine", return_value="x86_64"):
            build_script.install_linux_libs(self.root, package)
        libs = self.root / build_script.DESKTOP_LIBS_DIR / "linux-x86_64"
        self.assertEqual([p.name for p in libs.iterdir()], ["libsimplex.so"])
        self.assertTrue((self.root / "apps/multiplatform/build/links/linux-x64").is_symlink())

    def test_test_commands(self):
        self.assertEqual(
            build_script.parse_args(["test", "desktop", "simplex-desktop-x86_64.AppImage", "--out", "screens"]),
            build_script.TestRelease("desktop", pathlib.Path("simplex-desktop-x86_64.AppImage"), pathlib.Path("screens")),
        )
        self.assertEqual(
            build_script.parse_args(["test", "desktop", "app.deb", "--out", "o", "--database", "old", "--cli", "simplex-chat"]),
            build_script.TestRelease(
                "desktop", pathlib.Path("app.deb"), pathlib.Path("o"), pathlib.Path("old"), pathlib.Path("simplex-chat")
            ),
        )
        self.assertEqual(
            build_script.parse_args(["test", "apk", "simplex-armv7a.apk", "--out", "o", "--database", "old"]),
            build_script.TestRelease("apk", pathlib.Path("simplex-armv7a.apk"), pathlib.Path("o"), pathlib.Path("old")),
        )
        upgrade = ["test", "apk", "simplex-aarch64.apk", "--out", "o", "--upgrade-from-previous", "--cli", "simplex-chat"]
        self.assertEqual(
            build_script.parse_args(upgrade),
            build_script.TestRelease(
                "apk", pathlib.Path("simplex-aarch64.apk"), pathlib.Path("o"), cli=pathlib.Path("simplex-chat"), upgrade=True
            ),
        )
        self.assertEqual(
            build_script.parse_args(["test", "old-database", "--out", "old"]), build_script.MakeOldDatabase(pathlib.Path("old"))
        )
        for argv in (
            ["test", "desktop", "app.deb"],
            ["test", "ios", "x", "--out", "o"],
            ["test", "cli", "x", "--out", "o", "--cli", "y"],
            ["test", "apk", "app.apk", "--out", "o", "--upgrade-from-previous", "--database", "old"],
        ):
            with mock.patch("sys.stderr"), self.assertRaises(SystemExit):
                build_script.parse_args(argv)

    def test_desktop_upgrade_command_needs_the_cli_and_no_database(self):
        self.assertEqual(
            build_script.parse_args(["test", "desktop", "app.msi", "--out", "o", "--upgrade-from-previous", "--cli", "simplex-chat"]),
            build_script.TestRelease("desktop", pathlib.Path("app.msi"), pathlib.Path("o"), None, pathlib.Path("simplex-chat"), True),
        )
        upgrade = ["test", "desktop", "app.msi", "--out", "o", "--upgrade-from-previous"]
        for argv in (upgrade, [*upgrade, "--cli", "c", "--database", "old"], ["test", "cli", "x", "--out", "o", "--upgrade-from-previous"]):
            with mock.patch("sys.stderr"), self.assertRaises(SystemExit):
                build_script.parse_args(argv)

    def test_desktop_test_commands_go_to_their_tests(self):
        with tempfile.TemporaryDirectory() as tmp:
            out = pathlib.Path(tmp).resolve()
            with (
                patch_all("require_tools"),
                patch_all("test_desktop") as fresh,
                patch_all("test_desktop_upgrade") as upgrade,
            ):
                build_script.test(build_script.TestRelease("desktop", pathlib.Path("app.dmg"), out, cli=pathlib.Path("cli"), upgrade=True))
                build_script.test(build_script.TestRelease("desktop", pathlib.Path("app.dmg"), out))
        upgrade.assert_called_once_with(pathlib.Path("app.dmg").resolve(), out, pathlib.Path("cli").resolve())
        fresh.assert_called_once_with(pathlib.Path("app.dmg").resolve(), out, None, None)

    def test_install_commands(self):
        here = pathlib.Path(".")
        self.assertEqual(
            build_script.parse_args(["install", "libs", "android", "core", "--arch", "armv7a"]),
            build_script.InstallLibs("android", pathlib.Path("core"), Arch.ARMV7A, here),
        )
        self.assertEqual(
            build_script.parse_args(["install", "libs", "ios", "pkg.zip"]),
            build_script.InstallLibs("ios", pathlib.Path("pkg.zip"), Arch.AARCH64, here),
        )
        self.assertEqual(
            build_script.parse_args(["build", "libs", "android", "--install-nix", "--out", "o"]),
            build_script.BuildAndroidLibs(Arch.AARCH64, pathlib.Path("o"), here, True),
        )
        self.assertEqual(
            build_script.parse_args(["build", "libs", "ios-sim", "--out", "o"]), build_script.BuildIosSimLibs(pathlib.Path("o"), here)
        )


class MainTest(unittest.TestCase):
    def test_errors_exit_with_1(self):
        with (
            patch_all("reproduce", side_effect=build_script.BuildError("boom")),
            mock.patch.dict(os.environ, {}, clear=True),
            mock.patch("sys.stderr", new_callable=io.StringIO) as err,
        ):
            self.assertEqual(build_script.main(["reproduce", "v7.1.0"]), 1)
        self.assertIn("error: boom", err.getvalue())

    def test_characters_the_console_can_not_print_are_escaped(self):
        console = io.TextIOWrapper(io.BytesIO(), encoding="cp1252")
        with (
            patch_all("execute", side_effect=lambda command: build_script.log("/\u4840\u3f3f differs")),
            mock.patch("sys.stdout", console),
        ):
            self.assertEqual(build_script.main(["compare", "a", "b"]), 0)
        console.flush()
        self.assertIn(b"/\\u4840\\u3f3f differs", console.buffer.getvalue())

    def test_build_commands_go_to_build(self):
        with patch_all("build") as build:
            self.assertEqual(build_script.main(["build", "aab", "--libs", "l", "--out", "o"]), 0)
        self.assertIsInstance(build.call_args.args[0], build_script.BuildBundle)

    def test_apk_test_gets_the_upgrade_and_the_peer_cli(self):
        with tempfile.TemporaryDirectory() as tmp, patch_all("test_apk") as test_apk:
            out = pathlib.Path(tmp) / "screens"
            argv = ["test", "apk", "simplex-aarch64.apk", "--out", str(out), "--upgrade-from-previous", "--cli", "simplex-chat"]
            self.assertEqual(build_script.main(argv), 0)
            self.assertTrue(out.is_dir())
        self.assertEqual(
            test_apk.call_args.args,
            (pathlib.Path("simplex-aarch64.apk").resolve(), out.resolve(), None, True, pathlib.Path("simplex-chat").resolve()),
        )

    def test_missing_tools_are_named(self):
        with mock.patch.object(build_script.shutil, "which", return_value=None), self.assertRaises(build_script.BuildError) as e:
            build_script.require_tools(["git", "docker"])
        self.assertIn("git, docker", str(e.exception))

    def test_unreachable_docker_daemon(self):
        with (
            mock.patch.object(build_script.shutil, "which", return_value="/usr/bin/docker"),
            mock.patch.object(build_script.subprocess, "run", return_value=mock.Mock(returncode=1)),
            self.assertRaises(build_script.BuildError) as e,
        ):
            build_script.require_tools(["docker"])
        self.assertIn("daemon", str(e.exception))

    def test_windows_desktop_needs_powershell_for_the_msi(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = build_script.BuildDesktopWindows(pathlib.Path(tmp), pathlib.Path("."))
            with (
                patch_all("require_tools") as require,
                patch_all("host_platform", return_value=build_script.Platform.WINDOWS),
                patch_all("build_desktop_windows") as build,
                patch_all("write_sha256sums"),
            ):
                build_script.build(command)
        self.assertIn("powershell", require.call_args.args[0])
        build.assert_called_once()

    def test_ios_needs_nix_unless_installing_it(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = build_script.BuildIosArchive(pathlib.Path("a"), pathlib.Path(tmp), pathlib.Path("."), False)
            with (
                patch_all("require_tools"),
                patch_all("host_platform", return_value=build_script.Platform.MACOS),
                patch_all("NIX_PROFILE", pathlib.Path("/nonexistent")),
                self.assertRaises(build_script.BuildError) as e,
            ):
                build_script.build(command)
        self.assertIn("--install-nix", str(e.exception))


class ReproduceTest(unittest.TestCase):
    def run_reproduce(self, command, host, failed=()):
        with (
            patch_all("host_platform", return_value=host),
            patch_all("require_tools") as require,
            patch_all("clone", return_value=pathlib.Path("src")) as clone,
            patch_all("reproduce_macos", return_value=list(failed)) as macos,
            patch_all("reproduce_windows", return_value=[]) as windows,
            patch_all("reproduce_ios", return_value=[]) as ios,
            patch_all("reproduce_desktop", return_value=[]) as desktop,
            patch_all("log"),
            mock.patch.object(build_script.Path, "cwd", return_value=pathlib.Path("/r")),
        ):
            build_script.reproduce(command)
        return require, clone, {"macos": macos, "windows": windows, "ios": ios, "desktop": desktop}

    def test_reproduce_parses_the_targets_of_other_platforms_and_their_options(self):
        command = build_script.parse_args(["reproduce", "v7.1.0", "macos", "ios", "--art", "art", "--install-nix", "--repo", "a/b"])
        self.assertEqual(command.targets, (Target.MACOS, Target.IOS))
        self.assertEqual((command.art, command.install_nix, command.repo), (pathlib.Path("art"), True, "a/b"))

    def test_macos_and_ios_reproduce_on_macos_with_their_tools(self):
        command = build_script.Reproduce("v7.1.0", "a/fork", (Target.MACOS, Target.IOS), (), False, None, True)
        require, clone, called = self.run_reproduce(command, build_script.Platform.MACOS)
        tools = require.call_args.args[0]
        self.assertEqual(tools[0], "git")
        self.assertIn("hdiutil", tools)
        self.assertIn("xcodebuild", tools)
        self.assertEqual(len(tools), len(set(tools)))
        self.assertEqual(clone.call_args.args[1], "a/fork")
        self.assertEqual(called["macos"].call_args.args[:2], ("v7.1.0", "a/fork"))
        self.assertEqual(called["macos"].call_args.args[3], pathlib.Path("/r/v7.1.0-fork"))
        self.assertEqual(called["ios"].call_args.args[2:4], (None, True))
        called["windows"].assert_not_called()
        called["desktop"].assert_not_called()

    def test_each_target_runs_only_on_its_platform(self):
        elsewhere = ((Target.WINDOWS, "macos"), (Target.MACOS, "linux"), (Target.DESKTOP, "windows"), (Target.IOS, "linux"))
        for target, platform_name in elsewhere:
            command = build_script.Reproduce("v7.1.0", "a/b", (target,), (), False)
            with self.subTest(target=target), self.assertRaisesRegex(build_script.BuildError, "this build runs on"):
                self.run_reproduce(command, build_script.Platform(platform_name))

    def test_ios_needs_nix_unless_installing_it(self):
        command = build_script.Reproduce("v7.1.0", "a/b", (Target.IOS,), (), False)
        with (
            mock.patch.object(build_script, "NIX_PROFILE", pathlib.Path("/no/nix")),
            self.assertRaisesRegex(build_script.BuildError, "--install-nix"),
        ):
            self.run_reproduce(command, build_script.Platform.MACOS)

    def test_files_that_are_not_reproduced_fail_the_run(self):
        command = build_script.Reproduce("v7.1.0", "a/b", (Target.MACOS,), (), False)
        with self.assertRaisesRegex(build_script.BuildError, "simplex-desktop-macos-aarch64.dmg"):
            self.run_reproduce(command, build_script.Platform.MACOS, ["simplex-desktop-macos-aarch64.dmg"])


class CompareTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def file(self, name, data):
        (self.root / name).write_bytes(data)
        return self.root / name

    def test_compare_parses_both_files_and_the_repository(self):
        self.assertEqual(
            build_script.parse_args(["compare", "r.dmg", "b.dmg", "--repo", "a/b"]),
            build_script.Compare(pathlib.Path("r.dmg"), pathlib.Path("b.dmg"), "a/b"),
        )
        self.assertEqual(build_script.parse_args(["compare", "r", "b"]).repo, "simplex-chat/simplex-chat")

    def test_files_are_compared_by_their_format(self):
        msi, pe, other = (self.file(name, data) for name, data in (("a.msi", bytes.fromhex("d0cf11e0a1b11ae1")), ("a", b"MZ"), ("b", b"x")))
        dmg = self.file("a.dmg", b"koly")
        work = self.root / "work"
        with (
            patch_all("windows_differences", return_value=["w"]),
            patch_all("macos_differences", return_value=["m"]) as macos,
            patch_all("host_platform", return_value=build_script.Platform.MACOS),
            patch_all("require_tools") as require,
        ):
            self.assertEqual(build_script.release_file_differences(msi, other, "a/b", work), ["w"])
            self.assertEqual(build_script.release_file_differences(pe, other, "a/b", work), ["w"])
            self.assertEqual(build_script.release_file_differences(other, pe, "a/b", work), ["m"])
            self.assertEqual(build_script.release_file_differences(dmg, dmg, "a/b", work), ["m"])
        self.assertEqual(macos.call_args.args, (dmg, dmg, "a/b", work))
        self.assertIn("hdiutil", require.call_args.args[0])
        with patch_all("host_platform", return_value=build_script.Platform.LINUX), self.assertRaises(build_script.BuildError):
            build_script.release_file_differences(dmg, dmg, "a/b", work)

    def test_compare_fails_on_any_difference(self):
        released, built = self.file("cli", b"code"), self.file("cli.built", b"code")
        with patch_all("log") as log:
            build_script.compare(build_script.Compare(released, built, "a/b"))
            self.assertIn("with its signature", log.call_args.args[0])
            built.write_bytes(b"other")
            with self.assertRaisesRegex(build_script.BuildError, "other than by its signature"):
                build_script.compare(build_script.Compare(released, built, "a/b"))
        self.assertIn(build_script.CODE_DIFFERS, log.call_args.args[0])
