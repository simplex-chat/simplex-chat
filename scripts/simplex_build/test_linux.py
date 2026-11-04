import contextlib
import os
import pathlib
import stat
import tempfile
import unittest
import zipfile
from unittest import mock

from .testing import (
    JAMMY,
    JAMMY_ARM,
    NOBLE,
    REPO_ROOT,
    FakeRunner,
    Recorder,
    build_script,
    patch_all,
)


class DesktopTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.source = self.root / "src"
        self.multiplatform = self.source / "apps/multiplatform"
        self.multiplatform.mkdir(parents=True)

    def tearDown(self):
        self.tmp.cleanup()

    def release_workflow(self, text):
        workflow = self.source / ".github/workflows/build.yml"
        workflow.parent.mkdir(parents=True, exist_ok=True)
        workflow.write_text(text)
        return build_script.linux_container(self.source, JAMMY_ARM, self.root).run_args

    def test_container_keeps_the_cabal_store_where_the_release_workflow_does(self):
        run_args = self.release_workflow("docker run -t -d \\\n  -e CABAL_DIR=/home/builder/.cabal \\\n  --name builder\n")
        self.assertEqual(run_args[-2:], ["-e", "CABAL_DIR=/home/builder/.cabal"])

    def test_container_of_a_release_before_cabal_dir_uses_cabal_default(self):
        run_args = self.release_workflow("docker run -t -d \\\n  --name builder\n")
        self.assertFalse(any("CABAL_DIR" in arg for arg in run_args))

    def test_files_of_each_image(self):
        self.assertEqual(
            build_script.desktop_files(JAMMY),
            [
                "simplex-chat-ubuntu-22_04-x86_64",
                "simplex-chat-ubuntu-22_04-x86_64.deb",
                "simplex-desktop-ubuntu-22_04-x86_64.deb",
                "simplex-desktop-x86_64.AppImage",
            ],
        )
        self.assertNotIn("simplex-desktop-x86_64.AppImage", build_script.desktop_files(NOBLE))
        postgres = build_script.linux_libs_zip("x86_64", build_script.Database.POSTGRES)
        self.assertEqual(postgres, "simplex-chat-libs-linux-x86_64-postgres.zip")

    def test_aarch64_files_and_image(self):
        self.assertEqual(build_script.linux_image("22.04", "aarch64"), JAMMY_ARM)
        self.assertEqual(
            build_script.desktop_files(JAMMY_ARM),
            [
                "simplex-chat-ubuntu-22_04-aarch64",
                "simplex-chat-ubuntu-22_04-aarch64.deb",
                "simplex-desktop-ubuntu-22_04-aarch64.deb",
                "simplex-desktop-aarch64.AppImage",
            ],
        )
        with self.assertRaises(build_script.BuildError):
            build_script.linux_image("24.04", "riscv64")

    def write_exports(self, definitions, flake):
        (self.source / "src/Simplex/Chat").mkdir(parents=True)
        (self.source / "src/Simplex/Chat/Mobile.hs").write_text(
            'foreign export ccall "chat_send_cmd" cChatSendCmd :: X\nforeign export ccall "chat_reopen_store" cReopen :: X\n'
        )
        (self.source / "libsimplex.dll.def").write_text(definitions)
        (self.source / "flake.nix").write_text(flake)

    def test_exports_are_checked_without_the_android_only_ones(self):
        self.write_exports("EXPORTS\n  chat_send_cmd\n", '"chat_send_cmd"\n')
        build_script.check_exports(self.source)

    def test_missing_or_duplicated_exports_fail(self):
        self.write_exports("EXPORTS\n", '"chat_send_cmd"\n')
        with self.assertRaises(build_script.BuildError) as raised:
            build_script.check_exports(self.source)
        self.assertIn("libsimplex.dll.def", str(raised.exception))
        (self.source / "libsimplex.dll.def").write_text("chat_send_cmd\n")
        (self.source / "flake.nix").write_text('"chat_send_cmd"\n"chat_send_cmd"\n')
        with self.assertRaises(build_script.BuildError):
            build_script.check_exports(self.source)

    def test_touch_tree_follows_symlinks_and_skips_dangling_ones(self):
        tree = self.root / "tree"
        (tree / "dir").mkdir(parents=True)
        (tree / "dir/file").write_text("x")
        (tree / "link").symlink_to("dir/file")
        (tree / "dangling").symlink_to("missing")
        build_script.touch_tree(tree, "1704067200")
        for path in (tree, tree / "dir", tree / "dir/file"):
            self.assertEqual(path.stat().st_mtime, 1704067200)
        self.assertFalse((tree / "missing").exists())

    def test_edit_lines_keeps_newlines_and_removes_none(self):
        path = self.root / "f"
        path.write_text("a\nb\nc")
        build_script.edit_lines(path, lambda line: None if line == "b" else line + "!")
        self.assertEqual(path.read_text(), "a!\nc!")

    def test_desktop_entry_registers_the_url_schemes(self):
        self.assertEqual(build_script.register_url_schemes("Exec=/opt/simplex/bin/simplex"), "Exec=/opt/simplex/bin/simplex %u")
        self.assertEqual(build_script.register_url_schemes("Exec=simplex %U"), "Exec=simplex %U")
        self.assertEqual(build_script.register_url_schemes("MimeType="), build_script.DESKTOP_MIME_TYPES)
        self.assertEqual(build_script.register_url_schemes("Name=SimpleX"), "Name=SimpleX")

    def test_cli_deb(self):
        binary = self.root / "simplex-chat"
        binary.write_bytes(b"x" * 2049)
        work = self.root / "work"
        work.mkdir()
        runner = FakeRunner()
        deb = build_script.build_cli_deb(runner, binary, "7.1.0", "aarch64", work)
        package = work / "deb-build/simplex-chat"
        control = (package / "DEBIAN/control").read_text()
        self.assertIn("Version: 7.1.0\nSection: Messenger\n", control)
        self.assertIn("Architecture: arm64\n", control)
        self.assertTrue(control.endswith("Installed-Size: 3\n"))
        self.assertEqual(stat.S_IMODE((package / "usr/bin/simplex-chat").stat().st_mode), 0o755)
        self.assertEqual(stat.S_IMODE((package / "DEBIAN").stat().st_mode), 0o755)
        self.assertEqual((package / "usr/bin/simplex-chat").stat().st_mtime, 1764547200)
        self.assertEqual(runner.argvs()[0], ("dpkg-deb", "--build", "--root-owner-group", "--uniform-compression", "./simplex-chat"))
        self.assertEqual([s.env for s in runner.steps], [{"SOURCE_DATE_EPOCH": "1764547200"}] * 2)
        self.assertEqual(deb, work / "deb-build/simplex-chat.deb")

    def test_installed_size_rounds_as_printf(self):
        binary = self.root / "b"
        for size, expected in ((0, "1"), (512, "1"), (513, "2"), (1024, "2"), (1536, "2"), (2049, "3")):
            binary.write_bytes(b"x" * size)
            work = self.root / f"w{size}"
            work.mkdir()
            build_script.build_cli_deb(FakeRunner(), binary, "1", "x86_64", work)
            self.assertIn(f"Installed-Size: {expected}\n", (work / "deb-build/simplex-chat/DEBIAN/control").read_text())

    def test_linux_lib_copies_libsimplex_and_its_ghc_libraries(self):
        self.write_exports("chat_send_cmd\n", '"chat_send_cmd"\n')
        build = self.source / "dist-newstyle/build/x86_64-linux/ghc-9.6.3/simplex-chat-7.1.0/build"
        build.mkdir(parents=True)
        (build / "libHSsimplex-chat-7.1.0-inplace-ghc9.6.3.so").write_bytes(b"\x7fELF simplex")
        (self.multiplatform / "desktop/build/cmake").mkdir(parents=True)

        def copy_ghc_libs(step):
            for name in step.argv[1:-1]:
                (pathlib.Path(step.argv[-1]) / pathlib.Path(name).name).write_bytes(b"\x7fELF ghc")

        ldd = "\tlibHSbase-4.18-ghc9.6.3.so => /ghc/lib/libHSbase-4.18-ghc9.6.3.so (0x1)\n\tlibc.so.6 => /lib/libc.so.6 (0x2)\n"
        libs = self.source / build_script.DESKTOP_LIBS_DIR / "linux-x86_64"
        ghc_runpath, store_runpath = [("RUNPATH", "$ORIGIN")], [("RUNPATH", f"{STORE}:$ORIGIN")]
        shipped = readelf({libs / "libHSbase-4.18-ghc9.6.3.so": ghc_runpath, libs / "libsimplex.so": store_runpath})
        runner = FakeRunner(actions={"cp": copy_ghc_libs}, answers={"ldd": ldd, "readelf": shipped})
        libs = build_script.build_linux_lib(runner, self.source, "x86_64", build_script.Database.POSTGRES)
        cabal = runner.argvs()[0]
        self.assertIn("--builddir=dist-newstyle", cabal)
        self.assertIn("simplexmq +client_library +client_postgres", cabal)
        self.assertIn(("patchelf", "--replace-needed", "libffi.so.7", "libffi.so.8", "libsimplex.so"), runner.argvs())
        self.assertIn(("cp", "/ghc/lib/libHSbase-4.18-ghc9.6.3.so", str(build / "deps")), runner.argvs())
        self.assertEqual(runner.argvs()[-1], ("patchelf", "--set-rpath", "$ORIGIN", str(libs / "libsimplex.so")))
        self.assertEqual(libs, self.source / build_script.DESKTOP_LIBS_DIR / "linux-x86_64")
        self.assertEqual(sorted(p.name for p in libs.iterdir()), ["libHSbase-4.18-ghc9.6.3.so", "libsimplex.so"])
        self.assertFalse((self.multiplatform / "desktop/build/cmake").exists())

    def test_vlc_is_extracted_once(self):
        libs = self.root / "libs"
        libs.mkdir()
        work = self.root / "work"
        work.mkdir()

        def extract(step):
            (work / "squashfs-root/usr/lib/vlc").mkdir(parents=True)
            (work / "squashfs-root/usr/lib/libvlc.so").write_bytes(b"vlc")

        runner = FakeRunner(actions={"vlc-linux-x86_64.appimage": extract})
        with patch_all("fetch_pinned", lambda url, digest, target: target.write_bytes(b"") or target) as fetch:
            build_script.add_vlc_linux(runner, libs, "x86_64", work)
            build_script.add_vlc_linux(runner, libs, "x86_64", work)
        self.assertEqual(len(runner.steps), 1)
        self.assertEqual(runner.argvs()[0][1], "--appimage-extract")
        self.assertTrue((libs / "vlc/libvlc.so").exists())
        del fetch

    def test_libs_are_linked_where_gradle_finds_them(self):
        build_script.link_desktop_libs(self.source, "linux-aarch64", "linux-arm64")
        build_script.link_desktop_libs(self.source, "linux-aarch64", "linux-arm64")
        link = self.multiplatform / "build/links/linux-arm64"
        self.assertEqual(os.readlink(link), "../../common/src/commonMain/cpp/desktop/libs/linux-aarch64/")

    def test_desktop_deb_is_repacked_for_the_ubuntu_release(self):
        release = self.multiplatform / "release/main/deb"

        def gradle(step):
            release.mkdir(parents=True)
            (release / "simplex_7.1.0-1_amd64.deb").write_bytes(b"jpackage")

        def dpkg(step):
            if step.argv[1] == "-R":
                app = self.multiplatform / "extracted/opt/simplex"
                (app / "lib/app").mkdir(parents=True)
                (self.multiplatform / "extracted/DEBIAN").mkdir()
                (self.multiplatform / "extracted/DEBIAN/control").write_text("Package: simplex\nVersion: 7.1.0-1\n")
                (app / "lib/app/skiko-awt-runtime-linux-x64-0.8.jar").write_bytes(b"jar")
                cfg = "app.classpath=$APPDIR/a.jar\napp.classpath=$APPDIR/skiko-awt-runtime-linux-x64-0.8.jar\n"
                (app / "lib/app/simplex.cfg").write_text(cfg)
                (app / "lib/simplex-SimpleX.desktop").write_text("[Desktop Entry]\nExec=/opt/simplex/bin/simplex\nMimeType=\n")
            else:
                (release / "simplex_x86_64.deb").write_bytes(b"repacked")

        runner = FakeRunner(actions={"gradlew": gradle, "dpkg-deb": dpkg})
        captured = {}
        original_touch = build_script.touch_tree

        def touch(root, epoch):
            captured["control"] = (root / "DEBIAN/control").read_text()
            captured["cfg"] = (root / "opt/simplex/lib/app/simplex.cfg").read_text()
            captured["entry"] = (root / "opt/simplex/lib/simplex-SimpleX.desktop").read_text()
            captured["jars"] = sorted(p.name for p in (root / "opt/simplex/lib/app").iterdir())
            original_touch(root, epoch)

        with patch_all("touch_tree", touch):
            deb = build_script.package_desktop_deb(runner, self.source, "x86_64", "jammy")
        self.assertEqual(deb, release / "simplex_x86_64.deb")
        self.assertEqual(captured["control"], "Package: simplex\nVersion: 7.1.0-1~jammy\n")
        self.assertNotIn("skiko", captured["cfg"])
        self.assertEqual(captured["jars"], ["simplex.cfg"])
        self.assertIn("Exec=/opt/simplex/bin/simplex %u\n", captured["entry"])
        self.assertIn(build_script.DESKTOP_MIME_TYPES, captured["entry"])
        self.assertEqual(runner.argvs()[0], ("./gradlew", "-Psimplex.assets.dir=../../assets", "packageDeb"))
        self.assertEqual(runner.steps[2].env, {"SOURCE_DATE_EPOCH": "1704067200"})
        self.assertFalse((self.multiplatform / "extracted").exists())

    def test_ubuntu_codename(self):
        runner = FakeRunner(answers={"os-release": 'NAME="Ubuntu"\nVERSION_CODENAME=noble\n'})
        self.assertEqual(build_script.ubuntu_codename(runner), "noble")
        with self.assertRaises(build_script.BuildError):
            build_script.ubuntu_codename(FakeRunner())

    def test_appimage(self):
        libs = self.source / build_script.DESKTOP_LIBS_DIR / "linux-x86_64"
        libs.mkdir(parents=True)
        libcrypto = self.root / "libcrypto.so.3"
        libcrypto.write_bytes(b"pinned")
        distribute = self.multiplatform / "desktop/src/jvmMain/resources/distribute"
        distribute.mkdir(parents=True)
        (distribute / "chat.simplex.app.desktop").write_text("Exec=/opt/x/simplex %u\nIcon=chat.simplex.app\n")
        (distribute / "chat.simplex.app.appdata.xml").write_text("<x/>")
        app = self.multiplatform / "release/main/app"
        bundled_during_gradle = []

        def gradle(step):
            bundled_during_gradle.append((libs / "libcrypto.so.3").read_bytes())
            (app / "simplex/bin").mkdir(parents=True)
            (app / "simplex/lib/app").mkdir(parents=True)
            (app / "simplex/bin/simplex").write_bytes(b"launcher")
            (app / "simplex/lib/simplex.png").write_bytes(b"png")
            (app / "simplex/lib/app/skiko-awt-runtime-linux-x64.jar").write_bytes(b"jar")
            (app / "simplex/lib/app/simplex.cfg").write_text("skiko-awt-runtime-linux-x64.jar\nother\n")

        def appimagetool(step):
            (app / "AppDir/SimpleX_Chat-x86_64.AppImage").write_bytes(b"appimage")

        runner = FakeRunner(actions={"gradlew": gradle, "appimagetool-x86_64.AppImage": appimagetool})
        work = self.root / "work"
        work.mkdir()
        with patch_all("fetch_pinned", lambda url, digest, target: target.write_bytes(b"") or target):
            image = build_script.package_appimage(runner, self.source, "x86_64", libcrypto, work)
        self.assertEqual(bundled_during_gradle, [b"pinned"])
        self.assertFalse((libs / "libcrypto.so.3").exists())
        appdir = app / "AppDir"
        self.assertEqual(os.readlink(appdir / "AppRun"), "usr/bin/simplex")
        self.assertEqual((appdir / "usr/share/applications/chat.simplex.app.desktop").read_text(), "Exec=simplex %u\nIcon=simplex\n")
        self.assertTrue((appdir / "usr/share/metainfo/chat.simplex.app.appdata.xml").exists())
        self.assertFalse((appdir / "usr/lib/app/skiko-awt-runtime-linux-x64.jar").exists())
        self.assertEqual((appdir / "usr/lib/app/simplex.cfg").read_text(), "other\n")
        tool_step = next(s for s in runner.steps if s.argv[0].endswith("appimagetool-x86_64.AppImage"))
        self.assertEqual(tool_step.argv[1:4], ("--verbose", "--no-appstream", "--runtime-file"))
        self.assertEqual(tool_step.env, {"SOURCE_DATE_EPOCH": "1704067200"})
        self.assertEqual(image, self.multiplatform / "release/main/SimpleX_Chat-x86_64.AppImage")

    def test_a_checkout_with_this_script_and_old_scripts_builds_with_it_in_order_with_released_names(self):
        out = self.root / "out"
        out.mkdir()
        for script in (build_script.BUILD_SCRIPT, build_script.TAG_DESKTOP_DEB_SCRIPT):
            (self.source / script).parent.mkdir(parents=True, exist_ok=True)
            (self.source / script).write_text("#!/bin/sh\n")

        def made(name):
            def make(*args, **kwargs):
                path = self.root / name
                path.write_bytes(name.encode())
                return path

            return make

        container = FakeRunner(answers={"list-bin": "/project/dist/simplex-chat\n", "os-release": "VERSION_CODENAME=jammy\n"})
        container.__enter__ = lambda: container
        calls = []

        def record(name, result=None):
            def call(*args, **kwargs):
                calls.append(name)
                return result(*args) if result else None

            return call

        def cp(step):
            if step.argv[0] == "cp":
                pathlib.Path(step.argv[2]).write_bytes(b"cli")

        container.actions["cp"] = cp
        with (
            patch_all("linux_container", return_value=contextlib.nullcontext(container)),
            patch_all("build_cli_deb", record("cli deb", made("cli.deb"))),
            patch_all("build_linux_lib", record("lib", lambda *a: self.root)),
            patch_all("add_vlc_linux", record("vlc")),
            patch_all("link_desktop_libs", record("link")),
            patch_all("package_desktop_deb", record("desktop deb", made("desktop.deb"))),
            patch_all("zip_desktop_libs", record("libs zip")),
            patch_all("fetch_openssl", record("openssl", lambda work: work)),
            patch_all("package_appimage", record("appimage", made("app.AppImage"))),
            patch_all("log"),
        ):
            build_script.build_desktop_linux(self.source, JAMMY, "7.1.0", out)
        self.assertEqual(calls, ["cli deb", "lib", "vlc", "link", "desktop deb", "libs zip", "openssl", "appimage"])
        self.assertEqual(sorted(p.name for p in out.iterdir()), sorted(build_script.desktop_files(JAMMY)))
        configure = next(a for a in container.argvs() if a[:2] == ("perl", "Configure"))
        self.assertIn("--openssldir=/etc/ssl", configure)
        self.assertIn("flags: +openssl", (self.source / "cabal.project.local").read_text())

    def test_release_made_before_this_script_is_built_by_its_own_scripts(self):
        out, work = self.root / "out", self.root / "work"
        out.mkdir()
        work.mkdir()
        for script in (build_script.TAG_CLI_DEB_SCRIPT, build_script.TAG_DESKTOP_DEB_SCRIPT, build_script.TAG_APPIMAGE_SCRIPT):
            (self.source / script).parent.mkdir(parents=True, exist_ok=True)
            (self.source / script).write_text("#!/bin/sh\n")
        release = self.source / build_script.DESKTOP_RELEASE_DIR

        def run_script(step):
            name = pathlib.Path(step.argv[0]).name
            if name == "build-cli-deb.sh":
                pathlib.Path(step.argv[3]).mkdir()
                (pathlib.Path(step.argv[3]) / "simplex-chat.deb").write_bytes(b"cli deb")
            elif name == "make-deb-linux.sh":
                (release / "deb").mkdir(parents=True)
                (release / "deb/simplex_x86_64.deb").write_bytes(b"desktop deb")
            elif name == "make-appimage-linux.sh":
                (release / "SimpleX_Chat-x86_64.AppImage").write_bytes(b"appimage")

        container = FakeRunner(
            actions={name: run_script for name in ("build-cli-deb.sh", "make-deb-linux.sh", "make-appimage-linux.sh")},
            answers={"list-bin": "/project/dist/simplex-chat\n"},
        )
        container.actions["cp"] = lambda step: pathlib.Path(step.argv[2]).write_bytes(b"cli")
        with (
            patch_all("linux_container", return_value=contextlib.nullcontext(container)),
            mock.patch.object(build_script.tempfile, "TemporaryDirectory", return_value=contextlib.nullcontext(str(work))),
            patch_all("build_cli_deb") as python_deb,
            patch_all("log"),
        ):
            build_script.build_desktop_linux(self.source, JAMMY, "7.0.3", out)
        python_deb.assert_not_called()
        self.assertEqual(
            {p.name: p.read_bytes() for p in out.iterdir()},
            {
                "simplex-chat-ubuntu-22_04-x86_64": b"cli",
                "simplex-chat-ubuntu-22_04-x86_64.deb": b"cli deb",
                "simplex-desktop-ubuntu-22_04-x86_64.deb": b"desktop deb",
                "simplex-desktop-x86_64.AppImage": b"appimage",
            },
        )
        scripts = [s for s in container.steps if s.argv[0].startswith("scripts/")]
        self.assertEqual(scripts[0].argv[1], "7.0.3")
        self.assertEqual(scripts[1].env, {"ASSETS_DIR": "../../assets"})

    def reproduce(self, root, released_appimage):
        results = root / "v7.1.0-simplex-chat"

        def fake_build(source, image, version, out):
            for name in build_script.desktop_files(image):
                (out / name).write_bytes(name.encode())
            (out / f"simplex-chat-libs-linux-{image.arch}.zip").write_bytes(b"not a release file")

        urls = []

        def fake_download(url, target):
            urls.append(url)
            target.write_bytes(released_appimage if target.name.endswith(".AppImage") else target.name.encode())

        with (
            patch_all("checkout"),
            patch_all("build_desktop_linux", fake_build),
            patch_all("download", fake_download),
            patch_all("log"),
            mock.patch("builtins.print"),
            mock.patch("platform.machine", return_value="x86_64"),
        ):
            mismatched = build_script.reproduce_desktop("v7.1.0", "owner/fork", root, results)
        return mismatched, urls, results

    def test_reproduce_reports_released_files_that_differ_and_writes_no_sums(self):
        with tempfile.TemporaryDirectory() as tmp:
            mismatched, urls, results = self.reproduce(pathlib.Path(tmp), b"other")
            self.assertEqual(mismatched, ["simplex-desktop-x86_64.AppImage"])
            self.assertFalse((results / "_sha256sums").exists())
            self.assertIn("https://github.com/owner/fork/releases/download/v7.1.0/simplex-desktop-x86_64.AppImage", urls)

    def test_reproduce_sums_list_the_released_files_when_all_match(self):
        with tempfile.TemporaryDirectory() as tmp:
            mismatched, _, results = self.reproduce(pathlib.Path(tmp), b"simplex-desktop-x86_64.AppImage")
            self.assertEqual(mismatched, [])
            listed = [line.split()[1] for line in (results / "_sha256sums").read_text().splitlines()]
            self.assertEqual(listed, sorted(name for image in (JAMMY, NOBLE) for name in build_script.desktop_files(image)))

class LinuxLibsTest(unittest.TestCase):
    def test_host_build_updates_cabal_and_zips_the_libraries(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            libs = root / "libs"
            libs.mkdir()
            (libs / "libsimplex.so").write_bytes(b"lib")
            (libs / "vlc").mkdir()
            recorder = Recorder()
            with (
                patch_all("run", recorder),
                patch_all("build_linux_lib", return_value=libs) as lib,
            ):
                build_script.build_linux_libs(
                    root, JAMMY, build_script.Database.POSTGRES, "dist-newstyle", build_script.LibsBuilder(on_host=True), root
                )
            self.assertEqual(recorder.commands, [["cabal", "update"]])
            self.assertIsInstance(lib.call_args.args[0], build_script.HostRunner)
            with zipfile.ZipFile(root / "simplex-chat-libs-linux-x86_64-postgres.zip") as z:
                self.assertEqual(z.namelist(), ["libs/libsimplex.so"])

    def test_running_container_is_used_and_kept(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            libs = root / "libs"
            libs.mkdir()
            recorder = Recorder()
            with (
                patch_all("run", recorder),
                patch_all("build_linux_lib", return_value=libs) as lib,
            ):
                builder = build_script.LibsBuilder(container="builder")
                build_script.build_linux_libs(root, JAMMY, build_script.Database.SQLITE, "dist-lib", builder, root)
            self.assertEqual(recorder.commands, [["docker", "exec", "-w", "/project", "builder", "cabal", "update"]])
            self.assertEqual(lib.call_args.args[4], "dist-lib")


STORE = "/home/builder/.cabal/store/ghc-9.6.3/hashable-1.4.3.0-98ac/lib"


def readelf(entries):
    """What readelf -d prints of each file: entries maps a file to its (kind, value) search paths."""
    blocks = []
    for file, paths in entries.items():
        lines = [f"File: {file}", "", "Dynamic section at offset 0x2d6e8 contains 3 entries:", "  Tag        Type        Name/Value"]
        lines.append(" 0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]")
        for kind, value in paths:
            name = "Library rpath" if kind == "RPATH" else "Library runpath"
            lines.append(f" 0x000000000000001d ({kind})            {name}: [{value}]")
        blocks.append("\n".join(lines))
    return "\n\n".join(blocks) + "\n"


class LibrarySearchPathTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.libs = pathlib.Path(self.tmp.name) / "libs"
        (self.libs / "vlc").mkdir(parents=True)

    def tearDown(self):
        self.tmp.cleanup()

    def elf(self, name):
        path = self.libs / name
        path.write_bytes(b"\x7fELF\x02\x01\x01" + bytes(57))
        return path

    def test_search_paths_of_one_or_many_files(self):
        one = self.elf("libsimplex.so")
        single = readelf({one: [("RUNPATH", f"{STORE}:$ORIGIN")]}).split("\n", 2)[2]
        self.assertEqual(build_script.library_search_paths(FakeRunner(answers={"readelf": single}), [one]), {one: [STORE, "$ORIGIN"]})
        other = self.elf("libHSbase-4.18.1.0-ghc9.6.3.so")
        output = readelf({one: [("RUNPATH", "$ORIGIN")], other: [("RPATH", "$ORIGIN/../lib"), ("RUNPATH", "/usr/lib")]})
        self.assertEqual(
            build_script.library_search_paths(FakeRunner(answers={"readelf": output}), [one, other]),
            {one: ["$ORIGIN"], other: ["$ORIGIN/../lib", "/usr/lib"]},
        )
        self.assertEqual(build_script.library_search_paths(FakeRunner(), []), {})

    def test_host_runner_sets_origin_runpaths_in_the_libraries_directory(self):
        lib = self.elf("libsimplex.so")
        output = readelf({lib: [("RUNPATH", f"{STORE}:$ORIGIN")]})
        with patch_all("run", return_value=mock.Mock(stdout=output)) as run:
            build_script.set_origin_runpaths(build_script.HostRunner(), self.libs)
        self.assertEqual(
            [(call.args[0], call.kwargs["cwd"]) for call in run.call_args_list],
            [(("readelf", "-d", str(lib)), self.libs), (("patchelf", "--set-rpath", "$ORIGIN", str(lib)), self.libs)],
        )

    def test_libraries_that_look_elsewhere_look_only_in_their_own_directory(self):
        store = self.elf("libHShashable-1.4.3.0-98ac-ghc9.6.3.so")
        simplex = self.elf("libsimplex.so")
        ghc = self.elf("libHSbase-4.18.1.0-ghc9.6.3.so")
        ffi = self.elf("libffi.so.8")
        (self.libs / "README").write_text("not a library")
        output = readelf({ffi: [], ghc: [("RUNPATH", "$ORIGIN")], simplex: [("RUNPATH", f"{STORE}:$ORIGIN")], store: [("RUNPATH", STORE)]})
        runner = FakeRunner(answers={"readelf": output})
        build_script.set_origin_runpaths(runner, self.libs)
        patched = [argv for argv in runner.argvs() if argv[0] == "patchelf"]
        self.assertEqual(patched, [("patchelf", "--set-rpath", "$ORIGIN", str(path)) for path in (store, simplex)])
        self.assertNotIn(str(self.libs / "README"), runner.argvs()[0])

    def test_files_that_look_for_libraries_outside_their_directory_fail(self):
        planted = self.elf("libHSaeson-2.2.1.0-cf52-ghc9.6.3.so")
        vlc = self.elf("vlc/libvlc.so.5")
        runtime = self.elf("libawt.so")
        vlc_paths = [("RUNPATH", "$ORIGIN/../:$ORIGIN")]
        output = readelf({planted: [("RUNPATH", f"$ORIGIN:{STORE}")], runtime: [("RPATH", "$ORIGIN")], vlc: vlc_paths})
        with self.assertRaises(build_script.BuildError) as raised:
            build_script.check_origin_runpaths(FakeRunner(answers={"readelf": output}), [self.libs])
        self.assertEqual(str(raised.exception), f"these files look for libraries outside their own directory:\n{planted} ({STORE})")
        relative = readelf({planted: [("RUNPATH", "$ORIGIN")], runtime: [("RPATH", "$ORIGIN")], vlc: [("RUNPATH", "$ORIGIN/../:$ORIGIN")]})
        build_script.check_origin_runpaths(FakeRunner(answers={"readelf": relative}), [self.libs])
        lookalike = readelf({planted: [("RUNPATH", "$ORIGINAL/lib")], runtime: [("RPATH", "$ORIGIN")], vlc: []})
        with self.assertRaisesRegex(build_script.BuildError, r"\(\$ORIGINAL/lib\)"):
            build_script.check_origin_runpaths(FakeRunner(answers={"readelf": lookalike}), [self.libs])

    def test_app_library_is_linked_with_its_own_directory_only(self):
        cmake = (REPO_ROOT / "apps/multiplatform/common/src/commonMain/cpp/desktop/CMakeLists.txt").read_text()
        linux = cmake.split("if(UNIX AND NOT APPLE)", 1)[1].split("elseif(WIN32)", 1)[0]
        self.assertIn("set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)", linux)
        self.assertIn('set(CMAKE_INSTALL_RPATH "$ORIGIN")', linux)
        self.assertNotIn("CMAKE_BUILD_RPATH", linux)
