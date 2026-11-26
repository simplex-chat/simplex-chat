import io
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

from .testing import GITHUB, Arch, Recorder, build_script, patch_all


class ReleaseDetailsTest(unittest.TestCase):
    def test_tag_version(self):
        self.assertEqual(build_script.tag_version("v7.1.0-beta.7"), "7.1.0")
        self.assertEqual(build_script.tag_version("v6.4.8"), "6.4.8")
        self.assertEqual(build_script.tag_version("v7.1.99-repro-test"), "7.1.99-repro")

    def test_release_url_and_clone_of_simplex_chat_or_a_fork(self):
        self.assertEqual(
            build_script.release_url("v7.1.0", "simplex-aarch64.apk"),
            "https://github.com/simplex-chat/simplex-chat/releases/download/v7.1.0/simplex-aarch64.apk",
        )
        self.assertEqual(
            build_script.release_url("v7.1.99-test", "a.dmg", "owner/fork"), "https://github.com/owner/fork/releases/download/v7.1.99-test/a.dmg"
        )
        recorder = Recorder()
        with patch_all("run", recorder):
            self.assertEqual(build_script.clone(pathlib.Path("w"), "owner/fork"), pathlib.Path("w/simplex-chat"))
            build_script.clone(pathlib.Path("w"))
        self.assertEqual(recorder.commands[0][-4:], ["-c", "core.autocrlf=false", "https://github.com/owner/fork.git", "simplex-chat"])
        self.assertEqual(recorder.commands[1][-2], "https://github.com/simplex-chat/simplex-chat.git")

    def test_checkout_of_a_tag_has_its_submodules_and_their_submodules(self):
        recorder = Recorder()
        with patch_all("run", recorder):
            build_script.checkout(pathlib.Path("w/simplex-chat"), "v7.1.0")
        self.assertEqual(
            recorder.commands,
            [
                ["git", "-C", "w/simplex-chat", "reset", "--hard", "--quiet"],
                ["git", "-C", "w/simplex-chat", "clean", "-dfxq"],
                ["git", "-C", "w/simplex-chat", "checkout", "--quiet", "v7.1.0"],
                ["git", "-C", "w/simplex-chat", "submodule", "update", "--init", "--recursive", "--quiet"],
            ],
        )

    def test_android_build_tag(self):
        tags = ["v6.4.0", "v6.4.0-armv7a", "v7.1.0"]
        self.assertEqual(build_script.android_build_tag("v6.4.0", tags, Arch.ARMV7A), "v6.4.0-armv7a")
        self.assertEqual(build_script.android_build_tag("v6.4.0", tags, Arch.AARCH64), "v6.4.0")
        self.assertEqual(build_script.android_build_tag("v7.1.0", tags, Arch.ARMV7A), "v7.1.0")

    def test_arches(self):
        self.assertEqual([a.abi for a in Arch], ["arm64-v8a", "armeabi-v7a"])
        self.assertEqual(f"simplex-{Arch.ARMV7A}.apk", "simplex-armv7a.apk")

    def test_android_vercode(self):
        with tempfile.TemporaryDirectory() as tmp:
            props = pathlib.Path(tmp) / "apps/multiplatform/gradle.properties"
            props.parent.mkdir(parents=True)
            props.write_text("android.version_name=7.1-beta.7\nandroid.version_code=387\n")
            self.assertEqual(build_script.android_vercode(pathlib.Path(tmp)), "387")
            props.write_text("android.version_name=7.1\n")
            with self.assertRaises(build_script.BuildError):
                build_script.android_vercode(pathlib.Path(tmp))


class FilesTest(unittest.TestCase):
    def test_sha256sums_list_every_file_but_the_sums(self):
        with tempfile.TemporaryDirectory() as tmp:
            d = pathlib.Path(tmp)
            (d / "b.apk").write_bytes(b"b")
            (d / "a.zip").write_bytes(b"a")
            (d / "SHA256SUMS").write_text("old")
            with mock.patch("builtins.print"):
                build_script.write_sha256sums(d)
            self.assertEqual(
                (d / "SHA256SUMS").read_text(),
                f"{build_script.sha256(d / 'a.zip')}  a.zip\n{build_script.sha256(d / 'b.apk')}  b.apk\n",
            )

    def test_compare_dirs_reports_released_files_that_differ_or_are_not_built(self):
        with tempfile.TemporaryDirectory() as built, tempfile.TemporaryDirectory() as released:
            for name, data in (("same", b"1"), ("differs", b"2"), ("libs.zip", b"not released")):
                (pathlib.Path(built) / name).write_bytes(data)
            for name, data in (("same", b"1"), ("differs", b"x"), ("missing", b"3")):
                (pathlib.Path(released) / name).write_bytes(data)
            self.assertEqual(build_script.compare_dirs(pathlib.Path(built), pathlib.Path(released)), ["differs", "missing"])

    def test_permissions_are_normalized(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            (root / "dir").mkdir(mode=0o700)
            (root / "dir/script.sh").write_text("")
            (root / "dir/script.sh").chmod(0o700)
            (root / "dir/data").write_text("")
            (root / "dir/data").chmod(0o600)
            (root / "link").symlink_to("dir/data")
            build_script.normalize_permissions(root)
            modes = [(root / p).stat().st_mode & 0o777 for p in ("dir", "dir/script.sh", "dir/data")]
            self.assertEqual(modes, [0o755, 0o755, 0o644])

    def test_single_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            d = pathlib.Path(tmp)
            with self.assertRaises(build_script.BuildError):
                build_script.single_file(d, "*.aab")
            (d / "a.aab").write_bytes(b"")
            self.assertEqual(build_script.single_file(d, "*.aab"), d / "a.aab")
            (d / "b.aab").write_bytes(b"")
            with self.assertRaises(build_script.BuildError):
                build_script.single_file(d, "*.aab")


class PlatformTest(unittest.TestCase):
    def test_host_platform(self):
        for os_name, sys_platform, expected in (("nt", "win32", "windows"), ("posix", "darwin", "macos"), ("posix", "linux", "linux")):
            with mock.patch.object(build_script.os, "name", os_name), mock.patch.object(build_script.sys, "platform", sys_platform):
                self.assertEqual(build_script.host_platform(), expected)

    def test_builds_for_another_platform_are_refused(self):
        with (
            tempfile.TemporaryDirectory() as tmp,
            patch_all("host_platform", return_value=build_script.Platform.LINUX),
        ):
            out, here = pathlib.Path(tmp), pathlib.Path(".")
            for command in (
                build_script.BuildDesktopMacos(out, here, False),
                build_script.BuildOpenssl(build_script.Platform.WINDOWS, out, here),
                build_script.BuildIosArchive(pathlib.Path("a"), out, here, False),
            ):
                with self.assertRaises(build_script.BuildError) as raised:
                    build_script.build(command)
                self.assertIn("not on linux", str(raised.exception))


class GitHubLogTest(unittest.TestCase):
    def output(self, environ, action):
        with (
            mock.patch.dict(os.environ, environ, clear=True),
            mock.patch("sys.stdout", new_callable=io.StringIO) as out,
            mock.patch("sys.stderr", new_callable=io.StringIO) as err,
        ):
            action()
        return out.getvalue(), err.getvalue()

    def test_warn_annotates_on_github_actions(self):
        out, err = self.output(GITHUB, lambda: build_script.warn("unsigned build"))
        self.assertEqual((out, err), ("::warning::unsigned build\n", ""))

    def test_warn_prints_to_stderr_elsewhere(self):
        out, err = self.output({}, lambda: build_script.warn("unsigned build"))
        self.assertEqual(out, "")
        self.assertIn("warning: unsigned build", err)

    def test_error_escapes_newlines_and_percent(self):
        out, _ = self.output(GITHUB, lambda: build_script.error("100% failed\nat step 2"))
        self.assertEqual(out, "::error::100%25 failed%0Aat step 2\n")

    def test_group_is_closed_when_its_command_fails(self):
        def failing():
            with build_script.log_group("cabal build"):
                raise build_script.BuildError("failed")

        out, _ = self.output(GITHUB, lambda: self.assertRaises(build_script.BuildError, failing))
        self.assertEqual(out, "::group::cabal build\n::endgroup::\n")

    def test_run_groups_shown_output_only(self):
        def commands():
            build_script.run([sys.executable, "-c", "print('built')"])
            build_script.run([sys.executable, "-c", "print('hidden')"], capture=True, title="never shown")

        with mock.patch.object(subprocess, "run", return_value=mock.Mock(returncode=0, stdout="")):
            out, _ = self.output(GITHUB, commands)
        self.assertEqual(out, f"::group::{sys.executable} -c print('built')\n::endgroup::\n")

    def test_container_steps_are_titled_with_their_command(self):
        recorder = Recorder()
        container = build_script.Container(pathlib.Path("."), {}, {}, [])
        with patch_all("run", wraps=recorder) as run:
            container.run(build_script.Step(("cabal", "update"), env={"PATH": "/x"}))
        self.assertEqual(run.call_args.kwargs["title"], "cabal update")

    def test_summary_lists_files_on_github_actions(self):
        with tempfile.TemporaryDirectory() as tmp:
            d = pathlib.Path(tmp)
            (d / "simplex-aarch64.apk").write_bytes(b"apk")
            summary = d / "summary.md"
            self.output({**GITHUB, "GITHUB_STEP_SUMMARY": str(summary)}, lambda: build_script.write_sha256sums(d))
            text = summary.read_text()
        self.assertIn("| `simplex-aarch64.apk` | 3 | `", text)
        self.assertNotIn("SHA256SUMS`", text)

    def test_summary_is_skipped_elsewhere(self):
        with tempfile.TemporaryDirectory() as tmp:
            summary = pathlib.Path(tmp) / "summary.md"
            self.output({"GITHUB_STEP_SUMMARY": str(summary)}, lambda: build_script.add_summary("text"))
            self.assertFalse(summary.exists())


class ReleaseComparisonTest(unittest.TestCase):
    def test_release_files_are_downloaded_from_the_repository(self):
        with tempfile.TemporaryDirectory() as tmp:
            target = pathlib.Path(tmp) / "prebuilt"
            with patch_all("download") as download:
                build_script.download_release("v7.1.0", "owner/fork", ["a.dmg", "cli"], target)
            self.assertTrue(target.is_dir())
            self.assertEqual(
                download.call_args_list,
                [
                    mock.call("https://github.com/owner/fork/releases/download/v7.1.0/a.dmg", target / "a.dmg"),
                    mock.call("https://github.com/owner/fork/releases/download/v7.1.0/cli", target / "cli"),
                ],
            )

    def test_each_differing_or_unbuilt_file_fails_and_no_sums_are_written(self):
        with tempfile.TemporaryDirectory() as tmp:
            out = pathlib.Path(tmp)
            for directory, files in (("prebuilt", {"a": b"1", "b": b"2", "c": b"3"}), ("from-source", {"a": b"1", "b": b"x"})):
                (out / directory).mkdir()
                for name, data in files.items():
                    (out / directory / name).write_bytes(data)
            with patch_all("log") as log:
                self.assertEqual(build_script.compare_release(["a", "b", "c"], out, build_script.byte_differences), ["b", "c"])
            logged = [c.args[0] for c in log.call_args_list]
            self.assertIn("b: sha256 doesn't match", logged)
            self.assertIn("c: it is not built", logged)
            self.assertIn("a is reproduced", logged)
            self.assertFalse((out / "_sha256sums").exists())

    def test_sums_list_the_released_files_when_all_match(self):
        with tempfile.TemporaryDirectory() as tmp:
            out = pathlib.Path(tmp)
            for directory in ("prebuilt", "from-source"):
                (out / directory).mkdir()
                (out / directory / "a").write_bytes(b"1")
            with patch_all("log"):
                self.assertEqual(build_script.compare_release(["a"], out, lambda released, built: []), [])
            self.assertEqual((out / "_sha256sums").read_text().split(), [build_script.sha256(out / "prebuilt/a"), "a"])
