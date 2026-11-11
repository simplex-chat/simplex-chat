import os
import pathlib
import tempfile
import unittest
import zipfile
from unittest import mock

from .testing import (
    APKSIGNER_PADDING,
    COPIED,
    REPO_ROOT,
    SIGNED,
    SIGNING_ENV,
    Arch,
    Recorder,
    build_script,
    make_zip,
    patch_all,
)


class AndroidTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.source, self.out = self.root / "src", self.root / "out"
        props = self.source / "apps/multiplatform/gradle.properties"
        props.parent.mkdir(parents=True)
        props.write_text("android.version_code=387\n")
        self.out.mkdir()

    def tearDown(self):
        self.tmp.cleanup()

    def container(self):
        return build_script.Container(self.source, {})

    def test_armv7a_container_sets_pid_max_in_its_namespace(self):
        recorder = Recorder()
        with patch_all("run", recorder), build_script.android_container(self.source, self.out, set_pid_max=True):
            pass
        start = next(c for c in recorder.commands if c[1] == "run")
        self.assertIn("systempaths=unconfined", start)
        tee = next(c for c in recorder.commands if "tee" in c)
        self.assertIn("root", tee)
        self.assertIn("65535\n", recorder.inputs)

    def test_apk_build_retries_once_and_keeps_the_libraries(self):
        (self.source / "simplex-chat-armeabi-v7a.apk").write_bytes(b"apk")
        recorder = Recorder(
            fail_once=["build-android.sh"],
            answers={"HOME": "/home/builder\n", "PATH": "/usr/bin\n", "find": "/home/builder/.gradle/wrapper/dists/g/bin/gradle\n"},
        )
        with patch_all("run", recorder), patch_all("warn") as warn:
            build_script.build_apk(self.container(), self.source, Arch.ARMV7A, self.out, "simplex-armv7a.apk", True, True)
        self.assertIn("running it again", warn.call_args.args[0])
        builds = [c for c in recorder.commands if "./scripts/android/build-android.sh" in c]
        self.assertEqual(len(builds), 2)
        self.assertEqual(builds[0][-2:], ["-gs", "387"])
        self.assertIn("ARCHES=armv7a", builds[0])
        self.assertIn("PATH=/home/builder/.gradle/wrapper/dists/g/bin:/usr/bin", builds[0])
        self.assertTrue(any(a.startswith("NIX_CONFIG=extra-substituters") for a in builds[0]))
        self.assertEqual((self.out / "simplex-armv7a.apk").read_bytes(), b"apk")
        copies = [c[-2] for c in recorder.commands if c[:2] == ["docker", "cp"]]
        self.assertTrue(copies[0].endswith(":/project/result-armv7a-libsimplex/pkg-armv7a-android-libsimplex.zip"))
        self.assertEqual(len(copies), 2)

    def test_apk_build_without_cache_or_libraries(self):
        (self.source / "simplex-chat-arm64-v8a.apk").write_bytes(b"apk")
        recorder = Recorder(answers={"find": "/g/bin/gradle\n"})
        with patch_all("run", recorder):
            build_script.build_apk(self.container(), self.source, Arch.AARCH64, self.out, "x.apk", False, False)
        build = next(c for c in recorder.commands if "./scripts/android/build-android.sh" in c)
        self.assertFalse(any(a.startswith("NIX_CONFIG") for a in build))
        self.assertFalse(any(c[:2] == ["docker", "cp"] for c in recorder.commands))

    def test_missing_gradle_distribution(self):
        with patch_all("run", Recorder()), self.assertRaises(build_script.BuildError):
            build_script.build_apk(self.container(), self.source, Arch.AARCH64, self.out, "x.apk", False, False)

    def test_bundle_extracts_both_arches_with_release_properties(self):
        libs = self.root / "libs"
        for arch in Arch:
            directory = libs / f"android-{arch}-unsigned"
            directory.mkdir(parents=True)
            for lib in build_script.ANDROID_LIBS:
                with zipfile.ZipFile(directory / f"pkg-{arch}-android-{lib}.zip", "w") as z:
                    z.writestr(f"{lib}.so", f"{arch} {lib}")
        bundle_dir = self.source / build_script.ANDROID_BUNDLE_DIR
        bundle_dir.mkdir(parents=True)
        (bundle_dir / "android-google-release.aab").write_bytes(b"aab")
        recorder = Recorder()
        with patch_all("run", recorder):
            build_script.build_bundle(self.container(), self.source, libs, self.out)
        extracted = self.source / build_script.ANDROID_LIBS_DIR / "armeabi-v7a/libsupport.so"
        self.assertEqual(extracted.read_text(), "armv7a libsupport")
        self.assertEqual(recorder.steps(), [["./gradlew", "--no-daemon", ":android:bundleGoogleRelease"]])
        self.assertEqual(
            (self.source / "apps/multiplatform/local.properties").read_text(),
            "compression.level=5\nsimplex.assets.dir=../../assets\n",
        )
        self.assertEqual((self.out / "simplex.aab").read_bytes(), b"aab")

    def test_bundle_fails_without_a_library(self):
        with self.assertRaises(build_script.BuildError):
            build_script.build_bundle(self.container(), self.source, self.root / "none", self.out)

    def reproduce(self, repo, signer):
        with (
            patch_all("run", Recorder()),
            patch_all("download"),
            patch_all("release_signer", return_value=signer),
            patch_all("checkout"),
            patch_all("build_apk"),
            patch_all("verify_apk", return_value=True),
            patch_all("warn") as warn,
            patch_all("log"),
        ):
            failed = build_script.reproduce_android("v7.1.0", repo, [Arch.AARCH64], False, self.source, self.root / "results")
        return failed, warn

    def test_reproduce_requires_the_simplex_key_for_simplex_chat_releases(self):
        failed, warn = self.reproduce("simplex-chat/simplex-chat", build_script.SIMPLEX_KEY)
        self.assertEqual(failed, [])
        warn.assert_not_called()
        with self.assertRaisesRegex(build_script.BuildError, "not by the SimpleX key"):
            self.reproduce("simplex-chat/simplex-chat", "AA:BB")

    def test_reproduce_of_a_fork_reports_its_signer_without_checking_it(self):
        failed, warn = self.reproduce("owner/fork", "AA:BB")
        self.assertEqual(failed, [])
        self.assertIn("AA:BB, which is not checked", warn.call_args.args[0])

    def test_release_signer(self):
        recorder = Recorder(answers={"apksigner": "Signer #1 certificate SHA-256 digest: 3c52c4fd\n"})
        with patch_all("run", recorder):
            self.assertEqual(build_script.release_signer(self.container(), "simplex-aarch64.apk"), "3C:52:C4:FD")


class AndroidSigningTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.keystore = pathlib.Path(self.tmp.name) / "release.keystore"
        self.keystore.write_bytes(b"keystore")
        self.environ = {**SIGNING_ENV, "ANDROID_KEYSTORE": str(self.keystore)}

    def tearDown(self):
        self.tmp.cleanup()

    def test_unset_environment_builds_unsigned(self):
        self.assertIsNone(build_script.AndroidSigning.from_env({}))

    def test_partial_environment_names_what_is_missing(self):
        environ = dict(self.environ)
        del environ["ANDROID_KEY_PASSWORD"]
        with self.assertRaises(build_script.BuildError) as raised:
            build_script.AndroidSigning.from_env(environ)
        self.assertIn("missing: ANDROID_KEY_PASSWORD", str(raised.exception))

    def test_missing_keystore_file(self):
        with self.assertRaises(build_script.BuildError):
            build_script.AndroidSigning.from_env({**self.environ, "ANDROID_KEYSTORE": str(self.keystore) + ".gone"})

    def test_passwords_are_secrets_kept_out_of_env_and_repr(self):
        signing = build_script.AndroidSigning.from_env(self.environ)
        self.assertEqual(signing.env, {"ANDROID_KEYSTORE": "/keystore/release.keystore", "ANDROID_KEY_ALIAS": "simplex"})
        self.assertEqual(signing.secrets, {"ANDROID_KEYSTORE_PASSWORD": "store-secret", "ANDROID_KEY_PASSWORD": "key-secret"})
        self.assertNotIn("secret", repr(signing))

    def test_gradle_reads_the_same_variables(self):
        gradle = (REPO_ROOT / "apps/multiplatform/android/build.gradle.kts").read_text()
        names = ", ".join(f'"{name}"' for name in build_script.ANDROID_SIGNING_ENV)
        self.assertIn(f"val releaseSigningEnv = listOf({names})", gradle)

    def test_container_mounts_the_keystore_read_only(self):
        recorder = Recorder()
        signing = build_script.AndroidSigning.from_env(self.environ)
        root = self.keystore.parent
        with patch_all("run", recorder), build_script.android_container(root, root, False, signing):
            pass
        start = next(c for c in recorder.commands if c[1] == "run")
        self.assertIn(f"{self.keystore.resolve()}:/keystore/release.keystore:ro", start)

    def test_apk_build_passes_passwords_by_name_only(self):
        source = pathlib.Path(self.tmp.name)
        (source / "apps/multiplatform").mkdir(parents=True)
        (source / "apps/multiplatform/gradle.properties").write_text("android.version_code=387\n")
        (source / "simplex-chat-arm64-v8a.apk").write_bytes(b"signed apk")
        recorder = Recorder(answers={"find": "/g/bin/gradle\n"})
        signing = build_script.AndroidSigning.from_env(self.environ)
        with patch_all("run", recorder):
            build_script.build_apk(build_script.Container(source, {}), source, Arch.AARCH64, source, "x.apk", False, False, signing)
        index = next(i for i, c in enumerate(recorder.commands) if "./scripts/android/build-android.sh" in c)
        build = recorder.commands[index]
        self.assertIn("ANDROID_KEYSTORE=/keystore/release.keystore", build)
        self.assertIn("ANDROID_KEY_ALIAS=simplex", build)
        self.assertEqual(build[build.index("ANDROID_KEY_PASSWORD") - 1], "-e")
        self.assertFalse(any("secret" in a for a in build))
        self.assertEqual(recorder.envs[index], signing.secrets)

    def test_unsigned_build_warns(self):
        with mock.patch.dict(os.environ, {}, clear=True), patch_all("warn") as warn:
            self.assertIsNone(build_script.android_signing())
        self.assertIn("building unsigned", warn.call_args.args[0])


class VerifyApkTest(unittest.TestCase):
    def check(self, released_bytes, copier_outputs):
        """copier_outputs: what apksigcopier writes on each copy, in order."""
        with tempfile.TemporaryDirectory() as tmp:
            out = pathlib.Path(tmp)
            (out / "rel.apk").write_bytes(released_bytes)
            recorder = Recorder()
            outputs = list(copier_outputs)

            def fake_run(args, **kwargs):
                args = [str(a) for a in args]
                if "apksigcopier" in args:
                    (out / pathlib.Path(args[-1]).name).write_bytes(outputs.pop(0))
                return recorder(args, **kwargs)

            with patch_all("run", fake_run):
                result = build_script.verify_apk(build_script.Container(out, {}), out, "rel.apk", "built.apk")
            return result, recorder.steps()

    def test_agp_signed_release_verifies_without_realigning(self):
        result, steps = self.check(SIGNED, [SIGNED])
        self.assertTrue(result)
        self.assertEqual(steps, [["apksigcopier", "copy", "/out/rel.apk", "/out/built.apk", "/out/built.apk.signed"]])

    def test_flags_dropped_by_apksigcopier_are_restored(self):
        result, steps = self.check(SIGNED, [COPIED])
        self.assertTrue(result)
        self.assertEqual(len(steps), 1)

    def test_apksigner_padded_release_is_realigned_with_the_same_padding(self):
        padded = make_zip([("res/a.png", b"png", 0, APKSIGNER_PADDING)])
        other = make_zip([("res/a.png", b"png", 0, b"")])
        result, steps = self.check(padded, [other, padded])
        self.assertTrue(result)
        copy, zipalign, copy_aligned = steps
        self.assertIn("--pad-like-apksigner", zipalign)
        self.assertEqual(copy_aligned, ["apksigcopier", "copy", "/out/rel.apk", "/out/built.apk.aligned", "/out/built.apk.aligned.signed"])

    def test_zero_padded_release_is_realigned_without_apksigner_padding(self):
        other = make_zip([("classes.dex", b"other", 0, b"")])
        result, steps = self.check(SIGNED, [other, SIGNED])
        self.assertTrue(result)
        self.assertNotIn("--pad-like-apksigner", steps[1])

    def test_different_content_does_not_verify(self):
        other = make_zip([("classes.dex", b"other", 0, b"")])
        result, _ = self.check(SIGNED, [other, other])
        self.assertFalse(result)
