import io
import pathlib
import shutil
import tarfile
import tempfile
import unittest

from .testing import REPO_ROOT, build_script, patch_all


class OpenSslTest(unittest.TestCase):
    def tarball(self, path):
        member = tarfile.TarInfo(f"openssl-{build_script.OPENSSL_VERSION}/Configure")
        member.size = 4
        with tarfile.open(path, "w:gz") as tar:
            tar.addfile(member, io.BytesIO(b"perl"))

    def test_fetch_extracts_the_checked_tarball(self):
        with tempfile.TemporaryDirectory() as tmp:
            work = pathlib.Path(tmp)
            self.tarball(work / "fixture.tar.gz")
            digest = build_script.sha256(work / "fixture.tar.gz")

            def fixture_download(url, target):
                shutil.copy(work / "fixture.tar.gz", target)

            with patch_all("download", fixture_download), patch_all("OPENSSL_SHA256", digest):
                source = build_script.fetch_openssl(work)
            self.assertEqual((source / "Configure").read_text(), "perl")

    def test_fetch_rejects_a_changed_tarball(self):
        with tempfile.TemporaryDirectory() as tmp:
            work = pathlib.Path(tmp)
            with (
                patch_all("download", lambda url, target: self.tarball(target)),
                self.assertRaises(build_script.BuildError) as raised,
            ):
                build_script.fetch_openssl(work)
            self.assertIn("pinned SHA-256", str(raised.exception))
            self.assertFalse((work / f"openssl-{build_script.OPENSSL_VERSION}").exists())

    def test_build_installs_only_when_asked(self):
        build = build_script.openssl_build("darwin64-arm64-cc", "/w/openssl", "/etc/ssl", install=True)
        self.assertEqual(
            build[0],
            ("perl", "Configure", "darwin64-arm64-cc", "--prefix=/w/openssl", "--openssldir=/etc/ssl")
            + ("--libdir=lib", "no-docs", "no-tests"),
        )
        self.assertEqual(build[-1], ("make", "install_sw"))
        self.assertNotIn(("make", "install_sw"), build_script.openssl_build("mingw64", "/p", "/d", install=False))

    def test_flake_pins_the_same_openssl(self):
        flake = (REPO_ROOT / "flake.nix").read_text()
        pin = flake[flake.index("let pinOpenssl") : flake.index("}); in", flake.index("let pinOpenssl"))]
        self.assertIn(f'version = "{build_script.OPENSSL_VERSION}";', pin)
        self.assertIn(f'url = "{build_script.OPENSSL_URL}";', pin)
        self.assertIn(f'sha256 = "{build_script.OPENSSL_SHA256}";', pin)
