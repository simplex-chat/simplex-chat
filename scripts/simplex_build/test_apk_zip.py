import io
import unittest
import zipfile

from .testing import (
    APKSIGNER_PADDING,
    COPIED,
    SIGNED,
    UTF8_FLAG,
    build_script,
    central_flags,
    local_flags,
    make_zip,
)


class ApkZipTest(unittest.TestCase):
    def test_apksigner_padding_is_detected_on_any_entry(self):
        apk = make_zip([("classes.dex", b"dex", 0, b""), ("res/a.png", b"png", 0, APKSIGNER_PADDING)])
        self.assertEqual(build_script.apk_padding(apk), "apksigner")

    def test_zip_without_the_extra_field_has_zero_padding(self):
        apk = make_zip([("classes.dex", b"dex", 0, b""), ("res/a.png", b"png", 0, b"\x00\x00")])
        self.assertEqual(build_script.apk_padding(apk), "zero")

    def test_non_zip_is_rejected(self):
        with self.assertRaises(build_script.BuildError):
            build_script.apk_padding(b"not a zip")

    def test_v1_signature_files_get_the_flags_in_both_headers(self):
        patched = build_script.copy_v1_flags(SIGNED, COPIED)
        for name in ("META-INF/KEY0.SF", "META-INF/KEY0.RSA", "META-INF/MANIFEST.MF"):
            self.assertEqual(local_flags(patched, name), UTF8_FLAG, name)
            self.assertEqual(central_flags(patched, name), UTF8_FLAG, name)

    def test_other_entries_keep_their_flags(self):
        patched = build_script.copy_v1_flags(SIGNED, COPIED)
        for name in ("classes.dex", "META-INF/versions/9/OSGI-INF/MANIFEST.MF"):
            self.assertEqual(local_flags(patched, name), 0, name)
            self.assertEqual(central_flags(patched, name), 0, name)

    def test_patched_zip_matches_the_signed_one_byte_for_byte(self):
        self.assertNotEqual(COPIED, SIGNED)
        self.assertEqual(build_script.copy_v1_flags(SIGNED, COPIED), SIGNED)

    def test_patched_zip_still_reads(self):
        with zipfile.ZipFile(io.BytesIO(build_script.copy_v1_flags(SIGNED, COPIED))) as z:
            self.assertIsNone(z.testzip())
            self.assertEqual(z.read("META-INF/KEY0.SF"), b"sf")

    def test_v1_signature_file_names(self):
        self.assertTrue(build_script.is_v1_signature_file("META-INF/MANIFEST.MF"))
        self.assertTrue(build_script.is_v1_signature_file("META-INF/KEY0.SF"))
        self.assertTrue(build_script.is_v1_signature_file("META-INF/CERT.EC"))
        self.assertFalse(build_script.is_v1_signature_file("META-INF/versions/9/OSGI-INF/MANIFEST.MF"))
        self.assertFalse(build_script.is_v1_signature_file("META-INF/androidx.core_core.version"))
        self.assertFalse(build_script.is_v1_signature_file("assets/KEY0.SF"))
