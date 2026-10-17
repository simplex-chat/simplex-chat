#!/usr/bin/env python3
"""Response-file and scope regressions for the failure-only compiler probe."""

import importlib.util
from pathlib import Path
import tempfile
import unittest


SPEC = importlib.util.spec_from_file_location("select_sigbus", Path(__file__).with_name("select-ghc-sigbus.py"))
SELECT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(SELECT)
CALL = ["--make", "-fbuilding-cabal-package", "-this-unit-id", "simplex-chat-7.1.0.11-unit", "-DIOS"]


class SelectionTests(unittest.TestCase):
    def test_ghc_quoting(self):
        # Unlike a shell parser, GHC removes backslashes inside either quote.
        self.assertEqual(SELECT.unescape_response("'a\\'b' \"c\\qd\" a\\ b \\\\\n\"\" tail\\"),
                         ["a'b", "cqd", "a b", "\\", "tail"])
        self.assertEqual(SELECT.unescape_response("'unterminated"), ["unterminated"])
        self.assertEqual(SELECT.unescape_response("a\x85b\x1cc\u2028d\u00a0e"), ["a\x85b\x1cc\u2028d", "e"])

    def test_scope(self):
        self.assertTrue(SELECT.selected(CALL, "7.1.0.11"))
        for arguments in (["--numeric-version"], CALL + ["-prof"],
                          [x for x in CALL if x != "-DIOS"], CALL + ["-this-unit-id", "another-unit"]):
            self.assertFalse(SELECT.selected(arguments, "7.1.0.11"))
        self.assertFalse(SELECT.selected(CALL, "7.1.0.12"))

    def test_one_level_response_and_original_bytes(self):
        with tempfile.TemporaryDirectory() as folder:
            response = Path(folder) / "compile.rsp"
            payload = "\n".join(CALL) + "\n'path with spaces'\n"
            response.write_text(payload)
            original = ["@" + str(response)]
            expanded, evidence = SELECT.expand_for_selection(original)
            self.assertTrue(SELECT.selected(expanded, "7.1.0.11"))
            self.assertEqual(expanded[-1], "path with spaces")
            self.assertEqual(original, ["@" + str(response)])
            self.assertEqual(response.read_text(), payload)
            self.assertEqual(evidence[0]["bytes"], len(payload.encode()))
            nested = Path(folder) / "nested.rsp"
            nested.write_text("@" + str(response))
            expanded, _ = SELECT.expand_for_selection(["@" + str(nested)])
            self.assertEqual(expanded, original)
            self.assertFalse(SELECT.selected(expanded, "7.1.0.11"))

    def test_bounded_regular_response(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "large.rsp"
            path.write_bytes(b"x" * (SELECT.MAX_BYTES + 1))
            with self.assertRaises(ValueError):
                SELECT.expand_for_selection(["@" + str(path)])
            link = Path(folder) / "link.rsp"
            link.symlink_to(path)
            with self.assertRaises(OSError):
                SELECT.expand_for_selection(["@" + str(link)])
            with self.assertRaises(ValueError):
                SELECT.expand_for_selection(["@relative.rsp"])


if __name__ == "__main__":
    unittest.main()
