#!/usr/bin/env python3
from __future__ import annotations

import contextlib
import io
import os
import re
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parent))
import localizations as l10n  # noqa: E402

LOCALIZABLE = "en.lproj/Localizable.strings"
NOTE = "No comment provided by engineer."


def unit_xml(unit_id: str, source: str, target: str | None = None, state: str | None = None) -> str:
    lines = [f'      <trans-unit id="{unit_id}" xml:space="preserve">\n', f"        <source>{source}</source>\n"]
    if target is not None:
        attributes = f' state="{state}"' if state else ""
        lines.append(f"        <target{attributes}>{target}</target>\n" if target else f"        <target{attributes}></target>\n")
    lines += [f"        <note>{NOTE}</note>\n", "      </trans-unit>\n"]
    return "".join(lines)


def xliff_xml(language: str, units: list[str], original: str = LOCALIZABLE) -> str:
    return (
        '<?xml version="1.0" encoding="UTF-8"?>\n'
        '<xliff xmlns="urn:oasis:names:tc:xliff:document:1.2" version="1.2">\n'
        f'  <file original="{original}" source-language="en" target-language="{language}" datatype="plaintext">\n'
        "    <body>\n" + "".join(units) + "    </body>\n"
        "  </file>\n"
        "</xliff>\n"
    )


class RepositoryTestCase(unittest.TestCase):
    """Runs each test in a temporary directory laid out like the repository root."""

    def setUp(self) -> None:
        self.previous_cwd = os.getcwd()
        self.directory = tempfile.TemporaryDirectory()
        os.chdir(self.directory.name)
        for language in ("en", "de", "uk"):
            (l10n.IOS_DIR / f"{language}.lproj").mkdir(parents=True)
            l10n.xliff_path(language).parent.mkdir(parents=True)

    def tearDown(self) -> None:
        os.chdir(self.previous_cwd)
        self.directory.cleanup()

    def write(self, path: Path, text: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        l10n.write_file(path, text)


class PlaceholderTest(unittest.TestCase):
    def test_parse_sequential_positional_and_spelling(self) -> None:
        parsed = l10n.parse_placeholders("%@ sent %lld files, %02d left, %.1f%%")
        self.assertEqual(parsed.arguments, {1: "@", 2: "lld", 3: "02d", 4: ".1f"})
        self.assertTrue(parsed.escaped_percent)
        self.assertEqual(parsed.invalid, [])
        self.assertFalse(parsed.mixed)
        self.assertEqual(l10n.parse_placeholders("%2$@ %1$lld").arguments, {1: "lld", 2: "@"})

    def test_parse_invalid_mixed_and_conflicting(self) -> None:
        self.assertEqual(l10n.parse_placeholders("%их файл").invalid, ["%и"])
        self.assertEqual(l10n.parse_placeholders("%LLD groups").invalid, ["%L"])
        self.assertEqual(l10n.parse_placeholders("% % off").invalid, ["% %"])
        self.assertTrue(l10n.parse_placeholders("%1$@ and %@").mixed)
        self.assertEqual(l10n.parse_placeholders("%1$@ %1$d").invalid, ["%1$d"])
        self.assertFalse(l10n.parse_placeholders("100 percent").is_format)

    def test_problems_found_in_weblate(self) -> None:
        cases = {
            ("%lld messages moderated by %@", "%@ %lld üzenetet moderált"): "expected %1$lld, %2$@, found %1$@, %2$lld",
            ("%@ at %@:", "1$@, %2$@'de:"): "expected %1$@, %2$@, found %2$@",
            ("%d file(s) were deleted.", "%их файл(ів) було видалено."): "invalid placeholders '%и'",
            ("Desktop app version %@ is not compatible.", 'versión %" no es compatible.'): "invalid placeholders '%\"'",
            ("Error: %@", "错误： ％@"): "expected %1$@, found none",
            ("You are already connecting!", "Ви вже підключаєтеся до %@."): "placeholders not in the source: %1$@",
            ("%lld new interface languages", "%d nové jazyky rozhraní"): "expected %1$lld, found %1$d",
            ("changed your role to %@", "你修改了 %1$@ 你的身份為 %@"): "positional and non-positional placeholders mixed",
            ("Sent at: %@", "Posláno v: % @"): "expected %1$@, found %1$ @",
            ("%1$@ changed to %2$@", "Role %1$@ byla změněna na %2$ @"): "expected %1$@, %2$@, found %1$@, %2$ @",
            ("%1$02d hrs", "%1$2d h"): "expected %1$02d, found %1$2d",
        }
        for (source, target), expected in cases.items():
            with self.subTest(target=target):
                self.assertEqual(l10n.placeholder_problem(source, target), expected)

    def test_valid_translations_pass(self) -> None:
        cases = [
            ("%lld messages moderated by %@", "%2$@ %1$lld üzenetet moderált"),
            ("%1$@ at %2$@:", "%1$@, %2$@'de:"),
            ("%lld%% off", "%lld%% Rabatt"),
            ("Battery at 100%", "Akku bei 100 %"),
            ("No placeholders", "Keine Platzhalter"),
            ("Join [group](simplex:/g#a%3Db%2F) by %@", "%@ tritt [Gruppe](simplex:/g#a%3Db%2F) bei"),
        ]
        for source, target in cases:
            with self.subTest(target=target):
                self.assertIsNone(l10n.placeholder_problem(source, target))

    def test_describe_arguments(self) -> None:
        self.assertEqual(l10n.describe_arguments({2: "@", 1: "lld"}), "%1$lld, %2$@")
        self.assertEqual(l10n.describe_arguments({}), "none")


class LinkTest(unittest.TestCase):
    SOURCE = "Improved [directory bot](simplex:/contact#/?v=1-4%26dh%3DMCow) and https://simplex.chat/docs."

    def test_links_and_masking(self) -> None:
        self.assertEqual(l10n.links(self.SOURCE), (["simplex:/contact#/?v=1-4%26dh%3DMCow"], ["https://simplex.chat/docs."]))
        self.assertEqual(l10n.links("见 https://github.com/simplex-chat）！"), ([], ["https://github.com/simplex-chat"]))
        self.assertEqual(l10n.mask_links(self.SOURCE), "Improved [directory bot](<link>) and <link>")

    def test_link_problems(self) -> None:
        intact = "Подобрен [бот](simplex:/contact#/?v=1-4%26dh%3DMCow) и https://simplex.chat/docs."
        broken = "Подобрен [бот](simplex:/contact#/?v=1-4% 26dh%3DMCow) и https://simplex.chat/docs."
        self.assertIsNone(l10n.link_problem(self.SOURCE, intact))
        self.assertEqual(l10n.link_problem(self.SOURCE, broken), "markdown link changed or missing: 'simplex:/contact#/?v=1-4%26dh%3DMCow'")
        self.assertEqual(l10n.link_problem("No link", "See https://evil.example"), "URL not in the source: 'https://evil.example'")
        full_width = "Подобрен [бот]（simplex:/contact#/?v=1-4%26dh%3DMCow） и https://simplex.chat/docs."
        self.assertEqual(l10n.link_problem(self.SOURCE, full_width), "markdown link changed or missing: 'simplex:/contact#/?v=1-4%26dh%3DMCow'")
        self.assertEqual(l10n.link_problem("[a](https://x.example) [b](https://x.example)", "[a](https://x.example)"), "markdown link not in the source: 'https://x.example'")
        self.assertIsNone(l10n.link_problem("No link", "Kein Link"))

    def test_broken_link_is_not_reported_as_placeholder(self) -> None:
        broken = "Подобрен [бот](simplex:/contact#/?v=1-4% 26dh%3DMCow) и https://simplex.chat/docs."
        self.assertIsNone(l10n.placeholder_problem(self.SOURCE, broken))
        problems = l10n.translation_problems(Path("bg.xliff"), "key", self.SOURCE, broken)
        self.assertEqual([p.kind for p in problems], [l10n.LINK])


class StringsParserTest(unittest.TestCase):
    def test_parses_escapes_and_comments(self) -> None:
        text = '/* note */\n"a\\"b" = "line\\nnext \\U00e9";\n// comment\n"_italic_" = "\\\\_kursiv_";\n'
        self.assertEqual(l10n.parse_strings(text), [('a"b', "line\nnext é"), ("_italic_", "\\_kursiv_")])

    def test_empty_file(self) -> None:
        self.assertEqual(l10n.parse_strings("/* only a comment */\n"), [])

    def test_syntax_errors(self) -> None:
        for text in ['"key" = "value"', '"key" "value";', '"unterminated = "x";\n', "/* open"]:
            with self.subTest(text=text), self.assertRaises(l10n.StringsSyntaxError):
                l10n.parse_strings(text)


class XliffTransformTest(unittest.TestCase):
    def test_split_and_join_keep_the_text_byte_for_byte(self) -> None:
        text = xliff_xml("de", [unit_xml("A", "A", "Ä", "translated"), unit_xml("B", "B")])
        segments = l10n.split_xliff(text)
        self.assertEqual(l10n.join_xliff(segments), text)
        blocks = list(l10n.unit_blocks(segments))
        self.assertEqual([b.key for b in blocks], [(LOCALIZABLE, "A"), (LOCALIZABLE, "B")])
        self.assertEqual(blocks[0].unit, l10n.Unit("A", "Ä"))
        self.assertIsNone(blocks[1].unit.target)

    def test_split_rejects_units_sharing_lines(self) -> None:
        text = xliff_xml("de", ['      <trans-unit id="A"><source>A</source></trans-unit><trans-unit id="B"><source>B</source></trans-unit>\n'])
        with self.assertRaises(ValueError):
            l10n.split_xliff(text)

    def test_strip_empty_targets(self) -> None:
        segments = l10n.split_xliff(xliff_xml("de", [unit_xml("A", "A", "", "needs-translation"), unit_xml("B", "B", "Bee")]))
        self.assertEqual(l10n.strip_empty_targets(segments), 1)
        self.assertEqual(l10n.join_xliff(segments), xliff_xml("de", [unit_xml("A", "A"), unit_xml("B", "B", "Bee")]))
        self.assertEqual(l10n.strip_empty_targets(segments), 0)

    def test_drop_untranslated_units(self) -> None:
        segments = l10n.split_xliff(xliff_xml("de", [unit_xml("A", "A"), unit_xml("B", "B", "Bee"), unit_xml("C", "C", "")]))
        self.assertEqual(l10n.join_xliff(l10n.drop_untranslated_units(segments)), xliff_xml("de", [unit_xml("B", "B", "Bee")]))

    def test_multiline_target_counts_as_translated(self) -> None:
        unit = '      <trans-unit id="M" xml:space="preserve">\n        <source>M</source>\n        <target>\nzwei Zeilen</target>\n      </trans-unit>\n'
        text = xliff_xml("de", [unit])
        segments = l10n.split_xliff(text)
        self.assertEqual(l10n.join_xliff(l10n.drop_untranslated_units(segments)), text)
        self.assertEqual(l10n.saved_translations(segments)[(LOCALIZABLE, "M")].lines, ["        <target>\n", "zwei Zeilen</target>\n"])

    def test_restore_translations_dropped_by_xcode(self) -> None:
        weblate = xliff_xml("de", [unit_xml("A", "A", "Entwurf", "needs-translation"), unit_xml("B", "B", "Bee"), unit_xml("C", "Old C", "Zeh")])
        exported = l10n.split_xliff(xliff_xml("de", [unit_xml("A", "A"), unit_xml("B", "B", "Bee"), unit_xml("C", "New C")]))
        self.assertEqual(l10n.restore_translations(exported, l10n.saved_translations(l10n.split_xliff(weblate))), 1)
        expected = xliff_xml("de", [unit_xml("A", "A", "Entwurf", "needs-translation"), unit_xml("B", "B", "Bee"), unit_xml("C", "New C")])
        self.assertEqual(l10n.join_xliff(exported), expected)

    def test_restore_ignores_duplicate_ids_already_translated(self) -> None:
        text = xliff_xml("ar", [unit_xml("Invite", "Invite"), unit_xml("Invite", "Invite", "دعوة")])
        segments = l10n.split_xliff(text)
        self.assertEqual(l10n.restore_translations(segments, l10n.saved_translations(segments)), 0)
        self.assertEqual(l10n.join_xliff(segments), text)

    def test_translation_changes(self) -> None:
        path = Path("de.xliff")
        before = xliff_xml("de", [unit_xml("A", "A", "Ah"), unit_xml("B", "B", "Bee"), unit_xml("Gone", "Gone", "Weg"), unit_xml("U", "U")])
        after = l10n.split_xliff(xliff_xml("de", [unit_xml("B", "B", "Beh")]))
        source_keys = {(LOCALIZABLE, "A"), (LOCALIZABLE, "B"), (LOCALIZABLE, "U")}
        problems, removed = l10n.translation_changes(path, l10n.saved_translations(l10n.split_xliff(before)), after, source_keys, {})
        self.assertEqual(removed, 1)
        self.assertEqual([(p.kind, p.key, p.text) for p in problems], [(l10n.LOST, "A", "Ah"), (l10n.CHANGED, "B", "Beh")])


class FilesTest(RepositoryTestCase):
    def test_app_languages_put_the_source_first(self) -> None:
        self.assertEqual(l10n.app_languages(), ["en", "de", "uk"])

    def test_language_of(self) -> None:
        self.assertEqual(l10n.language_of(Path("apps/ios/SimpleX SE/zh-Hans.lproj/Localizable.strings")), "zh-Hans")
        self.assertEqual(l10n.language_of(l10n.xliff_path("pt-BR")), "pt-BR")

    def test_check_files_reports_each_kind(self) -> None:
        self.write(l10n.xliff_path("en"), xliff_xml("en", [unit_xml("%@ server", "%@ server", "%@ server")]))
        self.write(l10n.xliff_path("de"), xliff_xml("de", [unit_xml("%@ server", "%@ server", "Server"), unit_xml("Ok", "Ok", "")]))
        self.write(l10n.IOS_DIR / "de.lproj/Localizable.strings", '"Blank" = "";\n"%@ server" = "%@ Server";\n')
        self.write(l10n.IOS_DIR / "uk.lproj/Localizable.strings", '"Broken" = "x"\n')
        self.write(l10n.xliff_path("uk"), "<xliff><file>\n")
        self.write(l10n.IOS_DIR / "en.lproj/Localizable.strings", '"Ignored" = "";\n')
        self.write(l10n.xcloc_path("de") / "Source Contents/en.lproj/Localizable.strings", '"Ignored" = "";\n')
        stats: dict[str, l10n.LanguageStats] = {}
        with mock.patch.object(l10n.shutil, "which", return_value=None):
            problems = l10n.check_files(stats, {})
        self.assertEqual(
            [(p.kind, p.language, p.key) for p in problems],
            [
                (l10n.STALE, "de", "Ok"),
                (l10n.PLACEHOLDER, "de", "%@ server"),
                (l10n.BLANK, "de", "Ok"),
                (l10n.SYNTAX, "uk", ""),
                (l10n.BLANK, "de", "Blank"),
                (l10n.SYNTAX, "uk", ""),
            ],
        )
        self.assertEqual(stats["de"].translated, 1)

    def test_strings_problems_already_in_the_xliff_are_listed_once(self) -> None:
        self.write(l10n.xliff_path("de"), xliff_xml("de", [unit_xml("%@ server", "%@ server", "Server")]))
        self.write(l10n.IOS_DIR / "de.lproj/Localizable.strings", '"%@ server" = "Server";\n"%@ file" = "Datei";\n')
        with mock.patch.object(l10n.shutil, "which", return_value=None):
            problems = l10n.check_files({}, {})
        self.assertEqual([(p.path.suffix, p.key) for p in problems], [(".xliff", "%@ server"), (".strings", "%@ file")])

    def test_check_xliff_reports_duplicates_and_stale_units(self) -> None:
        path = l10n.xliff_path("de")
        self.write(path, xliff_xml("de", [unit_xml("A", "A"), unit_xml("A", "A", "Ah"), unit_xml("Old", "Old", "Alt")]))
        problems, translated = l10n.check_xliff(path, {(LOCALIZABLE, "A")}, {})
        self.assertEqual(translated, 2)
        self.assertEqual(
            [(p.kind, p.key, p.detail) for p in problems],
            [
                (l10n.DUPLICATE, "A", f"2 units with this id in {LOCALIZABLE}"),
                (l10n.STALE, "Old", "not in en.xliff, Xcode's export removes it"),
            ],
        )
        self.assertEqual(l10n.check_xliff(path, set(), {})[0][0].kind, l10n.DUPLICATE)
        self.assertEqual(len(l10n.check_xliff(path, set(), {})[0]), 1)

    def test_lint_with_plutil_reports_failures(self) -> None:
        output = "a.strings: OK\nb.strings: Unexpected character \" at line 2\n"
        completed = mock.Mock(stdout=output, stderr="")
        with mock.patch.object(l10n.shutil, "which", return_value="/usr/bin/plutil"), mock.patch.object(l10n.subprocess, "run", return_value=completed) as run:
            problems = l10n.lint_with_plutil([Path("a.strings"), Path("b.strings")])
        run.assert_called_once()
        self.assertEqual([(p.kind, str(p.path), p.detail) for p in problems], [(l10n.SYNTAX, "b.strings", 'plutil: Unexpected character " at line 2')])
        with mock.patch.object(l10n.shutil, "which", return_value=None):
            self.assertEqual(l10n.lint_with_plutil([Path("a.strings")]), [])


class ReportTest(unittest.TestCase):
    def test_report_exit_codes_and_blank_grouping(self) -> None:
        path = Path("apps/ios/SimpleX Localizations/de.xcloc/Localized Contents/de.xliff")
        problems = [l10n.Problem(l10n.BLANK, path, f"Key {i}", "empty target") for i in range(5)]
        problems.append(l10n.Problem(l10n.PLACEHOLDER, path, "%@ server", "expected %1 object, found none", "Server"))
        output = io.StringIO()
        with contextlib.redirect_stdout(output), mock.patch.object(l10n, "app_languages", return_value=["en", "de"]):
            self.assertEqual(l10n.report({"de": l10n.LanguageStats(translated=7)}, problems, {"en", "de"}), 1)
        text = output.getvalue()
        self.assertIn("5 x empty target", text)
        self.assertIn("'Key 0', 'Key 1', 'Key 2' and 2 more", text)
        self.assertIn("'%@ server'", text)
        self.assertIn("6 problem(s): 1 placeholder, 5 blank", text)
        with contextlib.redirect_stdout(io.StringIO()), mock.patch.object(l10n, "app_languages", return_value=["en"]):
            self.assertEqual(l10n.report({}, [], {"en"}), 0)

    def test_warnings_do_not_fail(self) -> None:
        path = Path("apps/ios/SimpleX Localizations/ar.xcloc/Localized Contents/ar.xliff")
        problems = [l10n.Problem(l10n.STALE, path, f"Old {i}", "not in en.xliff") for i in range(4)]
        problems.append(l10n.Problem(l10n.DUPLICATE, path, "A", "2 units with this id"))
        output = io.StringIO()
        with contextlib.redirect_stdout(output), mock.patch.object(l10n, "app_languages", return_value=["en"]):
            self.assertEqual(l10n.report({}, problems, {"en"}), 0)
        text = output.getvalue()
        self.assertIn("4 x not in en.xliff", text)
        self.assertIn("'Old 0', 'Old 1', 'Old 2' and 1 more", text)
        self.assertIn("5 warning(s): 1 duplicate, 4 stale", text)
        self.assertIn("No problems found.", text)

    def test_preview_shortens_long_keys(self) -> None:
        self.assertEqual(l10n.preview("short"), "short")
        self.assertEqual(l10n.preview("x" * 150), "x" * l10n.KEY_PREVIEW_LENGTH + "…")

    def test_format_duration(self) -> None:
        self.assertEqual(l10n.format_duration(4.4), "4s")
        self.assertEqual(l10n.format_duration(125), "2m 05s")

    def test_paint_respects_color_setting(self) -> None:
        with mock.patch.object(l10n, "USE_COLOR", False):
            self.assertEqual(l10n.paint("x", l10n.RED), "x")
        with mock.patch.object(l10n, "USE_COLOR", True):
            self.assertEqual(l10n.paint("x", l10n.BOLD, l10n.RED), "\033[1;31mx\033[0m")


class UpdateTest(RepositoryTestCase):
    def setUp(self) -> None:
        super().setUp()
        self.pbxproj = b"// original project\n"
        self.write(l10n.PBXPROJ, self.pbxproj.decode())
        self.weblate_de = xliff_xml("de", [unit_xml("A", "A", "Entwurf", "needs-translation"), unit_xml("B", "B", ""), unit_xml("%@ x", "%@ x", "%@ y")])
        self.write(l10n.xliff_path("de"), self.weblate_de)
        self.write(l10n.xliff_path("uk"), xliff_xml("uk", [unit_xml("A", "A", "А")]))
        self.write(l10n.xliff_path("en"), xliff_xml("en", [unit_xml("A", "A", "A"), unit_xml("B", "B", "B"), unit_xml("%@ x", "%@ x", "%@ x")]))
        self.imported: dict[str, str] = {}

    def fake_xcodebuild(self, arguments: list[str], log: Path) -> None:
        l10n.write_file(l10n.PBXPROJ, "// rewritten by Xcode\n")
        if arguments[0] == "-importLocalizations":
            language = Path(arguments[arguments.index("-localizationPath") + 1]).stem
            self.imported[language] = l10n.read_file(l10n.xliff_path(language))
            return
        languages = [arguments[i + 1] for i, argument in enumerate(arguments) if argument == "-exportLanguage"]
        self.assertEqual(languages, ["en", "de", "uk"])
        self.write(l10n.xliff_path("de"), xliff_xml("de", [unit_xml("A", "A"), unit_xml("B", "B"), unit_xml("%@ x", "%@ x", "%@ y"), unit_xml("New", "New")]))

    def run_update(self) -> tuple[int, str]:
        output = io.StringIO()
        with contextlib.ExitStack() as stack:
            stack.enter_context(mock.patch.object(l10n, "require_update_environment"))
            stack.enter_context(mock.patch.object(l10n, "update_pbxproj_libraries"))
            stack.enter_context(mock.patch.object(l10n, "xcodebuild", side_effect=self.fake_xcodebuild))
            stack.enter_context(mock.patch.object(l10n.shutil, "which", return_value=None))
            stack.enter_context(contextlib.redirect_stdout(output))
            code = l10n.update()
        return code, output.getvalue()

    def test_update_round_trip(self) -> None:
        code, output = self.run_update()
        self.assertEqual(code, 0, output)
        self.assertEqual(self.imported["de"], xliff_xml("de", [unit_xml("A", "A", "Entwurf", "needs-translation"), unit_xml("B", "B"), unit_xml("%@ x", "%@ x", "%@ y")]))
        self.assertEqual(
            l10n.read_file(l10n.xliff_path("de")),
            xliff_xml("de", [unit_xml("A", "A", "Entwurf", "needs-translation"), unit_xml("%@ x", "%@ x", "%@ y")]),
        )
        self.assertEqual(l10n.PBXPROJ.read_bytes(), self.pbxproj)
        self.assertIn("removed 1 empty targets", output)
        self.assertIn("No problems found.", output)

    def test_update_restores_the_project_when_xcode_fails(self) -> None:
        def failing_xcodebuild(arguments: list[str], log: Path) -> None:
            l10n.write_file(l10n.PBXPROJ, "// rewritten by Xcode\n")
            raise l10n.UpdateError("xcodebuild -importLocalizations failed")

        with mock.patch.object(l10n, "require_update_environment"), mock.patch.object(l10n, "update_pbxproj_libraries"), mock.patch.object(
            l10n, "xcodebuild", side_effect=failing_xcodebuild
        ), contextlib.redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(l10n.UpdateError, "git checkout -- apps/ios"):
                l10n.update()
        self.assertEqual(l10n.PBXPROJ.read_bytes(), self.pbxproj)

    def test_update_reports_a_malformed_xliff(self) -> None:
        self.write(l10n.xliff_path("uk"), "<xliff><file>\n")
        with mock.patch.object(l10n, "require_update_environment"), contextlib.redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(l10n.UpdateError, "uk.xliff: "):
                l10n.update()

    def test_require_update_environment_outside_macos(self) -> None:
        with mock.patch.object(l10n.platform, "system", return_value="Linux"):
            with self.assertRaisesRegex(l10n.UpdateError, "macOS with Xcode"):
                l10n.require_update_environment()


class SwiftUsageTest(RepositoryTestCase):
    def test_swift_string_literals(self) -> None:
        swift = 'let a = "\\n"\nText("Hello, \\"you\\"")\nlet b = "\\(count) items"\nalert("Error\\u{2026}")\n'
        self.write(l10n.IOS_DIR / "Shared/View.swift", swift)
        self.assertEqual(
            l10n.swift_string_literals(),
            {'Hello, "you"': "apps/ios/Shared/View.swift:2", "Error\u2026": "apps/ios/Shared/View.swift:4"},
        )

    def test_unescape_swift(self) -> None:
        self.assertEqual(l10n.unescape_swift('a\\"b\\\\ \\u{1F600}\\t'), 'a"b\\ \U0001F600\t')

    def test_stale_units_still_used_in_code(self) -> None:
        path = l10n.xliff_path("de")
        self.write(path, xliff_xml("de", [unit_xml("Gone", "Gone", "Weg"), unit_xml("Used", "Used", "Benutzt")]))
        problems, _ = l10n.check_xliff(path, {(LOCALIZABLE, "Other")}, {"Used": "apps/ios/A.swift:3"})
        self.assertEqual(
            [(p.kind, p.key, p.detail) for p in problems],
            [
                (l10n.STALE, "Gone", "not in en.xliff, Xcode's export removes it"),
                (l10n.IN_CODE, "Used", "not in en.xliff, but the Swift code has this string at apps/ios/A.swift:3"),
            ],
        )

    def test_removed_translation_still_used_in_code(self) -> None:
        saved = l10n.saved_translations(l10n.split_xliff(xliff_xml("de", [unit_xml("Used", "Used", "Benutzt"), unit_xml("Gone", "Gone", "Weg")])))
        problems, removed = l10n.translation_changes(Path("de.xliff"), saved, [], set(), {"Used": "apps/ios/A.swift:3"})
        self.assertEqual(removed, 2)
        self.assertEqual([(p.kind, p.key, p.text) for p in problems], [(l10n.IN_CODE, "Used", "Benutzt")])


BUILD_SCRIPT = """      val possibleFormat = listOf("s", "d", "1\\$s", "2\\$s", "1\\$d", "1\\$02d", "f")\n"""


def kotlin_xml(strings: list[str]) -> str:
    return "<resources>\n" + "".join(f"  {string}\n" for string in strings) + "</resources>\n"


class KotlinRulesTest(unittest.TestCase):
    POSSIBLE = ["s", "d", "1$s", "2$s", "3$s", "4$s", "1$d", "2$d", "3$d", "4$d", "1$02d", "2$02d", "3$02d", "2s", "f"]

    def formats(self, value: str) -> list[str]:
        return l10n.kotlin_formats(f'<string name="x">{value}</string>', self.POSSIBLE)

    def test_kotlin_formats(self) -> None:
        self.assertEqual(self.formats("No placeholders"), [])
        self.assertEqual(self.formats("%s sent %d"), ["s", "d"])
        self.assertEqual(self.formats("%2$s by %1$s"), ["1$s", "2$s"])
        self.assertEqual(self.formats("%1$02d:%2$02d"), ["1$02d", "2$02d"])

    def test_kotlin_formats_rejected_by_adjust_formatting(self) -> None:
        cases = {
            "% s": "unknown placeholder '% s', adjustFormatting fails the build",
            "50%% off": "unknown placeholder '%% o', adjustFormatting fails the build",
            "%1$s and %s": "positional and non-positional placeholders mixed, adjustFormatting fails the build",
            "%5$s": "unknown placeholder '%5$s', adjustFormatting fails the build",
        }
        for value, expected in cases.items():
            with self.subTest(value=value), self.assertRaisesRegex(l10n.KotlinFormatError, re.escape(expected)):
                self.formats(value)

    def test_kotlin_html_problem(self) -> None:
        valid = [
            '<string name="x"><![CDATA[Protected by <b>encryption</b>.]]></string>',
            '<string name="x">Plain text</string>',
            '<string name="x"><![CDATA[<font color="#0088ff">Blue</font>]]></string>',
        ]
        for element in valid:
            with self.subTest(element=element):
                self.assertIsNone(l10n.kotlin_html_problem(element))
        invalid = [
            '<string name="x"><![CDATA[Open <a href="https://x.example">link</a>]]></string>',
            '<string name="x"><![CDATA[Broken <b>bold]]></string>',
            '<string name="x">1 &lt; 2</string>',
        ]
        for element in invalid:
            with self.subTest(element=element):
                self.assertEqual(
                    l10n.kotlin_html_problem(element),
                    "HTML other than balanced <b>, <i>, <u> or <font> tags, adjustFormatting fails the build",
                )

    def test_kotlin_text(self) -> None:
        self.assertEqual(l10n.kotlin_text("<![CDATA[<b>Don\\'t</b>]]> &amp; \\\"x\\\" \\u00e9"), '<b>Don\'t</b> & "x" é')

    def test_substring_after(self) -> None:
        self.assertEqual(l10n.substring_after("a%b%c", "%"), "b%c")
        self.assertEqual(l10n.substring_after("abc", "%"), "abc")

    def test_remove_cdata(self) -> None:
        self.assertEqual(l10n.remove_cdata('<string name="x"><![CDATA[<b>a</b>]]></string>'), '<string name="x"><b>a</b></string>')
        self.assertEqual(l10n.remove_cdata("<string>a</string>"), "<string>a</string>")


class KotlinFilesTest(RepositoryTestCase):
    def setUp(self) -> None:
        super().setUp()
        self.write(l10n.KOTLIN_BUILD_SCRIPT, BUILD_SCRIPT)
        self.write(
            l10n.KOTLIN_RESOURCES / "base/strings.xml",
            kotlin_xml(
                [
                    '<string name="sent">%1$s sent %2$s</string>',
                    '<string name="hello">Hello</string>',
                    '<string name="docs">Read [docs](https://simplex.chat/docs)</string>',
                    '<string name="bold"><![CDATA[Be <b>bold</b>]]></string>',
                ]
            ),
        )

    def test_possible_formats_come_from_the_build_script(self) -> None:
        self.assertEqual(l10n.kotlin_possible_formats(), ["s", "d", "1$s", "2$s", "1$d", "1$02d", "f"])
        self.write(l10n.KOTLIN_BUILD_SCRIPT, "// nothing\n")
        with self.assertRaisesRegex(l10n.UpdateError, "possibleFormat not found"):
            l10n.kotlin_possible_formats()

    def test_check_kotlin_files_reports_each_kind(self) -> None:
        self.write(
            l10n.KOTLIN_RESOURCES / "de/strings.xml",
            kotlin_xml(
                [
                    '<string name="sent">%2$s von %1$s gesendet</string>',
                    '<string name="hello"></string>',
                    '<string name="hello">Hallo</string>',
                    '<string name="docs">Lies die [Doku] (https://simplex.chat/docs)</string>',
                    "<string name=\"bold\"><![CDATA[Sei <b>mutig</b>]]> l'ami</string>",
                    '<string name="removed">Weg</string>',
                ]
            ),
        )
        self.write(l10n.KOTLIN_RESOURCES / "fr/strings.xml", kotlin_xml(['<string name="sent">%1$s a envoyé % s</string>']))
        self.write(l10n.KOTLIN_RESOURCES / "uk/strings.xml", "<resources><string name='a'>\n")
        stats: dict[str, l10n.LanguageStats] = {}
        problems = l10n.check_kotlin_files(stats)
        self.assertEqual(
            [(p.language, p.kind, p.key) for p in problems],
            [
                ("de", l10n.BLANK, "hello"),
                ("de", l10n.DUPLICATE, "hello"),
                ("de", l10n.LINK, "docs"),
                ("de", l10n.HTML, "bold"),
                ("de", l10n.SYNTAX, "bold"),
                ("de", l10n.UNKNOWN, "removed"),
                ("fr", l10n.PLACEHOLDER, "sent"),
                ("uk", l10n.SYNTAX, ""),
            ],
        )
        self.assertEqual(problems[6].detail, "unknown placeholder '% s', adjustFormatting fails the build")
        self.assertEqual((stats["de"].translated, stats["fr"].translated, stats["uk"].translated), (6, 1, 0))

    def test_placeholder_mismatch_against_the_base(self) -> None:
        self.write(l10n.KOTLIN_RESOURCES / "de/strings.xml", kotlin_xml(['<string name="sent">%1$s gesendet</string>']))
        problems = l10n.check_kotlin_files({})
        self.assertEqual([(p.kind, p.detail) for p in problems], [(l10n.PLACEHOLDER, "expected %1$s, %2$s, found %1$s")])

    def test_missing_base_file(self) -> None:
        (l10n.KOTLIN_RESOURCES / "base/strings.xml").unlink()
        with self.assertRaisesRegex(l10n.UpdateError, "base/strings.xml not found"):
            l10n.check_kotlin_files({})

    def test_language_of_strings_xml(self) -> None:
        self.assertEqual(l10n.language_of(l10n.KOTLIN_RESOURCES / "pt-rBR/strings.xml"), "pt-rBR")


class SummaryTest(unittest.TestCase):
    def test_empty_columns_are_hidden(self) -> None:
        path = Path("apps/ios/SimpleX Localizations/de.xcloc/Localized Contents/de.xliff")
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            l10n.print_summary({"de": l10n.LanguageStats(translated=3)}, [l10n.Problem(l10n.LINK, path, "k", "d")], {"de"})
        header = output.getvalue().strip().splitlines()[0].split()
        self.assertEqual(header, ["language", "in", "app", "translated", "link"])


class MainTest(RepositoryTestCase):
    def test_check_command_exit_code(self) -> None:
        self.write(l10n.xliff_path("de"), xliff_xml("de", [unit_xml("%@ x", "%@ x", "y")]))
        with contextlib.redirect_stdout(io.StringIO()), mock.patch.object(l10n.shutil, "which", return_value=None):
            self.assertEqual(l10n.main(["check"]), 1)

    def test_check_kotlin_command_exit_code(self) -> None:
        self.write(l10n.KOTLIN_BUILD_SCRIPT, BUILD_SCRIPT)
        self.write(l10n.KOTLIN_RESOURCES / "base/strings.xml", kotlin_xml(['<string name="a">%s</string>']))
        self.write(l10n.KOTLIN_RESOURCES / "de/strings.xml", kotlin_xml(['<string name="a">%s</string>']))
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(l10n.main(["check-kotlin"]), 0)
        self.write(l10n.KOTLIN_RESOURCES / "de/strings.xml", kotlin_xml(['<string name="a">kein</string>']))
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(l10n.main(["check-kotlin"]), 1)

    def test_update_error_exit_code(self) -> None:
        stderr = io.StringIO()
        with mock.patch.object(l10n.platform, "system", return_value="Linux"), contextlib.redirect_stderr(stderr):
            self.assertEqual(l10n.main(["update"]), 2)
        self.assertIn("update needs macOS", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()
