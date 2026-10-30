#!/usr/bin/env python3
"""Round-trip the iOS translations between Weblate's XLIFF files and the app.

  update  import Weblate's XLIFF files into the app and export them back with Xcode,
          then lint and check the result (macOS with Xcode)
  check   lint the .strings files and report blank translations and placeholder errors
          (any platform)

Run from the repository root. Exits with 1 when problems are found, 2 on errors.
"""

from __future__ import annotations

import argparse
import contextlib
import os
import platform
import re
import shutil
import subprocess
import sys
import tempfile
import time
import xml.etree.ElementTree as ET
from collections.abc import Iterator
from dataclasses import dataclass, field
from pathlib import Path
from typing import Union

IOS_DIR = Path("apps/ios")
PROJECT = IOS_DIR / "SimpleX.xcodeproj"
PBXPROJ = PROJECT / "project.pbxproj"
LOCALIZATIONS_DIR = IOS_DIR / "SimpleX Localizations"
UPDATE_PBXPROJ = Path("scripts/ios/update-pbxproj.sh")
SOURCE_LANGUAGE = "en"
XLIFF_NS = "{urn:oasis:names:tc:xliff:document:1.2}"
LOG_TAIL_LINES = 40
GROUP_EXAMPLES = 3

UnitKey = tuple[str, str]

UNIT_START = re.compile(r"^\s*<trans-unit[\s>]")
UNIT_END = "</trans-unit>"
TARGET_START = re.compile(r"<target[\s>/]")
TARGET_END = re.compile(r"</target>|<target[^>]*/>")
EMPTY_TARGET = re.compile(r"^\s*<target(?:\s[^>]*)?(?:/>|></target>)\s*$")
SOURCE_END = "</source>"

SPECIFIER = re.compile(
    r"%(?:(?P<position>[1-9][0-9]*)\$)?"
    r"(?P<spec>[-+ #0']*[0-9]*(?:\.[0-9]+)?(?:hh|h|ll|l|q|L|z|t|j)?[@dDiuUxXoOfFeEgGaAcCsSp%])"
)
ESCAPED_PERCENT = "%"
# Link targets are percent-encoded, so they are compared verbatim instead of parsed for placeholders.
LINK_TARGET = re.compile(r"\]\(([^)]*)\)")
# Valid URLs are ASCII, so a bare URL ends before full-width punctuation such as "）".
BARE_URL = re.compile(r"\b(?:https?|simplex):/(?:(?![)\]])[!-~])*")
LINK_MASK = "<link>"
LINK_PREVIEW_LENGTH = 60
STRINGS_ESCAPES = {"n": "\n", "t": "\t", "r": "\r", '"': '"', "\\": "\\", "'": "'", "0": "\0"}
UNICODE_ESCAPE = re.compile(r"[uU]([0-9a-fA-F]{4})")

PLACEHOLDER = "placeholder"
BLANK = "blank"
LOST = "lost"
LINK = "link"
CHANGED = "changed"
SYNTAX = "syntax"
DUPLICATE = "duplicate"
STALE = "stale"
PROBLEM_KINDS = [PLACEHOLDER, LINK, BLANK, LOST, CHANGED, SYNTAX, DUPLICATE, STALE]
WARNING_KINDS = {DUPLICATE, STALE}
GROUPED_KINDS = {BLANK, STALE}

BOLD = "1"
DIM = "2"
RED = "31"
GREEN = "32"
YELLOW = "33"
CYAN = "36"
KIND_COLORS = {
    PLACEHOLDER: RED,
    LINK: RED,
    BLANK: RED,
    LOST: RED,
    CHANGED: YELLOW,
    SYNTAX: RED,
    DUPLICATE: YELLOW,
    STALE: YELLOW,
}
# https://no-color.org
USE_COLOR = sys.stdout.isatty() and "NO_COLOR" not in os.environ and os.environ.get("TERM") != "dumb"


def paint(text: str, *codes: str) -> str:
    return f"\033[{';'.join(codes)}m{text}\033[0m" if USE_COLOR and codes else text


class UpdateError(Exception):
    pass


class StringsSyntaxError(ValueError):
    pass


@dataclass
class Unit:
    source: str
    target: str | None

    @property
    def translated(self) -> bool:
        return bool(self.target)


@dataclass
class UnitBlock:
    """One <trans-unit>: its lines kept verbatim, so edits leave the rest of the file intact."""

    key: UnitKey
    unit: Unit
    lines: list[str]

    def target_lines(self) -> list[str]:
        start = next(i for i, line in enumerate(self.lines) if TARGET_START.search(line))
        end = next(i for i in range(start, len(self.lines)) if TARGET_END.search(self.lines[i]))
        return self.lines[start : end + 1]


Segment = Union[str, UnitBlock]


@dataclass
class SavedTranslation:
    source: str
    target: str
    lines: list[str]


@dataclass
class Placeholders:
    arguments: dict[int, str] = field(default_factory=dict)
    invalid: list[str] = field(default_factory=list)
    mixed: bool = False
    escaped_percent: bool = False

    @property
    def is_format(self) -> bool:
        return bool(self.arguments) or self.escaped_percent


@dataclass
class Problem:
    kind: str
    path: Path
    key: str
    detail: str
    text: str = ""

    @property
    def language(self) -> str:
        return language_of(self.path)


@dataclass
class LanguageStats:
    translated: int = 0
    restored: int = 0
    removed: int = 0


def read_file(path: Path) -> str:
    with path.open(encoding="utf-8", newline="") as file:
        return file.read()


def write_file(path: Path, text: str) -> None:
    with path.open("w", encoding="utf-8", newline="") as file:
        file.write(text)


def element_text(element: ET.Element | None) -> str | None:
    return None if element is None else "".join(element.itertext())


def parse_units(text: str) -> list[tuple[UnitKey, Unit]]:
    units = []
    for file in ET.fromstring(text).iter(f"{XLIFF_NS}file"):
        original = file.get("original", "")
        for element in file.iter(f"{XLIFF_NS}trans-unit"):
            units.append(
                (
                    (original, element.get("id", "")),
                    Unit(
                        source=element_text(element.find(f"{XLIFF_NS}source")) or "",
                        target=element_text(element.find(f"{XLIFF_NS}target")),
                    ),
                )
            )
    return units


def split_xliff(text: str) -> list[Segment]:
    """Split into verbatim lines and unit blocks; the XML is parsed once and matched to the blocks in order."""
    units = iter(parse_units(text))
    segments: list[Segment] = []
    block_lines: list[str] | None = None
    for line in text.splitlines(keepends=True):
        if block_lines is None:
            if not UNIT_START.match(line):
                segments.append(line)
                continue
            block_lines = []
        block_lines.append(line)
        if UNIT_END in line:
            key, unit = next(units)
            segments.append(UnitBlock(key, unit, block_lines))
            block_lines = None
    if block_lines is not None or next(units, None) is not None:
        raise ValueError("<trans-unit> elements must start and end on their own lines")
    return segments


def join_xliff(segments: list[Segment]) -> str:
    return "".join(s if isinstance(s, str) else "".join(s.lines) for s in segments)


def unit_blocks(segments: list[Segment]) -> Iterator[UnitBlock]:
    return (s for s in segments if isinstance(s, UnitBlock))


def strip_empty_targets(segments: list[Segment]) -> int:
    stripped = 0
    for block in unit_blocks(segments):
        lines = [line for line in block.lines if not EMPTY_TARGET.match(line)]
        if len(lines) != len(block.lines):
            stripped += 1
            block.lines = lines
            block.unit.target = None
    return stripped


def drop_untranslated_units(segments: list[Segment]) -> list[Segment]:
    """Remove units without a translation, as Weblate's "Remove blank strings" add-on does."""
    return [s for s in segments if isinstance(s, str) or s.unit.translated]


def saved_translations(segments: list[Segment]) -> dict[UnitKey, SavedTranslation]:
    saved: dict[UnitKey, SavedTranslation] = {}
    for block in unit_blocks(segments):
        if block.unit.translated:
            saved.setdefault(block.key, SavedTranslation(block.unit.source, block.unit.target or "", block.target_lines()))
    return saved


def restore_translations(segments: list[Segment], saved: dict[UnitKey, SavedTranslation]) -> int:
    """Put back translations that Xcode dropped, such as drafts, unless their source text changed."""
    blocks = list(unit_blocks(segments))
    translated = {block.key for block in blocks if block.unit.translated}
    restored = 0
    for block in blocks:
        translation = saved.get(block.key)
        if block.key in translated or translation is None or translation.source != block.unit.source:
            continue
        lines = [line for line in block.lines if not EMPTY_TARGET.match(line)]
        source_end = next(i for i, line in enumerate(lines) if SOURCE_END in line)
        block.lines = lines[: source_end + 1] + translation.lines + lines[source_end + 1 :]
        block.unit.target = translation.target
        translated.add(block.key)
        restored += 1
    return restored


def parse_placeholders(text: str) -> Placeholders:
    result = Placeholders()
    positional = sequential = False
    next_position = 1
    index = text.find("%")
    while index != -1:
        match = SPECIFIER.match(text, index)
        if match is None:
            result.invalid.append(text[index : index + 2])
            index = text.find("%", index + 1)
            continue
        spec = match["spec"]
        if spec == ESCAPED_PERCENT:
            result.escaped_percent = True
        elif spec.endswith(ESCAPED_PERCENT):
            result.invalid.append(match.group(0))
        else:
            if match["position"]:
                positional = True
                position = int(match["position"])
            else:
                sequential = True
                position = next_position
                next_position += 1
            if result.arguments.setdefault(position, spec) != spec:
                result.invalid.append(match.group(0))
        index = text.find("%", match.end())
    result.mixed = positional and sequential
    return result


def describe_arguments(arguments: dict[int, str]) -> str:
    return ", ".join(f"%{position}${spec}" for position, spec in sorted(arguments.items())) or "none"


def links(text: str) -> tuple[list[str], list[str]]:
    """Markdown link targets, and bare URLs outside them."""
    return LINK_TARGET.findall(text), BARE_URL.findall(LINK_TARGET.sub("", text))


def mask_links(text: str) -> str:
    return BARE_URL.sub(LINK_MASK, LINK_TARGET.sub(f"]({LINK_MASK})", text))


def link_problem(source: str, target: str) -> str | None:
    for kind, expected, found in zip(("markdown link", "URL"), links(source), links(target)):
        missing = [link for link in expected if link not in found]
        if missing:
            return f"{kind} changed or missing: {missing[0][:LINK_PREVIEW_LENGTH]!r}"
        extra = [link for link in found if link not in expected]
        if extra or sorted(expected) != sorted(found):
            return f"{kind} not in the source: {(extra or found)[0][:LINK_PREVIEW_LENGTH]!r}"
    return None


def placeholder_problem(source: str, target: str) -> str | None:
    """Each placeholder must be spelled as in the source; only its argument number may differ."""
    expected = parse_placeholders(mask_links(source))
    found = parse_placeholders(mask_links(target))
    if not expected.is_format:
        if found.arguments:
            return f"placeholders not in the source: {describe_arguments(found.arguments)}"
        return None
    if found.invalid:
        return "invalid placeholders " + ", ".join(repr(s) for s in found.invalid)
    if found.mixed:
        return "positional and non-positional placeholders mixed"
    if found.arguments != expected.arguments:
        return f"expected {describe_arguments(expected.arguments)}, found {describe_arguments(found.arguments)}"
    return None


def skip_strings_trivia(text: str, index: int) -> int:
    while index < len(text):
        if text[index].isspace():
            index += 1
        elif text.startswith("/*", index):
            end = text.find("*/", index + 2)
            if end == -1:
                raise StringsSyntaxError(f"unterminated comment at offset {index}")
            index = end + 2
        elif text.startswith("//", index):
            end = text.find("\n", index)
            index = len(text) if end == -1 else end + 1
        else:
            break
    return index


def read_strings_literal(text: str, index: int) -> tuple[str, int]:
    if not text.startswith('"', index):
        raise StringsSyntaxError(f'expected " at offset {index}')
    chars = []
    index += 1
    while index < len(text):
        char = text[index]
        if char == '"':
            return "".join(chars), index + 1
        if char == "\\" and index + 1 < len(text):
            unicode = UNICODE_ESCAPE.match(text, index + 1)
            if unicode:
                chars.append(chr(int(unicode.group(1), 16)))
                index = unicode.end()
            else:
                chars.append(STRINGS_ESCAPES.get(text[index + 1], text[index + 1]))
                index += 2
            continue
        chars.append(char)
        index += 1
    raise StringsSyntaxError("unterminated string literal")


def expect_strings_token(text: str, index: int, token: str) -> int:
    index = skip_strings_trivia(text, index)
    if not text.startswith(token, index):
        raise StringsSyntaxError(f"expected {token!r} at offset {index}")
    return index + len(token)


def parse_strings(text: str) -> list[tuple[str, str]]:
    entries = []
    index = skip_strings_trivia(text, 0)
    while index < len(text):
        key, index = read_strings_literal(text, index)
        index = expect_strings_token(text, index, "=")
        value, index = read_strings_literal(text, skip_strings_trivia(text, index))
        index = expect_strings_token(text, index, ";")
        entries.append((key, value))
        index = skip_strings_trivia(text, index)
    return entries


def app_languages() -> list[str]:
    names = sorted(p.name[: -len(".lproj")] for p in IOS_DIR.glob("*.lproj") if p.is_dir())
    return sorted(names, key=lambda name: name != SOURCE_LANGUAGE)


def language_of(path: Path) -> str:
    return path.parent.name[: -len(".lproj")] if path.suffix == ".strings" else path.stem


def xcloc_path(language: str) -> Path:
    return LOCALIZATIONS_DIR / f"{language}.xcloc"


def xliff_path(language: str) -> Path:
    return xcloc_path(language) / "Localized Contents" / f"{language}.xliff"


def translation_xliffs() -> list[Path]:
    return [
        path
        for path in sorted(LOCALIZATIONS_DIR.glob("*.xcloc/Localized Contents/*.xliff"))
        if path.stem != SOURCE_LANGUAGE
    ]


def translation_strings() -> list[Path]:
    return [
        path
        for path in sorted(IOS_DIR.glob("**/*.lproj/*.strings"))
        if language_of(path) != SOURCE_LANGUAGE and LOCALIZATIONS_DIR not in path.parents
    ]


def lint_with_plutil(paths: list[Path]) -> list[Problem]:
    """Apple's own parser, as the app build uses; only on macOS."""
    if shutil.which("plutil") is None or not paths:
        return []
    result = subprocess.run(["plutil", "-lint", *map(str, paths)], capture_output=True, text=True)
    problems = []
    for line in result.stdout.splitlines() + result.stderr.splitlines():
        name, _, message = line.partition(": ")
        if message and message != "OK":
            problems.append(Problem(SYNTAX, Path(name), "", f"plutil: {message}"))
    return problems


def translation_problems(path: Path, key: str, source: str, target: str) -> list[Problem]:
    problems = []
    placeholder = placeholder_problem(source, target)
    if placeholder:
        problems.append(Problem(PLACEHOLDER, path, key, placeholder, target))
    link = link_problem(source, target)
    if link:
        problems.append(Problem(LINK, path, key, link, target))
    return problems


def check_xliff(path: Path, source_keys: set[UnitKey]) -> tuple[list[Problem], int]:
    try:
        units = parse_units(read_file(path))
    except ET.ParseError as error:
        return [Problem(SYNTAX, path, "", f"invalid XML: {error}")], 0
    problems = []
    translated = 0
    occurrences: dict[UnitKey, int] = {}
    for unit_key, _unit in units:
        occurrences[unit_key] = occurrences.get(unit_key, 0) + 1
    for unit_key, count in occurrences.items():
        if count > 1:
            problems.append(Problem(DUPLICATE, path, unit_key[1], f"{count} units with this id in {unit_key[0]}"))
        if source_keys and unit_key not in source_keys:
            problems.append(Problem(STALE, path, unit_key[1], f"not in {SOURCE_LANGUAGE}.xliff, Xcode's export removes it"))
    for (_file, key), unit in units:
        if unit.target == "":
            problems.append(Problem(BLANK, path, key, "empty target, the import would show it blank"))
        elif unit.target is not None:
            translated += 1
            problems += translation_problems(path, key, unit.source, unit.target)
    return problems, translated


def check_strings(path: Path) -> list[Problem]:
    try:
        entries = parse_strings(read_file(path))
    except StringsSyntaxError as error:
        return [Problem(SYNTAX, path, "", str(error))]
    problems = []
    for key, value in entries:
        if value == "":
            problems.append(Problem(BLANK, path, key, "empty value, shown blank in the app"))
        else:
            problems += translation_problems(path, key, key, value)
    return problems


def check_files(stats: dict[str, LanguageStats]) -> list[Problem]:
    problems = []
    source_xliff = xliff_path(SOURCE_LANGUAGE)
    source_keys = {key for key, _ in parse_units(read_file(source_xliff))} if source_xliff.is_file() else set()
    for path in translation_xliffs():
        xliff_problems, translated = check_xliff(path, source_keys)
        stats.setdefault(path.stem, LanguageStats()).translated = translated
        problems += xliff_problems
    strings = translation_strings()
    for path in strings:
        problems += check_strings(path)
    return problems + lint_with_plutil(strings)


def translation_changes(
    path: Path, saved: dict[UnitKey, SavedTranslation], after: list[Segment], source_keys: set[UnitKey]
) -> tuple[list[Problem], int]:
    """Translations lost or changed by the round trip, and the count removed together with their source."""
    translated_after: dict[UnitKey, Unit] = {}
    for block in unit_blocks(after):
        if block.unit.translated:
            translated_after.setdefault(block.key, block.unit)
    problems = []
    removed = 0
    for key, translation in saved.items():
        result = translated_after.get(key)
        if result is None:
            if key in source_keys:
                problems.append(Problem(LOST, path, key[1], "translation lost", translation.target))
            else:
                removed += 1
        elif result.target != translation.target:
            problems.append(Problem(CHANGED, path, key[1], f"was {translation.target!r}", result.target or ""))
    return problems, removed


class Steps:
    def __init__(self, total: int) -> None:
        self.total = total
        self.current = 0

    @contextlib.contextmanager
    def step(self, title: str) -> Iterator[None]:
        self.current += 1
        print(paint(f"[{self.current}/{self.total}] ", BOLD, CYAN) + paint(title, BOLD), flush=True)
        started = time.monotonic()
        yield
        print(paint(f"      done in {format_duration(time.monotonic() - started)}", GREEN), flush=True)


def format_duration(seconds: float) -> str:
    minutes, seconds = divmod(int(round(seconds)), 60)
    return f"{minutes}m {seconds:02d}s" if minutes else f"{seconds}s"


def print_summary(stats: dict[str, LanguageStats], problems: list[Problem], in_app: set[str]) -> None:
    counts: dict[str, dict[str, int]] = {}
    for problem in problems:
        language_counts = counts.setdefault(problem.language, {})
        language_counts[problem.kind] = language_counts.get(problem.kind, 0) + 1
    headers = ["language", "in app", "translated", "restored", "removed", *PROBLEM_KINDS]
    rows = []
    for language in sorted(set(stats) | set(counts)):
        entry = stats.get(language, LanguageStats())
        language_counts = counts.get(language, {})
        cells = [
            (language, BOLD if language_counts else ""),
            ("yes", "") if language in in_app else ("no", DIM),
            (str(entry.translated), ""),
            (str(entry.restored or ""), YELLOW),
            (str(entry.removed or ""), DIM),
        ]
        cells += [(str(language_counts.get(kind, "")), KIND_COLORS[kind]) for kind in PROBLEM_KINDS]
        rows.append(cells)
    widths = [max([len(headers[i]), *(len(row[i][0]) for row in rows)]) for i in range(len(headers))]
    print()
    print("  " + "  ".join(paint(h.ljust(w) if i == 0 else h.rjust(w), DIM) for i, (h, w) in enumerate(zip(headers, widths))))
    for row in rows:
        padded = [(text.ljust(w) if i == 0 else text.rjust(w), color) for i, ((text, color), w) in enumerate(zip(row, widths))]
        print("  " + "  ".join(paint(text, color) if text.strip() and color else text for text, color in padded))


def print_problems(problems: list[Problem]) -> None:
    by_path: dict[Path, list[Problem]] = {}
    for problem in sorted(problems, key=lambda p: (p.language, str(p.path))):
        by_path.setdefault(problem.path, []).append(problem)
    for path, path_problems in by_path.items():
        print("\n  " + paint(f"[{path_problems[0].language}]", BOLD, CYAN) + " " + paint(str(path), BOLD))
        for grouped_kind in sorted(GROUPED_KINDS, key=PROBLEM_KINDS.index):
            grouped = [p for p in path_problems if p.kind == grouped_kind]
            if not grouped:
                continue
            examples = ", ".join(repr(p.key) for p in grouped[:GROUP_EXAMPLES])
            more = f" and {len(grouped) - GROUP_EXAMPLES} more" if len(grouped) > GROUP_EXAMPLES else ""
            print(f"    {paint(f'{grouped_kind:<11}', KIND_COLORS[grouped_kind])} {len(grouped)} x {grouped[0].detail}")
            print(paint(f"                {examples}{more}", DIM))
        for problem in path_problems:
            if problem.kind in GROUPED_KINDS:
                continue
            kind = paint(f"{problem.kind:<11}", KIND_COLORS[problem.kind])
            print(f"    {kind} {problem.key!r}" if problem.key else f"    {kind}")
            print(f"                {problem.detail}")
            if problem.text:
                print(paint(f"                -> {problem.text!r}", DIM))


def count_line(label: str, problems: list[Problem]) -> str:
    kinds: dict[str, int] = {}
    for problem in problems:
        kinds[problem.kind] = kinds.get(problem.kind, 0) + 1
    counts = ", ".join(paint(f"{kinds[kind]} {kind}", KIND_COLORS[kind]) for kind in PROBLEM_KINDS if kind in kinds)
    return paint(f"{len(problems)} {label}: ", BOLD) + counts


def report(stats: dict[str, LanguageStats], problems: list[Problem]) -> int:
    """Exit code 1 for problems; warnings are listed but do not fail."""
    print_summary(stats, problems, set(app_languages()))
    print_problems(problems)
    errors = [p for p in problems if p.kind not in WARNING_KINDS]
    warnings = [p for p in problems if p.kind in WARNING_KINDS]
    print()
    if warnings:
        print(count_line("warning(s)", warnings))
    if not errors:
        print(paint("No problems found.", BOLD, GREEN))
        return 0
    print(count_line("problem(s)", errors))
    print("Fix them in Weblate, so the next Weblate commit does not bring them back.")
    return 1


def require_update_environment() -> None:
    if platform.system() != "Darwin" or shutil.which("xcodebuild") is None:
        raise UpdateError("update needs macOS with Xcode; check runs on any platform")
    if not PROJECT.is_dir():
        raise UpdateError(f"{PROJECT} not found, run from the repository root")
    status = subprocess.run(["git", "status", "--porcelain", "--", str(IOS_DIR)], capture_output=True, text=True, check=True)
    if status.stdout.strip():
        raise UpdateError(f"uncommitted changes under {IOS_DIR}, commit or stash them first")


def xcodebuild(arguments: list[str], log: Path) -> None:
    with log.open("w", encoding="utf-8") as output:
        result = subprocess.run(["xcodebuild", *arguments], stdout=output, stderr=subprocess.STDOUT)
    if result.returncode != 0:
        tail = log.read_text(encoding="utf-8", errors="replace").splitlines()[-LOG_TAIL_LINES:]
        raise UpdateError(f"xcodebuild {arguments[0]} failed, full log: {log}\n" + "\n".join(tail))


def update_pbxproj_libraries() -> None:
    subprocess.run(["sh", str(UPDATE_PBXPROJ)], check=True, stdout=subprocess.DEVNULL)


@contextlib.contextmanager
def local_core_libraries() -> Iterator[None]:
    """Xcode links the app to extract strings, so the project temporarily references the local core libraries."""
    pbxproj = PBXPROJ.read_bytes()
    try:
        update_pbxproj_libraries()
        yield
    finally:
        PBXPROJ.write_bytes(pbxproj)


def import_languages(languages: list[str], log_dir: Path) -> None:
    for language in languages:
        started = time.monotonic()
        xcodebuild(
            ["-importLocalizations", "-project", str(PROJECT), "-localizationPath", str(xcloc_path(language)), "-skipPackageUpdates"],
            log_dir / f"import-{language}.log",
        )
        print(f"      {language:<8} " + paint(format_duration(time.monotonic() - started), DIM), flush=True)


def export_languages(languages: list[str], log_dir: Path) -> None:
    export_arguments = [argument for language in languages for argument in ("-exportLanguage", language)]
    xcodebuild(
        ["-exportLocalizations", "-project", str(PROJECT), "-localizationPath", str(LOCALIZATIONS_DIR), *export_arguments],
        log_dir / "export.log",
    )


def read_xliff(path: Path) -> list[Segment]:
    try:
        return split_xliff(read_file(path))
    except (ET.ParseError, ValueError) as error:
        raise UpdateError(f"{path}: {error}") from error


def update() -> int:
    require_update_environment()
    languages = app_languages()
    translated = [language for language in languages if language != SOURCE_LANGUAGE]
    stats = {language: LanguageStats() for language in translated}
    steps = Steps(5)

    with steps.step(f"Preparing {len(translated)} translation files"):
        weblate: dict[str, dict[UnitKey, SavedTranslation]] = {}
        stripped_total = 0
        for language in translated:
            segments = read_xliff(xliff_path(language))
            weblate[language] = saved_translations(segments)
            count = strip_empty_targets(segments)
            if count:
                write_file(xliff_path(language), join_xliff(segments))
            stripped_total += count
        print(paint(f"      removed {stripped_total} empty targets that the import would show blank", DIM))

    log_dir = Path(tempfile.mkdtemp(prefix="ios-localizations-"))
    try:
        with local_core_libraries():
            with steps.step(f"Importing {len(languages)} languages with Xcode (logs: {log_dir})"):
                import_languages(languages, log_dir)
            with steps.step(f"Exporting {len(languages)} languages with Xcode"):
                export_languages(languages, log_dir)
    except (UpdateError, subprocess.CalledProcessError) as error:
        raise UpdateError(f"{error}\nRestore the files with: git checkout -- {IOS_DIR}") from error

    problems: list[Problem] = []
    with steps.step("Restoring translations dropped by Xcode and removing untranslated units"):
        source_keys = {key for key, _ in parse_units(read_file(xliff_path(SOURCE_LANGUAGE)))}
        for language in translated:
            path = xliff_path(language)
            exported = read_xliff(path)
            stats[language].restored = restore_translations(exported, weblate[language])
            result = drop_untranslated_units(exported)
            write_file(path, join_xliff(result))
            changes, stats[language].removed = translation_changes(path, weblate[language], result, source_keys)
            problems += changes

    with steps.step("Linting .strings files and checking translations"):
        problems += check_files(stats)

    return report(stats, problems)


def check() -> int:
    stats: dict[str, LanguageStats] = {}
    steps = Steps(1)
    with steps.step("Linting .strings files and checking translations"):
        problems = check_files(stats)
    return report(stats, problems)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("update", help="import, export, lint and check (macOS with Xcode)")
    commands.add_parser("check", help="lint and check the files only (any platform)")
    arguments = parser.parse_args(argv)
    try:
        return update() if arguments.command == "update" else check()
    except UpdateError as error:
        print(paint("error: ", BOLD, RED) + str(error), file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
