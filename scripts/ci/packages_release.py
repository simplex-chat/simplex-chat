#!/usr/bin/env python3
"""Release automation for simplex-chat and @simplex-chat/types on npm and simplex-chat on PyPI.

addon-tag      print the add-on release tag for the current sources
changes        print whether the changes since a commit touch the chat core and the add-on
build-plan     print the platforms to build the add-on on and whether to run the package tests
prepare        update pins and versions for a release PR
publish-plan   print the package versions that are not published yet
record         record releases and published versions in releases.toml of simplex-chat-libs
test-names     rename the packages to NPM_SCOPE and PYPI_PROJECT (test repositories)
"""

from __future__ import annotations

import argparse
import base64
import copy
import hashlib
import json
import os
import re
import subprocess
import sys
import tomllib
import urllib.error
import urllib.request
from collections.abc import Callable
from dataclasses import dataclass, field, replace
from typing import Any
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NODEJS_DIR = "packages/simplex-chat-nodejs"
TYPES_DIR = "packages/simplex-chat-client/types/typescript"
PYTHON_DIR = "packages/simplex-chat-python"
RELEASE_FILE = f"{NODEJS_DIR}/src/release.ts"
PYTHON_VERSION_FILE = f"{PYTHON_DIR}/src/simplex_chat/_version.py"
ADDON_SOURCES = [f"{NODEJS_DIR}/binding.gyp", f"{NODEJS_DIR}/cpp"]
CORE_SOURCES = ["src", "simplex-chat.cabal", "cabal.project"]
ADDON_TAG_HASH_LENGTH = 12
LIBS_ASSETS = [
    "simplex-chat-libs-linux-aarch64.zip",
    "simplex-chat-libs-linux-x86_64-postgres.zip",
    "simplex-chat-libs-linux-x86_64.zip",
    "simplex-chat-libs-macos-aarch64.zip",
    "simplex-chat-libs-macos-x86_64.zip",
    "simplex-chat-libs-windows-x86_64.zip",
]
ADDON_RUNNERS = {
    "linux-aarch64": "ubuntu-22.04-arm",
    "linux-x86_64": "ubuntu-22.04",
    "macos-aarch64": "macos-latest",
    "macos-x86_64": "macos-15-intel",
    "windows-x86_64": "windows-latest",
}
ADDON_PLATFORMS = list(ADDON_RUNNERS)
# build.yml builds libsimplex of pull requests on this platform only
CORE_TEST_PLATFORM = "linux-x86_64"
ADDON_ASSETS = [f"simplex-chat-nodejs-{platform}.node" for platform in ADDON_PLATFORMS]
SHA256_HEX = re.compile(r"^[0-9a-f]{64}$")
RELEASES_FILE = "releases.toml"
RECORD_ATTEMPTS = 5
TOML_BARE_KEY = re.compile(r"^[A-Za-z0-9_-]+$")


# Pure functions


def addon_tag(files: dict[str, bytes], node_addon_api: str) -> str:
    digest = hashlib.sha256()
    for name in sorted(files):
        digest.update(name.encode() + b"\0" + files[name] + b"\0")
    digest.update(f"node-addon-api@{node_addon_api}".encode())
    return f"nodejs-addon-{digest.hexdigest()[:ADDON_TAG_HASH_LENGTH]}"


def bump_npm_version(version: str) -> str:
    """A package release of app version 7.1.0-beta.6 is 7.1.0-beta.6.1, then 7.1.0-beta.6.2."""
    _, _, prerelease = version.partition("-")
    if len(prerelease.split(".")) == 2:
        return f"{version}.1"
    head, _, last = version.rpartition(".")
    return f"{head}.{int(last) + 1}" if last.isdigit() else f"{version}.1"


def bump_python_version(version: str) -> str:
    """A package release of app version 7.1.0b6 is 7.1.0b6.post1, then 7.1.0b6.post2."""
    post = re.search(r"\.post(\d+)$", version)
    return f"{version[: post.start()]}.post{int(post[1]) + 1}" if post else f"{version}.post1"


def dist_tag(version: str) -> str:
    return "beta" if "-" in version else "latest"


def registry_name(name: str, scope: str | None) -> str:
    """Package name on npm: test repositories publish under their own scope."""
    return f"{scope}/{name.split('/')[-1]}" if scope else name


@dataclass(frozen=True)
class Pins:
    libs_release: str
    sha256: dict[str, str]
    addon_release: str | None = None


def parse_release(source: str) -> Pins:
    libs = re.search(r'export const LIBS_RELEASE = "([^"]+)"', source)
    addon = re.search(r'export const ADDON_RELEASE: string \| undefined = (?:"([^"]+)"|undefined)', source)
    if not libs or not addon:
        raise ValueError(f"cannot parse {RELEASE_FILE}")
    hashes = dict(re.findall(r'^ {2}"([^"]+)": "([0-9a-f]{64})",$', source, re.M))
    return Pins(libs[1], hashes, addon[1])


def render_release(pins: Pins) -> str:
    hashes = "".join(f'  "{asset}": "{pins.sha256[asset]}",\n' for asset in sorted(pins.sha256))
    addon = f'"{pins.addon_release}"' if pins.addon_release else "undefined"
    return (
        "// Written by scripts/ci/packages_release.py from the simplex-chat-libs releases.\n"
        "\n"
        f'export const LIBS_RELEASE = "{pins.libs_release}"\n'
        "\n"
        f"export const ADDON_RELEASE: string | undefined = {addon}\n"
        "\n"
        "export const SHA256: {readonly [asset: string]: string} = {\n"
        f"{hashes}}}\n"
    )


def parse_python_version(source: str) -> tuple[str, Pins]:
    version = re.search(r'^__version__ = "([^"]+)"', source, re.M)
    libs = re.search(r'^LIBS_RELEASE = "([^"]+)"', source, re.M)
    if not version or not libs:
        raise ValueError(f"cannot parse {PYTHON_VERSION_FILE}")
    hashes = dict(re.findall(r'^ {4}"([^"]+)": "([0-9a-f]{64})",$', source, re.M))
    return version[1], Pins(libs[1], hashes)


def render_python_version(version: str, pins: Pins) -> str:
    hashes = "".join(f'    "{asset}": "{pins.sha256[asset]}",\n' for asset in sorted(pins.sha256))
    return (
        '"""Package version and the simplex-chat-libs release it loads.\n'
        "\n"
        "Written by scripts/ci/packages_release.py: the release PR bumps the version\n"
        "and pins new library releases with the SHA-256 of their assets.\n"
        '"""\n'
        "\n"
        f'__version__ = "{version}"  # PEP 440 — read by hatchling for wheel metadata\n'
        f'LIBS_RELEASE = "{pins.libs_release}"  # simplex-chat-libs release tag\n'
        "LIBS_SHA256 = {\n"
        f"{hashes}}}\n"
    )


def release_digests(release: dict | None, assets: list[str]) -> dict[str, str] | None:
    """Asset digests of a GitHub release, or None when the release is missing or incomplete."""
    if release is None:
        return None
    digests = {a["name"]: (a.get("digest") or "").removeprefix("sha256:") for a in release["assets"]}
    if not all(SHA256_HEX.match(digests.get(asset, "")) for asset in assets):
        return None
    return {asset: digests[asset] for asset in assets}


@dataclass(frozen=True)
class Package:
    version: str
    changed: bool
    published: bool


@dataclass(frozen=True)
class State:
    libs_tag: str
    libs_digests: dict[str, str] | None
    addon_tag: str
    addon_digests: dict[str, str] | None
    nodejs_pins: Pins
    python_pins: Pins
    types: Package
    nodejs: Package
    python: Package
    types_version: str | None = None
    nodejs_version: str | None = None
    python_version: str | None = None


@dataclass(frozen=True)
class Plan:
    nodejs_pins: Pins
    python_pins: Pins
    types_version: str
    types_bumped: bool
    nodejs_version: str
    nodejs_changed: bool
    python_version: str
    python_changed: bool
    notes: list[str] = field(default_factory=list)

    @property
    def types_dependency(self) -> str | None:
        return f"^{self.types_version}" if self.types_bumped else None

    @property
    def title(self) -> str:
        parts = []
        if self.types_bumped:
            parts.append(f"types {self.types_version}")
        if self.nodejs_changed:
            parts.append(f"nodejs {self.nodejs_version}")
        if self.python_changed:
            parts.append(f"python {self.python_version}")
        return f"packages: release {', '.join(parts)}"


def plan(state: State) -> Plan:
    """What a release PR changes, from the state of the branch and the releases."""
    notes: list[str] = []

    def pin(pins: Pins, field_name: str, tag: str, digests: dict[str, str] | None, what: str) -> tuple[Pins, bool]:
        current = getattr(pins, field_name)
        if current == tag:
            return pins, False
        if digests is None:
            notes.append(f"{what} release {tag} is missing or incomplete; {current or 'none'} stays pinned")
            return pins, False
        notes.append(f"{what}: {tag}")
        return replace(pins, **{field_name: tag, "sha256": {**pins.sha256, **digests}}), True

    nodejs_pins, nodejs_libs = pin(
        state.nodejs_pins, "libs_release", state.libs_tag, state.libs_digests, "Node.js libsimplex"
    )
    nodejs_pins, addon = pin(nodejs_pins, "addon_release", state.addon_tag, state.addon_digests, "Node.js add-on")
    python_pins, python_libs = pin(
        state.python_pins, "libs_release", state.libs_tag, state.libs_digests, "Python libsimplex"
    )

    def next_version(pkg: Package, changed: bool, override: str | None, bump) -> str:
        if override:
            return override
        return bump(pkg.version) if changed and pkg.published else pkg.version

    types_version = next_version(state.types, state.types.changed, state.types_version, bump_npm_version)
    types_bumped = types_version != state.types.version
    nodejs_changed = nodejs_libs or addon or types_bumped or state.nodejs.changed
    nodejs_version = next_version(state.nodejs, nodejs_changed, state.nodejs_version, bump_npm_version)
    python_changed = python_libs or state.python.changed
    python_version = next_version(state.python, python_changed, state.python_version, bump_python_version)
    for name, old, new in [
        ("@simplex-chat/types", state.types.version, types_version),
        ("simplex-chat (npm)", state.nodejs.version, nodejs_version),
        ("simplex-chat (PyPI)", state.python.version, python_version),
    ]:
        if old != new:
            notes.append(f"{name}: {old} → {new}")
    return Plan(
        nodejs_pins=nodejs_pins,
        python_pins=python_pins,
        types_version=types_version,
        types_bumped=types_bumped,
        nodejs_version=nodejs_version,
        nodejs_changed=nodejs_changed or nodejs_version != state.nodejs.version,
        python_version=python_version,
        python_changed=python_changed or python_version != state.python.version,
        notes=notes,
    )


@dataclass(frozen=True)
class BuildPlan:
    platforms: list[str]
    test: bool


def build_plan(
    event: str, release_missing: bool, libs_artifact: bool, libs_release: str, core_changed: bool, addon_changed: bool
) -> BuildPlan:
    """Each package test runs once, against the libsimplex of the change: a pull request that changes the core
    is tested by build.yml with its own libsimplex (libs_artifact), a new library release with that release."""
    if libs_artifact:
        return BuildPlan([CORE_TEST_PLATFORM], test=True)
    if libs_release:
        return BuildPlan(ADDON_PLATFORMS, test=True)
    if event == "pull_request":
        if core_changed:
            return BuildPlan(ADDON_PLATFORMS if addon_changed else [], test=False)
        return BuildPlan(ADDON_PLATFORMS, test=True)
    return BuildPlan(ADDON_PLATFORMS if release_missing else [], test=True)


def build_matrix(platforms: list[str]) -> str:
    return json.dumps([{"platform": platform, "os": ADDON_RUNNERS[platform]} for platform in platforms])


def recorded_commit(releases: dict, key: str, version: str) -> str | None:
    """The simplex-chat commit recorded for a version; key is "core", "npm/<name>" or "pypi/<name>"."""
    table = releases
    for part in key.split("/", 1):
        table = table.get(part, {})
    commit = table.get(version)
    return commit if isinstance(commit, str) else None


def add_releases(releases: dict, entries: list[tuple[str, str]], commit: str) -> dict:
    """A recorded version keeps its first commit: re-running a release does not rewrite history."""
    result = copy.deepcopy(releases)
    for key, version in entries:
        table = result
        for part in key.split("/", 1):
            table = table.setdefault(part, {})
        table.setdefault(version, commit)
    return result


def render_releases(releases: dict) -> str:
    def toml_key(key: str) -> str:
        return key if TOML_BARE_KEY.match(key) else json.dumps(key)

    sections: list[str] = []

    # in the order of recording: entries are only added, so that order is chronological
    def walk(path: list[str], table: dict) -> None:
        values = [(k, v) for k, v in table.items() if isinstance(v, str)]
        if values:
            lines = [f"[{'.'.join(toml_key(part) for part in path)}]"]
            lines += [f"{toml_key(version)} = {json.dumps(commit)}" for version, commit in values]
            sections.append("\n".join(lines) + "\n")
        for name, child in table.items():
            if isinstance(child, dict):
                walk([*path, name], child)

    walk([], releases)
    return "\n".join(sections)


# Repository and registry access


def git(*args: str) -> str:
    return subprocess.run(["git", *args], cwd=ROOT, check=True, capture_output=True, text=True).stdout.strip()


def read_text(file: str) -> str:
    return (ROOT / file).read_text()


def read_field(file: str, pattern: str) -> str:
    found = re.search(pattern, read_text(file), re.M)
    if not found:
        raise ValueError(f"no match for {pattern} in {file}")
    return found[1]


def read_json(file: str) -> dict:
    return json.loads(read_text(file))


def write_json(file: str, value: dict) -> None:
    (ROOT / file).write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n")


def current_addon_tag() -> str:
    files = {
        path.relative_to(ROOT).as_posix(): path.read_bytes()
        for source in ADDON_SOURCES
        for path in ([ROOT / source] if (ROOT / source).is_file() else sorted((ROOT / source).rglob("*")))
        if path.is_file()
    }
    return addon_tag(files, read_json(f"{NODEJS_DIR}/package.json")["devDependencies"]["node-addon-api"])


def changed_since(base: str, sources: list[str]) -> bool:
    diff = subprocess.run(["git", "diff", "--quiet", base, "HEAD", "--", *sources], cwd=ROOT)
    return diff.returncode != 0


def changes(base: str) -> None:
    print(f"core={'true' if changed_since(base, CORE_SOURCES) else ''}")
    print(f"addon={'true' if changed_since(base, ADDON_SOURCES) else ''}")


def print_build_plan(args: argparse.Namespace) -> None:
    pull_request = args.event == "pull_request" and not args.libs_artifact
    result = build_plan(
        args.event,
        release_missing=args.release_missing,
        libs_artifact=args.libs_artifact,
        libs_release=args.libs_release,
        core_changed=pull_request and changed_since(args.base, CORE_SOURCES),
        addon_changed=pull_request and changed_since(args.base, ADDON_SOURCES),
    )
    print(f"platforms={build_matrix(result.platforms)}")
    print(f"test={'true' if result.test else ''}")


def package_state(
    key: str,
    version: str,
    directory: str,
    version_file: str,
    version_pattern: str,
    releases: dict,
    registry_published: Callable[[], bool],
) -> Package:
    """A recorded version is published from its recorded commit; the registry is asked about other versions,
    which are compared with the commit that set them."""
    recorded = recorded_commit(releases, key, version)
    base = (
        recorded
        if recorded and commit_exists(recorded)
        else git("log", "-1", "--format=%H", "-G", version_pattern, "--", version_file)
    )
    changed = changed_since(base, [directory, f":!{directory}/tests"])
    return Package(version, changed, recorded is not None or registry_published())


def http_json(url: str, headers: dict[str, str] | None = None, method: str = "GET", body: dict | None = None) -> Any:
    """The JSON response, or None for 404."""
    request = urllib.request.Request(
        url,
        headers={"User-Agent": "simplex-chat-release", **(headers or {})},
        method=method,
        data=json.dumps(body).encode() if body is not None else None,
    )
    try:
        with urllib.request.urlopen(request, timeout=60) as response:
            return json.load(response)
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return None
        raise


# Workflows pass unset repository variables as empty strings.
def npm_published(name: str, version: str) -> bool:
    registry = os.environ.get("NPM_REGISTRY") or "https://registry.npmjs.org"
    package = registry_name(name, os.environ.get("NPM_SCOPE")).replace("/", "%2F")
    return http_json(f"{registry}/{package}/{version}") is not None


def python_project() -> str:
    return read_field(f"{PYTHON_DIR}/pyproject.toml", r'^name = "([^"]+)"')


def pypi_published(version: str) -> bool:
    index = os.environ.get("PYPI_URL") or "https://pypi.org"
    project = os.environ.get("PYPI_PROJECT") or python_project()
    return http_json(f"{index}/pypi/{project}/{version}/json") is not None


def libs_api(path: str, method: str = "GET", body: dict | None = None) -> Any:
    """GitHub API of the simplex-chat-libs repository."""
    repo = os.environ.get("LIBS_REPOSITORY") or "simplex-chat/simplex-chat-libs"
    headers = {"Accept": "application/vnd.github+json"}
    if token := os.environ.get("GH_TOKEN"):
        headers["Authorization"] = f"Bearer {token}"
    return http_json(f"https://api.github.com/repos/{repo}/{path}", headers, method, body)


def github_release(tag: str) -> dict | None:
    return libs_api(f"releases/tags/{tag}")


def read_releases() -> tuple[dict, str | None]:
    """The releases and the blob of the file, which a change must name."""
    file = libs_api(f"contents/{RELEASES_FILE}")
    if file is None:
        return {}, None
    return tomllib.loads(base64.b64decode(file["content"]).decode()), file["sha"]


def record_releases(entries: list[tuple[str, str]], commit: str) -> str:
    """Adds the entries in one commit of simplex-chat-libs and returns that commit."""
    message = ", ".join(f"{key} {version}" for key, version in entries)
    attempt = 1
    while True:
        releases, blob = read_releases()
        updated = add_releases(releases, entries, commit)
        if blob and updated == releases:
            return libs_api(f"commits?path={RELEASES_FILE}&per_page=1")[0]["sha"]
        body = {
            "message": message,
            "content": base64.b64encode(render_releases(updated).encode()).decode(),
            "committer": {"name": "SimpleX Chat", "email": "build@simplex.chat"},
            **({"sha": blob} if blob else {}),
        }
        try:
            return libs_api(f"contents/{RELEASES_FILE}", "PUT", body)["commit"]["sha"]
        except urllib.error.HTTPError as e:
            # 409 and 422: another writer changed or created the file since it was read
            if e.code not in (409, 422) or attempt == RECORD_ATTEMPTS:
                raise
            attempt += 1


def commit_exists(commit: str) -> bool:
    found = subprocess.run(["git", "cat-file", "-e", f"{commit}^{{commit}}"], cwd=ROOT, capture_output=True)
    return found.returncode == 0


def npm_key(directory: str) -> str:
    return f"npm/{read_json(f'{directory}/package.json')['name']}"


def pypi_key() -> str:
    return f"pypi/{python_project()}"


def npm_package(directory: str, releases: dict) -> Package:
    package = read_json(f"{directory}/package.json")
    name, version = package["name"], package["version"]
    return package_state(
        npm_key(directory),
        version,
        directory,
        f"{directory}/package.json",
        '"version":',
        releases,
        lambda: npm_published(name, version),
    )


def prepare(args: argparse.Namespace) -> None:
    cabal_version = read_field("simplex-chat.cabal", r"^version:\s*(\S+)")
    libs_tag = f"core-{cabal_version}"
    addon = current_addon_tag()
    python_version, python_pins = parse_python_version(read_text(PYTHON_VERSION_FILE))
    releases, _ = read_releases()
    result = plan(
        State(
            libs_tag=libs_tag,
            libs_digests=release_digests(github_release(libs_tag), LIBS_ASSETS),
            addon_tag=addon,
            addon_digests=release_digests(github_release(addon), ADDON_ASSETS),
            nodejs_pins=parse_release(read_text(RELEASE_FILE)),
            python_pins=python_pins,
            types=npm_package(TYPES_DIR, releases),
            nodejs=npm_package(NODEJS_DIR, releases),
            python=package_state(
                pypi_key(),
                python_version,
                PYTHON_DIR,
                PYTHON_VERSION_FILE,
                "^__version__ =",
                releases,
                lambda: pypi_published(python_version),
            ),
            types_version=args.types_version,
            nodejs_version=args.nodejs_version,
            python_version=args.python_version,
        )
    )
    (ROOT / RELEASE_FILE).write_text(render_release(result.nodejs_pins))
    (ROOT / PYTHON_VERSION_FILE).write_text(render_python_version(result.python_version, result.python_pins))
    types = read_json(f"{TYPES_DIR}/package.json")
    write_json(f"{TYPES_DIR}/package.json", {**types, "version": result.types_version})
    nodejs = read_json(f"{NODEJS_DIR}/package.json")
    dependencies = nodejs["dependencies"]
    if result.types_dependency:
        dependencies = {**dependencies, "@simplex-chat/types": result.types_dependency}
    write_json(f"{NODEJS_DIR}/package.json", {**nodejs, "version": result.nodejs_version, "dependencies": dependencies})
    summary = result.title + "\n\n" + "".join(f"- {note}\n" for note in result.notes)
    if args.summary:
        Path(args.summary).write_text(summary)
    print(summary, end="")


def publish_plan() -> None:
    """A recorded version is published: npm and PyPI list a new version minutes after it is published."""
    releases, _ = read_releases()
    for key, directory in [("types", TYPES_DIR), ("nodejs", NODEJS_DIR)]:
        package = read_json(f"{directory}/package.json")
        published = recorded_commit(releases, npm_key(directory), package["version"]) or npm_published(
            package["name"], package["version"]
        )
        print(f"{key}_version={'' if published else package['version']}")
        print(f"{key}_dist_tag={dist_tag(package['version'])}")
    version, _ = parse_python_version(read_text(PYTHON_VERSION_FILE))
    published = recorded_commit(releases, pypi_key(), version) or pypi_published(version)
    print(f"python_version={'' if published else version}")


def record(args: argparse.Namespace) -> None:
    versions = [
        ("core", args.core),
        (npm_key(TYPES_DIR), args.types),
        (npm_key(NODEJS_DIR), args.nodejs),
        (pypi_key(), args.python),
    ]
    entries = [(key, version) for key, version in versions if version]
    if not entries:
        sys.exit("nothing to record")
    print(record_releases(entries, args.commit))


def test_names() -> None:
    if scope := os.environ.get("NPM_SCOPE"):
        # npm provenance requires the repository of the publishing workflow.
        url = f"git+https://github.com/{os.environ['GITHUB_REPOSITORY']}.git"
        types = read_json(f"{TYPES_DIR}/package.json")
        write_json(
            f"{TYPES_DIR}/package.json",
            {**types, "name": registry_name(types["name"], scope), "repository": {**types["repository"], "url": url}},
        )
        nodejs = read_json(f"{NODEJS_DIR}/package.json")
        alias = f"npm:{registry_name(types['name'], scope)}@{nodejs['dependencies'][types['name']]}"
        write_json(
            f"{NODEJS_DIR}/package.json",
            {
                **nodejs,
                "name": registry_name(nodejs["name"], scope),
                "repository": {**nodejs["repository"], "url": url},
                "dependencies": {**nodejs["dependencies"], types["name"]: alias},
            },
        )
    if project := os.environ.get("PYPI_PROJECT"):
        pyproject = ROOT / PYTHON_DIR / "pyproject.toml"
        pyproject.write_text(
            re.sub(r'^name = "[^"]+"', f'name = "{project}"', pyproject.read_text(), count=1, flags=re.M)
        )


def github_bool(value: str) -> bool:
    """GitHub expressions pass booleans to the shell as "true", "false" or ""."""
    return value == "true"


def main(argv: list[str]) -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("addon-tag")
    changes_parser = commands.add_parser("changes")
    changes_parser.add_argument("base", help="the commit to compare with")
    build_parser = commands.add_parser("build-plan")
    build_parser.add_argument("--event", required=True, help="github.event_name")
    build_parser.add_argument("--base", default="", help="the base commit of a pull request")
    build_parser.add_argument("--release-missing", type=github_bool, default=False)
    build_parser.add_argument("--libs-artifact", type=github_bool, default=False)
    build_parser.add_argument("--libs-release", default="")
    prepare_parser = commands.add_parser("prepare")
    for package in ["nodejs", "types", "python"]:
        prepare_parser.add_argument(f"--{package}-version", help="this version instead of a bump when needed")
    prepare_parser.add_argument("--summary", help="file for the PR title and body")
    commands.add_parser("publish-plan")
    record_parser = commands.add_parser("record")
    record_parser.add_argument("--commit", required=True, help="the simplex-chat commit released or published from")
    for name in ["core", "types", "nodejs", "python"]:
        record_parser.add_argument(f"--{name}", default="", help=f"{name} version, recorded when not empty")
    commands.add_parser("test-names")
    args = parser.parse_args(argv)
    if args.command == "addon-tag":
        print(current_addon_tag())
    elif args.command == "changes":
        changes(args.base)
    elif args.command == "build-plan":
        print_build_plan(args)
    elif args.command == "prepare":
        prepare(args)
    elif args.command == "publish-plan":
        publish_plan()
    elif args.command == "record":
        record(args)
    else:
        test_names()


if __name__ == "__main__":
    main(sys.argv[1:])
