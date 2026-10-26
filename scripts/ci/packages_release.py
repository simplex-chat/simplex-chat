#!/usr/bin/env python3
"""Release automation for simplex-chat and @simplex-chat/types on npm and simplex-chat on PyPI.

core-release      set the core version and the types version for a library release
claim-core        claim the library release of the core version for a commit
release-status    print whether a release of simplex-chat-libs is published, a draft or absent
addon-release     print the add-on release of the current add-on sources
latest-libs       print the newest published library release
publish-release   publish a draft release that has every asset
check-pins        check the pins of the packages against the published releases
download          download a release asset and check its digest
download-pinned   download a pinned asset and check its pin
changes           print whether the changes since a commit touch the chat core, the add-on, each build of build.yml and each scope of scope-tests.sh
scopes            print every scope of scope-tests.sh
build-plan        print the platforms to test the packages on, none when build.yml builds the core for the pull request
prepare           update pins and versions of the packages
publish-plan      print the package versions that are not published yet
record            record releases and published versions in releases.toml of simplex-chat-libs
test-names        rename the packages to NPM_SCOPE and PYPI_PROJECT (test repositories)
"""

from __future__ import annotations

import argparse
import base64
import copy
import hashlib
import json
import os
import re
import shlex
import subprocess
import sys
import tempfile
import tomllib
import urllib.error
import urllib.request
from collections.abc import Callable, Collection
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
# The pull_request paths of build.yml are the union of the globs of BUILD_CHANGES and SCOPE_CHANGES; ":!" excludes a path from its build.
BUILD_CHANGES = {
    "haskell": [
        "src/**",
        "apps/simplex-chat/**",
        "apps/simplex-bot/**",
        "apps/simplex-bot-advanced/**",
        "apps/simplex-badge-service/**",
        "apps/simplex-broadcast-bot/**",
        "apps/simplex-directory-service/**",
        "tests/**",
        "bots/src/**",
        "simplex-chat.cabal",
        "cabal.project",
        "Dockerfile*",
        "scripts/ci/**",
        ":!scripts/ci/ios-stub-libs.sh",
        "scripts/desktop/**",
        ".github/**",
    ],
    "kotlin": ["apps/multiplatform/**", "Dockerfile*", ".github/**"],
    "ios": ["apps/ios/**", "scripts/ci/ios-stub-libs.sh"],
    "packages": [
        "packages/simplex-chat-nodejs/**",
        "packages/simplex-chat-client/types/typescript/**",
        "packages/simplex-chat-python/**",
        "scripts/ci/package-tests.sh",
    ],
}
SCOPE_CHANGES = {
    "badge-web": ["apps/simplex-badge-service/web/**"],
    "ci-scripts": ["scripts/ci/packages_release.py", "scripts/ci/test_packages_release.py"],
}
CABAL_FILE = "simplex-chat.cabal"
CABAL_VERSION = re.compile(r"^(version:\s*)(\S+)", re.M)
TYPES_SOURCES = [f"{TYPES_DIR}/src"]
TYPES_README = f"{TYPES_DIR}/README.md"
TYPES_README_CORE = re.compile(r"^SimpleX Chat core: .*$", re.M)
PYTHON_TYPES_SOURCES = [f"{PYTHON_DIR}/src/simplex_chat/types"]
ADDON_HASH_LENGTH = 12
CORE_TAG = re.compile(r"^core-(\d+(?:\.\d+)*)$")
ADDON_TAG_PREFIX = "nodejs-addon-"
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
ADDON_ASSETS = [f"simplex-chat-nodejs-{platform}.node" for platform in ADDON_PLATFORMS]
SHA256_HEX = re.compile(r"^[0-9a-f]{64}$")
RELEASES_FILE = "releases.toml"
RECORD_ATTEMPTS = 5
DOWNLOAD_CHUNK = 1 << 20
TOML_BARE_KEY = re.compile(r"^[A-Za-z0-9_-]+$")


# Pure functions


def addon_hash(files: dict[str, bytes], node_addon_api: str) -> str:
    digest = hashlib.sha256()
    for name in sorted(files):
        digest.update(name.encode() + b"\0" + files[name] + b"\0")
    digest.update(f"node-addon-api@{node_addon_api}".encode())
    return digest.hexdigest()[:ADDON_HASH_LENGTH]


def addon_notes(sources_hash: str) -> str:
    return f"sources: {sources_hash}"


def version_key(version: str) -> tuple[int, ...]:
    return tuple(int(part) for part in version.split("."))


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
        "Written by scripts/ci/packages_release.py prepare: it bumps the version\n"
        "and pins new library releases with the SHA-256 of their assets.\n"
        '"""\n'
        "\n"
        f'__version__ = "{version}"  # PEP 440 — read by hatchling for wheel metadata\n'
        f'LIBS_RELEASE = "{pins.libs_release}"  # simplex-chat-libs release tag\n'
        "LIBS_SHA256 = {\n"
        f"{hashes}}}\n"
    )


def asset_digests(release: dict) -> dict[str, str]:
    return {a["name"]: (a.get("digest") or "").removeprefix("sha256:") for a in release["assets"]}


def release_digests(release: dict | None, assets: list[str]) -> dict[str, str] | None:
    """Asset digests of a GitHub release, or None when the release is missing or incomplete."""
    if release is None:
        return None
    digests = asset_digests(release)
    if not all(SHA256_HEX.match(digests.get(asset, "")) for asset in assets):
        return None
    return {asset: digests[asset] for asset in assets}


def release_assets(tag: str) -> list[str]:
    if CORE_TAG.match(tag):
        return LIBS_ASSETS
    if tag.startswith(ADDON_TAG_PREFIX):
        return ADDON_ASSETS
    raise ValueError(f"no asset set for release {tag}")


def preferred_release(releases: list[dict]) -> dict | None:
    """The published release among them, otherwise the first draft."""
    return next((r for r in releases if not r["draft"]), next(iter(releases), None))


def release_status(release: dict | None) -> str:
    if release is None:
        return "absent"
    return "draft" if release["draft"] else "published"


def tag_release(releases: list[dict], tag: str) -> dict | None:
    return preferred_release([r for r in releases if r["tag_name"] == tag])


def addon_release(releases: list[dict], sources_hash: str) -> dict | None:
    notes = addon_notes(sources_hash)
    return preferred_release(
        [r for r in releases if r["tag_name"].startswith(ADDON_TAG_PREFIX) and r.get("body") == notes]
    )


def core_version(tag: str) -> str | None:
    match = CORE_TAG.match(tag)
    return match[1] if match else None


def latest_libs(releases: list[dict], versions: Collection[str]) -> str | None:
    """The newest published release of the core versions that has every library asset."""
    tags = [
        r["tag_name"]
        for r in releases
        if not r["draft"] and core_version(r["tag_name"]) in versions and release_digests(r, LIBS_ASSETS)
    ]
    return max(tags, key=lambda tag: version_key(core_version(tag)), default=None)


def missing_assets(release: dict) -> list[str]:
    present = {a["name"] for a in release["assets"]}
    return [asset for asset in release_assets(release["tag_name"]) if asset not in present]


def pin_problems(pins: Pins, published: dict[str, dict | None]) -> list[str]:
    """Differences between the pins and the published releases of the pinned tags."""
    problems = []
    for tag, release in published.items():
        if release is None:
            problems.append(f"{tag} is not published")
            continue
        digests = asset_digests(release)
        for asset in release_assets(tag):
            if pins.sha256.get(asset) != digests.get(asset):
                problems.append(f"{asset} of {tag}: pinned {pins.sha256.get(asset)}, published {digests.get(asset)}")
    return problems


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
    types_dependency: str
    nodejs_types_dependency: str
    nodejs: Package
    python: Package
    packages: frozenset[str] = frozenset({"nodejs", "python"})
    nodejs_version: str | None = None
    python_version: str | None = None


@dataclass(frozen=True)
class Plan:
    nodejs_pins: Pins
    python_pins: Pins
    types_dependency: str | None
    nodejs_version: str
    nodejs_changed: bool
    python_version: str
    python_changed: bool
    notes: list[str] = field(default_factory=list)

    @property
    def title(self) -> str:
        parts = []
        if self.nodejs_changed:
            parts.append(f"nodejs {self.nodejs_version}")
        if self.python_changed:
            parts.append(f"python {self.python_version}")
        return f"packages: release {', '.join(parts)}"


def plan(state: State) -> Plan:
    """What a package release changes, from the state of the branch and the releases."""
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

    def next_version(pkg: Package, changed: bool, override: str | None, bump) -> str:
        if override:
            return override
        return bump(pkg.version) if changed and pkg.published else pkg.version

    nodejs_pins, nodejs_version, nodejs_changed, types_dependency = state.nodejs_pins, state.nodejs.version, False, None
    if "nodejs" in state.packages:
        nodejs_pins, nodejs_libs = pin(
            state.nodejs_pins, "libs_release", state.libs_tag, state.libs_digests, "Node.js libsimplex"
        )
        nodejs_pins, addon = pin(nodejs_pins, "addon_release", state.addon_tag, state.addon_digests, "Node.js add-on")
        if state.types_dependency != state.nodejs_types_dependency:
            types_dependency = state.types_dependency
            notes.append(f"@simplex-chat/types: {types_dependency}")
        nodejs_changed = nodejs_libs or addon or types_dependency is not None or state.nodejs.changed
        nodejs_version = next_version(state.nodejs, nodejs_changed, state.nodejs_version, bump_npm_version)
    python_pins, python_version, python_changed = state.python_pins, state.python.version, False
    if "python" in state.packages:
        python_pins, python_libs = pin(
            state.python_pins, "libs_release", state.libs_tag, state.libs_digests, "Python libsimplex"
        )
        python_changed = python_libs or state.python.changed
        python_version = next_version(state.python, python_changed, state.python_version, bump_python_version)
    for name, old, new in [
        ("simplex-chat (npm)", state.nodejs.version, nodejs_version),
        ("simplex-chat (PyPI)", state.python.version, python_version),
    ]:
        if old != new:
            notes.append(f"{name}: {old} → {new}")
    return Plan(
        nodejs_pins=nodejs_pins,
        python_pins=python_pins,
        types_dependency=types_dependency,
        nodejs_version=nodejs_version,
        nodejs_changed=nodejs_changed or nodejs_version != state.nodejs.version,
        python_version=python_version,
        python_changed=python_changed or python_version != state.python.version,
        notes=notes,
    )


def build_matrix(platforms: list[str]) -> str:
    return json.dumps([{"platform": platform, "os": ADDON_RUNNERS[platform]} for platform in platforms])


def core_key(branch: str) -> str:
    return f"core/{branch}"


def recorded_versions(releases: dict, key: str) -> dict[str, str]:
    """The recorded versions and their simplex-chat commits; key is "core/<branch>", "npm/<name>" or "pypi/<name>"."""
    table = releases
    for part in key.split("/", 1):
        table = table.get(part, {})
    return {version: commit for version, commit in table.items() if isinstance(commit, str)}


def recorded_commit(releases: dict, key: str, version: str) -> str | None:
    return recorded_versions(releases, key).get(version)


def highest_recorded(releases: dict, key: str) -> str | None:
    return max(recorded_versions(releases, key), key=version_key, default=None)


def recorded_core_commit(releases: dict, version: str) -> str | None:
    """The commit recorded for the core version on any branch: a release tag is shared by the branches."""
    for branch in releases.get("core", {}):
        if commit := recorded_commit(releases, core_key(branch), version):
            return commit
    return None


def previous_core_commit(releases: dict, branch: str) -> str | None:
    """The commit of the highest core version recorded for the branch."""
    previous = highest_recorded(releases, core_key(branch))
    return recorded_commit(releases, core_key(branch), previous) if previous else None


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


def current_addon_hash() -> str:
    files = {
        path.relative_to(ROOT).as_posix(): path.read_bytes()
        for source in ADDON_SOURCES
        for path in ([ROOT / source] if (ROOT / source).is_file() else sorted((ROOT / source).rglob("*")))
        if path.is_file()
    }
    return addon_hash(files, read_json(f"{NODEJS_DIR}/package.json")["devDependencies"]["node-addon-api"])


def cabal_version() -> str:
    return CABAL_VERSION.search(read_text(CABAL_FILE))[2]


def changed_since(base: str, sources: list[str]) -> bool:
    diff = subprocess.run(["git", "diff", "--quiet", base, "HEAD", "--", *sources], cwd=ROOT)
    return diff.returncode != 0


def build_pathspecs(build: str) -> list[str]:
    return [path if path.startswith(":") else f":(glob){path}" for path in (BUILD_CHANGES | SCOPE_CHANGES)[build]]


def changes(base: str) -> None:
    print(f"core={'true' if changed_since(base, CORE_SOURCES) else ''}")
    print(f"addon={'true' if changed_since(base, ADDON_SOURCES) else ''}")
    for build in BUILD_CHANGES:
        print(f"{build}={'true' if changed_since(base, build_pathspecs(build)) else ''}")
    print(f"scopes={' '.join(scope for scope in SCOPE_CHANGES if changed_since(base, build_pathspecs(scope)))}")


def print_build_plan(args: argparse.Namespace) -> None:
    platforms = [] if changed_since(args.base, build_pathspecs("haskell")) else ADDON_PLATFORMS
    print(f"platforms={build_matrix(platforms)}")


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
def npm_package_url(name: str) -> str:
    registry = os.environ.get("NPM_REGISTRY") or "https://registry.npmjs.org"
    return f"{registry}/{registry_name(name, os.environ.get('NPM_SCOPE')).replace('/', '%2F')}"


def npm_published(name: str, version: str) -> bool:
    return http_json(f"{npm_package_url(name)}/{version}") is not None


def python_project() -> str:
    return read_field(f"{PYTHON_DIR}/pyproject.toml", r'^name = "([^"]+)"')


def pypi_published(version: str) -> bool:
    index = os.environ.get("PYPI_URL") or "https://pypi.org"
    project = os.environ.get("PYPI_PROJECT") or python_project()
    return http_json(f"{index}/pypi/{project}/{version}/json") is not None


def libs_repository() -> str:
    return os.environ.get("LIBS_REPOSITORY") or "simplex-chat/simplex-chat-libs"


def libs_api(path: str, method: str = "GET", body: dict | None = None) -> Any:
    """GitHub API of the simplex-chat-libs repository."""
    headers = {"Accept": "application/vnd.github+json"}
    if token := os.environ.get("GH_TOKEN"):
        headers["Authorization"] = f"Bearer {token}"
    return http_json(f"https://api.github.com/repos/{libs_repository()}/{path}", headers, method, body)


def github_release(tag: str) -> dict | None:
    """The published release of the tag."""
    return libs_api(f"releases/tags/{tag}")


def libs_releases() -> list[dict]:
    """The releases of simplex-chat-libs; drafts are listed only with a token that can push to it."""
    releases: list[dict] = []
    page = 1
    while batch := libs_api(f"releases?per_page=100&page={page}"):
        releases += batch
        page += 1
    return releases


def download(tag: str, asset: str, directory: str, sha256: str) -> Path:
    """Saves the asset of the release to the directory; exits when its SHA-256 differs."""
    url = f"https://github.com/{libs_repository()}/releases/download/{tag}/{asset}"
    target = Path(directory) / asset
    target.parent.mkdir(parents=True, exist_ok=True)
    digest = hashlib.sha256()
    request = urllib.request.Request(url, headers={"User-Agent": "simplex-chat-release"})
    with urllib.request.urlopen(request, timeout=600) as response, target.open("wb") as file:
        while chunk := response.read(DOWNLOAD_CHUNK):
            file.write(chunk)
            digest.update(chunk)
    if digest.hexdigest() != sha256:
        target.unlink()
        sys.exit(f"SHA-256 of {asset} of {tag} is {digest.hexdigest()}, expected {sha256}")
    return target


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


def fetch_commit(commit: str) -> str:
    """The commit, fetched from origin when the checkout lacks it."""
    if not commit_exists(commit):
        git("fetch", "-q", "--depth=1", "origin", commit)
    return commit


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


def types_changed(releases: dict, branch: str) -> bool:
    """Whether the types sources differ from those of the previous core release on the branch."""
    previous = previous_core_commit(releases, branch)
    return previous is None or changed_since(fetch_commit(previous), TYPES_SOURCES)


def types_problem(version: str, releases: dict, branch: str) -> str | None:
    """Why the types can't be released with core <version>, or None when they can."""
    if not types_changed(releases, branch):
        return None
    types = read_json(f"{TYPES_DIR}/package.json")
    if recorded_commit(releases, npm_key(TYPES_DIR), types["version"]) or npm_published(types["name"], types["version"]):
        return f"{types['name']} changed, and its version {types['version']} is published"
    if types.get("simplexChatCore") != version:
        return f"simplexChatCore of {types['name']} is {types.get('simplexChatCore')}, expected {version}"
    return None


def claim_core(version: str, commit: str, branch: str) -> None:
    releases, _ = read_releases()
    highest = highest_recorded(releases, core_key(branch))
    if highest and version_key(version) < version_key(highest):
        sys.exit(f"core {version} is lower than core {highest} recorded for {branch}")
    if recorded := recorded_core_commit(releases, version):
        print(f"claim={'resume' if recorded == commit else 'skip'}")
        return
    if problem := types_problem(version, releases, branch):
        sys.exit(problem)
    libs_commit = record_releases([(core_key(branch), version)], commit)
    releases, _ = read_releases()
    if recorded_core_commit(releases, version) == commit:
        print("claim=new")
        print(f"commit={libs_commit}")
    else:
        print("claim=skip")


def current_branch() -> str:
    return git("rev-parse", "--abbrev-ref", "HEAD")


def core_release(version: str, branch: str, types_version: str | None) -> None:
    releases, _ = read_releases()
    highest = highest_recorded(releases, core_key(branch))
    if highest and version_key(version) <= version_key(highest):
        sys.exit(f"core {version} is not higher than core {highest} recorded for {branch}")
    (ROOT / CABAL_FILE).write_text(CABAL_VERSION.sub(rf"\g<1>{version}", read_text(CABAL_FILE), count=1))
    print(f"{CABAL_FILE}: {version}")
    if not types_version and not types_changed(releases, branch):
        return
    types = read_json(f"{TYPES_DIR}/package.json")
    if not types_version:
        previous = previous_core_commit(releases, branch)
        released = json.loads(git("show", f"{previous}:{TYPES_DIR}/package.json")) if previous else types
        types_version = bump_npm_version(released["version"])
    readme, found = TYPES_README_CORE.subn(f"SimpleX Chat core: {version}", read_text(TYPES_README), count=1)
    if not found:
        sys.exit(f"{TYPES_README} has no line 'SimpleX Chat core: <version>'")
    write_json(f"{TYPES_DIR}/package.json", {**types, "version": types_version, "simplexChatCore": version})
    (ROOT / TYPES_README).write_text(readme)
    print(f"{types['name']}: {types_version}")


def pinned_tags(pins: Pins) -> list[str]:
    return [tag for tag in (pins.libs_release, pins.addon_release) if tag]


def published_pin_problems(pins: Pins) -> list[str]:
    return pin_problems(pins, {tag: github_release(tag) for tag in pinned_tags(pins)})


def prepare(args: argparse.Namespace) -> None:
    packages = frozenset(args.packages or ["nodejs", "python"])
    branch = args.branch or current_branch()
    releases, _ = read_releases()
    libs = libs_releases()
    libs_tag = latest_libs(libs, recorded_versions(releases, core_key(branch)))
    if libs_tag is None:
        sys.exit(f"no published library release recorded for {branch}")
    core = core_version(libs_tag)
    if version_key(cabal_version()) > version_key(core):
        print(f"warning: core-{cabal_version()} is not published, {libs_tag} is pinned", file=sys.stderr)
    sources_hash = current_addon_hash()
    addon = addon_release(libs, sources_hash)
    addon_digests = release_digests(addon, ADDON_ASSETS) if release_status(addon) == "published" else None
    if "nodejs" in packages and addon_digests is None:
        sys.exit(f"the add-on release of sources {sources_hash} is not published with every asset")
    types = read_json(f"{TYPES_DIR}/package.json")
    types_name = types["name"]
    if "nodejs" in packages and not npm_published(types_name, types["version"]):
        sys.exit(f"{types_name} {types['version']} is not published")
    if "python" in packages:
        core_commit = recorded_commit(releases, core_key(branch), core)
        if changed_since(core_commit, PYTHON_TYPES_SOURCES):
            sys.exit(f"the Python types differ from those of {libs_tag}")
    nodejs = read_json(f"{NODEJS_DIR}/package.json")
    python_version, python_pins = parse_python_version(read_text(PYTHON_VERSION_FILE))
    result = plan(
        State(
            libs_tag=libs_tag,
            libs_digests=release_digests(tag_release(libs, libs_tag), LIBS_ASSETS),
            addon_tag=addon["tag_name"] if addon else "",
            addon_digests=addon_digests,
            nodejs_pins=parse_release(read_text(RELEASE_FILE)),
            python_pins=python_pins,
            types_dependency=f"^{types['version']}",
            nodejs_types_dependency=nodejs["dependencies"][types_name],
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
            packages=packages,
            nodejs_version=args.nodejs_version,
            python_version=args.python_version,
        )
    )
    selected_pins = [
        pins for name, pins in [("nodejs", result.nodejs_pins), ("python", result.python_pins)] if name in packages
    ]
    if problems := [problem for pins in selected_pins for problem in published_pin_problems(pins)]:
        sys.exit("\n".join(problems))
    assets = {
        (tag, asset, pins.sha256[asset])
        for pins in selected_pins
        for tag in pinned_tags(pins)
        for asset in release_assets(tag)
    }
    with tempfile.TemporaryDirectory() as directory:
        for tag, asset, sha256 in sorted(assets):
            download(tag, asset, directory, sha256)
            print(f"checked {asset} of {tag}", file=sys.stderr)
    if "nodejs" in packages:
        (ROOT / RELEASE_FILE).write_text(render_release(result.nodejs_pins))
        dependencies = nodejs["dependencies"]
        if result.types_dependency:
            dependencies = {**dependencies, types_name: result.types_dependency}
        write_json(f"{NODEJS_DIR}/package.json", {**nodejs, "version": result.nodejs_version, "dependencies": dependencies})
    if "python" in packages:
        (ROOT / PYTHON_VERSION_FILE).write_text(render_python_version(result.python_version, result.python_pins))
    print(result.title + "\n\n" + "".join(f"- {note}\n" for note in result.notes), end="")


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
        (core_key(args.branch), args.core),
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


def print_release_status(tag: str) -> None:
    print(f"status={release_status(tag_release(libs_releases(), tag))}")


def print_addon_release() -> None:
    sources_hash = current_addon_hash()
    release = addon_release(libs_releases(), sources_hash)
    print(f"notes={shlex.quote(addon_notes(sources_hash))}")
    print(f"status={release_status(release)}")
    print(f"tag={release['tag_name'] if release else ''}")


def publish_release(tag: str) -> None:
    release = tag_release(libs_releases(), tag)
    if release is None:
        sys.exit(f"{tag}: no release")
    if not release["draft"]:
        print(f"{tag} is published")
        return
    if missing := missing_assets(release):
        sys.exit(f"{tag}: missing {', '.join(missing)}")
    libs_api(f"releases/{release['id']}", "PATCH", {"draft": False})
    print(f"{tag} published")


def check_pins() -> None:
    _, python_pins = parse_python_version(read_text(PYTHON_VERSION_FILE))
    problems = published_pin_problems(parse_release(read_text(RELEASE_FILE))) + published_pin_problems(python_pins)
    if problems:
        sys.exit("\n".join(problems))
    print("the pins match the published releases")


def download_asset(tag: str, asset: str, directory: str) -> None:
    digests = release_digests(github_release(tag), [asset])
    if digests is None:
        sys.exit(f"{asset} of {tag} is not published with a digest")
    download(tag, asset, directory, digests[asset])


def download_pinned(asset: str, directory: str) -> None:
    pins = parse_release(read_text(RELEASE_FILE))
    tag = next((tag for tag in pinned_tags(pins) if asset in release_assets(tag)), None)
    if tag is None or asset not in pins.sha256:
        sys.exit(f"{asset} is not pinned in {RELEASE_FILE}")
    download(tag, asset, directory, pins.sha256[asset])


def main(argv: list[str]) -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest="command", required=True)
    branch_help = "the branch of the core versions, the checked out branch by default"
    core_release_parser = commands.add_parser("core-release")
    core_release_parser.add_argument("version", help="the new core version")
    core_release_parser.add_argument("--branch", help=branch_help)
    core_release_parser.add_argument("--types-version", help="this types version instead of a bump when needed")
    claim_parser = commands.add_parser("claim-core")
    claim_parser.add_argument("version", help="the core version of simplex-chat.cabal")
    claim_parser.add_argument("commit", help="the simplex-chat commit to release it from")
    claim_parser.add_argument("branch", help="the branch of the commit")
    status_parser = commands.add_parser("release-status")
    status_parser.add_argument("tag")
    commands.add_parser("addon-release")
    latest_libs_parser = commands.add_parser("latest-libs")
    latest_libs_parser.add_argument("--branch", help=branch_help)
    publish_release_parser = commands.add_parser("publish-release")
    publish_release_parser.add_argument("tag")
    commands.add_parser("check-pins")
    download_parser = commands.add_parser("download")
    download_parser.add_argument("tag")
    download_parser.add_argument("asset")
    download_parser.add_argument("directory")
    download_pinned_parser = commands.add_parser("download-pinned")
    download_pinned_parser.add_argument("asset")
    download_pinned_parser.add_argument("directory")
    changes_parser = commands.add_parser("changes")
    changes_parser.add_argument("base", help="the commit to compare with")
    commands.add_parser("scopes")
    build_parser = commands.add_parser("build-plan")
    build_parser.add_argument("--base", required=True, help="the base commit of the pull request")
    prepare_parser = commands.add_parser("prepare")
    prepare_parser.add_argument("packages", nargs="*", choices=["nodejs", "python"], help="all when none is listed")
    prepare_parser.add_argument("--branch", help=branch_help)
    for package in ["nodejs", "python"]:
        prepare_parser.add_argument(f"--{package}-version", help="this version instead of a bump when needed")
    commands.add_parser("publish-plan")
    record_parser = commands.add_parser("record")
    record_parser.add_argument("--commit", required=True, help="the simplex-chat commit released or published from")
    record_parser.add_argument("--branch", default="master", help="the branch of the core version")
    for name in ["core", "types", "nodejs", "python"]:
        record_parser.add_argument(f"--{name}", default="", help=f"{name} version, recorded when not empty")
    commands.add_parser("test-names")
    args = parser.parse_args(argv)
    if args.command == "core-release":
        core_release(args.version, args.branch or current_branch(), args.types_version)
    elif args.command == "claim-core":
        claim_core(args.version, args.commit, args.branch)
    elif args.command == "release-status":
        print_release_status(args.tag)
    elif args.command == "addon-release":
        print_addon_release()
    elif args.command == "latest-libs":
        releases, _ = read_releases()
        versions = recorded_versions(releases, core_key(args.branch or current_branch()))
        print(f"tag={latest_libs(libs_releases(), versions) or ''}")
    elif args.command == "publish-release":
        publish_release(args.tag)
    elif args.command == "check-pins":
        check_pins()
    elif args.command == "download":
        download_asset(args.tag, args.asset, args.directory)
    elif args.command == "download-pinned":
        download_pinned(args.asset, args.directory)
    elif args.command == "changes":
        changes(args.base)
    elif args.command == "scopes":
        print(f"scopes={' '.join(SCOPE_CHANGES)}")
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
