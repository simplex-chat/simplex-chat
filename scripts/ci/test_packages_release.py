import argparse
import base64
import contextlib
import email.message
import hashlib
import io
import json
import re
import subprocess
import tomllib
import urllib.error
import tempfile
import unittest
from dataclasses import replace
from pathlib import Path
from unittest import mock

import packages_release
from packages_release import (
    ADDON_ASSETS,
    CABAL_FILE,
    NODEJS_DIR,
    PYTHON_DIR,
    PYTHON_VERSION_FILE,
    RELEASE_FILE,
    TYPES_DIR,
    TYPES_README,
    ADDON_PLATFORMS,
    BUILD_CHANGES,
    SCOPE_CHANGES,
    LIBS_ASSETS,
    Package,
    Pins,
    State,
    addon_hash,
    addon_notes,
    addon_release,
    build_matrix,
    bump_npm_version,
    bump_python_version,
    changed_since,
    changes,
    add_releases,
    claim_core,
    core_release,
    download,
    highest_recorded,
    latest_libs,
    missing_assets,
    package_state,
    pin_problems,
    prepare,
    print_build_plan,
    publish_plan,
    record_releases,
    recorded_commit,
    release_status,
    render_releases,
    dist_tag,
    npm_published,
    parse_python_version,
    parse_release,
    plan,
    pypi_published,
    registry_name,
    release_digests,
    render_python_version,
    render_release,
    tag_release,
    types_problem,
)


def sha(n: int) -> str:
    return f"{n:x}" * 64


def digests(assets: list[str], n: int) -> dict[str, str]:
    return {asset: sha(n) for asset in assets}


def github_release(assets: list[str], n: int) -> dict:
    return {"assets": [{"name": name, "digest": f"sha256:{sha(n)}"} for name in assets]}


def listed_release(tag: str, assets: list[str], n: int, draft: bool = False, body: str | None = None) -> dict:
    return {"id": n, "tag_name": tag, "draft": draft, "body": body, **github_release(assets, n)}


def state(**overrides) -> State:
    current = State(
        libs_tag="core-1.0.0.1",
        libs_digests=digests(LIBS_ASSETS, 1),
        addon_tag="nodejs-addon-aaa",
        addon_digests=digests(ADDON_ASSETS, 2),
        nodejs_pins=Pins("core-1.0.0.1", {**digests(LIBS_ASSETS, 1), **digests(ADDON_ASSETS, 2)}, "nodejs-addon-aaa"),
        python_pins=Pins("core-1.0.0.1", digests(LIBS_ASSETS, 1)),
        types_dependency="^0.12.1",
        nodejs_types_dependency="^0.12.1",
        nodejs=Package("7.1.0-beta.6.1", changed=False, published=True),
        python=Package("7.1.0b6", changed=False, published=True),
    )
    return replace(current, **overrides)


NEW_LIBS = {"libs_tag": "core-1.0.0.2", "libs_digests": digests(LIBS_ASSETS, 3)}
NEW_ADDON = {"addon_tag": "nodejs-addon-bbb", "addon_digests": digests(ADDON_ASSETS, 4)}


class VersionTest(unittest.TestCase):
    def test_npm_bump_adds_a_package_release_to_an_app_version(self):
        self.assertEqual(bump_npm_version("7.1.0-beta.6"), "7.1.0-beta.6.1")
        self.assertEqual(bump_npm_version("7.1.0-beta.6.1"), "7.1.0-beta.6.2")
        self.assertEqual(bump_npm_version("0.12.9"), "0.12.10")
        self.assertEqual(bump_npm_version("7.0.2"), "7.0.3")

    def test_python_bump_uses_post_releases(self):
        self.assertEqual(bump_python_version("7.1.0b6"), "7.1.0b6.post1")
        self.assertEqual(bump_python_version("7.1.0b6.post9"), "7.1.0b6.post10")
        self.assertEqual(bump_python_version("7.0.2"), "7.0.2.post1")

    def test_prereleases_publish_as_beta(self):
        self.assertEqual(dist_tag("7.1.0-beta.6.2"), "beta")
        self.assertEqual(dist_tag("7.1.0"), "latest")

    def test_test_scope_renames_npm_packages(self):
        self.assertEqual(registry_name("@simplex-chat/types", "@test"), "@test/types")
        self.assertEqual(registry_name("simplex-chat", "@test"), "@test/simplex-chat")
        self.assertEqual(registry_name("simplex-chat", None), "simplex-chat")


class AddonHashTest(unittest.TestCase):
    def test_depends_on_sources_and_node_addon_api(self):
        files = {"a.cc": b"1", "b.h": b"2"}
        sources_hash = addon_hash(files, "^8.5.0")
        self.assertRegex(sources_hash, r"^[0-9a-f]{12}$")
        self.assertEqual(addon_hash(dict(reversed(files.items())), "^8.5.0"), sources_hash)
        self.assertNotEqual(addon_hash({**files, "b.h": b"3"}, "^8.5.0"), sources_hash)
        self.assertNotEqual(addon_hash({"a.cc": b"1", "c.h": b"2"}, "^8.5.0"), sources_hash)
        self.assertNotEqual(addon_hash(files, "^8.6.0"), sources_hash)


class PinFilesTest(unittest.TestCase):
    def test_release_ts_round_trip(self):
        pins = Pins("core-1.0.0.1", {"b.zip": sha(2), "a.zip": sha(1)}, "nodejs-addon-aaa")
        self.assertEqual(parse_release(render_release(pins)), pins)
        self.assertIsNone(parse_release(render_release(replace(pins, addon_release=None))).addon_release)

    def test_python_version_round_trip(self):
        pins = Pins("core-1.0.0.1", {"b.zip": sha(2), "a.zip": sha(1)})
        self.assertEqual(parse_python_version(render_python_version("7.1.0b6.post1", pins)), ("7.1.0b6.post1", pins))


class ReleaseDigestsTest(unittest.TestCase):
    def test_requires_every_asset_with_a_digest(self):
        self.assertEqual(release_digests(github_release(LIBS_ASSETS, 3), LIBS_ASSETS), digests(LIBS_ASSETS, 3))
        self.assertIsNone(release_digests(github_release(LIBS_ASSETS[1:], 3), LIBS_ASSETS))
        self.assertIsNone(release_digests(None, LIBS_ASSETS))
        no_digests = {"assets": [{"name": name, "digest": None} for name in LIBS_ASSETS]}
        self.assertIsNone(release_digests(no_digests, LIBS_ASSETS))


class PlanTest(unittest.TestCase):
    def test_nothing_changes_when_current(self):
        result = plan(state())
        self.assertEqual((result.nodejs_changed, result.python_changed, result.types_dependency), (False, False, None))
        self.assertEqual(result.notes, [])

    def test_new_libs_release_updates_both_libraries(self):
        result = plan(state(**NEW_LIBS))
        self.assertEqual(result.nodejs_pins.libs_release, "core-1.0.0.2")
        self.assertEqual(result.python_pins.libs_release, "core-1.0.0.2")
        self.assertEqual(result.nodejs_pins.sha256[LIBS_ASSETS[0]], sha(3))
        self.assertEqual(result.nodejs_pins.sha256[ADDON_ASSETS[0]], sha(2))
        self.assertEqual(result.python_pins.sha256[LIBS_ASSETS[0]], sha(3))
        self.assertEqual(result.title, "packages: release nodejs 7.1.0-beta.6.2, python 7.1.0b6.post1")

    def test_new_addon_release_updates_nodejs_only(self):
        result = plan(state(**NEW_ADDON))
        self.assertEqual(result.nodejs_pins.addon_release, "nodejs-addon-bbb")
        self.assertEqual(result.nodejs_pins.sha256[ADDON_ASSETS[0]], sha(4))
        self.assertEqual(result.title, "packages: release nodejs 7.1.0-beta.6.2")

    def test_incomplete_release_keeps_the_pins(self):
        result = plan(state(addon_tag="nodejs-addon-bbb", addon_digests=None))
        self.assertEqual(result.nodejs_pins.addon_release, "nodejs-addon-aaa")
        self.assertFalse(result.nodejs_changed)
        self.assertEqual(
            result.notes,
            ["Node.js add-on release nodejs-addon-bbb is missing or incomplete; nodejs-addon-aaa stays pinned"],
        )

    def test_source_changes_bump_their_package(self):
        self.assertEqual(
            plan(state(nodejs=Package("7.1.0-beta.6.1", True, True))).title, "packages: release nodejs 7.1.0-beta.6.2"
        )
        self.assertEqual(
            plan(state(python=Package("7.1.0b6", True, True))).title, "packages: release python 7.1.0b6.post1"
        )

    def test_new_types_version_updates_the_nodejs_dependency(self):
        result = plan(state(types_dependency="^0.12.2"))
        self.assertEqual(result.types_dependency, "^0.12.2")
        self.assertEqual(result.title, "packages: release nodejs 7.1.0-beta.6.2")

    def test_all_changes_combine_into_one_release(self):
        result = plan(
            state(
                **NEW_LIBS,
                **NEW_ADDON,
                types_dependency="^0.12.2",
                nodejs=Package("7.1.0-beta.6.1", True, True),
                python=Package("7.1.0b6", True, True),
            )
        )
        self.assertEqual(
            (result.nodejs_pins.libs_release, result.nodejs_pins.addon_release), ("core-1.0.0.2", "nodejs-addon-bbb")
        )
        self.assertEqual(result.title, "packages: release nodejs 7.1.0-beta.6.2, python 7.1.0b6.post1")

    def test_unlisted_package_keeps_its_pins_and_version(self):
        result = plan(state(**NEW_LIBS, types_dependency="^0.12.2", packages=frozenset({"python"})))
        self.assertEqual(result.nodejs_pins, state().nodejs_pins)
        self.assertEqual(
            (result.nodejs_version, result.nodejs_changed, result.types_dependency), ("7.1.0-beta.6.1", False, None)
        )
        self.assertEqual(result.python_pins.libs_release, "core-1.0.0.2")
        self.assertEqual(result.title, "packages: release python 7.1.0b6.post1")

    def test_unpublished_versions_are_not_bumped_again(self):
        result = plan(
            state(
                **NEW_LIBS, nodejs=Package("7.1.0-beta.6.2", True, False), python=Package("7.1.0b6.post1", False, False)
            )
        )
        self.assertEqual(result.title, "packages: release nodejs 7.1.0-beta.6.2, python 7.1.0b6.post1")

    def test_requested_versions_win(self):
        result = plan(state(nodejs_version="7.2.0", python_version="7.2.0"))
        self.assertEqual(result.title, "packages: release nodejs 7.2.0, python 7.2.0")


class BuildPlanTest(unittest.TestCase):
    def test_build_yml_runs_for_the_paths_of_its_builds(self):
        workflow = (packages_release.ROOT / ".github/workflows/build.yml").read_text()
        found = re.search(r'^  pull_request:\n    paths:\n((?:      - ".*"\n)+)', workflow, re.M)
        self.assertIsNotNone(found, "no pull_request paths in build.yml")
        paths = re.findall(r'^      - "(.*)"$', found[1], re.M)
        globs = {path for build in (BUILD_CHANGES | SCOPE_CHANGES).values() for path in build if not path.startswith(":")}
        self.assertEqual(sorted(paths), sorted(globs))
        self.assertEqual(len(paths), len(set(paths)), "duplicate pull_request paths in build.yml")

    def test_matrix_names_the_runner_of_each_platform(self):
        self.assertEqual(
            build_matrix(["linux-x86_64", "windows-x86_64"]),
            '[{"platform": "linux-x86_64", "os": "ubuntu-22.04"}, {"platform": "windows-x86_64", "os": "windows-latest"}]',
        )


class RegistryUrlTest(unittest.TestCase):
    def urls(self, env: dict[str, str]) -> list[str]:
        with (
            mock.patch.dict("os.environ", env),
            mock.patch.object(packages_release, "http_json", return_value={}) as get,
        ):
            npm_published("simplex-chat", "1.0.0")
            pypi_published("1.0.0")
            packages_release.github_release("core-1.0.0.1")
        return [call.args[0] for call in get.call_args_list]

    def test_empty_variables_use_the_public_registries(self):
        self.assertEqual(
            self.urls({"NPM_REGISTRY": "", "NPM_SCOPE": "", "PYPI_URL": "", "PYPI_PROJECT": "", "LIBS_REPOSITORY": ""}),
            [
                "https://registry.npmjs.org/simplex-chat/1.0.0",
                "https://pypi.org/pypi/simplex-chat/1.0.0/json",
                "https://api.github.com/repos/simplex-chat/simplex-chat-libs/releases/tags/core-1.0.0.1",
            ],
        )

    def test_variables_select_test_registries(self):
        self.assertEqual(
            self.urls(
                {
                    "NPM_SCOPE": "@test",
                    "PYPI_URL": "https://test.pypi.org",
                    "PYPI_PROJECT": "sx-test",
                    "LIBS_REPOSITORY": "a/b-libs",
                }
            ),
            [
                "https://registry.npmjs.org/@test%2Fsimplex-chat/1.0.0",
                "https://test.pypi.org/pypi/sx-test/1.0.0/json",
                "https://api.github.com/repos/a/b-libs/releases/tags/core-1.0.0.1",
            ],
        )


class ReleasesFileTest(unittest.TestCase):
    RELEASES = {
        "core": {"7.1.0.11": "c1"},
        "npm": {"simplex-chat": {"7.1.0-beta.6.3": "n1"}, "@simplex-chat/types": {"0.12.2": "t1"}},
        "pypi": {"simplex-chat": {"7.1.0b6": "p1"}},
    }

    def test_renders_toml_that_reads_back(self):
        text = render_releases(self.RELEASES)
        self.assertEqual(tomllib.loads(text), self.RELEASES)
        self.assertIn('[core]\n"7.1.0.11" = "c1"\n', text)
        self.assertIn('[npm."@simplex-chat/types"]\n', text)
        self.assertIn("[npm.simplex-chat]\n", text)

    def test_keeps_the_order_of_recording(self):
        releases = {"npm": {"simplex-chat": {"7.1.0-beta.6.9": "a"}}}
        updated = add_releases(releases, [("npm/simplex-chat", "7.1.0-beta.6.10"), ("core", "7.1.0.15")], "b")
        self.assertEqual(
            render_releases(updated),
            '[npm.simplex-chat]\n"7.1.0-beta.6.9" = "a"\n"7.1.0-beta.6.10" = "b"\n\n[core]\n"7.1.0.15" = "b"\n',
        )

    def test_finds_the_recorded_commit(self):
        self.assertEqual(recorded_commit(self.RELEASES, "npm/@simplex-chat/types", "0.12.2"), "t1")
        self.assertEqual(recorded_commit(self.RELEASES, "core", "7.1.0.11"), "c1")
        self.assertIsNone(recorded_commit(self.RELEASES, "npm/simplex-chat", "7.1.0-beta.6.4"))
        self.assertIsNone(recorded_commit({}, "pypi/simplex-chat", "7.1.0b6"))

    def test_highest_recorded_version_is_compared_numerically(self):
        releases = {"core": {"7.1.0.9": "a", "7.1.0.11": "b"}, "npm": {"@simplex-chat/types": {"0.9.0": "c"}}}
        self.assertEqual(highest_recorded(releases, "core"), "7.1.0.11")
        self.assertEqual(highest_recorded(releases, "npm/@simplex-chat/types"), "0.9.0")
        self.assertIsNone(highest_recorded(releases, "npm/simplex-chat"))

    def test_adds_entries_and_keeps_recorded_commits(self):
        updated = add_releases(self.RELEASES, [("npm/simplex-chat", "7.1.0-beta.6.4"), ("core", "7.1.0.11")], "x")
        self.assertEqual(updated["npm"]["simplex-chat"], {"7.1.0-beta.6.3": "n1", "7.1.0-beta.6.4": "x"})
        self.assertEqual(updated["core"], {"7.1.0.11": "c1"})
        self.assertEqual(self.RELEASES["npm"]["simplex-chat"], {"7.1.0-beta.6.3": "n1"})


class RecordReleasesTest(unittest.TestCase):
    def file(self, releases, blob):
        return {"content": base64.b64encode(render_releases(releases).encode()).decode(), "sha": blob}

    def conflict(self):
        error = urllib.error.HTTPError("url", 409, "Conflict", email.message.Message(), io.BytesIO())
        self.addCleanup(error.close)
        return error

    def test_creates_the_file_and_returns_the_commit(self):
        calls = []

        def api(path, method="GET", body=None):
            calls.append((path, method, body))
            return None if method == "GET" else {"commit": {"sha": "libs1"}}

        with mock.patch.object(packages_release, "libs_api", api):
            self.assertEqual(record_releases([("core", "7.1.0.12")], "sx1"), "libs1")
        _, method, body = calls[1]
        self.assertEqual((method, body["message"], "sha" in body), ("PUT", "core 7.1.0.12", False))
        self.assertEqual(tomllib.loads(base64.b64decode(body["content"]).decode()), {"core": {"7.1.0.12": "sx1"}})

    def test_retries_when_another_writer_changed_the_file(self):
        reads = iter([self.file({"core": {"1": "a"}}, "blob1"), self.file({"core": {"1": "a", "2": "b"}}, "blob2")])
        puts = []

        def api(path, method="GET", body=None):
            if method == "GET":
                return next(reads)
            puts.append(body)
            if len(puts) == 1:
                raise self.conflict()
            return {"commit": {"sha": "libs2"}}

        with mock.patch.object(packages_release, "libs_api", api):
            self.assertEqual(record_releases([("npm/simplex-chat", "3")], "sx3"), "libs2")
        self.assertEqual([body["sha"] for body in puts], ["blob1", "blob2"])
        self.assertEqual(
            tomllib.loads(base64.b64decode(puts[1]["content"]).decode()),
            {"core": {"1": "a", "2": "b"}, "npm": {"simplex-chat": {"3": "sx3"}}},
        )

    def test_recorded_entries_return_the_last_commit_of_the_file(self):
        def api(path, method="GET", body=None):
            self.assertEqual(method, "GET")
            return [{"sha": "libs0"}] if path.startswith("commits") else self.file({"core": {"1": "a"}}, "blob1")

        with mock.patch.object(packages_release, "libs_api", api):
            self.assertEqual(record_releases([("core", "1")], "other"), "libs0")


class GitRepoTest(unittest.TestCase):
    def setUp(self):
        self.repo = Path(tempfile.mkdtemp())
        self.git("init", "-q")

    def git(self, *args):
        subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@t", *args], cwd=self.repo, check=True)

    def write(self, file, text):
        (self.repo / file).parent.mkdir(parents=True, exist_ok=True)
        (self.repo / file).write_text(text)

    def commit(self, message):
        self.git("add", "-A")
        self.git("commit", "-q", "-m", message)

    def head(self):
        return subprocess.run(
            ["git", "rev-parse", "HEAD"], cwd=self.repo, capture_output=True, text=True
        ).stdout.strip()


class PrintBuildPlanTest(GitRepoTest):
    def setUp(self):
        super().setUp()
        self.write("packages/pkg/src.txt", "a")
        self.commit("base")
        self.base = self.head()

    def platforms(self):
        output = io.StringIO()
        with mock.patch.object(packages_release, "ROOT", self.repo), contextlib.redirect_stdout(output):
            print_build_plan(argparse.Namespace(base=self.base))
        return [entry["platform"] for entry in json.loads(output.getvalue().removeprefix("platforms="))]

    def test_package_change_is_tested_on_every_platform(self):
        self.write("packages/pkg/src.txt", "b")
        self.commit("package change")
        self.assertEqual(self.platforms(), ADDON_PLATFORMS)

    def test_change_that_runs_build_yml_is_tested_there(self):
        self.write("packages/pkg/src.txt", "b")
        self.write("src/Lib/Module.hs", "m")
        self.commit("package and core change")
        self.assertEqual(self.platforms(), [])

    def test_patterns_match_as_path_filters(self):
        self.write("packages/pkg/Dockerfile", "d")
        self.commit("nested Dockerfile")
        self.assertEqual(self.platforms(), ADDON_PLATFORMS)
        self.write("Dockerfile.build", "d")
        self.commit("top-level Dockerfile")
        self.assertEqual(self.platforms(), [])

    def test_app_change_leaves_the_add_on_to_this_workflow(self):
        self.write("packages/pkg/src.txt", "b")
        self.write("apps/ios/Shared/View.swift", "v")
        self.write("apps/multiplatform/common/Model.kt", "k")
        self.commit("package and app change")
        self.assertEqual(self.platforms(), ADDON_PLATFORMS)


class ChangesTest(GitRepoTest):
    def setUp(self):
        super().setUp()
        self.write("README.md", "r")
        self.commit("base")
        self.base = self.head()

    def builds(self, *files):
        for file in files:
            self.write(file, "changed")
        self.commit("change")
        output = io.StringIO()
        with mock.patch.object(packages_release, "ROOT", self.repo), contextlib.redirect_stdout(output):
            changes(self.base)
        self.values = dict(line.split("=", 1) for line in output.getvalue().splitlines())
        return {build for build in BUILD_CHANGES if self.values[build] == "true"}

    def test_ios_change_builds_only_ios(self):
        self.assertEqual(self.builds("apps/ios/Shared/View.swift"), {"ios"})

    def test_kotlin_change_builds_only_kotlin(self):
        self.assertEqual(self.builds("apps/multiplatform/common/Model.kt"), {"kotlin"})

    def test_app_changes_build_both_apps_without_the_core(self):
        self.assertEqual(self.builds("apps/ios/Shared/View.swift", "apps/multiplatform/common/Model.kt"), {"ios", "kotlin"})

    def test_core_change_builds_only_the_core(self):
        self.assertEqual(self.builds("src/Simplex/Chat.hs"), {"haskell"})

    def test_ios_stub_script_builds_only_ios(self):
        self.assertEqual(self.builds("scripts/ci/ios-stub-libs.sh"), {"ios"})

    def test_other_ci_script_builds_the_core(self):
        self.assertEqual(self.builds("scripts/ci/prune-old-builds.sh"), {"haskell"})

    def test_build_image_change_builds_the_core_and_kotlin(self):
        self.assertEqual(self.builds("Dockerfile.build"), {"haskell", "kotlin"})

    def test_workflow_change_builds_the_core_and_kotlin(self):
        self.assertEqual(self.builds(".github/workflows/build.yml"), {"haskell", "kotlin"})

    def test_unrelated_change_builds_nothing(self):
        self.assertEqual(self.builds("docs/guide.md", "packages/pkg/src.txt"), set())

    def test_package_change_builds_only_packages(self):
        self.assertEqual(self.builds("packages/simplex-chat-python/src/simplex_chat/client.py"), {"packages"})

    def test_badge_web_change_builds_the_core_and_tests_its_scope(self):
        self.assertEqual(self.builds("apps/simplex-badge-service/web/src/catalog.ts"), {"haskell"})
        self.assertEqual(self.values["scopes"], "badge-web")

    def test_release_script_change_tests_the_ci_scripts(self):
        self.assertEqual(self.builds("scripts/ci/packages_release.py"), {"haskell"})
        self.assertEqual(self.values["scopes"], "ci-scripts")


class PackageStateTest(GitRepoTest):
    def setUp(self):
        super().setUp()
        self.write("pkg/version.txt", 'version = "1"\n')
        self.write("pkg/src.txt", "a")
        self.commit("set version")

    def state(self, releases=None, registry_published=False):
        with mock.patch.object(packages_release, "ROOT", self.repo):
            return package_state(
                "npm/pkg", "1", "pkg", "pkg/version.txt", "^version =", releases or {}, lambda: registry_published
            )

    def changed(self):
        return self.state().changed

    def test_recorded_version_is_published_and_compared_with_its_commit(self):
        self.write("pkg/src.txt", "b")
        self.commit("change published with version 1")
        releases = {"npm": {"pkg": {"1": self.head()}}}
        self.assertEqual(self.state(releases), Package("1", changed=False, published=True))
        self.write("pkg/src.txt", "c")
        self.commit("change after the publish")
        self.assertEqual(self.state(releases), Package("1", changed=True, published=True))

    def test_unrecorded_version_asks_the_registry(self):
        self.assertFalse(self.state(registry_published=False).published)
        self.assertTrue(self.state(registry_published=True).published)

    def test_publish_plan_skips_recorded_versions_before_the_registries_list_them(self):
        self.write(f"{TYPES_DIR}/package.json", '{"name": "@simplex-chat/types", "version": "0.1.0"}')
        self.write(f"{NODEJS_DIR}/package.json", '{"name": "simplex-chat", "version": "1.0.0-beta.1.1"}')
        self.write(PYTHON_VERSION_FILE, render_python_version("1.0.0b1.post1", Pins("core-1", {})))
        self.write("pyproject.toml", "")
        self.commit("packages")
        releases = {"npm": {"simplex-chat": {"1.0.0-beta.1.1": "c"}}, "pypi": {"simplex-chat": {"1.0.0b1.post1": "c"}}}
        output = io.StringIO()
        with (
            mock.patch.object(packages_release, "ROOT", self.repo),
            mock.patch.object(packages_release, "read_releases", return_value=(releases, "blob")),
            mock.patch.object(packages_release, "python_project", return_value="simplex-chat"),
            mock.patch.object(packages_release, "npm_published", return_value=False),
            mock.patch.object(packages_release, "pypi_published", return_value=False),
            contextlib.redirect_stdout(output),
        ):
            publish_plan()
        self.assertEqual(
            output.getvalue().splitlines(),
            [
                "types_version=0.1.0",
                "types_dist_tag=latest",
                "nodejs_version=",
                "nodejs_dist_tag=beta",
                "python_version=",
            ],
        )

    def test_unknown_recorded_commit_falls_back_to_the_version_commit(self):
        self.write("pkg/src.txt", "b")
        self.commit("change")
        state = self.state({"npm": {"pkg": {"1": "0" * 40}}})
        self.assertEqual(state, Package("1", changed=True, published=True))

    def test_compares_with_the_commit_that_set_the_version(self):
        self.assertFalse(self.changed())
        self.write("pkg/src.txt", "b")
        self.commit("change")
        self.assertTrue(self.changed())

    def test_new_version_resets_the_comparison(self):
        self.write("pkg/src.txt", "b")
        self.commit("change")
        self.write("pkg/version.txt", 'version = "2"\n')
        self.commit("set version 2")
        self.assertFalse(self.changed())

    def test_changed_since_looks_only_at_the_sources(self):
        base = subprocess.run(
            ["git", "rev-parse", "HEAD"], cwd=self.repo, capture_output=True, text=True
        ).stdout.strip()
        self.write("other/file.txt", "o")
        self.commit("other change")
        with mock.patch.object(packages_release, "ROOT", self.repo):
            self.assertFalse(changed_since(base, ["pkg"]))
            self.write("pkg/src.txt", "b")
            self.commit("source change")
            self.assertTrue(changed_since(base, ["pkg"]))

    def test_ignores_tests_and_other_directories(self):
        self.write("pkg/tests/t.txt", "t")
        self.write("other/file.txt", "o")
        self.commit("tests and other files")
        self.assertFalse(self.changed())


class ReleasesTest(unittest.TestCase):
    def test_latest_libs_is_the_highest_complete_published_version(self):
        releases = [
            listed_release("core-7.1.0.9", LIBS_ASSETS, 1),
            listed_release("core-7.1.0.11", LIBS_ASSETS, 2),
            listed_release("core-7.1.0.12", LIBS_ASSETS[1:], 3),
            listed_release("core-7.1.0.13", LIBS_ASSETS, 4, draft=True),
            listed_release("core-test-7.1.0.14", LIBS_ASSETS, 5),
            listed_release("nodejs-addon-1111111", ADDON_ASSETS, 6),
        ]
        versions = {"7.1.0.9", "7.1.0.11", "7.1.0.12", "7.1.0.13", "test-7.1.0.14"}
        self.assertEqual(latest_libs(releases, versions), "core-7.1.0.11")
        self.assertEqual(latest_libs(releases, {"7.1.0.9"}), "core-7.1.0.9")
        self.assertIsNone(latest_libs(releases, {"7.0.0.12"}))

    def test_published_release_is_preferred_to_a_draft(self):
        draft = listed_release("core-7.1.0.12", LIBS_ASSETS, 1, draft=True)
        published = listed_release("core-7.1.0.12", LIBS_ASSETS, 2)
        self.assertEqual(tag_release([draft, published], "core-7.1.0.12"), published)
        self.assertEqual(tag_release([draft], "core-7.1.0.12"), draft)
        statuses = [
            release_status(tag_release(releases, "core-7.1.0.12")) for releases in ([draft, published], [draft], [])
        ]
        self.assertEqual(statuses, ["published", "draft", "absent"])

    def test_addon_release_is_found_by_its_notes(self):
        other = listed_release("nodejs-addon-1111111", ADDON_ASSETS, 1, body=addon_notes("bbb"))
        draft = listed_release("nodejs-addon-2222222", ADDON_ASSETS, 2, draft=True, body=addon_notes("aaa"))
        published = listed_release("nodejs-addon-3333333", ADDON_ASSETS, 3, body=addon_notes("aaa"))
        self.assertEqual(addon_release([other, draft, published], "aaa"), published)
        self.assertEqual(addon_release([other, draft], "aaa"), draft)
        self.assertIsNone(addon_release([other], "aaa"))

    def test_missing_assets_of_a_draft(self):
        self.assertEqual(missing_assets(listed_release("core-7.1.0.12", LIBS_ASSETS[1:], 1)), LIBS_ASSETS[:1])
        self.assertEqual(missing_assets(listed_release("nodejs-addon-1111111", ADDON_ASSETS, 1)), [])

    def test_pin_problems_list_mismatches_and_unpublished_releases(self):
        pins = Pins("core-7.1.0.11", {**digests(LIBS_ASSETS, 1), **digests(ADDON_ASSETS, 2)}, "nodejs-addon-1111111")
        published = {
            "core-7.1.0.11": github_release(LIBS_ASSETS, 1),
            "nodejs-addon-1111111": github_release(ADDON_ASSETS, 2),
        }
        self.assertEqual(pin_problems(pins, published), [])
        self.assertEqual(
            len(pin_problems(pins, {**published, "core-7.1.0.11": github_release(LIBS_ASSETS, 3)})), len(LIBS_ASSETS)
        )
        self.assertEqual(
            pin_problems(pins, {**published, "nodejs-addon-1111111": None}), ["nodejs-addon-1111111 is not published"]
        )


class PublishReleaseTest(unittest.TestCase):
    def publish(self, releases):
        api = mock.Mock()
        with (
            mock.patch.object(packages_release, "libs_releases", return_value=releases),
            mock.patch.object(packages_release, "libs_api", api),
            contextlib.redirect_stdout(io.StringIO()),
        ):
            packages_release.publish_release("core-7.1.0.12")
        return api

    def test_complete_draft_is_published(self):
        api = self.publish([listed_release("core-7.1.0.12", LIBS_ASSETS, 7, draft=True)])
        api.assert_called_once_with("releases/7", "PATCH", {"draft": False})

    def test_draft_with_missing_assets_stays_a_draft(self):
        with self.assertRaises(SystemExit):
            self.publish([listed_release("core-7.1.0.12", LIBS_ASSETS[1:], 7, draft=True)])

    def test_published_release_is_left_unchanged(self):
        self.publish([listed_release("core-7.1.0.12", LIBS_ASSETS, 7)]).assert_not_called()


class ClaimCoreTest(unittest.TestCase):
    def claim(self, recorded, after_record=None, problem=None, version="7.1.0.12", branch="master"):
        reads = iter([(recorded, "blob"), (after_record or recorded, "blob")])
        self.record = mock.Mock(return_value="libs1")
        output = io.StringIO()
        with (
            mock.patch.object(packages_release, "read_releases", lambda: next(reads)),
            mock.patch.object(packages_release, "record_releases", self.record),
            mock.patch.object(packages_release, "types_problem", return_value=problem),
            contextlib.redirect_stdout(output),
        ):
            claim_core(version, "sx1", branch)
        return output.getvalue().splitlines()

    def test_lower_version_fails(self):
        with self.assertRaises(SystemExit):
            self.claim({"core": {"master": {"7.1.0.13": "sx0"}}})

    def test_version_recorded_for_this_commit_resumes(self):
        self.assertEqual(self.claim({"core": {"master": {"7.1.0.12": "sx1"}}}), ["claim=resume"])

    def test_version_recorded_for_another_commit_is_skipped(self):
        self.assertEqual(self.claim({"core": {"master": {"7.1.0.11": "sx0", "7.1.0.12": "sx0"}}}), ["claim=skip"])

    def test_version_recorded_on_another_branch_is_skipped(self):
        recorded = {"core": {"master": {"7.1.0.11": "sx0"}, "stable": {"7.1.0.12": "sx2"}}}
        self.assertEqual(self.claim(recorded), ["claim=skip"])

    def test_failed_types_check_fails_before_recording(self):
        with self.assertRaises(SystemExit):
            self.claim({"core": {"master": {"7.1.0.11": "sx0"}}}, problem="types changed")
        self.record.assert_not_called()

    def test_unrecorded_version_is_claimed(self):
        claimed = {"core": {"master": {"7.1.0.11": "sx0", "7.1.0.12": "sx1"}}}
        self.assertEqual(self.claim({"core": {"master": {"7.1.0.11": "sx0"}}}, claimed), ["claim=new", "commit=libs1"])
        self.record.assert_called_once_with([("core/master", "7.1.0.12")], "sx1")

    def test_stable_versions_are_compared_with_stable_only(self):
        recorded = {"core": {"master": {"7.1.0.11": "sx0"}, "stable": {"7.0.0.12": "sx3"}}}
        claimed = {"core": {**recorded["core"], "stable": {"7.0.0.12": "sx3", "7.0.0.13": "sx1"}}}
        self.assertEqual(
            self.claim(recorded, claimed, version="7.0.0.13", branch="stable"), ["claim=new", "commit=libs1"]
        )
        self.record.assert_called_once_with([("core/stable", "7.0.0.13")], "sx1")
        with self.assertRaises(SystemExit):
            self.claim(recorded, version="7.0.0.11", branch="stable")

    def test_concurrent_claim_of_another_commit_is_skipped(self):
        claimed = {"core": {"master": {"7.1.0.11": "sx0", "7.1.0.12": "sx2"}}}
        self.assertEqual(self.claim({"core": {"master": {"7.1.0.11": "sx0"}}}, claimed), ["claim=skip"])


class TypesProblemTest(unittest.TestCase):
    def problem(self, types, changed=True, published=False, releases=None):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        root = Path(directory.name)
        (root / TYPES_DIR).mkdir(parents=True)
        (root / TYPES_DIR / "package.json").write_text(json.dumps({"name": "@simplex-chat/types", **types}))
        with (
            mock.patch.object(packages_release, "ROOT", root),
            mock.patch.object(packages_release, "types_changed", return_value=changed),
            mock.patch.object(packages_release, "npm_published", return_value=published),
        ):
            return types_problem("7.1.0.12", releases or {}, "master")

    def test_unchanged_types_pass(self):
        self.assertIsNone(self.problem({"version": "0.12.1", "simplexChatCore": "7.1.0.11"}, changed=False))

    def test_changed_types_with_a_new_version_for_the_core_pass(self):
        self.assertIsNone(self.problem({"version": "0.12.2", "simplexChatCore": "7.1.0.12"}))

    def test_changed_types_with_a_published_version_fail(self):
        self.assertIsNotNone(self.problem({"version": "0.12.1", "simplexChatCore": "7.1.0.12"}, published=True))
        recorded = {"npm": {"@simplex-chat/types": {"0.12.1": "c"}}}
        self.assertIsNotNone(self.problem({"version": "0.12.1", "simplexChatCore": "7.1.0.12"}, releases=recorded))

    def test_changed_types_for_another_core_fail(self):
        self.assertIsNotNone(self.problem({"version": "0.12.2", "simplexChatCore": "7.1.0.11"}))


class CoreReleaseTest(GitRepoTest):
    def setUp(self):
        super().setUp()
        self.write(CABAL_FILE, "name:           simplex-chat\nversion:        7.1.0.11\n")
        types = {"name": "@simplex-chat/types", "version": "0.12.1", "simplexChatCore": "7.1.0.11"}
        self.write(f"{TYPES_DIR}/package.json", json.dumps(types, indent=2) + "\n")
        self.write(TYPES_README, "# Types\n\nSimpleX Chat core: 7.1.0.11\n")
        self.write(f"{TYPES_DIR}/src/types.ts", "a")
        self.commit("types 0.12.1")
        self.releases = {"core": {"master": {"7.1.0.11": self.head()}}}

    def release(self, version, types_version=None):
        with (
            mock.patch.object(packages_release, "ROOT", self.repo),
            mock.patch.object(packages_release, "read_releases", return_value=(self.releases, "blob")),
            contextlib.redirect_stdout(io.StringIO()),
        ):
            core_release(version, "master", types_version)

    def read(self, file):
        return (self.repo / file).read_text()

    def types(self):
        return json.loads(self.read(f"{TYPES_DIR}/package.json"))

    def test_changed_types_get_a_new_version_for_the_core(self):
        self.write(f"{TYPES_DIR}/src/types.ts", "b")
        self.commit("types change")
        self.release("7.1.0.12")
        self.assertIn("version:        7.1.0.12\n", self.read(CABAL_FILE))
        self.assertEqual((self.types()["version"], self.types()["simplexChatCore"]), ("0.12.2", "7.1.0.12"))
        self.assertIn("SimpleX Chat core: 7.1.0.12\n", self.read(TYPES_README))

    def test_unchanged_types_keep_their_version(self):
        self.release("7.1.0.12")
        self.assertIn("version:        7.1.0.12\n", self.read(CABAL_FILE))
        self.assertEqual((self.types()["version"], self.types()["simplexChatCore"]), ("0.12.1", "7.1.0.11"))

    def test_requested_types_version_wins(self):
        self.release("7.1.0.12", "7.1.0-beta.6.1")
        self.assertEqual((self.types()["version"], self.types()["simplexChatCore"]), ("7.1.0-beta.6.1", "7.1.0.12"))

    def test_types_version_is_bumped_from_the_previous_core_release(self):
        self.write(f"{TYPES_DIR}/package.json", json.dumps({**self.types(), "version": "0.12.5"}))
        self.write(f"{TYPES_DIR}/src/types.ts", "b")
        self.commit("types change and a manual version")
        self.release("7.1.0.12")
        self.assertEqual(self.types()["version"], "0.12.2")

    def test_version_must_be_higher_than_the_recorded_one(self):
        with self.assertRaises(SystemExit):
            self.release("7.1.0.11")
        self.assertIn("version:        7.1.0.11\n", self.read(CABAL_FILE))


class PrepareTest(GitRepoTest):
    def setUp(self):
        super().setUp()
        nodejs = {
            "name": "simplex-chat",
            "version": "7.1.0-beta.6.1",
            "dependencies": {"@simplex-chat/types": "^0.12.1"},
            "devDependencies": {"node-addon-api": "^8.5.0"},
        }
        self.write(CABAL_FILE, "version:        1.0.0.2\n")
        self.write(f"{NODEJS_DIR}/binding.gyp", "{}")
        self.write(f"{NODEJS_DIR}/cpp/simplex.cc", "add-on")
        self.write(f"{NODEJS_DIR}/package.json", json.dumps(nodejs))
        self.write(RELEASE_FILE, render_release(state().nodejs_pins))
        self.write(f"{TYPES_DIR}/package.json", json.dumps({"name": "@simplex-chat/types", "version": "0.12.1"}))
        self.write(PYTHON_VERSION_FILE, render_python_version("7.1.0b6", state().python_pins))
        self.write(f"{PYTHON_DIR}/src/simplex_chat/types/_types.py", "a")
        self.write(f"{PYTHON_DIR}/pyproject.toml", 'name = "simplex-chat"\n')
        self.commit("packages")
        with mock.patch.object(packages_release, "ROOT", self.repo):
            sources_hash = packages_release.current_addon_hash()
        self.libs = [
            listed_release("core-1.0.0.2", LIBS_ASSETS, 3),
            listed_release("nodejs-addon-1111111", ADDON_ASSETS, 4, body=addon_notes(sources_hash)),
        ]
        self.releases = {
            "core": {"master": {"1.0.0.2": self.head()}},
            "npm": {"simplex-chat": {"7.1.0-beta.6.1": self.head()}},
            "pypi": {"simplex-chat": {"7.1.0b6": self.head()}},
        }

    def prepare(self, *packages):
        published = {release["tag_name"]: release for release in self.libs}
        download = mock.Mock()
        with (
            mock.patch.object(packages_release, "ROOT", self.repo),
            mock.patch.object(packages_release, "read_releases", return_value=(self.releases, "blob")),
            mock.patch.object(packages_release, "libs_releases", return_value=self.libs),
            mock.patch.object(packages_release, "github_release", published.get),
            mock.patch.object(packages_release, "npm_published", return_value=True),
            mock.patch.object(packages_release, "pypi_published", return_value=True),
            mock.patch.object(packages_release, "download", download),
            contextlib.redirect_stdout(io.StringIO()),
            contextlib.redirect_stderr(io.StringIO()),
        ):
            prepare(
                argparse.Namespace(packages=list(packages), branch="master", nodejs_version=None, python_version=None)
            )
        return download

    def test_listed_package_is_released_alone(self):
        download = self.prepare("nodejs")
        pins = parse_release((self.repo / RELEASE_FILE).read_text())
        self.assertEqual((pins.libs_release, pins.addon_release), ("core-1.0.0.2", "nodejs-addon-1111111"))
        self.assertEqual(json.loads((self.repo / NODEJS_DIR / "package.json").read_text())["version"], "7.1.0-beta.6.2")
        python = parse_python_version((self.repo / PYTHON_VERSION_FILE).read_text())
        self.assertEqual(python, ("7.1.0b6", state().python_pins))
        self.assertEqual(download.call_count, len(LIBS_ASSETS) + len(ADDON_ASSETS))

    def test_python_types_newer_than_the_pinned_core_stop_the_release(self):
        self.write(f"{PYTHON_DIR}/src/simplex_chat/types/_types.py", "b")
        self.commit("python types change")
        with self.assertRaises(SystemExit):
            self.prepare("python")
        self.assertEqual(parse_python_version((self.repo / PYTHON_VERSION_FILE).read_text())[0], "7.1.0b6")


class DownloadTest(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.directory = directory.name

    def download(self, sha256):
        with mock.patch.object(packages_release.urllib.request, "urlopen", return_value=io.BytesIO(b"data")):
            return download("core-1.0.0.1", "a.zip", self.directory, sha256)

    def test_matching_digest_keeps_the_file(self):
        self.assertEqual(self.download(hashlib.sha256(b"data").hexdigest()).read_bytes(), b"data")

    def test_mismatching_digest_removes_the_file(self):
        with self.assertRaises(SystemExit):
            self.download(sha(1))
        self.assertFalse((Path(self.directory) / "a.zip").exists())


if __name__ == "__main__":
    unittest.main()
