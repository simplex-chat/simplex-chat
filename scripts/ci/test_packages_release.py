import argparse
import base64
import contextlib
import email.message
import hashlib
import io
import json
import os
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

    def test_app_test_scripts_build_nothing(self):
        self.assertEqual(self.builds("scripts/ci/ios-sim/smoke-test.sh", "scripts/ci/desktop-linux/smoke-test.sh", "scripts/ci/android/ui", "scripts/ci/snapshots-compare.py", "scripts/ci/README.md", "scripts/ci/app-tests/feature-tests.sh", "scripts/ci/desktop-linux/tests/private-notes.sh"), set())

    def test_build_image_change_builds_the_core_and_kotlin(self):
        self.assertEqual(self.builds("Dockerfile.build"), {"haskell", "kotlin"})

    def test_workflow_change_builds_the_core_and_kotlin(self):
        self.assertEqual(self.builds(".github/workflows/build.yml"), {"haskell", "kotlin"})

    def test_build_action_change_builds_the_core_and_kotlin(self):
        self.assertEqual(self.builds(".github/actions/prepare-build/action.yml"), {"haskell", "kotlin"})

    def test_other_workflow_change_builds_nothing(self):
        self.assertEqual(self.builds(".github/workflows/ios-sim.yml", ".github/workflows/desktop-linux.yml", ".github/ISSUE_TEMPLATE/bug.md"), set())

    def test_unrelated_change_builds_nothing(self):
        self.assertEqual(self.builds("docs/guide.md", "packages/pkg/src.txt"), set())

    def test_package_change_builds_only_packages(self):
        self.assertEqual(self.builds("packages/simplex-chat-python/src/simplex_chat/client.py"), {"packages"})

    def test_badge_web_change_builds_the_core_and_tests_its_scope(self):
        self.assertEqual(self.builds("apps/simplex-store-service/web/src/catalog.ts"), {"haskell"})
        self.assertEqual(self.values["scopes"], "badge-web")

    def test_release_script_change_tests_the_ci_scripts(self):
        self.assertEqual(self.builds("scripts/ci/packages_release.py"), set())
        self.assertEqual(self.values["scopes"], "ci-scripts")

    def test_feature_test_selection_change_tests_the_ci_scripts(self):
        self.assertEqual(self.builds("scripts/ci/app-tests/select-tests.sh"), set())
        self.assertEqual(self.values["scopes"], "ci-scripts")


class SelectTestsTest(GitRepoTest):
    SHARED_FILES = [
        "scripts/ci/app-tests/checks.sh",
        "scripts/ci/desktop-linux/lib.sh",
        "scripts/ci/desktop-linux/ui",
        "scripts/ci/desktop-linux/screen-text.py",
        "scripts/ci/desktop-linux/backend.sh",
    ]

    def setUp(self):
        super().setUp()
        for file in [
            *self.SHARED_FILES,
            "scripts/ci/android/lib.sh",
            "apps/mp/views/chat/Compose.kt",
            "apps/mp/Other.kt",
            "apps/ios/View.swift",
        ]:
            self.write(file, "x")
        self.write("scripts/ci/desktop-linux/tests/alpha.sh", "# covers: apps/mp/views/chat/*\n")
        self.write(
            "scripts/ci/desktop-linux/tests/beta.sh",
            "# covers: apps/mp/views/chatlist/* apps/ios/*.swift\n# covers: apps/ios/SimpleX?NSE/*\n",
        )
        self.write("scripts/ci/android/tests/alpha.sh", "# covers: apps/mp/views/chat/*\n")
        self.commit("base")
        self.base = self.head()

    def change_from_base(self, *files):
        self.git("switch", "-q", "--detach", self.base)
        for file in files:
            self.write(file, "changed")
        self.commit("change")

    def select(self, tests_input=""):
        script = packages_release.ROOT / "scripts/ci/app-tests/select-tests.sh"
        base = "" if tests_input else self.base
        return subprocess.run(
            [script, "scripts/ci/desktop-linux", base, tests_input], cwd=self.repo, capture_output=True, text=True
        )

    def selected(self, tests_input=""):
        result = self.select(tests_input)
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout.split()

    def test_input_selects_names_all_or_none(self):
        self.assertEqual(self.selected(), [])
        self.assertEqual(self.selected("none"), [])
        self.assertEqual(self.selected("all"), ["alpha", "beta"])
        self.assertEqual(self.selected("beta"), ["beta"])
        self.assertEqual(self.selected("alpha beta"), ["alpha", "beta"])
        self.assertEqual(self.selected("beta alpha beta"), ["alpha", "beta"])

    def test_other_words_in_the_input_fail(self):
        self.assertEqual(self.select("../alpha").returncode, 2)
        self.assertEqual(self.select("-x").returncode, 2)
        self.assertEqual(self.select("Alpha").returncode, 2)
        self.assertEqual(self.select("all beta").returncode, 2)
        self.assertEqual(self.select("none beta").returncode, 2)

    def test_name_no_platform_has_fails(self):
        self.assertEqual(self.select("gamma").returncode, 2)
        self.assertEqual(self.select("alpha gamma").returncode, 2)

    def test_name_only_another_platform_has_selects_nothing(self):
        self.write("scripts/ci/android/tests/delta.sh", "")
        self.assertEqual(self.selected("delta"), [])

    def test_none_overrides_changes(self):
        self.change_from_base("apps/mp/views/chat/Compose.kt")
        self.assertEqual(self.selected("none"), [])

    def test_added_or_changed_test_is_selected(self):
        self.change_from_base("scripts/ci/desktop-linux/tests/gamma.sh")
        self.assertEqual(self.selected(), ["gamma"])
        self.change_from_base("scripts/ci/desktop-linux/tests/beta.sh")
        self.assertEqual(self.selected(), ["beta"])

    def test_deleted_or_moved_file_selects_the_tests_covering_it(self):
        self.git("rm", "-q", "apps/mp/views/chat/Compose.kt")
        self.commit("delete covered file")
        self.assertEqual(self.selected(), ["alpha"])
        self.git("switch", "-q", "--detach", self.base)
        self.git("mv", "apps/mp/views/chat/Compose.kt", "apps/mp/Moved.kt")
        self.commit("move covered file")
        self.assertEqual(self.selected(), ["alpha"])

    def test_deleted_shared_file_selects_every_remaining_test(self):
        self.git("rm", "-q", "scripts/ci/desktop-linux/lib.sh", "scripts/ci/desktop-linux/tests/beta.sh")
        self.commit("delete shared file and test")
        self.assertEqual(self.selected(), ["alpha"])

    def test_change_selects_the_tests_covering_it(self):
        self.change_from_base("apps/mp/views/chat/Compose.kt")
        self.assertEqual(self.selected(), ["alpha"])
        self.change_from_base("apps/mp/Other.kt")
        self.assertEqual(self.selected(), [])

    def test_covers_line_lists_patterns_separated_by_spaces(self):
        self.change_from_base("apps/ios/View.swift")
        self.assertEqual(self.selected(), ["beta"])

    def test_star_matches_a_slash_and_question_mark_a_space(self):
        self.change_from_base("apps/mp/views/chat/item/Item.kt")
        self.assertEqual(self.selected(), ["alpha"])
        self.change_from_base("apps/ios/SimpleX NSE/Info.plist")
        self.assertEqual(self.selected(), ["beta"])

    def test_change_to_a_shared_file_selects_every_test(self):
        for file in self.SHARED_FILES:
            self.change_from_base(file)
            self.assertEqual(self.selected(), ["alpha", "beta"], file)

    def test_docs_claude_files_and_other_platforms_select_nothing(self):
        self.change_from_base(
            "scripts/ci/desktop-linux/README.md",
            "scripts/ci/desktop-linux/claude-test.md",
            "scripts/ci/desktop-linux/claude-scope",
            "scripts/ci/android/lib.sh",
            "scripts/ci/android/tests/alpha.sh",
        )
        self.assertEqual(self.selected(), [])

    def test_test_file_whose_name_cannot_be_selected_fails(self):
        self.change_from_base("scripts/ci/desktop-linux/tests/Bad_Name.sh")
        self.assertEqual(self.select().returncode, 2)

    def test_only_changes_between_the_base_and_head_count(self):
        self.change_from_base("apps/mp/views/chat/Compose.kt")
        self.write("apps/mp/Other.kt", "changed")
        self.commit("second change")
        branch = self.head()
        self.change_from_base("apps/ios/View.swift")
        self.git("switch", "-q", "--detach", branch)
        self.assertEqual(self.selected(), ["alpha"])


class FeatureTestsTest(unittest.TestCase):
    TESTS = {
        "passes": 'check "one" true\ncheck "two" true\n',
        "fails-a-check": 'check "good" true\ncheck "bad" false\ncheck "after" true\n',
        "stops": 'check "before" true\nfail "cannot go on"\ncheck "never" true\n',
        "needs-backend": '# needs: backend\ncheck "backend at $BACKEND_DIR" true\n',
        "no-checks": "",
    }

    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        work = Path(directory.name)
        self.platform = work / "platform"
        self.out = work / "out"
        self.summary = work / "summary"
        self.ui_log = work / "ui.log"
        self.backend = work / "backend"
        (self.platform / "tests").mkdir(parents=True)
        ui = self.platform / "ui"
        ui.write_text('#!/bin/bash\necho "$* $UI_OUT" >> "$UI_LOG"\n[ "$1" != layout ] || [ -z "$UI_LAYOUT_FAILS" ]\n')
        ui.chmod(0o755)
        checks = packages_release.ROOT / "scripts/ci/app-tests/checks.sh"
        (self.platform / "lib.sh").write_text(f'ui="{ui}"\n. "{checks}"\n')
        for name, steps in self.TESTS.items():
            (self.platform / "tests" / f"{name}.sh").write_text(
                f'#!/bin/bash\nset -euo pipefail\n. "$(dirname "$0")/../lib.sh"\n{steps}'
            )

    def run_tests(self, *names, backend_dir="", layout_fails=False):
        script = packages_release.ROOT / "scripts/ci/app-tests/feature-tests.sh"
        env = {
            "GITHUB_STEP_SUMMARY": str(self.summary),
            "UI_LOG": str(self.ui_log),
            "BACKEND_DIR": backend_dir,
            "UI_LAYOUT_FAILS": "1" if layout_fails else "",
        }
        with mock.patch.dict("os.environ", env):
            return subprocess.run([script, self.platform, self.out, *names], capture_output=True, text=True)

    def run_every_kind(self):
        return self.run_tests("passes", "fails-a-check", "stops", "needs-backend")

    def checks(self, name):
        return (self.out / name / "checks.txt").read_text().splitlines()

    def test_failed_test_fails_the_run(self):
        self.assertEqual(self.run_every_kind().returncode, 1)

    def test_test_continues_after_a_failed_check(self):
        self.run_tests("fails-a-check")
        self.assertEqual(self.checks("fails-a-check"), ["PASS good", "FAIL bad", "PASS after"])

    def test_fail_ends_the_test_and_records_the_reason(self):
        self.run_tests("stops")
        self.assertEqual(
            self.checks("stops"),
            ["PASS before", "NOTE stopped: cannot go on", "NOTE the test exited with status 1"],
        )

    def test_test_needing_the_backend_is_skipped_without_it(self):
        self.assertEqual(self.run_tests("needs-backend").returncode, 0)
        self.assertEqual(
            self.checks("needs-backend"), ["NOTE skipped: requires the test backend, absent from this run"]
        )

    def test_test_needing_the_backend_runs_with_it(self):
        self.assertEqual(self.run_tests("passes", "needs-backend", backend_dir=str(self.backend)).returncode, 0)
        self.assertEqual(self.checks("needs-backend"), [f"PASS backend at {self.backend}"])

    def test_summary_has_each_tests_result_and_checks(self):
        self.run_every_kind()
        self.assertEqual(
            self.summary.read_text().splitlines(),
            [
                "### Feature test passes: passed",
                "",
                "- PASS one",
                "- PASS two",
                "### Feature test fails-a-check: failed",
                "",
                "- PASS good",
                "- FAIL bad",
                "- PASS after",
                "### Feature test stops: failed",
                "",
                "- PASS before",
                "- NOTE stopped: cannot go on",
                "- NOTE the test exited with status 1",
                "### Feature test needs-backend: skipped",
                "",
                "- NOTE skipped: requires the test backend, absent from this run",
            ],
        )

    def test_app_restarts_before_each_test_and_a_failed_test_has_a_screenshot_in_its_folder(self):
        self.run_every_kind()
        self.assertEqual(
            self.ui_log.read_text().splitlines(),
            [
                f"layout reachable {self.out}/passes",
                f"layout reachable {self.out}/fails-a-check",
                f"shot final {self.out}/fails-a-check",
                f"layout reachable {self.out}/stops",
                f"shot final {self.out}/stops",
            ],
        )

    def test_name_that_is_not_a_test_is_rejected_before_any_test_runs(self):
        for name in ["../tests/passes", "nonexistent"]:
            self.assertEqual(self.run_tests("passes", name).returncode, 2, name)
        self.assertFalse(self.ui_log.exists())

    def test_failed_restart_fails_the_test(self):
        self.assertEqual(self.run_tests("passes", layout_fails=True).returncode, 1)
        self.assertEqual(self.checks("passes"), ["FAIL the app restarted at the chat list in the reachable layout"])
        self.assertEqual(self.summary.read_text().splitlines()[0], "### Feature test passes: failed")

    def test_test_without_checks_passes_with_a_note_in_the_summary(self):
        self.assertEqual(self.run_tests("no-checks").returncode, 0)
        self.assertEqual(
            self.summary.read_text().splitlines(),
            ["### Feature test no-checks: passed", "", "- NOTE no checks were recorded"],
        )


class ChecksTest(unittest.TestCase):
    def run_checks(self, *steps):
        script = packages_release.ROOT / "scripts/ci/app-tests/checks.sh"
        with tempfile.TemporaryDirectory() as out:
            with mock.patch.dict("os.environ", {"UI_OUT": out}):
                return subprocess.run(
                    ["bash", "-c", "\n".join([f'. "{script}"', *steps])], capture_output=True, text=True
                )

    def test_failed_check_fails_the_final_gate(self):
        result = self.run_checks('check "good" true', 'check "bad" false', 'fail_on_failed_checks "the smoke test"')
        self.assertEqual(result.returncode, 1)
        self.assertIn("error: the smoke test failed checks: 1 of 2", result.stdout)

    def test_passed_checks_and_notes_pass_the_final_gate(self):
        result = self.run_checks('check "good" true', 'note "remark"', 'fail_on_failed_checks "the smoke test"')
        self.assertEqual(result.returncode, 0)


class ChooseFeatureTestsTest(GitRepoTest):
    def setUp(self):
        super().setUp()
        self.write("scripts/ci/desktop-linux/lib.sh", "x")
        self.write("scripts/ci/desktop-linux/tests/alpha.sh", "# covers: apps/mp/*\n")
        self.write("scripts/ci/desktop-linux/tests/beta.sh", "# covers: apps/ios/*\n")
        self.write("apps/mp/Compose.kt", "x")
        self.commit("base")
        self.base = self.head()
        self.write("apps/mp/Compose.kt", "changed")
        self.commit("change")
        self.work = self.repo / ".git" / "choose-test"
        self.work.mkdir()
        origin = self.work / "origin.git"
        self.git("clone", "-q", "--bare", ".", str(origin))
        self.git("-C", str(origin), "config", "uploadpack.allowAnySHA1InWant", "true")
        self.git("remote", "add", "origin", f"file://{origin}")
        self.gh_log = self.work / "gh.log"
        self.output = self.work / "output"
        self.summary = self.work / "summary"
        gh = self.work / "bin" / "gh"
        gh.parent.mkdir()
        gh.write_text(
            "#!/bin/bash\n"
            'echo "$*" >> "$GH_LOG"\n'
            'case "$2" in\n'
            '  */compare/*) echo "$BASE_COMMIT" ;;\n'
            "  *) echo root-owner ;;\n"
            "esac\n"
        )
        gh.chmod(0o755)

    def choose(self, tests_input):
        script = packages_release.ROOT / "scripts/ci/app-tests/choose-feature-tests.sh"
        env = {
            "PATH": f"{self.work / 'bin'}:{os.environ['PATH']}",
            "TESTS": tests_input,
            "GH_LOG": str(self.gh_log),
            "BASE_COMMIT": self.base,
            "GITHUB_REPOSITORY": "fork-owner/simplex-chat",
            "GITHUB_SHA": self.head(),
            "GITHUB_OUTPUT": str(self.output),
            "GITHUB_STEP_SUMMARY": str(self.summary),
        }
        with mock.patch.dict("os.environ", env):
            return subprocess.run(
                [script, "scripts/ci/desktop-linux"], cwd=self.repo, capture_output=True, text=True
            )

    def test_blank_input_selects_from_the_changes_since_the_root_repository_master(self):
        result = self.choose("  ")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.output.read_text(), "tests=alpha\n")
        self.assertEqual(self.summary.read_text(), "- Feature tests: alpha\n")
        self.assertEqual(
            self.gh_log.read_text().splitlines(),
            [
                "api repos/fork-owner/simplex-chat --jq .source.owner.login // .owner.login",
                f"api repos/fork-owner/simplex-chat/compare/root-owner:master...{self.head()} --jq .merge_base_commit.sha",
            ],
        )

    def test_named_input_selects_without_github_calls(self):
        result = self.choose("beta")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.output.read_text(), "tests=beta\n")
        self.assertFalse(self.gh_log.exists())

    def test_none_gives_an_empty_output(self):
        self.assertEqual(self.choose("none").returncode, 0)
        self.assertEqual(self.output.read_text(), "tests=\n")
        self.assertEqual(self.summary.read_text(), "- Feature tests: none\n")

    def test_name_no_platform_has_fails_the_step(self):
        self.assertNotEqual(self.choose("gamma").returncode, 0)
        self.assertFalse(self.output.exists())


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
