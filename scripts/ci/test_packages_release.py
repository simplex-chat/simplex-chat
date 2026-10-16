import base64
import contextlib
import email.message
import io
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
    NODEJS_DIR,
    PYTHON_VERSION_FILE,
    TYPES_DIR,
    ADDON_PLATFORMS,
    BuildPlan,
    LIBS_ASSETS,
    Package,
    Pins,
    State,
    addon_tag,
    build_matrix,
    build_plan,
    bump_npm_version,
    bump_python_version,
    changed_since,
    add_releases,
    package_state,
    publish_plan,
    record_releases,
    recorded_commit,
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
)


def sha(n: int) -> str:
    return f"{n:x}" * 64


def digests(assets: list[str], n: int) -> dict[str, str]:
    return {asset: sha(n) for asset in assets}


def github_release(assets: list[str], n: int) -> dict:
    return {"assets": [{"name": name, "digest": f"sha256:{sha(n)}"} for name in assets]}


def state(**overrides) -> State:
    current = State(
        libs_tag="core-1.0.0.1",
        libs_digests=digests(LIBS_ASSETS, 1),
        addon_tag="nodejs-addon-aaa",
        addon_digests=digests(ADDON_ASSETS, 2),
        nodejs_pins=Pins("core-1.0.0.1", {**digests(LIBS_ASSETS, 1), **digests(ADDON_ASSETS, 2)}, "nodejs-addon-aaa"),
        python_pins=Pins("core-1.0.0.1", digests(LIBS_ASSETS, 1)),
        types=Package("0.12.1", changed=False, published=True),
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


class AddonTagTest(unittest.TestCase):
    def test_depends_on_sources_and_node_addon_api(self):
        files = {"a.cc": b"1", "b.h": b"2"}
        tag = addon_tag(files, "^8.5.0")
        self.assertRegex(tag, r"^nodejs-addon-[0-9a-f]{12}$")
        self.assertEqual(addon_tag(dict(reversed(files.items())), "^8.5.0"), tag)
        self.assertNotEqual(addon_tag({**files, "b.h": b"3"}, "^8.5.0"), tag)
        self.assertNotEqual(addon_tag({"a.cc": b"1", "c.h": b"2"}, "^8.5.0"), tag)
        self.assertNotEqual(addon_tag(files, "^8.6.0"), tag)


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
        self.assertEqual((result.nodejs_changed, result.python_changed, result.types_bumped), (False, False, False))
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

    def test_types_bump_updates_the_nodejs_dependency(self):
        result = plan(state(types=Package("0.12.1", True, True)))
        self.assertEqual(result.types_dependency, "^0.12.2")
        self.assertEqual(result.title, "packages: release types 0.12.2, nodejs 7.1.0-beta.6.2")

    def test_all_changes_combine_into_one_release(self):
        result = plan(
            state(
                **NEW_LIBS,
                **NEW_ADDON,
                types=Package("0.12.1", True, True),
                nodejs=Package("7.1.0-beta.6.1", True, True),
                python=Package("7.1.0b6", True, True),
            )
        )
        self.assertEqual(
            (result.nodejs_pins.libs_release, result.nodejs_pins.addon_release), ("core-1.0.0.2", "nodejs-addon-bbb")
        )
        self.assertEqual(result.title, "packages: release types 0.12.2, nodejs 7.1.0-beta.6.2, python 7.1.0b6.post1")

    def test_unpublished_versions_are_not_bumped_again(self):
        result = plan(
            state(
                **NEW_LIBS, nodejs=Package("7.1.0-beta.6.2", True, False), python=Package("7.1.0b6.post1", False, False)
            )
        )
        self.assertEqual(result.title, "packages: release nodejs 7.1.0-beta.6.2, python 7.1.0b6.post1")

    def test_requested_versions_win(self):
        result = plan(state(nodejs_version="7.2.0", types_version="0.13.0", python_version="7.2.0"))
        self.assertEqual(result.types_dependency, "^0.13.0")
        self.assertEqual(result.title, "packages: release types 0.13.0, nodejs 7.2.0, python 7.2.0")


class BuildPlanTest(unittest.TestCase):
    def plan(self, event="push", release_missing=False, libs_artifact=False, libs_release="", core=False, addon=False):
        return build_plan(event, release_missing, libs_artifact, libs_release, core, addon)

    def test_push_builds_a_missing_addon_release_and_tests_it(self):
        self.assertEqual(self.plan(release_missing=True), BuildPlan(ADDON_PLATFORMS, test=True))
        self.assertEqual(self.plan(), BuildPlan([], test=True))

    def test_pull_request_tests_with_the_pinned_libs(self):
        self.assertEqual(self.plan("pull_request"), BuildPlan(ADDON_PLATFORMS, test=True))
        self.assertEqual(self.plan("pull_request", addon=True), BuildPlan(ADDON_PLATFORMS, test=True))

    def test_core_change_leaves_tests_to_build_yml(self):
        self.assertEqual(self.plan("pull_request", core=True), BuildPlan([], test=False))
        self.assertEqual(self.plan("pull_request", core=True, addon=True), BuildPlan(ADDON_PLATFORMS, test=False))

    def test_libsimplex_of_the_pull_request_is_tested_on_its_platform(self):
        self.assertEqual(
            self.plan("pull_request", libs_artifact=True, core=True), BuildPlan(["linux-x86_64"], test=True)
        )

    def test_new_library_release_is_tested_on_every_platform(self):
        self.assertEqual(self.plan(libs_release="core-1.0.0.2"), BuildPlan(ADDON_PLATFORMS, test=True))

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


class PackageStateTest(unittest.TestCase):
    def setUp(self):
        self.repo = Path(tempfile.mkdtemp())
        self.git("init", "-q")
        self.write("pkg/version.txt", 'version = "1"\n')
        self.write("pkg/src.txt", "a")
        self.commit("set version")

    def git(self, *args):
        subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@t", *args], cwd=self.repo, check=True)

    def write(self, file, text):
        (self.repo / file).parent.mkdir(parents=True, exist_ok=True)
        (self.repo / file).write_text(text)

    def commit(self, message):
        self.git("add", "-A")
        self.git("commit", "-q", "-m", message)

    def state(self, releases=None, registry_published=False):
        with mock.patch.object(packages_release, "ROOT", self.repo):
            return package_state(
                "npm/pkg", "1", "pkg", "pkg/version.txt", "^version =", releases or {}, lambda: registry_published
            )

    def changed(self):
        return self.state().changed

    def head(self):
        return subprocess.run(
            ["git", "rev-parse", "HEAD"], cwd=self.repo, capture_output=True, text=True
        ).stdout.strip()

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


if __name__ == "__main__":
    unittest.main()
