import subprocess
import tempfile
import unittest
from dataclasses import replace
from pathlib import Path
from unittest import mock

import packages_release
from packages_release import (
    ADDON_ASSETS,
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
    changed_since_publish,
    published_tag,
    dist_tag,
    parse_python_version,
    parse_release,
    plan,
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


class ChangedSincePublishTest(unittest.TestCase):
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

    def changed(self):
        with mock.patch.object(packages_release, "ROOT", self.repo):
            return changed_since_publish("pkg", "1", "pkg", "pkg/version.txt", "^version =")

    def test_compares_with_the_commit_that_set_the_version(self):
        self.assertFalse(self.changed())
        self.write("pkg/src.txt", "b")
        self.commit("change")
        self.assertTrue(self.changed())

    def test_compares_with_the_published_commit(self):
        self.write("pkg/src.txt", "b")
        self.commit("change")
        self.git("tag", "pkg-v1")
        self.assertFalse(self.changed())
        self.write("pkg/src.txt", "c")
        self.commit("change after publish")
        self.assertTrue(self.changed())

    def test_publish_tag_marks_the_version_published(self):
        with mock.patch.object(packages_release, "ROOT", self.repo):
            self.assertIsNone(published_tag("pkg", "1"))
            self.git("tag", "pkg-v1")
            self.assertEqual(published_tag("pkg", "1"), "refs/tags/pkg-v1")
            self.assertIsNone(published_tag("pkg", "2"))

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
