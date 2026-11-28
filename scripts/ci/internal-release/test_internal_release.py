import base64
import hashlib
import io
import json
import os
import plistlib
import re
import subprocess
import tarfile
import tempfile
import time
import unittest
import zipfile
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SCRIPTS = ROOT / "scripts/ci/internal-release"
APP_UPDATER_TEST = ROOT / "apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/AppUpdaterTest.kt"
PROPERTIES = "apps/multiplatform/gradle.properties"
PBXPROJ = "apps/ios/SimpleX.xcodeproj/project.pbxproj"
LIBS = "apps/multiplatform/common/src/commonMain/cpp/desktop/libs"
REPOSITORY = "simplex-chat/simplex-chat"
TEST_REPOSITORY = "simplex-chat/simplex-chat-test"
RUN_URL = "https://github.com/simplex-chat/simplex-chat/actions/runs/1"
RELEASE_URL = "https://github.com/simplex-chat/simplex-chat-test/releases/tag/7.1.5-176"
RUN_JQ = '.workflow_runs[0] | "\\(.status) \\(.conclusion) \\(.html_url)"'
OWN_JARS = {
    "common-jvm-7.1.5.jar": "chat/simplex/common/BuildConfigCommon.class",
    "desktop-jvm-7.1.5-0123456789abcdef.jar": "chat/simplex/desktop/MainKt.class",
    "nanohttpd-2.3.1.jar": "org/nanohttpd/protocols/http/NanoHTTPD.class",
}
DEPENDENCY_JARS = {
    "c-3-1234567890abcdef.jar": "c/C.class",
    "b-1.0-0123456789abcdef0123456789abcdef.jar": "b/B.class",
    "a-2.jar": "a/A.class",
}
PACKAGES = [
    "simplex-desktop-macos-aarch64.dmg",
    "simplex-desktop-ubuntu-22_04-x86_64.deb",
    "simplex-desktop-windows-x86_64.msi",
    "simplex-desktop-x86_64.AppImage",
]
LOGIN_KEYCHAIN = "/Users/runner/Library/Keychains/login.keychain-db"
EXPORT_OPTIONS = {
    "method": "app-store-connect",
    "destination": "upload",
    "testFlightInternalTestingOnly": True,
    "manageAppVersionAndBuildNumber": False,
    "teamID": "5NN7GUYB6T",
    "signingStyle": "manual",
    "signingCertificate": "Apple Distribution",
    "provisioningProfiles": {
        "chat.simplex.app": "app-profile",
        "chat.simplex.app.SimpleX-NSE": "nse-profile",
        "chat.simplex.app.SimpleX-SE": "se-profile",
    },
}
SERVICE_ACCOUNT_EMAIL = "release@simplex-chat.iam.gserviceaccount.com"
TOKEN_URI = "https://oauth2.googleapis.com/token"
EDITS = "https://androidpublisher.googleapis.com/androidpublisher/v3/applications/chat.simplex.app/edits"
UPLOADS = "https://androidpublisher.googleapis.com/upload/androidpublisher/v3/applications/chat.simplex.app/edits"
NUMBERS = {
    "android": {"version_name": "7.1.5", "version_code": 399},
    "desktop": {"version_name": "7.1.5", "version_code": 176},
    "ios": {"version_name": "7.1.5", "build": 373},
}

FAKE = r"""#!/bin/bash
set -euo pipefail
shopt -s nullglob
calls=("$FAKE_CALLS"/*)
call=$FAKE_CALLS/$(printf %03d "${#calls[@]}")-NAME
mkdir -p "$call/files"
env -0 > "$call/env"
pwd > "$call/cwd"
cat > "$call/stdin"
: > "$call/argv"
i=0
for arg in "$@"; do
  printf '%s\0' "$arg" >> "$call/argv"
  if [ -f "${arg#@}" ]; then
    cp "${arg#@}" "$call/files/$i"
  fi
  i=$((i + 1))
done
mine=("$FAKE_CALLS"/*-NAME)
response=$FAKE_RESPONSES/NAME/${#mine[@]}
"""
GH = r"""
if [ ! -e "$response" ]; then
  echo "gh: Not Found (HTTP 404)" >&2
  exit 1
fi
filter=.
while [ $# -gt 0 ]; do
  if [ "$1" = --jq ]; then
    filter=$2
  fi
  shift
done
jq -r "$filter" "$response"
"""
CURL = r"""
cat "$response"
"""
APKSIGNER = r"""
if [ "$1" = verify ]; then
  echo "Signer #1 certificate DN: CN=SimpleX test"
  echo "Signer #1 certificate SHA-256 digest: 5e0f"
  echo "Signer #1 certificate SHA-1 digest: 9a1b"
  exit 0
fi
while [ $# -gt 1 ]; do
  if [ "$1" = --out ]; then
    out=$2
  fi
  shift
done
{ cat "$1"; echo signed; } > "$out"
"""
JARSIGNER = r"""
echo signed >> "${@: -2:1}"
"""
SECURITY = rf"""
case $1 in
  list-keychains)
    if [ $# = 3 ]; then
      echo '    "{LOGIN_KEYCHAIN}"'
    fi
    ;;
  cms)
    jq . "$4" > "$6"
    ;;
esac
"""
PLUTIL = r"""
jq -r --arg key "$2" '.[$key]' "$4"
"""


def properties(
    android: int = 399, desktop: int = 176, android_name: str = "7.1.5", desktop_name: str = "7.1.5"
) -> str:
    return (
        "kotlin.version=2.1.21\n"
        f"android.version_name={android_name}\n"
        f"android.version_code={android}\n"
        "\n"
        f"desktop.version_name={desktop_name}\n"
        f"desktop.version_code={desktop}\n"
        "desktop.ui_update_key=\n"
    )


def pbxproj(builds: tuple[int, ...] = (373, 373, 373), version: str = "7.1.5") -> str:
    return "".join(
        "\t\t\t\tCODE_SIGN_STYLE = Automatic;\n"
        f"\t\t\t\tCURRENT_PROJECT_VERSION = {build};\n"
        f"\t\t\t\tMARKETING_VERSION = {version};\n"
        for build in builds
    )


def app_store(version: str) -> dict:
    return {"resultCount": 1, "results": [{"bundleId": "chat.simplex.app", "version": version}]}


def app_ui_base() -> str:
    test = re.search(r'fun testUiBase\(\) \{.*?assertEquals\("([0-9a-f]{64})"', APP_UPDATER_TEST.read_text(), re.S)
    return test[1]


def desktop_app(jars: Path, runtime: Path) -> None:
    (jars / "resources").mkdir(parents=True)
    (jars / "resources/core-hash").write_text("core-1\n")
    for name, entry in {**OWN_JARS, **DEPENDENCY_JARS}.items():
        with zipfile.ZipFile(jars / name, "w") as jar:
            jar.writestr(entry, name)
    (jars / "SimpleX.cfg").write_text(
        "[Application]\n"
        + "".join(f"app.classpath=$APPDIR/{name}\n" for name in sorted({**OWN_JARS, **DEPENDENCY_JARS}))
        + "app.mainclass=chat.simplex.desktop.MainKt\n"
        "\n"
        "[JavaOptions]\n"
        "java-options=-Djpackage.app-version=7.1.4\n"
        "java-options=-Dcompose.application.resources.dir=$APPDIR/resources\n"
    )
    runtime.parent.mkdir(parents=True)
    runtime.write_text('JAVA_VERSION="17.0.13"\n\nMODULES="java.base"\n')


def link_scripts(tree: Path) -> None:
    for directory in ["scripts/ci/internal-release", "scripts/ci/desktop-linux"]:
        (tree / directory).parent.mkdir(parents=True, exist_ok=True)
        (tree / directory).symlink_to(ROOT / directory)


def b64(text: str) -> str:
    return base64.b64encode(text.encode()).decode()


def base64url(text: str) -> bytes:
    return base64.urlsafe_b64decode(text + "=" * (-len(text) % 4))


def sha256(file: Path) -> str:
    return hashlib.sha256(file.read_bytes()).hexdigest()


def openssl(*args, stdin: bytes | None = None) -> bytes:
    return subprocess.run(["openssl", *map(str, args)], input=stdin, check=True, capture_output=True).stdout


@dataclass(frozen=True)
class Call:
    name: str
    directory: Path

    @property
    def argv(self) -> list[str]:
        return (self.directory / "argv").read_text().split("\0")[:-1]

    @property
    def stdin(self) -> bytes:
        return (self.directory / "stdin").read_bytes()

    @property
    def cwd(self) -> Path:
        return Path((self.directory / "cwd").read_text().removesuffix("\n"))

    def file(self, arg: str) -> bytes:
        return (self.directory / "files" / str(self.argv.index(arg))).read_bytes()

    def env(self, name: str) -> str | None:
        variables = (self.directory / "env").read_text().split("\0")[:-1]
        return dict(variable.split("=", 1) for variable in variables).get(name)


class Repo:
    def __init__(self, path: Path, env: dict[str, str]):
        self.path = path
        self.env = env
        path.mkdir()
        self.git("init", "-q")
        self.git("config", "user.name", "t")
        self.git("config", "user.email", "t@t")

    def git(self, *args: str) -> str:
        return subprocess.run(
            ["git", *args], cwd=self.path, env=self.env, check=True, capture_output=True, text=True
        ).stdout.strip()

    def write(self, file: str, text: str) -> None:
        (self.path / file).parent.mkdir(parents=True, exist_ok=True)
        (self.path / file).write_text(text)

    def read(self, file: str) -> str:
        return (self.path / file).read_text()

    def set_numbers(self, gradle: str = properties(), xcode: str = pbxproj()) -> None:
        self.write(PROPERTIES, gradle)
        self.write(PBXPROJ, xcode)

    def commit(self, message: str) -> str:
        self.git("add", "-A")
        self.git("commit", "-q", "-m", message)
        return self.head()

    def head(self) -> str:
        return self.git("rev-parse", "HEAD")


class ScriptTest(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.tmp = Path(directory.name)
        for name in ["bin", "calls", "responses", "home", "tmp"]:
            (self.tmp / name).mkdir()
        self.env = {
            "PATH": f"{self.tmp / 'bin'}{os.pathsep}{os.environ['PATH']}",
            "HOME": str(self.tmp / "home"),
            "TMPDIR": str(self.tmp / "tmp"),
            "FAKE_CALLS": str(self.tmp / "calls"),
            "FAKE_RESPONSES": str(self.tmp / "responses"),
            "PYTHONDONTWRITEBYTECODE": "1",
        }

    def fake(self, name: str, body: str, script: Path | None = None) -> None:
        script = script or self.tmp / "bin" / name
        script.parent.mkdir(parents=True, exist_ok=True)
        script.write_text(FAKE.replace("NAME", name) + body)
        script.chmod(0o755)

    def respond(self, name: str, *responses) -> None:
        directory = self.tmp / "responses" / name
        directory.mkdir()
        for n, response in enumerate(responses, 1):
            if response is not None:
                (directory / str(n)).write_text(json.dumps(response))

    def run_script(self, script: str, *args, cwd: Path, **env: str) -> subprocess.CompletedProcess:
        return subprocess.run(
            [SCRIPTS / script, *map(str, args)],
            cwd=cwd,
            env={**self.env, **env},
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
        )

    def calls(self, name: str | None = None) -> list[Call]:
        calls = [Call(call.name.split("-", 1)[1], call) for call in sorted((self.tmp / "calls").iterdir())]
        return [call for call in calls if name is None or call.name == name]

    def temporary_files(self) -> list[Path]:
        return list((self.tmp / "tmp").iterdir())


class BuildNumbersTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.repo = Repo(self.tmp / "repo", self.env)
        self.repo.set_numbers()
        self.release = self.repo.commit("7.1.5")

    def build_numbers(self, *args: str) -> subprocess.CompletedProcess:
        return self.run_script("build-numbers.sh", *args, cwd=self.repo.path)

    def raised(self, gradle: str, xcode: str) -> subprocess.CompletedProcess:
        self.repo.set_numbers(gradle, xcode)
        self.repo.commit("build numbers")
        return self.build_numbers("raised", self.release, "HEAD")

    def assert_no_bump(self) -> None:
        head = self.repo.head()
        result = self.build_numbers("bump")
        self.assertEqual((result.returncode, result.stdout), (0, ""))
        self.assertEqual(result.stderr, f"::notice title=No bump::no commits after the build numbers changed in {head}\n")
        self.assertEqual(self.repo.head(), head)

    def test_show_reads_the_version_names_and_build_numbers(self):
        result = self.build_numbers("show", "HEAD")
        self.assertEqual(json.loads(result.stdout), NUMBERS)

    def test_show_requires_one_value_of_each_number(self):
        self.repo.set_numbers(xcode=pbxproj((373, 374, 373)))
        self.repo.commit("two iOS build numbers")
        result = self.build_numbers("show", "HEAD")
        self.assertEqual((result.returncode, result.stdout), (1, ""))
        self.assertEqual(result.stderr, "::error::expected one value of CURRENT_PROJECT_VERSION, found: 373\n374\n")
        self.repo.set_numbers(gradle=properties().replace("desktop.version_code=176\n", ""))
        self.repo.commit("no desktop build number")
        result = self.build_numbers("show", "HEAD")
        self.assertEqual((result.returncode, result.stdout), (1, ""))
        self.assertEqual(result.stderr, "::error::expected one value of desktop.version_code, found: none\n")

    def test_raised_releases_when_every_build_number_rises_and_the_version_names_stay(self):
        result = self.raised(properties(400, 177), pbxproj((374, 374, 374)))
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "release=true\n", ""))

    def test_raised_requires_every_build_number_raised(self):
        for kept, (android, desktop, ios) in {
            "android kept": (399, 177, 374),
            "android lowered": (398, 177, 374),
            "desktop kept": (400, 176, 374),
            "ios kept": (400, 177, 373),
        }.items():
            with self.subTest(kept):
                result = self.raised(properties(android, desktop), pbxproj((ios, ios, ios)))
                self.assertEqual((result.returncode, result.stdout), (0, "release=false\n"))
                self.assertEqual(result.stderr, "::notice title=No internal release::not every build number is raised\n")

    def test_raised_requires_the_version_names_kept(self):
        for changed, (gradle, xcode) in {
            "android": (properties(400, 177, android_name="7.1.6"), pbxproj((374, 374, 374))),
            "desktop": (properties(400, 177, desktop_name="7.1.6"), pbxproj((374, 374, 374))),
            "ios": (properties(400, 177), pbxproj((374, 374, 374), "7.1.6")),
        }.items():
            with self.subTest(changed):
                result = self.raised(gradle, xcode)
                self.assertEqual((result.returncode, result.stdout), (0, "release=false\n"))
                self.assertEqual(result.stderr, "::notice title=No internal release::the version names changed\n")

    def test_bump_raises_every_build_number_after_new_commits(self):
        self.repo.write("README.md", "change")
        self.repo.commit("change")
        result = self.build_numbers("bump")
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "bumped=true\n", ""))
        self.assertEqual(self.repo.git("log", "-1", "--format=%s"), "7.1.5: android 400, desktop 177, ios 374")
        self.assertEqual(self.repo.git("show", "--name-only", "--format=", "HEAD").splitlines(), [PBXPROJ, PROPERTIES])
        self.assertEqual(self.repo.read(PROPERTIES), properties(400, 177))
        self.assertEqual(self.repo.read(PBXPROJ), pbxproj((374, 374, 374)))
        self.assertEqual(self.repo.git("status", "--porcelain"), "")

    def test_bump_requires_commits_after_the_build_numbers_changed(self):
        with self.subTest("release"):
            self.assert_no_bump()
        with self.subTest("iOS build number raised"):
            self.repo.set_numbers(xcode=pbxproj((374, 374, 374)))
            self.repo.commit("iOS build 374")
            self.assert_no_bump()
        with self.subTest("Android build number raised"):
            self.repo.set_numbers(properties(400), pbxproj((374, 374, 374)))
            self.repo.commit("Android build 400")
            self.assert_no_bump()
        with self.subTest("bump"):
            self.repo.write("README.md", "change")
            self.repo.commit("change")
            self.assertEqual(self.build_numbers("bump").stdout, "bumped=true\n")
            self.assert_no_bump()

    def test_bump_follows_new_version_names(self):
        self.repo.set_numbers(properties(android_name="7.1.6", desktop_name="7.1.6"), pbxproj(version="7.1.6"))
        self.repo.commit("7.1.6")
        self.assertEqual(self.build_numbers("bump").stdout, "bumped=true\n")
        self.assertEqual(self.repo.git("log", "-1", "--format=%s"), "7.1.6: android 400, desktop 177, ios 374")

    def test_bump_fails_when_a_build_number_is_not_raised(self):
        self.repo.set_numbers(properties().replace("\n", "\r\n"))
        self.repo.commit("CRLF")
        self.repo.write("README.md", "change")
        self.repo.commit("change")
        result = self.build_numbers("bump")
        self.assertEqual((result.returncode, result.stdout), (1, ""))
        self.assertRegex(result.stderr, r"(?s)\A::error::7\.1\.5.: android 400, desktop 177, ios 374 does not raise every build number\n\Z")


class BuildPassedTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.fake("gh", GH)
        self.fake("sleep", "")
        self.repo = Repo(self.tmp / "repo", self.env)
        self.repo.set_numbers()
        self.repo.commit("7.1.5")
        self.repo.write("README.md", "change")
        self.change = self.repo.commit("change")

    def bump(self, *other_files: str) -> str:
        self.repo.set_numbers(properties(400, 177), pbxproj((374, 374, 374)))
        for file in other_files:
            self.repo.write(file, "bump")
        return self.repo.commit("7.1.5: android 400, desktop 177, ios 374")

    def build_passed(self, *responses: dict | None) -> subprocess.CompletedProcess:
        self.respond("gh", *responses)
        return self.run_script("build-passed.sh", cwd=self.repo.path, GITHUB_REPOSITORY=REPOSITORY, GH_TOKEN="token")

    def requests(self) -> list[list[str]]:
        return [call.argv for call in self.calls("gh")]

    def sleeps(self) -> list[list[str]]:
        return [call.argv for call in self.calls("sleep")]

    @staticmethod
    def runs_request(sha: str) -> list[str]:
        return ["api", f"repos/{REPOSITORY}/actions/workflows/build.yml/runs?head_sha={sha}&event=push", "--jq", RUN_JQ]

    @staticmethod
    def count_request(sha: str) -> list[str]:
        return ["api", f"repos/{REPOSITORY}/actions/workflows/build.yml/runs?head_sha={sha}&event=push", "--jq", ".total_count"]

    @staticmethod
    def runs(status: str, conclusion: str | None = None) -> dict:
        return {"total_count": 1, "workflow_runs": [{"status": status, "conclusion": conclusion, "html_url": RUN_URL}]}

    def test_passes_when_build_passed_on_the_commit(self):
        result = self.build_passed(self.runs("completed", "success"))
        self.assertEqual((result.returncode, result.stdout), (0, f"build.yml passed on {self.change}: {RUN_URL}\n"))
        self.assertEqual(self.requests(), [self.runs_request(self.change)])

    def test_fails_when_build_failed(self):
        result = self.build_passed(self.runs("completed", "failure"))
        error = f"::error title=No internal release::build.yml of {self.change}: completed failure {RUN_URL}\n"
        self.assertEqual((result.returncode, result.stdout), (1, error))

    def test_waits_a_minute_between_requests_while_build_runs(self):
        result = self.build_passed(self.runs("queued"), self.runs("in_progress"), self.runs("completed", "success"))
        self.assertEqual((result.returncode, result.stdout), (0, f"build.yml passed on {self.change}: {RUN_URL}\n"))
        self.assertEqual(self.requests(), [self.runs_request(self.change)] * 3)
        self.assertEqual(self.sleeps(), [["60"], ["60"]])

    def test_fails_when_build_does_not_complete_in_180_requests(self):
        result = self.build_passed(*[self.runs("in_progress")] * 181)
        error = f"::error title=No internal release::build.yml of {self.change}: in_progress null {RUN_URL}\n"
        self.assertEqual((result.returncode, result.stdout), (1, error))
        self.assertEqual(len(self.requests()), 180)

    def test_fails_without_a_build_run(self):
        result = self.build_passed({"total_count": 0, "workflow_runs": []})
        error = f"::error title=No internal release::build.yml of {self.change}: null null null\n"
        self.assertEqual((result.returncode, result.stdout), (1, error))
        self.assertEqual(self.requests(), [self.runs_request(self.change)])

    def test_uses_the_parent_of_a_bump_without_a_push_run(self):
        bump = self.bump()
        result = self.build_passed({"total_count": 0, "workflow_runs": []}, self.runs("completed", "success"))
        self.assertEqual((result.returncode, result.stdout), (0, f"build.yml passed on {self.change}: {RUN_URL}\n"))
        self.assertEqual(self.requests(), [self.count_request(bump), self.runs_request(self.change)])

    def test_uses_a_bump_with_its_own_push_run(self):
        bump = self.bump()
        result = self.build_passed(self.runs("completed", "success"), self.runs("completed", "success"))
        self.assertEqual((result.returncode, result.stdout), (0, f"build.yml passed on {bump}: {RUN_URL}\n"))
        self.assertEqual(self.requests(), [self.count_request(bump), self.runs_request(bump)])

    def test_fails_on_a_bump_whose_push_runs_cannot_be_counted(self):
        bump = self.bump()
        result = self.build_passed(None, {"total_count": 0, "workflow_runs": []})
        error = f"::error title=No internal release::build.yml of {bump}: null null null\n"
        self.assertEqual((result.returncode, result.stdout), (1, error))
        self.assertEqual(self.requests(), [self.count_request(bump), self.runs_request(bump)])

    def test_uses_a_commit_that_changes_more_than_the_build_numbers(self):
        commit = self.bump("README.md")
        result = self.build_passed(self.runs("completed", "success"))
        self.assertEqual((result.returncode, result.stdout), (0, f"build.yml passed on {commit}: {RUN_URL}\n"))
        self.assertEqual(self.requests(), [self.runs_request(commit)])


class TestFlightTrainTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.fake("curl", CURL)
        self.repo = Repo(self.tmp / "repo", self.env)
        (self.repo.path / ".git/info/exclude").write_text("/scripts\n")
        (self.repo.path / "scripts").symlink_to(ROOT / "scripts")
        self.output = self.tmp / "output"

    def trains(self, lookups: dict[str, dict | None]) -> dict[str, tuple[int, str, str]]:
        self.respond("curl", *lookups.values())
        results = {}
        for version in lookups:
            self.repo.set_numbers(xcode=pbxproj(version=version))
            self.repo.commit(version)
            self.output.write_text("")
            result = self.run_script("testflight-train.sh", cwd=self.repo.path, GITHUB_OUTPUT=str(self.output))
            results[version] = (result.returncode, self.output.read_text(), result.stdout)
        return results

    def test_train_is_open_above_the_version_in_the_app_store(self):
        results = self.trains({"7.1.5": app_store("7.1.4"), "7.1.10": app_store("7.1.9"), "7.10.0": app_store("7.9.1")})
        self.assertEqual(results, {version: (0, "open=true\n", "") for version in results})

    def test_train_is_closed_up_to_the_version_in_the_app_store(self):
        released = {"7.1.5": "7.1.5", "7.1.6": "7.1.7", "7.1.9": "7.1.10"}
        notice = "::notice title=TestFlight::the App Store has {released}, so the TestFlight train of {version} is closed: the iOS upload is skipped until MARKETING_VERSION is above {released}\n"
        self.assertEqual(
            self.trains({version: app_store(app_store_version) for version, app_store_version in released.items()}),
            {version: (0, "", notice.format(released=app_store_version, version=version)) for version, app_store_version in released.items()},
        )

    def test_train_is_open_for_an_app_missing_from_the_app_store(self):
        self.assertEqual(self.trains({"7.1.5": {"resultCount": 0, "results": []}}), {"7.1.5": (0, "open=true\n", "")})

    def test_failed_lookup_leaves_the_train_open(self):
        self.assertEqual(self.trains({"7.1.5": None}), {"7.1.5": (0, "open=true\n", "")})

    def test_app_store_is_asked_for_the_app(self):
        self.trains({"7.1.5": app_store("7.1.4")})
        self.assertEqual(
            [call.argv for call in self.calls("curl")], [["-fsS", "--retry", "3", "https://itunes.apple.com/lookup?bundleId=chat.simplex.app"]]
        )


class UiZipTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.jars = self.tmp / "app/lib/app"
        runtime = self.tmp / "app/lib/runtime/release"
        desktop_app(self.jars, runtime)
        (self.tmp / PROPERTIES).parent.mkdir(parents=True)
        (self.tmp / PROPERTIES).write_text(properties())
        self.result = self.run_script("ui-zip.py", self.jars, runtime, self.tmp / "out", cwd=self.tmp)

    def test_base_is_the_base_of_the_app_with_the_same_inputs(self):
        self.assertEqual(self.result.returncode, 0)
        self.assertEqual([update.name for update in (self.tmp / "out").iterdir()], [f"simplex-desktop-ui-{app_ui_base()}.zip"])

    def test_zip_has_the_own_jars_and_their_hashes(self):
        [update] = (self.tmp / "out").iterdir()
        with zipfile.ZipFile(update) as z:
            self.assertEqual(z.namelist(), ["ui.json", *sorted(OWN_JARS)])
            manifest = json.loads(z.read("ui.json"))
            for name in OWN_JARS:
                self.assertEqual(z.read(name), (self.jars / name).read_bytes())
        jars = {name: sha256(self.jars / name) for name in OWN_JARS}
        self.assertEqual(manifest, {"version_code": 176, "base": app_ui_base(), "jars": jars})
        self.assertEqual(self.result.stdout, f"{update.name}: {', '.join(sorted(OWN_JARS))}\n")


class BuildLinuxPackagesTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.tree = self.tmp / "tree"
        self.fake("install-core.sh", "", self.tree / "scripts/ci/desktop-linux/install-core.sh")
        self.fake("prepare-vlc-linux.sh", "", self.tree / "scripts/desktop/prepare-vlc-linux.sh")
        self.fake("make-deb-linux.sh", f'cp {LIBS}/linux-*/core-hash "$call/core-hash"\n', self.tree / "scripts/desktop/make-deb-linux.sh")
        self.fake("make-appimage-linux.sh", "", self.tree / "scripts/desktop/make-appimage-linux.sh")

    def build(self, os_version: str, arch: str) -> subprocess.CompletedProcess:
        (self.tree / LIBS / f"linux-{arch}").mkdir(parents=True)
        return self.run_script("build-linux-packages.sh", os_version, arch, "core1", cwd=self.tree)

    def test_ubuntu_22_04_gets_the_deb_and_the_appimage(self):
        self.build("22.04", "x86_64").check_returncode()
        calls = self.calls()
        self.assertEqual(
            [(call.name, call.argv) for call in calls],
            [
                ("install-core.sh", ["core/desktop-core.tar.gz"]),
                ("prepare-vlc-linux.sh", []),
                ("make-deb-linux.sh", []),
                ("make-appimage-linux.sh", []),
            ],
        )
        _, _, deb, appimage = calls
        self.assertEqual((deb.directory / "core-hash").read_text(), "core1\n")
        self.assertEqual((deb.env("SKIP_LIB"), deb.env("ASSETS_DIR")), ("1", "../../assets"))
        self.assertEqual((appimage.env("SKIP_LIB"), appimage.env("ASSETS_DIR")), (None, "../../assets"))

    def test_other_ubuntu_versions_get_the_deb_only(self):
        self.build("24.04", "aarch64").check_returncode()
        self.assertEqual([call.name for call in self.calls()], ["install-core.sh", "prepare-vlc-linux.sh", "make-deb-linux.sh"])
        self.assertEqual((self.tree / LIBS / "linux-aarch64/core-hash").read_text(), "core1\n")


class NameLinuxPackagesTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.tree = self.tmp / "tree"
        link_scripts(self.tree)
        release = self.tree / "apps/multiplatform/release/main"
        (release / "deb").mkdir(parents=True)
        for arch in ["x86_64", "aarch64"]:
            (release / f"deb/simplex_{arch}.deb").write_text(f"deb {arch}\n")
        (release / "SimpleX_Chat-x86_64.AppImage").write_text("AppImage\n")
        app = self.tree / "apps/multiplatform/extracted/opt/simplex"
        desktop_app(app / "lib/app", app / "lib/runtime/release")
        (self.tree / PROPERTIES).write_text(properties())

    def packages(self, os_version: str, arch: str) -> dict[str, bytes]:
        self.run_script("name-linux-packages.sh", cwd=self.tree, OS=os_version, ARCH=arch).check_returncode()
        return {package.name: package.read_bytes() for package in (self.tree / "out").iterdir()}

    def assert_ui_update(self, packages: dict[str, bytes]) -> None:
        with zipfile.ZipFile(io.BytesIO(packages[f"simplex-desktop-ui-{app_ui_base()}.zip"])) as z:
            self.assertEqual(z.namelist(), ["ui.json", *sorted(OWN_JARS)])

    def test_ubuntu_22_04_release_has_the_deb_the_appimage_and_the_ui_update(self):
        packages = self.packages("22.04", "x86_64")
        self.assertEqual(
            sorted(packages),
            ["simplex-desktop-ubuntu-22_04-x86_64.deb", f"simplex-desktop-ui-{app_ui_base()}.zip", "simplex-desktop-x86_64.AppImage"],
        )
        self.assertEqual(packages["simplex-desktop-ubuntu-22_04-x86_64.deb"], b"deb x86_64\n")
        self.assertEqual(packages["simplex-desktop-x86_64.AppImage"], b"AppImage\n")
        self.assert_ui_update(packages)

    def test_other_ubuntu_releases_have_the_deb_and_the_ui_update(self):
        packages = self.packages("24.04", "aarch64")
        self.assertEqual(sorted(packages), ["simplex-desktop-ubuntu-24_04-aarch64.deb", f"simplex-desktop-ui-{app_ui_base()}.zip"])
        self.assertEqual(packages["simplex-desktop-ubuntu-24_04-aarch64.deb"], b"deb aarch64\n")
        self.assert_ui_update(packages)


class BuildMacosDmgTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.tree = self.tmp / "tree"
        link_scripts(self.tree)
        self.fake("prepare-vlc-mac.sh", "", self.tree / "scripts/desktop/prepare-vlc-mac.sh")
        self.fake("build-desktop-mac.sh", f'cp {LIBS}/mac-*/core-hash "$call/core-hash"\n', self.tree / "scripts/ci/build-desktop-mac.sh")
        self.fake("gradlew", 'cp common/src/commonMain/cpp/desktop/libs/mac-*/core-hash "$call/core-hash"\n', self.tree / "apps/multiplatform/gradlew")
        (self.tmp / "libapp-lib.dylib").write_text("core\n")
        (self.tree / "core").mkdir()
        with tarfile.open(self.tree / "core/desktop-core.tar.gz", "w:gz") as core:
            core.add(self.tmp / "libapp-lib.dylib", "./libapp-lib.dylib")
        release = self.tree / "apps/multiplatform/release/main"
        (release / "dmg").mkdir(parents=True)
        (release / "dmg/SimpleX-7.1.5.dmg").write_text("dmg\n")
        contents = release / "app/SimpleX.app/Contents"
        desktop_app(contents / "app", contents / "runtime/Contents/Home/release")
        (self.tree / PROPERTIES).write_text(properties())
        self.zip_line = f"simplex-desktop-ui-{app_ui_base()}.zip: {', '.join(sorted(OWN_JARS))}\n"

    def build(self, arch: str, compose: str, keychain: str) -> subprocess.CompletedProcess:
        return self.run_script(
            "build-macos-dmg.sh", arch, compose, "core1", cwd=self.tree, APPLE_SIMPLEX_SIGNING_KEYCHAIN=keychain, ASSETS_DIR="../../assets", SKIP_LIB="1"
        )

    def assert_release(self, arch: str, compose: str) -> None:
        out = self.tree / "out"
        self.assertEqual(sorted(package.name for package in out.iterdir()), [f"simplex-desktop-macos-{arch}.dmg", f"simplex-desktop-ui-{app_ui_base()}.zip"])
        self.assertEqual((out / f"simplex-desktop-macos-{arch}.dmg").read_text(), "dmg\n")
        with zipfile.ZipFile(out / f"simplex-desktop-ui-{app_ui_base()}.zip") as z:
            self.assertEqual(z.namelist(), ["ui.json", *sorted(OWN_JARS)])
        libs = self.tree / LIBS / f"mac-{arch}"
        self.assertEqual({file.name: file.read_text() for file in libs.iterdir()}, {"libapp-lib.dylib": "core\n", "core-hash": "core1\n"})
        self.assertEqual(os.readlink(self.tree / f"apps/multiplatform/build/links/macos-{compose}"), f"../../common/src/commonMain/cpp/desktop/libs/mac-{arch}/")

    def test_dmg_is_signed_with_the_signing_keychain(self):
        result = self.build("aarch64", "arm64", "keychain")
        self.assertEqual((result.returncode, result.stdout), (0, self.zip_line))
        self.assertEqual([(call.name, call.argv) for call in self.calls()], [("prepare-vlc-mac.sh", []), ("build-desktop-mac.sh", [])])
        [build] = self.calls("build-desktop-mac.sh")
        self.assertEqual((build.directory / "core-hash").read_text(), "core1\n")
        self.assert_release("aarch64", "arm64")

    def test_dmg_is_unsigned_without_the_signing_keychain(self):
        result = self.build("x86_64", "x64", "")
        notice = "::notice title=macOS::APPLE_SIMPLEX_* are not set: the dmg is not signed\n"
        self.assertEqual((result.returncode, result.stdout), (0, notice + self.zip_line))
        self.assertEqual(
            [(call.name, call.argv) for call in self.calls()],
            [("prepare-vlc-mac.sh", []), ("gradlew", ["-Psimplex.assets.dir=../../assets", "packageDmg"])],
        )
        [gradlew] = self.calls("gradlew")
        self.assertEqual(gradlew.cwd, self.tree / "apps/multiplatform")
        self.assertEqual((gradlew.directory / "core-hash").read_text(), "core1\n")
        self.assert_release("x86_64", "x64")


class PublishTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.fake("gh", GH)
        for version in ["9.0.0", "35.0.0"]:
            self.fake(f"apksigner-{version}", APKSIGNER, self.tmp / f"android-sdk/build-tools/{version}/apksigner")
        self.repo = Repo(self.tmp / "repo", self.env)
        self.repo.set_numbers()
        self.commit = self.repo.commit("7.1.5")
        (self.repo.path / "scripts").symlink_to(ROOT / "scripts")
        self.desktop = self.tmp / "desktop"
        jars = self.tmp / "app/lib/app"
        runtime = self.tmp / "app/lib/runtime/release"
        desktop_app(jars, runtime)
        self.run_script("ui-zip.py", jars, runtime, self.desktop, cwd=self.repo.path).check_returncode()
        [self.ui_update] = [update.name for update in self.desktop.iterdir()]
        for package in PACKAGES:
            (self.desktop / package).write_text(package)
        self.android = self.tmp / "android"
        self.android.mkdir()
        (self.android / "simplex-aarch64.apk").write_text("aarch64\n")
        (self.android / "simplex-armv7a.apk").write_text("armv7a\n")
        self.release = self.tmp / "release"
        self.key = self.tmp / "ui-update.pem"
        openssl("genpkey", "-algorithm", "ed25519", "-out", self.key)
        self.secrets = {
            "DESKTOP_UI_UPDATE_KEY": self.key.read_text(),
            "ANDROID_TEST_KEYSTORE": b64("test keystore"),
            "ANDROID_TEST_KEYSTORE_PASSWORD": "keystore password",
            "ANDROID_TEST_KEY_ALIAS": "test",
            "ANDROID_TEST_KEY_PASSWORD": "key password",
            "SIMPLEX_CHAT_TEST_TOKEN": "test token",
        }
        self.notes = (
            f"Internal build of https://github.com/{REPOSITORY}/commit/{self.commit}\n"
            "\n"
            "Android 7.1.5 (399), desktop 7.1.5 (176), iOS 7.1.5 (373)"
        )
        self.numbers = {**NUMBERS, "commit": self.commit}

    def publish_release(self, blob: str | None = "blob1") -> subprocess.CompletedProcess:
        self.respond("gh", {"sha": blob} if blob else None, {"commit": {"sha": "commit2"}}, RELEASE_URL)
        return self.publish(**self.secrets)

    def publish(self, **secrets: str) -> subprocess.CompletedProcess:
        return self.run_script(
            "publish.sh",
            self.desktop,
            self.android,
            self.release,
            cwd=self.repo.path,
            GITHUB_REPOSITORY=REPOSITORY,
            GITHUB_SHA=self.commit,
            GITHUB_SERVER_URL="https://github.com",
            GITHUB_STEP_SUMMARY=str(self.tmp / "summary.md"),
            ANDROID_HOME=str(self.tmp / "android-sdk"),
            **secrets,
        )

    def released(self) -> list[str]:
        return sorted(os.listdir(self.release))

    def requests(self, blob: str | None) -> list[list[str]]:
        content = base64.b64encode(f"{json.dumps(self.numbers, indent=2)}\n".encode()).decode()
        files = [str(self.release / name) for name in self.released()]
        return [
            ["api", f"repos/{TEST_REPOSITORY}/contents/release.json", "--jq", ".sha"],
            [
                "api",
                "-X",
                "PUT",
                f"repos/{TEST_REPOSITORY}/contents/release.json",
                "-f",
                "message=7.1.5-176",
                "-f",
                f"content={content}",
                *(["-f", f"sha={blob}"] if blob else []),
                "--jq",
                ".commit.sha",
            ],
            ["release", "create", "7.1.5-176", *files, "--repo", TEST_REPOSITORY, "--target", "commit2", "--title", "7.1.5-176", "--notes", self.notes],
        ]

    def test_ui_update_signature_passes_the_app_verification(self):
        self.publish_release().check_returncode()
        with zipfile.ZipFile(self.release / self.ui_update) as z:
            self.assertEqual(z.namelist(), ["ui.json", *sorted(OWN_JARS), "ui.json.sig"])
            (self.tmp / "ui.json").write_bytes(z.read("ui.json"))
            (self.tmp / "ui.json.sig").write_bytes(z.read("ui.json.sig"))
        (self.tmp / "app-key.der").write_bytes(openssl("pkey", "-in", self.key, "-pubout", "-outform", "DER"))
        verified = openssl(
            "pkeyutl", "-verify", "-pubin", "-keyform", "DER", "-inkey", self.tmp / "app-key.der",
            "-rawin", "-in", self.tmp / "ui.json", "-sigfile", self.tmp / "ui.json.sig",
        )
        self.assertEqual(verified, b"Signature Verified Successfully\n")

    def test_apks_are_signed_with_the_test_key_by_the_newest_apksigner(self):
        self.publish_release().check_returncode()
        self.assertEqual(self.calls("apksigner-9.0.0"), [])
        aarch64, armv7a, verify = self.calls("apksigner-35.0.0")
        keystore = aarch64.argv[aarch64.argv.index("--ks") + 1]
        self.assertEqual(aarch64.file(keystore), b"test keystore")
        key = ["--ks", keystore, "--ks-pass", "env:ANDROID_TEST_KEYSTORE_PASSWORD", "--ks-key-alias", "test", "--key-pass", "env:ANDROID_TEST_KEY_PASSWORD"]
        self.assertEqual(aarch64.argv, ["sign", "--alignment-preserved", *key, "--out", f"{self.release}/simplex.apk", f"{self.android}/simplex-aarch64.apk"])
        self.assertEqual(armv7a.argv, ["sign", "--alignment-preserved", *key, "--out", f"{self.release}/simplex-armv7a.apk", f"{self.android}/simplex-armv7a.apk"])
        self.assertEqual(verify.argv, ["verify", "--print-certs", f"{self.release}/simplex.apk"])
        self.assertEqual((self.release / "simplex.apk").read_text(), "aarch64\nsigned\n")
        self.assertEqual((self.release / "simplex-armv7a.apk").read_text(), "armv7a\nsigned\n")
        self.assertEqual(self.temporary_files(), [])

    def test_release_is_published_to_simplex_chat_test(self):
        result = self.publish_release()
        self.assertEqual((result.returncode, result.stdout), (0, f"{RELEASE_URL}\n::notice title=simplex-chat-test::7.1.5-176 is published\n"))
        self.assertEqual(self.released(), sorted([*PACKAGES, self.ui_update, "simplex.apk", "simplex-armv7a.apk"]))
        self.assertEqual([call.argv for call in self.calls("gh")], self.requests("blob1"))
        self.assertEqual({call.env("GH_TOKEN") for call in self.calls("gh")}, {"test token"})
        files = "".join(f"- `{name}`\n" for name in self.released())
        self.assertEqual(
            (self.tmp / "summary.md").read_text(),
            "Signer #1 certificate SHA-256 digest: 5e0f\n"
            "### 7.1.5-176\n"
            "\n"
            f"{self.notes}\n"
            "\n"
            f"{files}"
            "\n"
            "```json\n"
            f"{json.dumps(self.numbers, indent=2)}\n"
            "```\n",
        )

    def test_release_json_is_created_when_absent(self):
        self.publish_release(blob=None).check_returncode()
        self.assertEqual([call.argv for call in self.calls("gh")], self.requests(None))

    def test_each_step_without_its_secret_is_skipped_with_a_notice(self):
        result = self.publish(**{name: "" for name in self.secrets})
        self.assertEqual(
            (result.returncode, result.stdout),
            (
                0,
                "::notice title=UI updates::DESKTOP_UI_UPDATE_KEY is not set: the release has no UI updates\n"
                "::notice title=Android test APKs::ANDROID_TEST_KEYSTORE is not set: the release has no APKs\n"
                "::notice title=simplex-chat-test::SIMPLEX_CHAT_TEST_TOKEN is not set: 7.1.5-176 is not published\n",
            ),
        )
        self.assertEqual(self.released(), sorted(PACKAGES))
        self.assertEqual(self.calls(), [])
        self.assertEqual(self.temporary_files(), [])


class TestFlightTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.fake("security", SECURITY)
        self.fake("plutil", PLUTIL)
        self.fake("xcodebuild", "")
        self.archive = self.tmp / "SimpleX.xcarchive"
        self.info = self.archive / "Products/Applications/SimpleX.app/Info.plist"
        self.info.parent.mkdir(parents=True)
        self.info.write_text(json.dumps({"CFBundleVersion": "373"}))
        self.profiles = {name: json.dumps({"UUID": f"{name}-profile"}) for name in ["app", "nse", "se"]}
        self.secrets = {
            "IOS_DISTRIBUTION_CERTIFICATE": b64("distribution certificate"),
            "IOS_DISTRIBUTION_CERTIFICATE_PASSWORD": "certificate password",
            **{f"IOS_PROFILE_{name.upper()}": b64(profile) for name, profile in self.profiles.items()},
            "APP_STORE_CONNECT_KEY": "-----BEGIN PRIVATE KEY-----\nkey\n-----END PRIVATE KEY-----\n",
            "APP_STORE_CONNECT_KEY_ID": "KEY1",
            "APP_STORE_CONNECT_ISSUER_ID": "issuer1",
        }

    def upload(self, **secrets: str) -> subprocess.CompletedProcess:
        return self.run_script("testflight.sh", self.archive, cwd=self.tmp, **secrets)

    def test_archive_is_exported_with_the_distribution_identity_and_uploaded(self):
        result = self.upload(**self.secrets)
        self.assertEqual((result.returncode, result.stdout), (0, "::notice title=TestFlight::build 373 is uploaded for internal testing\n"))
        calls = self.calls()
        keychain = calls[0].argv[-1]
        work = Path(keychain).parent
        self.assertEqual(work.parent, self.tmp / "tmp")
        install_profile = [
            ("security", ["cms", "-D", "-i", f"{work}/profile.mobileprovision", "-o", f"{work}/profile.plist"]),
            ("plutil", ["-extract", "UUID", "raw", f"{work}/profile.plist"]),
        ]
        export = [
            "-exportArchive",
            "-archivePath", str(self.archive),
            "-exportPath", f"{work}/export",
            "-exportOptionsPlist", f"{work}/ExportOptions.plist",
            "-authenticationKeyPath", f"{work}/AuthKey.p8",
            "-authenticationKeyID", "KEY1",
            "-authenticationKeyIssuerID", "issuer1",
        ]
        self.assertEqual(
            [(call.name, call.argv) for call in calls],
            [
                ("security", ["create-keychain", "-p", "", f"{work}/distribution.keychain-db"]),
                ("security", ["set-keychain-settings", keychain]),
                ("security", ["unlock-keychain", "-p", "", keychain]),
                ("security", ["import", f"{work}/distribution.p12", "-k", keychain, "-P", "certificate password", "-T", "/usr/bin/codesign"]),
                ("security", ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", "", keychain]),
                ("security", ["list-keychains", "-d", "user"]),
                ("security", ["list-keychains", "-d", "user", "-s", keychain, LOGIN_KEYCHAIN]),
                *install_profile * 3,
                ("xcodebuild", export),
                ("plutil", ["-extract", "CFBundleVersion", "raw", str(self.info)]),
                ("security", ["delete-keychain", keychain]),
            ],
        )
        [certificate] = [call.file(f"{work}/distribution.p12") for call in calls if call.argv[0] == "import"]
        self.assertEqual(certificate, b"distribution certificate")
        decoded = [call.file(f"{work}/profile.mobileprovision").decode() for call in calls if call.argv[0] == "cms"]
        self.assertEqual(decoded, list(self.profiles.values()))
        [xcodebuild] = self.calls("xcodebuild")
        self.assertEqual(plistlib.loads(xcodebuild.file(f"{work}/ExportOptions.plist")), EXPORT_OPTIONS)
        self.assertEqual(xcodebuild.file(f"{work}/AuthKey.p8").decode(), self.secrets["APP_STORE_CONNECT_KEY"])
        installed = Path(self.env["HOME"]) / "Library/Developer/Xcode/UserData/Provisioning Profiles"
        self.assertEqual(
            {profile.name: profile.read_text() for profile in installed.iterdir()},
            {f"{name}-profile.mobileprovision": profile for name, profile in self.profiles.items()},
        )
        self.assertEqual(self.temporary_files(), [])

    def test_failed_profile_decode_fails_the_upload(self):
        result = self.upload(**{**self.secrets, "IOS_PROFILE_NSE": b64("not a profile")})
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertEqual(
            [(call.name, call.argv[0]) for call in self.calls()],
            [
                ("security", "create-keychain"),
                ("security", "set-keychain-settings"),
                ("security", "unlock-keychain"),
                ("security", "import"),
                ("security", "set-key-partition-list"),
                ("security", "list-keychains"),
                ("security", "list-keychains"),
                ("security", "cms"),
                ("plutil", "-extract"),
                ("security", "cms"),
                ("security", "delete-keychain"),
            ],
        )
        installed = Path(self.env["HOME"]) / "Library/Developer/Xcode/UserData/Provisioning Profiles"
        self.assertEqual([profile.name for profile in installed.iterdir()], ["app-profile.mobileprovision"])
        self.assertEqual(self.temporary_files(), [])

    def test_upload_without_its_secrets_is_skipped_with_a_notice(self):
        for missing in ["IOS_DISTRIBUTION_CERTIFICATE", "APP_STORE_CONNECT_KEY"]:
            with self.subTest(missing):
                result = self.upload(**{**self.secrets, missing: ""})
                notice = "::notice title=TestFlight::IOS_DISTRIBUTION_CERTIFICATE or APP_STORE_CONNECT_KEY is not set: the archive is not uploaded\n"
                self.assertEqual((result.returncode, result.stdout), (0, notice))
                self.assertEqual(self.calls(), [])


class GooglePlayTest(ScriptTest):
    def setUp(self):
        super().setUp()
        self.fake("curl", CURL)
        self.fake("jarsigner", JARSIGNER, self.tmp / "jdk/bin/jarsigner")
        self.bundle = self.tmp / "bundle/simplex.aab"
        self.bundle.parent.mkdir()
        self.bundle.write_text("bundle\n")
        self.key = self.tmp / "service-account.pem"
        openssl("genpkey", "-algorithm", "RSA", "-pkeyopt", "rsa_keygen_bits:2048", "-out", self.key)
        account = {"type": "service_account", "client_email": SERVICE_ACCOUNT_EMAIL, "private_key": self.key.read_text(), "token_uri": TOKEN_URI}
        self.secrets = {
            "ANDROID_UPLOAD_KEYSTORE": b64("upload keystore"),
            "ANDROID_UPLOAD_KEYSTORE_PASSWORD": "keystore password",
            "ANDROID_UPLOAD_KEY_ALIAS": "upload",
            "ANDROID_UPLOAD_KEY_PASSWORD": "key password",
            "GOOGLE_PLAY_SERVICE_ACCOUNT": json.dumps(account),
        }
        self.respond("curl", {"access_token": "play token"}, {"id": "edit1"}, {"versionCode": 401}, {}, {})

    def google_play(self, **secrets: str) -> subprocess.CompletedProcess:
        return self.run_script("google-play.sh", self.bundle, cwd=self.tmp, JAVA_HOME=str(self.tmp / "jdk"), **secrets)

    def test_bundle_is_signed_and_released_to_the_internal_track(self):
        result = self.google_play(**self.secrets)
        self.assertEqual(
            (result.returncode, result.stdout),
            (0, "::add-mask::play token\n::notice title=Google Play::version code 401 is released to internal testing\n"),
        )
        calls = self.calls()
        self.assertEqual([call.name for call in calls], ["jarsigner", "curl", "curl", "curl", "curl", "curl"])
        jarsigner, token, edit, upload, track, commit = calls
        keystore = jarsigner.argv[1]
        self.assertEqual(
            jarsigner.argv,
            ["-keystore", keystore, "-storepass:env", "ANDROID_UPLOAD_KEYSTORE_PASSWORD", "-keypass:env", "ANDROID_UPLOAD_KEY_PASSWORD", str(self.bundle), "upload"],
        )
        self.assertEqual(jarsigner.file(keystore), b"upload keystore")
        jwt = token.argv[-1].removeprefix("assertion=")
        self.assertEqual(
            token.argv,
            ["-sS", "--fail-with-body", TOKEN_URI, "--data-urlencode", "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer", "--data-urlencode", f"assertion={jwt}"],
        )
        play =["-sS", "--fail-with-body", "-H", "Authorization: Bearer play token"]
        self.assertEqual(edit.argv, [*play, "-X", "POST", EDITS])
        self.assertEqual(
            upload.argv,
            [*play, "-X", "POST", "-H", "Content-Type: application/octet-stream", "--data-binary", f"@{self.bundle}", f"{UPLOADS}/edit1/bundles?uploadType=media"],
        )
        self.assertEqual(upload.file(f"@{self.bundle}"), b"bundle\nsigned\n")
        self.assertEqual(track.argv, [*play, "-X", "PUT", "-H", "Content-Type: application/json", "--data-binary", "@-", f"{EDITS}/edit1/tracks/internal"])
        self.assertEqual(json.loads(track.stdin), {"track": "internal", "releases": [{"versionCodes": ["401"], "status": "completed"}]})
        self.assertEqual(commit.argv, [*play, "-X", "POST", f"{EDITS}/edit1:commit?changesInReviewBehavior=ERROR_IF_IN_REVIEW"])
        self.assertEqual(self.temporary_files(), [])

    def test_token_is_requested_with_a_jwt_signed_by_the_service_account(self):
        issued_after = int(time.time())
        self.google_play(**self.secrets).check_returncode()
        issued_before = int(time.time())
        header, claims, signature = self.calls("curl")[0].argv[-1].removeprefix("assertion=").split(".")
        for part in [header, claims, signature]:
            self.assertRegex(part, r"\A[A-Za-z0-9_-]+\Z")
        self.assertEqual(json.loads(base64url(header)), {"alg": "RS256", "typ": "JWT"})
        issued = json.loads(base64url(claims))
        self.assertEqual(
            issued,
            {
                "iss": SERVICE_ACCOUNT_EMAIL,
                "scope": "https://www.googleapis.com/auth/androidpublisher",
                "aud": TOKEN_URI,
                "iat": issued["iat"],
                "exp": issued["iat"] + 600,
            },
        )
        self.assertTrue(issued_after <= issued["iat"] <= issued_before)
        (self.tmp / "public.pem").write_bytes(openssl("pkey", "-in", self.key, "-pubout"))
        (self.tmp / "signature").write_bytes(base64url(signature))
        verified = openssl(
            "dgst", "-sha256", "-verify", self.tmp / "public.pem", "-signature", self.tmp / "signature",
            stdin=f"{header}.{claims}".encode(),
        )
        self.assertEqual(verified, b"Verified OK\n")

    def test_upload_without_its_secrets_is_skipped_with_a_notice(self):
        for missing in ["ANDROID_UPLOAD_KEYSTORE", "GOOGLE_PLAY_SERVICE_ACCOUNT"]:
            with self.subTest(missing):
                result = self.google_play(**{**self.secrets, missing: ""})
                notice = "::notice title=Google Play::ANDROID_UPLOAD_KEYSTORE or GOOGLE_PLAY_SERVICE_ACCOUNT is not set: the bundle is not uploaded\n"
                self.assertEqual((result.returncode, result.stdout), (0, notice))
                self.assertEqual(self.calls(), [])


if __name__ == "__main__":
    unittest.main()
