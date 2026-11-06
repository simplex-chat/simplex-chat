# Feature tests on the app test runners

Scripted UI tests of a feature run on GitHub's runners after the smoke test, in the same app: on iOS (`ios-sim`), Android (`android-emu`) and desktop (`desktop-linux`). A feature's test is added as a new file.

## The files

The test of a feature on one platform is `scripts/ci/<platform>/tests/<feature>.sh`, with `<platform>` `ios-sim`, `android` or `desktop-linux`. The feature name is the same on every platform with a test. A name consists of lowercase letters, digits and dashes. Examples: `desktop-linux/tests/private-notes.sh`, `android/tests/private-notes.sh`, `ios-sim/tests/settings.sh`.

```bash
#!/bin/bash
# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/*
# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/*

set -euo pipefail
. "$(dirname "$0")/../lib.sh"

"$ui" click-text "Private notes"
...
check "the note shows in the chat" "$ui" find "scripted note"
```

- `# covers:` lines contain the code paths exercised by the test, as shell patterns from the repo root separated by spaces (`*` also matches `/`; `?` matches any one character, which is how a space in a path is written). The test is selected when a file matching a pattern is changed on the branch.
- `# needs: backend` declares that the test requires the test backend. In a run with the backend, `BACKEND_DIR` is set to its work directory (`names-server`, `directory-link`, and on iOS `seed.md`). In a run without it, the test is skipped with a note.
- The test body is bash over the platform's `ui`, with the commands a Claude session sends. `ui` without arguments prints them; each platform's README describes its driver.
- `lib.sh` contains the platform's shared steps (on desktop and Android: finding text and positions, returning to the chat list, opening Settings; on iOS: `in_tree`) and sources `scripts/ci/app-tests/checks.sh`:
  - `check "<what is true>" <command>` records `PASS` or `FAIL`, and the test continues;
  - `fail "<why>"` ends the test;
  - `note "<text>"` records a remark for the summary.
- A step required by more than one feature is placed in `lib.sh`. After a change to `lib.sh`, every test of that platform is selected.
- iOS tests, `scripts/ci/app-tests/feature-tests.sh` and `checks.sh` run under macOS's `/bin/bash` 3.2, so `mapfile`, associative arrays, `${x,,}` and the expansion of an empty array under `set -u` are excluded from them.

## What a test starts from

The app in the state left by the smoke test, restarted at the chat list with the reachable toolbar (one-hand UI on iOS, "Reachable app toolbars" on desktop). `ui layout top` switches to the top bar.

| Platform | Profile | Chats | With the backend |
|---|---|---|---|
| desktop | onboarded as citester | "seeded notes", a group with the two images sent by the smoke test; "Private notes" | the test SMP server is the only one resolving names (`seeded.testing` resolves to ci_contact) |
| Android | onboarded | "seeded notes", a group with the two images sent by the smoke test; "Private notes" | the test SMP server added under Your servers |
| iOS | onboarded, local files unencrypted, SimpleX Lock notice marked as shown | "image notes", a group with the two images sent by the smoke test; "Private notes" | — |

Tests run one after another in name order, in one profile, and each test must pass with the state left by earlier tests.

## Test selection

The smoke test always runs. Once its checks pass (on iOS, also when `snapshots.sh` fails, for example on differing preview snapshots), the step "Run feature tests" runs the tests selected by comparing the branch with the upstream master. A deleted or moved file counts as changed. The selected tests are:

- tests added or changed on the branch;
- tests whose `covers` patterns match a file changed on the branch;
- every test of a platform when a shared file changes: a file in `scripts/ci/app-tests/`, or a file in the platform's folder other than `tests/`, `*.md` and `claude-*`.

Changes to workflow files are excluded from selection, so a branch with changes to the feature test step is run with `tests: all`.

The run's `tests` input overrides the selection: feature names separated by spaces, `all` or `none` (Actions tab, or `gh workflow run <workflow>.yml --ref <branch> -f tests="private-notes"`). The plan job fails for a name with a test on no platform. The reason for each selected test is printed in the plan job's log, and the selected tests are listed in its summary.

`SelectTestsTest` and `FeatureTestsTest` in `scripts/ci/test_packages_release.py` verify these rules and the runner; the reduced build runs them when a file in `scripts/ci/app-tests/` changes.

Each test has a section in the job summary, and its screens and `checks.txt` are stored in `tests/<feature>/` of the smoke-test artifact. After a failed test the remaining tests still run, and the step fails if any test failed. The app job fails when a feature test fails, and GitHub then skips the Claude job. With `tests: none`, feature tests are excluded from the run.

## After the feature is merged

The tests remain on master, and a test is selected for any branch with changes to the code it covers. When it fails, the test or the app is updated in that branch's pull request, as with a core test.

## Writing a test

1. Read the screens. The smoke-test artifact of an earlier run (`gh run download <id> -n <artifact>`, with `ios-sim-smoke-test`, `android-smoke-test` or `desktop-smoke-test`) contains, per step, a screenshot and the text read from it: desktop `NN-<step>.txt` (`x,y phrase` from OCR), Android `NN-<step>.xml` and `.txt` (the accessibility tree), iOS `NN-<step>.json` (AXe's tree).
2. Write the steps by text or label: `click-text`, `tap`, `wait`, `find`. Coordinates are required only for icons without text, mostly on desktop (the toolbar's buttons); the Android and iOS trees contain the description and position of each labelled icon.
3. Verify the feature's behaviour with `check`, and take a `ui shot <name>` at each result.
4. Push and start a run (`gh workflow run <workflow>.yml --ref <branch>`, or the Actions tab). When a test stops, its folder contains the evidence: the last screens, `test.log` (the test's output, ending with the error) and, on desktop and Android, `steps.log` (each `ui` command). The app log is `app.log` in the test's folder on desktop and at the artifact's root on Android; on iOS it is recorded during the smoke test only. After a failed restart, the error is in the job log.

### From a Claude run (optional)

A Claude session on the runner explores the app with the same `ui` commands. Its artifact contains:

- the guard log: every `ui` command sent by the session, marked `allow` or `refuse` (`claude-transcript.jsonl.guard.log` on desktop and Android, `<layout>-guard.log` on iOS);
- the transcript, with the output of each command;
- the report, with the result of each step, and the screens with their trees or OCR text.

To convert a session into a scripted test, list its commands (`grep '^allow' <guard log>`), keep those that changed the app's state, drop those that only read the screen, and rewrite its conclusions as `check` lines. Then verify the test with a run without Claude.

`ios-sim/tests/settings.sh` was written this way, from a session of the earlier smoke plan that opened Settings from the profile picker and closed it. The taps and the swipe were taken from the guard log, and the checks from the session's report ("Your settings", "Network & servers", and Settings closed after the swipe). The fixed point of a tap was replaced by a search for the element in the tree.

The Claude job starts at a different point from the feature tests. On desktop and Android it starts at the app's first screen, so a session used for this must first onboard and create the group, as stated in `claude-test.md`; on iOS it starts at the chat list without conversations, the state left by the smoke test before its images case.

## Writing tests without the app

- The app runs only on the runner, so a test's flow and timing are verified there.
- Text and element searches can be checked locally against a run's saved screens:
  - Android: `python3 scripts/ci/android/screen-tree.py <NN-step.xml> find "<text>"` (or `lines`, `tree`) returns what `ui find` returns on the runner.
  - iOS: `NN-step.json` is AXe's tree; `jq` over it, or the filter in `compact_tree` of `scripts/ci/ios-sim/ui`, prints the labels, values and frames of `ui tree`.
  - Desktop: `NN-step.txt` contains the phrases read by OCR and their centres. Reading a screenshot again requires tesseract and ImageMagick, installed on the runner; OCR errors appear only there.
- iOS and Android tests can therefore be written mostly by label from a run's screens, and desktop tests from the phrases, with coordinates for elements without a label.
