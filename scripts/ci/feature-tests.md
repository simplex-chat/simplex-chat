# Feature tests on the app test runners

A feature's scripted UI tests run on GitHub's runners after the smoke test, in the same app: on iOS (`ios-sim`), Android (`android-emu`) and desktop (`desktop-linux`). A feature adds them as new files; no shared script changes.

## The files

A feature's test on one platform is `scripts/ci/<platform>/tests/<feature>.sh`, with `<platform>` `ios-sim`, `android` or `desktop-linux`. The feature has the same name on every platform it covers; a platform without the file isn't covered. Names are lowercase letters, digits and dashes. Examples: `desktop-linux/tests/private-notes.sh`, `android/tests/private-notes.sh`, `ios-sim/tests/settings.sh`.

```bash
#!/bin/bash
# Private notes: a note sent in "Private notes" shows in the chat and as the chat's preview in the chat list.
# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/*
# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/*

set -euo pipefail
. "$(dirname "$0")/../lib.sh"

"$ui" click-text "Private notes"
...
check "the note shows in the chat" "$ui" find "scripted note"
```

- The first comment line says what the test checks.
- `# covers:` lines list the code the test exercises, as shell patterns from the repo root (`*` also matches `/`). A run selects the test when its branch changes a file that matches one.
- `# needs: backend` marks a test that needs the test backend. It gets `BACKEND_DIR`, the backend's work directory (`names-server`, `directory-link`, and on iOS `seed.md`). Without the backend, the test is skipped with a note.
- The rest is bash over the platform's `ui`, the same commands a Claude session sends. `ui` alone prints them; each platform's README describes its driver.
- `lib.sh` holds the platform's shared steps (finding text and positions, getting back to the chat list, opening Settings) and loads `scripts/ci/checks.sh`:
  - `check "<what is true>" <command>` records `PASS` or `FAIL` and goes on;
  - `fail "<why>"` ends the test;
  - `note "<text>"` records a remark for the summary.
- A step that more than one feature needs belongs in `lib.sh`. A change to `lib.sh` runs every test of that platform.
- iOS tests run under macOS's `/bin/bash` 3.2: no `mapfile`, no associative arrays, no `${x,,}`, and no expansion of an empty array under `set -u`.

## What a test starts from

The app as the smoke test left it, restarted at the chat list with the reachable toolbar (one-hand UI on iOS, "Reachable app toolbars" on desktop). `ui layout top` switches to the top bar.

| Platform | Profile | Chats | With the backend |
|---|---|---|---|
| desktop | onboarded as citester | "seeded notes", "Private notes" | the test SMP server is the only one resolving names (`seeded.testing` resolves to ci_contact) |
| Android | onboarded | "seeded notes", "Private notes" | the test SMP server added under Your servers |
| iOS | onboarded | none: the chat list shows its onboarding pages | — |

Tests run one after another in name order and share that profile. A test may leave state behind; it names what it creates after itself and must still pass with what earlier tests left.

## Which tests a run runs

The smoke test always runs. After it, the step "Run feature tests" runs, compared with the upstream master:

- the tests the branch adds or changes;
- the tests whose `covers` patterns match a file the branch changes;
- every test of a platform when what the tests run on changes: `scripts/ci/checks.sh`, `feature-tests.sh`, `select-tests.sh` or `choose-feature-tests.sh`, or a file in the platform's folder other than `tests/`, `*.md` and `claude-*`.

The run's `tests` input overrides this: feature names separated by spaces, `all` or `none` (Actions tab, or `gh workflow run <workflow>.yml --ref <branch> -f tests="private-notes"`). The plan job's log says why each test was selected, and its summary lists them.

Each test gets a section in the job summary, and its screens and `checks.txt` go to `tests/<feature>/` in the smoke-test artifact. One failed test doesn't stop the others; the step fails if any failed. A failed feature test fails the app job, so a run that also asks for Claude skips the Claude job: start it with `tests: none` to run Claude anyway.

## After the feature merges

The tests stay on master. A run of a branch that touches code a test covers runs it. When it fails, that branch's PR updates the test or the app, as with a core test. Before a release, run each workflow with `tests: all`.

## Writing a test

1. Read what the screens show. The smoke-test artifact of an earlier run (`gh run download <id> -n <artifact>`, with `ios-sim-smoke-test`, `android-smoke-test` or `desktop-smoke-test`) has, per step, a screenshot and what was read from it: desktop `NN-<step>.txt` (`x,y phrase` from OCR), Android `NN-<step>.xml` and `.txt` (the accessibility tree), iOS `NN-<step>.json` (AXe's tree).
2. Write the steps by text or label: `click-text`, `tap`, `wait`, `find`. Coordinates are needed only for icons without text, mostly on desktop (the toolbar's buttons); the Android and iOS trees give every icon's description and position.
3. Check what the feature promises with `check`, and take a `ui shot <name>` at each result.
4. Push and start a run (`ci-run <workflow>`, or the Actions tab). Where the test stops, its last screens and `steps.log` show what the app showed.

### From a Claude run (optional)

A Claude session on the runner explores the app with the same `ui` commands. Its artifact holds:

- the guard log: every `ui` command it sent, allowed or refused (`claude-transcript.jsonl.guard.log` on desktop and Android, `<layout>-guard.log` on iOS);
- the transcript, with each command's output;
- its report, with what it saw at each step, and its screens with their trees or OCR text.

To turn a session into a scripted test, list the commands it sent (`grep '^allow' <guard log>`), keep the ones that moved the app forward, drop the reads it made to find its way, and turn its conclusions into `check` lines. Then prove the test with a plain run.

`ios-sim/tests/settings.sh` was written this way, from the session that ran case S3 of `device-test-plan.md`. Its taps and the swipe come from the guard log. The checks come from its report ("Your settings", "Network & servers", "Talk to someone" again after the swipe). The fixed point it tapped became a lookup of the element in the tree.

On desktop and Android the Claude job starts at the app's first screen, not at the feature tests' starting point, so a session meant for this must first onboard and create the group, as `claude-test.md` asks.

## Composing tests here, without the app

- The app runs only on the runner. A test's flow and timing are proven there.
- Lookups can be checked here against a run's saved screens:
  - Android: `python3 scripts/ci/android/screen-tree.py <NN-step.xml> find "<text>"` (or `lines`, `tree`) answers as `ui find` does on the runner.
  - iOS: `NN-step.json` is AXe's tree; `jq` over it, or the filter in `scripts/ci/ios-sim/ui`'s `compact_tree`, shows the labels, values and frames `ui tree` prints.
  - Desktop: `NN-step.txt` has the phrases OCR read and their centres. Reading a screenshot again needs tesseract and ImageMagick, which the runner installs; OCR misreads show only there.
- So iOS and Android tests can be written fully by label from a run's screens. Desktop tests can be written from the phrases, with coordinates only for icons.
