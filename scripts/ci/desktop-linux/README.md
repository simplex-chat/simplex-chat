# Desktop app CI on Linux

`.github/workflows/desktop-linux.yml` builds the desktop app from the branch on a GitHub runner (Ubuntu 24.04), runs its JVM tests (`:common:desktopTest`), and drives the app on a virtual display: onboarding to the chat list, then the chat list with the toolbar at the bottom (the default) and at the top. On request it also runs Claude Code driving the same path, a test backend, and the latest release as a baseline. With the test backend, both the scripted test and Claude also search the test directory and a SimpleX name in both layouts.

## Running it

On any branch, in either of two ways:

- **Dispatch:** Actions → desktop-linux → Run workflow, with the `claude` and `backend` checkboxes (both off by default).
- **Push** with a tag in the head commit's message, which is how agents in push sessions ask for a run:

| Tag | Runs |
|---|---|
| `[app-ci]` | the scripted test, here and in the iOS app's workflow where the branch has it |
| `[app-ci:desktop]` | the scripted test, here only |
| `[claude-test]` | the scripted test and the Claude job, here and in the iOS app's workflow where the branch has it |
| `[claude-test:desktop]` | the scripted test and the Claude job, here only |
| `[ci-backend]` | with one of the above: also the test backend and the release job |

A push without these tags (or with only iOS tags) starts a run whose jobs are all skipped, and it doesn't cancel a run in progress. A newer run that was asked for cancels an older one on the same branch. Pushes to master and stable never start it, because a squash merge's message lists the merged commits' subjects with their tags; dispatch it there instead.

Every run builds the core library, and the backend when it's asked for, from the branch's own Haskell code. It reuses an earlier build only when the sources are unchanged: the core by a cache keyed on `src/**`, `cabal.project`, `simplex-chat.cabal`, `PRIVACY.md` and the build scripts; the backend by an artifact of the same branch named after a hash of its sources (30 days). So a branch that changes core and app code together tests both.

## Jobs

| Job | Does | Time |
|---|---|---|
| `plan` | chooses the jobs, finds an earlier backend build | 10 s |
| `core` | `scripts/desktop/build-lib-linux.sh` via `build-core.sh` | 15 s cached, about 23 min not |
| `app` | the app (`:desktop:createDistributable`), desktopTest, `smoke-test.sh` | about 10 min: build 4.5, desktopTest 0.5, smoke test 3.5 (about 25 with the backend's searches) |
| `claude` (opt-in) | the app at its first screen, or with the backend set up at the chat list, then Claude Code drives it | about 7 min, Claude 2.5 |
| `backend` (opt-in) | `build-backend.sh`: `simplex-chat`, `simplex-directory-service`, and `smp-server` from the simplexmq commit `cabal.project` pins | about 27 min, skipped when reused |
| `release` (opt-in) | the latest release's .deb for this Ubuntu, onboarding only, to tell a branch's failure from the runner's | 1.5 min |

Queueing for a runner adds from under a minute to 20 minutes. Results are artifacts: `desktop-smoke-test` (numbered screenshots, the text OCR read from each, `steps.log`, `checks.txt`, `app.log`), `desktop-test-report`, `desktop-claude-test`, and the backend's logs. The job summaries list the checks, the desktopTest results and the Claude usage.

## Scripts

- `ui`: the UI driver. Compose for Desktop has no accessibility tree on Linux, so it finds text on screenshots by OCR (tesseract) and acts with xdotool on an Xvfb display. `ui` alone prints its commands: start (with app settings), shot, text, find, wait, click, click-text, scroll, type, key, avatar (a profile image's colour), ink (a text's height), do (several in a row), stop. `screen-text.py`, `screen-colour.py` and `screen-ink.py` are its helpers.
- `smoke-test.sh <app dir> <out dir> [start|onboarding|setup|backend|search]`: the scripted cases; the default is onboarding and both layouts. `setup` also creates a local group and makes the test SMP server the app's only names server; `backend` does that, connects to the seeded name, and checks both layouts; `search`, which the workflow runs when the backend started, also searches in both layouts: "Search in Directory" for "ci seed", and the full search for the name `seeded` with Enter (the local chat, the Names part, the Directory part, in order), and with the toolbar at the bottom the full search in the New chat sheet, checking headers, rows, avatars, counts and the cards hidden while results show. It then joins `ci_channel` from its Directory row and checks, in both layouts, the note "All results on this page are already in your chats" when the typed `ci_channel` finds only the listed chat, and, with "seeded notes" open and `ci seed` typed, `ci_channel` as its chat's row in the Directory part (OCR reads no `#` before its name and no "Join channel" next to it; `#ci_group` keeps "Join group") that opens the channel without an alert; and the same row in the New chat sheet. The search case makes 129 checks (106 before the known-chat ones).
- `backend.sh start <tar.gz> <dir>` / `stop <dir>`: the test backend, below.
- `claude-test.sh`, `claude-test.md` (the prompt), `claude-search.md` (the prompt when the backend runs), `claude-guard.py`, `claude-usage.py`: the Claude job.
- `build-core.sh`, `install-core.sh`, `build-backend.sh`, `earlier-run.sh`, `release-app.sh`, `test-summary.py`: builds, reuse and reports.

## The test backend

`backend.sh` starts, on the runner, with CLI peers:

- a chat relay (`--relay --headless`) and a directory service on the preset servers;
- a group `ci_group` and a channel `ci_channel` with letter avatars as large as the apps make them (`avatar`), registered and approved by the admin `ci_admin`, and found by a new user who sends the directory "ci seed", and by a search request as the app sends it (`/_service_request`), which must page through them, as their images don't fit in one response;
- an SMP server on 127.0.0.1:5223 whose `[NAMES]` section asks `names-resolver.py`, and a contact `ci_contact` whose address it resolves as `seeded.testing`, checked with a connect plan from another client.

It writes the directory's address to `<dir>/directory-link` and the SMP server's to `<dir>/names-server`. The app reaches the names server only with private routing set to unknown servers (`ui start … NetworkSMPProxyMode=Unknown`), since the preset proxies can't reach 127.0.0.1, and with the test server's receiving role off: a directory can't answer requests whose reply queue is on it. The workflow points the app at the test directory before building it (`DIRECTORY_SERVICE_LINK` in `DirectorySearch.kt`, the only change it makes to app sources).

## The Claude job

It needs the GitHub environment `app-sim-claude` with the secret `CLAUDE_CODE_OAUTH_TOKEN`, a Claude subscription token. Which branches may use it is decided only by that environment's branch rule in the repository settings: on a branch the rule doesn't allow, a requested Claude run fails at the environment check. The token goes only to the step that runs `claude`, and every `ui` call unsets it. With `[ci-backend]` the job also starts the test backend and waits for its build, sets the app up at the chat list (`smoke-test.sh … setup`) and gives Claude `claude-search.md`, the online search test, in place of the onboarding path in `claude-test.md`: the directory and full searches in both layouts (parts A and B), then part C, which joins `ci_channel` from its Directory row and checks that later searches show it as a chat (the note for a page of chats already listed, its chat row in the Directory part of the chat list and of New chat, opening without an alert). The job installs Claude Code 2.1.296 and runs `claude-haiku-5-5` with `--max-turns 120` and `--max-budget-usd 3`. Its Bash runs only single `ui` commands (`claude-guard.py`, a PreToolUse hook: Claude Code runs read-only commands without asking in every permission mode). Before anything the session wrote is printed or uploaded, `claude-test.sh` checks it for the token; if it's there, the output, transcript, Claude Code's stderr and the guard log are deleted and the job fails. It passes when Claude's final message ends with `CLAUDE-TEST: PASS`; `report.md`, the transcript, the guard's log and `claude-usage.json` are in `desktop-claude-test`.

## Cost

The runners are free for this public repository. A Claude run spends the subscription's usage limits, which interactive sessions share: Haiku 5.5 does this smoke path in 15 to 17 turns, about 0.008 USD estimated at API prices (Claude Code's estimate, not a bill on a subscription).
