# Desktop app CI on Linux

`.github/workflows/desktop-linux.yml` builds the desktop app from the branch on a GitHub runner (Ubuntu 24.04), runs its JVM tests (`:common:desktopTest`), and drives the app on a virtual display: onboarding to the chat list, then the chat list with the toolbar at the bottom (the default) and at the top. On request it also runs Claude Code driving the same path, a test backend, and the latest release as a baseline.

## Running it

The workflow runs only when started by hand, on any branch, from the Actions tab or with `gh workflow run`: [CI on GitHub's runners](../README.md) says how and lists the inputs.

Every run builds the core library, and the backend when it's asked for, from the branch's own Haskell code. It reuses an earlier build only when the sources are unchanged, as an artifact of the same branch named after a hash of its sources (30 days): for the core `src/**`, `cabal.project`, `simplex-chat.cabal`, `PRIVACY.md` and the build scripts, for the backend `src/**`, the CLI's and the directory's sources, `cabal.project`, `simplex-chat.cabal` and its build script. So a branch that changes core and app code together tests both. The workflow saves no Actions caches, which this repository evicts within hours and shares with `build.yml`.

## Jobs

| Job | Does | Time |
|---|---|---|
| `plan` | chooses the jobs, finds earlier core and backend builds | 10 s |
| `core` | `scripts/desktop/build-lib-linux.sh` via `build-core.sh` | skipped when reused, about 24 min not |
| `app` | the app (`:desktop:createDistributable`), desktopTest, `smoke-test.sh` | about 12 min: build 4.5, desktopTest 0.5, smoke test 3.5 |
| `claude` (opt-in) | after `app` passed: its distributable at the first screen, then Claude Code drives it | about 4 min, Claude 3 |
| `backend` (opt-in) | `build-backend.sh`: `simplex-chat`, `simplex-directory-service`, and `smp-server` from the simplexmq commit `cabal.project` pins | about 30 min, skipped when reused |
| `release` (opt-in) | the latest release's .deb for this Ubuntu, onboarding only, to tell a branch's failure from the runner's | 1.5 min |

Queueing for a runner adds from under a minute to 20 minutes. Results are artifacts: `desktop-smoke-test` (numbered screenshots, the text OCR read from each, `steps.log`, `checks.txt`, `app.log`), `desktop-test-report`, `desktop-claude-test`, and the backend's logs; `desktop-app`, the distributable for manual tests and the Claude job, is kept 14 days. The job summaries list the checks, the desktopTest results and the Claude usage.

## Scripts

- `ui`: the UI driver. Compose for Desktop has no accessibility tree on Linux, so it finds text on screenshots by OCR (tesseract) and acts with xdotool on an Xvfb display. `ui` alone prints its commands: start (with app settings), shot, text, find, wait, click, click-text, scroll, type, key, avatar (a profile image's colour), ink (a text's height), do (several in a row), stop. `screen-text.py`, `screen-colour.py` and `screen-ink.py` are its helpers.
- `smoke-test.sh <app dir> <out dir> [start|onboarding|setup|backend]`: the scripted cases; the default is onboarding and both layouts. `setup` also creates a local group and makes the test SMP server the app's only names server; `backend` does that, connects to the seeded name, and checks both layouts.
- `backend.sh start <tar.gz> <dir>` / `stop <dir>`: the test backend, below.
- `claude-test.sh`, `claude-test.md` (the prompt), `claude-scope` (whether the prompt uses the test backend), `claude-guard.py`, `claude-usage.py`: the Claude job.
- `build-core.sh`, `install-core.sh`, `build-backend.sh`, `earlier-run.sh`, `release-app.sh`, `test-summary.py`: builds, reuse and reports.

## The test backend

`backend.sh` starts, on the runner, with CLI peers:

- a chat relay (`--relay --headless`) and a directory service on the preset servers;
- a group `ci_group` and a channel `ci_channel` with letter avatars as large as the apps make them (`avatar`), registered and approved by the admin `ci_admin`, and found by a new user who sends the directory "ci seed";
- an SMP server on 127.0.0.1:5223 whose `[NAMES]` section asks `names-resolver.py`, and a contact `ci_contact` whose address it resolves as `seeded.testing`, checked with a connect plan from another client.

It writes the directory's address to `<dir>/directory-link` and the SMP server's to `<dir>/names-server`. The app reaches the names server only with private routing set to unknown servers (`ui start … NetworkSMPProxyMode=Unknown`), since the preset proxies can't reach 127.0.0.1, and with the test server's receiving role off: a directory can't answer requests whose reply queue is on it.

## The Claude job

It needs the GitHub environment `app-sim-claude` with the secret `CLAUDE_CODE_OAUTH_TOKEN`, a Claude subscription token. Which branches may use it is decided only by that environment's branch rule in the repository settings: on a branch the rule doesn't allow, a requested Claude run fails at the environment check. It runs once the app job has passed, on that job's distributable, one per branch at a time. The token goes only to the step that runs `claude`; `claude-test.sh` passes it to the `claude` process alone, and every `ui` call unsets it too. The job starts the test backend when `claude-scope` says `backend: yes`; a run that asks for Claude must then ask for the backend too, or the plan job fails. On master it says `no`, since this prompt doesn't use the backend. The job installs Claude Code at the version the iOS workflow pins too, and runs `claude-haiku-5-5` with `--max-turns 60`, `--max-budget-usd 3` and the iOS sessions' other flags (`--restricted`, `--permission-prompts none`, `--effort high`). Its Bash runs only single `ui` commands (`claude-guard.py`, a PreToolUse hook: Claude Code runs read-only commands without asking in every permission mode). Before anything the session wrote is printed or uploaded, `claude-test.sh` checks it for the token; if it's there, the output, transcript, Claude Code's stderr and the guard log are deleted and the job fails. It passes when Claude's final message ends with `CLAUDE-TEST: PASS`; `report.md`, the transcript, the guard's log and `claude-usage.json` are in `desktop-claude-test`.

## Cost

The runners are free for this public repository. A Claude run spends the subscription's usage limits, which interactive sessions share: Haiku 5.5 does this smoke path in 15 to 30 turns, 0.01 to 0.02 USD estimated at API prices (Claude Code's estimate, not a bill on a subscription).
