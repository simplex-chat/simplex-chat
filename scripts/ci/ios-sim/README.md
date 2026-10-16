# iOS app in the simulator on GitHub's runners

`.github/workflows/ios-sim.yml` builds the core library and the iOS app from the branch's code on a macOS runner, starts the app in an iPhone 17 simulator, and drives it through onboarding to the chat list with a scripted smoke test. On request it also runs a Claude session that follows a device test plan, and a test backend (relay, directory, names server, contacts) for tests that need servers and peers.

## Running it

The workflow runs only when asked, on any branch:

- **Dispatch** it from the Actions tab (workflow "ios-sim", "Run workflow"), with the inputs `claude` (also run the Claude session) and `backend` (also start the test backend), both off by default.
- **Push** a commit whose message carries a tag: `[app-ci]` runs the scripted tests of both apps (iOS and desktop), `[app-ci:ios]` only this one; `[claude-test]` (both apps) or `[claude-test:ios]` runs the scripted test and then the Claude session; add `[ci-backend]` to build and start the test backend. Only the pushed head commit counts, and a tag matches anywhere in its message, body included, so don't quote a tag in a message that shouldn't run it.

A push without a tag starts nothing and doesn't cancel a running test; a newer requested run on the same branch cancels the older one. Pushes to master and stable never start it, because a squash merge's message lists the merged commits' subjects with their tags; dispatch it there instead. Which branches may run the Claude session is decided only by the `app-sim-claude` environment's branch rule in the repository settings: on a branch it doesn't allow, a run that asks for Claude fails at the environment check, and because the app job holds the environment, its scripted test fails with it. Ask for Claude only on branches the rule allows.

Results: the job log, the job summary (Claude's usage) and the artifacts `ios-sim-smoke-test` (a screenshot, a 1000-px view and an accessibility tree per step, the app log), `ios-sim-backend-logs` and `ios-sim-claude-test` (per session: `prompt.md`, `report.md`, `shots/`, `transcript.jsonl`, `usage.json`). Read them with `gh run view <id> --log` and `gh run download <id> -n <artifact>`.

## How long it takes, what it costs

- `plan` (ubuntu, 15 s) names the core library and the backend after their sources, `hashFiles` over the Haskell sources, `cabal.project`, `simplex-chat.cabal` and the build script, and reuses an artifact of the same name that an earlier run of the same branch uploaded (30 days). So every run builds the core library, and the backend when asked, from the branch's own Haskell code, and rebuilds only when those sources change.
- `core` (macOS, 35–45 min when not reused): `build-core.sh` builds `lib:simplex-chat` with cabal and the iOS flags, as the Nix iOS build does, into the package `scripts/ios/prepare.sh` expects, and `prepare-libs.sh` marks it for the simulator (`mac2ios`).
- `backend` (macOS, 20–30 min when not reused, only when asked): `build-backend.sh` builds `simplex-chat`, `simplex-directory-service` and `smp-server`.
- `app` (macOS): the Xcode build 8–22 min, the smoke test about 10 min, the backend start about 10 min when asked, Claude as long as its plan needs.

A run with reused builds and no extras takes about 26 minutes plus the wait for a macOS runner. Hosted minutes are free for this public repository; the limit is the repository's five concurrent macOS jobs, shared with PR builds (each `build.yml` run takes two), so queue waits can be long. Caches are evicted within hours (the repository is over its cache limit); the artifacts are what make runs fast.

## The Claude session

It runs after the smoke test, in the same job, on the simulator the smoke test leaves running, once per layout listed in `claude-scope`, with Claude Code installed from npm at a pinned version. The job then references the environment `app-sim-claude`, which holds the secret `CLAUDE_CODE_OAUTH_TOKEN` (a Claude subscription token) and admits the branches its rule allows; jobs without Claude reference no environment. Only the Claude step gets the token, and `claude-test.sh` passes it to the `claude` processes alone. The job checks out without persisting its GitHub token and installs a pinned AXe (`install-axe.sh`), since it later holds the Claude token.

- `claude-scope`: the model, the layouts, the cases per layout, optional entries per layout, `max-turns` and `max-budget-usd` per session.
- `device-test-plan.md`: the plan (`| ID | Do | Expect |` tables). On master it is a three-case smoke plan; a feature branch replaces it with its own and names its cases in `claude-scope`.
- `claude-prompt.md`: what any session needs to know about the app, the `ui` helper and the test backend; `claude-test.sh` adds the `ui` help, the session's cases, layout, start state and limits, the seeded backend and the plan.
- `claude-guard.py`: a PreToolUse hook that lets through only one plain `ui` command per Bash call, since read-only commands run without asking in every permission mode and Bash inherits the token. Read, Write and Edit are allowed only in the session's own directory, so a session can't change `ui` or the scripts it runs. After each session `claude-test.sh` deletes the session's files if the token appears in them.
- `claude-usage.sh`: each session's turns, duration, tokens and cost "estimated at API prices" (Claude Code's own estimate, not a bill) to the job summary and `claude-usage.json`.

Haiku 5.5 drives the app well: the smoke plan takes a few dozen turns and cents; a 50-case feature plan took 300–450 turns, 30–60 minutes and $1.1–1.7 per session, estimated at API prices. Every run spends the subscription's usage limits, which interactive sessions share.

## Driving the app: `ui`

`ui` wraps AXe (`install-axe.sh`) for scripts and for Claude: tap by label, value or point, swipe, type, paste, keys, wait for text, find, screenshots with trees, switch the layout (one-hand UI or top bar), relaunch, text size, and the backend's directory (`ui` with no arguments lists them). It needs `SIM_UDID` and `UI_OUT`, which the smoke test exports to the job. Lessons kept in its comments and in the smoke test: text fields have no label (tap them by value), the keyboard capitalizes and autocorrects (compare typed text case-insensitively), the closed user picker is always in the tree, "Cancel" clears the search field and its "Close" image doesn't, the keyboard's keys are below the screen.

## The test backend

`backend.sh start <backend zip> <work dir>` (with `[ci-backend]` or the `backend` input) starts, on the runner, against the preset SMP and XFTP servers:

- a chat relay for channels, and a SimpleX Directory whose address it writes to `<work dir>/directory-link`;
- a CLI peer `ci_admin` that creates, registers and gets approved groups and channels with the profile JSON the app sends: `ci_group` and `ci_channel` ("ci seed"), `ci_cards`, six entries with app-size avatars made by `avatar.swift` ("ci avatar"), a long name, markdown in a description, and `cinews`, `cinews_digest`, `cinews_weekly`;
- a local SMP server with a `[NAMES]` section and `names-resolver.py`, the names `cinews.testing`, `ciboth.testing`, `cialice.testing` and `cibobby.testing` claimed by their owners, and the contacts cialice and Robert, online and accepting connections; its address goes to `<work dir>/names-server`;
- checks before the app starts: a search sent to the directory as a message must list `ci_group` and `ci_channel`, and `/_connect plan` must resolve each name from a fresh peer.

Not yet verified on master: the backend ran on `f/directory-search`'s code, and its checks were rewritten for master's (the directory answers searches sent as messages there; service requests need the feature branch).

It writes what it seeded to `<work dir>/seed.md` (the Claude sessions get it) and logs to `<work dir>/*.log`; `stop`, `directory-stop`, `directory-start` and `listings` are its other commands. The app reaches it through the simulator, which shares the runner's network; pointing the app at the names server is a few steps in Settings, described in `claude-prompt.md`.

## Other files

- `use-macos26-sdk.sh`: runner images from 20261013 add Command Line Tools with a macOS 27 SDK that Xcode 26.6's linker can't read; every macOS job points the default SDK back at macOS 26.5.
- `earlier-run.sh <artifact name>`: finds the newest run of this branch in this repository with that artifact; artifacts from forks and other branches are never used.
