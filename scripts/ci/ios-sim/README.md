# iOS app in the simulator on GitHub's runners

`.github/workflows/ios-sim.yml` builds the core library and the iOS app from the branch's code on a macOS runner, starts the app in an iPhone 17 simulator, and drives it through onboarding to the chat list with a scripted smoke test. On request it also runs a Claude session that follows a device test plan, and a test backend (relay, directory, names server, contacts) for tests that need servers and peers.

## Running it

The workflow runs only when started by hand, on any branch, from the Actions tab or with `gh workflow run`: [CI on GitHub's runners](../README.md) says how and lists the inputs.

Results: the job logs, the Claude job's summary (Claude's usage) and the artifacts `ios-sim-smoke-test` (a screenshot, a 1000-px view and an accessibility tree per step, the app log, the snapshots in `snapshots/`), `ios-sim-backend-logs` and `ios-sim-claude-test` (the Claude job's smoke test in `smoke-test/`, and per session: `prompt.md`, `report.md`, `shots/`, `transcript.jsonl`, `usage.json`). Read them with `gh run view <id> --log` and `gh run download <id> -n <artifact>`.

## How long it takes, what it costs

- `plan` (ubuntu-24.04, 10 s) reads what the run asks for, names the core library and the backend after their sources, `hashFiles` over the Haskell sources, `cabal.project`, `simplex-chat.cabal` and the build script, and reuses an artifact of the same name that an earlier run of the same branch uploaded (30 days). So every run builds the core library, and the backend when asked, from the branch's own Haskell code, and rebuilds only when those sources change.
- `core` (macOS, 30–45 min when not reused): `build-core.sh` builds `lib:simplex-chat` with cabal and the iOS flags, as the Nix iOS build does, into the package `scripts/ios/prepare.sh` expects, and `prepare-libs.sh` marks it for the simulator (`mac2ios`).
- `backend` (macOS, 20–45 min when not reused, only when asked): `build-backend.sh` builds `simplex-chat`, `simplex-directory-service` and `smp-server`.
- `app` (macOS): the Xcode build 8–22 min, the smoke test about 10 min, the snapshots about 18 min more, the backend start about 10 min when asked.
- `claude` (macOS, only when asked, after `app` passed): a new simulator with the app job's build and the smoke test, about 7 min, then the sessions (the smoke plan's took 2.6 min).

A run with reused builds and no extras takes about 25 minutes plus the wait for a macOS runner. Hosted minutes are free for this public repository; the limit is the repository's five concurrent macOS jobs, shared with PR builds (each `build.yml` run takes two), so queue waits can be long. The workflow saves no Actions caches, which this repository evicts within hours and shares with `build.yml`: builds are reused only as the artifacts above, and a core or backend rebuild compiles its dependencies too.

The macOS jobs run on `macos-26`, the image with Xcode 26 and the iPhone 17 simulator; `macos-latest` moves to each new macOS and its Xcode.

## The Claude session

It runs in its own job, `claude`, once the app job has passed: the job installs that job's build (the artifact `ios-sim-app`, kept 14 days) in a new simulator, runs the smoke test to reach the sessions' starting state, then one session per layout listed in `claude-scope`, with Claude Code installed from npm at the version the desktop workflow pins too. Only this job references the environment `app-sim-claude`, which holds the secret `CLAUDE_CODE_OAUTH_TOKEN` (a Claude subscription token) and admits the branches its rule allows; one runs per branch at a time. Only the Claude step gets the token, and `claude-test.sh` passes it to the `claude` processes alone. The job checks out without persisting its GitHub token and installs a pinned AXe (`install-axe.sh`), since it holds the Claude token. It starts the test backend when `claude-scope` says `backend: yes`; a run that asks for Claude must then ask for the backend too, or the plan job fails. On master it says `no`, since the smoke plan doesn't use the backend.

Each session's final message ends with `CLAUDE-TEST: PASS`, or `CLAUDE-TEST: FAIL` and the failed cases; the step fails unless every session passes.

- `claude-scope`: whether the cases use the test backend, the model, the layouts, the cases per layout, optional entries per layout, `max-turns` and `max-budget-usd` per session.
- `device-test-plan.md`: the plan (`| ID | Do | Expect |` tables). On master it is a three-case smoke plan; a feature branch replaces it with its own and names its cases in `claude-scope`.
- `claude-prompt.md`: what any session needs to know about the app, the `ui` helper and the test backend; `claude-test.sh` adds the `ui` help, the session's cases, layout, start state and limits, the seeded backend and the plan.
- `claude-guard.py`: a PreToolUse hook that lets through only one plain `ui` command per Bash call, since read-only commands run without asking in every permission mode and Bash inherits the token. Read, Write and Edit are allowed only in the session's own directory, so a session can't change `ui` or the scripts it runs. After each session `claude-test.sh` deletes the session's files, and fails, if the token appears in them or the check for it fails.
- `claude-usage.sh`: each session's turns, duration, tokens and cost "estimated at API prices" (Claude Code's own estimate, not a bill) to the job summary and `claude-usage.json`, or why they are unknown (a session cut short before its token check is not uploaded).

Haiku 5.5 drives the app well: the smoke plan takes a few dozen turns and cents; a 50-case feature plan took 300–450 turns, 30–60 minutes and $1.1–1.7 per session, estimated at API prices. Every run spends the subscription's usage limits, which interactive sessions share.

## Driving the app: `ui`

`ui` wraps AXe (`install-axe.sh`) for scripts and for Claude: tap by label, value or point, long-press, swipe, type, paste, keys, wait for text, find, screenshots with trees, switch the layout (one-hand UI or top bar), relaunch, text size, and the backend's directory (`ui` with no arguments lists them). It needs `SIM_UDID` and `UI_OUT`, which the smoke test exports to the job. Lessons kept in its comments and in the smoke test: text fields have no label (tap them by value), the keyboard capitalizes and autocorrects (compare typed text case-insensitively), the closed user picker is always in the tree, "Cancel" clears the search field and its "Close" image doesn't, the keyboard's keys are below the screen.

## Snapshots: `snapshots.sh`

At the end of the app job's smoke test, `smoke-test.sh` runs `snapshots.sh <derived data> <smoke test dir>/snapshots` when the app it tested lies in the derived data of a build (`…/Build/Products/Debug-iphonesimulator`); the Claude job, which installs the app from a tarball, skips it.

`snapshots.sh` builds the unit-test target `SimpleX Snapshots` (`apps/ios/SimpleX Snapshots/SnapshotTests.swift`, hosted by the app) with `xcodebuild build-for-testing`, then runs it in the smoke test's simulator three times, after `xcrun simctl ui <udid> content_size` `large`, `extra-extra-extra-large` and `accessibility-medium`, and sets `large` back. The test hosts each preview listed in `previewScenes()` (the `previews` of a `PreviewProvider`, in a `VStack`) in a `UIHostingController` in a window of the screen's width and of the larger of two heights, the preview's ideal height and its height in a frame of the screen's size, in the light and dark themes without a wallpaper, ends text editing and removes the running animations, so a text cursor or a spinner is drawn the same in every run, and draws it with `drawHierarchy` at scale 1, one pixel per point. The chat item previews (`allSizes`) are rendered at the three sizes, the other previews at `large` only.

`previewScenes()` lists every `PreviewProvider` in `apps/ios` except those in `EXCLUDED` of `apps/ios/SimpleX Snapshots/check-previews.py`, whose renders differ between runs and can be made identical only by a change of the app's code. The iOS job of `build.yml` runs `check-previews.py`, which fails when a `PreviewProvider` is neither listed nor excluded, or when a file has a `#Preview`, since the test renders previews by their type.

The test runs in UTC (`NSTimeZone.default`) and en_US (`-testLanguage en -testRegion US`), so the renders do not depend on the runner's time zone or locale; a preview that shows the current date or time differs between runs, so the previews and the sample contact use `previewDate` (`SimpleXChat/ChatTypes.swift`) in place of the current time. Previews of QR codes draw the image `preview-qr-code` of `apps/ios/Shared/Preview Content`, a development asset catalog of the app target, in place of a generated code. The keyboard autocorrects the profile name typed in the smoke test differently between runs, so the test sets the app's current user to the sample user.

The images go to `snapshots/` of the artifact `ios-sim-smoke-test` as 8-bit sRGB PNG files without alpha (`preferredRange = .standard`, `opaque`), named `<preview>-<light|dark>-<size>.png`, with `index-<size>.json` listing each file's scene, theme, size, pixel width and height, scale and the content size category the app saw.

When `apps/ios/SimpleX Snapshots/References` exists, `snapshots.sh` then runs `scripts/ci/snapshots-compare.py <references> <renders> <diff dir>`. It compares each render listed in `index-*.json` with the reference of the same name: a render passes when its file is identical to the reference, or when no channel of any pixel differs from it by more than 8 of 255. A missing or extra render, a changed size or a pixel beyond the tolerance fails: the script writes `compare.json` (each file's result and the failed files) next to the renders and, per failed file, `snapshots-diff/<file>` with the reference, the render and the changed pixels in red side by side, lists the failed files in the job summary and exits with 1, which fails the smoke test step.

`scripts/ci/snapshots-remote.py ios record|check [branch]` renders the snapshots of a pushed branch (the current one by default) on GitHub's runner: it starts `ios-sim.yml` on the branch, waits for the run and downloads `snapshots/` and `snapshots-diff/` of its `ios-sim-smoke-test` to a temporary folder. `record` replaces the PNG files in `apps/ios/SimpleX Snapshots/References` with the run's renders and prints the folder's `git status`, for review and commit; `check` prints the files that failed the run's comparison and the folder of their diff images, and exits with 1 when any failed. It takes the token from `GITHUB_TOKEN` or `GH_TOKEN`, otherwise from `gh auth token`; the token must be allowed to start workflows.

## The test backend

`backend.sh start <backend zip> <work dir>` (with the `backend` input) starts, on the runner, against the preset SMP and XFTP servers:

- a chat relay for channels, and a SimpleX Directory whose address it writes to `<work dir>/directory-link`;
- a CLI peer `ci_admin` that creates, registers and gets approved groups and channels with the profile JSON the app sends: `ci_group` and `ci_channel` ("ci seed"), `ci_cards`, six entries with app-size avatars made by `avatar.swift` ("ci avatar"), a long name, markdown in a description, and `cinews`, `cinews_digest`, `cinews_weekly`;
- a local SMP server with a `[NAMES]` section and `names-resolver.py`, the names `cinews.testing`, `ciboth.testing`, `cialice.testing` and `cibobby.testing` claimed by their owners, and the contacts cialice and Robert, online and accepting connections; its address goes to `<work dir>/names-server`;
- checks before the app starts: a search sent to the directory as a message must list `ci_group` and `ci_channel`, and `/_connect plan` must resolve each name from a fresh peer.

On master's code it starts and passes these checks (the directory answers searches sent as messages); service requests to the directory, which only `f/directory-search` answers, are not checked here.

It writes what it seeded to `<work dir>/seed.md` (the Claude sessions get it) and logs to `<work dir>/*.log`; `stop`, `directory-stop`, `directory-start` and `listings` are its other commands. The app reaches it through the simulator, which shares the runner's network; pointing the app at the names server is a few steps in Settings, described in `claude-prompt.md`.

## Other files

- `use-macos26-sdk.sh`: runner images from 20261013 add Command Line Tools with a macOS 27 SDK that Xcode 26.6's linker can't read; every macOS job points the default SDK back at macOS 26.5. The pinned `macos-26` label runs such an image.
- `earlier-run.sh <artifact name>`: finds the newest run of this branch in this repository with that artifact; artifacts from forks and other branches are never used.
