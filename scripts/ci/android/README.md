# Android app CI in an emulator

`.github/workflows/android-emu.yml` builds the core library and a debug APK from the branch on a GitHub runner (Ubuntu 24.04), installs the app in an Android emulator, and drives it: onboarding to the chat list, then the chat list with the reachable toolbar (the default) and with the top bar. On request it also runs Claude Code driving the same path, a test backend, and the latest release as a baseline.

## Running it

The workflow runs only when started by hand, on any branch: in the Actions tab, `android-emu` → Run workflow, or `gh workflow run android-emu.yml --ref <branch>` with `-f claude=true`, `-f backend=true` or `-f release=true`. [CI on GitHub's runners](../README.md) lists the inputs. A newer run on the same branch cancels the older one.

Every run builds the core library, and the backend when it's asked for, from the branch's own Haskell code. It reuses an earlier build only when the sources are unchanged, as an artifact of the same branch named after a hash of its sources (30 days): for the core `src/**`, `cabal.project`, `simplex-chat.cabal`, `PRIVACY.md`, the Nix files (`flake.nix`, `flake.lock`, `scripts/nix/**`) and `build-core.sh`; the backend is desktop-linux's, under its name and hash, so a backend either workflow built is reused by both. The workflow saves no Actions caches, which this repository evicts within hours and shares with `build.yml`.

## Jobs

| Job | Does | Time |
|---|---|---|
| `plan` | reads the inputs, finds earlier core and backend builds | 10 s |
| `core` | `build-core.sh`: `lib:simplex-chat` and `lib:support` for aarch64-android with Nix, as `scripts/android/build-android.sh` builds them | skipped when reused, about 27 min not |
| `app` | the debug APK (`:android:assembleFossDebug` for arm64-v8a only), the emulator, `smoke-test.sh` | about 13 min: APK 7–8, emulator 1.5, smoke test 3.5 (4 with the backend case, after 2.5 to start the backend) |
| `claude` (opt-in) | after `app` passed: its APK in a new emulator at the first screen, then Claude Code drives it | about 5 min, Claude 2.5 |
| `backend` (opt-in) | desktop-linux's `build-backend.sh` | about 30 min, skipped when reused |
| `release` (opt-in) | the latest release's arm64 APK, its signer checked against SimpleX's key, onboarding only | about 4 min |

Results are artifacts: `android-smoke-test` (numbered screenshots, the accessibility tree and the text of each, `steps.log`, `checks.txt`, logcat as `app.log`, `emulator.log`), `android-release-smoke-test`, `android-claude-test`, and the backend's logs; `android-app`, the APK for manual tests and the Claude job, is kept 14 days. The job summaries list the checks and the Claude usage.

## The emulator

A Google APIs x86_64 system image of API 34 (revision 14) with KVM, on the x86_64 runner: the arm64 runner has no KVM, and Google publishes no Linux arm64 emulator. The image's ARM translation (`libndk_translation`) runs the app's arm64-v8a code, the only ABI besides armeabi-v7a the core is built for. `install-sdk.sh` installs the emulator, platform-tools and the image at pinned revisions from Google's repository, checking each archive's SHA-1 and SHA-256, and writes the AVD itself; `emulator.sh` boots it without a window. Screenshots come from the emulator console (`adb emu screenrecord screenshot`): without a window, the guest's `screencap` returns an empty or black image.

## Scripts

- `ui`: the UI driver, with desktop's command names: text and positions from uiautomator's accessibility tree, taps and keys with adb. `ui` alone prints its commands: start (with app settings), shot, text, tree, find, wait, click, click-text, scroll, type, key, layout (the `OneHandUI` preference and a restart), relaunch, text-size, do, stop. `screen-tree.py` reads the tree; `prefs.py` writes the settings, which only a debug build accepts (`run-as`).
- `smoke-test.sh <apk> <out dir> [start|onboarding|layouts|backend]`: the scripted cases; the default is onboarding, a local group and both layouts. Onboarding on Android includes the app's instant notifications alert and the system's battery dialog. `backend` also adds the test SMP server under Your servers and checks that it passes Test servers.
- `install-sdk.sh`, `emulator.sh`: the emulator, above.
- `build-core.sh`, `install-core.sh`, `release-apk.sh`: builds and the release.
- `claude-test.md` (the prompt) and `claude-scope` (whether it uses the test backend): the Claude job, which runs desktop-linux's `claude-test.sh`, `claude-guard.py` and `claude-usage.py`.

## The test backend

Desktop-linux's `backend.sh` starts it on the runner as it does for desktop: [its README](../desktop-linux/README.md#the-test-backend) describes what it seeds. Its SMP server listens on the runner's 127.0.0.1:5223, and its addresses, the links peers create on it included, have 127.0.0.1, which in the emulator is the emulator itself: `emulator.sh reverse 5223` forwards the emulator's 127.0.0.1:5223 to the runner's (`adb reverse`) and waits until the server answers. The emulator's alias for the runner, 10.0.2.2, was unreachable on this image, and couldn't cover the addresses in links anyway. The app gets private routing to unknown servers only (`NetworkSMPProxyMode=Unknown`), since the preset proxies can't reach 127.0.0.1. The scripted backend case checks that the app reaches the server; the directory and SimpleX names aren't checked here.

## The Claude job

It needs the GitHub environment `app-sim-claude` with the secret `CLAUDE_CODE_OAUTH_TOKEN`, a Claude subscription token. Which branches may use it is decided only by that environment's branch rule in the repository settings: on a branch the rule doesn't allow, a requested Claude run fails at the environment check. It runs once the app job has passed, on that job's APK in a new emulator, one per branch at a time. The token goes only to the step that runs `claude`; `claude-test.sh` passes it to the `claude` process alone, and every `ui` call unsets it too; it never reaches the emulator. The job starts the test backend when `claude-scope` says `backend: yes`; a run that asks for Claude must then ask for the backend too, or the plan job fails. On master it says `no`, since this prompt doesn't use the backend. The job installs Claude Code at the version the other app workflows pin, and runs `claude-haiku-5-5` with desktop's harness: `--max-turns 60`, `--max-budget-usd 3`, Bash limited to single `ui` commands by `claude-guard.py`, file tools limited to the session's directory, and its output checked for the token before anything is printed or uploaded. It passes when Claude's final message ends with `CLAUDE-TEST: PASS`.

## Known limits

- **The app runs under ARM translation**, Google's closed-source `libndk_translation`, not natively: it's slower than on a phone and can hit bugs that only translation has. In one of the debug build's first ten starts here, the app died at start with a SIGSEGV inside the translator (`ndk_translation_HandleNoExec`); the smoke test then fails at once and prints the crash. The slowed app also dropped the end of a long text typed at once, so `ui type` types a few characters at a time and checks each part on the field. Before calling a failure an app bug, run the workflow with `release` and check the released app in the same emulator.
- **The core build relies on cache.zw3rk.com**, the zw3rk Hydra's cache, for the cross-compiling toolchain and the dependencies: with it only simplex-chat itself is compiled. A change of the flake's inputs (`flake.lock`, the haskell.nix pin, the index state) that the cache doesn't hold yet, or the cache being down, turns the build into compiling GHC and every dependency, 12 to 24 hours, longer than a job may run. The core job then times out; a branch whose Haskell and Nix sources are unchanged keeps reusing its last build for 30 days.
- On a first start the app can show its What's new sheet over the first onboarding screen; the smoke test closes it and notes it in `checks.txt`.

## Cost

The runners are free for this public repository. A Claude run spends the subscription's usage limits, which interactive sessions share: Haiku 5.5 did this smoke path in 24 turns, 0.01 USD estimated at API prices (Claude Code's estimate, not a bill on a subscription).
