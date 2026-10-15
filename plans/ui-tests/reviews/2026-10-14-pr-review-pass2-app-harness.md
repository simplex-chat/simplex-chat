# PR review pass 2 (A): UI test harness (uncommitted, /home/user/wt-uitest)

Scope: everything in `git diff` plus the untracked files. The review was read-only: no builds, no tests, no agents. Paths are relative to `apps/multiplatform/` unless they start with `scripts/`, `tests/` or `/`.

## 1. App code with hooks disabled (release builds)

### Checked, no behavior change found
- `Modifier.ui(key)` (common/src/commonMain/kotlin/chat/simplex/common/ui/Ui.kt:18) returns `this` when hooks are off. So no `.ui(...)` call can change a modifier chain: not as the first element, not in the middle (for example ChatListView.kt `.ui(...)` before `.clip/.clickable`), and not at the end. In AlertManager.kt:420, `Modifier.ui(..).then(x)` is equal to `x`.
- Every new `modifier =` argument replaces a parameter whose default is `Modifier`. That covers DropdownMenuItem in all five ItemAction overloads, ExposedDropdownMenuBox, the DropdownMenuItem options, DefaultSwitch (DefaultSwitch.kt:17), Slider, OperatorCheckView TextButton, IconButton (UserProfileButton, Stopped, UnreadFilter, BarMenu), the desktop `OnboardingActionButton(modifier = Modifier, ...)` (SimpleXInfo.kt:194) and `SimpleButtonFrame(click, modifier = Modifier, ...)` (SimpleButton.kt:92). No padding, size or click target changes.
- Signature changes still resolve to the same functions:
  - `SimpleButton`: calls that pass `disabled =` still go to the `disabled` overload, and the others go to the uiKey overload.
  - `UserProfileButton`: ShareListView.kt:109 uses a trailing lambda, which binds to `onButtonClicked`.
  - `SelectItemAction`: GroupChatInfoView.kt:1213 now uses a named `selectItem =`.
  - `ContactRequestMenuItems`: the new `chatId` parameter is last and optional, and ContactListNavView passes `onSuccess` by name.
  - `TextButtonBelowOnboardingButton`, `NewChatAction` (it replaces `Triple` one-to-one) and `ProfileNameField`: no change.
  - `expect ChatListNavLinkLayout(uiKey = null)` and both `actual`s are consistent.
- Hot paths add only trivial work when hooks are off:
  - `sendCmd` (SimpleXAPI.kt:940): one lambda allocation, two `is` checks and one volatile read.
  - Event loop (SimpleXAPI.kt:809): one lambda and one volatile read.
  - `withApi` / `withBGApi` / `withLongRunningApi` (Utils.kt:37-57): one lambda allocation and one volatile read.
  - Exceptions and cancellation propagate as before.
- `idle = false` on ChatModel.kt:827, ProcessedErrors.kt:19 and AppUpdater.kt:171 changes nothing for users.
- Android `initUiTags` (UiTags.android.kt:7) enables hooks only for `FLAG_DEBUGGABLE` builds. On desktop the flag is only set by `UiTestApp.bootJvm`.

### A1. `UiKey` uses identity equality, so composables that get a freshly built key can no longer skip, in release builds too (medium; mechanism high confidence, user-visible cost PLAUSIBLE)
- Ui.kt:7-8: `@Immutable class UiKey(val id: String, val arg: Any?)` has no `equals`. Kotlin is 2.1.20 (gradle.properties:33), where strong skipping is on by default. Parameters of a stable type are compared with `equals()`, which here means identity. Factory keys such as `Ui.ChatList.Row(...)` create a new instance on every recomposition, and they are built even when hooks are off.
- Affected call sites, where the key is a composable parameter (not a Modifier):
  - ChatListNavLinkView.kt:85/106/127/147/168/185 → `ChatListNavLinkLayout(uiKey = ...)`
  - ShareListNavLinkView.kt:43/61/69
  - GroupChatInfoView.kt:799 (member rows)
  - AddGroupMembersView.kt:355
  - UserPicker.kt:214
- Failure scenario: `chatModel.deletedChats` or `chatRunning` changes, or any other state that `ChatListNavLinkView` reads (ChatListNavLinkView.kt:43,50). Every visible row's `ChatListNavLinkView` then recomposes. Before this PR, `ChatListNavLinkLayout` could skip: all its parameters are stable, and strong skipping memoizes the lambdas. Now it always runs, for every row, in release builds.
- There is also a per-recomposition string allocation for every reaction chip and every chat item in ChatItemView.kt:159/328/367 (`"$itemId:$emoji"`). It is modifier-only, so it costs allocation, not skipping.
- Fix: make `UiKey` a `data class`, which keeps keys equal across recompositions. Or build keys only when `uiTestHooksEnabled` is set.

## 2. Harness correctness (flaky or false-pass risks)

### H1. Android `Bridge.call` keeps the socket after a read timeout, so every later reply is shifted by one (medium; mechanism high, trigger PLAUSIBLE)
- android/src/androidTest/java/chat/simplex/app/ui/harness/CliPeer.kt:14-26 has `soTimeout = 60_000`. A `SocketTimeoutException` leaves `socket` set, and the bridge's late reply stays unread in the stream.
- The trigger is realistic. The bridge's `start` waits up to `START_TIMEOUT_MS = 60_000` for the peer (desktopTest CliPeer.kt:45/128 via UiBridge.kt:50), so a peer that is slow to start makes the client time out at the same moment the bridge replies `err ...`.
- What follows:
  - `finish()` calls `stop` (CliPeer.kt:37). It reads the stale `err`, and `runCatching` swallows the error. Its own `ok` stays queued.
  - From then on, every call in the instrumentation process reads the previous reply.
  - `Bridge.servers()` (CliPeer.kt:28) gets `ok` and throws IndexOutOfBounds, so every later `app(user)` fails.
  - `drain` gets `ok` and inserts a fake peer line `ok`.
- Result: one slow peer start fails the rest of that pass in a cascade. The runner only resumes after a process crash, so these failures are not retried.
- Fix: on any exception, close the socket and set it to null. The bridge then stops that connection's peers (UiBridge.kt:34).

### H2. `uitest-android.sh` can exit 0 after giving up on a crashing pass (medium; PLAUSIBLE)
- scripts/uitest-android.sh:117 prints "giving up" and `break`s, but no failure flag is set. The exit status (:128) looks only at `results.txt`.
- A pass that crashes before any `INSTRUMENTATION_STATUS_CODE: 1` adds no line. Examples: a crash in a class initializer or a static `bootOnce` path, or a crash between tests that repeats.
- In that case, after 4 passes the script exits 0 with that class's tests missing.
- More generally, nothing checks that every test produced a result.
- Fix: count "Process crashed" passes and fail when the limit is hit, or compare against the expected test list (`am instrument -e log true`).

### H3. Known-issue allowlist can hide app errors raised during the body (low-medium; PLAUSIBLE)
- desktopTest UiTest.kt:45-55, 349-355. When `bodyCompleted` is true, any thrown exception (after removing wrappers) whose cause chain contains "Controller is not initialized" or "database closed" is logged and the test passes.
- Compose test reports uncaught exceptions from the composition's coroutines (LaunchedEffect on the test dispatcher) only at teardown. So such an error from the middle of the body is indistinguishable from a teardown error and is swallowed.
- Example: a regression that closes the DB while a chat is open makes ChatListView's or ChatView's LaunchedEffect API call throw "database closed". The assertions that follow are model-based and already satisfied, so the test passes.
- Fix: only allow these errors when they come from the reset or teardown path, for example by recording the time with `bodyCompleted` and checking a marker set in `resetForTest`.

### H4. Android `clipboardText()` falls back to the system clipboard, which is never reset between tests (low; PLAUSIBLE)
- UiTest.kt (androidTest) `clipboardText()`: if no captured intent carries `EXTRA_TEXT`, it returns `primaryClip`.
- That clipboard is device-wide and survives `pm clear` and earlier tests. Example: `AddressScreen.create()` (Screens.kt:161-167) has a Share that fails to fire. The poll then passes with a link copied in a previous test, and the test goes on with a wrong or deleted address.
- The usual result is a misleading connection timeout rather than a false pass. A test that only checks "something was shared" would pass falsely.
- Fix: clear the clip in `resetForTest`, or do not fall back when a share was expected.

### H5. Bridge rebuild check misses `PeerLines.kt` (low-medium; high confidence)
- scripts/uitest-android.sh:30 lists the bridge sources used for the `-newer` staleness check. It omits common/src/uiTestShared/kotlin/chat/simplex/app/ui/harness/PeerLines.kt, which `CliPeer` (the bridge's peer) extends and which defines `Waiter`, `PeerFlags`, `types` and `LINE_TIMEOUT_MS`.
- Editing PeerLines.kt therefore leaves the old `bridge-classes` snapshot in use, with a stale peer behavior or protocol.
- Fix: compute staleness over the whole `harness/` directories, or always run `uitestBridgeClasspath`, since Gradle up-to-date checks are cheap.

### H6. `OnboardingSharedState.selectedOperatorIds` is not reset between tests (low; latent)
- YourNetwork.kt:39-41 is a plain `var` in an object that is not one of the ModelSnapshot roots (CommonReset.kt:19).
- `OnboardingConditionsView` seeds from it (ChooseServerOperators.kt:51). Today every test passes through YourNetworkView, which overwrites it, so nothing fails.
- A future test that reaches Step4 without YourNetwork would inherit the previous test's selection, for example `setOf(enabled.first())` from `onboardingChooseOperatorsCannotDeselectAll`.
- Fix: add `OnboardingSharedState` to the snapshot roots.

### H7. `desktopUiTest` silently succeeds without the runner (low)
- common/build.gradle.kts `onlyIf { SIMPLEX_UITEST_RUNNER == "1" }`. Running `./gradlew :common:desktopUiTest` directly reports SKIPPED / BUILD SUCCESSFUL with zero tests.
- Fix: throw instead (`doFirst { if (...) throw GradleException("run scripts/uitest.sh") }`).

## 3. Runner scripts (killing processes, deleting files, touching home dirs)

### R1. `uitest-android.sh` leaks the bridge, the SMP server and the CLI peers on Ctrl-C or kill (medium-high; verified)
- scripts/uitest-android.sh:32-37 cleans up only through `trap cleanup EXIT`, and `/bin/sh` here is dash. I verified that dash does not run the EXIT trap on SIGINT or SIGTERM (`dash -c 'trap "echo x" EXIT; kill -TERM $$'` prints nothing).
- The bridge is started with `setsid` (:50), so it is in its own session and does not get the terminal's SIGINT either.
- Result after Ctrl-C:
  - The bridge JVM keeps running with its `uitest-servers` and CLI peers.
  - Port 7777 stays bound, so the next run fails with "bridge exited".
  - The Waydroid claim is not released until the 30-minute TTL expires.
  - A backgrounded `wd.sh up` keeps running.
- Fix: `trap 'cleanup; exit 130' INT; trap 'cleanup; exit 143' TERM` in addition to EXIT.

### R2. In the Android path the SMP test server listens on all interfaces of the host (low)
- tests/UITestServers.hs:37 uses `smpServerCfg`, which calls `startTCPServer ... Nothing port` and binds 0.0.0.0/::.
- On desktop this is harmless because tests run inside `unshare -n`. The Android bridge runs in the host network namespace (uitest-android.sh:50).
- For the whole run, port `20000+100*slot+1` is an open SMP relay on the LAN, using the fixture TLS key that is published in the repo.
- Fix: bind to 127.0.0.1, since `adb reverse` only needs loopback.

### R3. `kill -TERM -1` in `--inside` (informational; safe as used)
- scripts/uitest.sh:11. Inside `unshare --pid --fork --mount-proc` this is PID 1 of a private PID namespace, so `-1` reaches only the test processes.
- Invoking `scripts/uitest.sh --inside <dir>` directly, outside unshare, and then sending it TERM would signal every process of the user.
- Fix: guard with `[ $$ -eq 1 ] || { echo "--inside is internal" >&2; exit 2; }`.

### R4. `pm clear` on a configurable package (informational)
- scripts/uitest-android.sh:82. With `UITEST_SERIAL=<personal phone>` and `UITEST_APP_ID=chat.simplex.app`, the script wipes that app's real data without confirmation. It only prints a note (:67). By design, but worth a hard guard (refuse unless the package ends in `.debug` or a `--yes` flag is given).

### Checked, OK
- uitest.sh deletes only `build/uitest/run-*` relative to `apps/multiplatform` (:4, :23).
- Gradle uses `-g $PWD/.gradle/uitest-home`, never `~/.gradle`.
- `desktopUiTest` removes `XDG_DATA_HOME` and `XDG_CONFIG_HOME` and sets `XDG_CACHE_HOME`, `java.io.tmpdir` and `skiko.data.path`.
- `UiJvm.setup` redirects `user.home`, and `UiTestApp.bootJvm` (UiTestApp.kt:30) refuses to run if dataDir or configPath is outside the per-JVM dir.
- CLI peers and the peer template run with `HOME=<private dir>`.
- No writes to ~/.simplex or ~/.config were found.
- cabal uses its normal ~/.cabal / ~/.cache/cabal store, which is expected.
- Outer Ctrl-C in uitest.sh is handled: the trap KILLs `unshare --kill-child`, which tears down the whole PID namespace.
