# Code review: uncommitted changes in /home/user/wt-uitest (nd/desktop-ui-tests vs origin/master)

Scope: 60 modified files (+570/-212), plus untracked `common/src/uiTestShared/**`, `common/src/desktopTest/.../ui/**`, `android/src/androidTest/.../ui/**`, `common/.../ui/{Ui,UiIdle,UiTags.android}.kt`, `scripts/uitest*.sh`, `tests/UITestServers.hs`.
All paths are relative to `apps/multiplatform/` unless they start with `tests/` or `simplex-chat.cabal`. Every finding below was checked by reading the code. Nothing was built or run.

Checked and fine:
- Test tags stay off in release builds. Desktop: `uiTagsEnabled` is only set in `UiTestApp.bootJvm`. Android: it is set from `FLAG_DEBUGGABLE`, which only the `debug` build type can set.
- `Modifier.ui` returns `this` when tags are off.
- Normal `desktopTest` excludes `chat/simplex/app/ui/**`.
- `desktopUiTest` only runs when `SIMPLEX_UITEST_RUNNER=1`.
- The `uitest-servers` executable can only be built with the manual `uitest` flag, and not with `client_library`.
- Tests contain no comments and no coordinates or pixel offsets. Coordinates appear only in the harness `tapPointInNode` and in the `UITEST click` log line, which is covered in S6.

---

## Bugs

### B1. The desktop harness reports real failures as passes when the error text contains a "known issue" string
`common/src/desktopTest/kotlin/chat/simplex/app/ui/harness/UiTest.kt:43-51`, `:339-343`

- `isKnownAppIssue` checks each message in the cause chain for `"Controller is not initialized"` or `"database closed"`.
- `poll()` builds its timeout `AssertionError` message with `last condition error: $it` (line 311). So if a poll condition throws `Exception("Controller is not initialized")`, which `sendCmd` throws (`SimpleXAPI.kt` `sendCmd`), the timeout error matches.
- After that, `if (bodyStarted) break` leaves the loop and `passed = true` is set.
- `app()`, and therefore `resetForTest`, runs inside the body. So a reset that fails with "database closed" also ends as PASSED, with the rest of the test never run.
- Result: a test that stopped halfway through is reported as passed.

Fix:
- Never count a failure as passed once the body has started. Either throw `org.junit.AssumptionViolatedException` so the test shows as skipped, or retry the whole test.
- Match the known issues only on the exception's own class and message, never on harness-built `AssertionError` text.

### B2. `scripts/uitest-android.sh` only works on this machine
`scripts/uitest-android.sh:5,8,9,20,36-40,56,65`

- `ADB=/home/user/Android/Sdk/platform-tools/adb`
- `PATH="/home/user/waydroid:$PATH"`, and the script depends on private `wd.sh` (claim/up/serial/install/release). `wd.sh` is not in the repo.
- `java=/usr/lib/jvm/java-17-openjdk-amd64/bin/java`
- `pkg=chat.simplex.app.debug`. The default `application_id.suffix` is `""` (`build.gradle.kts:25`), so this only works with a local `local.properties` override.

Fix:
- Use `${ANDROID_HOME:-$ANDROID_SDK_ROOT}/platform-tools/adb`, or `adb` from `PATH`, and `ANDROID_SERIAL` for the device.
- Use `${JAVA_HOME}/bin/java`.
- Read the package from the APK (`aapt2 dump packagename`), or take it as an argument.
- Make the Waydroid claim/up steps optional (`if command -v wd.sh`), or keep this script out of the PR as personal tooling.

### B3. A JVM crash at exit fails the Gradle task, and the crash logs were left in the source tree
- Untracked files `common/hs_err_pid340.log` and `common/hs_err_pid1387.log` would be committed by `git add -A`.
- Both are SIGSEGVs in `__pthread_rwlock_wrlock` (address 0x18) on a native thread with `VM state: at safepoint (shutting down)`. Both came from `desktopUiTest` workers.
- In `/home/user/uitest-review-tmp/b14-desktop.log:1308` this made `:common:desktopUiTest` fail with `Process 'Gradle Test Executor 3' finished with non-zero exit value 134`. The same thing in `b7-desktop-forks3.log:178`.
- Observed fact, not an established cause: in both logs the crash comes right after `Exception in thread "GTK Theme Detector Thread" java.lang.NullPointerException ... GnomeThemeDetector.java:119`, and after the fork's last test.

Fix:
- Delete both logs.
- Add `-XX:ErrorFile=$runDir/hs_err_pid%p.log` to the `desktopUiTest` jvmArgs (`common/build.gradle.kts:381`) so crash logs land in the run directory.
- Investigate the exit crash. Candidates: stop the chat core (`chatCloseStore`) in the `UiJvm` shutdown hook (`UiJvm.kt:14-19`), and keep the system-theme detector from starting in tests.
- Until this is fixed, a fully green run can still fail the task.

---

## Should-fix

### S1. `UiIdle` tracking runs in every release build
- `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt:809-818`: every received event allocates an `Item` and the string `"event ${msg.responseType}"`, plus a map insert and remove.
- `SimpleXAPI.kt:947-967`: the same for every `sendCmd`.
- `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt:37-57`: every `withApi` / `withBGApi` / `withLongRunningApi` adds an `invokeOnCompletion` handler and a map entry.
- Release builds have no test tags, but they still pay for all of this.
- Jobs that never finish stay in `items` forever. Today they are only excluded through the `idle = false` call sites (`ChatModel.kt:827`, `ProcessedErrors.kt:19`, `AppUpdater.kt:171`) and a hard-coded list of command types (`SimpleXAPI.kt:947`). These are test concerns placed in production code.

Fix: gate the tracking on the same flag as the tags (consider renaming it to `uiTestHooksEnabled`):
```kotlin
fun start(what: String): Long { if (!uiTagsEnabled) return 0; ... }
fun finish(id: Long) { if (id == 0L) return; ... }
fun eventProcessed() { if (!uiTagsEnabled) return; ... }
```
Also skip `trackedBy` when tracking is off, and build the `what` string lazily (`start { "cmd ${cmd.cmdType}" }`).

### S2. `UiIdle` has unused members
`common/src/commonMain/kotlin/chat/simplex/common/ui/UiIdle.kt:7,12-14,21,26,31`

`startedCount`, `finishedCount`, `eventCount` and `Item.startedAt` are written but never read anywhere. They look left over from probing. Remove them.

### S3. Keys defined or applied that no test uses (the rule allows keys only on elements a test acts on or reads)
- `common/src/commonMain/kotlin/chat/simplex/common/ui/Ui.kt:57` `NewChat.PasteLink`, `:59` `NewChat.CreateChannel` (applied at `views/newchat/NewChatSheet.kt:193,197`). No test uses them. Make `NewChatAction.uiKey` nullable and pass `null`, or drop the two keys.
- `Ui.kt:135` `Link.Profile(userId)` (applied at `views/newchat/NewChatView.kt:376`): no test uses it.
- `Ui.kt:177` `Alert.Title`: defined, never applied, never used.
- `views/chatlist/ChatListView.kt:366` `Column(Modifier.ui(Ui.ChatList.ConnectBanner))`: tests only use `.part("newLink")` and `.part("pasteLink")`. Drop the tag on the Column and keep the key only as a namespace.
- The alert-option ids `"deleteWithoutNotification"` (`views/chat/ChatInfoView.kt:342`) and `"fromHistory"` (`views/chat/item/ChatItemView.kt:1415`) are never used by a test.
- Keys tests only check for presence:
  - `Ui.UserPicker.Sheet` (`ChatListToolbarSearchTest.kt:36,38`, `BackKeyTest.kt:110,112,132`). On Android it also needs extra app logic, `.ui(if (pickerIsVisible) Ui.UserPicker.Sheet else null)` (`androidMain/.../chatlist/UserPicker.android.kt:150`).
  - `Ui.Onboarding.SetupNotifications` (`OnboardingTest.kt:241`), where the `seesText("Setup notifications")` next to it already covers the check.

  Either drop these keys or state an explicit exception to the rule.

### S4. Unused imports
- `Ui.kt:6-7`: `SemanticsProperties`, `semantics`.
- `common/src/desktopTest/kotlin/chat/simplex/app/ui/harness/Reset.kt:7,13,15,16`: `AppLock`, `MutableStateFlow`, `java.lang.reflect.Modifier`, `IdentityHashMap` (left over from moving code into `CommonReset.kt`).
- `harness/DbTemplate.kt:8`: `FileAlreadyExistsException`.
- `common/src/uiTestShared/kotlin/chat/simplex/app/ui/chat/SendCompatTest.kt:5`: `chat.simplex.common.ui.Ui`.

### S5. Dead harness and build code
- `Reset.kt:92-94`: the `staticField(cls, name, value)` setter overload is never called. Also the stray blank line at `:54`.
- `UiJvm.kt:6-7,15`: `shuttingDown` is written but never read.
- `Reset.kt:76`: `completePendingDialogs(): Int`. The return value is never used, so make it `Unit`.
- `common/build.gradle.kts:412-420`: the `uitestBridge` JavaExec task is never used. `uitest-android.sh` uses `uitestBridgeClasspath` plus `java` directly. Remove it.
- `android/build.gradle.kts:195`: `androidx.test.uiautomator:uiautomator:2.3.0` is not used anywhere. `FailureShots` uses the platform `UiAutomation`. Remove it.
- `harness/TestRoot.kt:17-20`: `RecordingUriHandler.opened` is recorded but never read. Use a no-op `UriHandler` (named for what it does), or assert on it.
- `open class` on 5 test classes that are never subclassed: `AddressCompatTest`, `OneTimeLinkCompatTest`, `ChatListRequestsCardsCompatTest`, `SendCompatTest`, `JoiningCompatTest`. Drop `open`.

### S6. Debug and probe leftovers
- `harness/UiTest.kt:210`: `println("UITEST click ${key.tag} at ... popups=...")` on every click. It floods stdout (`showStandardStreams = true`) and prints pixel coordinates (see `b14-desktop.log`). Remove it, or print only inside the failure message.
- `android/.../harness/UiTest.kt:179`: the same as a `Log.i`.
- `harness/HarnessMeasureTest.kt` (whole file): a benchmark that runs 20 resets, 120 frames and 10 captures, prints timings, and asserts nothing. It runs inside every `desktopUiTest` run because it matches `chat/simplex/app/ui/**`. Delete it or `@Ignore` it.
- `harness/HarnessBootTest.kt:10-11`, `harness/HarnessCliPeerTest.kt:14,19-20`: `println` of timings, texts, link, contact id and chats. `HarnessCliPeerTest` calls `bob.contactId("alice")` only to print it. Assert instead, or drop the line.
- `harness/UiTest.kt:47`: the log text "(plan 15.5)" points to a private planning doc. Drop it.

### S7. Duplicated code (CODE.md: duplicate function bodies are not acceptable)
- **CliPeer line matching is copied between desktop and Android.** `Waiter`, `PeerFlags`, `expects`, `expectsMessage`, `eventually`, `eventuallyMessage`, `eventuallyMatching`, `nextLine`, `contactId`, `pending`, `stripTime`, `takeFirst` are all near-identical in:
  - `desktopTest/.../harness/CliPeer.kt:9-13,99-158,203-206`
  - `androidTest/.../harness/CliPeer.kt:7-11,48-121`

  The timeouts already differ: `LINE_TIMEOUT_MS` is 10 s on desktop and 15 s on Android. Fix: add a shared `PeerLines` (or `abstract class CliPeerBase`) in `uiTestShared` with one abstract `fill()` hook. It is a no-op on desktop and `Bridge.call("drain")` on Android.
- `desktopTest/.../harness/CliPeer.kt:60-64` `typeLine` is identical to `:93-97` `types`. Keep `types`, and have `UiBridge.kt:53` call it.
- The sequence "wait for UiIdle to be empty, `settle()`, `rightClick(row)`" appears four times:
  - `uiTestShared/.../harness/Screens.kt:66-68`
  - `harness/ChatListMenusScreens.kt:8-10`
  - `harness/ChatListListsScreens.kt:41-43`
  - `harness/ChatListFilterScreens.kt:47-49`

  Fix: one `UiScope.openContextMenu(key: UiKey)`.
- `harness/Helpers.kt:28-33` (`connectPeers`) repeats `CliPeer.createOneTimeLink()` (`ChatListMenusScreens.kt:14-19`). Fix: `val link = a.createOneTimeLink()`.
- `harness/ChatListFilterScreens.kt:88-90` (`createGroupLink`) repeats `hostGroup` (`Helpers.kt:41-43`). `groups/JoiningCompatTest.kt:12-14` inlines `hostGroup` again.
- "Open the group row, confirm the join alert, wait for joined" is already `joinGroupFromList` (`ChatListRowsScreens.kt:22-30`), but is inlined in `groups/GroupsTest.kt:25-27,41-43`, `groups/JoiningCompatTest.kt:15-17` and `main/ChatListMenusPendingTest.kt:63-68`.
- `groups/GroupsTest.kt:10` and `groups/MemberInfoTest.kt:10` both declare `private const val GROUP_DELIVERY_MS = 30_000L`, which equals `CONNECT_TIMEOUT_MS` (`Helpers.kt:3`). Use the shared constant.
- `desktop/ChatListMenusPendingTest.kt:13,38-42` and `desktop/ChatListToolbarSearchTest.kt:16,66-70` both declare `planConfirm` and an identical `enterLinkInSearch`. Move them into the desktop harness.
- `androidTest/.../back/BackKeyTest.kt:189-197` (`back_closesWhatsNew`) inlines `whatsNewFromSettings` (`ChatListToolbarScreens.kt:51-56`).
- `desktop/WhatsNewTest.kt:23-33` (`finishOnboarding`) repeats `OnboardingScreen.useRandomPassphrase` / `complete`.

### S8. Test names promise UI checks that are never made
- `main/ChatListRowsTest.kt:178` `incognitoIconOnRow`: only checks `contactConnIncognito` in the model and that notes are not incognito. No row icon is checked.
- `ChatListRowsTest.kt:187` `verifiedIconOnRow`: only checks `contact.verified` in the model, then closes. No row icon is checked.
- `ChatListRowsTest.kt:58` `previewLink`: the poll's description says "the link image", but it checks the model's `MCLink` type, not an image on the row.

Fix: assert the row's content descriptions (`rowDescriptions(name)`), as `favoriteStarWhenNoUnread` does, or rename the tests to what they actually check. Similarly, the "Gray" in `unreadCounterGrayWhenMuted` (`:107`) is never checked.

### S9. Hard-coded What's New version
`uiTestShared/.../harness/ChatListToolbarScreens.kt:45-47` sets `VERSION = "v7.0.1"`, used by `main/WhatsNewTest.kt:29` and `desktop/WhatsNewTest.kt:17`. The app's value is `if (isInUs()) "v7.0.1" else "v7.0"` (`views/onboarding/WhatsNewView.kt:950`). A `v7.1` entry is already commented in (`:979`).

Problem: the tests break when that entry ships, and on any build where `androidPlayStoreCountry` is set and not US.

Fix: read the app's value (expose `lastVersion` as `internal`, or use `versionDescriptions.last().version`).

### S10. The XFTP server is configured but never started
- `TestServers.kt:31` builds `xftp1` at `portBase + 2` with a hard-coded key hash.
- `configureServers` (`uiTestShared/.../harness/ServersConfig.kt:14`) makes it the only enabled XFTP server.
- `uitest-android.sh:71` reverse-forwards that port.
- But `tests/UITestServers.hs` starts only SMP.

Result: any file or voice test hangs on transfer errors.

Fix: start an XFTP server in `UITestServers.hs` and print `ready xftp1 ...` (parsed the same way as `smp1`), or name it honestly (`unusedXftp`) and leave a known-limits note. Separately, `KEY_HASH` (`TestServers.kt:12`) duplicates `testServerKeyHash` from `tests/ChatClient.hs:142`. Take it from the server's `ready` line instead.

### S11. The Android runner silently depends on the desktop runner
`uitest-android.sh:41` points the bridge at `$mp/build/uitest/env.properties`. Only `scripts/uitest.sh:58-64` writes that file, and that is also where the `simplex-chat` and `uitest-servers` binaries get built.

Result: on a fresh tree, the bridge exits with `env.properties has no servers` (`UiEnv.kt:10`).

Fix: have the Android script build the binaries and write the file itself (factor the cabal and env part of `uitest.sh` into a shared function), or check for the file up front with a clear message.

### S12. Desktop and Android `UiScope` behave differently in ways shared tests rely on
- `poll`: desktop extends past the deadline by up to 60 s while the same API calls are still in flight (`desktopTest/.../harness/UiTest.kt:306-313`). Android fails at the deadline (`androidTest/.../harness/UiTest.kt:126-129`). A shared test can pass on desktop and time out on Android. Fix: put the same rule in both, ideally a shared helper in `uiTestShared`.
- `clipboardText()`: desktop returns the clipboard (`desktop UiTest.kt:125`). Android returns the `EXTRA_TEXT` of the latest captured share intent, falling back to the clipboard (`android UiTest.kt:307-311`). So `AddressScreen.create()` (`Screens.kt:168`) accepts "shared" on Android and "copied" on desktop. Fix: rename it to `handedOutText()`, or split it into `sharedText()` and `clipboardText()`.
- `rightClick` is a long-press on Android (`android UiTest.kt:195-201`). Name it `openContextMenu` on both.
- `desktop UiTest.kt:176` `scrollIntoView` is public but only used inside the harness. Make it `private`.

### S13. A bridge read can block without a timeout
`androidTest/.../harness/CliPeer.kt:21`: `Socket("127.0.0.1", PORT)` has no `soTimeout`, so `input.readLine()` (`:27,29`) can block until the 300 s watchdog kills the process. Set `s.soTimeout = 60_000` and fail with the command in the message.

---

## Nits

- `Ui.kt:20-21`, `desktopTest/.../harness/UiTest.kt:138-139,158-160`: doubled or tripled blank lines.
- `.ui()` sometimes comes first and sometimes after `padding` in a modifier chain. Placing it after `padding` changes the tagged bounds. Examples: `WelcomeView.kt` `Modifier.padding(end = DEFAULT_PADDING_HALF).ui(Ui.Onboarding.Migrate)`, `YourNetwork.kt` `...fillMaxWidth().ui(Ui.Onboarding.Continue)`, `ChooseServerOperators.kt` `(...).ui(Ui.Onboarding.OperatorsOk)`. Pick one convention, preferably first in the chain.
- `views/chatlist/ChatListView.kt:703`: a positional `Modifier.ui(...)` after the named `click = click`. Use `modifier = Modifier.ui(...)`.
- `android/src/main/java/chat/simplex/app/SimplexApp.kt:6`: the new import sits in the middle of the `android.*` block. In `androidTest/.../harness/UiTest.kt:20-26`, `android.*` imports come after `org.junit`. `Helpers.kt:7,10,14-15,20,24` and `androidTest/.../harness/UiTestApp.kt:54-58` use fully qualified names instead of imports.
- `main/ChatListToolbarSearchTest.kt:106`: `private companion object` sits between test methods. Move it to the end.
- Helper files whose names don't match their contents: `ChatListToolbarScreens.kt` holds `WhatsNewScreen`, `backToChatList` and `enableOperator`. `ChatListMenusScreens.kt` holds `CliPeer.createOneTimeLink`.
- The same simple class names exist in two packages (`main.WhatsNewTest` / `desktop.WhatsNewTest`, likewise `ChatListMenusPendingTest`, `ChatListFiltersListsTest`, `ChatListToolbarSearchTest`), which makes reports and IDE navigation confusing. Suffix the desktop-only ones, for example `...DesktopTest`.
- Window size 1600x900 is written in three places: `harness/UiTest.kt:36`, `harness/UiTestApp.kt:38`, `harness/Reset.kt:61`. Use one constant.
- Absence checks that pass on the first frame: `desktop/ChatListToolbarSearchTest.kt:43-44,61-62` (`settle(); poll { !exists(planConfirm) }`), `main/ChatListRequestsCardsTest.kt:96`, `main/ChatListMenusPendingTest.kt:108,123`. Prefer `quiesce()` followed by a single `check(!...)`.
- `main/ChatListMenusPendingTest.kt:23-47` (`contactMenuMarkRead`, `contactMenuMarkUnread`) mostly repeat `main/ChatListMenuTest.kt:9-20`.
- `main/ChatListMenusPendingTest.kt:140-143`: a `poll` whose condition sends CLI commands and runs a nested 10 s wait. It works, but the side effect inside a poll condition is surprising. Use an explicit retry loop.
- `Screens.kt:276-278`: `enterWhenSendable` presses Enter a second time inside `runCatching`. That hides a missed first Enter, which is an app bug the test should report.
- `views/newchat/NewChatSheet.kt:188-211,448`: the `Triple` to `NewChatAction` refactor, and the re-indent of the whole `sendCmd` body (`SimpleXAPI.kt:945-968`), make the diff larger than it needs to be (CODE.md minimal-diff rule). The `sendCmd` change can be one wrapper line, for example `return uiIdle("cmd ...") { withContext(Dispatchers.IO) { ...unchanged... } }`.
- `android/build.gradle.kts:196`: `ui-test-junit4:1.8.2` is hard-coded. Derive it from the Compose version, or note why it has to match `compose.version=1.8.2`.
- `common/build.gradle.kts:1`: `import java.time.Duration` has no blank line before `plugins {`.
- `scripts/uitest.sh:43-46`: if `common/src/commonMain/cpp/desktop/libs/linux-x86_64` is missing (it is untracked), `ls` fails, `grep -v` exits 1, and `set -e` stops the script without a message. Check for the directory first with a clear error.
- `scripts/uitest.sh:79-80`: the `trap` is set after the `&` launch. Set it before.
- `desktopTest/.../harness/UiBridge.kt:23`: an unauthenticated command server on fixed port 127.0.0.1:7777 that spawns CLI processes and types into them. This is acceptable for a local dev tool, but any local user can drive it. Mention it in the runner docs, or bind a random port and pass it through `adb reverse`.
- `harness/HarnessBootTest.kt:9`: `seesText("Chats") || seesText("alice")` is weak, and the class duplicates `androidTest/.../SmokeTest.kt`.
- Keys are built on every recomposition even when tags are off (`Ui.Chat.Item(cItem.id)`, `ReactionChip(cItem.id, r.reaction.text)`, `RowMenu(...)` with string concatenation). Negligible, but a `Modifier.ui { key }` overload would avoid it.
