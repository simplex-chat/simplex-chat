# Desktop UI tests: typed component keys, a headless driver and a test DSL

**Date:** 2026-10-10 (revised 2026-10-13 after adversarial review rounds 1–49; review stopped on a time box; revised 2026-10-14: what is built is in 0.5, status in section 15)
**Base:** master at `d72f68e6d` (`~/wt-uitest-ref`)
**Files:** this plan and its companions are in `plans/ui-tests/`: the Android plan (`2026-10-12-android-ui-test-automation.md`), the Android research log (`2026-10-12-android-research-progress.md`), the P1a start page (`2026-10-13-p1a-start-here.md`), and the test catalogue (`catalogue/`: chapters `00`–`05`, review and progress notes in `catalogue/verify/`, count and status scripts in `catalogue/tools/`). Paths starting with `~/` refer to the author's machine (reference worktree, build scripts, Waydroid tooling).
**Start here (implementers):** `plans/ui-tests/2026-10-13-p1a-start-here.md`. It covers the reading order, build order and exit check for P1a (section 12). P1a is complete, so that page is historical; the current harness is described in 0.5.

## Summary

The desktop app has no UI tests and no names for its components: `testTag` and `Modifier.semantics` appear zero times. Manual GUI testing (the PR #7573 matrix) had to drive the app by screen coordinates and read its database to check what it showed. Clicks went to the wrong row whenever the chat list reordered, and reading the live database froze the app once.

This plan adds four layers:

1. **Keys.** Every interactive or inspectable component gets a typed key from one `Ui` object, applied with `Modifier.ui(key)`. Rows, items and members carry their entity id in the key.
2. **Typed semantics.** The state a test asserts on (unread count, item status and metadata, sender, verification, incognito, muted) is published as typed semantics properties, not judged from pixels.
3. **Driver.** A headless in-process driver renders the real `AppScreen()` with the real core in a test JVM, using Compose's skiko test scene (`runInternalSkikoComposeUiTest` with a `StandardTestDispatcher`, 7.2, 14; `runDesktopComposeUiTest` as the fallback). Other users are CLI processes of the current build or a stable release downloaded by tag. Servers are local, and the whole test run is cut off from the real network.
4. **DSL.** Screen objects and a test DSL that reads like `tests/ChatTests`: `bob sends "/c $link"`, `bob expects "confirmation sent!"`, `alice.chatList.row("bob") shouldHave unread(1)`.

Decisions (2026-10-10):
- Thorough coverage: every screen and every user-facing feature of the desktop app. The catalogue (section 10) lists 1,177 test cases (28 of them `IGNORE` with a reason) in four files plus a legend under `plans/ui-tests/catalogue/`.
- In-process driver only; no remote automation server. Tests run locally.
- Typed keys on every component.
- DSL in Kotlin, in the `desktopTest` source set, run by a separate `desktopUiTest` task. (As built: test bodies and screens are in `common/src/uiTestShared`, compiled into `desktopTest` and Android `androidTest`, 0.5.1.)
- No badge-display tests for now: no badge semantics property, no test issuer key. Keys on the badge screens are still added, and the supporter-perks screens are tested without badge display.
- The stable CLI peer is the release binary downloaded by tag.
- Screenshots (added 2026-10-12): every passing catalogue test (except the `REALTIME` ones and runs with `-PuiShots=off`, 7.7) saves committed PNG screenshots, rewritten only when the screen changes outside masked volatile regions, to show UI changes as image diffs, give evidence that the tests ran (new and changed images, plus each run's summary), and document every screen (7.7).

Changed since (13, 2026-10-13/14): Android is in scope through shared test bodies (`uiTestShared`); the minimal-app-code rule of 2026-10-14 supersedes items 1–2 above and "typed keys on every component" for now; the shared bodies live in `common/src/uiTestShared`, compiled into `desktopTest` and Android `androidTest`. What is built is in 0.5.

## 0.5 As built (2026-10-14)

Sections 3–11 remain the design. This section describes what is implemented in `~/wt-uitest` (branch `nd/desktop-ui-tests`, base `eda94a505`, nothing committed), checked against the code on 2026-10-14 at about 20:05 UTC. Paths are relative to `apps/multiplatform`. Each design subsection that differs starts with a one-line "As built" note pointing here.

### 0.5.1 Layout and test counts

| Source set | Path | Compiled into | `@Test` |
|---|---|---|---|
| Shared harness and tests | `common/src/uiTestShared/kotlin/chat/simplex/app/ui/{harness,main,chat,groups,settings}` | `desktopTest` (`common/build.gradle.kts`, `kotlin.srcDir`) and `:android` `androidTest` (`android/build.gradle.kts`, `sourceSets["androidTest"].java.srcDir`) | 112 |
| Desktop-only catalogue tests | `common/src/desktopTest/kotlin/chat/simplex/app/ui/desktop` | `desktopTest` | 25 |
| Desktop harness and its tests | `common/src/desktopTest/kotlin/chat/simplex/app/ui/harness` | `desktopTest` | 5 (`HarnessMeasureTest` is `@Ignore`d, a manual benchmark) |
| Android harness and Android-only tests | `android/src/androidTest/java/chat/simplex/app/ui/{harness,back}`, `ui/SmokeTest.kt` | `androidTest` | 15 |
| **Desktop run** | | | **142** (141 run, 1 ignored) |
| **Android run** | | | **127** (112 + 15) |

- Shared (112): ChatListFiltersListsTest 21, ChatListRowsTest 16, ChatListToolbarSearchTest 16, ChatListMenusPendingTest 10, ChatListRequestsCardsTest 8, MessagingTest 6, ContactsTest 5, ChatListMenuTest 4, WhatsNewTest 4, GroupsTest 3, ContactInfoTest 2, ForwardTest 2, MemberInfoTest 2, and 1 each in AddressCompatTest, AppearanceTest, ChatListRequestsCardsCompatTest, ChatProfilesTest, ChatSearchTest, DeleteMessageTest, EditMessageTest, JoiningCompatTest, NotesTest, OneTimeLinkCompatTest, OneTimeLinkTest, SendCompatTest, UserPickerTest.
- Desktop-only, package `chat.simplex.app.ui.desktop` (names end in `DesktopTest`, except `OnboardingTest`, so none repeats a shared class name): OnboardingTest 22, ChatListFiltersListsDesktopTest 2, WhatsNewDesktopTest 1.
- Desktop harness: HarnessResetTest 2, HarnessBootTest 1, HarnessCliPeerTest 1, HarnessMeasureTest 1.
- Android-only: `back/BackKeyTest` 14 (helpers in `back/BackKeyHelpers.kt`), `SmokeTest` 1.
- Shared harness files: `Screens.kt` (`App`, `ChatListScreen`, `NewChatScreen`, `UserPickerScreen`, `ProfilesScreen`, `AddressScreen`, `AppearanceScreen`, `ProfileScreen`, `ChatScreen`, `ContactInfoScreen`, `GroupInfoScreen`, `MemberInfoScreen`, `ownGroup`), `ChatList{Filter,Lists,Menus,Rows,Toolbar}Screens.kt`, `OnboardingScreens.kt`, `Helpers.kt`, `PeerLines.kt`, `TapPoint.kt`, `TestPrefs.kt`, `ServersConfig.kt`, `CommonReset.kt` (`ModelSnapshot`, `CommonUiState`, `CommonPrefs`).
- Each platform has its own `UiScope` class with the same method names, and the shared code compiles against either. There is no `UiDriver` interface (7.4).

### 0.5.2 App code

- `common/src/commonMain/kotlin/chat/simplex/common/ui/Ui.kt`: `UiKey`, `Modifier.ui(key)`, `object Ui` and the flag `uiTestHooksEnabled` (renamed from `uiTagsEnabled`). `Modifier.ui` and `UiIdle` do nothing unless the flag is set. The desktop harness sets it in `bootJvm`. On Android, `initUiTags(context)` (`common/src/androidMain/kotlin/chat/simplex/common/ui/UiTags.android.kt`, called from `SimplexApp`) sets it when the app is debuggable. Release builds never set it.
- **Keys rule** (user, 2026-10-14; decision in 13): keys go only on elements a test acts on or must read, and there are no observation-only helpers. `Ui.UserPicker.Sheet` is the one presence-only key, because the Android user picker stays composed off screen. States without a key are read through the model (`contact.verified`, `chatInfo.incognito`, `draftChatId`, chat items), visible texts or existing content descriptions. Not built: typed semantics (4), `UiSemantics.kt`, `uiVolatile()`, the key lint (9.4).
- **Pseudo-key `described(text)`** (`Helpers.kt`, id `@described`): matches a node with that content description and a click action, on both platforms. It replaces the planned `Ui.Modal.Back`.
- **`ui/UiIdle.kt`**: in-flight items (id, label), `abandon`, `lastEventAt`; no counters. The hooks are thin wrappers:
  - `sendCmd` runs as `UiIdle.track { withContext(Dispatchers.IO) { … } }`; the status polls `GetAgentSubsTotal` and `GetAgentServersSummary` are not tracked;
  - the receiver wraps event processing in `UiIdle.trackEvent`;
  - `withApi`/`withBGApi`/`withLongRunningApi` jobs are tracked until they complete, except those launched with `idle = false`: `PopChatCollector` (ChatModel.kt:827), `ProcessedErrors` (ProcessedErrors.kt:19), `createUpdateJob` (AppUpdater.kt:171).
- Size: 56 modified app files (`commonMain`, `desktopMain`, `androidMain`, `SimplexApp.kt`; +380/−169 lines), plus the new `Ui.kt`, `UiIdle.kt` and `UiTags.android.kt`.

### 0.5.3 Desktop harness (`common/src/desktopTest/kotlin/chat/simplex/app/ui/harness`)

- **Per JVM** (`UiJvm`, `UiEnv`, `UiTestApp.bootJvm`):
  - directory `<run>/jvm-<slot>-<pid>`, with `user.home` and `java.io.tmpdir` inside it;
  - `bootJvm`: path check, `System.load(libapp-lib)` and `initHS`, English locale, `uiTestHooksEnabled = true`, a no-op `NtfManager` (records nothing), window state 1600 × 900, `runMigrations`, theme applied, toolbar blur 0, `PrefStores.capture`, `ModelSnapshot.capture`;
  - the shutdown hook stops the core, the peers and `uitest-servers`, then destroys the remaining descendant processes.
- **`uiTest { }`** (no parameters):
  - runs `runInternalSkikoComposeUiTest(1600, 900, StandardTestDispatcher())` with `autoAdvance = false`, then the body, then `drain()` inside the scene;
  - `finally`: two EDT flushes, `drain()`, `resetForTest(null)` if the test left no controller, peers stopped, and `<jvm>/t<n>` deleted after a pass only;
  - the known-issue policy is in 0.5.8.
- **`app(user)`** (`null` = onboarding boot) runs `resetForTest(user)` and composes `TestRoot { AppScreen() }`. `resetForTest`, in order:
  1. `drain`;
  2. stop the core (wait at most 10 s for `ctrlInitInProgress`, `stopChatAsync`, `chatCloseStore`, `setChatCtrl(null)`) and `deleteChatDatabaseFilesAndState()`;
  3. `drain`, `PrefStores.restore`, `clearUiState`, `TestServers.start()` (once per JVM), `DbTemplate.installIfPresent`, `TestPrefs.preset()` (onboarded boots only);
  4. `initChatController`; the first JVM without a template stops the core, publishes the template with an atomic rename and initialises again;
  5. with a user: `apiCreateActiveUser`, model and onboarding stage set, `startChat`, `configureServers`, `setLastVersionDefault`, `clearTransientUi`.
- **`restart()`**: empty composition; `restartCore` (`apiStopChat` if running, `chatRunning = null`, `chatCloseStore`, `setChatCtrl(null)`, `drain`, `clearUiState`, `initChatControllerOnStart`); compose again; poll (30 s) until `chatDbStatus != null` and init has finished.
- **`TestRoot`**: an in-memory `LocalClipboardManager` and a recording `LocalUriHandler`.
- **`poll(timeout = 10 s, what) { cond }`**:
  - each iteration: `advanceTimeBy(16)`, `waitForIdle`, the condition, `Thread.sleep(16)`; a condition that throws counts as "not yet", and its last error goes into the timeout message;
  - past the deadline it keeps waiting while any `UiIdle` item that was in flight at the deadline still is, up to 60 s more;
  - a timeout writes `<run>/failures/<jvm>-NN-<what>.png` and reports the in-flight list and the first 60 visible texts;
  - `settle()` advances 30 frames of 16 ms.
- **Actions:**
  - `click`: wait for the node; `scrollIntoView` (at most 10 `ScrollBy` steps: by the overflow plus 50 px, or 0.6 of a page for an unplaced node, then 40 frames); non-empty stable bounds; enabled; no open popup unless the target is inside one; `RequestFocus` and 3 frames; then a touch tap at `tapPointInNode` (`TapPoint.kt`: the centre, or the middle of the widest span no nested clickable covers, nested targets widened to 48 dp);
  - `rightClick`: a mouse right click;
  - `typeInto`: `performTextInput`, verified through `EditableText`; `enterText`: the same without verification;
  - `replaceText`: select all, input, and `performTextReplacement` if the field doesn't hold the text after 2 s;
  - `pressEnter`: the Enter key; `escape()`: 3 frames, then the window back stack;
  - `scrollTo(key)`: pages each lazy list (`ScrollBy` 0.6 page, 40 frames, at most 20 pages) until the key appears; `performScrollTo*` is never used (15.4 item 20);
  - `setSlider`: `SetProgress`.
- **Reads:** `exists` (any match), `waitFor`, `textOf`, `textsOf`, `descriptionsOf`, `editableText`, `isEnabled`, `seesText`, `seesDescription`, `texts`, `sliderValue`, `storedSetting` (`settings.properties`), `clipboardText`. Lookups use keys or literal English text, not `MR.strings`.

### 0.5.4 Android harness (`android/src/androidTest/java/chat/simplex/app/ui/harness`)

- **`uiTest { }`** under `createEmptyComposeRule()`: a 300 s watchdog (thread dump to logcat, then `Process.killProcess`; the runner resumes), `bootOnce`, the body, then `finish()` (peers stopped, `Intents.release()` before `ActivityScenario.close()`, `drain`).
- **`bootOnce`:**
  - waits at most 180 s for the app's own startup, and rebuilds a fresh database if it has users (15.4 item 10a);
  - decrypts it: `apiStopChat`, then `apiStorageEncryption(key → "")`; on failure it keeps the encrypted template and logs the error. decryption succeeds (verified in the final run, 15.4 item 25);
  - saves the template to `dataDir/uitest-db-template` (an empty key file means decrypted);
  - presets blur 0, `notificationsMode = OFF`, both background-service notices shown, `canAskToEnableNotifications = false`, `privacyProtectScreen = false`;
  - `PrefStores.capture` (the two SharedPreferences stores and `themes.yaml`), `ModelSnapshot.capture`.
- **`app(user)`**: `resetForTest` (stop, delete, `PrefStores.restore`, `CommonUiState.clear`, `TestPrefs`, template, init, user, `startChat`, servers from the bridge, `setLastVersionDefault`), then `ActivityScenario.launch(MainActivity)` with Espresso-Intents stubbing external intents.
- **`restart()`**: `restartCore` first, then a poll (60 s) until chat runs with the user loaded, then close and relaunch the activity (15.4 item 10).
- **`poll`**: `advanceTimeBy(48)`, `waitForIdle`, the condition, `sleep(50)`; the deadline extension, last error and message are as on desktop. Failure screenshots go through `UiAutomation` into the app's external files directory, and the runner pulls them. `settle()` is one 500 ms advance.
- **Differences from desktop:**
  - `click` uses `performScrollTo`, then centres the node in its `ScrollBy` parent;
  - `rightClick` is a long press; `escape()` and `goBack()` press system Back (`Espresso.pressBack`);
  - the message field (an `EditText`) is set with Espresso `replaceText`, retried every 2 s for up to 10 s, and read with a `ViewAssertion`; `pressEnter` on it clicks Send, elsewhere it sends the IME action;
  - `scrollTo` uses `performScrollToNode`;
  - `clipboardText` reads the last recorded intent's `EXTRA_TEXT` (inside `ACTION_CHOOSER` too), then the system clipboard;
  - `Timing` logs `UITEST phase N ms: …` and `UITEST wait N ms: …` (waits of 500 ms or more) to logcat;
  - there is no known-issue allowlist.

### 0.5.5 Peers and the bridge

- **`PeerLines`** (shared) holds the line buffer and all matching. The platform `CliPeer`s implement only `fill()` and `type()`.
  - `sends` types the command, then consumes the first matching echo anywhere in the buffer (the command, or the sent item without its `HH:MM ` prefix);
  - also `types`, `expects`, `expectsMessage`, `eventually`, `eventuallyMessage`, `eventuallyMatching`, `nextLine`, `contactId`, `pending`;
  - line waits default to `LINE_TIMEOUT_MS` = 15 s; a timeout appends the next 10 buffered lines.
- **Desktop `CliPeer`:**
  - pty4j, 4000 × 50; `setpriv --pdeathsig KILL sh -c 'stty -echo -icanon; exec <cli> …'`;
  - args `-d`, `--user-display-name`, `-s <smp1>`, plus `--reactions`, `--mark-read`, `--show-full-links` from `PeerFlags`;
  - `HOME` = the peer directory `<jvm>/t<n>/peers/<name>`, `TMPDIR` inside it, `LC_ALL=C.UTF-8`, `TERM=xterm-256color`, working directory the repo root;
  - the database is copied from `<run>/peer-template` on first start; `cli()` returns at once, and the first use waits at most 60 s for `Current user:` and the prompt;
  - typing: write, wait until the input line equals the text (1.5 s unchanged window), else erase with DEL and retype (5 attempts); Enter, and Enter once more if the line still holds the text after 500 ms;
  - `VtScreen` is the terminal model.
- **Android `CliPeer`** talks to the host through `Bridge`: TCP `127.0.0.1:7777` (fixed, through `adb reverse`), a 60 s read timeout, one connection per instrumentation process.
- **The bridge** (`UiBridge.kt`, in the desktop harness; the Android runner starts it as a host JVM) owns `TestServers` and desktop `CliPeer`s, serves one client at a time, and stops all peers when the client disconnects. Protocol, one line per command:
  - `servers` → `ok <smp1> <xftp1>`;
  - `start <name> <flags, comma-separated>` → `ok` once the peer has started;
  - `type <name> <text>` → `ok`;
  - `drain <name>` → `lines N`, then N lines;
  - `stop <name>` → `ok`;
  - any failure → `err <message>`.

  Matching runs device-side in `PeerLines`. Bridge peer directories (`<bridge jvm>/bridge-peers/<nanoTime>-<name>`) are not deleted.

### 0.5.6 Servers

- `tests/UITestServers.hs` (cabal flag `uitest`, `exe:uitest-servers`) takes `<dir> <portBase>`, runs **`smp1` only** (persistent store, port base + 1), prints `ready smp1 smp://…@localhost:<port>` and exits on stdin EOF. It has no control protocol.
- `TestServers`: 120 slots (`slot-<k>.lock` in the run directory), base 20000 + 100 k. It is spawned under `setpriv --pdeathsig KILL` from the test thread during the first reset (in the bridge, from its main thread).
- `configureServers(smp, xftp)` disables the operator groups and adds a custom group with `smp1` and an `xftp1` address on base + 2 that **nothing serves**. Any XFTP test fails until P1b.

### 0.5.7 Helpers and timeouts

`Helpers.kt`:
- `described`; `goBack()` (desktop clicks `described("Back")`, Android presses system Back);
- `quiesce()`: no API in flight and no event for 1 s, bounded by 30 s;
- `connect(peer)`: a GUI link from `apiAddContact`, joined by the peer; `connectPeers`, `hostGroup`, `CliPeer.createGroup`, `CliPeer.createOneTimeLink`;
- `openContextMenu`: waits for no API in flight, settles, then right-clicks (Android: long press);
- `joinGroupFromList`: opens the row, confirms, waits for the host's "joined" line and for `MemConnected`, then `quiesce`.

`WhatsNewScreen.VERSION` reads the app's `lastVersion` by reflection.

| Wait | Desktop | Android |
|---|---|---|
| `poll` default | 10 s, plus up to 60 s while the same API calls stay in flight | same |
| Connections (`CONNECT_TIMEOUT_MS`) | 60 s | 60 s |
| Peer line / peer start | 15 s / 60 s | 15 s / 60 s (in the bridge); bridge socket read 60 s |
| `quiesce` / `drain` | 30 s / 30 s, then stuck items are abandoned | same |
| `restart()` | 30 s | 60 s |
| App startup in `bootOnce` | — | 180 s |
| Hard timeout per test | 240 s: thread dump, `Runtime.halt(3)`; the JVM's remaining tests are lost (15.4 item 20) | 300 s: thread dump, process killed; the runner resumes |
| Gradle task | 12 h | — |

### 0.5.8 Known-issue policy (desktop)

`KNOWN_APP_ISSUES` lists "Controller is not initialized" and "database closed" (15.5 item 3). When the scene runner throws:
1. the coroutines-test wrappers (`UncaughtExceptionsBeforeTest`, `DiagnosticCoroutineContextException`) are dropped;
2. if every remaining error matches somewhere in its cause chain and none is an `AssertionError`:
   - a test whose body completed passes, logged as `UITEST known app issue, not failing the test`;
   - a test whose body never started is retried (at most 3 attempts);
   - a test whose body started but did not complete fails;
3. any other error fails the test, with the suppressed errors listed.

Android has no allowlist.

### 0.5.9 Runners and Gradle

**Desktop: `scripts/uitest.sh [gradle args]`.**
- Refuses to start with less than 3000 MB `MemAvailable`. Takes `flock -n build/uitest/runner.lock`. Creates `build/uitest/run-<timestamp>` (with `jvmtmp` and `cache`) and keeps the newest 6 run directories.
- Outside the namespace:
  - `cabal build -O0 -f uitest --builddir=dist-uitest exe:simplex-chat exe:uitest-servers`, paths from `cabal list-bin`;
  - in the background, the peer database template (`--maintenance -e /v -t 0`, accepted when the output says "no active user in maintenance mode");
  - checks that the native libs are staged;
  - `./gradlew -g .gradle/uitest-home --no-daemon :desktop:cmakeBuildAndCopy :common:desktopTestClasses :common:uitestPrepare`, then `--stop`; skipped when a fingerprint (build files, the cpp sources outside `libs`, `ls -l` of the staged libs except `libapp-lib.so`) matches `build/uitest/prepare.key`;
  - writes `build/uitest/env.properties` (`libAppLib`, `runDir`, `repoRoot`, `cli`, `servers`).
- Adds `-PuiForks=N`, N = (MemAvailable MB − 2500) / 1000 clamped to 1..4, unless `-PuiForks` is given.
- Inside `unshare -rn --pid --fork --kill-child --mount-proc` (loopback up, multicast route): `./gradlew -g … --offline --no-daemon :common:desktopUiTest -PuiRunDir=<run> -PuiPass=1 <args>`. One pass. Every Gradle call uses `-Pkotlin.compiler.execution.strategy=in-process`.

**`desktopUiTest`** (`common/build.gradle.kts`; a `KotlinJvmTest` that runs only with `SIMPLEX_UITEST_RUNNER=1`):
- includes `chat/simplex/app/ui/**`, which `desktopTest` excludes;
- `forkEvery` = `-PuiForkEvery`, default **0** (one JVM per fork runs many classes); `maxParallelForks` = `-PuiForks`; 2 GB heap; 12 h timeout; `-PuiAllowEmpty`;
- environment: `XDG_DATA_HOME`, `XDG_CONFIG_HOME` and `FREETYPE_PROPERTIES` removed, `GSETTINGS_BACKEND=memory`, `XDG_CACHE_HOME=<run>/cache`;
- JVM: `en_US`, UTC, `java.io.tmpdir=<run>/jvmtmp`, heap dump on OOM and `-XX:ErrorFile` crash logs into the run directory; headless, skiko, env and IO-parallelism properties;
- `testLogging` with full exceptions and standard streams; results under `common/build/{test-results,reports/tests}/desktopUiTest/pass1`.

`uitestPrepare` only resolves `desktopTestRuntimeClasspath`. `uitestBridgeClasspath` copies the harness classes to `build/uitest/bridge-classes` and writes `build/uitest/bridge.classpath`.

**Android: `scripts/uitest-android.sh <app.apk> <androidTest.apk> [am instrument -e …]`.**
- Requires a previous `scripts/uitest.sh` run (for `env.properties`, the CLI and `uitest-servers`) and 4500 MB `MemAvailable`.
- Environment:
  - `UITEST_SERIAL`: any adb device; Waydroid is then not used;
  - `WD_INSTANCE` (default 2), `WD_OWNER` (default `uitest-$USER`);
  - `UITEST_APP_ID` (default `chat.simplex.app.debug`);
  - `ADB`, `JAVA_HOME` (defaults: `adb` from `ANDROID_HOME`/`ANDROID_SDK_ROOT` or `PATH`, `java` from `JAVA_HOME` or `PATH`).
- Setup:
  - rebuilds the bridge classpath only when one of its 7 sources is newer;
  - claims the Waydroid instance and starts it in the background if needed;
  - starts the bridge (`setsid java -cp … UiBridge 7777`) and kills it by `$!` on exit;
  - installs the app APK unless its sha256 and `lastUpdateTime` match the stamp, and always the test APK;
  - `adb reverse` for 7777, base + 1 and base + 2.
- Each pass: `pm clear`, grants POST_NOTIFICATIONS, CAMERA and RECORD_AUDIO, then `am instrument -w -r`. After "Process crashed" it resumes with `-e notClass <finished tests>` (at most 4 passes).
- Output in `build/uitest-android/run-<timestamp>/`:
  - `results.txt`, one line per test, `<class>#<test> <code>`: 0 pass, −2 fail, −3/−4 skipped, or `crashed`;
  - `instrument-<n>.txt`, `instrument.txt`, `logcat.txt` (`logcat -G 16M`), `timings.txt`, `failures/`, `bridge.log`.
- The exit status is non-zero on any −2 or `crashed`, or when there are no results.
- It does **not** run `wd.sh net isolate` (open item). The container keeps internet access during a run, and isolation rests on the local server configuration and on onboarding opening no connection.

APKs: `ANDROID_TEST=1 bash ~/build/android.sh` (run in the worktree) builds the app APK and publishes the test APK next to it.

### 0.5.10 How to run

```sh
cd apps/multiplatform
scripts/uitest.sh                                   # all desktop tests
scripts/uitest.sh --tests '*ChatListRowsTest*'      # one class (any Gradle test filter)
scripts/uitest.sh -PuiForks=1 -PuiForkEvery=1       # serial, one JVM per class
ANDROID_TEST=1 bash ~/build/android.sh              # app APK + androidTest APK
scripts/uitest-android.sh <app.apk> <test.apk>      # all Android tests on Waydroid instance 2
scripts/uitest-android.sh <app.apk> <test.apk> -e class chat.simplex.app.ui.main.ChatListRowsTest
UITEST_SERIAL=<serial> scripts/uitest-android.sh <app.apk> <test.apk>   # another adb device
```

### 0.5.11 Design items not built yet (P1b+)

| Design | Not built |
|---|---|
| 4, 5, 9.4 | typed semantics, keys everywhere, key lint: superseded for now by the minimal-app-code rule (13) |
| 6 | the real-time clock target and input-lead correction (the loop advances a fixed 16 ms, Android 48 ms, so composition delays fire late in real time); the end-of-test dialog drain; closing alerts with Escape or by reflection (alerts are only dropped with `hideAllAlerts` at the next reset); the end-of-test `ctrlInitInProgress` wait; `setCatchNonTestRelatedExceptions(false)`, `UiUncaught`, `allowUncaught` and composition-error bookkeeping; JVM poisoning, the `Slow` category, the EDT probe and per-shot allowances (the hard timeout halts the JVM instead) |
| 7.1, 7.1.1 | JUnit categories (`-PuiTags`, `-PuiExcludeTags`, `Smoke`, …); pinned fonts (`FONTCONFIG_FILE`); the `shots`, `shotTolerance`, `pass` and `strict` system properties; Kover and `uitestCoverageMerge`; `uitestPrepare` steps 2–4 (stable and fixture CLI downloads, migration lists) and the migration check; the second (`Multicast`) pass, the empty-run check, screenshot cleanup and coverage merge |
| 7.2, 7.3 | fixtures (`app(…, fixture)`, `restart(fixture)`); `RecordingNtfManager`, `UiTestPlatform` and the seams of 11.2; deleting `dataDir/tmp`; the `CC.ShowVersion` check; the lock-screen restart path and the alert exit of the restart poll; the `changingActiveUserMutex` check, manual `migrationState` handling, and the `OnboardingSharedState`/`BadgeStore` roots |
| 7.4, 7.5 | `UiDriver`/`UiNode`/`InProcessDriver`; `MR.strings` lookups and exactly-one matching; `hold`, `dragTo`, `hover`, `clickAtTextRange`, `press(key, modifiers)`, `wait(d)`, `composeField()` (placeholder); the report directory with semantics dump, peer tails and command log (only the failure PNG and the message exist) |
| 7.6, 7.7, 7.8 | `UiFiles` dialog responders (pending dialogs are only completed empty at reset); `LocalClipboard`; a throwing URI handler; toast and notification recording; screenshots; coverage |
| 8.1 | the Stable CLI and `CliVersion`; `--mute` (the D-Bus notification concern of 8.3 stands); `--xftp-server`; `$HOME/Downloads`; the start–stop–start cycle; the `! `/`> `/`\ ` typing rule; `PeerFlags.key`, `PeerServers`; `expectsAny`, `repeatUntil`; `stop`/`start`/`snapshot`/`restore` inside a test; `RelayPeer`; mobile hosts |
| 8.2 | `xftp1` (configured but not served), `smp2`, `xftp2`, the names resolver, the control protocol, `restoreBaseline` |
| 9.1–9.3 | `GuiUser`, `ChatRow` and the typed row classes; `uiTest(realTimes)`, `app(name, onboarded, fixture)`, `connect(gui, peer)`, `relay`, `mobileHost`, `servers`, `allowUncaught`, `screenshot`, `shouldHave`; the one-`app()`-per-test rule; `…CompatStableTest` subclasses |
| 10.1 | `CatalogueTraceabilityTest`, stub generation, the catalogue in the repo; a traceability rule for the desktop-only (`chat.simplex.app.ui.desktop`) and Android-only (`chat.simplex.app.ui`, `chat.simplex.app.ui.back`) packages |
| 11 | local HTTP and SOCKS stubs, committed media fixtures, multicast tests, server control, SQL fixtures |
| Android | notification tests; the 10-run exit check; `wd.sh net isolate` in the runner; `testTagsAsResourceId` and `ui-test-manifest` (not needed by the instrumented layer); the debug network-security-config (approved, 13) |

## Terms

Paths are repo-relative. **C** = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common`, **D** = `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common`, **A** = `apps/multiplatform/common/src/androidMain/kotlin/chat/simplex/common`, **Hs** = `src/Simplex/Chat`. Every `file:line` reference in the plan and the catalogue is to master at `d72f68e6d` (2026-10-10), checked out at `~/wt-uitest-ref`.

- **Key** — a value of type `UiKey`: a dotted id plus an optional entity argument, e.g. `chatList.row` + `@12`. Rendered as one `testTag` string, `chatList.row[@12]`.
- **Semantics node** — the unit Compose tests find and act on. `clickable`, `Button`, `IconButton`, `Switch`, `Tab`, `DropdownMenuItem` merge their children into one node.
- **Driver** — the object that finds nodes and performs actions, backed by `SkikoComposeUiTest` (the scene runner's receiver, 7.2) in the test JVM.
- **Screen object** — a Kotlin class for one screen whose methods are user actions and whose properties are what the screen shows.
- **Peer** — a non-GUI user in a test: a CLI process driven by its terminal text, as in the Haskell tests (`CliPeer`, also used for remote-desktop mobile hosts).
- **Relay** — a chat relay CLI process in headless mode (`RelayPeer`, 8.1); it has no terminal, only a log, and takes commands only through `run(cmd)`.
- **Sub-area** — one `###` section of a catalogue file (e.g. 01 §7 "1-time link"); the `Area` column inside a section is finer and only labels rows.

## 1. Goals and non-goals

Goals:
- Tests that read like the CLI tests and name what the user sees.
- Stable lookups: by key or by `MR.strings` key, never by position or pixel.
- Assertions on typed state (unread count, delivery status, item metadata, sender, verification, incognito, muted). (Deferred by the 2026-10-14 minimal-app-code rule, 13: states are read through the model, visible texts or content descriptions, 0.5.2.)
- Version matrices: the GUI on the current build, peers on the current CLI or a stable release CLI.
- Local runs: headless, no Xvfb, no display, no real network, deterministic waits, failure output with a semantics dump and a screenshot (no test assertion reads pixels; committed screenshots are compared outside the test logic, 7.7).

Non-goals for this plan:
- Badge display (see the decisions above).
- A remote automation server, tests against real AppImages, and tests with two GUIs.
- CI integration (tests run locally; a CI job can call the same runner script later).
- Android-only UI (catalogue `05-android-only.md`) is a separate effort. The catalogue tests themselves run on Android too: since 2026-10-13 the Android harness shares screens, helpers and test bodies through `common/src/uiTestShared` (13, 0.5.4).
- Pixel assertions inside tests: a test's own checks read semantics only; committed screenshots (7.7) are compared outside the test logic and fail a test only with `-PuiShots=verify`.

The parts of the app that leave the process are tested up to their boundary: calls up to the browser launch (a `LocalUriHandler` stub records the call link), VLC playback and voice recording behind approved seams (11.2), OS notifications (recorded), file choosers (completed by the test). Tray and window chrome are not rendered. Sections 7.6 and 11 list each surface.

## 2. Architecture

(As built: see 0.5. Differs: no `UiDriver` layer, each platform has a `UiScope` with the same methods; no typed semantics; `uitest-servers` runs `smp1` only; no `RelayPeer`.)

```
runner script          uitest.sh: build + fetch outside, then Gradle inside a loopback-only network namespace
  │
tests (DSL)            uiTest { val alice = app("alice"); val bob = cli("bob") ... }   // peer version from peerVersion (10.1)
  │
screen objects         ChatListScreen, ChatRow, ChatScreen, NewChatSheet, ConnectPlanAlert, UserPickerScreen, ...
  │
UiDriver               node(key) / node(text) / nodes(matcher) → UiNode; input; poll; dump
  │                    skiko test scene (manual clock) + real core over JNI, one GUI per test JVM
app code               Modifier.ui(Ui.X.Y), semantics { unread = ... }    (keys + typed semantics)
peers                  CliPeer: CLI binary under a pty, sends/expects on terminal lines (mobile hosts too); RelayPeer: headless relay
servers                uitest-servers (SMP, second SMP, XFTP, second XFTP, names resolver), once per test JVM
```

## 3. Keys

### 3.1 Type and modifier

(As built: see 0.5.2. Differs: the flag is `uiTestHooksEnabled` and also gates `UiIdle`; Android sets it through `initUiTags(context)` in `A/ui/UiTags.android.kt`, called from `SimplexApp`; `Ui` holds only the keys tests use, with no `Ui.Alert.Title`, `Ui.Modal.Back` or `ModalPlacement` local; `described("Back")` replaces `Ui.Modal.Back`.)

New file `C/ui/Ui.kt` (commonMain). The key code compiles into every build, but tags are emitted only in test and debug builds (decided 2026-10-12: test tags are for testing only): `Modifier.ui` is a no-op unless `uiTagsEnabled` is set, which the desktop harness does in `bootJvm` (the `desktopTest` source set sees `internal` members of `:common`) and, on Android, a new public common function called from `SimplexApp.initMultiplatform()` sets it when the app is debuggable (`ApplicationInfo.FLAG_DEBUGGABLE`, true for the `debug` build type unless `local.properties` sets `debuggable=false`, android/build.gradle.kts:56; `BuildConfig` belongs to the `:android` module, which can't set an `internal` var, and no existing common init has a `Context`); desktop release packages and Android release builds never set it.

```kotlin
@Immutable
class UiKey(val id: String, val arg: Any? = null) {
  val tag: String get() = if (arg == null) id else "$id[$arg]"
  fun option(value: Any) = UiKey("$id.option", value)
  fun part(name: String) = UiKey("$id.$name", arg)
}

@Volatile internal var uiTagsEnabled = false                     // set by the test harness (desktop) or a debug build (Android)
fun Modifier.ui(key: UiKey?): Modifier = if (key == null || !uiTagsEnabled) this else testTag(key.tag)

object Ui {
  object ChatList {
    val Bar = UiKey("chatList.bar")                                   // DefaultAppBar: title = Bar.part("title")
    val List = UiKey("chatList.list"); val Search = UiKey("chatList.search"); val NewChat = UiKey("chatList.newChat")
    val UserPicker = UiKey("chatList.userPicker")
    fun Row(chatId: String, remoteHostId: Long? = null) = UiKey("chatList.row", rowArg(chatId, remoteHostId))
    fun RowMenu(chatId: String, action: String, remoteHostId: Long? = null) = UiKey("chatList.row.menu.$action", rowArg(chatId, remoteHostId))
    fun Tag(tag: Any) = UiKey("chatList.tag", tag)
    private fun rowArg(chatId: String, rh: Long?) = if (rh == null) chatId else "$rh:$chatId"
  }
  object Chat {
    val Items = UiKey("chat.items")
    val Bar = UiKey("chat.bar"); val Info = UiKey("chat.info"); val Banner = UiKey("chat.banner")   // title (opens info) = Bar.part("title")
    val Message = UiKey("chat.compose"); val Send = UiKey("chat.send"); val Attach = UiKey("chat.attach")
    fun Item(itemId: Long) = UiKey("chat.item", itemId)
    fun ItemMenu(itemId: Long, action: String) = UiKey("chat.item.menu.$action", itemId)
    fun MemberAvatar(groupMemberId: Long) = UiKey("chat.memberAvatar", groupMemberId)
  }
  enum class AlertSource { Shared, PrivacySensitive }
  object Alert {
    fun Title(src: AlertSource) = UiKey("alert.title", src)
    fun Confirm(src: AlertSource) = UiKey("alert.confirm", src)
    fun Dismiss(src: AlertSource) = UiKey("alert.dismiss", src)
    fun Option(src: AlertSource, name: String) = UiKey("alert.option.$name", src)
  }
  object Modal { fun Back(placement: ModalPlacement?) = UiKey("modal.back", placement) }
  // ... one nested object per UI area (Search, Screen, NewChat, Share, UserPicker, Settings, Group, …)
}
```

- **Typed, hierarchical, compile-checked.** A renamed key breaks the build of the test that uses it.
- **Why not composable function names:** references to `@Composable` functions (`::SendMsgView`) don't compile, and the composable names in `sourceInformation` are stripped or unreliable after inlining.
- **Release cost:** one volatile read per keyed modifier and no semantics property, because release builds never set `uiTagsEnabled` (above); in test and debug builds, one semantics property per keyed node (`testTag` isn't announced by screen readers).
- **Naming rule:** `area.component[.part]` in camelCase. Entity keys always take the core id (`chat.id`, `cItem.id`, `groupMemberId`, `userId`), never a list index, except where no id exists (section 3.4). Lazy lists get a list key too (`Ui.ChatList.List`, `Ui.Chat.Items`), used by `scrollTo` (7.4).
- **Two alert managers.** `AlertManager.shared` and `AlertManager.privacySensitive` both render (`privacySensitive` at C/App.kt:242, only when the user is authorized; `shared` at :255), each showing its last alert (C/views/helpers/AlertManager.kt:432). Alert keys therefore carry the manager.
- **Modal placement.** `ModalManager(placement: ModalPlacement? = null)` (C/views/helpers/ModalView.kt:97): on Android every manager has placement `null`, and the floating terminal renders `ModalView` with no manager (D/DesktopApp.kt:219). The placement is provided as a CompositionLocal (default `null`) in `ModalManager.showInView()` (:201/:217, already composable and already wrapping each modal in a `CompositionLocalProvider`).

### 3.2 Placement rule

`Modifier.ui(key)` goes **on the same modifier chain as the `clickable`/`combinedClickable`/`toggleable`** (or on the component that owns the click, such as `ExposedDropdownMenuBox`), before it. A tag on a child of a merged node is hidden from the default (merged) tree, because testTag's merge policy keeps the parent's value (verified in ui 1.8.2). Examples: `SectionItemView` Row (C/views/helpers/Section.kt:213), `SimpleButtonFrame` (C/views/helpers/SimpleButton.kt:90), the bubble in `ChatItemView` (C/views/chat/item/ChatItemView.kt:327).

### 3.3 Helper parameters (one edit keys many screens)

Add `uiKey: UiKey? = null` (named `uiKey` because some helpers already have a parameter called `key`, e.g. `PassphraseField(key: MutableState<String>, …)`, C/views/database/DatabaseEncryptionView.kt:317) or a fixed internal key to these shared helpers. Site counts are approximate.

| Helper | file:line | Sites | Key |
|---|---|---|---|
| `SectionItemView` (+ `WithoutMinPadding`, `LongClickable`, `SpaceBetween`) | C/views/helpers/Section.kt:198/221/236/264 | 274 / 7 / 5 / 28 | param |
| `SectionItemWithValue`, `SectionViewSelectable(Cards)` | Section.kt:287/155/178 | 4 / 3 / 2 | param; options `key.option(value)` |
| `SectionTextFooter` (AnnotatedString overload; the String overload at :314 delegates) | Section.kt:319-328 | ~100 | new `uiKey` param on its `Text`, so link spans in footers (SetSimplexNameView.kt:217) have a keyed ancestor (3.5, 9.4) |
| `InfoRow` (read-only) / `InfoRowEllipsis` (clickable, via `SectionItemViewSpaceBetween`) | Section.kt:370/437 | 64 / 1 | param |
| `SettingsActionItem`, `SettingsActionItemWithContent` | C/views/usersettings/SettingsView.kt:384/393 | 98 / 37 | param |
| `SettingsPreferenceItem`, `PreferenceToggle(WithIcon)` | SettingsView.kt:423/437/453 | 23 / 11 / 6 | param, applied to the Switch (the row has no click) |
| `SharedPreferenceToggle`, `SharedPreferenceToggleWithIcon` (2 overloads) | C/views/usersettings/CallSettings.kt:85/101/131 | 1 / 2 | param on both overloads (`SharedPreferenceRadioButton` :155 has no call sites) |
| `DefaultSwitch` | C/views/helpers/DefaultSwitch.kt:14 | 24 | already has `modifier` |
| `ExposedDropDownSettingRow` → `ExposedDropDownSetting`; `ExposedDropDownSettingWithIcon` | C/views/helpers/ExposedDropDownSettingRow.kt:156 → 25; :89 | 30 + 1 (Android only, CallView.android.kt:361) | key on `ExposedDropdownMenuBox` (:37-38, owns `onExpandedChange`); `DropdownMenuItem` :68 (and :133 in `…WithIcon`) = `key.option(value)` |
| `IntSettingRow`, `TimeoutSettingRow` | C/views/usersettings/networkAndServers/AdvancedNetworkSettings.kt:436/495 | 5 / 6 | param + options |
| `DropdownCustomTimePickerSettingRow` | C/views/helpers/CustomTimePicker.kt:83 | 2 | param |
| `ItemAction` (5 overloads; all context menus) | ChatItemView.kt:1096/1116/1142/1170/1190 | 92 | param (no modifier today) |
| `SimpleButton*` → `SimpleButtonFrame` | SimpleButton.kt:26/54/39/70 → 88 | 23 | param in the frame |
| `NavigationButtonBack/Close/Menu`, `ShareButton` | C/views/helpers/DefaultTopAppBar.kt:128/137/155/146 | ~20 | fixed keys |
| `ModalView` back button | C/views/helpers/ModalView.kt:59-61 | 60 + ~115 `showModal`/`showModalCloseable` | fixed `Ui.Modal.Back(placement)` from the CompositionLocal of 3.1 |
| `DefaultAppBar` title / search | DefaultTopAppBar.kt:24 (:39, :84) | 10 | param `uiKey` (the screen's bar, e.g. `Ui.ChatList.Bar`, `Ui.Chat.Bar`; the chat list and the chat are shown side by side, ChatListView.kt:612, ChatView.kt:1443), title and search = `uiKey.part("title"/"search")`; trailing `buttons` keyed at call sites |
| `AppBarTitle` | C/views/helpers/AppBarTitle.kt:20 | 111 | `Ui.Screen.X` screen-root key, param |
| AlertManager built-in buttons | C/views/helpers/AlertManager.kt:155/159, 196/200, 239/244, 400/408/415, title 441 | ~440 alert calls | fixed `Ui.Alert.*(source)` from the rendering manager (3.1) |
| `TextEditor` / `PlainTextEditor` | C/views/helpers/TextEditor.kt:29/108 | 8 / 2 | `TextEditor` has `modifier`; `PlainTextEditor` gets the param |
| `DefaultBasicTextField`, `DefaultConfigurableTextField`, `PassphraseField` | C/views/helpers/DefaultBasicTextField.kt:30/115 (+ reveal button :189); DatabaseEncryptionView.kt:317 | 9 / 11 | through their `modifier` (applied to the text field; PassphraseField's at :357); reveal = `key.part("reveal")` |
| `PlatformTextField` (compose message field) | C/platform/PlatformTextField.kt:12; desktop actual D/platform/PlatformTextField.desktop.kt:44-58 | 1 | the expect/actual signature gets the param (the desktop actual has no `modifier`); fixed `Ui.Chat.Message` |
| `SearchTextField` | C/views/helpers/SearchTextField.kt:32 (clear :122) | 5 | param `uiKey`, clear = `uiKey.part("clear")`: the chat list passes `Ui.ChatList.Search` (ChatListView.kt:799), the other direct callers their screen's key (GroupChatInfoView.kt:1390, AddGroupMembersView.kt:217, NewChatSheet.kt:494), `DefaultAppBar` its own `uiKey.part("search")`; a fixed key would match twice while the chat list and an end-pane screen are both shown |
| `ProfileNameField` | C/views/WelcomeView.kt:415 | 13 | param |
| `OnboardingActionButton` (expect :187 + 2 actuals, and :190), `TextButtonBelowOnboardingButton` | C/views/onboarding/SimpleXInfo.kt:187/190/223 | 25 / 11 | :190 already has `modifier`; the expect overload and :223 get the param |
| `InfoViewActionButton` | C/views/chat/ChatInfoView.kt:1061 | 13 | param |
| `ActionButton` (2) | C/views/newchat/NewChatSheet.kt:785/828 | 10 | param |
| `OpenChatButton` | C/views/chat/group/GroupMemberInfoView.kt:879 | 3 | param |
| `UserPickerOptionRow`, `UserProfilePickerItem`, `DevicePickerRow` | C/views/chatlist/UserPicker.kt:483/399/493 | few | param (entity: userId / remoteHostId) |
| `ChatListNavLinkLayout` (expect + 2 actuals) | C/views/chatlist/ChatListNavLinkView.kt:1027; android actual :14 (combinedClickable :26); desktop actual :28 (combinedClickable :39) | 8 | param, passed `Ui.ChatList.Row(chat.id, chat.remoteHostId)` |
| `ChatItemView` bubble | ChatItemView.kt:69 (:327) | 4 | `Ui.Chat.Item(cItem.id)` |
| `SendMsgButton` | C/views/chat/SendMsgView.kt:452 (:466) | 3 (:136/:246/:256) | fixed `Ui.Chat.Send` |
| `SelectableCard` | C/views/onboarding/SetNotificationsMode.kt:68/73 | 5 | `key.option(value)` |

Exception: `showAlertDialogButtons(Column)` takes caller-built buttons (44 calls in 25 files, built from `SectionItemView`, `TextButton` and small helper buttons; e.g. C/views/newchat/ConnectPlan.kt:463, :567, :599). Those are keyed at the call site with `Ui.Alert.Option(source, name)`.

### 3.4 Entity keys

| Key | Emitted at | Id |
|---|---|---|
| `ChatList.Row(chatId, remoteHostId)` + row menu | C/views/chatlist/ChatListView.kt:1099 → ChatListNavLinkView.kt:67-164; menu :278-640 | `chat.id` (`@12`, `#5`) and `chat.remoteHostId` |
| `NewChat.Contact(chatId)` | NewChatSheet.kt:341/430/752 → C/views/contacts/ContactListNavView.kt:47/107 | `chat.id` (also add LazyList keys there) |
| `Share.Row(chatId)` | C/views/chatlist/ShareListView.kt:213 | `chat.id` (add LazyList key) |
| `ChatList.Tag(id)` | ChatListView.kt:1295-1318; C/views/chatlist/TagListView.kt:100/115 | `PresetTagKind` / `chatTagId` |
| `Chat.Item(itemId)` (+ reactions, select) | C/views/chat/ChatView.kt:2398 → :1922 → ChatItemView.kt:327; reactions :143/195/363; select overlay ChatView.kt:2236 | `cItem.id`; merged groups `merged.newest().item.id` |
| `Chat.MemberAvatar(groupMemberId)` | ChatView.kt:2086 | `groupMemberId` |
| Members (info list, channel members, support, mentions, relays, delivery) | C/views/chat/group/GroupChatInfoView.kt:778/1099; ChannelMembersView.kt:115; MemberSupportView.kt:131; C/views/chat/group/GroupMentions.kt:226; ChannelRelaysView.kt:79; C/views/chat/ChatItemInfoView.kt:430 | `groupMemberId` |
| Add-members contacts | C/views/chat/group/AddGroupMembersView.kt:310/321 | `contact.apiId` |
| Relays | C/views/newchat/AddChannelView.kt:452/545; AddGroupRelayView.kt:138; C/views/chat/ComposeView.kt:1961/2070; chat-relay lists ProtocolServersView.kt:92, OperatorView.kt:247 | `groupRelayId` (AddChannelView), `relayId` (AddGroupRelayView), `groupMemberId` (ComposeView :2070 iterates `GroupMember`), relay id or address (settings lists) |
| User profiles | UserPicker.kt:399 (android :55/81, desktop :47-54); C/views/usersettings/UserProfilesView.kt:177; ComposeContextProfilePickerView.kt:260; C/views/newchat/NewChatView.kt:443/452 | `userId` (add LazyList keys at NewChatView.kt:443/452) |
| Remote hosts / controllers | UserPicker.kt:533/493; C/views/remote/ConnectDesktopView.kt:420 | `remoteHostId` / `remoteCtrlId` |
| Operators | C/views/usersettings/networkAndServers/NetworkAndServers.kt:215/674; onboarding ChooseServerOperators.kt:211 | `operatorId` |
| Servers | ProtocolServersView.kt:105/139; OperatorView.kt:335-452 | server address (ids are null for unsaved servers) |
| Commands, versions, wallpapers, tabs, image previews | CommandsMenuView.kt:231; ChatItemInfoView.kt:327; Appearance.kt:398; NewChatView.kt:136; ComposeImageView.kt:35 | command label (menus have no keyword) / version index / enum / index (add LazyList keys at Appearance.kt:398 and ComposeImageView.kt:35) |
| Badges ledger, pay, level | C/views/badges/BadgesLedgerView.kt:83, BadgesPayView.kt:148, BadgesYourLevelView.kt:125 (pay and level are unreachable today, 9.4) | `entryId` / period / level |

### 3.5 Controls that need more than a click on a keyed node

- **Links inside messages.** `ClickableText` (C/views/chat/item/TextItemView.kt:426) handles clicks with `pointerInput`, so links have no click semantics. `UiNode.clickAtTextRange(range)` reads the text node's `GetTextLayoutResult` (in the unmerged tree), computes the bounding box of the link's characters and clicks there. No app change. Texts built with `withLink(LinkAnnotation.Url…)` (e.g. the Wefunder link, WhatsNewView.kt:1210; SetSimplexNameView.kt:217) are different: Compose 1.8.2 gives each link span its own child node with `OnClick` and `LinkTestMarker`, which no `Modifier.ui` reaches, so tests click it as the `LinkTestMarker` child of the keyed `Text` with `performClick`; the Wefunder `Text` gets its key at its call site, and footer links get theirs through `SectionTextFooter`'s `uiKey` (3.3).
- **Voice record press-and-hold** (SendMsgView.kt:418): hold-versus-tap is decided by wall-clock time (`firstTapTime + 1000L < System.currentTimeMillis()`, GestureDetector.kt:265-267), so `UiNode.hold(1.2.seconds)` presses on the node (`performMouseInput { moveTo(center); press() }`; a new injection otherwise presses where the previous one left the pointer, InputDispatcher.kt:145-153), waits 1.2 s of real time through the polling loop of section 6 (so the Compose clock advances and the press interaction progresses; a plain `Thread.sleep` would freeze composition), and releases (with the recorder seam of 11.2).
- **Long-press and right-click menus:** `longClick()` / `rightClick()` on the keyed row.
- **Keyboard:** `press(key, modifiers)` sends key events to the focused node (Enter, Shift+Enter, Up, Escape, Ctrl+C/V, digits, Backspace).
- **Drag and hover:** `dragTo(other)` (text selection between items, list reordering) and `hover()` (hover-only buttons) use `performMouseInput`.
- The selection toolbars (C/views/chat/group/GroupMembersToolbar.kt, C/views/chat/SelectableChatItemToolbars.kt) use plain `IconButton`s (GroupMembersToolbar.kt:62/71/80, SelectableChatItemToolbars.kt:95/104/113); the `pointerInput` at :54/:87 only swallows background gestures. They are keyed like any button.

## 4. Typed semantics

(As built: not built. Superseded for now by the 2026-10-14 minimal-app-code rule (13); tests read these states through the model, visible texts or content descriptions, 0.5.2.)

New file `C/ui/UiSemantics.kt` (its properties are set only when `uiTagsEnabled`, like the tags of 3.1):

```kotlin
val UnreadKey = SemanticsPropertyKey<UiUnread>("SimplexUnread")
var SemanticsPropertyReceiver.unread by UnreadKey
data class UiUnread(val count: Int, val mentions: Int, val unreadChat: Boolean)

val ItemMetaKey = SemanticsPropertyKey<UiItemMeta>("SimplexItemMeta")
data class UiItemMeta(val status: String, val sentViaProxy: Boolean?, val encrypted: Boolean?, val signed: Boolean?, val timedTtl: Int?, val edited: Boolean, val live: Boolean)

val SenderKey = SemanticsPropertyKey<String>("SimplexSender")   // display name of a group item's sender, on every item, not only the first of a run
// likewise: VerifiedKey, IncognitoKey, MutedKey(ntfsMode), FavoriteKey, SelectedKey, FileStatusKey(CIFileStatus kind), SubStatusKey

val VolatileKey = SemanticsPropertyKey<Boolean>("SimplexVolatile", mergePolicy = { parent, _ -> parent })   // content that changes every run (durations, QR codes, links, codes; displayed times come from the display clock); masked in screenshot comparison (7.7); set on the volatile child and not merged into its row or item
fun Modifier.uiVolatile() = if (!uiTagsEnabled) this else semantics { set(VolatileKey, true) }
```

Each property is set on the **merged node that carries the key**, not on the child icon, so one lookup returns the row or item with all its state:

| State | Rendered at | Set on |
|---|---|---|
| Unread / mentions / unread mark | C/views/chatlist/ChatPreviewView.kt:461-500, `UnreadBadge` :591; UserPicker.kt:639 | `ChatList.Row`, user rows |
| Muted, favorite | ChatPreviewView.kt:502-521 | `ChatList.Row` |
| Incognito | ChatPreviewView.kt:557, ChatInfoImage.kt:59, ContactPreviewView.kt:145 | rows, info headers |
| Verified | ChatPreviewView.kt:97; ChatView.kt:1742; ChatInfoView.kt:737; GroupMemberInfoView.kt:775; VerifyCodeView.kt:67-69 (in the `Row` at :66) | rows, title, info headers, the security code screen's status `Row` (VerifyCodeView.kt:66-72; it has no click, so it gets its own read-only key `Ui.VerifyCode.status` with `mergeDescendants`, area K3) |
| Item status, sent via proxy, encrypted, signed, timed, edited | C/views/chat/item/CIMetaView.kt:69-125 | `Chat.Item` (`ItemMetaKey`) |
| Live message | C/views/chat/item/FramedItemView.kt:287, TextItemView.kt:176-183 | `Chat.Item` (`ItemMetaKey.live`) |
| Group sender | ChatView.kt:2039/2097 (drawn outside the bubble, only on the first item of a run) | `Chat.Item` (`SenderKey`) |
| File status | CIFileView.kt:139-177, CIImageView.kt:73-98, CIVideoView.kt:498-561 | `Chat.Item` |
| Subscription status | C/views/helpers/SubscriptionStatusIcon.kt:12, ChatView.kt:1635 | indicator node |
| Selected / current | `ChatListNavLinkLayout(selectedChat)`, UserPicker.kt:425, TagListView.kt:153 | row |

Matchers in the test library are `SemanticsMatcher`s over these properties: `unread(n)`, `itemStatus("sent")` (the `UiItemMeta.status` string; e.g. `sent`, `delivered`, `error`), `edited()`, `sender(name)`, `verified()`, `incognito()`, `hasTagPrefix(prefix)`, ...

## 5. Rollout of keys ("everywhere")

(As built: not followed. Superseded for now by the 2026-10-14 minimal-app-code rule (13): keys only on elements tests act on or must read, 0.5.2.)

### 5.1 Size

The helpers of 3.3 have about 1,000 call sites (the inventory counted 776 interactive ones, excluding `InfoRow` and `AppBarTitle`). Ad-hoc `clickable`/`IconButton`/`TextButton`/`Button` sites: about 300 in about 100 files of commonMain and desktopMain. In total about 190 files are touched. Keying the helpers reaches about 70% of interaction sites.

### 5.2 Key areas

This is a map of where keys go, not a PR sequence; PRs follow the phases of section 12 (one PR per catalogue sub-area, carrying the keys that sub-area's tests need). The file counts are rough and overlap.

| Area | Scope | Files (approx.) |
|---|---|---|
| K0 | `Ui.kt`, `UiSemantics.kt`, `Modifier.ui`, `UiIdle.kt` and its hooks (6; pending confirmation, 13); every helper parameter of 3.3; fixed keys in AlertManager, ModalView (+ placement local), the DefaultTopAppBar navigation and share buttons, PlatformTextField, `SendMsgButton` (`Ui.Chat.Send`); `uiKey` parameters on DefaultAppBar and SearchTextField; `uiVolatile()` (7.7) on QRCode, the invitation/address link and security-code texts, ServersSummaryView statistics, call and voice durations, server addresses with ports, link-preview URLs, multi-relay lists, incognito profile names, the remote pairing port field and session codes (desktop and Android), the pasted desktop invitation (Android), the chat console's line list, the "Message queue info" alert (a volatile-text parameter on `showAlertMsg`), the chat-relay address editor, the live-message send-button pulse, the typing indicator and animated images; the display-clock seam (11.2) in ChatModel.kt's time formatters, `FloatingDate`, the grouping check and the SimpleX-name sale countdown | ~30 |
| K1 | Chat list, user picker, new chat sheet, paste link, connect plan alerts; unread/muted/incognito semantics on rows | ~15 |
| K2 | Chat view: toolbar (ChatView.kt:1194-1433), items + item menus, compose/send/attach (ComposeView, SendMsgView, Compose*View, ContextItemView, CommandsMenuView, GroupMentions, SelectableChatItemToolbars), item info; item meta, sender and file semantics | ~30 |
| K3 | Chat/contact/group/member info, member lists, add members, preferences, verify code, group/channel creation and links, relays | ~25 |
| K4 | Address screens, settings tree (SettingsView, profiles, privacy, appearance, network and servers, operators, calls, developer, database, migration), badges screens | ~60 |
| K5 | Onboarding (SimpleXInfo, WelcomeView, ChooseServerOperators, YourNetwork, WhatsNewView), remote desktop link, misc | ~20 |

Ad-hoc sites with many raw clickables to key directly (from the inventory): ChatListView.kt (20: FAB 480, toolbar 531/547/586/706/750, tag chips 1212-1474), ChatView.kt toolbar and FABs (1242-1433, 2850/3266/3284), ChatItemView.kt (195/270/363), item views (CIVoiceView 267/305/351, CIVideoView 322-465, CIImageView 112-147, CIFileView 193, FramedItemView 126/294/405), SendMsgView.kt (316, 376-418, 439, 466, 504), ComposeView.kt (1285, 1319, 1562, 2119/2144), WhatsNewView.kt (13 sites), Appearance.kt (9), UserProfileView.kt (6), NewChatView.kt (136, 218-253, 589/612, 719, 751/758), OperatorView.kt (528-858), YourNetwork.kt (`ConfigureRoutersButton`, :172-177).

## 6. Synchronization (no sleeps, no hangs)

(As built: see 0.5.2–0.5.4, 0.5.7, 0.5.8. Differs: each poll iteration advances a fixed 16 ms (Android 48 ms) with no real-time target or input-lead correction, so composition delays fire late in real time; the `sendCmd` item wraps the `withContext(Dispatchers.IO)` block instead of sitting inside it; no end-of-test dialog drain, no Escape or reflection closing of alerts (they are dropped by `hideAllAlerts` at the next reset), no change to the coroutines-test collector and no `UiUncaught`; known app errors follow 0.5.8; the hard timeout is 240 s then `Runtime.halt(3)` (Android 300 s, process killed, runner resumes), with no poisoning, `Slow` category or EDT probe.)

**Manual clock, one polling loop.** Each test runs with `mainClock.autoAdvance = false`. The driver has one loop, `poll(what, timeout) { condition }`: sleep about 16 ms of real time, advance the Compose clock towards real time, evaluate the condition; repeat until it holds or the deadline passes. Every lookup, assertion and peer `expects` goes through it, and so does every wait inside an input action (`hold`, 3.5). The only fixed wait is `wait(d)` (7.4), the same loop running for `d` of real time with no condition, used only where a catalogue row names the interval (for example "retry more than 3 s after the failure").

- **Clock target.** `advanceTimeBy` rounds up to whole 16 ms frames (AbstractMainTestClock.kt:44) and input injection advances the clock as well, so advancing by each iteration's elapsed time would run the clock ahead of real time (up to about 2×). The loop instead keeps `target = clockAtTestStart + realTimeSinceTestStart` and advances by `target - mainClock.currentTime` when that is positive and otherwise by 0, with `ignoreFrameDuration = true` (an advance by 0 still runs the test scheduler's current tasks, which with the `StandardTestDispatcher`, 14, run inside `advanceScheduler` — a render runs the scene's own flush queue — and the closing `runTest`; the advance renders no frame); input injection advances the clock by each event's time (InputDispatcher.skiko.kt:246-259; a `longClick` advances the long-press timeout + 100 ms, MouseInjectionScope.kt:395), so after each input action the driver adds the clock's lead over the loop's target (`mainClock.currentTime - target`, which excludes what settle loops already added) to `clockAtTestStart`, as screenshot settle loops do for what they advance (7.7) — otherwise no frame renders until real time catches up (measured: state written after a `longClick` appeared after about 0.6 s instead of 17–21 ms); virtual time may lag the target but never leads it after these corrections (input leads: about 60–76 ms per click, 50 ms per key press, 600 ms per `longClick`; `performTextInput` doesn't move the clock): a settle loop advances the clock by whole seconds and then adds what it advanced to `clockAtTestStart`, so the loop continues from there instead of freezing until real time catches up, and from then on runs that far (up to about 11 s per shot) ahead of real time, as it does by the input leads added after each action; composition delays pending at a shot therefore fire early. Each advance is capped (e.g. 100 ms per iteration), so if rendering a frame costs more than real time the clock lags instead of starving the EDT (P1 measures the render cost, 14).

- The app's work outside Compose (core threads, receiver loop, EDT jobs, `withBGApi` executor) runs in real time anyway; advancing the clock by the real elapsed time keeps composition-scoped delays and animations (the live-message update loop `delay(3000)`, SendMsgView.kt:545-550; toast timers; the 1 s status polls) in step with it.
- With `autoAdvance = false`, Compose's own `waitForIdle` returns as soon as layout is done (it ignores idling resources and infinite animations), so no animation loop can block a test — neither indeterminate spinners nor the finite animations restarted in a loop (SendMsgView.kt:534-544), nor the `delay` loops (TextItemView.kt:176-183, the call timer ChatView.kt:1264-1268). Progress screens can be asserted while their command runs.
- The EDT (`withApi`, `withContext(Dispatchers.Main)`; on desktop `Dispatchers.Main` is kotlinx-coroutines-swing, which posts with `invokeLater`) is not part of the Compose clock; the polling loop simply waits for the condition.

**`UiIdle` (busy tracking).** A commonMain object, always on, records each in-flight item with its start time:
- core commands in `ChatController.sendCmd` (C/model/SimpleXAPI.kt:936-958), added before and removed in `finally`, both inside its `withContext(Dispatchers.IO)` block (:939-957): a composition coroutine's continuation dispatched after the scene's closing `runTest` never runs with the `StandardTestDispatcher` (14), so a `finally` outside that block could leave the item in flight; the periodic status polls (`GetAgentSubsTotal`, ChatListView.kt:683-692; `GetAgentServersSummary`, ServersSummaryView.kt:706-722) are excluded by command type, not by the `log` flag (user commands such as badge redemption also use `log = false`, SimpleXAPI.kt:580);
- events in `startReceiver` (SimpleXAPI.kt:782-831): added when `recvMsg` returns an event (:805), removed in `finally` after `processReceivedMsg` and `trySend` (which run inside `withTimeoutOrNull`, :808-811, with exceptions caught at :823-828); processed events are also counted, with the time of the last one, and items started and finished are counted, for the screenshot quiet wait and settle check (7.7);
- jobs started by `withBGApi`/`withLongRunningApi`/`withApi` (C/views/helpers/Utils.kt:34-49), added on launch and removed on completion. `withLongRunningApi` gets a parameter `idle: Boolean = true`; jobs that never end, or that wait long and are cancelled by the reset, pass `idle = false`: `PopChatCollector.init` (C/model/ChatModel.kt:827; one collector per `ChatsContext`) and the app-update checker `createUpdateJob` (D/views/helpers/AppUpdater.kt:170-176, which re-launches itself after `delay(24 h)`; its `withLongRunningApi` call at :171 is the one site to mark; reached only through `setupUpdateChecker()`, which desktop `main` calls and `bootJvm` doesn't, so the mark only guards a test that starts it), and the agent-error alert timer of `ProcessedErrors` (C/views/helpers/ProcessedErrors.kt:19, which can wait up to 60 s and is cancelled by the reset, 7.3).

Coroutines launched outside these helpers are not tracked (e.g. the 1 s delayed secondary-chat close, SimpleXAPI.kt:3271-3277, attachment processing, ComposeView.kt:299/310/457, and the file dialogs awaited in composition-scoped coroutines: the attach pickers, ComposeView.desktop.kt:24-38, and the QR save's first dialog, QRCode.kt:91-94); waits on their effects are ordinary polled assertions.

It is used in four ways:
- an assertion's deadline (default 10 s) is extended while any item that was already in flight at the original deadline is still in flight, up to the original deadline + 60 s, so legitimately long commands (export of a 1 GB file, a 30 s SOCKS timeout) don't make waits expire while a stream of short receiver events can't keep a failing wait alive; tests that wait longer pass an explicit timeout;
- `drain()` waits until nothing is in flight (at most 30 s) before a reset; items still in flight when it gives up are listed in the log and marked abandoned, and abandoned items are ignored from then on by every use of "in flight" (later drains, the deadline extension, the end-of-test dialog poll, the screenshot quiet wait and settle check, 7.7), so a stuck job costs one reset, not every reset or shot in the JVM;
- the failure report lists what was in flight;
- screenshots wait for it to be quiet (nothing in flight, no event processed for 1 s) before settling, and the settle check requires nothing in flight at the second of two equal captures and nothing started, finished or processed between them (7.7).

**End of each test.** After a passing body of a catalogue test, `uiTest` first drains file dialogs in passes of at most 5: each pass runs `waitForIdle`, then polls (at most 10 s, like the quiet wait of 7.7) until some `DialogState` is pending or UiIdle has nothing in flight, then completes every pending `DialogState` with an empty result (as below); it stops after a pass that completed none (measured with the QR flow ending after `respondSave(<dir>)`: passes completed 1 then 0, and the end-of-test shot was quiet and settled, 10 of 10; without the poll the first pass completed none, the second dialog opened afterwards, and the shot was `unquiet` and `unsettled`, 10 of 10). The poll is needed because a dialog the body answered last can open the next one only after a `waitForIdle` resumes its waiter, and then from a `Dispatchers.Default` job (e.g. `respondSave(<dir>)` on the QR save's folder dialog, 7.6: `shareFile` runs in `withLongRunningApi`, Utils.kt:46-49, Share.desktop.kt:26); the drain's own empty results open no further dialog (`saveTempImageUncompressed` returns null, Utils.desktop.kt:240-241, and QRCode.kt:95-96 shares only a file). A dialog awaited from a tracked job keeps that job in flight (`withLongRunningApi`, e.g. CIFileView.kt:109-117), so 7.7's quiet wait and settle check could never succeed while it is open; the attach pickers (ComposeView.desktop.kt:24-38) and the QR save's first dialog (QRCode.kt:91-94) wait in composition-scoped coroutines, which aren't tracked. File choosers are native windows that the shot doesn't show either way. Then the end-of-test screenshot is captured (7.7; not in `REALTIME` tests or with `-PuiShots=off`), and written only once the test has passed. Then, while the scene still exists, `uiTest` (in `finally`, so also after a failing body) closes what would otherwise leave coroutines waiting forever: it completes every pending `DialogState` with an empty result (completing the captured deferred, so a dialog that completes in between causes no `NullPointerException`) (null, or an empty list for `openMultipleDialog`, DesktopApp.kt:274; file choosers, 7.6; `clearTransientUi`, 7.3), then sends Escape to each dialog root until the rendered alert managers show no alert (alerts in a manager that isn't rendered go to the reflection path below) (at most 10 tries), which runs each alert's `onDismissRequest` and so completes the deferreds and continuations behind it (e.g. "Start chat?", `showStartChatAfterRestartAlert`, Core.kt:226-235; the retry alert of `sendCmdWithRetry`, SimpleXAPI.kt:842-869). `hideAllAlerts` alone only drops the composables (AlertManager.kt:48-50) and would leave `initChatController` waiting with `ctrlInitInProgress` set. An alert that remains after the tries is reported and fails the test. After the alerts are closed (by Escape or by the reflection path below), a plain loop — not `poll` — waits at most 10 s, as the reset does (7.2), for `ctrlInitInProgress` to clear (closing "Start chat?" only completes its deferred; initialisation then still loads the user's data before clearing the flag, Core.kt:170-188); if it is still set then, the JVM is poisoned (below).

**Uncaught errors in app coroutines.** The scene runner (`runInternalSkikoComposeUiTest`, 7.2, 14; `runDesktopComposeUiTest` goes through the same `withScene`) ends with `testScope.runTest { }` (`TestScope` at ComposeUiTest.skiko.kt:155, `runTest` at :201), and kotlinx-coroutines-test 1.8.0 (the version ui-test-desktop 1.8.2 brings) from the first `runTest` on keeps a JVM-wide exception collector enabled (`catchNonTestRelatedExceptions` is `true`): an unhandled error in any coroutine — the app's `withBGApi`/`withLongRunningApi`/`withApi` jobs catch nothing (C/views/helpers/Utils.kt:36-49), and about 36 API wrappers in SimpleXAPI.kt throw, as does `sendCmd` while the controller is null (:937) — is queued and thrown by the next `runTest`, from `withScene`'s `finally` — the same test's when it happens before its scene closes (body or end-of-test cleanup; as `UncaughtExceptionsBeforeTest`, replacing the test's own assertion error), the following test's when it happens after (reset). `bootJvm` therefore turns the collector off (`TestScopeKt.setCatchNonTestRelatedExceptions(false)`, public in the bytecode but internal in Kotlin, so called by reflection; it must run before any `runTest`, since once the collector is enabled it stays on and a later change makes every `runTest` fail its own check) and installs its own `Thread.setDefaultUncaughtExceptionHandler` (the desktop app installs none: `registerGlobalErrorHandler`, C/platform/UI.kt:20-22, is called only on Android, and window crashes go to `showApp`'s `WindowExceptionHandler`, D/DesktopApp.kt:42-43, which isn't rendered here). The handler records each error with its phase; where the `UiIdle` launch hook exists (6), an error from a job is attributed to the test and phase that launched it, so a job abandoned by `drain` can't fail a later test. An error during the test body fails that test at its end (its own assertion error, if any, stays the cause and the recorded errors are added as suppressed exceptions; a test that expects one wraps the step in `allowUncaught(T::class) { … }`, 9.2, which polls (6) until an error of type `T` is recorded, consumes it, and fails if none arrives — the error usually comes from a job on another thread after the click returns); an error during a reset, a restart or the end of a test is written to the run log (and to the failure report if the test fails) without failing any test.

Errors in composition-scoped coroutines (`LaunchedEffect`, `rememberCoroutineScope().launch`, e.g. VersionInfoView.kt:29-31 calls `sendCmd`, which throws while the controller is null) don't reach that handler: they go to the scene's own handler and are rethrown by the next `waitForIdle` (ComposeUiTest.skiko.kt:270-279), behind every node lookup (`getRoots`, :378-380), and by `withScene`'s `finally` (:202). `throwUncaught` clears what it throws (UncaughtExceptionHandler.kt:72-79), so whoever catches one owns it. They always fail the test, whatever phase they surface in: each `poll` iteration first calls `waitForIdle()` on its own and treats any throwable from it, except `InterruptedException` (the watchdog's, rethrown), as a composition error (so one that happens to be an `AssertionError` is not mistaken for a matcher failure), fails at once if the test has a recorded composition error (a `restartCore` that kept one goes on with a scene that no longer recomposes), and only then evaluates the lookup through `onAllNodes(…).fetchSemanticsNodes()` with its own counting (so anything thrown out of the fetch's own wait for idle is likewise a composition error, not a matcher failure), retrying its own matcher failures and the documented transient states (`scrollTo` waits until the list's `IndexForKey(key) >= 0` before `performScrollToKey`, Actions.kt:197-200; `UiNode.get`/`assert`/`child` evaluate their matchers inside the lookup's transform on the EDT, not on the test thread, Assertions.kt:258-259); each end-of-test step and `restartCore`'s waits for idle (including the one inside `setContent`, ComposeUiTest.skiko.kt:338) catch a `Throwable` from it, rethrow `InterruptedException` (the watchdog's), keep anything else as a composition error and continue; after the scene runner returns, `uiTest` fails the test with the first composition error, or, if the body has its own throwable, keeps that as the cause and adds the composition errors (and a different throwable the scene runner ended with) as suppressed. A failed effect cancels the scene's recomposer job (the recomposer's effect job is a child of it, not a `SupervisorJob`), so after a composition error the dialogs no longer recompose and Escape would keep hitting the same stale dialog root: the end-of-test step then completes the remaining alerts by reflection instead of Escape: each entry of `AlertManager`'s `alertViews` (AlertManager.kt:37) is a `ComposableLambdaImpl` whose `_block` is the show function's lambda, and the harness invokes the dismiss callback that lambda captured — `$onDismissRequest` for `showAlertDialog` (which covers "Start chat?", Core.kt:226-235, and the retry alert, SimpleXAPI.kt:880-888), `showAlertDialogStacked` and `showAlertDialogButtonsColumn`, `$onDismiss` for `showOpenChatAlert`, none for `showAlertDialogButtons`, `showAlertMsg` and `showAlertMsgWithProgress`, whose dismissal only hides — and then drops the entries with `hideAllAlerts`. The field names are compiler-generated, so P1's harness tests check them. The same path is used for every alert in a manager that isn't rendered when the test ends, not only after a composition error: `AlertManager.privacySensitive` while the app is locked has no dialog root (App.kt:240-242). The wait for `ctrlInitInProgress` described under "End of each test" follows. The captured callbacks are invoked on the EDT, as `clearTransientUi` is, since they write Compose state.

**Hard timeout and poisoning.** `uiTest` checks a poison flag on entry, before the scene runner (whose scene setup and teardown run on the EDT with `invokeAndWait`), and runs the scene runner on a dedicated daemon thread that the JUnit thread joins with a deadline: 2 min (10 min for tests in the `Slow` category), plus 28 s for each screenshot the test takes (10 s quiet wait plus 11 settle steps of about 1.4 s, the steps about 12% slower with the Kover agent attached, 7.8, measured on a toy project; P1 measures the step on the app and adjusts this), plus 10 s for each end-of-test drain pass that waits (6, "End of each test"; normally only the last pass waits, and it waits its full bound only while something stays in flight with no dialog opening, e.g. a long export) — the JUnit thread joins in slices and recomputes the deadline from a shot counter the body increments when each shot starts (a shot costs up to 10 s of quiet wait plus up to 11 settle steps — one before focus is cleared, then at most 10 — of about 1.4 s each on an animated scene, 7.7). When the deadline passes the JUnit thread first probes the EDT (`invokeLater` plus a 5 s latch), then writes the failure report while the scene is still as the test left it: the non-UI parts (peer tails, `UiIdle` list, command log) always; the semantics dump and screenshot only if the EDT probe succeeded, built on a helper thread with its own 20 s timeout, inside `runOnUiThread { … }` (the body thread can still be rendering on the EDT — input injection advances the clock itself, InputDispatcher.skiko.kt:246-259, and every node lookup renders a pending layout, ComposeUiTest.skiko.kt:273-279, 378-380 — while `captureToImage` snapshots on the calling thread, :356-358, so a capture off the EDT can catch a cleared surface; measured: 26 of 150 captures blank off the EDT, 0 of 150 inside `runOnUiThread`, which runs inline on the EDT without deadlock), which records a composition error that its `waitForIdle` surfaces instead of dropping it (`printToString` and `captureToImage` call `waitForIdle`, which renders through `invokeAndWait`, ComposeUiTest.skiko.kt:356-358/:378-380, and would hang the JUnit thread on a stuck EDT). Only then does it interrupt the body thread (the interrupt unwinds through the end-of-test cleanup and closes the scene, ComposeUiTest.skiko.kt:196-203). If the body thread is still alive 15 s after the interrupt, or the probe fails, or a reset's 10 s wait for `ctrlInitInProgress` expires (7.2), or `changingActiveUserMutex` is still held after `drain()` (7.3), or `ctrlInitInProgress` is still set 10 s after the end-of-test alert completion (above), or the staged core's migrations differ from the current CLI's (7.1), the JVM is *poisoned*: every later `uiTest` in it fails at entry with "JVM poisoned by <test>" without touching the EDT, so the rest of the class fails fast instead of hanging on a thread stuck in JNI or on the EDT. The Gradle task also has a `timeout` as a last backstop.

## 7. In-process driver

### 7.1 Gradle

(As built: see 0.5.9. Differs: `forkEvery` defaults to 0; no categories, pinned fonts, `shots`/`shotTolerance`/`pass`/`strict` properties or Kover; `uitestPrepare` only resolves `desktopTestRuntimeClasspath`, and `env.properties` (5 keys) is written by the runner; no migration check. Added: `kotlin.srcDir("src/uiTestShared/kotlin")`, heap dumps and `-XX:ErrorFile` crash logs in the run directory, `testLogging` with standard streams, and the `uitestBridgeClasspath` task for the Android bridge.)

In `apps/multiplatform/common/build.gradle.kts` the existing `val desktopTest by getting` (line 149) is replaced by:

```kotlin
val desktopTest by getting {
  languageSettings.optIn("androidx.compose.ui.test.ExperimentalTestApi")   // error-level opt-ins of the scene runner (7.2)
  languageSettings.optIn("androidx.compose.ui.test.InternalTestApi")       // runInternalSkikoComposeUiTest (14)
  languageSettings.optIn("androidx.compose.ui.InternalComposeUiApi")       // its semanticsOwnerListener parameter type, PlatformContext (ComposeUiTest.skiko.kt:91); set module-wide so no test file needs its own @OptIn
  dependencies {
    implementation(compose.desktop.uiTestJUnit4)  // the skiko test scene runner (ui-test-desktop 1.8.2, downloaded into the dedicated Gradle home by the outside step, 7.1.1)
    implementation(compose.desktop.currentOs)     // skiko native runtime
    implementation("org.jetbrains.pty4j:pty4j:0.13.13")   // pinned: 0.14.x needs kotlin-stdlib 2.4; 0.13.13 brings jna 5.14.0, slf4j-api 2.0.13
                                                          //   and raises kotlin-stdlib to 2.1.21 in the desktopTest classpath only (harmless)
  }
}
```

and the test tasks:

```kotlin
tasks.named<Test>("desktopTest") { exclude("chat/simplex/app/ui/**") }   // existing unit tests unchanged

tasks.register("uitestCoverageMerge") { /* merges the <run directory>/coverage/pass<n>.ic files that exist (one or two) through Kover's reporter and writes the HTML/XML report and summary; run by the runner after both passes (7.8) */ }

tasks.register("uitestPrepare") {                                         // run outside the namespace (7.1.1)
  // 1. resolve desktopTestRuntimeClasspath, so the offline run inside the namespace finds every runtime-only artifact
  // 2. download the stable CLI (-PuiStableTag) and fixture CLIs (-PuiFixtureTags), verify their published SHA-256 (mandatory) and set the executable bit
  // 2b. resolve Kover's koverJvmAgent and koverJvmReporter configurations (7.8), so the offline run and the merge find them
  // 3. run the current CLI (built before Gradle, 7.1.1) on a scratch database and record its chat and agent migration lists (7.1)
  // 4. write <rootProject>/build/uitest/env.properties (apps/multiplatform/build/uitest), including the run directory (-PuiRunDir) and the two lists
}

tasks.register<KotlinJvmTest>("desktopUiTest") {                       // KotlinJvmTest with targetName, so Kover instruments it (7.8)
  targetName = "desktop"
  onlyIf { System.getenv("SIMPLEX_UITEST_RUNNER") == "1" }              // only from uitest.sh: Kover makes check/build/koverVerify depend on every instrumented desktop KotlinJvmTest (7.8)
  val dt = tasks.named<Test>("desktopTest").get()
  testClassesDirs = dt.testClassesDirs; classpath = dt.classpath
  include("chat/simplex/app/ui/**")
  forkEvery = (findProperty("uiForkEvery") as String?)?.toLong() ?: 0    // 0 = one JVM per fork runs many classes (15.4 item 24); -PuiForkEvery=1 = one JVM per class
  maxParallelForks = (findProperty("uiForks") as String?)?.toInt() ?: 1
  maxHeapSize = "2g"
  timeout.set(Duration.ofHours(12))                                       // last backstop (6); `import java.time.Duration` at the top of the file: inside the script `java` resolves to the project's `java` extension, so `java.time.…` doesn't compile (found in P1a)
  filter { isFailOnNoMatchingTests = findProperty("uiAllowEmpty") == null }   // the runner's passes may each match nothing (7.1.1)
  useJUnit {
    fun cats(p: String) = (findProperty(p) as String?)?.split(",")?.filter { it.isNotBlank() }?.map { "chat.simplex.app.ui.$it" }?.toTypedArray()
    cats("uiTags")?.let { includeCategories(*it) }
    cats("uiExcludeTags")?.let { excludeCategories(*it) }
  }
  environment.remove("XDG_DATA_HOME"); environment.remove("XDG_CONFIG_HOME")
  environment("GSETTINGS_BACKEND", "memory")   // the system theme then resolves to light, whatever the host's GTK theme (7.7)
  environment("XDG_CACHE_HOME", "${findProperty("uiRunDir")}/cache")   // fontconfig's cache for the pinned font directory and JNA's native-library extraction, instead of the real ~/.cache (measured)
  environment("FONTCONFIG_FILE", file("src/desktopTest/uitest-fonts/fonts.conf").absolutePath)   // pinned fonts (7.7), committed beside fonts.conf, <dir prefix="relative">
  environment.remove("FREETYPE_PROPERTIES")                               // hinting overrides would change every screenshot (7.7)
  jvmArgs("-Duser.language=en", "-Duser.country=US", "-Duser.timezone=UTC", "-Djava.io.tmpdir=${findProperty("uiRunDir")}/jvmtmp")   // AppCommon.desktop.kt:19 captures defaultLocale at class init; UTC for screenshots (7.7)
  systemProperty("simplex.uitest.shots", (findProperty("uiShots") as String?) ?: "update")           // update | verify | off (7.7)
  systemProperty("simplex.uitest.shotTolerance", (findProperty("uiShotTolerance") as String?) ?: "0")
  // Kover agent on this task unless -PuiCoverage=off; forks append to one per-task report, copied per pass by the runner (7.8)
  systemProperty("java.awt.headless", "true")
  systemProperty("skiko.data.path", rootProject.file("build/uitest/skiko").absolutePath)
  systemProperty("simplex.uitest.env", rootProject.file("build/uitest/env.properties").absolutePath)
  systemProperty("simplex.uitest.pass", (findProperty("uiPass") as String?) ?: "1")   // failure reports go to <run directory>/report/pass<n> (7.5)
  systemProperty("simplex.uitest.strict", (findProperty("uiStrict") != null).toString())   // strict traceability (10.1, P7)
  systemProperty("kotlinx.coroutines.io.parallelism", "256")   // old receivers block IO threads up to 300 s each (7.3)
  outputs.upToDateWhen { false }                                          // the core, CLIs and servers are not task inputs; always run
  val pass = (findProperty("uiPass") as String?) ?: "1"                   // each runner pass keeps its own results (7.1.1)
  reports.junitXml.outputLocation.set(layout.buildDirectory.dir("test-results/desktopUiTest/pass$pass"))
  reports.html.outputLocation.set(layout.buildDirectory.dir("reports/tests/desktopUiTest/pass$pass"))
  binaryResultsDirectory.set(layout.buildDirectory.dir("test-results/desktopUiTest/pass$pass/binary"))
}
```

- `env.properties` holds the absolute paths of `libapp-lib.so` and its library folder, the current CLI, the stable CLI and the tag it was downloaded for, each fixture CLI and its tag, `uitest-servers`, the repo root, the run directory (port slots, 8.2; per-JVM directories, 7.1; failure reports, 7.5), and the current CLI's chat and agent migration lists (7.1). The catalogue is read from the test classpath (10).
- The JUnit category interfaces (`Smoke`, `StableCompat`, `NeedsRelay`, `NeedsNames`, `NeedsFixture`, `Multicast`, `Slow`) live in package `chat.simplex.app.ui`; Gradle loads every listed category up front.
- **Per-JVM data directories.** The app's paths are read once at class initialisation: `dataDir` from `XDG_DATA_HOME` or `user.home` (D/platform/Platform.desktop.kt:6-8), `desktopPlatform.configPath` (:7, :12; settings file at D/platform/Resources.desktop.kt:39-40), `tmpDir` from `java.io.tmpdir` (D/platform/Files.desktop.kt:15). The task removes `XDG_DATA_HOME` and `XDG_CONFIG_HOME`; the `UiTest` base class's companion initialiser — which runs before any app class is initialised (JUnit and Gradle load classes without initialising them) — registers the harness's single shutdown hook of 8.2 (Kover's agent has its own) (it stops peers and relays, closes `uitest-servers`' stdin and waits for it, then destroys the remaining descendant processes with `destroyForcibly()`, `ProcessHandle.current().descendants()` — which also covers the `gsettings monitor` process the app's theme detector starts, Theme.desktop.kt:8-11, which never exits on its own under the memory backend, measured one orphan per JVM; it first sets a shutting-down flag, after which `UiUncaught` drops errors, since destroying the `gsettings monitor` process makes the detector's reader thread throw an uncaught `NullPointerException`), creates a fresh directory per JVM, `<run directory>/jvm-<slot>-<pid>` (slots are reused by later JVMs, and a reused directory would bring back the previous JVM's peer databases, server stores and settings) (on the same filesystem as the checkout, not `/tmp`, which may be a small root filesystem; one test picks a sparse file over 1 GB), and sets `user.home` and `java.io.tmpdir` inside it. `bootJvm()` asserts that `dataDir` and `configPath` are inside that directory, failing fast otherwise. Setting `java.io.tmpdir` at runtime covers the app's `tmpDir`, but not the JDK's NIO `Files.createTempFile`/`createTempDirectory`, nor `File.createTempFile` once it has been used (measured, JDK 17), so the task also passes `-Djava.io.tmpdir=<run directory>/jvmtmp` (created by the runner) and library and harness temp files stay off `/tmp`.
- Native libs: `libsimplex.so` and GHC libs are staged into `apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-x86_64` (gitignored; `bash ~/build/stage-libs.sh` in the worktree, or upstream `scripts/desktop/build-lib-linux.sh`). `libapp-lib.so` comes from `:desktop:cmakeBuildAndCopy` (it links against the staged `libsimplex.so`, cpp/desktop/CMakeLists.txt:57). The staged core and the current CLI must be the same version: fixture SQL edits run the CLI with `-y` on the GUI's database (11.1), so a newer CLI would apply migrations the GUI's core doesn't know, and "current" peers would differ from the GUI. The check compares the migrations applied to a fresh database. `uitestPrepare` runs the current CLI once on a scratch database with `--user-display-name uitest -e "/sql chat select name from migrations order by name" -t 0`, and again with `/sql agent …` (not `--maintenance`, which exits on a database with no user, Hs/Core.hs:89-91; the CLI's `/v` prints only the version, View.hs:216, :2711-2716), and writes both lists to `env.properties` (dropping the `Current user:` line, the `name` header row and the trailing blank line). The first `resetForTest` in the JVM whose init succeeds (a fixture with a startup database error defers it to the next) sends `CC.ShowVersion()` (SimpleXAPI.kt:4110) through `chatController.sendCmd(null, …, log = false)` — not `apiGetVersion()`, which returns only the version (SimpleXAPI.kt:2737-2742), and without logging, so the console of the JVM's first test shows no extra items — and reads `chatMigrations`/`agentMigrations` (`UpMigration.upName`, DatabaseUtils.kt:109-112) from the `CR.VersionInfo` result (:6901; the core reads the database's `migrations` table sorted by name, Hs/Library/Commands.hs:3671-3676, the same rows as the CLI's query), and fails the current test and poisons the JVM, with both lists in the report, when they differ.

#### 7.1.1 Runner script

(As built: see 0.5.9. Differs: one pass (`-PuiPass=1`), so no `Multicast` pass, empty-run check, screenshot cleanup or coverage merge, and `-PuiTags` has no effect; added: the 3000 MB `MemAvailable` guard, the `-O0` cabal build with `cabal list-bin`, the peer database template, the fingerprint skip of the outside Gradle step, automatic `-PuiForks`, in-process Kotlin compilation; `--stop` runs only after the outside step, when it runs.)

Tests are run by `apps/multiplatform/scripts/uitest.sh [gradle args]`:

1. **Outside the namespace** (network allowed; the script runs in `apps/multiplatform`, and relative paths below are from there): the script first runs `mkdir -p build/uitest` and takes `flock -n` on `build/uitest/runner.lock`, held through both passes (a second runner would rewrite `env.properties` and re-copy `libapp-lib.so` under the first run's JVMs, so it exits instead), then creates a fresh run directory `$PWD/build/uitest/run-<timestamp>` and its `jvmtmp` subdirectory (the test JVMs' launch-time `java.io.tmpdir`, 7.1) (absolute, since `uitestPrepare` and the test JVM resolve paths from `common/`; port-slot locks, 8.2) and removes older `run-*` directories except the last 5 (keeping 5 old ones plus the new one, since a run keeps each JVM's GUI data and server stores, the peer databases of failing tests, and its reports), then runs `./gradlew -g "$PWD/.gradle/uitest-home" --no-daemon :desktop:cmakeBuildAndCopy :common:desktopTestClasses :common:uitestPrepare` with `-PuiRunDir=<that directory>` (written into `env.properties`) and the user's `-PuiStableTag` and `-PuiFixtureTags` (they select the binaries `uitestPrepare` downloads), then `./gradlew -g "$PWD/.gradle/uitest-home" --stop`. Both steps use their own Gradle user home, `.gradle/uitest-home` in `apps/multiplatform` (git-ignored, and outside `build/`, which the root `clean` task deletes, build.gradle.kts:112-113; filled by this outside step, which has network; the first run downloads the wrapper distribution and dependencies once, an estimated 1–2.5 GB on disk): a Gradle process holds cache locks (`caches/journal-1`, shared across Gradle versions) that a Gradle process on the other side of the namespace can't ask it to release, so with a shared home an outside daemon makes the inside run fail with "Timeout waiting to lock journal cache", and a run of several hours makes every outside build on the same home fail the same way (the daily prebuild timer, `~/build/linux.sh`, IDE syncs; verified both ways, the outside build failed after 69 s). A build or IDE sync of the same checkout still conflicts during a run, on the project's own `.gradle/8.12/fileHashes` lock (verified: failed after 64 s), so other work during a run uses another worktree. Every invocation on that home uses `--no-daemon`, so its only daemon is the single-use one that exits with its build; the script still runs `./gradlew -g … --stop` before entering the namespace and before pass 2, as a guard against a process left by an interrupted run. Before the Gradle invocation, the outside step runs `cabal build -f uitest --builddir=dist-uitest exe:simplex-chat exe:uitest-servers`, because `uitestPrepare` runs the built CLI (a separate build directory, because the desktop library is built with `client_library` and sharing `dist-newstyle` would rebuild the package each time).
2. **Inside the namespace** (8.3; the inner Gradle runs get `SIMPLEX_UITEST_RUNNER=1`, without which `desktopUiTest` is skipped, 7.1): the script re-executes itself under
   ```sh
   self=$(readlink -f "$0")   # set at the top of the script, so the re-exec still resolves whatever directory step 1 runs in
   unshare -rn --pid --fork --kill-child --mount-proc sh -c '
     ip link set lo up && ip link set lo multicast on && ip route add 224.0.0.0/4 dev lo src 127.0.0.1 &&
     exec "$0" --inside "$@"' "$self" "$@" &
   trap 'kill -KILL $!' INT TERM; wait $!
   ```
   The outer script doesn't `exec`: `unshare --fork` blocks SIGINT and SIGTERM while it waits, so the outer script kills it with SIGKILL on Ctrl+C or SIGTERM, and `--kill-child` tears the namespace down; it returns `wait`'s status (verified with util-linux 2.38.1: no leftover processes, status 130). Because `unshare` is started with `&` from a non-interactive shell, SIGINT is ignored for everything inside the namespace, and `dash` can't trap a signal ignored on entry; so Ctrl+C is handled only by the outer trap. In `--inside` mode the script is PID 1 of the pid namespace, which receives only signals it handles; it runs `trap 'kill -TERM -1 2>/dev/null; exit 143' TERM` (for a SIGTERM sent from inside), records each pass's exit code and exits non-zero if either failed.
   and runs the Gradle passes there: `./gradlew -g "$PWD/.gradle/uitest-home" --offline --no-daemon :common:desktopUiTest …`. The whole Gradle process runs inside, because the test worker connects back to the Gradle process over loopback (a JVM alone in the namespace can't reach it; verified). `-g` is needed anyway, because inside the user namespace the uid is 0 and `user.home` resolves to `/root`. The pid namespace kills every leftover process (peers, relays, servers) when the run ends.
3. **Two passes.** The script reads the user's `-PuiTags`, `-PuiExcludeTags`, `--tests`, `-PuiForks`, `-PuiShots` and `-PuiCoverage` arguments (the last two decide the whole-run screenshot and coverage steps, 7.7, 7.8). Pass 1 runs with the user's arguments and `Multicast` added to the exclusions. Pass 2 runs only if the user's `uiTags` is empty or contains `Multicast`: it runs the user's arguments (exclusions, `--tests`, `-PuiShots`, `-PuiCoverage`, `-PuiStrict`, `-PuiShotTolerance`) with `-PuiTags=Multicast` and `-PuiForks=1` (8.3) replacing the user's `-PuiTags` and `-PuiForks` rather than appending to them. Pass 1 is skipped when the user's `uiTags` is exactly `Multicast`. Both passes add `-PuiRunDir=<run directory>` (the task's `java.io.tmpdir` is its `jvmtmp`, 7.1), `-PuiAllowEmpty`, so a pass whose filters match nothing doesn't fail Gradle ("No tests found for given includes", verified), and `-PuiPass=1` or `-PuiPass=2`, so each pass writes its XML and HTML results to its own directory (a second Gradle invocation of the same test task deletes the first one's results, verified with Gradle 8.12). The script deletes both pass directories before pass 1, and fails the run if neither pass executed a test (it counts the XML results in `common/build/test-results/desktopUiTest/pass1` and `pass2`, the `:common` project's build directory). After both passes, when no test filter (`uiTags`, `uiExcludeTags`, `--tests`) was given and `uiShots` is `update`, it deletes committed screenshots whose test no longer exists or that a passing test no longer produced, and regenerates the screenshot docs index (7.7). After both passes it also merges the per-pass coverage reports and writes the coverage report and summary (7.8), unless `-PuiCoverage=off`.

Usage: `scripts/uitest.sh` (full run), `scripts/uitest.sh -PuiTags=Smoke -PuiExcludeTags=StableCompat` (smoke run; P1b, no categories yet), `scripts/uitest.sh --tests '*OneTimeLink*Test'`, `scripts/uitest.sh -PuiForks=2` (default chosen from free memory: 10.2).

### 7.2 Boot, reset and restart

(As built: see 0.5.3 and 0.5.4. Differs: `bootJvm` has no collector change, uncaught handler, `platform` or seams, uses a no-op `NtfManager` and applies the theme; `resetForTest(user)` has no fixture, uses `deleteChatDatabaseFilesAndState()` (so `dataDir/tmp` is not deleted), and runs `PrefStores.restore` → `clearUiState` → `TestServers.start()` → `DbTemplate.installIfPresent` (so the template's key is installed after the prefs are restored, as 15.1 says; the code below has the opposite order); no `MigrationCheck`; `TestRoot` provides `LocalClipboardManager` and `LocalUriHandler` only, with no throwing option.)

`initHaskell()` is private to the desktop module (desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt:50-91), so the harness repeats its essential steps. `acquireSingleInstance()` (D/SingleInstance.kt:33) is skipped. The reset adds no app code beyond `UiIdle` (6): it uses existing functions, public state and reflection from test code (private fields, and the model walk of 7.3).

```kotlin
object UiTestApp {
  val dbPrefix: String get() = dbAbsolutePrefixPath             // C/platform/Files.kt:24, D/platform/Files.desktop.kt:20
  fun bootJvm() {                                             // once per JVM, from the base class
    TestScopeKt.setCatchNonTestRelatedExceptions(false)        // by reflection, before the first runTest (6)
    Thread.setDefaultUncaughtExceptionHandler(UiUncaught)     // records uncaught app errors with the current phase (6)
    check(dataDir.startsWith(UiEnv.jvmDir) && File(desktopPlatform.configPath).startsWith(UiEnv.jvmDir))   // 7.1 per-JVM data directories
    System.load(UiEnv.libAppLib); initHS()                      // C/platform/Core.kt:18
    Locale.setDefault(Locale.ENGLISH)
    uiTagsEnabled = true                                      // test tags and typed semantics are emitted only in test/debug builds (3.1)
    ntfManager = RecordingNtfManager                          // lateinit, C/platform/NtfManager.kt:18
    platform = UiTestPlatform                                 // same PlatformInterface as desktop/Main.kt:65-90
    simplexWindowState.windowState = WindowState(size = DpSize(1600.dp, 900.dp))  // lateinit, D/DesktopApp.kt:271
    tmpDir.mkdirs()                                           // databaseExportDir; desktop main creates it
    runMigrations()                                           // C/platform/AppCommon.kt:46; writes lastMigratedVersionCode
    appPrefs.appearanceBarsBlurRadius.set(0)                  // toolbar blur off in every test, onboarding included, for performance (user decision 2026-10-13);
                                                              //   set before the capture, so every reset restores it; tests about blur turn it on
    PrefStores.capture()                                      // settings.properties, themes.properties, themes.yaml as they are now (7.3)
    ModelSnapshot.capture()                                   // initial values of ChatModel, ChatsContext, AppLock fields (7.3)
    UiSeams.installFakes()                                    // recorder, audio/video provider, Ctrl+V clipboard reader (11.2);
                                                              //   the display clock is fixed or real per test, from uiTest(realTimes) (7.7)
  }

  fun resetForTest(user: String?, fixture: ((dbPrefix: String) -> Unit)?) {   // called by app() at the start of each test
    UiIdle.drain()                                            // let the previous test's jobs finish (6)
    stopAndCloseCore()                                        // as deleteStorageAndRestart does (C/views/localauth/LocalAuthView.kt:55-68):
                                                              //   wait while ctrlInitInProgress (at most 10 s, else poison the JVM with a report);
                                                              //   stopChatAsync only if chatRunning == true (it also sets chatRunning = false);
                                                              //   chatCloseStore only if ctrl != null && ctrl != -1; then setChatCtrl(null)
                                                              //   (DatabaseView.kt:581), which also stops the receiver on the old controller
    UiIdle.drain()                                            // events in flight when the receiver stopped, before clearUiState (as restartCore, 7.3)
    deleteDatabaseFiles()                                     // the files deleteChatDatabaseFilesAndState removes (C/views/database/DatabaseView.kt:556-599),
                                                              //   plus dataDir/tmp (coreTmpDir, the core's temp folder and the agent's XFTP work folder; deleteChatDatabaseFilesAndState removes java.io.tmpdir/simplex and dataDir/migration_temp_files, not dataDir/tmp, DatabaseView.kt:572-574; a transfer still running when the core stops leaves its prepared copy there, a failed one is removed by the agent, FileTransfer/Agent.hs:513-517),
                                                              //   without its asynchronous UI clearing (:585-596); clearUiState does that synchronously;
                                                              //   then re-creates filesDir, tmpDir and wallpapersDir as it does (:570, :574, :576), since they are otherwise created once per JVM
    DbTemplate.installIfPresent()                             // the run's pre-migrated empty database and its key (<run directory>/db-template),
                                                              //   so initChatController opens instead of migrating (15.1: reset 7.2 s → 0.94 s);
                                                              //   the JVM that finds none migrates once, closes the store, copies the files and key,
                                                              //   publishes them with an atomic rename and re-opens; FIXTURE rows replace the files after this
    PrefStores.restore()                                      // 7.3
    clearUiState()                                            // 7.3, synchronously on the EDT: model, navigation, window state
    TestServers.restoreBaseline()                             // 8.2
    fixture?.invoke(dbPrefix)                                 // FIXTURE rows: prepare the database while the core is stopped (11.1)
    if (user != null) TestPrefs.preset()                      // 11.1 "Pre-set prefs"; onboarding boots keep defaults
    runBlocking { initChatController() }                      // C/platform/Core.kt:70; fresh DB → random key, no prompt; a fixture DB made
                                                              //   without a key opens with the empty key (DatabaseUtils.kt:51-62)
    if (!migrationsChecked && chatModel.chatDbStatus.value == DBMigrationResult.OK) MigrationCheck.compare()   // 7.1: CC.ShowVersion's chatMigrations/agentMigrations vs the CLI's lists; poison on mismatch
    if (chatModel.chatDbStatus.value != DBMigrationResult.OK) return clearTransientUi()   // fixture with a startup DB error
    val fixtureUser = chatModel.currentUser.value             // a fixture database may already have a user (initChatController started it)
    if (fixtureUser == null && user == null) return clearTransientUi()   // onboarding: Core.kt:153-166 sets Step1 and starts without a user
    val u = fixtureUser ?: runBlocking { chatController.apiCreateActiveUser(null, Profile(user!!, "", null)) }!!  // display name only
    chatModel.localUserCreated.value = true
    appPrefs.onboardingStage.set(OnboardingStage.OnboardingComplete)
    chatModel.currentUser.value = u                           // getUserServers/setUserServers read the user id from the model (currentUserId)
    if (fixtureUser == null) runBlocking { chatController.startChat(u) }   // SimpleXAPI.kt:663
    runBlocking { TestServers.configure(u) }                  // 8.2: /_servers with the operator groups disabled and one custom group;
                                                              //   assigns the fresh getServerOperators result to chatModel.conditions;
                                                              //   after startChat: before it the core answers chatNotStarted (found in P1a)
    setLastVersionDefault(chatModel)                          // WhatsNewView.kt:1024, so What's New doesn't open
    clearTransientUi()                                        // 7.3: modals, passcode views, alerts and toasts stale jobs put back;
                                                              //   keeps chatDbStatus, localUserCreated and the chats startChat loaded
  }

  fun restartCore(fixture: ((dbPrefix: String) -> Unit)?) { … }   // RESTART rows, 7.3
}
```

- Each test gets a new composition from the scene runner: `runInternalSkikoComposeUiTest(width = 1600, height = 900, coroutineDispatcher = StandardTestDispatcher()) { setContent { TestRoot { AppScreen() } } }` (14; the fallback is `runDesktopComposeUiTest(width = 1600, height = 900)`; density 1, so 1600 px = 1600 dp). The size matches `windowState` and leaves room for the end pane: END modals expand the window to `388·√fontScale·2 + 590` dp (ModalView.kt:146-148, Resources.desktop.kt:106-108), 1366 dp at font scale 1 and about 1540 dp at the largest desktop font scale, 1.5 (Appearance.kt:251).
- `TestRoot` provides the test stubs as composition locals: in-memory `LocalClipboardManager` **and** `LocalClipboard` (app code uses the former, ~40 sites; Compose text fields and `SelectionContainer` use the latter in 1.8.2), a recording `LocalUriHandler` (links, the browser launch for calls, D/views/call/CallView.desktop.kt:157-174). The stubs belong to the test (so a migration test can carry a link across a `restart()`).
- Window-only features (tray, window-state persistence, the file-dialog windows, toasts' `LaunchedEffect`, floating terminal, close policy; D/DesktopApp.kt:40-257) are not rendered; 7.6 says how each is covered.

### 7.3 Isolation between tests, and restart inside a test

(As built: see 0.5.3, 0.5.4. Differs: `ModelSnapshot` unwraps and walks `Lazy` fields (15.4 item 26); its roots are `ChatModel`, `AppLock`, `ConnectProgressManager`, `ChannelRelaysModel`, `BadgeModel`, `VideoPlayerHolder`; no `changingActiveUserMutex` check, no manual `migrationState`, `ProcessedErrors` timers not cancelled explicitly; `restartCore` drains once and has no fixture, lock-screen path or alert exit (desktop polls 30 s for init; Android restarts the core before relaunching the activity, 15.4 item 10).)

Every test calls `app(...)` first, which runs `resetForTest`: a fresh database, restored preference stores, cleared in-memory state, servers back to their baseline (8.2), then a fresh composition. Peers are started per test, each in a directory of its own test (8.1), and stopped at the end. `hs_init` runs once per JVM.

**`PrefStores.capture()` / `restore()`.** Each `SharedPreference` copies its value into a private `_state` when created and stores no default (SimpleXAPI.kt:3903-3917), and `appPrefs` is a lazy singleton (:537), so rewriting the files is not enough. `capture()` (in `bootJvm`, after `runMigrations`) copies the raw `Properties` of `settings.properties` and `themes.properties` and the content of `themes.yaml`, as `runMigrations()` leaves them (it writes `lastMigratedVersionCode`, `currentThemeIds` and an empty theme-overrides list, AppCommon.kt:46-66, Files.kt:153-167). The restore: clear and refill the raw `Properties`, rewrite `settings.properties`, `themes.properties` and `themes.yaml` from the capture (otherwise the files would hold the previous test's values until the first pref write); null the private static `themeOverridesStore` cache (SimpleXAPI.kt:528, used by the preference at :388-391); refresh the cached state of every `SharedPreference` field of `AppPreferences` by reflection (`_state.value = get()`), never calling `set`: `set` would write every default into the file, and defaults computed from other prefs while the key is absent would stop following them (`mkTimeoutPreference`, SimpleXAPI.kt:320-327, picks the proxy defaults only while the timeout key is unset); apply the restored theme through `ThemeManager.applyTheme(appPrefs.currentTheme.get()!!)` (as Theme.kt:823 does; it writes `currentTheme` only when the value differs, ThemeManager.kt:105-108, so passing the stored value writes nothing); `Locale.setDefault(Locale.ENGLISH)` (switching language calls `Locale.setDefault`, D/views/usersettings/Appearance.desktop.kt:52-60); set `.value` of the `ChatModel` fields that are copied from prefs once (`val … by lazy { mutableStateOf(…) }`, ChatModel.kt:211/220/222/248) (`notificationPreviewMode`, `showAuthScreen`, `showChatPreviews`, `simplexLinkMode`).

**`clearUiState()`** (before the core starts, in `resetForTest` and `restartCore`), on the EDT, waiting for completion:
- everything `clearTransientUi()` clears (below);
- `ChatModel`, its `ChatsContext` objects and `AppLock`, by reflection rather than by a hand-written list (reviews kept finding fields the list missed, e.g. `desktopOnboardingRandomPassword`, which would make later onboarding tests skip the passphrase step, WelcomeView.kt:400; `showCallView`, App.kt:226-237; `AppLock.laFailed`, App.kt:218): `ModelSnapshot.capture()` in `bootJvm`, before the first init, records the initial state of these objects by walking their declared fields (private ones included), and `clearUiState` restores it: `MutableState` and `MutableStateFlow` fields get their initial value; `SnapshotStateList`/`SnapshotStateMap` and plain mutable collections (tested with Kotlin `is MutableCollection<*>` / `is MutableMap<*, *>`, which excludes Kotlin's read-only collections such as the `EmptySet` in `terminalsVisible`, ChatModel.kt:183, so those are restored by reference only) are cleared and refilled with their initial contents; non-final fields get their initial reference back and then, like every field, the captured value inside it (a `var` holding a `MutableState`, such as `draft`/`draftChatId`, ChatModel.kt:240/242, is never reassigned but its `.value` changes), which covers plain `var`s the app sets outside any state (`chatListBanner`, ChatModel.kt:206; `centerPanelBackgroundClickHandler`, :274, set at UserAddressView.kt:549 in a `LaunchedEffect` with no `onDispose`); final fields holding an object of an app class are walked the same way, once per object, which reaches each context's `chatState` (`ActiveChatState`: six `MutableStateFlow`s and `@Volatile var selectionActive`, ChatItemsMerger.kt:196-206, whose six flows production resets through `clearAndNotify`, DatabaseView.kt:587; `selectionActive` is not reset there), `popChatCollector` (its pending map; stale chat ids would pop the wrong chats; `deleteChatDatabaseFilesAndState` also clears it, DatabaseView.kt:589/594) and `processedCriticalError`/`processedInternalError` (ChatModel.kt:270-271; their `timer` job is cancelled first, as a pending one waits up to 60 s and then calls `hideAllAlerts`, ProcessedErrors.kt:15-28; it is launched with `withLongRunningApi(idle = false)` so that `UiIdle.drain()` doesn't wait for it before the reset cancels it). The walk skips Kotlin `object`s (`controller`, managed by `resetForTest`), `MutableSharedFlow`s that aren't `MutableStateFlow`s (the check for `MutableStateFlow` comes first, as it is a subtype) and `lazy` delegates. Exceptions, handled by hand: `changingActiveUserMutex` (ChatModel.kt:257, a kotlinx `Mutex`, not an app class; a job abandoned by `drain` may hold it, and every later user switch would then hang) is checked instead: it is a `static final` field of the `ChatModel` object (ChatModel.kt:140), so reflection can't replace it, and its only holders are `currentUserId` and the active-user section (SimpleXAPI.kt:653, :749-753), so a lock still held after `drain()` means a job is stuck in the core and the JVM is poisoned with a report (6); `migrationState` (re-created as MigrateToDevice.kt:55 does), the four fields copied from prefs (restored by `PrefStores`), and `chatsContext`/`secondaryChatsContext` themselves (the existing objects are reset in place, `secondaryChatsContext` set to null). The fields that matter most for the next test — `chatId`, chats, items, `chatState` (each flow and `selectionActive`), `chatListBanner`, `users`, `draft`/`draftChatId` (ComposeView.kt:1447-1462 can save them on dispose; ids restart at 1 in a fresh database), `chatDbStatus`, `chatDbEncrypted`, `localUserCreated`, `chatRunning`, remote state, calls — are asserted in P1's harness tests to be back to their initial values after a reset;
- other singletons: `BadgeModel`, `BadgeStore`'s load state, `ChannelRelaysModel`, `ConnectProgressManager`, `VideoPlayerHolder`, `OnboardingSharedState.selectedOperatorIds` (YourNetwork.kt:37-39), `CIFile.cachedRemoteFileRequests` (ChatModel.kt:4626, read by `fileReady` for every file, Files.kt:170-172, and cleared in production only on a chat switch, ComposeView.kt:1425, whose `KeyChangeEffect` skips the first composition), image caches: `clearImageCaches()` (D/views/helpers/Utils.desktop.kt:132-134) empties `loadedImageCache` (:128, a 30-entry cache keyed by the file's path, which the database deletion of 7.2 doesn't clear, so a later test's file with the same name would show the previous test's bitmap), and `WallpaperType.cachedImages` (ChatWallpaper.kt:342); the seam fakes' per-test state, `AudioPlayer.currentlyPlaying` and `RecorderInterface.stopRecording` (11.2);
- file-level mutable state of UI files, reset by reflection on their `…Kt` classes: `ChatListViewKt.lazyListState` (ChatListView.kt:907, the chat list's scroll position saved in `onDispose`, :979, and used as the next composition's start, :240) back to `0 to 0`, and `ChatViewKt.reportsListState` (ChatView.kt:1747) to null; otherwise each test would start with the list where the previous one left it, rows above it uncomposed and unfindable, and `restartCore` would restore an offset a real restart doesn't. P1's harness tests assert the list is at index 0 after a reset, and the source-level check of 9.4 fails on top-level `var`s and mutable `val`s of UI files that are neither reset nor allow-listed (the allow-list starts with those found harmless: `chatViewScrollState`, cleared when its scroll collector completes, ChatView.kt:2481-2486, measured; the call WebSocket list, CallView.desktop.kt:28; the composable bookkeeping sets, Utils.kt:632-633; the lazily built icons, CustomIcons.kt:37/83/111/143; the update checker's job, AppUpdater.kt:160, whose job is `idle = false`; extension properties without a backing field, such as MigrateFromDevice.kt:102 and MigrateToDevice.kt:117, hold no state and are skipped);
- window state: `simplexWindowState.windowState` re-created at 1600 × 900 dp (END modals widen it, ModalView.kt:146-148), `windowFocused` and `windowVisible` true (`NTF` tests set focus to false; left false it stops the 1 s status poll, ChatListView.kt:688), `AppLock.clearAuthState()`.

**`clearTransientUi()`** (also the last step of `resetForTest` on every path, including the early returns for a startup DB error and for onboarding), on the EDT: `ModalManager` all placements (`closeAllModalsEverywhere`, ModalView.kt:260-266) and the private `passcodeView`/`onTimePasscodeView` (:112-113, via reflection); pending `DialogState`s completed with an empty result (as at the end of each test, 6); both alert managers (`hideAllAlerts`, AlertManager.kt:48; alerts a test left open were already dismissed at its end, 6); recorded toasts.

**`restartCore(fixture)`** (RESTART rows) simulates a real app restart: end the composition first (its `onDispose` handlers start core jobs, e.g. ConnectMobileView.kt:339-342 stops the remote host, SetupDatabasePassphrase.kt:82-87 starts the chat), wait for idle, then `UiIdle.drain()`; if the controller is neither null nor -1 (a startup DB error leaves it null; -1 is the process-start value, SimpleXAPI.kt:535, which the lock-screen path keeps because init is skipped, AppCommon.desktop.kt:35; `sendCmd` would pass -1 to the core as a pointer) and `chatRunning` isn't false, raw `apiStopChat` (not `stopChatAsync`, which would set `chatStopped` and trigger the "Start chat?" alert on start, Core.kt:61-62; `apiStopChat` leaves `chatStopped` unchanged, SimpleXAPI.kt:1093-1097), then `chatModel.chatRunning.value = null` (the process-start value, ChatModel.kt:146; with `true` left in place, `startChatWithoutUser` returns at once, SimpleXAPI.kt:705, and a user-less app stays on the splash screen); `chatCloseStore` under the same controller check, then `setChatCtrl(null)` and a second `drain()` (events in flight when the receiver stopped); `fixture?.invoke(dbPrefix)` (FIXTURE rows that copy, restore or edit data the test produced); clear in-memory state as `clearUiState` does, including `chatDbStatus` (files and preference stores are kept); then start as desktop startup does — `initChatControllerOnStart()` (Core.kt:59-67), or, when a self-destruct passcode is set, the lock-screen path (`AppCommon.desktop.kt:35` skips init; the harness restores the process-start controller with `setChatCtrl(null)` then `setChatCtrl(-1L)`, which LocalAuthView.kt:36 requires before it initialises) — and compose again with `setContent { TestRoot { AppScreen() } }` at once (the same `TestRoot` stubs), as desktop `showApp` composes while initialisation runs (initialisation can wait on an alert inside `AppScreen`, e.g. "Start chat?": Core.kt:61-62 passes `showStartChatAfterRestartAlert`, :226-235, and :170 awaits it; and `initChatControllerOnStart` only launches a coroutine, Core.kt:59, so `ctrlInitInProgress` may still be false right after it returns). Then poll until `chatModel.chatDbStatus.value != null && !chatModel.ctrlInitInProgress.value`, or until an alert is shown (`AlertManager.shared.hasAlertsShown()`); on the lock-screen path, where `chatDbStatus` stays null until the passcode is entered (LocalAuthView.kt:36-37), poll instead until `AppLock.userAuthorized.value == false` and the passcode view is shown (`ModalManager.fullscreen.showPasscodeInView`, App.kt:222, in the block at :210-224). Persistence rows additionally assert the stored value in `settings.properties`.

A class may therefore hold many tests, and the catalogue's class mapping (10.1) works with any `forkEvery`. Each reset leaves the previous Haskell controller and its agent threads behind (`chatCloseStore` only closes the stores, Mobile.hs:364-367), and old receiver threads may block up to 300 s in `chatRecvMsgWait` (SimpleXAPI.kt:532, :962); both are bounded by the number of tests one JVM runs. With `forkEvery = 0` (as built, 15.4 item 24) that is a share of the whole run, so RSS (about 4.6 MB per reset, 15.1) and one receiver thread per reset accumulate per fork; `-PuiForkEvery=1` bounds them by one class (at most 29 tests, legend class map).

### 7.4 Driver

(As built: see 0.5.3, 0.5.4. Differs: no `UiDriver`/`UiNode`/`InProcessDriver`; the API is methods on each platform's `UiScope`; lookups use keys, `described(text)` or literal English text, not `MR.strings`; `exists` means any match; clicks are touch input at `tapPointInNode`; alerts are not closed with `press(Key.Escape)`.)

```kotlin
interface UiDriver {
  fun node(key: UiKey): UiNode
  fun node(text: StringResource, within: SemanticsMatcher? = null): UiNode  // MR.strings, resolved with generalGetString
  fun node(text: String, within: SemanticsMatcher? = null): UiNode          // literal text (message bodies, names)
  fun nodes(matcher: SemanticsMatcher): List<UiNode>
  fun scrollTo(list: UiKey, itemKey: Any)                                    // performScrollToKey on the keyed lazy list
  fun poll(what: String, timeout: Duration = 10.seconds, cond: () -> Boolean) // the loop of section 6
  fun wait(d: Duration)                                                      // the poll loop running for d of real time with no condition; only where a catalogue row names the interval (e.g. "retry more than 3 s after the failure")
  fun escape()                                                               // simplexWindowState.backstack.lastOrNull()?.invoke(), as DesktopApp.kt:125-130
  fun composeField(): Pair<String, String>                                   // the message field's text and placeholder: desktop from semantics (EditableText; the placeholder Text exists only while the field is empty), Android from the EditText (catalogue 05 setup item 5)
  fun dump(): String; fun screenshot(): ImageBitmap
}
interface UiNode {
  fun click(): UiNode; fun rightClick(): UiNode; fun longClick(): UiNode; fun hover(): UiNode
  fun hold(duration: Duration): UiNode; fun dragTo(other: UiNode): UiNode; fun clickAtTextRange(range: IntRange): UiNode
  fun type(text: String): UiNode; fun press(key: Key, vararg modifiers: Key): UiNode
  fun <T> get(key: SemanticsPropertyKey<T>): T?; fun assert(m: SemanticsMatcher): UiNode
  fun child(text: String): UiNode?
  fun capture(): ImageBitmap                                               // captureToImage of this node (7.7)
}
class InProcessDriver(private val t: SkikoComposeUiTest) : UiDriver      // the supertype of DesktopComposeUiTest; also fits runInternalSkikoComposeUiTest (14)
```

- UI labels are looked up by `MR.strings` key, so tests are locale-independent. Message text is looked up with `within = hasTagPrefix("chat.item[")` (a node or ancestor whose tag starts with `chat.item[`), because the chat list preview (`ChatPreviewView`) shows the same text.
- `node(...)` polls until exactly one node matches (a pending row and the contact row can briefly coexist); zero or several matches after the deadline fail with the semantics dump.
- Lazy lists: `scrollTo` uses the list key and the list's own item keys: `Pair(chat.remoteHostId, chat.id)` for the chat list (ChatListView.kt:1099) and `ChatViewItemKey(item.id, createdAt ms)` for chat items (ChatView.kt:2398 via `keyForItem`, :3663, which becomes `internal`). An item hidden inside a merged group is reached through the group's newest item. Lists without keys get keys (3.4).
- Input runs on the EDT: injected pointer and key events are dispatched on the thread that calls `performMouseInput`/`performKeyInput`/`performClick` (their `flush` calls `sendPointerEvent`/`sendKeyEvent` directly, InputDispatcher.skiko.kt:69-75, 130-138, 215-225, and the pre-event layout pass runs there too, BaseComposeScene.skiko.kt:60-65), which would run every click handler and the app code it calls off the EDT, racing the app's own EDT jobs. So the driver wraps each input injection in `runOnUiThread { … }`, which runs inline on the EDT (DesktopSynchronization.desktop.kt:28-40), keeping clock advances and lookups working; `hold` keeps its real-time wait between separate press and release injections. Semantics actions already run on the EDT (Actions.kt:581). The wrapper is per-platform: on Android the dispatcher already injects on the main thread and a wrap would throw (research doc §5.1).
- Dialogs and menus render as extra roots: lookups use `onAllNodes(..., useUnmergedTree)` and `isDialog()`.
- `escape()` covers modals; it invokes the back-stack lambda inside `runOnUiThread`, as the window key handler runs it on the EDT (D/DesktopApp.kt:125-128), since `closeModal` changes plain collections (ModalView.kt:105, 108, 168-179). Alerts register no back handler, so a test closes an alert with `press(Key.Escape)` on the dialog root (the Compose dialog dismisses on Escape).
- `type(text)` uses `performTextInput`. On the compose field any typed text of two or more characters whose every line names an existing regular file (absolute, or relative to the JVM's working directory) is attached as files instead (PlatformTextField.desktop.kt:97-104, :222-236); tests type such text only to attach a file: as the feature itself (02 `paste_filePathText_attachesFile`) or as the attach step of file tests whose rows say so (01 `serversSummaryFilesTabShowsXftp`, 02 §G).

### 7.5 Failure output

(As built: only `<run>/failures/<jvm>-NN-<what>.png` (Android: pulled into `<run>/failures/`), and a message with the in-flight list and the first 60 visible texts; peer timeouts add the peer's next 10 lines. P1b for the rest.)

On any failed assertion or timeout: the semantics tree of every root (`onAllNodes(isRoot(), useUnmergedTree = true).printToString(maxDepth = Int.MAX_VALUE)`; the collection overload defaults to depth 0, Output.kt:83-86), a PNG via `captureToImage()`, the last 50 terminal lines of each peer and the last 50 log lines of each relay, the recorded toasts and URIs, the `UiIdle` in-flight list, recorded uncaught errors with their phase (6), the screenshot diff image in `verify` mode (7.7), and the app's command log, written to `<run directory>/report/pass<n>/<TestClass>/<test>/` (the run directory comes from `env.properties` and the pass from the `simplex.uitest.pass` property, 7.1, so reports of earlier runs never mix with new ones; the test JVM's working directory is `common/`).

### 7.6 Surfaces outside the scene

(As built: in-memory `LocalClipboardManager` and recording `LocalUriHandler` (desktop); pending `DialogState`s are completed empty at reset only; notifications go to a no-op manager; no toast recording. Android uses Espresso-Intents, 0.5.4.)

| Surface | Location | In tests |
|---|---|---|
| File choosers | D/views/helpers/DefaultDialog.desktop.kt:78-165; `simplexWindowState.openDialog/saveDialog/openMultipleDialog` (D/platform/Files.desktop.kt:68-101) | `UiFiles.respondOpen(path)`, `respondOpenMultiple(paths)`, `respondSave(path)` poll until the dialog's deferred is set and not completed (the `DialogState` field `onResult$delegate`, a `MutableState`, read by reflection; `isAwaiting` is not used, because it stays true until the waiting coroutine resumes, D/DesktopApp.kt:289-303, and a flow such as the QR save, QRCode.kt:90-97 → `shareFile` → a second `saveDialog.awaitResult`, Files.desktop.kt:78, opens the next dialog on the same `DialogState` within one poll step: polling for `isAwaiting` false failed 40 of 40 runs, measured), then complete that captured deferred, failing if `complete()` returns false (40 of 40 passed under load, measured); and `UiFiles.awaiting()` tells whether any `DialogState` has a non-completed deferred (for tests asserting that none opens; a waiter cancelled while it awaits, e.g. because its screen left composition, leaves its deferred set and not completed, since `awaitResult` clears it only after a normal resume, D/DesktopApp.kt:294-300, so `awaiting()` counts it until the end-of-test drain (6) or `clearTransientUi` completes it, and `respondX` only accepts a deferred that was not already pending at the test's previous UI action (the driver records the pending deferreds before each input action's input is injected, 7.4, and before each `respondX`, never after: flows that wait on a background dispatcher, such as the `CIFileView` save, `exportArchive` and `shareFile`, have already set their deferred when the click returns, and a record taken then made `respondSave` reject the right dialog in 12 of 24 runs, measured), so a stale one is never answered in place of the next dialog; a dialog must therefore be answered before the next input action, and when `respondX` times out while a recorded deferred is still pending it fails with "dialog opened before the previous action; answer it right after the action that opens it"); some save dialogs are directories-only (e.g. `saveTempImageUncompressed`, which waits on `saveDialog` with no file name, Utils.desktop.kt:239-240, DefaultDialog.desktop.kt:101-102), so `respondSave` takes a directory there |
| Text clipboard | `ClipboardManager.shareText` (D/platform/Share.desktop.kt:20-23); Compose `LocalClipboardManager`, `LocalClipboard` | in-memory implementations from `TestRoot` (headless AWT has no clipboard and the Compose one silently drops calls) |
| Ctrl+V in the compose field | D/platform/PlatformTextField.desktop.kt:141-152: if the (Compose) clipboard text is a list of existing files they are attached (:144-146, covered by the in-memory clipboard); otherwise — plain text or an image — it reads the AWT system clipboard (:150), which throws `HeadlessException` headless | file-list pastes need only `CB`; any other Ctrl+V in the compose field needs the approved clipboard seam (11.2) |
| Links, browser, calls | `LocalUriHandler`; outgoing calls open the browser through it (D/views/call/CallView.desktop.kt:157-174) | recording `LocalUriHandler` from `TestRoot`, with a harness option that makes `openUri` throw an `Exception` (an `Error` would escape the app's catch; the "Error opening browser" alert that ends the call, CallView.desktop.kt:168-175). An outgoing call invitation is sent only after the browser page reports its capabilities (CallView.desktop.kt:41-43), so outgoing-call tests assert the recorded call link and the waiting state; incoming calls from a peer (`/_call invite`), call items and alerts are tested (the incoming-call alert plays its ringtone through `SoundPlayer` → `AudioPlayer`, IncomingCallAlertView.kt:30-33, RecAndPlay.desktop.kt:275-289, so those rows add `AV`); WebRTC is not |
| Toasts | `simplexWindowState.toasts`, drained by the Window's `LaunchedEffect` (D/DesktopApp.kt:165-182), which isn't rendered | the harness drains and records them; tests assert on the record |
| Notifications | D/model/NtfManager.desktop.kt:97-148 (`displayNotification` → `displayNotificationViaLib`, `Toast.builder()` :123) | `RecordingNtfManager` records each call; the notification-preview text is built inside the desktop manager (:100-102), so preview-mode tests assert the preference and the recorded call |
| Esc | Window key handler (D/DesktopApp.kt:125-130) | `escape()` (7.4) for modals, `press(Key.Escape)` for alerts |
| VLC audio/video, voice recording | D/platform/RecAndPlay.desktop.kt:25-90/92; D/platform/VideoPlayer.desktop.kt:217 | approved seams (11.2) |
| OS folder opener | `desktopOpenDir`, D/platform/Files.desktop.kt:37-48 (no-op when `Desktop.isDesktopSupported()` is false), reached from the database-folder buttons via `desktopOpenDatabaseDir` (:33; DatabaseView.kt:243, DatabaseErrorView.kt:330) and from the app-update download (AppUpdater.kt:361-480, out of reach, 11.3); `openFile` (Share.desktop.kt:46) has no callers | not covered: rows are `IGNORE` until the seam of section 13 is approved |
| Tray, floating console window, window size/position persistence, close policy, crash-restart loop | D/DesktopTray.kt:34-80; DesktopApp.kt:34-120, 194-257, 215 | not rendered; catalogue 04 has `IGNORE` rows for them (11.3) |
| Single-instance dialog | D/SingleInstance.kt:104 | not rendered; not a user feature, no catalogue row |

### 7.7 Screenshots (committed, rewritten only when they change)

(As built: not built; P1b.)

Tests save PNG screenshots that are committed to the repo. They serve three purposes: UI regressions and intended changes show up as image diffs in every PR at no extra cost; new and changed images prove that a real test rendered that screen (for unchanged screens the evidence is the run's `screenshots/summary.txt`, which lists every shot as unchanged, changed, new, unsettled, unquiet, orphaned or unverifiable); and together they document every screen and feature, indexed by the catalogue. Decided with the user: every passing catalogue test except the `REALTIME` ones (and none with `-PuiShots=off`) saves an end-of-test shot plus any named shots its author adds, and the PNGs are stored with Git LFS. A test's own assertions still read only semantics (1); screenshots are compared outside the test logic and fail a test only in `verify` mode (below).

**What is captured.** After a passing body, once pending file dialogs are completed and before the rest of the end-of-test cleanup (6), the test captures `<test>.png` (written once the test has passed). A test adds named shots at intermediate steps with `screenshot("after-join")`, or `screenshot("profile", of = node)` for one element (`UiNode.capture()`, 7.4). Named shots are captured when called but written (or compared) only after the test has passed, including the end-of-test checks of 6 (recorded uncaught and composition errors), so a failing test saves none. Named shots are the test author's choice and are not listed in the catalogue; they are taken with no file dialog pending (a dialog awaited from a tracked job keeps it in flight, so the quiet wait could only end `unquiet`, 6). Capture uses `SkikoComposeUiTest.captureToImage()` (the whole surface, including open dialogs, which are separate roots) or the node's `captureToImage()`, so the image is what the semantics assertions saw, after composition-scoped delays up to the settle time have run (for example visible items get marked read after 600 ms, ChatView.kt:3090-3094, and "in progress" spinners appear after 1 s, ChatListNavLinkView.kt:52-59). Failure screenshots stay separate (7.5) and are never committed. The key lint and the harness's own tests save no screenshots.

**Where.** `apps/multiplatform/common/src/desktopTest/uitest-screenshots/<catalogue file>/<TestClass>/<test>[-<name>].png`, next to a sidecar `….json` with the masks below and stable metadata (catalogue file, row feature, test name, step). The PNG also carries these fields as `tEXt` chunks. Nothing run-specific (dates, commit, run id) goes into the image or the sidecar, so an unchanged screen produces an unchanged file. A `…CompatStableTest` run saves its own copies under its class name.

**Determinism.** A screenshot is only useful if an unchanged screen renders the same pixels on every run:
- Rendering: the fixed 1600 × 900 dp scene at density 1 (7.2), headless Skiko software rasterisation, the app's bundled fonts (Inter in `apps/multiplatform/common/src/commonMain/moko-resources/fonts`; Noto Color Emoji in `…/desktopMain/moko-resources/fonts`, applied only to emoji-only messages, an emoji-only compose field (ComposeView.kt:1071-1072), reactions and tag emoji, EmojiItemView.kt:20-21, ChatItemView.desktop.kt:29, TagListView.desktop.kt:60), with the host fonts pinned for the rest — monospace text (`FontFamily.Monospace`: markdown snippets, the console, verification codes, server addresses; ChatModel.kt:5178/5186, TerminalView.kt:174, VerifyCodeView.kt:82) uses the pinned DejaVu Sans Mono, and emoji inside ordinary text, which is Inter (C/ui/theme/Type.kt), use Inter's own glyph where it has one (e.g. ❤ ♥ ☀ ⚠ ⬆ ⬜ ▶ ↩ ©) and otherwise fall back to the pinned fonts (in `FontFamily.Default` text, which is DejaVu Sans 2.37, the emoji it covers are its own monochrome glyphs — most of the emoticons block, 64 of 80, but not e.g. 🙂 U+1F642 or 🙏 U+1F64F; ❤; and many Miscellaneous Symbols and Dingbats such as ☀ ☕ ⚡ ✌ ✔ — and all other emoji fall back to Noto Color Emoji; all deterministic) — through `FONTCONFIG_FILE` (7.1) pointing at a fontconfig file that the harness ships with a fixed font directory (DejaVu Sans, DejaVu Sans Mono and Noto Color Emoji; Compose's default family on Linux is the first available of Noto Sans, DejaVu Sans and Arial, which in the pinned directory is DejaVu Sans, and the app has text styles without a family, e.g. unread badges, ChatPreviewView.kt:596-600, and quotes, FramedItemView.kt:67/93 — without a sans font, `FontFamily.Default` throws "Could not load font"), so a screenshot depends only on the checkout, not on the host's fonts (glyphs in none of these fonts render as empty boxes, the same on every run; Skiko bundles its own FreeType inside `libskiko-linux-x64.so` and binds its `FT_*` calls to that copy, so the host's `libfreetype` does not affect rasterisation, but the bundled FreeType reads `FREETYPE_PROPERTIES`, which changes hinting, so the task removes it), the light theme and default prefs restored by `PrefStores` (7.3) — the default theme is SYSTEM (SimpleXAPI.kt:233), which jSystemThemeDetector resolves through `gsettings get org.gnome.desktop.interface gtk-theme` on any Linux (D/ui/theme/Theme.desktop.kt:8-24; its GNOME check is always true), so the task sets `GSETTINGS_BACKEND=memory` (7.1) and the host user's dark GTK theme can't turn every screenshot dark — the locale `Locale.ENGLISH` set by `bootJvm` and `PrefStores` (7.2, 7.3) and `-Duser.timezone=UTC` (7.1; the app formats times in the system zone, `TimeZone.currentSystemDefault()`, ChatModel.kt:3945-3994).
- Animation state: with the manual clock, `waitForIdle` does not finish animations (`isIdle` ignores them, ComposeUiTest.skiko.kt:254-266, and virtual time follows real time, 6; e.g. `FloatingDate` fades over 350 ms, ChatView.kt:2966-2970), so before each capture the driver first waits (at most 10 s) until `UiIdle` has nothing in flight and no core event has been processed for 1 s — content driven by core events keeps changing after the last assertion (a sent item's status, member joins, relays, transfer progress, chat-list order), and `UiIdle` counts processed receiver events for this — and then repeats: advance the clock by 1 s (`mainClock.advanceTimeBy(1000, ignoreFrameDuration = true)`; virtual time leads real time here, and the loop of 6 is shifted by the same amount afterwards), wait for idle, capture — until two consecutive captures are identical outside the masks with no core event processed between them, nothing in `UiIdle` in flight at the second one and nothing started or finished between them (settle steps run virtual time ahead, so a composition `delay` can start tracked work mid-shot) (at most 10 steps; a shot that never settles, or whose quiet wait hit its bound, is saved and marked `unsettled` or `unquiet` in the summary). A test whose last step leaves a known change pending (e.g. a sent message's delivery) waits for it before ending. Advancing is what renders a new frame: with `autoAdvance = false`, `captureToImage` only snapshots the last frame unless a layout is pending, so two captures without an advance would always be equal. Looped finite animations never settle — the live-message send-button pulse (SendMsgView.kt:533-544) and the typing indicator (TextItemView.kt:176-183) — so their composables are volatile too, and so are animated images (`rememberAnimatedImage`, D/views/chat/item/CIImageView.desktop.kt:25-26 and ImageFullScreenView.desktop.kt:25; the flag goes on a wrapper around `ImageView` in each platform's `SimpleAndAnimatedImageView` and on `FullScreenImageView`'s `modifier`, only when the image is animated (`looksAnimatable(data)`, AnimatedImage.desktop.kt:106, on desktop; on Android coil animates GIFs always, even under blur and modals, A/views/chat/item/CIImageView.android.kt:22-58, ImageFullScreenView.android.kt:32, and the check is a header check of the image bytes for GIF or animated WebP, since Android's `isAnimImage` needs a URI, Images.android.kt:127), so static images stay unmasked): their frames are decoded on `Dispatchers.Default` and wait in real time, so the test clock can't stop them (D/platform/AnimatedImage.desktop.kt:51-56, 131-148), and a GIF with loop count 0 plays forever. Indeterminate progress indicators (semantics `ProgressBarRangeInfo.Indeterminate`) are masked like `VolatileKey` nodes. A blinking caret blinks in step with the 1 s settle advances, so a focused field would look settled while differing between runs. The end-of-test shot therefore first advances the clock by one settle step (so delayed `requestFocus` calls in `LaunchedEffect`s fire — 17 call sites in 13 files request focus 100 ms–1 s after composition, e.g. WelcomeView.kt:175, AddGroupView.kt:181, and the confirm button of every two-button alert, AlertManager.kt:152), then clears focus, then settles; `screenshot(name)` follows the same sequence — one settle step, then it notes the focused text field (semantics `Focused`), clears focus, settles (at most 10 steps), captures — and then gives the field focus back with its `SemanticsActions.RequestFocus` action, so the next `press` still reaches it (3.5). The compose field keeps focus after sending (it clears focus only when sending becomes disabled, PlatformTextField.desktop.kt:71-73), and several screens focus a field soon after opening (above), so a named shot often has a focused field to handle.
- Times are fixed by a display clock; other volatile content is masked. Times are not only text: message bubbles reserve width for the time (`reserveSpaceForMeta`, CIMetaView.kt:130-195, used at TextItemView.kt:158, FramedItemView.kt:359, CIFileView.kt:213), and item grouping depends on it (a new timestamp shows when 60 s pass between items or the minute text changes, ChatView.kt:3765-3768, :3784), so masking the time would not keep the rest of the screen stable. A display-clock seam (11.2, approved 2026-10-12) maps every timestamp and "now" used for display to one fixed instant in UI tests: `getTimestampText`, `getTimestampDateText`, `localTimestamp` and `localDate` (ChatModel.kt:3944-3998), `FloatingDate` (ChatView.kt:2894) and the 60 s grouping check (ChatView.kt:3765, :3784); and the SimpleX-name sale countdown, which ticks every second from `System.currentTimeMillis()` (SetSimplexNameView.kt:195-205); P1 audits the remaining display uses of `Clock.System.now()` and `System.currentTimeMillis()` (the chat console is volatile as a whole instead: its lines carry wall-clock times, `TerminalItem.date`, SimpleXAPI.kt:7524, TerminalView.kt:173, and include every core event in arrival order, SimpleXAPI.kt:2905). Times then render as one fixed time and date, and grouping depends only on sender and direction, on every run. The core keeps its real clock, so ordering, TTLs and timers are unaffected. Tests about real time display or time-based grouping (date separators, the floating date, grouping by time; catalogue code `REALTIME`) turn the seam off with `uiTest(realTimes = true)` and save no screenshots, since their screens can't be stable. What remains volatile is masked: QR codes and the link text of invitations and addresses (new keys on every run, QRCode.kt:23-64), security codes, server statistics (ServersSummaryView), call and voice durations, and the looped animations above. These composables get the semantics flag `VolatileKey` through `Modifier.uiVolatile()` (4; added in K0, 5.2), on the volatile child itself; `VolatileKey` does not merge into the parent row or item (4). Before comparing, the harness fills the bounds of every node of the **unmerged** tree with `VolatileKey` with a flat colour in both the old and the new image (union of the old sidecar's and the current masks); a change in the set of volatile nodes (their count or keys, recorded in the sidecar) also counts as a change, so a regression that drops a QR code isn't hidden by the old mask. Where P1 finds that a masked text's width moves its neighbours, the volatile node is widened to cover them. Run-dependent content outside the time display is volatile too: server addresses with their ports (ports come from the JVM's slot, 8.2; the custom servers' editor, ProtocolServerView.kt:181-184, and the server summaries, ServersSummaryView.kt:534, :562) and link-preview URLs (`http://127.0.0.1:<port>`), and the relay list of a channel with several relays, whose order is random (`chooseRandomRelays`, AddChannelView.kt:183-217; ChannelRelaysView.kt:62-79). Ports appear in more places than can be listed reliably (the contact and member "Receiving via"/"Sending via" rows, `SimplexServers`, ChatInfoView.kt:1156-1163, GroupMemberInfoView.kt:659-663; a link-preview test's message text `http://127.0.0.1:<port>/page`, FramedItemView.kt:396-400, and its chat-list preview), so in addition the harness masks, for every text node of the unmerged tree (`Text` or `EditableText`) whose text contains one of the run's own ports (the JVM's server and stub ports, 8.2), whether or not it is flagged, the bounds of its outermost ancestor that merges its descendants' semantics, searching up only to the nearest scroll container (a node with `VerticalScrollAxisRange`; walking to the root would reach full-screen clickable scrims such as the user picker's, UserPicker.desktop.kt:96-100); that is the chat bubble even for a port inside a quote, whose own quote column merges too (FramedItemView.kt:292-308) while the bubble's width follows it, and the whole row for list and info rows; the node itself when there is none (measured on a replica of the item layout: 0 pixels differ): ports change with the JVM's slot between runs and the app font's digits differ in width, so a port's text moves its neighbours (measured: 20001 against 20101 moved an icon after a left-aligned port and a right-aligned bubble's edge by 3 px; with the ancestor masked, 0 pixels differed). Also volatile: incognito profile names, which the core generates at random for each incognito connection (`generateRandomProfile`, Hs ProfileGenerator.hs:9-22) — "Your random profile" in chat info (ChatInfoView.kt:583-589), "Your random profile" in channel info (GroupChatInfoView.kt:628) and the incognito member's own row in group info (:772) and "Join as %s" in a group invitation preview (ChatPreviewView.kt:583-587); the remote-desktop pairing screen's port field, bound to a system-chosen port when none is set (ConnectMobileView.kt:476-485; simplexmq RemoteControl/Discovery.hs:83), and its session verification code (ConnectMobileView.kt:231-238), and on Android the "Use from desktop" session code and pasted invitation (`SessionCodeText`, ConnectDesktopView.kt:341-349, used at :157-160, :273, :326-329; the invitation field, :386-396); the chat console's line list, flagged on the list itself (`LazyColumnWithScrollBar`, TerminalView.kt:159-167, so the number of volatile nodes doesn't vary with the number of lines; each line, :168-174, carries a wall-clock time and core events arrive in their own order); the "Message queue info" alert (shown at ChatInfoView.kt:694-698 and GroupMemberInfoView.kt:717-721 through `showAlertMsg`, text built at ChatInfoView.kt:1399-1403; random queue and message ids and timestamps; `showAlertMsg` gets a volatile-text parameter in K0, AlertManager.kt:212-218); and the chat-relay address editor, whose link has new keys on every run (ChatRelayView.kt:231-241; a preset relay shows its fixed preset address instead). The committed PNG itself is left unmasked, so it reads naturally as documentation.

**Write only on change.** After capture, if a committed PNG exists with the same size, every pixel outside the masks is identical and the set of volatile nodes matches the sidecar's (tolerance 0 by default; `-PuiShotTolerance=<n>` allows a per-channel delta for other hosts), the file is not touched. Otherwise the new PNG and sidecar are written (each to a temporary file renamed into place, so an interrupted write never leaves a truncated PNG; a committed PNG that can't be decoded (and isn't an LFS pointer) counts as changed) and a diff image (changed pixels in red over a grey copy) goes to `<run directory>/screenshots/diff/<TestClass>/<test>[-<name>].png`. Each JVM appends one line per shot to `<run directory>/screenshots/summary.txt` (new / changed / unchanged / unsettled / unquiet / orphaned / unverifiable (LFS pointer), with the share of changed pixels); appends are single `write` calls of one line, which do not interleave. Modes, `-PuiShots=`:
- `update` (default): write new and changed screenshots; the test does not fail. `git diff` then shows exactly the screens that changed.
- `verify`: a changed or missing screenshot fails the test, with the diff image in the failure report (7.5) (for PR checks).
- `off`: no capture (quick local runs).
Tests that are `@Ignore`d or skipped by an assumption produce no screenshot and keep their existing ones. Whole-run steps happen in the runner after both passes (7.1.1), only when no test filter was given and the mode is `update`: it deletes committed screenshots of tests that no longer exist (per the catalogue and the compiled classes, not the XML results, which lack entries for a crashed JVM) and those of tests that **passed** this run but didn't produce them (the summary lists every shot each test produced; failing tests save no shots and keep theirs), records them as `orphaned` in the summary, and regenerates the docs index. The traceability check (10.1) fails on screenshots without a catalogue test whenever it runs.

**Docs index.** A generated `uitest-screenshots/README.md` lists, per catalogue file and sub-area, each row's feature with its screenshots; the runner rewrites it only when its content changes, like the images.

**Size.** A flat 1600 × 900 UI screenshot compresses to roughly 40–120 KB. The 1,149 runnable tests run 1,185 times (the 36 `S` tests run again in their stable subclasses), so with one or two shots each there are roughly 1,200–2,366 images (the 2 `REALTIME` runs save none), about 50–280 MB in the working tree, and history grows with every changed image. The PNGs are written losslessly with maximum compression and stored with Git LFS (`.gitattributes`: `apps/multiplatform/common/src/desktopTest/uitest-screenshots/**/*.png filter=lfs diff=lfs merge=lfs -text`; sidecars and the index stay in plain git), so clones stay small while PR diffs still show the images. P1 adds the attribute and requires git-lfs (`git lfs install`, which also sets `filter.lfs.required`; it is not installed on this host yet, and without it git stores the PNGs in plain history silently, verified): the runner refuses `uiShots=update` unless `git config filter.lfs.clean` is set and `git check-attr filter` reports `lfs` for a screenshot path (`git lfs env` alone succeeds even without `git lfs install`); no LFS operation needs the network during the run. The compare step reads committed PNGs from the working tree; a checkout without `git lfs pull` has pointer files (they start with `version https://git-lfs`), which `update` never overwrites — such a shot is reported as `unverifiable (LFS pointer)` — and `verify` fails with the same hint.

### 7.8 Coverage measurement

(As built: not built; P1b.)

Decided 2026-10-12: the UI test run measures real coverage, so catalogue completeness is checked against what actually ran rather than estimated.

- **Tool.** Kover (the Kotlin coverage plugin with the IntelliJ agent) is applied to `:common`. Kover instruments only test tasks of type `KotlinJvmTest` bound to the JVM target, so `desktopUiTest` is registered as `tasks.register<KotlinJvmTest>("desktopUiTest") { targetName = "desktop"; … }` (7.1). All forks of a pass append to Kover's single per-task binary report (`common/build/kover/bin-reports/desktopUiTest.ic`; the agent appends, so parallel forks share it safely), which Kover deletes when the task starts, so the runner copies it, when present (a pass that is skipped or matches no test writes none), to `<run directory>/coverage/pass<n>.ic` after each pass and then deletes it (otherwise a later plain `koverHtmlReport`, with `desktopUiTest` skipped, would include stale UI-test coverage) and after both passes merges the pass files that exist (one or two) with a small Gradle task of the harness that reads them through Kover's reporter (resolved in `uitestPrepare`, so it works offline). Because Kover makes `check`, `build` and its own report and verify tasks depend on every instrumented `KotlinJvmTest` of the `desktop` target, `desktopUiTest` runs only when the runner sets `SIMPLEX_UITEST_RUNNER=1` (`onlyIf`, 7.1); a plain `./gradlew check` skips it and never starts the suite outside the namespace. Gradle resolves the plugin when it configures the build (step 1 of 7.1.1 runs outside the namespace), and `uitestPrepare` also resolves the `koverJvmAgent` and `koverJvmReporter` configurations, so the offline run and the merge find them; a run with `-PuiCoverage=off` skips the agent, whose overhead P1 measures.
- **Scope.** The report covers the UI and model code the tests exercise: `chat.simplex.common.views.*`, `chat.simplex.common.ui.*`, `chat.simplex.common.model.*`, `chat.simplex.common.platform.*`, excluding the test harness. Line and branch coverage per package, file and composable function, as HTML and XML in `<run directory>/coverage/report/`, plus a short summary (per-package percentages and the files with zero covered lines) in `<run directory>/coverage/summary.txt`.
- **Use.** Files and composables with no covered lines after a full run are the starting list for new catalogue rows: each is either given a row, or recorded in the legend as unreachable on desktop (Android-only, camera, WebRTC) with its reason, so the next full run's zero-coverage list contains only known exclusions. Coverage is reported, not enforced: no threshold fails the run, since screens that need a seam not yet approved (13) or an out-of-reach surface (11.3) can't be covered.
- **Interaction with screenshots.** Coverage runs and screenshot runs are the same runs; coverage adds no files to the repo (reports stay in the run directory).

## 8. Peers and servers

### 8.1 CLI peers

(As built: see 0.5.5. Differs: Current CLI only; args `-d`, `--user-display-name`, `-s <smp1>` and the three display flags, with no `--mute`, `--xftp-server` or `--key`; no `$HOME/Downloads`; `TERM=xterm-256color`; `stty -echo -icanon` before exec instead of the start–stop–start cycle; the database comes from the run's peer template (15.1); typing uses a 1.5 s unchanged window and 5 attempts, with no `! `/`> `/`\ ` rule; `sends` consumes the first matching echo anywhere in the buffer; line waits 15 s; no `expectsAny`, `repeatUntil`, `stop`/`start`/`snapshot`/`restore`, relays or mobile hosts; added `eventuallyMessage`, `eventuallyMatching`, `pending`. Matching is shared in `PeerLines`; on Android it runs device-side over the bridge.)

```kotlin
sealed interface CliVersion {
  object Current : CliVersion                               // built from this checkout
  data class Stable(val tag: String = UiEnv.defaultStableTag) : CliVersion
}
sealed interface TestUser { val name: String }
class CliPeer(override val name: String, val version: CliVersion, val flags: PeerFlags, val servers: PeerServers) : TestUser {   // flags and servers apply on every start
  infix fun sends(cmd: String)                 // like ##>; types the command with verified input (Typing, below), then waits for the command's echo (the command line itself; for message commands, which the CLI echoes only when they fail, Terminal/Input.hs:64/68/89-99: the sent-item line `HH:MM <cmd>`, timestamp stripped, View.hs:2427-2429, or, if the command fails, the plain line `<cmd>` before the error) and re-sends Enter once if the input line still holds it. Some message commands print a sent item that differs from the command, so they are sent with `types` and read with `expectsMessage`/`expects`: a quote reply `> @alice (hi) re` prints `HH:MM @alice > hi` then `      re` (View.hs:2400-2402); a forward `@carol <- @alice hi` prints `HH:MM @carol <- @alice` then `      hi`; text with bold, italic, strike or colour markdown loses those markers (`@alice *b* !1 r!` prints `HH:MM @alice b r`; code and secret keep theirs; View.hs:2461-2462, Styled.hs:38-47; received items the same, View.hs:2362); `/feed <text>` prints `/feed (<n>) HH:MM <text>`, read with `nextLine()` and a prefix check (measured)
  infix fun types(text: String)                // like Haskell `send`: types the text with verified input (Typing, below) and Enter, waits for no echo; for `/live` (a successful one is not echoed and its item is hidden while live, Terminal/Input.hs:64,91, View.hs:909-911) and for text typed into the live message that follows (the input line is pre-filled with the message, so the verified input line is that text plus the typed text, and Enter ends the live message with the whole line, Input.hs:101-110,186-196; the sender then prints `HH:MM @<contact> [LIVE] <text>`, View.hs:2400-2408, time prefix :2427-2429, which a later `expectsMessage` consumes; if that line never appears and the input row stops changing, the peer has locked up: ending the live message kills the prompt's blink thread, Input.hs:189-190, which may hold the terminal lock while it redraws, and `withTermLock`, Output.hs:138-142, doesn't release it on kill; measured 3 times in 100 Enters sent exactly on the blink, never with ordinary timing; the receiver already has the final text, so the harness restarts the peer with `stop()`/`start()`; the live state is set only after the core returns, which also overwrites the input line, Input.hs:68-72,101-113, so typing into the live message must wait until the receiver, the GUI, shows the LIVE item: typed back-to-back, the text went as a separate message and the live message stayed open, measured; the sender prints nothing to wait for)
  infix fun expects(line: String)              // like <##: the next output line must equal this (polled, 6)
  infix fun expectsMessage(line: String)       // like <#: the next line, with the timestamp stripped
  fun expectsAny(vararg lines: String)         // the next N lines, in any order, like concurrently_
  fun eventually(line: String)                 // skip lines until one equals this
  fun nextLine(timeout: Duration = 10.seconds): String   // the next complete output line, returned rather than asserted (JSON responses such as `/_get chat … count=1`, a link the CLI prints for the GUI to paste)
  fun repeatUntil(cmd: String, every: Duration, deadline: Duration, done: (String) -> Boolean): String   // sends cmd (as `sends`) every `every` until a line printed after it (its response or a later event, before the next send) satisfies `done`, returns that line; fails at the deadline. Catalogue rows that say "repeats <cmd> until …" use it (e.g. `/_connect plan 1 <link>` until the AUTH error, `/get servers summary 1` until the single `agent servers summary: {…}` line, about 4,600 characters and so wrapped and joined by the output model, contains `"_connDeleted":1`, a send every 2 s until the AUTH error line)
  fun stop(); fun start()                      // PEERCTL: stop/start the process on the same database; start() returns once the input prompt is drawn
  fun snapshot(): PeerSnapshot; fun restore(s: PeerSnapshot)  // PEERCTL: copy and restore its database files, including any -journal (a stopped CLI can leave one)
  fun contactId(name: String): Long            // sends `/i @<name>` (as `sends`), consumes its echo, takes the first later line "contact ID: N" (View.hs:1885; v7.0.3 too) and removes the contiguous block from it through "peer chat protocol version range: …" (View.hs:1898/1945), or, with no active connection, through the last line of that response's batch (one response is printed as one locked batch, Output.hs:311-314; UI theme and custom-data lines, View.hs:1899-1900, would follow, and peers set neither); event lines before it stay for later `expects`
}
data class PeerFlags(val reactions: Boolean = false, val markRead: Boolean = false, val fullLinks: Boolean = false,
                     val key: String? = null)   // `--key`, passed on every start (incl. the names-role restart and `start()`)
data class PeerServers(val smp: String = "smp1", val xftp: String = "xftp1")   // `-s` / `--xftp-server` (8.2); `smp2`/`xftp2` run only in tests with code `SMP2`/`XFTP2`
```

- **Own directories, per test.** At the end of a passing test every peer and relay is stopped and waited for (`waitFor`, force-killed after a timeout; pty4j's `destroy()` sends SIGTERM to the process group and closes only streams that were never opened, so after `waitFor` the harness closes the pty's input and output streams itself, which frees the master fd; `destroy()` never sends SIGKILL, `destroyForcibly()` does), then its `<jvmDir>/t<n>` directory is deleted (a fresh CLI database is about 1.8 MB and a full run starts over 1,000 peers and relays); a deletion error is logged, not failed; a failing test also stops and waits for its peers and relays but keeps the directory for the report. Every CLI process (peers, relays, mobile hosts, fixture and `RelayPeer.run` one-shots) gets `TMPDIR=<its directory>/tmp` (for fixture one-shots, the directory of the database prefix), created before start: otherwise the agent's XFTP work folder is the system `/tmp` (the CLI passes no temp folder to the agent, Hs/Library/Commands.hs:271-273, so it falls back to `getTemporaryDirectory`, simplexmq Agent/Client.hs:1433-1436; the chat's own fallback is Hs/Files.hs:27; `--temp-folder`, Options.hs:421-426, would set the same in both builds, while `TMPDIR` also covers the chat's own temp use), and moving a received file into `$HOME/Downloads` with `renameFile` (Hs/Library/Subscriber.hs:410) fails with EXDEV when they are on different filesystems — `/tmp` is on the root filesystem here and `/home` on another, and the stable release has no copy fallback (in one experiment a 7.1.0.10 build completed a cross-filesystem receive, mechanism unknown, so the rule holds for every build); it also keeps peer temp files inside `t<n>`. Peers, relays and mobile hosts run with the repo root as working directory, so the catalogue's repo-relative paths (`/f #team tests/fixtures/test.pdf`) resolve: the CLI resolves file paths against its own working directory (Hs/Library/Internal.hs:743-745), and the test JVM's is `common/`. Each peer runs with `HOME=<jvmDir>/t<n>/peers/<name>` (`n` = the test's index in the JVM; a peer reused across tests would reopen a database that already has a contact `alice` from the previous test, and the new contact would become `alice_1`); `stop()`/`start()` inside one test keep the directory. `$HOME/Downloads` is created before the first start (received files go there only if it exists, otherwise to the temp directory, i.e. `TMPDIR`, Files.hs:29-32), and `-d` is always given (the default database would be the real `~/.simplex`, Terminal/Main.hs:26). Every peer also gets `--user-display-name <name>` and `--mute` (the network namespace doesn't isolate D-Bus, so unmuted peers would send desktop notifications).
- **Terminal only.** The CLI runs the terminal UI, the `-p` WebSocket server, the headless mode or a single `-e` command (`src/Simplex/Chat/Terminal/Main.hs:31-43`), so interactive peers are terminal peers; structured data comes from typing `/_get …` and similar commands.
- **Display flags.** The real CLI's defaults differ from the Haskell test client's (`testView`, `showReactions`, `markRead`, `showFullLinks` are on in tests/ChatClient.hs:184-189, `testView` at :308, and off in the CLI: src/Simplex/Chat.hs:117-123, with `--mark-read` an off-by-default switch, Options.hs:458-462). `testView` has no CLI option, so catalogue rows quote only lines the real CLI prints. The others are per-peer `PeerFlags`, set by `cli(name, flags = …)`: `--reactions` (received reactions are printed), `-r/--mark-read` (shown items are marked read, which starts a received timed message's timer), `--show-full-links` (full links, the "… link for old clients" lines; the option exists only in the current build: v7.0.3 has no such option and prints those lines unconditionally, View.hs:1053/1125/1187 at the tag, so `cli` passes the flag only to current-build peers). Rows that need a flag name it in their notes.
- **pty.** The CLI needs a terminal (without one it crashes with `Prelude.undefined` from the terminal library), so peers run under pty4j with a fixed size: 4000 × 50, so that most output lines don't wrap (P1 checks that the longest full link fits; longer lines — JSON responses such as `/_get chat … count=1` — are joined by the output model below; a zero-size pty makes the CLI fail with `divide by zero`).
- **Output model.** The CLI draws with absolute cursor positioning: it moves the cursor above the input line, writes the message, erases to end of line and lets the terminal scroll (Terminal/Output.hs:321-380), and it redraws the input line on every keystroke. The peer runs a VT screen model (cursor positioning, erase sequences, `\r`, scrolling) that keeps scrolled-off history and yields each output line once it is complete above the input line, with prompt prefixes removed and lines the terminal soft-wrapped at the pty width joined back (the model tracks the wrap flag of each row: a row is marked wrapped only when a printable character arrives with the last column already filled, so a line of exactly the pty width followed by erase and CR LF is not joined, and an erase while that wrap is pending keeps the last column, unlike xterm, which would lose the line's last character; verified against the stable CLI's 4,197-character `/_get chat` line); it counts one cell per code point, as the CLI positions text by character count (Output.hs:321-375), and ignores colour and cursor show/hide sequences; each peer keeps a cursor, as `getTermLine` does. P1 tests the model against the CLI's own output.
- **Typing.** The CLI loses typed characters while it prints: every output event, including ones that print nothing (the sender's own status and receipt events, Terminal/Output.hs:144-158), runs `withTerminal` (Output.hs:92,97-98,311-319), and haskell-terminal starts a stdin reader for each such call that reads into a slot nobody empties and is cancelled when the call ends (terminal `Platform.hsc:58-70,149-166`); each event can take up to 2 characters, Enter included. Measured: 200 sends of about 85 characters while receipts arrived, 14 echoes didn't match and 2 had none; with no events arriving, 0 of 200. So `sends`, `types` and `contactId` write the text without Enter and poll the VT model's input line (the row after the prompt, which is `> `, or `> <chat name> [LIVE] ` / `> <chat name> [    ] ` while a live message is open, switching every second, Input.hs:112-113) until it equals the text; if it is unchanged for 200 ms and still differs (each keystroke redraws the whole line, Output.hs:336, so there is no short absolute deadline), they erase with Backspace, sent as DEL `0x7f`, the pty's erase character (`0x08` is read as Ctrl-H plus forward Delete, which erases nothing at the end of the line: haskell-terminal Decoder.hs:27, Platform.hsc:107; only VERASE maps to Backspace, Platform.hsc:104), until the line is empty (a stolen first character can leave a line longer than the text, because typing into an empty line inserts the active chat prefix, Input.hs:351-357), or in a live message down to the pre-filled text (see `types`), and retype (bounded retries, then the test fails). Only then they write Enter; `sends` then waits for the echo, and both `sends` and `types` wait until the input line no longer holds the typed text after either prompt (plain `> `, or the live prompt while a live message is open) and re-send Enter if it still does after about 500 ms (safe for `/live`: after its Enter the line is empty and then holds the pre-filled message, Input.hs:110, never the typed command; a successful Enter in a live message clears the line and sets the plain prompt at once, Input.hs:208,221) (an extra Enter on an empty line outside a live message does nothing, Input.hs:177, and submitting clears the line at once, Input.hs:215-221, so this can't submit twice). The CLI also inserts the active chat while keys arrive (Input.hs:234, 351-357): into an empty line for any first character except `@ # / > \ ! + -`, and after a space typed right after a lone `!`, `>` or `\` (the active chat is set by sending, reading, editing or deleting, Input.hs:78-81, 84, by received messages that notify, contact events and joining a group, Output.hs:178-180,189,191,194,202, and unset at Output.hs:196 and Input.hs:76, 82-83, 86; format Output.hs:236-252). So texts are written with an explicit chat (`@alice hi`), and for a text that starts with `! `, `> ` or `\ ` the peer types those two characters, waits until the input line starts with them (the CLI inserts the chat in the same redraw as the space), erases with DEL whatever follows them, then types the rest; the input-line check expects the full text. Measured over the same 200 sends: 0 corrupted, 0 missing echoes, 18 retypes; typing into a live message the moment the receiver shows it lost 2 characters in 7 of 60 runs (the live prompt's blink thread starts redrawing then, Input.hs:106,120-126), and with this rule 20 of 20 runs with a deliberately dropped character ended with the right text.
- **Consuming output.** As in the Haskell tests, each `expects` consumes one line; helpers such as `connect()` consume the lines they cause. Lines a test doesn't care about are skipped with `eventually`.
- **Contact ids differ from the Haskell tests.** A user created through `CreateActiveUser` — every real CLI user (`--user-display-name` or the prompt, Hs/Core.hs:151-157, :177) and the GUI's user (`apiCreateActiveUser` in `resetForTest`) — also gets the preset "Ask SimpleX Team" contact card and a notes folder (Hs/Library/Commands.hs:466-478; v7.0.3 too), so contact 2 is the card and the first real contact is 3; the Haskell test client creates users with `createUserRecordAt` (tests/ChatClient.hs:366) and has no card. Catalogue rows therefore use name commands where they exist (`/d alice`, `/set disappear @alice yes 3`, `@#team @alice <text>`, `/call @alice`, `! @alice <text>`, `/accept member #team @alice`), and `contactId(name)` for API commands that take only ids (`/_call end @<id>`, `/_get content types @<id>`, `/_send @<id> json …`, `/_set prefs @<id> …`). On the GUI the card is in `chatModel.chats` but hidden from the list (`filteredChats`, ChatListView.kt:1554; `isContactCard`, ChatModel.kt:1933); with the notes folder it still counts wherever the app counts `chatModel.chats` (the SimpleX Lock notice, App.kt:100, and the update notice, desktop Main.kt:69-73) and blocks the "You have no chats" state (ChatListView.kt:458), so catalogue 01 `youHaveNoChatsEmptyState` uses a fixture.
- **Names only.** The GUI user and peers have display names only, so CLI lines read `alice: contact is connected`; a full name, description or badge would add text (View.hs:3022-3034).
- `CliVersion.Current` = the CLI built by the runner (`cabal build -f uitest --builddir=dist-uitest exe:simplex-chat`, 7.1.1).
- `CliVersion.Stable(tag)` = the release binary from the GitHub release `tag`: asset `simplex-chat-ubuntu-22_04-x86_64` for v6.4.2 and later, `simplex-chat-ubuntu-22_04-x86-64` for earlier tags (the fixture CLI v6.3.7 included; the other name returns 404), checked against the release's `_sha256sums`. The tag is a Gradle property, `-PuiStableTag=<tag>` (one per run), with the default `v7.0.3` pinned in `build.gradle.kts` (GitHub's "Latest" release on 2026-10-10; v7.0.4 is a pre-release) and bumped by hand; to test another release, run again with another tag. `uitestPrepare` downloads it outside the namespace into `~/.cache/simplex-uitest/cli/<tag>/`, verifies the published SHA-256, and writes the path into `env.properties`. The binary needs glibc 2.34 (the host has 2.36); it reports its own version (v7.0.3 prints `v7.0.0.12`), so tests never assert the tag in CLI output. A stable CLI can't be a remote host (`minRemoteHostVersion` 7.1.0.9, Remote.hs:81-82).
- **Relays and mobile hosts** are CLI processes too. Relays run without a pty: `--relay --headless --user-display-name <name> -d <dir> -s <smp1> --xftp-server <xftp1>` (headless creates the relay address on first start and prints `Chat relay address is created:` followed by the link, Hs/Core.hs:190-196; `--relay-web-domain` and `--relay-web-dir` must be given together, Options.hs:295-298); the harness reads the link and registers the relay in the GUI's servers (or the settings test does it through the UI). A headless relay takes no typed commands, so `RelayPeer.run(cmd)` stops the relay, runs the command once with `-e <cmd> -t 3` and the running relay's `--relay`, `-s`, `--xftp-server` and `-d` options on its database, with stdin `/dev/null` (`-t` is the wait after the command, default 3 s, Options.hs:387-395, used at Terminal/Main.hs:61; the process must stay alive after the command returns, because async effects such as `/da`'s server-side deletion run in a background thread nothing waits for, simplexmq Agent.hs:1013, and after a restart the agent retries them only from its cleanup task 30 s later, Env/SQLite.hs:247 — with `-t 0` the deletion reached the server in 0 of 4 runs, with `-t 3` in 4 of 4), and starts it again (`PEERCTL`). A headless relay that has no address after the command (e.g. after `/da`) creates and prints a new one on start, and `RelayPeer.link` is refreshed from that output. `relay(…)` and `start()` wait for the `Chat relay address is created:` line (on later starts, `Current user: <name>`, Hs/Core.hs:87, the only line a headless relay prints then after the welcome banner, Terminal/Main.hs:38-41; a `start()` after a `run()` that deleted the address waits for the address line instead) with their own 60 s timeout (first start measured 4–8 s, slower while peers start). Relays are usable right after that line: messages wait on the SMP server until the relay subscribes (measured: delivery 0.41–0.43 s after a post sent the instant `Current user:` appeared, 0.35 s in steady state). Each relay restart logs `internal error: SEMsgNotFound "setMsgUserAck"`, which is harmless and must not fail a test that checks relay output for errors. Relay directories are per test, like peers'. Relays have no pty and don't read stdin, so they are started under `setpriv --pdeathsig KILL` and die with their JVM even when it is killed (the parent-death signal fires when the *thread* that spawned the process ends, so the harness spawns them from one daemon thread that lives as long as the JVM). Every CLI process (peers, relays, mobile hosts, fixture and `RelayPeer.run` one-shots) gets `TERM=xterm` and `LC_ALL=C.UTF-8` in its environment (in the C locale an emoji typed into the CLI silently kills its input thread, and one it prints kills it with `commitAndReleaseBuffer: invalid argument`, measured; emoji reach peers in normal rows, e.g. reaction lines; the host has `C.utf8`, and P1 asserts it); pty peers end with their pty when the JVM dies. A remote-desktop mobile host is a current-build terminal peer (catalogue 04 §K).
- Peers that use SimpleX names need the names server role, which `-s` doesn't enable on the first start (Operators.hs:189-193). The Haskell tests enable it with `/_servers 1 <json>` (`enableNamesRole`, tests/ChatClient.hs:487-505), but the real CLI prints `/_servers` as text without server ids (View.hs:139, :1664-1690), so the harness has no JSON to edit. Instead `cli(…)` (and so `mobileHost(…)`) starts every peer once, stops it and starts it again before the test sees it: on every later start `-s` sets the servers with all roles, names included (src/Simplex/Chat.hs:300-304), so every peer on `smp1` (where the resolver is) can resolve names; no catalogue row depends on a peer failing to resolve one. The restart is needed for a second reason, so it stays even if the names role is set another way: the same per-event `withTerminal` calls save and restore the terminal settings, and on a peer's very first start this can leave the pty in echo mode, so every typed command comes back as an extra output line (measured: 8 of 21 and 5 of 12 first starts; 0 of 96 restarts). In addition `start()` clears ECHO and ICANON on the pty through its master fd (pty4j `UnixPtyProcess.getPty().getMasterFD()`; `tcsetattr` on the master changes the slave's flags, measured), and P1 checks that a started peer's pty does not echo. A new peer's first start creates its database and takes about 4–15 s depending on load (measured: one-shot 3.8–4.6 s; under a pty 4.6–11.2 s alone, 7.0–14.6 s with four in parallel; a restart 0.1–0.4 s), so `cli(…)` returns at once and does the first start, stop and restart in the background; the peer's first `sends`/`types`/`expects` waits for it, so a test's peers start in parallel (that wait has its own 30 s deadline, measured with the start–stop–start cycle in parallel: 9 peers 11.3 s, 21 peers 15.7 s, 20.6 s with 2 cores busy; the 9-peer member-list test would otherwise spend 45–110 s of its 2-minute deadline starting peers).
- Kotlin can't define `##>` (`<` and `>` aren't allowed in JVM method names), so infix words keep the same shape.

### 8.2 Servers: `uitest-servers`

(As built: see 0.5.6. Differs: `smp1` only, args `<dir> <portBase>`, no control protocol or names resolver, no `restoreBaseline`; `xftp1` is configured in the GUI but not served (P1b); the process is spawned from the test thread during the first reset (bridge: its main thread), not from a dedicated daemon thread.)

A test-only executable component in `simplex-chat.cabal`, built only with a manual flag:

```
flag uitest
  manual: True
  default: False

executable uitest-servers
  if !flag(uitest) || flag(client_library)
    buildable: False
  main-is: UITestServers.hs
  hs-source-dirs: tests
  other-modules: ChatClient, ChatTests.DBUtils, ChatTests.DBUtils.SQLite, NameResolver
  default-extensions: StrictData (as in the test-suite)
  ghc-options: -threaded
  build-depends: (the test-suite's build-depends, incl. hspec with the same GHC conditional, wai, warp, http-types, sqlcipher-simple)
  (client_postgres conditions copied from the test-suite: -DdbPostgres and ChatTests.DBUtils.Postgres; cabal-version 1.12 has no common stanzas)
```

The default-off flag keeps CI's `cabal build --enable-tests` from building it. `UITestServers.hs` reuses the server configurations of the Haskell tests in `tests/ChatClient.hs` (no simplexmq test modules are needed; the server code is in the simplexmq library), but runs each server as an `async` instead of the `with*` brackets, so a server can be stopped and started (starting an XFTP server first creates its `filesPath` with all parent directories (`createDirectoryIfMissing True`; `withXFTPServer'`, tests/ChatClient.hs:752-757, creates only the last directory), since the server doesn't create it and `reset` deletes the server's whole directory):

- **Per server id** (`smp1`, `smp2`, `xftp1`, `xftp2`) its own `TestParams`: `tmpPath = <jvmDir>/servers/<id>` (created first), `portBase = base + 10 × index` (smp binds portBase+1, xftp portBase+2, ChatClient.hs:638/:709), and query-stats maps from `TM.emptyIO` (tests/ChatTests/DBUtils/SQLite.hs:7-13).
- SMP: `smpServerCfg` (:635-685) with `serverStoreCfg = persistentServerStoreCfg` (:687-688; the default is `SSCMemory Nothing`, :645), so a stop/start keeps queues. The test server's message quota is 16 per queue (:640); `PEERCTL` tests send fewer than 16 messages to a stopped peer, except catalogue 02 `status_quotaExceeded_warningAlert`, which sends 17 to reach the quota.
- XFTP: `xftpServerConfig` (:706-746), files, store log and stats under the server's own `tmpPath`.
- **Distinct hosts.** `/_servers` rejects two servers with the same host (`USEDuplicateServer`, Operators.hs:555-562, compares the host only), so `smp1`/`xftp1` use `localhost` and `smp2`/`xftp2` use `127.0.0.1` (as tests/ChatTests/Profiles.hs:3240 does); the servers bind dual-stack, and clients check the CA fingerprint, not the host name.
- **Names.** One `NameResolver` (Warp, tests/NameResolver.hs) runs for the process lifetime and `smp1` is started with `resolverNamesConfig` (NameResolver.hs:86) pointing at it, so registrations survive server restarts within a test. Its registry is an in-memory map with insert only (NameResolver.hs:37-55); `UITestServers.hs` adds a `clear-names` command that empties it, and `restoreBaseline()` sends it in every reset, so one test's registrations can't change another's result. The GUI's servers get the names role only in `NAMES` tests.
- TLS: `tests/fixtures/tls/{ca.crt,server.crt,server.key}`; the harness starts the process with the repo root as working directory (the configs use relative paths). The fingerprint is fixed (`LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=`).
- **Ports.** Each JVM takes the first free slot `k` (0 ≤ k < 120) by holding an exclusive lock (`FileChannel.tryLock`, an fcntl record lock; the channel and lock are kept in a static field, since a collected channel closes and releases the lock, and the slot file is never opened again in that JVM, as closing any descriptor of the file drops the lock) on `slot-<k>.lock` in the run directory for its lifetime (the runner creates a fresh run directory for each invocation, 7.1.1; a slot is released when its JVM exits, and a JVM shutdown hook first stops its peers and relays, closes `uitest-servers`' stdin and waits for it to exit, at most 10 s, and kills any remaining descendant processes (e.g. the `gsettings monitor` the theme detector spawns, which otherwise outlives every GUI JVM), so the next JVM on the slot never meets the old servers on its ports; `uitest-servers` is spawned like the relays, under `setpriv --pdeathsig KILL` from the same daemon thread, 8.1, which covers a JVM that is killed), and gives the process `base = 20000 + 100 × k`, so every port stays below the namespace's ephemeral range (32768–60999), where a fixed bind could collide with an outgoing connection. Other fixed ports a test opens (HTTP and SOCKS stubs, a chosen remote-desktop port) come from the same block.

Control protocol on stdin/stdout, one line per command, every command run under `try`:

```
start <id> smp|xftp            → ready <id> <server address>
stop <id>                      → stopped <id>      (after the server has saved its store: stop = cancel, which waits)
reset <id>                     → ready <id> <server address>   (stop, delete the store, start: "restarted without its store")
register <domain> [contact=<link>] [channel=<link>]   → registered <domain>
clear-names                    → cleared
(any failure)                  → error <id|-> <message>
```

The process sets `LineBuffering` on stdout (a pipe is block-buffered by default), and the harness ignores output lines that aren't control responses (server logs). It starts `smp1` and `xftp1` at launch and exits on stdin EOF (each SMP server replaces the process's SIGINT handler, so the harness ends it by closing stdin, then, after 10 s, SIGKILL from the shutdown hook's `destroyForcibly()` (7.1)). `TestServers.restoreBaseline()` in each reset starts or resets any server the previous test stopped or reset, stops `smp2`/`xftp2` if they ran, and clears the names registry. `smp2`/`xftp2` are started only for tests that need them (catalogue codes `SMP2`, `XFTP2`).

`TestServers.configure(user)` sends `/_servers <userId> <json>` with the preset operator groups `enabled: false` and one custom group with `smp1` and `xftp1`; this needs no conditions acceptance (`APISetUserServers`, Library/Commands.hs:1805-1819, and `validateUserServers`, Operators.hs:537-596, never check it; disabled groups are skipped through `opEnabled`, :544, :592-593). The preset operators are global rows created at chat start (src/Simplex/Chat.hs:295) and enabled by default (Presets.hs:27/41). `configure` runs right after `startChat` (the core rejects `/_servers` with `chatNotStarted` before it, found in P1a); a fresh user has no connections, so nothing connects in between; a fixture database that already has a user is started by `initChatController` before `configure` (7.2), which is harmless because the run has no network and fixtures are created against the local servers (11.1). In `NAMES` tests the custom group's `smp1` also gets the names role (see **Names** above).

This replaces the `smp-mini` approach: the `smp-server` executable doesn't build from the cabal tarball because the TH `embedDir` of `.well-known` is missing from `extra-source-files`.

### 8.3 Network isolation

(As built: as below for desktop (0.5.9); peers have no `--mute` yet, so the D-Bus point stands. The Android runner does not apply `wd.sh net isolate`, open item, 0.5.9.)

The Gradle run executes in an unprivileged user, network and pid namespace with only loopback (runner script, 7.1.1). Everything it starts (test JVMs, CLI peers, relays, `uitest-servers`) inherits it. Checked on this host: loopback works, `localhost` resolves from `/etc/hosts`, public DNS and every public address fail with "Network is unreachable".

- The isolation is network-only: D-Bus and X11 sockets are files and stay reachable, hence `--mute` for peers (8.1) and the recording notification manager for the GUI.
- Tests can never reach public servers by accident, including the preset operators' servers.
- Onboarding needs no core change: onboarding opens no connection (the conditions text is built into the core); after onboarding, the test calls `GuiUser.useLocalServers()` (same `/_servers` call as `configure`) before anything connects. `acceptConditions` re-enables the preset operators during onboarding (ChooseServerOperators.kt:301-305), which is why the call comes after "Chats" appears.
- Remote desktop ("Link a mobile") works over loopback: simplexmq's `getLocalAddress` keeps 127.x addresses (ordering them last), and ConnectMobileView.kt:526 keeps `127.0.0.1` when it is the first address — in the namespace it is the only one.
- Multicast discovery works on loopback with `ip route add 224.0.0.0/4 dev lo src 127.0.0.1` (without `src` the packet's source is 0.0.0.0 and simplexmq rejects it because the source must equal the invitation host, RemoteControl/Client.hs:387-389). All JVMs share one namespace and UDP port 5227, so multicast tests are in the `Multicast` category and run in the serial second pass (7.1.1). A multicast reconnect needs a stored pairing first (new hosts are refused over multicast, RemoteControl/Client.hs:118). (`ip link set lo multicast on`, also run, is not needed on kernel 6.18 but harmless; measured with a copy of simplexmq's Discovery.hs listener and sender: both group joins succeed and receive with source 127.0.0.1)
- Inside the namespace every process runs as uid 0 of the user namespace; files it creates belong to the real user outside.
- Link previews use `http://127.0.0.1:<port>` pages from the local HTTP server; `localhost` URLs are not parsed as links by the core (Markdown.hs:388).

## 9. Screen objects and the DSL

### 9.1 Screen objects

(As built: see 0.5.1. Built screens: `App`, `ChatListScreen`, `NewChatScreen`, `UserPickerScreen`, `ProfilesScreen`, `AddressScreen`, `AppearanceScreen`, `ProfileScreen`, `ChatScreen`, `ContactInfoScreen`, `GroupInfoScreen`, `MemberInfoScreen`, plus the chat-list, onboarding and What's New helpers; no `GuiUser`, `ChatRow`, `ContactRequestRow`, `GroupInvitationRow`, `ConnectPlanAlert` or `RelayPeer`; `ChatListScreen.row()` returns the screen.)

```kotlin
class GuiUser(override val name: String, internal val d: UiDriver) : TestUser {
  val chatList get() = ChatListScreen(d)
  fun userPicker(): UserPickerScreen = chatList.openUserPicker()
  fun settings(): SettingsScreen = userPicker().settings()
  fun newChat(): NewChatSheet = chatList.newChat()
  fun openChat(name: String, block: ChatScreen.() -> Unit = {}): ChatScreen = chatList.row(name).open(block)
  fun useLocalServers()                        // after onboarding (8.3)
  fun console(cmd: String): List<String>       // Developer tools → Chat console, for setup steps the UI has no button for
}
class ChatListScreen(private val d: UiDriver) {
  fun row(name: String): ChatRow               // resolves name → chat id from chatModel, waits for exactly one match
  fun request(name: String): ContactRequestRow // a contact with a pending request ("Open to accept"; accept/reject through the row menu)
  fun invitation(group: String): GroupInvitationRow  // tap opens the "Join group?" alert (ChatListNavLinkView.kt:196-199)
  fun openUserPicker(): UserPickerScreen
  fun newChat(): NewChatSheet
  fun pasteLink(link: String): ConnectPlanAlert
}
class ChatRow(private val d: UiDriver, val key: UiKey) {
  val unread: UiUnread; val incognito: Boolean; val node: UiNode
  fun open(block: ChatScreen.() -> Unit = {}): ChatScreen; fun menu(action: String)
}
class ContactRequestRow(private val d: UiDriver, val key: UiKey) { val node: UiNode; fun accept(incognito: Boolean = false): ChatRow; fun reject() }
class GroupInvitationRow(private val d: UiDriver, val key: UiKey) { fun join(): ChatRow; fun delete() }
class ChatScreen(private val d: UiDriver) {
  val title: UiNode; val banner: UiNode
  val messageText: String; val placeholder: String   // the compose field's text and placeholder, through UiDriver.composeField() (7.4); on Android read from its EditText (catalogue 05 setup item 5)
  fun send(text: String)                       // Ui.Chat.Message + Ui.Chat.Send
  fun item(text: String): ChatItemNode         // text matched inside chat.item[…] nodes only
  fun shouldSeeMessage(text: String)           // waits for an item with this text
  fun info(): ChatInfoScreen
}
class ChatItemNode(private val n: UiNode) { val sender: String?; val meta: UiItemMeta; fun menu(action: String) }
class NewChatSheet(private val d: UiDriver) { fun createOneTimeLink(): String; fun createGroup(name: String): ChatScreen; fun createChannel(name: String): ChatScreen }
class ConnectPlanAlert(private val d: UiDriver) { val profileName: String; fun openNewChat(): ChatScreen }
class UserPickerScreen(private val d: UiDriver) { fun address(): AddressScreen; fun settings(): SettingsScreen; fun profiles(): ProfilesScreen }
class SettingsScreen(private val d: UiDriver)
class AddressScreen(private val d: UiDriver) { fun create(): String; fun businessAddress(on: Boolean) }
class ChatInfoScreen(private val d: UiDriver)      // members added by the sub-areas that use them
class ProfilesScreen(private val d: UiDriver)
class RelayPeer(val name: String) { val link: String; fun run(cmd: String): List<String>; fun stop(); fun start() }  // 8.1
```

The address screen is reached from the user picker ("Create SimpleX address" / "Your SimpleX address", UserPicker.kt:220-223); Settings has no address entry. Names resolve to ids through `chatModel` (the test runs in the same JVM), so a test says `row("bob")` while the driver finds `chatList.row[@12]` regardless of list order. Each catalogue sub-area adds the screen objects its tests need.

### 9.2 DSL entry

(As built: `uiTest { }` takes no parameters; `app(user: String?): App` (`null` = onboarding boot); `restart(): App`; `cli(name, flags)`; `connect(peer)` and the other helpers of 0.5.7. `app()` is not limited to one call per test. Not built: `realTimes`, `fixture`, `relay`, `mobileHost`, `servers`, `allowUncaught`, `screenshot`, `shouldHave`, `peerVersion`.)

```kotlin
abstract class UiTest {                        // base class: per-JVM setup in its companion, watchdog, report on failure
  open val peerVersion: CliVersion = CliVersion.Current      // `…CompatStableTest` subclasses override it (10.1)
  fun uiTest(realTimes: Boolean = false, block: UiTestScope.() -> Unit)   // realTimes: display clock off (7.7)
  inner class UiTestScope {
    fun app(name: String, onboarded: Boolean = true, fixture: ((String) -> Unit)? = null): GuiUser  // runs resetForTest; first call, once per test
    fun cli(name: String, version: CliVersion = peerVersion, flags: PeerFlags = PeerFlags(), servers: PeerServers = PeerServers()): CliPeer   // 8.1
    fun relay(name: String, web: Boolean = false, register: Boolean = true): RelayPeer  // headless relay; registered in the GUI's servers unless register = false (relay-settings tests add it through the UI); run(cmd) via stop + `-e` + start
    fun mobileHost(name: String): CliPeer                           // current-build CLI acting as the remote-desktop host, created as cli(name, CliVersion.Current) (the start–stop–start cycle of 8.1 included)
    val servers: TestServers                                        // stop/start/reset (SRVCTL), register names (NAMES)
    fun restart(fixture: ((dbPrefix: String) -> Unit)? = null)      // RESTART: restartCore(fixture) (7.3)
    fun connect(gui: GuiUser, peer: CliPeer)                        // one-time link from the GUI, joined by the peer; consumes its lines
    fun <T : Throwable> allowUncaught(type: KClass<T>, block: () -> Unit)   // a step expected to raise an uncaught app error of this type (6)
    fun screenshot(name: String, of: UiNode? = null)                // named shot (7.7); every passing catalogue test also saves one at its end (except realTimes tests and -PuiShots=off); in a realTimes test it fails
    infix fun UiNode.shouldHave(m: SemanticsMatcher); infix fun ChatRow.shouldHave(m: SemanticsMatcher)
    infix fun <T> T.shouldBe(expected: T)
  }
}
```

`app()` must be the first call of a test and may be called once; a second call fails. Across tests the JVM is reused through `resetForTest` (7.3).

### 9.3 Example tests

(As built: the three tests exist as `OneTimeLinkCompatTest`, `AddressCompatTest` and `JoiningCompatTest` without stable subclasses. `JoiningCompatTest` reads `connect(bob)`, `alice.chatList.open("team")`, `click(Ui.Alert.Confirm(Ui.AlertSource.Shared))`, `bob.eventually("#team: alice joined the group")`.)

These are catalogue rows written in the DSL (the names are catalogue names). All three rows include `S`, so their tests live in `…CompatTest` classes with a stable subclass (10.1).

```kotlin
package chat.simplex.app.ui.main

open class OneTimeLinkCompatTest : UiTest() {        // catalogue 01, "1-time link"
  @Test fun oneTimeLinkCliConnectsViaShortLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = alice.newChat().createOneTimeLink()
    bob sends "/c $link"; bob expects "confirmation sent!"
    bob expects "alice: contact is connected"
    alice.chatList.row("bob")                          // the pending row has become contact "bob"
  }
}
@Category(StableCompat::class)
class OneTimeLinkCompatStableTest : OneTimeLinkCompatTest() { override val peerVersion: CliVersion = CliVersion.Stable() }

open class AddressCompatTest : UiTest() {            // catalogue 01, "Address"
  @Test fun addressCliConnectsAndRequestAppears() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().address().create()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")                      // contact row with "Open to accept" (accepting is another row's test)
  }
}
@Category(StableCompat::class)
class AddressCompatStableTest : AddressCompatTest() { override val peerVersion: CliVersion = CliVersion.Stable() }
```

```kotlin
package chat.simplex.app.ui.groups

open class JoiningCompatTest : UiTest() {            // catalogue 03, "Joining"
  @Test fun groupInvitation_tapRowShowsJoinAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")    // bob is the group host
    connect(alice, bob)
    bob sends "/g team"; bob expects "group #team is created"
    bob expects "to add members use /a team <name> or /create link #team"
    bob sends "/a team alice"; bob expects "invitation to join the group #team sent to alice"
    alice.chatList.invitation("team").join()          // tap the row → «Join group?» → «Join»
    bob expects "#team: alice joined the group"
  }
}
@Category(StableCompat::class)
class JoiningCompatStableTest : JoiningCompatTest() { override val peerVersion: CliVersion = CliVersion.Stable() }
```

Each test asserts what its catalogue row lists; the bodies above show the shape.

### 9.4 Key lint

(As built: not built. Superseded for now by the 2026-10-14 minimal-app-code rule (13), which makes "keys everywhere" a non-goal.)

A UI test that opens every screen reachable through the screen objects and fails if any node with a click action has no `TestTag` (a semantics walk: `SemanticsActions.OnClick` present, `TestTag` absent), with two exemptions no `Modifier.ui` can reach: `LinkTestMarker` link nodes whose nearest ancestor carrying a `TestTag` in the merged tree is their `Text` or the row it merges into (3.5), and the `ModalBottomSheetLayout` scrim that Material adds while a sheet is shown (matched as `OnClick`, no role, no `TestTag`, content description ["Close sheet"] and no children, not by bounds: start-modal sheets are 388 dp wide, measured; CreateProfile's image sheet, WelcomeView.kt:73; the profile editor's, UserProfileView.kt:80; AddGroupView.kt:88, AddChannelView.kt:248, GroupProfileView.kt:89, the attach sheet ChatView.kt:990). Screens that can't be reached in a test — composables not reachable from the UI (`BadgesYourLevelView` and `BadgesPayView`, behind the uncalled `ChooseLevelButton`, BadgesSupportSimplexView.kt:77) and the badge-display screens, which need a badge credential the harness doesn't have — are excluded; their keys are added anyway. It keeps "keys everywhere" true once all areas are keyed. A second, source-level check lists the top-level `var`s of the app's UI files (commonMain and desktopMain `views/`), and the top-level `val`s there whose type is mutable (`MutableState`, `MutableStateFlow`, mutable collections; e.g. `chatViewScrollState`, ChatView.kt:3230; `connections`, CallView.desktop.kt:28; `failedComposables`, Utils.kt:633), and fails on any that is not in the reset list of `clearUiState` (7.3) or an explicit allow-list (extension properties without a backing field hold no state and are skipped, 7.3), so file-level UI state added later is reset too.

## 10. Test catalogue (all screens, all features)

The catalogue is the definition of "thorough": one row per user-facing feature, with the entry path (exact `strings.xml` labels), the main file:line, the test method names, what the GUI must show and what the CLI peer must print (with the matching Haskell test), what the test needs, and notes. `00-legend.md` holds the shared format, Needs codes, output conventions, the owner of each overlapping feature and the class map (sub-area → test class and package).

| File | Area | Sub-areas | Tests | Ignored |
|---|---|---|---|---|
| `01-onboarding-chatlist-connect-profiles.md` | Onboarding, What's New, chat list (search, filters, lists, rows, row menus, requests, pending connections, cards), servers summary, user picker and chat profiles, new chat sheet, 1-time links, contact/address connection plans, prepared direct chats, pending connection info, address and business-address settings, SimpleX name, contact info, preferences, delete-contact variants | 15 | 352 | 0 |
| `02-chat-view-messages.md` | Chat view shell and toolbar, composing (markdown, links, previews, mentions, bot commands), live and timed messages, voice, attachments, replies, edit and history, delete and rendering of moderated items, multi-select, forwarding, reactions, copy/share/save, member-side reports, item info, unread and scrolling, search and content filters, drafts, status icons, special items, notes, context menus and keys | 24 | 281 | 4 |
| `03-groups-channels-business.md` | Create group, invite, group info rows, profile and welcome message, group preferences, member admission and review, group link, joining and group/channel/business plans, member list and multi-select, member info, member contacts, support chats, moderator-side reports, moderation, mentions-only notifications, events and history, leave/delete/clear, channels as owner and as subscriber, business chats from both sides | 20 | 312 | 4 |
| `04-settings-database-remote.md` | Settings root, privacy, SimpleX Lock and self-destruct, appearance, network and servers, calls settings, chat data and database, migration, version and developer tools, help, startup notices, remote desktop "Link a mobile" (desktop is the controller, a CLI is the host), supporter-perks screens without badge display | 13 | 232 | 20 |
| | | **72** | **1,177** | **28** |

The four catalogue files and the legend live in `plans/ui-tests/catalogue/` while the plan is in review; P1 moves all five into the repo at `apps/multiplatform/common/src/desktopTest/resources/uitest-catalog/` (with `05-android-only.md` and `05-android-carryover.md` beside them, for the Android harness) (their paths are already repo-relative), and tests read them from the classpath.

### 10.1 Rules

(As built: none of the category, Compat-stable or traceability machinery is built (P1b). Tests outside these rules exist: desktop-only tests in `chat.simplex.app.ui.desktop` (`OnboardingTest`, `…DesktopTest`) and Android-only tests in `chat.simplex.app.ui` and `chat.simplex.app.ui.back`; the traceability check needs a rule for them before it is built.)

- **Classes.** Each sub-area maps to one test class, or to several consecutive classes when it has more than 30 tests (82 class bases in all; the legend's class map lists each base and its first test; the package is `chat.simplex.app.ui.<main|chat|groups|settings>` for catalogues 01–04, legend rules). A base with `S` tests has, for those tests, `open class <Base>CompatTest` and `@Category(StableCompat::class) class <Base>CompatStableTest : <Base>CompatTest()`, which overrides `peerVersion`; its other tests are in `<Base>Test`. JUnit 4 runs the Compat tests once per peer version. Test method names are the catalogue names. `S` is never combined with `RELAY`.
- **Traceability check.** `CatalogueTraceabilityTest` is a plain JUnit test (it doesn't boot the app). It parses catalogues 01–04 from the classpath (not 05, which belongs to the Android harness) and fails if:
  - a catalogue name has no declared `@Test` method in its mapped class (the Compat class for `S` names), or has more than one;
  - an `IGNORE` name's method lacks `@Ignore`, or its reason differs from the row's (the cell text after `IGNORE: `, verbatim);
  - a method's Needs-derived categories (method and class annotations) differ from those its row implies (`RELAY` → `NeedsRelay`, `NAMES` → `NeedsNames`, `FIXTURE` → `NeedsFixture`, `MCAST` → `Multicast`, `SLOW` → `Slow`); `Smoke` and `StableCompat` are not compared;
  - a base with `S` names lacks `<Base>CompatStableTest` with `@Category(StableCompat::class)` overriding `peerVersion`;
  - a `@Test` method in `chat.simplex.app.ui` has no catalogue row (except the traceability test, the key lint and its source-level check (9.4) and the harness's own tests in `chat.simplex.app.ui.harness`: VT model, longest link, reset completeness, alert dismissal by reflection, render cost; the harness tests carry no `Smoke` category);
  - a committed screenshot under `uitest-screenshots/` belongs to no catalogue test (its `<TestClass>/<test>` has no catalogue name);
  - in strict mode (`-PuiStrict`, passed to the test JVM as `simplex.uitest.strict`, 7.1; P7), any non-`IGNORE` name still carries `@Ignore`.
- The check finds classes by walking the test classes directory and loads them with `Class.forName(name, false, loader)`, so no test class is initialised (initialising one would run `UiTest`'s per-JVM setup).
- **Assumption skips.** A test may skip itself through a JUnit assumption only where its row says so (one row: catalogue 02 `meta_signature_missingAlert`, which depends on the stable tag); the runner reports skips separately from passes.
- **Overlaps are tested once.** The owner of each overlapping feature is fixed in `00-legend.md`.
- **Badge display stays excluded.** No row asserts a badge; the supporter-perks screens are tested without badge display.
- **Categories.** `XFTP` needs no category (`xftp1` runs in every JVM). `StableCompat` marks the stable subclasses. `Smoke` marks one test per sub-area with runnable tests (71, P2), never an `MCAST` test; `CatalogueTraceabilityTest` also carries `@Category(Smoke::class)`, so `-PuiTags=Smoke -PuiExcludeTags=StableCompat` runs the 71 smoke tests plus the traceability check.

### 10.2 Runtime and resources

Per test: a reset (database, prefs, composition; a few seconds) plus the test itself (5–20 s typical; the 30-second timed-message test and the other `Slow` tests up to 10 min). The full suite is several hours serially. Classes run in parallel with `-PuiForks=N`; each JVM has its own data directory and its own server ports. Memory: the Gradle single-use daemon (~2 GB, `org.gradle.jvmargs`), plus per parallel JVM a 2 GB heap, the native core, `uitest-servers` and the peers (an idle CLI peer is about 60–80 MB RSS; the 21-member group test needs about 1.7 GB of peers). As built, the runner picks `-PuiForks` from free memory: (MemAvailable − 2.5 GB) / 1 GB, between 1 and 4, unless `-PuiForks=N` is given, and refuses to start with less than 3 GB available (15.4 item 24); pass a lower `-PuiForks` when a class with a large group runs. The `Smoke` run (71 sub-areas with runnable tests, plus the traceability check) boots 71 JVMs, each with a boot, server start, a reset and one test. With `forkEvery = 1` (the design; as built the default is 0, 0.5.9) Gradle also forks a JVM for every other class whose tests the categories filter out (verified with Gradle 8.12; such a JVM initialises no test class and exits quickly), so about 115 forks in all: an estimated 30 minutes serially and about 15 with 2 forks (P1 measures it). It is meant for checks before a PR, not for every edit; a single class runs with `--tests`.

## 11. Infrastructure the catalogue needs

### 11.1 Harness pieces (test code only)

| Piece | Used by (Needs code) | How |
|---|---|---|
| Local SMP and XFTP servers | everything (`SMP`, `XFTP`) | (As built: `smp1` only; `xftp1` is configured in the GUI but not served, P1b, 0.5.6.) `uitest-servers` `smp1`/`xftp1` (8.2), once per JVM; peers get `-s`/`--xftp-server`, the GUI gets them in `resetForTest` (or `useLocalServers()` after onboarding) |
| Second SMP / XFTP server | private routing, sent-via-proxy, unknown-XFTP approval, manual server add, a member connection that fails (`SMP2`, `XFTP2`) | `uitest-servers` `smp2`/`xftp2` on host `127.0.0.1` |
| Names | SimpleX names (`NAMES`; 13 tests) | the process-lifetime resolver behind `smp1` and `register` lines; GUI and peers get the names role |
| Chat relays | channels and relay settings (`RELAY`; 90 tests) | headless relay processes (8.1) |
| Network namespace | every test | 8.3 |
| CLI peers | `P`, `S` | `CliPeer` (8.1): the current build from `dist-uitest`, or the release binary where a `P` row names a stable peer; for `S`, the release binary, in `…CompatStableTest` (10.1). Peers use `smp1`/`xftp1` unless a row puts them on `smp2`/`xftp2` (`PeerServers`) |
| Multicast | `MCAST` | the loopback multicast route (8.3); `Multicast` category, run serially in the second pass (7.1.1) |
| Developer tools | `DT` | `appPrefs.developerTools.set(true)` after `app()` (SimpleXAPI.kt:149); the toggle itself is tested through the UI in catalogue 04 |
| Local HTTP server | link previews (`HTTP`) | serves pages with Open Graph tags on `http://127.0.0.1:<port>`; `privacyLinkPreviewsShowAlert` is pre-set except in the first-time-alert tests |
| SOCKS5 stub | SOCKS proxy routing (`SOCKS`) | in-JVM SOCKS5 server on a port from the JVM's block, with RFC 1929 username/password auth (the default proxy mode isolates by credentials, SimpleXAPI.kt:5301) and domain addressing; records CONNECTs to the local SMP port |
| Server control | delivery errors, retries, upload/download failures, address-switch abort (the switch stays abortable only while the SMP server is stopped) (`SRVCTL`) | `stop`/`start`/`reset` (8.2); races are avoided by stopping the server before the action (a 17 MB file leaves only about 0.4 s of visible upload and about 1 s of download, measured, so a large file alone doesn't avoid them); an XFTP request in flight when its server is reset hangs until its request timeout (TCP connection background timeout + protocol background timeout + 4096 KB × the per-KB timeout: `http2RequestTimeout` adds the connection timeout to the chunk timeout, simplexmq Transport/HTTP2/Client.hs:189-194, 209-210, FileTransfer/Client.hs:209-214, 300-306), and the retry 2 s later gets AUTH at once; the error arrives 75 s after the reset with CLI defaults (127 s with `--tcp-timeout 14`; 118 s with the GUI's 30 s background timeout and 10 ms/KB, SimpleXAPI.kt:5256-5258), so a test that resets an XFTP server during a transfer first sets short network timeouts and passes an explicit timeout, or is `Slow` with an explicit 180 s timeout (measured, matching that sum plus the 2 s retry — 57 s with the lowest values the settings UI offers, about 20 s with the UI's lowest per-KB value plus `/network socks=off timeout=1` in the chat console, which catalogue 02 `sndFile_xftpResetDuringUpload_showsFileError` uses); the next reset restores the baseline. After a plain stop/start of the XFTP server, by contrast, an in-flight upload may hang the same way (2 of 4 measured runs; the other 2 finished 0.8–1.7 s after the start) and then completes: the store log keeps the FNEW record, so the retried chunk is accepted (tests/ChatClient.hs:711-712, simplexmq FileTransfer/Server/StoreLog.hs:121-123, FileTransfer/Server.hs:572-605); while the server is still down the errors are temporary and never surface: each round of 3 attempts ends, after the backoff that follows the third failure, without an error, and the worker restarts at once (FileTransfer/Agent.hs:268-277, 520-544, Env/SQLite.hs:257, Agent/Client.hs:2311), taking the chunk with the fewest retries (each retry adds 1, AgentStore.hs:3777, 3824) from its stored delay, so the upload resumes once the server is back, though after a longer outage the next attempt can be up to 180 s away. After a restart, clients reconnect with backoff from 2 s up to 180 s (simplexmq `defaultReconnectInterval`, Agent/Env/SQLite.hs:197-202; message delivery retries cap at 120 s, :205-219; both stay at 2 s for 10 s, then grow ×1.5), which `UiIdle` doesn't see. `/reconnect` (`ReconnectAllServers`) does not shorten this: with the server down the agent holds no connection to close, and the resubscription and delivery workers keep sleeping (Agent/Client.hs:1048-1079 closes only existing clients; the resubscription worker, Client.hs:820-853, and delivery retries, Agent.hs:2601, keep sleeping; measured: 40 s down → delivery 14.9 s after the start, with or without `/reconnect`; 10 s down → 3.2 s; 20 s → 4.4 s). So tests keep outages under about 10 s (the backoff stays at 2 s for the first 10 s), or stop and start their CLI peers after the server starts (`PEERCTL`, delivery within about 2 s, measured 0.9 s unencrypted and 1.5 s with `--key`; the GUI side the same with `restart()`, about 1–3 s), or pass an explicit timeout of about half the outage plus 5 s |
| Peer control | ratchet sync, offline peers, stopped relays (`PEERCTL`) | `CliPeer.stop/start/snapshot/restore` (8.1), `RelayPeer.stop/start` (9.1) |
| Core restart inside a test | passphrase prompts, lock screen, startup DB errors, persistence, self-destruct, notices checked at first composition (`RESTART`) | `restart()` (7.3) |
| Boot without a user | onboarding (`ONBOARD`) | `app(name, onboarded = false)` (7.2); `ONBOARD` means the test boots with no user |
| Database fixtures | legacy states, startup DB screens, invalid-data items, backdated items (`FIXTURE`) | `app(name, fixture = …)` (7.2), or a fixture passed to `restart(fixture)` (applied while the core is stopped, 7.3) for data the test produced itself. **SQL edits** (backdating, fake migration rows, legacy rows) use the current CLI, as the system `sqlcipher` (3.x) can't open SQLCipher 4 databases: `<current CLI> -d <prefix> [--key <key>] --maintenance -y -e "/sql chat <SQL>" -t 0`, with stdin `/dev/null` (with several users and none active the CLI would otherwise wait at "Select user profile:"; it re-activates a lone inactive user on open, so a fixture that clears `active_user` does that last), and the helper fails if the output contains `chat db error` (a failing `/sql` still exits 0) (or `/sql agent`; `--maintenance` doesn't start the chat; `-y` confirms pending migrations, which the default console mode would prompt for, Simplex/Chat.hs:77, e.g. on a v6.3.7 fixture). The database must already have a user: with none the CLI exits with "no active user in maintenance mode" (Hs/Core.hs:89-91), so SQL edits apply only to databases that have a user (the GUI's own after onboarding, or one created by a CLI). The key is `ksDatabasePassword.get()` for the GUI's own database and absent for a fixture database created unencrypted. **Databases from an older CLI** (`-PuiFixtureTags`, default `v6.3.7`, the last release before short links; downloaded by `uitestPrepare`; path from `UiEnv.fixtureCli(tag)`) are created without a pty and without `CliPeer` flags (v6.3.7 has no `--user-display-name`): `printf '<name>\n' \| <cli> -d <prefix> -s <smp1 address> -e '<cmd>' -t 1`, the display name answered on stdin; with no `--key` the database is unencrypted, which the GUI opens with the empty key (7.2). Fixtures always use the local servers |
| TTL expiry | chat TTL tests | setting or lowering a TTL through the UI expires backdated items at once (`APISetChatTTL`/`APISetChatItemTTL`, Library/Commands.hs:1842/1884); group items created in the last 12 h are kept (:5969-5970), so fixtures backdate both `item_ts` and `created_at` by more than 12 h. Only "expires on startup" waits for the expiration thread (first run 30 s after start, then every 30 min, src/Simplex/Chat.hs:124-127) and is `Slow` |
| Stubs | `FD`, `CB`, `URI`, `NTF`, `TOAST` | file dialogs (7.6), in-memory clipboards, recording `LocalUriHandler`, `RecordingNtfManager`, recorded toasts, AppLock focus simulation (`appWasHidden()` + `recheckAuthState()`), `escape()` |
| `platform` object | app-update notice | `UiTestPlatform`, the same `PlatformInterface` as desktop/Main.kt:65-90 |
| Pre-set prefs | every onboarded test not about them | `laNoticeShown`, `appUpdateNoticeShown`, `whatsNewVersion` (via `setLastVersionDefault`), `oneHandUICardShown`, `addressCreationCardShown`, `getStakeBannerDismissed`, `supporterBannerShown`, `showHiddenProfilesNotice`, `showMuteProfileAlert`, `showDeleteConversationNotice`, `showDeleteContactNotice`, `privacyDeliveryReceiptsSet`, `privacyLinkPreviewsShowAlert`. Tests about a notice clear its pref; notices checked once at first composition (SimpleX Lock notice, App.kt:94-103; app-update notice, desktop Main.kt:79-87; What's New, ChatListView.kt:219-228) need a `restart()` after their conditions are set up Separately, the per-JVM baseline (7.2 `bootJvm`, before `PrefStores.capture`) sets `AppearanceBarsBlurRadius` = 0, toolbar blur off, for every test including onboarding boots (user decision 2026-10-13, performance); `appearance_toolbarBlur` switches it on itself. |
| Fixture files | attachments | In the repo: `tests/fixtures/test.jpg`, `test.pdf`, `test_1MB.pdf`, `test.txt`; `images/files.gif` (animated GIF, 1.0 MB); `website/src/video/connect.mp4` (1.9 MB, over the 1023 KB auto-receive limit); `docs/themes/example.theme` (theme import). Added in P1 to `apps/multiplatform/common/src/desktopTest/resources/uitest-fixtures/`, generated once with ffmpeg (commands in a README beside them, `-fflags +bitexact`, so they are reproducible) and committed: `small.mp4` (a 2 s test pattern, under 1023 KB), `audio.webm` (audio only), `video.webm` (with a video track), `broken.mp4` (random bytes with an .mp4 name, undecodable). Written by the harness at run time: a 150 MB file of random bytes (the XFTP-reset-during-upload test needs a few seconds of visible upload; a 17 MB file gives about 0.4 s), a sparse file over 1 GB, and a JPEG over 510 KiB (received as an image that isn't auto-accepted) |

### 11.2 App-side seams

Besides keys and typed semantics (which include the `PlatformTextField` key parameter of 3.3 and making `keyForItem` internal) and the `UiIdle` busy tracker of section 6 (hooks in `ChatController.sendCmd`, `startReceiver`, `withBGApi`/`withLongRunningApi`/`withApi`; approved 2026-10-13 and implemented, 0.5.2), the catalogue needs three seams, which are approved, and the screenshots (7.7) need a fourth, the display clock (approved 2026-10-12). Each is an `internal var` (or constructor parameter) defaulting to the production implementation. `bootJvm` installs the fakes of the three catalogue seams for every test (the display clock is set per test by `uiTest(realTimes)`, 7.7) (the production implementations need VLC, audio devices or an AWT clipboard, and some paths reach them without asserting on them), and `clearUiState` resets their per-test state together with the app's globals around them (`AudioPlayer.currentlyPlaying`, D/platform/RecAndPlay.desktop.kt:95; `RecorderInterface.stopRecording`, C/platform/RecAndPlay.kt:10); the catalogue codes mark the rows that drive or assert on a seam.

| Seam | Location | Unblocks |
|---|---|---|
| Recorder factory | D/platform/RecAndPlay.desktop.kt:25-90 (VLC `pulse://`/`dshow://`) | voice recording and voice drafts (9 tests; catalogue code `REC`) |
| Audio player / video frame provider | RecAndPlay.desktop.kt:92, D/platform/VideoPlayer.desktop.kt:217 (player), :221 (`getBitmapFromVideo`), :269 (`hasVideoTrack`) | voice playback, video previews and playback, the decoding-error alert, the video-or-file choice for pasted .webm files, the incoming-call ringtone (11 tests; code `AV`); the seam is an app-side interface (vlcj's `MediaPlayer.status()`/`media()` and their API classes are final and need native libvlc, so the fake can't be a vlcj object); the fake starts at position 0 and advances on the real clock (a fake that reports the end on the first poll raced the two assignments `play()` makes after `start()` returns, leaving the pause icon in 64 of 600 runs), and at the end of a track it reports a position equal to the media duration (`status().time()` == `media().info().duration()`, RecAndPlay.desktop.kt:269-273) while `isPlaying` is still true, as VLC does (:132-137), so the app's poll ends playback and sets progress to 0 for `resetOnEnd` (voice items, CIVoiceView.kt:66) or to the full duration (the compose preview, ComposeVoiceView.kt:56), :209-212, instead of leaving a partial slider |
| Display clock | `getTimestampText`, `getTimestampDateText`, `localTimestamp`, `localDate` (C/model/ChatModel.kt:3944-3998), `FloatingDate` (C/views/chat/ChatView.kt:2894), the 60 s grouping check (ChatView.kt:3765, :3784), the SimpleX-name sale countdown (SetSimplexNameView.kt:195-205): an `internal var` with `displayInstant(t)` and `displayNow()`, production = identity and `Clock.System.now()` | stable screenshots (7.7): in UI tests every displayed time and "now" map to one fixed instant (all tests unless `realTimes = true`) |
| Clipboard read on Ctrl+V | D/platform/PlatformTextField.desktop.kt:147-152 (AWT system clipboard, throws headless) | Ctrl+V in the compose field when the clipboard holds plain text or an image rather than a file list (1 test; code `IMGCB`) |

OS drag and drop is not exercised: the drop target (`desktopOnExternalDrag`, D/platform/Modifier.desktop.kt:19-58, used at ChatView.kt:979-987) needs an AWT drop event, and the compose state it feeds is local to `ChatView` (ChatView.kt:148). The attach path behind it (`onFilesAttached`, ComposeView.kt:290) is reached by Ctrl+V of a file list from the in-memory clipboard (PlatformTextField.desktop.kt:144-145 → ComposeView.kt:1516). Message links are clicked through the text layout (3.5, no app change).

### 11.3 Out of reach in-process (rows marked `IGNORE` with one of these reasons)

- Real WebRTC calls (the browser page).
- Tray, close-to-tray, window size/position persistence, the floating console window, the crash-restart loop and crash alert.
- The app-update download (hard-coded GitHub URL and private HTTP client).
- OS folder opener (`desktopOpenDir`), until the seam in section 13 is approved.
- OS drag and drop onto the window (11.2).
- Destinations on public servers (e.g. "Follow SimpleX Network News", badge-code redemption, the migration upload, which goes to the preset XFTP servers of a temporary user created at `SimpleXAPI.kt:722`): unreachable in the namespace.
- Known app bugs that make the expected result unreachable, until fixed (each is reported when the test is written): the "Error importing chat database" alert (`apiImportArchive` throws `Exception`, `importArchive` catches only `Error`).
- Pixel-level visual settings: only the persisted prefs are asserted.
- Host "pending approval" and "join rejected", which need Haskell `chatHooks`.
- External resources: the channel-webpage embed script (loaded from simplex.chat); real Tor and onion services.
- Code that is commented out or unused, where a visible entry point remains in the source: "Sign messages", `migrationFromStage`, the expired group-invitation item (`CIGISExpired` is never set by the core); commented-out features with no remaining entry point ("Renegotiate encryption", invite by email) get no rows.

## 12. Phases and PRs

| Phase | Deliverable | Depends on |
|---|---|---|
| P1a (complete 2026-10-13, 15.1) | A thin vertical slice that proves the approach on the real app before the rest is built (about 1–2 weeks): the runner script and namespace (7.1.1, 8.3); `desktopUiTest`; per-JVM directories; `UiTestApp` (`bootJvm`, `resetForTest`, `restartCore`, onboarding boot); `UiIdle` (approved, 6, 13) with the hard timeout (no other watchdog parts); `InProcessDriver` with click, text input, keys and `waitUntil`; `CliPeer` Current only; `uitest-servers` with one SMP server only (no XFTP, relays, names resolver or control protocol); pre-set prefs; the keys and screen objects these tests use. Tests (catalogue names, Current CLI only, no stable subclasses): the three of 9.3 (`oneTimeLinkCliConnectsViaShortLink`, `addressCliConnectsAndRequestAppears`, `groupInvitation_tapRowShowsJoinAlert`), 02 `sendText_enter_peerReceives` and 04 `appearance_settingsPersistAcrossRestart` (a `restartCore` with `PrefStores`). Not in P1a: screenshots, Kover, the seams of 11.2, the dialog drain and file choosers, the Stable CLI, relays, mobile hosts, HTTP and SOCKS stubs, stub generation and traceability, K0. Exit: the 5 tests pass 20 consecutive runs; these are measured on the real app and recorded in 15: boot and reset time per test, frame render cost (14), `captureToImage` time on the full scene (the settle-step input of 7.7), memory growth over 20 resets in one JVM, and the offline Gradle run in the namespace; the scene runner choice (`runInternalSkikoComposeUiTest` or the fallback) and the DSL shape are confirmed or changed before P1b | — |
| P1b | The rest of the harness, completing what P1a started: runner script and namespace (7.1.1, 8.3), `desktopUiTest` and `uitestPrepare` tasks, per-JVM directories, `UiTestApp` (`bootJvm`, `resetForTest`, `restartCore`, onboarding boot, fixtures), `UiIdle` (approved 2026-10-13, 13) and the watchdog (6), the four seams of 11.2 and their fakes (and the fixed display clock), screenshots (7.7: capture, settle loop, masks, compare-and-write, summary, docs index, the `.gitattributes` LFS line; `uiVolatile()` on the volatile composables), coverage measurement (7.8: Kover on `desktopUiTest`, per-pass reports merged by the runner, the zero-coverage list), `InProcessDriver` with all input methods, `TestRoot` stubs, `CliPeer` (Current + Stable, peer control), relays and mobile hosts, `uitest-servers` (cabal flag, all servers, names resolver, control protocol), HTTP and SOCKS stubs, pre-set prefs; K0 (all helper parameters and fixed keys) and the keys and screen objects the three 9.3 tests use (chat list rows and requests, group invitation row, user picker and address screen, new chat sheet and one-time link, chat screen, compose and items); the catalogue and legend moved into the repo; `CatalogueTraceabilityTest`; every catalogue name generated as a stub in its mapped class (the Compat class and its stable subclass for `S` names) with the categories its Needs imply (10.1) — `@Ignore("not written")` for runnable rows, `@Ignore("<the row's IGNORE reason>")` for `IGNORE` rows (the generator escapes `"`, `\` and `$`); the three tests of 9.3; measurements of frame render cost (14) and of the `Smoke` run time (10.2); one run of the connection-failure recipe of catalogue 03 `memberInfo_connectionFailedSection`, which is derived from the core and agent code only, choosing its sync point before the server reset; on day one, `./gradlew -g .gradle/uitest-home --offline --no-daemon help` on the real project inside the namespace, after the outside step has filled that home (configuring the real project there has worked once, `./gradlew help` in 3 min 11 s; an offline run is not confirmed yet)  | P1a |
| P2 | One stub per sub-area with runnable tests (71) turned into a passing `Smoke` test, with the keys each needs | P1b |
| P3 | Catalogue 01 in full (352 tests) | P2 |
| P4 | Catalogue 02 in full (281 tests) | P2 |
| P5 | Catalogue 03 in full (312 tests) | P2 |
| P6 | Catalogue 04 in full (232 tests) | P2 |
| P7 | Key lint (9.4); strict traceability: every name is a passing test or `@Ignore` with its row's `IGNORE` reason | P3–P6 |
| P8 | Changed-area runs (user decision 2026-10-13, later phase): `scripts/uitest.sh -PuiAffected=<base>` runs only the tests a branch can affect. `git diff <base>` lists the changed files, and each file selects test classes two ways. (1) Catalogue refs: the rows whose code-ref cells cite the file give the test names, and through them their classes. (2) Coverage map: each full run records, from Kover (7.8), which test class executed which file; this also covers files no row cites. Changes to shared foundations (theme, `ChatModel`, `SimpleXAPI`, common helpers) add `Smoke`. Haskell core changes run `Smoke`, or everything on request. Unmapped files are reported and run `Smoke` | P7, 7.8 |

All shared infrastructure is in P1, so P3–P6 are independent and can proceed in parallel. Within each, one PR per catalogue sub-area keeps reviews small (keys for that sub-area + its screen objects + its tests).

P1a is the proof of concept: it runs real tests on the real app early, with the riskiest parts in place (boot, reset, restart, the scene runner, CLI peers, the driver), and its measurements adjust P1b before anything else is built. P1 (P1a and P1b together) fixes the API shape before keys are spread across about 190 files. Elsewhere in this plan "P1" means P1a and P1b together; measurements attributed to P1 are taken in P1a where its scope allows. The catalogue rows already contain the expected GUI state and CLI lines, so writing a test is mostly translating a row into DSL calls.

## 13. Decisions log and open questions

Settled (2026-10-10, with additions dated 2026-10-12):
- **Local servers:** `uitest-servers`, an executable component in `simplex-chat.cabal` behind a default-off `uitest` flag, using `tests/ChatClient.hs` (8.2).
- **Onboarding presets:** no core change; the run is network-isolated and tests switch to local servers after onboarding (8.3).
- **App seams (11.2):** approved — recorder factory, audio/video provider, Ctrl+V clipboard reader; and (2026-10-12) the display clock for stable screenshots (7.7).
- **Android (2026-10-12, changed 2026-10-13):** first out of scope, researched separately (`plans/ui-tests/2026-10-12-android-ui-test-automation.md`, Waydroid) with an Android-only catalogue, `05-android-only.md`. Since 2026-10-13 Android is in scope: its harness is built in the same worktree, and screens, helpers and test bodies are shared through `common/src/uiTestShared`, added to `desktopTest` and to `:android` `androidTest` (0.5.1, 0.5.4, 15.2–15.4). Android-only UI (catalogue 05) remains a separate effort.
- **Coverage (2026-10-12):** the run measures line and branch coverage of the UI, model and platform packages with Kover (7.8); zero-coverage files feed new catalogue rows or recorded exclusions; reported, not enforced.
- **Test tags only in test/debug builds (2026-10-12):** `uiTagsEnabled`, renamed `uiTestHooksEnabled` (2026-10-14), gates the tags and the `UiIdle` hooks (3.1, 0.5.2); release builds carry none.
- **Stable CLI:** downloaded by tag; default tag v7.0.3 (GitHub "Latest" on 2026-10-10).
- **Screenshots (2026-10-12, user request):** tests save committed PNG screenshots, rewritten only when the screen changes outside masked volatile regions or the set of volatile elements changes (7.7); purposes: regressions as image diffs, evidence that tests ran, documentation. Every passing catalogue test except `REALTIME` ones (and none with `-PuiShots=off`) saves an end-of-test shot plus named shots; PNGs are stored with Git LFS; displayed times come from a display-clock seam fixed in UI tests (11.2).
- **Implementation:** P1a approved (2026-10-13); it goes on a new branch off master in its own worktree, with one build per milestone. P1b is re-planned from P1a's measurements.
- **Decisions of 2026-10-13 (section 15 table, all approved):** the `UiIdle` hooks are approved (6); the OS folder-opener seam is deferred to P6; for Android, the restart is in-process (05 question 14), the debug-only network-security-config is approved (15), the `FIXTURE` proposal is approved (16), screenshots are deferred (17), and the toast hook is decided only if the `UiAutomation` check fails (13).
- **`UiIdle` hooks: approved 2026-10-13 and implemented** (0.5.2, 15.1). The fallback below is kept in case the hooks have to be removed.
- **Desktop forks (2026-10-14):** `forkEvery = 0` by default (one JVM per fork runs many classes; `-PuiForkEvery=1` restores one JVM per class), and the runner picks the number of parallel forks from free memory (15.4 item 24, 0.5.9).
- **Minimal app code (user rule, 2026-10-14):** keys go only on elements a test acts on or must read; no observation-only helpers (15.4 item 31). For now this supersedes typed semantics (4), "keys everywhere" (Summary item 1, 5) and the key lint (9.4). Revisit only with a new user decision.

Open:
- **`UiIdle` fallback (kept for reference; the hooks are settled above).** The busy tracker adds a few lines to `sendCmd`, `startReceiver` and the `with*Api` launchers (and an `idle` parameter on `withLongRunningApi`); like the keys it is app-side code. **Approved 2026-10-13** (section 15). The fallback is kept documented in case the hooks have to be removed later; without them the harness works at a cost: waits use fixed deadlines (tests with long commands pass explicit timeouts), `drain()` becomes "`ctrlInitInProgress` false, then 1 s with no new chat event", the screenshot quiet wait becomes "1 s with no new chat event" and settled becomes "two equal captures with no chat event between them" (7.7), the end-of-test dialog poll becomes "a `DialogState` is pending, or 1 s with no new chat event" (6), and the `changingActiveUserMutex` check first waits up to 30 s for the lock (a user switch may be behind a slow core command), failure reports lose the in-flight list, and uncaught job errors are attributed by phase only, not to the test that launched the job (6).
- **OS folder-opener seam — deferred.** Opening a folder in the OS (`desktopOpenDir`, D/platform/Files.desktop.kt:37-48, reached from the database-folder buttons via `desktopOpenDatabaseDir`, :33, DatabaseView.kt:243, DatabaseErrorView.kt:330) can't be observed headless (it is also called from the app-update download, which is out of reach anyway). A fifth seam (an `internal var` folder opener) would make those rows testable. **Deferred to P6 (2026-10-13)**; until then those rows stay `IGNORE`.

## 14. Risks

- **Scale:** about 190 files of keys and 1,177 tests. Mitigated by keying the helpers first (K0, about 70% of sites), one PR per catalogue sub-area, and the traceability check showing what is left.
- **Suite duration:** hours when run serially. Mitigated by parallel JVMs (`-PuiForks`, bounded by RAM, 10.2), the `Smoke` category before PRs, and `--tests` for single classes.
- **Catalogue drift:** line numbers in the catalogue rot as the code changes; the test names and labels are what matter, and the traceability check only matches names.
- **Merge conflicts** with ongoing UI PRs while keys spread. Mitigated by sub-area-sized PRs merged quickly; keys are additive modifier edits.
- **Leaks per reset:** each reset leaves a Haskell controller with its agent threads, and old receiver threads can block up to 300 s; bounded by the number of tests one JVM runs. With `forkEvery = 0` (as built) that is a share of the whole run, so RSS (about 4.6 MB per reset, 15.1) and one receiver thread per reset accumulate per fork; `-PuiForkEvery=1` bounds them by one class.
- **Threading differences:** the default test dispatcher is `UnconfinedTestDispatcher` (ComposeUiTest.skiko.kt:116), so composition coroutines continue on whatever thread resumes them (often IO threads) rather than the EDT, which allows races the real app doesn't have; it also runs `snapshotFlow` collectors inline inside the measure pass, where they see a half-updated list state (measured on a copy of the console's two collectors, TerminalView.kt: the newest line stayed visible 1 time in 30, auto-scroll having turned itself off; ChatView has 7 similar collectors, ChatView.kt:2482-2955). P1 therefore uses `runInternalSkikoComposeUiTest(coroutineDispatcher = StandardTestDispatcher())` (:86-103; needs the `InternalTestApi` and `InternalComposeUiApi` opt-ins, both error-level, set module-wide in 7.1), which runs them during `advanceScheduler` on the EDT (measured: 30 of 30; continuations still queued when the scene closes run on the body thread inside `runTest`, ComposeUiTest.skiko.kt:201, and ones dispatched after that never run), unless it finds a reason not to, in which case the console rows (catalogue 04 `console_*`) read `chatModel.terminalItems` and scroll to the newest line; a failure that looks like such a race is checked against the real app before being reported as a bug.
- **Render cost:** with a manual clock every 16 ms of virtual time renders a full 1600 × 900 software frame on the EDT (ComposeUiTest.skiko.kt:206-221). If a frame costs more than 16 ms, virtual time lags real time (the advance cap, 6), so composition-scoped delays fire late. Measured in P1a: 2.6 ms per chat-list frame (15.1). As built, the poll loop advances a fixed 16 ms per iteration plus a 16 ms sleep, so virtual time runs at most about half of real time anyway (0.5.3).
- **One GUI per JVM.** Every other user in a test is a CLI peer; GUI ↔ GUI scenarios are not covered by this plan.
- **Early class loading.** If a test or JUnit extension initialises an app class before the base class sets `user.home`, the paths would point to the real home; `bootJvm()` checks the paths and fails fast.
- **Headless rendering** (Skiko software, no AWT window): layout code reads `simplexWindowState.windowState`, which the harness sets to the scene size; code reading real AWT windows (`LocalWindowWidth`, D/views/chat/item/CIVideoView.desktop.kt:37-44) falls back to it.

## 15. Review status and open items (shipped 2026-10-13)

**Review.** The plan and catalogue went through 49 rounds of adversarial review. Each round had five concurrent reviewers:
- A: consistency
- B: plan claims vs code
- C: catalogue rows vs code
- D: feasibility, measured on toy projects built on the real app jar and core
- E: Android catalogue and carry-over map

Every finding was applied. The review then stopped on a time box rather than at "two clean rounds", because the two halves behave differently:
- **The harness design has converged.** B was clean in rounds 38–41, 46 and 48, D in rounds 37, 39, 40 and 48, and E in rounds 45, 46 and 49. The last design findings were details: the image-cache reset (7.3), the key for the security-code status line (4) and the dialog drain at the end of a test (6). All are fixed.
- **Catalogue rows have not converged.** Each fresh sample of 30–40 rows still finds 3–6 row-level issues. The recurring classes are:
  - a confirmation alert missing from the steps (e.g. "Use current profile", "Connection request sent!", "Update network settings?");
  - an assertion on text that is only an icon's content description;
  - a row that lists labels without a pass condition;
  - a multi-test row that gives an outcome for only some of its tests.

  Expect issues at a similar rate in rows not re-checked since round 18. These are cheap to fix where they surface. Rule for P1 onwards: when a newly written test fails, check the row against the code before the app, and fix the row in the same PR.

**Open items.**

1. Decisions. **All recommendations below were approved by the user on 2026-10-13**; the "Recommendation" column is now the decision.

   | Decision | Recommendation | Why | Cost if declined |
   |---|---|---|---|
   | Go-ahead | Start P1a only (12) | 5 real tests on the real app validate boot, reset, restart, the scene runner and the CLI peers, and give the timings that every budget in this plan depends on; P1b is re-planned from those numbers | — |
   | `UiIdle` hooks (6, 13) | Approve | A few lines in `sendCmd`, `startReceiver` and the `with*Api` launchers, counters only, with no behaviour change. Waits end as soon as the app is idle instead of at fixed deadlines, and failure reports name the job in flight | Fixed timeouts: slower tests, more flakiness, and errors attributed by phase only |
   | OS folder-opener seam (13) | Defer to P6 | It only unblocks the few database-folder rows in 04; decide when P6 reaches them | Those rows stay `IGNORE` |
   | Android question 14, restart | Defer until Android work starts; then prefer the in-process restart (stop the core, run the startup steps again, relaunch `MainActivity`) | It mirrors desktop `restartCore` and keeps each test in one `am instrument` run | — |
   | Android question 15, debug-only network-security-config | Approve when Android starts | A debug source-set XML that allows cleartext only to `127.0.0.1`; release builds are unchanged | 3 link-preview tests fail, and 1 passes vacuously |
   | Android question 16, `FIXTURE` mechanism | Approve the proposal when Android starts | It reuses the desktop SQL helper host-side; no app change | 38 carried-over tests stay unrunnable |
   | Android question 17, screenshots | Defer | Android screenshots are a nice-to-have; failure PNGs already work | No committed Android shots |
   | Android question 13, toast hook | Decide only if the `UiAutomation` check fails on the device | The hook is needed only if system toasts don't reach the listener | `TOAST` rows can't assert |

2. Measurements. Measured in P1a on the real app: the render cost per 16 ms frame (2.6 ms, 15.1). Not measurable until P1b builds them (so far measured only on toy projects with the real jar):
   - the settle-step time and per-shot budget (6, 7.7);
   - the timing of the end-of-test dialog drain (6);
   - the Kover overhead (7.8).
3. App bugs found while cataloguing, which are outside this plan and are reported when their tests are written (11.3):
   - The "Error importing chat database" alert is never shown, because `apiImportArchive` throws `Exception` and `importArchive` catches only `Error`.
   - The migration upload goes to preset public XFTP servers, so its rows are `IGNORE` in the network-isolated run.
4. Known nit, left as is: `previewMutex` (LinkPreviews.kt:46) is a `Mutex`, not one of the mutable types the 9.4 source check lists. It is harmless, because its fetches have bounded timeouts.
5. Review artefacts: the per-round findings are in the session scratchpad (`r<N>{a..e}/findings.md`), and the resume state is in `plans/ui-tests/catalogue/verify/REVIEW-STATE.md`. Toy harness sources (real-jar experiments) are in `r46d`–`r48d`.

### 15.1 P1a results so far (2026-10-13, first session)

These were measured on the real app, built from master `eda94a505` on branch `nd/desktop-ui-tests` in `~/wt-uitest`, with no commits yet. Each run used a headless scene of 1600 × 900 with a `StandardTestDispatcher`, inside the namespace with the dedicated Gradle home, offline. The harness so far is `HarnessBootTest` and `HarnessMeasureTest` (P1a progress: `plans/ui-tests/catalogue/verify/P1a-progress.md`).

- **Day-1 risk check passed.** The outside step filled `.gradle/uitest-home` (1.1 GB), and the offline `--no-daemon` run inside `unshare -rn --pid` compiled and ran the tests. One test costs 66–80 s of wall time, mostly Gradle configuration.
- **Boot works.** The JVM loaded `libapp-lib.so` and ran `initHS`, `initChatController`, `apiCreateActiveUser` and `startChat`, and `AppScreen()` showed the chat list ("Chats", "Private notes"). Boot, the first reset and the first frame took 10.7–11.5 s.
- **Reset time is the cost driver.** A reset took 7.2 s (median of 20; the slowest was 8.5 s), and 6–8 s of that is `initChatController` creating and migrating a fresh database. The other steps took 5–14 ms to stop and delete, 40–120 ms to create the user and 40–340 ms to start the chat. Over 1,177 tests that is about 2.3 h of serial reset time. **Fixed in P1a (same day):** a template database is migrated once per run and copied in before `initChatController` (7.2, `DbTemplate`). The JVM that creates it publishes it with an atomic rename, and parallel forks reuse it. A reset now takes **0.94 s** (median of 20; the slowest was 1.08 s), and the first reset of a later JVM 3.8 s instead of 11.5 s. The template is per run, so it always matches the current core's migrations.
- **Leaks per reset.** RSS grew from 412 to 504 MB over 20 resets (about 4.6 MB each), and live threads went from 15 to 35 (one per reset, the old receiver as 14 predicts), while the Java heap stayed at 21 MB. That is within the class-sized JVMs of 10.2.
- **Rendering is cheap.** One 16 ms frame of the chat list took 2.6 ms on average, and `captureToImage` of the full scene took 10–23 ms. The settle-step and per-shot budgets of 6 and 7.7 (about 1.4 s per step, 28 s per shot) are therefore conservative, and P1b can lower them after measuring a busy chat.
- **Confirmed as predicted:** the theme detector's thread throws a `NullPointerException` when the shutdown hook destroys `gsettings monitor` (8.2). No harness code suppresses it: `UiUncaught` and the shutting-down flag are P1b (0.5.11). Boot without the pre-set prefs of 11.1 opens the connect-onboarding view (`addressCreationCardShown`). Without `TestServers.configure` (8.2), What's New opens with "View updated conditions". With `uitest-servers` in place (below) the stopgap is gone: `TestServers.configure` disables the operators and refreshes `chatModel.conditions`.
- **Setup found:** a new worktree needs `git submodule update --init --recursive` (nanohttpd). `~/build/stage-libs.sh` refuses on current master, because master's `build-lib-linux.sh` no longer matches the reviewed hash. The libs were copied from the build slot instead (`cp -al`, with `libapp-lib.so` as a real copy, since `cmakeBuildAndCopy` writes it in place).

- **First catalogue tests pass** (session 2): `oneTimeLinkCliConnectsViaShortLink`, `sendText_enter_peerReceives` and `groupInvitation_tapRowShowsJoinAlert` pass together in 178 s. They use the first keys in app code: `Ui.kt`, alert confirm and dismiss keyed by manager, chat-list rows on both platforms, the new-chat buttons, `SectionItemView`'s `uiKey` parameter, `LinkTextView` and the message field. Four driver rules were found and are now implemented:
  1. A click waits until the node's bounds are unchanged between two polls. A click on a sheet still sliding in landed behind it.
  2. `typeInto` waits until the field's `EditableText` holds the text before the next action. Text input settles a few frames late, and an Enter before that sends nothing.
  3. Received CLI messages carry an `HH:MM ` prefix, which `expectsMessage` strips (8.1).
  4. A key on the text field itself carries `EditableText` on the same node, while `LinkTextView`'s key is on its clickable parent, so field reads match the node or its descendants.

- **All five P1a tests pass individually** (session 2): plus `addressCliConnectsAndRequestAppears` and `appearance_settingsPersistAcrossRestart`. The second uses `restartCore` as 7.3 specifies, and its theme, font scale and zoom are checked both in `settings.properties` and in the UI after the restart. More keys were added: the user picker and its address and settings entries, `SettingsActionItem`'s and `ExposedDropDownSetting`'s `uiKey` (with options), the address create and share buttons, the appearance sliders, and the color-mode dropdown. Findings:
  1. **`TestRoot`'s in-memory clipboard is required, not optional.** The address is read from "Share address", which goes to `ClipboardManager.shareText`; with no AWT clipboard in a headless JVM, the stub is the only way to read it.
  2. **Some keyed clickables don't merge.** `ExposedDropdownMenuBox`'s clickable node does not merge its children, so the selected value is a child `Text` even in the merged tree. `textOf(key)` reads the node's own text, then its first descendant's, after waiting for the node.
  3. **Sliders work through `SetProgress`.** The semantics action also runs `onValueChangeFinished`, so the value is stored.
  4. **The restart logs harmless errors.** It logs `Error updating users` / `Error updating remote hosts: Controller is not initialized` from composables that run before init completes. The test passes; P1b decides whether `restartCore` should compose only after init has started.
- **Exit check, first attempt:** 12 of 12 consecutive runs passed (all five tests, 149–210 s per run). It was stopped there to continue work, and will be rerun on the final P1a code.
- **`UiIdle` hooks are in** (approved, 6):
  - `sendCmd`: inside `withContext(Dispatchers.IO)`, with the status polls excluded by command type (since 2026-10-14 a thin `UiIdle.track` wrapper around that block, 0.5.2);
  - receiver events: also counted, with the time of the last one (counters removed 2026-10-14; only the time of the last event is kept);
  - `withApi`/`withBGApi`/`withLongRunningApi`: `idle = false` at `PopChatCollector`, `ProcessedErrors` and `createUpdateJob`.

  The harness uses them in `drain()`, the deadline extension and timeout messages. All 8 tests pass with them.
- **Peer start is asynchronous, as 8.1 intends, with a 60 s deadline.** A new peer's first start (the `-O0` current CLI) took 8.1–9.6 s at a load average of 9–17, and more than 30 s right after a 13-minute Kotlin compile (load average about 20), which failed a test with the synchronous 30 s start. `cli()` now returns at once, and the peer's first command waits for `Current user:` and the prompt.


- **The full reset of 7.2–7.3 is in:**
  - `PrefStores`: raw `Properties`, the files and `themes.yaml` restored; the `themeOverridesStore` cache nulled; every `SharedPreference._state` refreshed by reflection; theme and locale re-applied; the four model fields copied from prefs once refreshed.
  - `ModelSnapshot`: a reflection walk of `ChatModel`, its contexts, `AppLock`, `ConnectProgressManager`, `ChannelRelaysModel`, `BadgeModel` and `VideoPlayerHolder`, excluding `ChatController` and `AppPreferences`.
  - `clearUiState`: image and wallpaper caches, `cachedRemoteFileRequests`, the file-level list states, window state and auth state.
  - `clearTransientUi`: modals, passcode views, pending dialogs, both alert managers and toasts.

  The template database's key is installed after `PrefStores.restore`, which would otherwise wipe it. `HarnessResetTest` (two tests in one JVM) shows that a preference, an open sheet and a contact from the first test are gone in the second. All 10 tests pass in one run; the median reset is 1.67 s.

- **Two run-time cuts (user request, 2026-10-13):**
  1. **The runner skips the outside Gradle step** when a fingerprint is unchanged. The fingerprint covers the build files (`*.gradle.kts`, `gradle.properties`, version catalogs), the native C sources, and the staged native libs other than `libapp-lib.so`, which the step itself writes. `env.properties` is now written by the script on every run, and `uitestPrepare` only resolves `desktopTestRuntimeClasspath`. The step took 40–150 s per run; the inside run still compiles changed Kotlin, offline.
  2. **A peer database template.** While the outside step runs, the runner migrates one user-less CLI database per run: `--maintenance` exits with "no active user in maintenance mode" after running all 174 migrations. Each new peer's directory gets a copy, and the peer creates its user on first start. Measured, interleaved at a load average of 28–31: a first start with a fresh database took 11.4–13.6 s; from the template, 0.36–0.42 s. This replaces 8.1's "4–15 s per new peer".

- **P1a exit check passed (2026-10-13):** 20 of 20 consecutive runs of the five tests on the final P1a code (all passed, 98–329 s per run, mean 142 s; the slow early runs were at a load average of about 30 from other sessions, the later ones 98–127 s). Earlier loops on intermediate code: 12/12 and 7/7. **P1a is complete.**

Next: re-plan P1b from these results.

### 15.2 First test batch after P1a (2026-10-13, user request: "the most important parts of the app")

**Desktop: 12 more catalogue tests pass**, 17 in total including P1a:

| Area | Tests |
|---|---|
| Messaging | `unreadCounterIncrementsAndClears`, `reply_menu_sendQuoted_peerSeesQuote`, `delete_ownForEveryone_peerSeesMarkedDeleted`, `reaction_add_peerSeesAdded`, `reaction_received_chipCount` |
| Contacts | `contactRequestMenuAccept`, `contactRequestMenuReject`, `contactInfoSetLocalAlias` |
| Profile | `editProfileDisplayName` |
| Groups | `createGroup_opensChatAndInviteSheet`, `groupSend_allMembersReceive`, `leaveGroup_fromChatListMenu` |

**Two are parked with `@Ignore` and a reason** (both unparked in batch 2, P1a progress note; no test is `@Ignore`d now except the `HarnessMeasureTest` benchmark):
- `edit_menu_updatesPeer`: a test can't replace the pre-filled text of the controlled message field in edit mode. Neither `performTextReplacement` nor select-all plus `performTextInput` reaches the compose state.
- `deleteContactAndNotify`: "Delete contact" is an unplaced lazy-list item on the info screen, with zero bounds, and a bounded `ScrollBy` did not bring it into view.

**New keys:**
- chat items, their context-menu actions (`ItemAction` gained a `uiKey` parameter, on 5 overloads), reaction buttons and chips;
- the send button;
- chat-list row menus (accept, accept incognito, reject, leave);
- `DefaultAppBar`'s title (a `uiKey` parameter); the info screen's alias field and delete button; the delete dialogs' options (`Ui.Alert.Option`);
- the user picker's current user (desktop);
- `ProfileNameField` (a `uiKey` parameter); the group name and create button; the profile save button.

**Driver rules found and implemented:**
1. **Message commands have no echo.** Their CLI "echo" is the sent item `HH:MM @alice …`, so `sends` accepts either form (8.1).
2. **The verified Enter.** After a context-menu action, the first Enter is dropped (cause still open), so `send` waits for the send button to be enabled, presses Enter, and presses once more if the field hasn't cleared within 1.5 s (now 5 s, and only while the field still holds text and Send is enabled: 15.4 item 10b). This is safe, because `inProgress` blocks a second send (`ComposeView.kt:631`).
3. **`escape()`** invokes the window back stack (`simplexWindowState.backstack`, filled by the desktop `BackHandler`), for example to close the address screen before using the chat list behind it.
4. **Scroll into view, bounded.** `performScrollTo` looped until the heap ran out (`OutOfMemoryError` with a 2 GB heap) on a node in the end pane. The driver now scrolls the nearest `ScrollBy` ancestor by the overflow (or a page for an unplaced lazy item), at most 10 times.
5. **Connections get 30 s.** Under load, a connection took more than 10 s, so the waits for "contact is connected" are 30 s (now 60 s, `CONNECT_TIMEOUT_MS`, 0.5.7).
6. **A per-test hard timeout** of 4 min (thread dump, then halt), plus heap dumps on OOM. Before them, one hang cost 35 minutes.
7. **Group sends wait for members to connect.** A group message sent before a member is directly connected arrives forwarded by the host, with a ` [>>]` suffix. The test waits for `#team: member alice is connected` on each member before sending (catalogue 02:40 note).

**Full desktop run at the end of the day:** 24 tests (17 catalogue + 5 harness + 2 parked). After the group-send fix, everything passes except the 2 parked tests; the Groups class was rerun separately with 3/3 passing.

**Catalogue facts confirmed by the real CLI:**
- a reaction to the peer's own message prints `alice> >> hi` (catalogue 02:177 fixed);
- leaving prints `#team: alice left the group (signed)`, as 03:240 already says.

**Android (started on the same day):**
- `~/build/android.sh` gained `ANDROID_TEST=1`, which builds and publishes the `androidTest` APK next to the app APK.
- `android/build.gradle.kts` got the AndroidX runner, Compose `ui-test-junit4` 1.8.2, espresso-intents and uiautomator.
- `initUiTags(context)` switches tags on when the app is debuggable.
- `SmokeTest` boots, creates a user, launches `MainActivity` and finds the new-chat key.
- `scripts/uitest-android.sh` claims a Waydroid instance (default 2), installs both APKs, runs `pm clear` and grants permissions, runs `am instrument -r`, saves logcat and releases the claim.
- Research and the full step list: `plans/ui-tests/catalogue/verify/android-harness-research.md`.
- **The first Android instrumented test passes** (2026-10-13, Waydroid instance 2, claimed and released by the runner): `SmokeTest` reports `OK (1 test)`. The app's own startup in the instrumentation process took 15.4 s after `pm clear` (the estimate was ~45 s). The real `MainActivity` then showed the chat list, and the debug-only key `chatList.newChat` was found 18.2 s after start. The build takes 5 min 44 s for both APKs, from a slot whose Kotlin was already compiled. (The runner's `OK` check was later replaced: it parses `am instrument -r` into per-test `results.txt`, 0.5.9.)
- **Remaining for Android P1a** (about 2 days, from the research step list; status 2026-10-14 in brackets):
  1. the boot step plus reset and template (`AndroidUiTestApp`, `AndroidPrefStores`, `AndroidUiState`) [done as `UiTestApp`, `PrefStores` and the shared `CommonUiState`];
  2. the shared `uiTestShared` source set (screens and test bodies over a `UiDriver`/`Peer` interface), with the desktop re-run [done, without the interfaces: each platform has a `UiScope` with the same methods, and peers share `PeerLines`];
  3. a host-side peer bridge reached through `adb reverse`, with the SMP server outside the network namespace [done];
  4. porting the 5 P1a tests [done];
  5. the Android-only Back and notification tests [Back done, `BackKeyTest` 14; notification tests open];
  6. a 10-run exit check [open].
- Decisions: a shared `uiTestShared` source set for screens and test bodies, and the `~/build` test mode.

### 15.3 Second batch (2026-10-14)

**Desktop.**
- Shared tests now live in `common/src/uiTestShared`: screens, helpers, server config, `CommonReset` and the test bodies.
- New this batch: `ChatListMenuTest` (mark read/unread, favorite, mute, clear chat from the row menu) and `OneTimeLinkTest.oneTimeLinkSwitchToIncognito`. All 5 pass.
- The earlier full run passed 24/24 (19 catalogue + 5 harness). With the new tests that makes 24 catalogue tests in all; the whole suite has not been run again since.
- New keys:
  - row-menu `markRead`, `markUnread`, `favorite`/`unfavorite`, `ntfs.<next mode>` and `clear`;
  - the one-time link's "Share profile" row (`link.shareProfile`) and its picker options (`link.profile.incognito`, `link.profile(<userId>)`).
- New driver calls:
  - `descriptionsOf(key)` returns content descriptions; for example, the row star is "Favorite" and the muted icon is "Notifications".
  - `click` taps a point no nested button covers (15.4 item 8). The incognito option's centre is on its ⓘ button.
  - `CliPeer.eventuallyMatching(regex)`.
- Peer timeouts now show the peer's output at the moment of timeout. Before, they showed the output at the start of the wait, which wrongly made a failed connect look like "the peer saw nothing".

**Android.**
- Host bridge, shared tests over `adb reverse`, Espresso-Intents for share and copy, and an app APK install that is skipped when unchanged.
- Fixes this batch:
  1. `poll` advances the Compose clock (48 ms per step), as desktop does. Effect `delay()`s run on the test clock, so mark-read (600 ms) and the alias save (500 ms) never finished.
  2. The message field retries Espresso `replaceText` when the field is recreated while the chat opens.
  3. Accepting a contact request waits the 30 s connect timeout.
  4. A timeout takes a screenshot, which the runner pulls into `<run>/failures/`.
  5. The runner stops the bridge JVM by its PID (`$!` of `setsid java …`). Gradle's forked daemon is in its own session, so stopping the process group left the bridge and servers running.
  6. The runner stops at once if the bridge exits.
- **Last run** (`run-20261014-090056`): 14 of 14 executed tests passed, including 3 of the 6 previous failures (unread badge, edit, group send).
  - Claude Code stopped the run while the host was critically low on memory: 0.7 GB free, with about 7 GB held by another session's JVMs.
  - Not reached: `ContactsTest` (5, including the remaining previous failures: alias, delete contact, accept request), 3 of the 4 `ChatListMenuTest` tests and the 2 one-time-link tests.

**Status 2026-10-14, about 20:05 UTC (code before the PR; counts from the code, 0.5.1):**
- Desktop: 142 `@Test` methods (112 shared + 25 desktop-only + 5 harness; `HarnessMeasureTest` is `@Ignore`d, so 141 run).
- Android: 127 (112 shared + 14 `BackKeyTest` + `SmokeTest`). The last Android run (`run-20261014-164429`, 53/53) predates the exhaustive batches, so 74 shared tests had not run on Android before the final runs.
- **Latest run results (2026-10-14, final code):** desktop `b22-desktop.log`: 142 completed, 141 passed, 0 failed, 1 skipped (`HarnessMeasureTest`, manual), 6 min 37 s with automatic forks; Android full run `run-20261014-214038` on Waydroid instance 2: 127 tests, 126 passed, 0 failed, 1 skipped (`connectBannerCardOpensPasteLink`, no camera), 18.5 min. Open flakes seen in earlier runs of the same day: 15.4.1, 15.4.1a, item 41.

**Earlier status, 2026-10-14 about 17:00:** desktop 43/43, twice in a row (`b13-desktop-1/2.log`); Android 53/53 in one run (`run-20261014-164429`: 38 shared + 14 Back-key + smoke, 16.5 min of instrumentation). The exhaustive pass then added 99 catalogue tests (74 shared, 25 desktop-only) in two batches (P1a progress, batches 5 and 6).

**Earlier status, end of 2026-10-14 afternoon:**
- **Desktop 29/29** (24 catalogue + 5 harness): `~/uitest-review-tmp/b6-desktop-full.log`, 7 min 23 s including the build.
- **Android 25/25** (24 catalogue + smoke): `run-20261014-134126`, 39 s setup plus 5.3 min of instrumentation, with no crashes and no resumes.

**Open.**
- ~~Rerun those 10 Android tests when memory allows~~ (done: all pass).
- ~~Delete-contact failure~~ (fixed, 15.4 item 6).
- ~~Android `PrefStores` reset~~ (done: `android/.../harness/PrefStores.kt`, used by `resetForTest`); ~~Android Back tests~~ (done: `BackKeyTest`, 14).
- Android notification tests; an Android 10-run exit check.
- Android template decryption: `bootOnce` now calls `apiStopChat` before `apiStorageEncryption` (15.4 item 25); verified on Waydroid in the final run (reset median 585 ms, n = 126).
- `wd.sh net isolate` in the Android runner (0.5.9); `xftp1` served by `uitest-servers` (0.5.6); peer `--mute` (8.1).
- The test-APK build spends about 80 s on the `~/build` snapshot and slot plus a cold Gradle daemon, before about 60 s of real work. A warm daemon during runs would save most of that, but costs 1.5–2.5 GB of RAM; not done.
- Nothing is committed.

### 15.4 Edge-case failures found while running (2026-10-13/14)

Each entry gives the symptom, the cause (how it was confirmed), and the fix or mitigation. These are traps any later test or harness change can hit again.

**Android harness (instrumented, Waydroid)**
1. **Effect `delay()` never finishes.**
   - Symptom: the unread badge stays after opening the chat; a local alias never saves.
   - Cause: under `createEmptyComposeRule`, `LaunchedEffect` coroutines run on the test clock. Mark-read waits 600 ms (`ChatView.kt` `MarkItemsReadAfterDelay`) and the alias save debounces for 500 ms (`ChatInfoView.kt` `LocalAliasEditor`). Neither finishes unless the clock advances, and `waitForIdle()` doesn't advance it for a pending `delay`.
   - Fix: every `poll` iteration advances `mainClock` by 48 ms, as desktop already advances 16 ms. Both tests passed after the change.
2. **Message field emptied after typing.**
   - Symptom: "message field to hold `hello`" times out right after opening a chat (edit, group send).
   - Cause (inferred): the compose `EditText` is recreated while the chat finishes opening, so text set by Espresso `replaceText` lands in the old view.
   - Fix: `setMessageField` retries `replaceText` every 2 s for up to 10 s. Both tests passed after the change.
3. **45 s stall at the end of every test.**
   - Symptom: tests took about 45 s longer than their steps.
   - Cause: `ActivityScenario.close()` starts `InstrumentationActivityInvoker$EmptyActivity` from the test package to push the app to the background. The Espresso-Intents stub `not(isInternal())` treats the test package as external, so logcat shows "Stubbing intent … EmptyActivity", and `close()` waits out its 45 s timeout.
   - Fix: `Intents.release()` before `scenario.close()`. Close now takes 0.4 s.
4. **The Haskell core crashes or hangs in database init** (arm64 core under libndk translation).
   - Two failures, both in `chatMigrateInit` while re-creating a database between tests:
     - (a) SIGSEGV in a `ghc_worker` thread. One register held an SQL fragment ("ate TEXT"), so it happened during migrations.
     - (b) A hang at 0% CPU for 29 min. The thread dump (via `wd.sh rootsh kill -3`) shows `Instr` blocked in native `CoreKt.chatMigrateInit`, with no Kotlin lock involved.
   - Not reproducible on demand, and not seen on desktop. Mitigations:
     - (i) The template database is saved from the app's own startup database, so later resets copy it and run no migrations. The reset went from 31.6 s to about 4 s.
     - (ii) A 5 min per-test watchdog logs all stacks and kills the process.
     - (iii) The runner resumes with the tests not yet run (`-e notClass`, at most 4 passes) and records the test that was running as `crashed`.
5. **Clicking the middle of a row hits a nested button.**
   - Symptom: "Incognito" in the profile picker opened the incognito info screen instead.
   - Cause: the row's centre falls on its ⓘ button.
   - Fix: `click` finds the nested buttons and taps a point they don't cover (item 8). A first fix, `clickStart` at 5% of the width, was removed: tests must not encode positions.
6. **A bottom app bar covers scrolled content** (Android delete contact).
   - Cause, from the click log: the tap went to `(20,1381)–(700,1440)` in a 1440 px window, under the info screen's bottom app bar. One-hand UI is the Android default and puts bars at the bottom. `performScrollTo` stops as soon as the node is inside the scroll viewport, and the viewport extends behind the bar.
   - Fix: after scrolling, `click` centres the node in its nearest `ScrollBy` ancestor, so bars at either edge can't cover it. The test passes after the change.
7. **Nodes with empty bounds counted as stable.**
    - Symptom: after a restart on Android, `userPicker.settings` was clicked at `Rect(0,0,0,0)`, and on desktop the off-screen font slider was "stable" with zero size.
    - Fix: `waitStable` requires non-empty bounds. Every action that needs a visible node scrolls it into view first; `setSlider` now scrolls too. Without that, a semantics-only action can hide the fact that a user couldn't reach the control.
8. **Rows with nested buttons** (generalises item 5). There are no tap coordinates or fractions in tests.
    - `click` takes the target's unmerged semantics node and collects its clickable descendants (nodes with `OnClick`, for example the ⓘ button in the profile picker's "Incognito" row).
    - It taps the centre when no descendant covers it. Otherwise it taps the middle of the widest span of the centre line that no descendant covers (`uiTestShared/.../harness/TapPoint.kt`).
    - `clickStart` was removed. The incognito test passes on desktop with a plain `click`.
9. **Failure screenshots silently missing on Android.** The test package has no external files directory, so the screenshot errors were swallowed. Screenshots now go to the app's external files directory, and the runner pulls them after each pass, before `pm clear`. Screenshot errors are logged.
10. **A second activity in one test froze after its first frame** (Android `restart()`).
    - Symptom: after `restart()` (close, `restartCore`, launch again), the chat list draws but taps change nothing. The profile button's tap sets the picker state, yet the picker never appears.
    - Probes (throwaway, since deleted) isolated it:
      - dark theme, then picker: passes;
      - font scale, then picker: passes;
      - relaunch the activity only: passes;
      - `restartCore` only, activity kept: passes;
      - close the activity, then `restartCore` with no activity, then launch while the core is still starting: fails.
    - Fix: `restart()` restarts the core first, waits until chat is running with the user loaded, then closes and relaunches the activity. Restart semantics are unchanged: both the core and the UI are rebuilt, and settings are re-read. `AppearanceTest` passes on Android.
    - The root cause inside the app or the test framework, where the UI comes up during core init with no prior activity, is not investigated. It may matter for a real cold start with a slow core and is worth a look if users report a frozen first screen.

**Runner and host**

10a. **The template depended on test order.**
    - Symptom: a full-package run failed 26 of 27 tests with `apiCreateActiveUser(alice) failed`.
    - Cause: `SmokeTest` ran first, created "alice" on its own path, and `bootOnce` then saved the template from that database.
    - Fix:
      - `bootOnce` asks the core (`listUsers`) and builds a fresh database before saving when users exist;
      - `SmokeTest` now runs on the harness (`uiTest { app("alice") }`).
    - Rule: no test may bypass `UiTest`/`app()` to set up state.

10b. **The verified Enter double-pressed a slow send.**
    - Symptom: the Android edit test failed intermittently, waiting for `chat.send` after the edit was already sent: the screenshot shows the edited message and an empty field.
    - Cause: the retry for the desktop dropped-Enter quirk pressed again when the field hadn't cleared within 1.5 s. Under translation the send itself took longer, so by the second press the Send button had been replaced by the mic button.
    - Fix: wait up to 5 s; re-press only while the field still holds text and Send is enabled; tolerate the button vanishing. The outcome check stays that the field has cleared.

11. **The bridge outlived the run.**
   - Cause: Gradle `--no-daemon` forks a single-use daemon in its own session (`setsid`), so killing the wrapper's process group missed the bridge's `JavaExec` child.
   - Fix: the bridge is now started directly with `java -cp` and killed by its PID.
12. **App-code edits cost a 4–7 min desktop compile before any Android run.**
   - Cause: Gradle `uitestBridge` depended on `desktopTestClasses`.
   - Fix: `uitestBridgeClasspath` snapshots the harness classes once. The runner runs `java -cp` and calls Gradle only when a bridge source file is newer than the snapshot. The bridge is now up in 1 s.
   - Rule: never edit `commonMain` while a bridge compile is running; its tests then compile against stale main classes ("Unresolved reference").
13. **`adb root` on Waydroid restarts `adbd`**, which ends a running `am instrument` (exit 255). Use `wd.sh rootsh <cmd>` for root actions such as `kill -3 <pid>` for a thread dump. `run-as` is blocked on this image.
14. **Misleading peer timeout output.** "seen: …" was evaluated when the wait started, so a failed connect showed `[]`. Peer timeouts now append the output at timeout time.
15. **Host memory pressure.**
    - Other sessions on the qube took about 7 GB mid-run, and Claude Code stopped the background run with 0.7 GB free. The runner's 4.5 GB check at start doesn't protect a 30+ min run.
    - Keep runs short (class filters), and stop Waydroid when idle.
    - Never `pkill -f` a pattern that matches your own shell command line.
15a. **`pm clear` on a shared instance.** The runner runs `pm clear chat.simplex.app.debug` at the start of every pass. On instance 1, which other sessions use with the same debug package, that wipes their app state even when the claim is free. UI test runs use instance 2 only; the queueing helper waits for instance 2 and never falls back to 1.

16. **Waydroid ran alongside a bridge compile.** The bridge compile now runs before the Waydroid boot, and the boot overlaps only the 1 s bridge start. Waydroid comes up 30–34 s after the run starts.

**Desktop harness** (already in 15.2, listed here for completeness): the first Enter after a context-menu action is dropped (verified Enter); `performScrollTo` looped until OOM in the end pane (bounded `ScrollBy`); select-all must settle before `performTextInput` replaces text; connections need 30 s under load; group sends wait for each member's "member alice is connected".

17. **Popups swallow the next pointer or key event.** A desktop popup that is still closing after a menu-item click takes the next right-click or Enter as an outside click and only dismisses itself. This is the likely cause of the old "first Enter after a context-menu action is dropped" quirk.
    - Fix: `click`, `rightClick` and `pressEnter` wait until no popup is open, unless the target is itself inside one.
18. **The first tap after typing is lost** (desktop). Focus moves from the text field to the tapped control, and the focus change restarts the control's pointer handler mid-press. "Create profile" needed two taps.
    - Fix: if the target supports `RequestFocus` and is not inside a popup, the harness focuses it through semantics, settles 3 frames, then taps.
19. **Controls clicked while still disabled.** "Create profile" stays disabled until the typed name validates.
    - Fix: `click` waits until the target is enabled, with a clear failure message if it never is.
20. **`performScrollToNode` deadlocks on desktop.** It runs the scroll on the UI thread and waits for animation frames, but the desktop harness runs with a paused clock. One test JVM hit the 240 s hard timeout, and the halt lost that JVM's remaining tests.
    - Fix: desktop `scrollTo(key)` pages lazy lists with `ScrollBy`, advancing 40 frames per page, until the key appears. It is polled, so rows that load late, such as group members, are found too.
    - Android keeps `performScrollToNode`, because its clock auto-advances.
    - Rule: never call `performScrollTo*` on desktop.
21. **The chat list lags the model after a settings change, and the model can be reverted by queued group events.**
    - `rowMenu` waits for no API in flight and settles about 0.5 s of app clock before opening the menu.
    - Group helpers call `quiesce()` (no API in flight and no events for 1 s) after joins.
    - See 15.5 item 1 for the app-side race.
22. **Esc pressed before an async screen close finished.** Creating a profile is a chain of database calls (create, list users, reload chats) ending in `close()`. The test pressed Esc on seeing the new active user, which was too early.
    - Fix: `ProfilesScreen.add` waits for the new-profile screen to disappear, not just for the active user to change. The general lesson: wait for the visible end state of an operation, not an intermediate model change.
23. **Parallel desktop JVMs raced on the shared DB template.** The losing atomic directory rename throws a plain `FileSystemException` ("Directory not empty"), not `DirectoryNotEmptyException`.
    - Fix: if the template exists afterwards, the error is ignored.
24. **Speed (desktop).** `forkEvery = 0` (one JVM per fork runs many classes) plus parallel forks chosen from free memory (1 GB per fork, 2.5 GB reserved, at most 4). The full suite went from 7 min 23 s (serial, one JVM per class) to about 4.5 min for 43 tests.
25. **Android template encryption.** Decrypting the template with `apiStorageEncryption` while the startup chat controller held the agent database failed with "database is locked". Chat is now stopped first with `apiStopChat` before `apiStorageEncryption`. In the last run (16:46, before that change) decryption still failed with "database is locked", so resets opened an encrypted template (`reset.init` 3.5–5.2 s); verified in the final Android run (2026-10-14 21:40): no "database is locked", and `reset.init` dropped to about 50 ms.

26. **The reset missed `by lazy` model state.** `ChatModel.migrationState` is declared `by lazy`, so the reflective `ModelSnapshot` saw only the `Lazy` wrapper. After the "migrate from another device" test, the onboarding screens refused to advance in every later test of the JVM, and 12 onboarding tests failed.
    - Fix: `ModelSnapshot` unwraps `Lazy` fields (initialising them) and snapshots their contents. This applies to every `by lazy` model field on both platforms.
27. **App background errors were attributed to the wrong test.** A job started by the test's own actions (or by its UI teardown) fails after `runTest` returns, and coroutines-test reports it as "uncaught exceptions before the test started" in the NEXT test.
    - Fix: each test body ends with `drain()` inside the test, so the test's own background jobs finish while it is still current. The desktop `finally` also flushes the Swing queue and drains, because scene disposal is queued on the EDT.
    - The full exception list (`suppressed`) is now printed; Gradle's short format hid it.
28. **Known app issues are allowlisted by message, not ignored wholesale.** `KNOWN_APP_ISSUES` lists app errors traced to a documented finding (15.5 item 3); a match is logged as "UITEST known app issue, not failing the test". It passes a test only when its body completed; a test whose body never ran because of such an error is rerun (at most 3 attempts), and one whose body started but did not complete fails. `AssertionError`s never match. Any other uncaught exception still fails the test. Desktop only; details in 0.5.8.
29. **Desktop failure screenshots.** A desktop `poll` timeout writes `<run>/failures/<jvm>-NN-<what>.png` (`captureToImage` of the window root). The first one showed that the "chat list title" expectation was wrong (15.6).
30. **CLI typing under parallel load.** The CLI's echo of a 200-character `/c` link paused for more than the 200 ms "line unchanged" window when 4 JVMs ran together. The window is now 1.5 s (it only matters on a mismatch), and failures report what the input line held.
31. **App-code additions are kept minimal** (user rule, 2026-10-14). Keys go only on elements a test acts on, or whose text cannot be read otherwise. Observation-only helpers were removed: the `uiMarker` modifier, row icon marks, a key attached only while an error shows, and a QR key. The tests assert those states through the model (`contact.verified`, `chatInfo.incognito`, `draftChatId`, chat item content), visible texts, or existing content descriptions (`seesDescription`, added to both harnesses).

32. **Taps landed on a nested control's touch target.** The chat-list title tap hit the subscription indicator: Compose inflates a small clickable to a 48 dp touch target, so a point outside its drawn bounds still belongs to it. `tapPointInNode` now inflates nested clickables to 48 dp before choosing a free point.
33. **Desktop has no Back handler on some screens.** The New chat sheet and the pending-connection info ("You invited a contact", opened with `showClose = appPlatform.isAndroid`) register no `BackHandler` on desktop, so Esc does nothing there. The sheet has a "Back" arrow, so `goBack()` clicks `described("Back")` on desktop and presses system Back on Android. The pending-connection info has no close control on desktop; it stays in the center pane until another chat opens, so `closePendingConnectionInfo()` closes it on Android only. Because the panel stays open on desktop, the app turns it into the contact's chat when the peer connects (`replaceConnReqView`), and the open chat is always listed whatever the search (`ChatListView.kt` `filteredChats`); `searchFiltersByAlias` therefore asserts "listed for non-matching text exactly when it is the open chat", which holds on both platforms. This was the cause of the last 3 desktop failures in run b18 (`searchFiltersByAlias`, `searchMatchesPendingConnectionAlias`, `pendingConnectionMenuSetName`).
34. **`described(text)` needs the merged tree.** A content description on an `Icon` inside an `IconButton` is merged into the button; the unmerged tree has the description on a non-clickable node. Described keys match `hasContentDescription + hasClickAction` and use the merged tree for the tap point.
35. **Clicks during a user switch were lost.** `UserPickerScreen.switchTo` waits for `switchingUsersAndHosts` to clear and then for `quiesce()`.
36. **Peer line matching is shared.** `PeerLines` (uiTestShared) holds `sends/expects/eventually*`; desktop and Android `CliPeer` only implement `fill()` and `type()`. The Android bridge socket has a 60 s read timeout so a dead bridge fails the test instead of hanging it.
37. **Test hooks are off in release builds.** `UiIdle` and `Modifier.ui` do nothing unless `uiTestHooksEnabled` (set from `FLAG_DEBUGGABLE` on Android and by the desktop harness); `sendCmd` and the event loop wrap their bodies with `UiIdle.track`/`trackEvent`, which cost one volatile read when off.
38. **Android runner is portable.** `UITEST_SERIAL` selects any adb device and skips Waydroid; `ADB`, `JAVA_HOME`, `UITEST_APP_ID`, `WD_INSTANCE`, `WD_OWNER` are read from the environment; the runner refuses to start before `scripts/uitest.sh` has written `env.properties`.
39. **A new key parameter after a trailing lambda broke Android-only callers.** `uiKey` was added after `onChange`/`selectItem` in `SettingsPreferenceItem`, `SharedPreferenceToggle` and `SelectItemAction`; `androidMain` callers that pass the lambda as a trailing lambda (`Appearance.android.kt`, `PrivacySettings.android.kt`) stopped compiling, and the desktop suite does not compile `androidMain`. Rule: a new `uiKey` parameter goes before the last function-typed parameter, and every app-code change is compiled for Android (`android.sh`) as well as desktop before a run is reported.
40. **First full Android run of the shared tests (127 tests, 22 min, 121 passed).** Android-only causes of the 6 failures:
    - Back on the pending-connection info first closes the soft keyboard ("Set contact name" focuses the name field), so one Back left the screen open; `closePendingConnectionInfo()` taps the screen's own "Back" arrow (`described("Back")`) on Android.
    - Confirming a contact link plan opens the prepared chat full screen on Android, covering the chat list; `contactMenuForNotReadyContactOnlyDelete` calls `backToChatList()` before opening the row menu (no-op on desktop, where the list stays visible).
    - The "Paste link / Scan" banner opens the QR scanner; Waydroid has no camera, so CameraX fails and the app crashes when the scanner is disposed (15.5 item 4). `connectBannerCardOpensPasteLink` calls `assumeCamera()` (skipped on devices without a camera; no-op on desktop).
41. **`chatCloseStore` failures were ignored.** It returns an error text ("" on success); the resets deleted the database files whatever it returned. One desktop run (b21) had `apiNewGroup` fail with "SQLite3 returned ErrorIO … disk I/O error" right after a reset (first time in about 20 runs; the disk was not full). Both harnesses now fail the reset when `chatCloseStore` returns an error, so a recurrence names its cause. The I/O error itself is not explained yet (open item).

### 15.4.1 Open flake: a CLI member's group join not completed (desktop, 2 of 5 runs)

- `MemberInfoTest.memberInfo_changeRoleToAdmin` (run b18) and `memberInfo_removeMember` (run b15): bob sends `/j team` and never prints "#team: you joined the group" within 60 s (30 s in b15). No API call is in flight in the app.
- Bob's agent database after the failure: the group connection's send queue is `confirmed` and its receive queue is `new`, so bob's confirmation was sent and alice's app never replied (no HELLO/secure). Alice's database was reset by the next test, so her side was not inspected.
- Not reproduced in isolation yet. Next step: keep the app database of a failing test (copy before reset) and check whether alice's agent received the CONF and whether the chat layer called `allowConnection`.

### 15.4.1a Open flake: group row preview not updated (Android, 1 of 2 runs)

- `ChatListRowsTest.previewGroupSenderPrefix` (full Android run, 2026-10-14 21:07): after alice leaves the group chat, bob sends "#team hello all"; for 10 s the `team` row showed the unread badge "1" but kept the preview "mine" (failure screenshot). Passed in the rerun of the class.
- The badge proves the row recomposed with new chat stats; the preview text comes from `chat.chatItems.lastOrNull()` (`ChatPreviewView.kt:228`). Either the new item did not reach the chat's preview list while its stats did (app-side model update), or an earlier group event incremented the counter and the message was still in transit. Not resolved; next step is to capture `chatModel.chats` for the row on failure.

### 15.4.2 Test strength limits (second review, 2026-10-14)

Under the minimal-app-code rule (item 31) some states are asserted through the model, not the UI. These tests pass if the model is right even when the UI fails to render the state:
- row icons without a description or key: `incognitoIconOnRow`, `verifiedIconOnRow`, the link image in `previewLink`, the edit icon in `previewShowsDraft`, the badge colour in `unreadCounterGrayWhenMuted`, mark-unread badges (`ChatListMenuTest`, `ChatListMenusPendingTest`);
- `ChatListScreen.gone()` checks the model only (the list is rendered from the same model);
- `profilesAddProfile` checks `chatModel.users`;
- `createListDuplicateNameError` cannot see the error text because it is always composed (transparent when hidden, `TagListView.kt`).
Harness behaviours that can hide an app defect:
- `enterWhenSendable` presses Enter again if the field has not cleared after 5 s (needed for 15.5 item 2), so a dropped first Enter is not reported;
- Android `pressEnter` on the message field taps Send; desktop `escape()` invokes the window back stack directly instead of sending an Esc key event;
- `isEnabled` is always true for `SectionItemView` keys (a disabled item drops `clickable` and sets no Disabled semantics).
Known harness limits (accepted for P1a):
- the known-issue allowlist cannot tell a body-time "database closed" from a teardown one, because Compose reports uncaught coroutine exceptions only when the test ends;
- Android `clipboardText()` falls back to the device clipboard, which is not cleared between tests;
- `OnboardingSharedState.selectedOperatorIds` is not reset between tests (no current test depends on it);
- `:common:desktopUiTest` run without `scripts/uitest.sh` is skipped and reports success with zero tests;
- on the Android path the test SMP server listens on all host interfaces with the repo's fixture TLS key, and `pm clear` acts on whatever `UITEST_APP_ID` names; never point `UITEST_SERIAL` at a personal device with the release package.
Fixed from the second code review: `UiKey` is a `data class`, so keys passed to composables compare equal and do not break recomposition skipping (keys are built in release too); the Android bridge client drops its socket after an IO error or EOF so the next call reconnects instead of reading a stale reply, and the bridge keeps accepting after a client drops; `uitest-android.sh` exits non-zero when it gives up after 4 crashed passes, runs its cleanup (bridge, Waydroid claim) on Ctrl-C/TERM, and rebuilds the bridge when `PeerLines.kt` changes; `uitest.sh --inside` refuses to run unless it is PID 1 of the runner's PID namespace (its TERM trap kills `-1`).
Fixed in the same review: `createGroup_opensChatAndInviteSheet` now checks the open chat, `search_direct_filtersResults` checks the matching item nodes, onboarding `!isEnabled` checks wait for the node first, the Android `TagEditor.Emoji` key was removed (no Android test acts on it), and the desktop scroll margin is 48 dp, not 50 px.

### 15.5 App findings from the UI tests (2026-10-14; not fixed, reported only)

1. **The group mute setting can revert in the UI right after joining a group** (catalogue 01:86, `groupMenuMuteModesAllMentionsNone`).
   - What was seen: alice joins bob's group and changes the mode from the row menu (All → Mentions). The model shows Mentions once `apiSetSettings` returns, then about 0.5 s later shows All again, with no further user action. It happened in 2 of about 12 runs. Each time, the model's `chatSettings.enableNtfs` was captured in the failure message.
   - Likely cause: a group event (`UserAcceptedGroupSent` or `UserJoinedGroup`, `SimpleXAPI.kt:3150,3278`) carries a `GroupInfo` read before the settings write, waits in the receiver queue, and is applied after the command reply with `updateGroup(...)`, overwriting `chatSettings`.
   - Effect: the chat list shows the old mode. Notification decisions read the model, so a just-muted group can still notify until the app reloads its chats.
   - Not yet confirmed on the core side: the core's stored setting was correct (Mentions) in the probe runs without the revert.
   - The test now waits for event traffic to stop (`quiesce()`: no API in flight and no events for 1 s) before changing modes.
   - Fix idea, not done: event-driven `updateGroup` keeps the model's `chatSettings` when it is newer than the event, or the settings command response is applied through the same ordered queue.

2. **The first tap after typing in a text field is lost on desktop** (harness-visible; the real-mouse impact is unknown). Focus moves to the tapped control, and the focus change restarts its pointer handler mid-press. The harness requests focus on the target first (`focusBeforeTap`). Whether a real mouse hits the same problem is not verified; a manual check is needed before reporting it as an app bug.

3. **The onboarding passphrase screen starts chat in the background when disposed** (`SetupDatabasePassphrase.kt:84-95`, `DisposableEffect.onDispose` → `withBGApi { startChat(user) }` when `chatRunning != true`).
   - Two effects were seen:
     - after "Encrypt", this races with the encrypt flow's own `startChat(newKey)`, and one of them fails with "failed to list users … database closed" (seen in `onboardingPassphrasePersistsAcrossRestart`);
     - if the screen is disposed while no controller exists, `startChat` fails with "Controller is not initialized".
   - Both escape as uncaught coroutine exceptions. In the app they are probably logged ("E: failed starting chat"); a manual check is needed to see whether a user sees an alert.
   - Allowlisted in the harness as a known app issue (15.4 item 28).

4. **Leaving the QR scanner crashes the app when the camera cannot be initialised** (`QRCodeScanner.android.kt:57-60`). `onDispose` calls `cameraProviderFuture?.get()?.unbindAll()`; when CameraX initialisation failed (no camera; on real devices possibly a camera held by another app or a camera service error), `get()` throws `ExecutionException(InitializationException(CameraUnavailableException))` during disposal, which is uncaught. Seen on Waydroid (0 cameras) in `connectBannerCardOpensPasteLink`; the activity then never reaches DESTROYED. The listener at :89-124 calls `get()` the same way. Fix idea, not done: catch the failure around `get()` in both places.

### 15.6 Catalogue corrections applied from passing tests (exhaustive pass)

- 01:34 and 01:8: accepting conditions enables the operators selected in onboarding, not all preset ones.
- 01:34: on desktop with no conversations the app then shows the "Talk to someone" connect-onboarding view, and the chat-list title and search are hidden (`shouldShowOnboarding()`, `OnboardingCards.kt:52`), not "the chat list".
- 01:37: `TestServers.configure()` (not `useLocalServers()`).
- The other corrections drafted per batch are in `~/uitest-drafts/b*/*/NOTES.md`, sections "Catalogue corrections". They are applied to the catalogue files after each batch passes.

