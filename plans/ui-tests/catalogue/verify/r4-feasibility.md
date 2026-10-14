The plan is not CLEAN. I found 8 designs that would fail as written. Findings 1–3 are confirmed by experiments on this host; the rest are traced through the source of the pinned versions.

### 1. The outside build leaves a Gradle daemon running, and it blocks the Gradle run inside the namespace (every run)
- **Plan line 323:** "`./gradlew --stop` (an outside daemon can't be asked to release `~/.gradle` locks from inside), then `./gradlew :desktop:cmakeBuildAndCopy :common:desktopTestClasses :common:uitestPrepare`"
- **Problem:** `--stop` runs first. The build command after it starts a new daemon, and that daemon stays alive holding `caches/journal-1`. Gradle asks a lock holder to release by pinging it over loopback, which can't cross network namespaces.
- **Evidence:** I reproduced this with Gradle 8.12 and a scratch `GRADLE_USER_HOME`. I ran one daemon build outside, then `unshare -rn … gradle --offline --no-daemon` inside. It failed after 62 s with `Timeout waiting to lock journal cache (…/caches/journal-1). It is currently in use by another Gradle instance. Owner PID: 174285`. After `--stop`, the same inside command passed in 6.8 s.
- **Also:** `journal-1` and `modules-2` are shared across Gradle versions, but `--stop` only stops daemons of its own version (`~/.gradle/daemon/` has both 8.7 and 8.12).
- **Fix (replacement text):** "`./gradlew --no-daemon :desktop:cmakeBuildAndCopy :common:desktopTestClasses :common:uitestPrepare`, then `./gradlew --stop`. The script then refuses to enter the namespace while any `GradleDaemon` process (of any version, including an IDE's) uses the same Gradle user home, because a daemon outside holds cache locks the Gradle run inside can't make it release."

### 2. Pass 2 fails with "No tests found" for the plan's own `--tests` example
- **Plan line 331:** "Pass 2 runs only if the user's `uiTags` is empty or contains `Multicast`: it runs `-PuiTags=Multicast` with the user's exclusions and `--tests`". Plan line 333 gives the usage `scripts/uitest.sh --tests '*OneTimeLink*Test'`.
- **Problem:** That example leaves `uiTags` empty, so pass 2 runs Multicast ∩ `*OneTimeLink*` = 0 tests. Gradle's `failOnNoMatchingTests` (default true) then fails the build. Pass 1 fails the same way when `--tests` selects only Multicast tests.
- **Evidence:** Gradle 8.12 project with JUnit 4.13.2, `forkEvery = 1` and a category filter: `test --tests '*OneTimeLink*' -Pinc=a.Multicast` → `> No tests found for given includes: [*OneTimeLink*](--tests filter)`, `BUILD FAILED`.
- **Fix:** add to `desktopUiTest`: `filter { isFailOnNoMatchingTests = findProperty("uiAllowEmpty") == null }`. Replace the pass rule with: "Both passes add `-PuiAllowEmpty`; the script fails the run only if neither pass executed a test (it counts the XML results in `build/test-results/desktopUiTest`)."

### 3. Ctrl+C and SIGTERM don't stop a run; `timeout` returns while the namespace keeps running
- **Plan line 326–330:** the script re-executes as `sh -c '… exec "$0" --inside "$@"'`, so the script becomes PID 1 of the pid namespace.
- **Problem:** PID 1 of a pid namespace ignores signals it has no handler for. `unshare --fork` also ignores SIGINT and SIGTERM while it waits for its child.
- **Evidence (util-linux 2.38.1):**
  - Sending SIGINT to the process group killed the inner `sleep` (standing in for Gradle pass 1). The PID-1 `sh` carried on to the next command ("pass 2") and unshare exited 0.
  - `kill -TERM <unshare>`: unshare survived and the inner commands ran to completion.
  - `timeout 2 ./t2.sh` returned 124 while the inner commands kept running.
  - Only SIGKILL to unshare triggered `--kill-child`.
- **Effect:** the first Ctrl+C kills pass 1's Gradle, its test JVMs and peers, then pass 2 starts, and the exit status can be 0.
- **Fix (replacement text):** "In `--inside` mode the script first runs `trap 'kill -TERM -1 2>/dev/null; exit 130' INT TERM` (PID 1 receives only signals it handles), records each pass's exit code, and exits non-zero if either failed. The outer invocation doesn't `exec`: it runs `unshare … &`, sets `trap 'kill -KILL $!' INT TERM`, and returns `wait`'s status, so `--kill-child` tears the namespace down."

### 4. `UiIdle` never drains: every reset waits the full 30 s, and assertion deadlines stretch to about 120 s
- **Plan line 260:** "jobs started by `withBGApi`/`withLongRunningApi`/`withApi` … added on launch and removed on completion". Plan line 262: "`drain()` waits until nothing is in flight (at most 30 s) before a reset".
- **Evidence:** `PopChatCollector.init` launches `withLongRunningApi { subject.throttleLatest(2000).collect { … } }`, which never completes (C/model/ChatModel.kt:826-835).
  - One collector is created per `ChatsContext`: ChatModel.kt:161 (`val chatsContext = ChatsContext(null)`, created on the JVM's first access to `ChatModel`), MemberSupportChatView.kt:160, GroupReportsView.kt:80, ChatView.kt:3832/3915, ChatItemView.kt:1491/1542.
  - A grep of every `withBGApi`/`withLongRunningApi`/`withApi` site found no other job that never ends.
- **Effect:**
  - Every `resetForTest` spends 30 s in `drain()`, about 9.7 h over 1,161 tests, against the plan's "a few seconds" per reset.
  - For the first 120 s of each JVM, and after any support-chat or reports screen opens, every assertion deadline is extended, so a failing check takes about 2 min instead of 10 s.
- **Fix (replacement text):** "jobs started by `withBGApi`/`withLongRunningApi`/`withApi`, added on launch and removed on completion. `withLongRunningApi` gets a parameter `idle: Boolean = true`; `PopChatCollector.init` (ChatModel.kt:827) passes `idle = false`, because its collector never ends."

### 5. `restartCore()` blocks on the "Start chat?" alert before `AppScreen` is composed
- **Plan line 396:** "start as desktop startup does — `initChatControllerOnStart()` … — wait until initialisation has finished, and compose `AppScreen()` again with `setContent`". Catalogue 04 line 24 uses the same order.
- **Problem 1 (deadlock):** when `chatStopped` is set and a passphrase is stored, `initChatControllerOnStart` calls `initChatController(startChat = ::showStartChatAfterRestartAlert)` (Core.kt:60-61).
  - Initialisation then waits on `startChat().await()` (Core.kt:144), which only the alert's buttons complete (Core.kt:226-235).
  - The alert renders only inside `AppScreen`, which isn't composed yet, so `ctrlInitInProgress` stays true.
  - Test `dbMgmt_stoppedStatePersistsRestart` hits the 2-min watchdog every time.
- **Problem 2 (race):** `initChatControllerOnStart` only launches a coroutine (Core.kt:59) and `ctrlInitInProgress` is still false before it starts (Core.kt:72-73). A wait on that flag can return before initialisation begins.
- **Fix (replacement text):** "…clear `chatDbStatus` to null, call `initChatControllerOnStart()` (or take the lock-screen path), compose `AppScreen()` again with `setContent` at once, as desktop `showApp` composes while initialisation runs, then poll until `chatModel.chatDbStatus.value != null && !chatModel.ctrlInitInProgress.value`, or until an alert is shown (`AlertManager.shared.hasAlertsShown()`)."

### 6. Restarting a profile-less (onboarding) app ends on the splash screen
- **Plan line 396:** "raw `apiStopChat` (not `stopChatAsync` …) … clear in-memory state as `clearUiState` does". The `clearUiState` list (plan line 392) doesn't include `chatRunning`.
- **Problem:** raw `apiStopChat` leaves `chatModel.chatRunning == true`.
  - On restart with no user, `initChatController` sets `localUserCreated = null` and calls `startChatWithoutUser()` (Core.kt:151-163).
  - `startChatWithoutUser()` returns at once because of `if (chatModel.chatRunning.value == true) return` (SimpleXAPI.kt:704), so `localUserCreated` stays null.
  - App.kt:156 then shows `localUserCreated == null -> SplashView()` indefinitely. A real restart starts from `chatRunning = null` (ChatModel.kt:146).
- **Failing case:** `migrateTo_resumeAfterRestart` (Needs `… RESTART, ONBOARD`; migrate-to is reachable only from onboarding).
- **Fix (replacement text):** "raw `apiStopChat` … restore `chatStopped` to its previous value and set `chatModel.chatRunning.value = null` (the process-start value, ChatModel.kt:146); `chatCloseStore`; …"

### 7. The watchdog's "poisoned JVM" can't take effect when the EDT is stuck
- **Plan line 264:** "interrupts the test thread, waits 15 s, and if the thread hasn't finished marks the JVM poisoned: every later `app()` call in this JVM fails immediately".
- **Evidence:** in ui-test-desktop 1.8.2 (official sources jar), `runDesktopComposeUiTest` → `SkikoComposeUiTest.withScene` runs `scene = runOnUiThread(::createUi)` first and `runOnUiThread(scene::close)` in `finally`. `runOnUiThread` is `SwingUtilities.invokeAndWait` when called off the EDT (DesktopSynchronization.desktop.kt).
- **Effect:**
  - If the test body runs on the JUnit thread, JUnit never reaches the next test.
  - If the body runs on its own thread and the stuck thread is the EDT (the plan names "stuck … on the EDT"), the next test blocks in `createUi` before `app()` checks the flag.
  - Either way that fork hangs, and the whole `desktopUiTest` task hangs until its 12 h `timeout`.
- **Fix (replacement text):** "`uiTest` checks the poison flag on entry, before `runDesktopComposeUiTest`, and runs `runDesktopComposeUiTest { … }` on a dedicated daemon thread that the JUnit thread joins with the deadline. When the deadline passes it also probes the EDT (`invokeLater` plus a 5 s latch). If the body thread is still alive 15 s after the interrupt, or the probe fails, the JVM is poisoned and every later `uiTest` in it fails at entry without touching the EDT."

### 8. Minor compile and configuration errors
- **8a. Plan line 301:** `timeout.set(Duration.ofHours(12))` won't compile. Gradle 8.12's Kotlin DSL implicit imports are `org.gradle.kotlin.dsl.*`, `…plugins.dsl.*`, `Callable`, `TimeUnit`, `BigDecimal`, `BigInteger`, `File`, `Inject` plus Gradle API packages (javap of `ImplicitImports` in gradle-kotlin-dsl-8.12.jar). `common/build.gradle.kts` has no imports. **Fix:** `timeout.set(java.time.Duration.ofHours(12))`.
- **8b. Plan lines 688 and 692:** "`-PuiTags=Smoke …` runs one test per sub-area plus the traceability check". JUnit 4 `includeCategories(Smoke)` drops uncategorised tests, and `CatalogueTraceabilityTest` is described only as "a plain JUnit test". **Fix:** "`CatalogueTraceabilityTest` carries `@Category(Smoke::class)` (it is exempt from the Needs-to-category rule)."

### What checked out
- **ui-test 1.8.2 API** (official sources jars):
  - `runDesktopComposeUiTest(width, height, effectContext, block)` exists; `ExperimentalTestApi` is an error-level opt-in.
  - With `autoAdvance = false`, `waitForIdle` returns once layout is done and ignores idling resources.
  - `advanceTimeBy` runs on the EDT; input injection advances the clock itself.
  - `setContent` can be called again; `captureToImage()`, `performScrollToKey`, `performMouseInput`, `printToString`, `isDialog()` and `LocalClipboard` all exist.
  - Material `AlertDialog` renders as a same-canvas `Dialog` layer with `dialog()` semantics and Escape-to-dismiss. The AWT-window `DefaultDialog` has no callers.
  - `compose.desktop.uiTestJUnit4` maps to `org.jetbrains.compose.ui:ui-test-junit4`.
- **Dependencies and Gradle wiring:**
  - pty4j 0.13.13 exists and needs kotlin-stdlib 2.1.21, which is compatible.
  - Skiko 0.9.4.2 unpacks its native library with a temp file and atomic move, so parallel JVMs are safe.
  - `desktopTest` already runs on JUnit 4 (kotlin-test-junit).
  - `libapp-lib` has `$ORIGIN` rpath.
  - `startReceiver` is guarded, so `startChat` after `startChatWithoutUser` is safe.
  - A fresh-database `initChatController` gets a random key, and a CLI fixture database opens with the empty key.
- **Namespace** (tested here):
  - `unshare -rn --pid --fork --kill-child --mount-proc` with lo up, multicast on and the 224.0.0.0/4 route all work.
  - The ephemeral port range is 32768–60999; `localhost` resolves to ::1 and 127.0.0.1 even with `AI_ADDRCONFIG`.
  - Multicast to 224.0.0.251:5227 arrives from source 127.0.0.1.
  - A pty works inside the namespace.
- **Haskell:**
  - `ChatClient.hs` has no export list and imports only library modules plus `ChatTests.DBUtils` and `NameResolver`, so the plan's `other-modules` is complete.
  - `smpServerCfg` writes no shared files (stats and Prometheus intervals are `Nothing`).
  - `NameResolver` binds a free ephemeral port.
  - The SMP server saves its store in `finally stopServer`, so `cancel` waits for it. It binds `[::]` with `ReuseAddr` and installs a SIGINT handler.
  - `USEDuplicateServer` compares hosts within one protocol only.
  - The v7.0.3 assets publish SHA-256 digests, and v7.0.3 has `--mute` and `--user-display-name`.
- **Port slots:** 108 concrete test classes fit under k < 120 in one run, provided slots are taken lazily and the counter is reset each run. Per-slot lock files would remove that dependency.

My experiments are in `/tmp/claude-1000/r4feas` (`glock/`, `t.sh`, `t2.sh`, `mc.py`, `src/`).