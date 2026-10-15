# UI test plan documents vs code: sync review

Date: 2026-10-14, about 19:45 UTC. Report only: no file was edited and nothing was built or run.

Code: `~/wt-uitest` (branch `nd/desktop-ui-tests`, base `eda94a505`, nothing committed). Paths below are relative to `apps/multiplatform` unless they start with `~` or `tests/`.

**The code was changing during this review.** `UiTest.kt` (both platforms), `UiTestApp.kt` (desktop), `Screens.kt`, `TapPoint.kt`, `Helpers.kt` and `ChatListToolbarSearchTest.kt` were modified at 19:42–19:44, and desktop run `b16-desktop.log` was in progress at 19:47. Line numbers are from that snapshot.

Docs reviewed:
- **PLAN**: `plans/ui-tests/2026-10-10-desktop-ui-test-dsl-plan.md` (1267 lines, modified 19:02)
- **ANDR**: `plans/ui-tests/2026-10-12-android-ui-test-automation.md` (297 lines, modified 12:02)
- **START**: `plans/ui-tests/2026-10-13-p1a-start-here.md` (68 lines, modified 2026-10-13 11:58)
- **PROG**: `plans/ui-tests/catalogue/verify/P1a-progress.md` (80 lines, modified 13:47)

---

## 0. Summary

1. **The test counts in every doc are stale.** PLAN 15.3 says "desktop 43/43 … Android 53/53 (38 shared …)", and PROG's last entry says "Desktop 29/29; Android 25/25". The code now has 108 shared tests, 142 desktop tests and 123 Android tests (§1). The last complete desktop run, b15, passed 130 of 142. Android has not run since the shared tests grew from 38 to 108.
2. **Sections 6–9 still read as the implementation spec, but P1a built something simpler.** No doc says which parts were built. The main differences:
   - The poll loop advances a fixed 16 ms per iteration. The plan's real-time clock target and input-lead correction were not built.
   - The hard timeout is 240 s and then halts the JVM. The plan's "2 min + poison" was not built.
   - `forkEvery` defaults to 0, not 1.
   - The runner makes one pass. Multicast pass 2, screenshots and coverage were not built.
   - `uitest-servers` runs `smp1` only, with no `xftp1`.
   - `CliPeer` has no `--mute` and no `CliVersion`/`PeerServers`.
   - There is no `UiDriver`/`UiNode` layer, and section 4's typed semantics do not exist.
   - Alerts are not closed with Escape, and nothing sets the uncaught-error collector.

   Suggested fix: add a "P1a as built" subsection, or mark each block "design, P1b", instead of rewriting each paragraph.
3. **Rule conflict.** PLAN 15.4 item 31 (user rule 2026-10-14: minimal app code; keys only on elements tests act on; no observation-only helpers) conflicts with Summary items 1–2, §1 Goals, §4 typed semantics, §5 "keys everywhere" and §9.4 key lint. §13 records no decision. This needs a recorded decision before the PR.
4. **Several items listed as open are done**, and some marked done are only partly done (§(c)).
5. **ANDR still says "not built / not verified"** (§0 "To build", §3.3) and "one Waydroid instance". Its proposed bridge protocol differs from the one built. The runner does not apply `wd.sh net isolate`, which ANDR §6.3.c recommends.

---

## 1. Current test counts (`@Test` methods; no `@Ignore` anywhere)

| Source set | Path | @Test | Classes (count) |
|---|---|---|---|
| Shared (desktop + Android) | `common/src/uiTestShared/kotlin/chat/simplex/app/ui/{main,chat,groups,settings}` | **108** | ChatListFiltersListsTest 21, ChatListRowsTest 16, ChatListToolbarSearchTest 13, ChatListMenusPendingTest 9, ChatListRequestsCardsTest 8, MessagingTest 6, ContactsTest 5, ChatListMenuTest 4, WhatsNewTest 4, GroupsTest 3, ContactInfoTest 2, ForwardTest 2, MemberInfoTest 2, and 1 each: OneTimeLinkCompatTest, OneTimeLinkTest, AddressCompatTest, ChatProfilesTest, UserPickerTest, ChatListRequestsCardsCompatTest, JoiningCompatTest, SendCompatTest, ChatSearchTest, DeleteMessageTest, EditMessageTest, NotesTest, AppearanceTest |
| Desktop-only catalogue tests | `common/src/desktopTest/kotlin/chat/simplex/app/ui/desktop` | **29** | OnboardingTest 22, ChatListToolbarSearchTest 3, ChatListFiltersListsTest 2, ChatListMenusPendingTest 1, WhatsNewTest 1 |
| Desktop harness tests | `common/src/desktopTest/kotlin/chat/simplex/app/ui/harness` | **5** | HarnessResetTest 2, HarnessBootTest 1, HarnessCliPeerTest 1, HarnessMeasureTest 1 |
| Android-only | `android/src/androidTest/java/chat/simplex/app/ui` | **15** | BackKeyTest 14, SmokeTest 1 |
| **Desktop run total** | | **142** | 108 + 29 + 5 |
| **Android run total** | | **123** | 108 + 15 |

Last runs:
- **Desktop.** `~/uitest-review-tmp/b15-desktop.log` (19:01–19:12): 142 completed, 12 failed, 10 min 58 s. Failures:
  - ChatListMenusPendingTest × 5: `contactMenuForNotReadyContactOnlyDelete`, `groupMenuMuteModesAllMentionsNone`, `pendingConnectionMenuSetName`, `pendingConnectionClickOpensInfo`, `pendingConnectionMenuDeleteInitiated`;
  - ChatListToolbarSearchTest × 6: `searchMatchesPendingConnectionAlias`, `chatListScrollToTopOnTitleClick`, `chatListUnreadDotOnProfileButtonForOtherUsers`, `searchClearsWhenFilterChanges`, `searchFiltersByAlias`, `newChatButtonOpensNewChatSheet`;
  - MemberInfoTest × 1: `memberInfo_removeMember`.

  `b16-desktop.log` was still running.
- **Android.** `build/uitest-android/run-20261014-164429` (16:45–17:01): 53/53 passed. That was before the exhaustive batch, so 70 of today's 123 Android tests have never run on Android.

Note: some class names exist in both `ui.main` (shared) and `ui.desktop` (`ChatListToolbarSearchTest`, `ChatListFiltersListsTest`, `ChatListMenusPendingTest`, `WhatsNewTest`). PLAN 10.1 says packages are `chat.simplex.app.ui.<main|chat|groups|settings>` only, and that each sub-area maps to one class. A desktop-only `ui.desktop` package with same-named classes is not described anywhere. If the traceability check (10.1) is built as written, it will reject these, so this needs a rule.

---

## (a) Statements that are wrong or stale versus the code

Priority: **H** = a status, count or behaviour claim that a reader would rely on; **M** = a design text that reads as current but differs from what was built; **L** = minor wording.

### PLAN, status sections (13, 15)

| # | Pri | Doc line | Doc says | Code | Suggested text |
|---|---|---|---|---|---|
| 1 | H | PLAN:1113 | "desktop 43/43 … Android 53/53 in one run (… 38 shared + 14 Back-key + smoke …). Batch 1 of the exhaustive pass (50 tests …) is being drafted." | The batch is written: 108 shared + 29 desktop-only + 5 harness = 142 desktop; 108 + 15 = 123 Android. b15: 130/142 (12 failures listed in §1). Android not rerun. | "Status 2026-10-14 19:15: desktop 142 tests (108 shared, 29 desktop-only, 5 harness); last full run b15 130/142, failures in ChatListMenusPendingTest (5), ChatListToolbarSearchTest (6), MemberInfoTest (1). Android 123 tests; last run (16:45, before the exhaustive batch) 53/53; the 70 new shared tests are not yet run on Android." |
| 2 | H | PLAN:1122 | Open: "Android `PrefStores` reset; Android Back and notification tests; a 10-run exit check" | `android/.../harness/PrefStores.kt:1-47` exists and runs in `resetForTest` (`UiTestApp.kt:76`). BackKeyTest has 14 tests. No notification test exists. No Android exit-check loop or log was found. | "Open: Android notification tests; Android 10-run exit check. (Android PrefStores reset: done. Android Back tests: done, `BackKeyTest`, 14.)" |
| 3 | H | PLAN:970-974 | Open item 2: settle-step time, render cost per frame, dialog-drain timing and Kover overhead are still to be measured in P1a | Render cost was measured: PLAN 15.1:989 gives 2.6 ms per frame and 10–23 ms per capture. The other three need features that are not built: screenshots, the end-of-test dialog drain, Kover. | "2. Measured in P1a: render cost per frame (2.6 ms, 15.1). Not measurable until P1b builds them: settle step and per-shot budget (7.7), end-of-test dialog drain (6), Kover overhead (7.8)." |
| 4 | H | PLAN:911 (§13 Settled) and PLAN:52 (§1 non-goals) | "Android: out of scope for this plan"; "Android tests are a separate effort" | The Android harness is in the same worktree and shares `uiTestShared`: `android/build.gradle.kts:96` adds `../common/src/uiTestShared/kotlin` to `androidTest`. | §13: "Android (2026-10-13): the Android harness is built in the same worktree; screens, helpers and test bodies are shared through `common/src/uiTestShared` (added to `desktopTest` and to `:android` `androidTest`); details in 15.2–15.4 and ANDR §8." §1: drop the Android non-goal, or say "Android-only catalogue 05 is a separate effort". |
| 5 | M | PLAN:919-920 | The `UiIdle` hooks are listed under "Open:" | Approved and built: `common/src/commonMain/kotlin/chat/simplex/common/ui/UiIdle.kt`; hooks in `SimpleXAPI.kt` (`sendCmd`, receiver) and `Utils.kt` (`trackedBy`); `idle = false` at `ChatModel.kt:827`, `ProcessedErrors.kt:19` and `AppUpdater.kt:171`. | Move the bullet under "Settled" as "UiIdle hooks: approved 2026-10-13, implemented (15.1)". Keep the fallback text as an appendix if wanted. |
| 6 | M | PLAN:862 (§11.2) | "the `UiIdle` busy tracker … still open, section 13" | Implemented (row 5). | "… the `UiIdle` busy tracker of section 6 (approved and implemented, 15.1) …" |
| 7 | H | PLAN:1229 (15.4 item 24) vs PLAN:330, 479, 831, 929 | 15.4 says `forkEvery = 0`. §7.1 code says `forkEvery = 1 // one JVM per test class`. §7.3 says the leaks are "bounded by the number of tests in one class (at most 29)". §10.2 says "With `forkEvery = 1` … about 115 forks". §14 says "bounded by the tests in one class (one JVM per class)". | `common/build.gradle.kts:370`: `forkEvery = (findProperty("uiForkEvery") as String?)?.toLong() ?: 0`. One JVM per fork runs many classes. | §7.1 code: show `forkEvery = (findProperty("uiForkEvery") …) ?: 0` with the comment "0 = one JVM per fork runs many classes (15.4 item 24); -PuiForkEvery=1 restores one JVM per class". §7.3, §14: "bounded by the number of tests one fork runs; with forkEvery = 0 that is a share of the whole run, so RSS (~4.6 MB per reset, 15.1) and one receiver thread per reset accumulate per fork". §10.2: recompute, or mark the Smoke-run fork count as "assumes forkEvery = 1". |
| 8 | H | PLAN:831 (§10.2) | "Rule of thumb: `-PuiForks` ≤ (available RAM − 2 GB) / 3 GB … 2" | `scripts/uitest.sh:67-75`: unless `-PuiForks=` is given, forks = (MemAvailable MB − 2500) / 1000, clamped to 1..4. The runner also refuses to start below 3000 MB available (`:16-17`). | "The runner picks `-PuiForks` from free memory: (MemAvailable − 2.5 GB) / 1 GB, between 1 and 4, unless `-PuiForks=N` is given; it refuses to start with less than 3 GB available (15.4 item 24)." |
| 9 | M | PLAN:1225-1226 (15.4 item 25) | "Chat is now stopped first, as the app's own flow does (`stopChatAsync` before `apiStorageEncryption`). Until that works, each reset spends about 4.25 s …" | `android/.../harness/UiTestApp.kt:38-41` calls `chatController.apiStopChat()`, not `stopChatAsync`. The last Android run (16:46 logcat) still logged "template stays encrypted … database is locked", and its `reset.init` phases took 3.5–5.2 s. The file was changed at 18:17, after that run, so whether the current code works is not known. | "Chat is now stopped first with `apiStopChat` before `apiStorageEncryption`. In the last run (16:46) decryption still failed with 'database is locked', so resets still open an encrypted template (`reset.init` 3.5–5.2 s); the 18:17 change is not yet run." |
| 10 | L | PLAN:1107 (15.3 Android fix 5) | "The runner stops the bridge JVM by the PID it prints" | `scripts/uitest-android.sh:41-42, 26` kills `$!`, the PID of `setsid java …`. The PID the bridge prints is not used. | "… by its PID (`$!` of `setsid java …`)." |
| 11 | L | PLAN:1075 (15.2) | "The runner's `OK` check strips adb's CR line endings." | The runner has no `OK` check now. It parses `am instrument -r` status lines into `results.txt` (`uitest-android.sh:80-86, 104`) and fails on `-2` or `crashed` (`:119`). | "The runner parses `am instrument -r` into per-test `results.txt` (0 pass, -2 fail, -3/-4 skipped, `crashed`) …" |
| 12 | L | PLAN:1055 (15.2 rule 2) | "presses once more if the field hasn't cleared within 1.5 s" | Superseded by 15.4 10b. `Screens.kt` `ENTER_RETRY_MS = 5_000L`; it re-presses only while the field still holds text and Send is enabled. | Add "(now 5 s, see 15.4 10b)". |
| 13 | L | PLAN:1041-1043 (15.2) | Two tests parked with `@Ignore` | No `@Ignore` exists. Both were unparked in batch 2 (PROG:68). | Add "(both unparked in batch 2)". |
| 14 | L | PLAN:3 | "revised 2026-10-13" | 15.3–15.6 are dated 2026-10-14. | "revised 2026-10-14 (status in section 15)". |
| 15 | L | PLAN:990 (15.1) | "the theme detector's thread throws a NPE when the shutdown hook destroys `gsettings monitor` (8.2, `UiUncaught` drops it after the shutting-down flag)" | No `UiUncaught` exists. `UiJvm.shuttingDown` is set (`UiJvm.kt:6,15`) but nothing reads it. The NPE is not suppressed by any harness code. | "… throws a NPE …; the harness only sets a `shuttingDown` flag (`UiJvm.kt`), nothing reads it yet; `UiUncaught` is P1b." |

### PLAN, design sections that read as current but differ from the code

These are mostly P1b design. The fix is either one "P1a as built" note per section or a "(P1b)" tag. The divergences:

| # | Pri | Doc line | Doc says | Code (as built) |
|---|---|---|---|---|
| 16 | H | PLAN:260-262 (§6 "Clock target") and START:41 ("the clock-lead rule after input") | The poll loop tracks `target = clockAtTestStart + realTime`, caps each advance at 100 ms, and corrects the clock lead after input. | Desktop `UiTest.kt:297-316`: each iteration does `advanceTimeBy(16)`, `waitForIdle`, the condition, then `Thread.sleep(16)`. There is no real-time target and no lead correction, so virtual time runs at most about half of real time. Android `UiTest.kt:115-132`: 48 ms per iteration plus a 50 ms sleep. `settle()` is 30 × 16 ms on desktop and one 500 ms advance on Android. Suggested text: "As built (P1a): a fixed 16 ms advance per iteration (Android: 48 ms); composition delays therefore fire late in real time; the real-time target is P1b if needed." |
| 17 | H | PLAN:287 (§6 hard timeout and poisoning) | 2 min (10 min `Slow`) plus per-shot and drain allowances; EDT probe; failure report; interrupt; JVM poisoning. | `UiTest.kt:16, 23-31`: one 240 s timer for every test prints a thread dump and calls `Runtime.halt(3)`, which loses the JVM's remaining tests (PLAN 15.4 item 20 says this). There is no poison flag, no `Slow` category and no EDT probe. Android: 300 s, then `Process.killProcess`, and the runner resumes. |
| 18 | H | PLAN:281-285 (§6 "End of each test", "Uncaught errors") and START:66 | Dialog-drain passes; Escape on each dialog root until no alert; reflection dismiss; wait for `ctrlInitInProgress`; `setCatchNonTestRelatedExceptions(false)`; `UiUncaught` handler; `allowUncaught`. | `UiTest.kt:32-64`: the body ends with `UiTestApp.drain()` inside the scene. On a throwable, the `UncaughtExceptionsBeforeTest`/`DiagnosticCoroutineContextException` wrappers are filtered out, and if every remaining error matches `KNOWN_APP_ISSUES` the test passes (or is retried, up to 3 attempts, if its body never started); otherwise it is rethrown with suppressed traces. `finally`: two `invokeAndWait {}` flushes, `drain()`; if no controller is left, `resetForTest(null)`; stop peers. Alerts are only dropped with `hideAllAlerts` at the next reset (`CommonReset.kt:79-80`), which START:66 calls insufficient. Uncaught app coroutine errors reach kotlinx-coroutines-test's default collector, which is still on. |
| 19 | H | PLAN:298-305 and 309-361 (§7.1 code) | Category filters (`uiTags`/`uiExcludeTags`), `FONTCONFIG_FILE`, `simplex.uitest.shots`/`shotTolerance`/`pass`/`strict`, `uitestCoverageMerge`, `uitestPrepare` steps 2–4, `forkEvery = 1`. | `common/build.gradle.kts:150-160, 362-419`. Built: `kotlin.srcDir("src/uiTestShared/kotlin")`, the three opt-ins, the three dependencies, the `desktopTest` exclude, `KotlinJvmTest` with `onlyIf SIMPLEX_UITEST_RUNNER`, `uiForkEvery` (default 0), `uiForks`, 2 g heap, 12 h timeout, `uiAllowEmpty`, XDG/GSETTINGS/FREETYPE env, `user.*` and tmpdir jvmArgs, **heap dump on OOM to the run dir**, headless/skiko/env/io.parallelism properties, **`testLogging` with `showStandardStreams`**, per-pass result dirs. Not built: categories, fonts, shots/strict/pass properties, coverage. `uitestPrepare` only resolves `desktopTestRuntimeClasspath` (15.1 says so; 7.1 still shows four steps). **Not in 7.1 at all:** `uitestBridgeClasspath` (`:399-410`) and `uitestBridge` (`:412-419`, `-PuiBridgePort`, default 7777). |
| 20 | H | PLAN:363 (§7.1 `env.properties`) | `uitestPrepare` writes the stable CLI, fixture CLIs and migration lists. | `scripts/uitest.sh:58-64` writes only `libAppLib`, `runDir`, `repoRoot`, `cli`, `servers`. There is no migration check (`MigrationCheck` does not exist) and no `CC.ShowVersion` comparison. |
| 21 | H | PLAN:370-385 (§7.1.1) | Two passes (Multicast in pass 2); `-PuiAllowEmpty`; `-PuiPass=1/2`; delete pass dirs; fail if no test ran; screenshot cleanup; coverage merge; usage `-PuiTags=Smoke …`; `cabal build -f uitest …` (no `-O0`); `--stop` before entering the namespace and before pass 2. | `scripts/uitest.sh`: **one pass**, always `-PuiPass=1` (`:12`). No pass-2, empty-run or screenshot/coverage logic; `-PuiTags` has no effect. Additions not in 7.1.1: the MemAvailable ≥ 3000 MB guard (`:16-17`); `-O0` cabal plus `cabal list-bin` (`:26-28`); the peer DB template (`:30-41`); the fingerprint skip of the outside Gradle step via `build/uitest/prepare.key` (`:43-56`); `--stop` only after the outside step, and only when it runs; auto `-PuiForks` (`:67-75`); `-Pkotlin.compiler.execution.strategy=in-process` on every Gradle call (`:6`); the run dir passed as the first `--inside` argument. Old run dirs are pruned to the newest 6, i.e. 5 old plus the new one (`:23`), as the doc says. Usage line: drop the Smoke example. |
| 22 | H | PLAN:391-452 (§7.2 code) | `bootJvm` sets the collector off, `UiUncaught`, `RecordingNtfManager`, `platform = UiTestPlatform`, `UiSeams.installFakes()`. `resetForTest(user, fixture)` order: drain, stop, drain, delete (incl. `dataDir/tmp`), **DbTemplate**, **PrefStores.restore**, clearUiState, `TestServers.restoreBaseline`, fixture, TestPrefs, init, MigrationCheck, fixture-user path, configure, `setLastVersionDefault`, clearTransientUi. | Desktop `UiTestApp.kt:27-45, 59-85`. `bootJvm`: path check, `System.load`, `initHS`, locale, `uiTagsEnabled`, **`NoNtfManager`** (records nothing), window state, `tmpDir`, `runMigrations`, **`ThemeManager.applyTheme`**, blur 0, `PrefStores.capture`, `ModelSnapshot.capture`. Absent: collector, uncaught handler, `platform`, seams. `resetForTest(user)` (no fixture): drain → `stopAndDeleteCore` (uses the app's `deleteChatDatabaseFilesAndState()`; `dataDir/tmp` is not deleted) → drain → **PrefStores.restore → clearUiState → `TestServers.start()` (once per JVM) → DbTemplate.installIfPresent** → TestPrefs → init → (if no template: stop, save, init) → create user, `startChat`, `TestServers.configure()`, `setLastVersionDefault`, clearTransientUi. The doc's order (DbTemplate before PrefStores) contradicts its own 15.1:1020 and the code. |
| 23 | M | PLAN:477 (§7.3 `restartCore(fixture)`) | Ends the composition, drains twice, handles the lock-screen path, polls until init or until an alert shows. | Desktop `UiTest.kt:89-98` + `UiTestApp.kt:87-99`: `setContent {}`, `waitForIdle`, `apiStopChat` (if the controller is valid and `chatRunning != false`), `chatRunning = null`, `chatCloseStore`, `setChatCtrl(null)`, **one** drain, `clearUiState`, `initChatControllerOnStart`, `setContent`, then a poll (30 s) until `chatDbStatus != null && !ctrlInitInProgress`. No fixture, no lock-screen path, no alert exit. Android (`UiTestApp.kt:94-105`, `UiTest.kt:80-89`): `restartCore` first, then a poll (60 s) until chat is running with a user, then close and relaunch the activity (15.4 item 10). |
| 24 | M | PLAN:470 (§7.3 ModelSnapshot) | "The walk skips … `lazy` delegates"; `migrationState` and `changingActiveUserMutex` are handled by hand; roots include `OnboardingSharedState`, `BadgeStore`. | `CommonReset.kt:12-55`: `Lazy` fields are **unwrapped and walked** (15.4 item 26). There is no `changingActiveUserMutex` check and no manual `migrationState` handling. Roots: `ChatModel, AppLock, ConnectProgressManager, ChannelRelaysModel, BadgeModel, VideoPlayerHolder` (no `OnboardingSharedState`, no `BadgeStore`). `ProcessedErrors` timers are not cancelled explicitly. |
| 25 | M | PLAN:459, 524 (§7.2, §7.6 `TestRoot`) | Provides `LocalClipboardManager` **and** `LocalClipboard`; the URI handler can be set to throw. | `TestRoot.kt:22-27`: `LocalClipboardManager` and `LocalUriHandler` only. There is no throw option. |
| 26 | M | PLAN:483-513 (§7.4 driver) | `UiDriver`/`UiNode`/`InProcessDriver`; lookups by `MR.strings`; `node()` requires exactly one match; alerts closed with `press(Key.Escape)`. | No driver interfaces. The API is methods on `UiScope` (desktop `UiTest.kt:78-337`, Android `UiTest.kt:69-312`): `app`, `restart`, `cli`, `poll`, `waitFor`, `exists`, `click`, `rightClick`, `typeInto`, `replaceText`, `pressEnter`, `escape`, `scrollTo`, `scrollIntoView` (desktop), `waitStable`, `settle`, `textOf`, `textsOf`, `descriptionsOf`, `editableText`, `isEnabled`, `seesText`, `seesDescription`, `texts`, `setSlider`, `sliderValue`, `storedSetting`, `clipboardText`. Lookups use literal English text; `exists` means any match. Clicks are **touch** input at `tapPointInNode` (`TapPoint.kt`), not mouse. |
| 27 | M | PLAN:517 (§7.5) | Report dir `<run>/report/pass<n>/<Class>/<test>/` with semantics dump, peer tails and the `UiIdle` list. | Only `<run>/failures/<jvmDir>-NN-<what>.png` (`UiTest.kt:320-325`); the assertion message carries the in-flight list and the first 60 visible texts. Peer timeouts append the peer's next 10 lines. |
| 28 | H | PLAN:595-607 (§8.1) | Every peer gets `--mute`, `--xftp-server`, `$HOME/Downloads`, `TERM=xterm`; start–stop–start cycle with a 30 s deadline; the pty master fd is set raw; 200 ms "unchanged" window; special handling of `! `, `> `, `\ `; `CliVersion`, `PeerServers`, `PeerFlags.key`, `expectsAny`, `repeatUntil`, `snapshot/restore`. | Desktop `CliPeer.kt:13-53, 208-218`: args `-d`, `--user-display-name`, `-s <smp1>`, plus optional `--reactions`, `--mark-read`, `--show-full-links`. **No `--mute`** (the plan's D-Bus notification concern applies), no `--xftp-server`, no Downloads dir, `TERM=xterm-256color`, `LC_ALL=C.UTF-8`, `setpriv --pdeathsig KILL` plus `stty -echo -icanon` before exec. No restart cycle: the first start copies `<run>/peer-template` (15.1:1024) and waits 60 s. Typing: 5 attempts, a 1.5 s unchanged window, a 500 ms Enter retry; no `! `/`> `/`\ ` handling. Extra methods: `expectsMessage`, `eventuallyMessage`, `eventuallyMatching`, `typeLine`, `drainLines`, `awaitStart`, `pending`. `sends` consumes the first matching echo **anywhere** in the buffer, not only the next line. Line timeout: 10 s on desktop, **15 s** on Android (`android/.../CliPeer.kt:319`). |
| 29 | H | PLAN:639, 652, 839-840 (§8.2, §11.1) | "It starts `smp1` and `xftp1` at launch"; control protocol `start/stop/reset/register/clear-names`; `restoreBaseline()` each reset; peers get `--xftp-server`. | `tests/UITestServers.hs:18-46`: args `<dir> <portBase>`; starts **smp1 only**, prints `ready smp1 smp://…@localhost:<base+1>`, exits on stdin EOF; no protocol. `TestServers.kt:31` still builds an `xftp1` address on `base+2`, and `configureServers` (`ServersConfig.kt`) adds it to the GUI's custom group, but **nothing listens there**. `uitest-android.sh:71` also reverses `base+2`. Suggested 8.2 status line: "P1a: smp1 only; xftp1 is configured in the GUI but not served (P1b)." PROG:38 "adds smp1/xftp1" needs the same note. |
| 30 | M | PLAN:639 | "`uitest-servers` is spawned … from the same daemon thread" (so that pdeathsig doesn't fire early) | `TestServers.start()` runs on the test thread inside the first `resetForTest`; peers are spawned from the test thread too. On the bridge it is the main thread. | State the actual spawning threads. |
| 31 | M | PLAN:719-741, 747-790 (§9.2, §9.3) | `uiTest(realTimes)`; `app(name, onboarded, fixture): GuiUser`; `connect(gui, peer)`; `relay`, `mobileHost`, `servers`, `allowUncaught`, `screenshot`, `shouldHave`; "`app()` … may be called once; a second call fails"; examples use `alice.chatList.invitation("team").join()`, `connect(alice, bob)` and `bob expects "#team: alice joined the group"`. | `uiTest { }` takes no parameters. `app(user: String?): App` (`null` = onboarding boot). `connect(peer)` (`Helpers.kt`). `restart(): App`. **`app()` is not limited**: `HarnessMeasureTest` calls it 20 times. `JoiningCompatTest.kt:11-17` uses `connect(bob)`, `alice.chatList.open("team")`, `click(Ui.Alert.Confirm(Ui.AlertSource.Shared))` and `bob.eventually("#team: alice joined the group")`. `ChatListScreen.row()` returns the screen, not a `ChatRow`. |
| 32 | M | PLAN:1-25, 201-235, 237-256, 796 (Summary 1–2, §4, §5, §9.4) vs PLAN:1240 (15.4 item 31) | Typed semantics on rows and items; keys on every component; key lint. | `common/src/commonMain/kotlin/chat/simplex/common/ui/` has only `Ui.kt` and `UiIdle.kt`; there is no `UiSemantics.kt`. The keys are the minimal set the tests use (`Ui.kt`, 182 lines). The user rule of 2026-10-14 (15.4 item 31) is in force. **Needs a decision recorded in §13:** whether §4, "keys everywhere" and §9.4 are dropped, deferred or still the target. |
| 33 | L | PLAN:22 (Summary) | "DSL in Kotlin, in the `desktopTest` source set" | Shared bodies are in `common/src/uiTestShared/kotlin`, compiled into `desktopTest` (`common/build.gradle.kts:151`) and `:android` `androidTest` (`android/build.gradle.kts:96`). | Update. |
| 34 | L | PLAN:10.1 traceability, categories (PLAN:364, 814-827) | `Smoke`, `StableCompat` etc. in package `chat.simplex.app.ui`; stable subclasses | No category interfaces, no `CatalogueTraceabilityTest`, no `…CompatStableTest` classes. This is consistent with P1a scope, but nothing says it is P1b-pending. | Tag "(P1b)". |

### START (`plans/ui-tests/2026-10-13-p1a-start-here.md`)

The document is a pre-implementation guide. P1a is complete (PLAN:1026), so it is historical. Suggested: add one line at the top: "P1a completed 2026-10-13 (plan 15.1); this page is kept for history; for the current harness see plan 15.2–15.4."

| # | Pri | Line | Says | Code |
|---|---|---|---|---|
| 35 | M | START:25 | "`forkEvery = 1`" | `forkEvery` defaults to 0 (row 7). |
| 36 | M | START:41 | "the clock-lead rule after input" | Not built (row 16). |
| 37 | M | START:43 | "`CliPeer` … with the peer flags and typing rules of §8.1" | Partial: no `--mute`, different typing constants (row 28). |
| 38 | M | START:66 | "Close alerts with Escape" | The harness never closes alerts with Escape; `hideAllAlerts` is used at reset (row 18). |

### PROG (`plans/ui-tests/catalogue/verify/P1a-progress.md`)

| # | Pri | Line | Says | Code / correction |
|---|---|---|---|---|
| 39 | H | PROG:76-80 (last entry) | Batch 4 is the last entry: "Desktop 29/29; Android 25/25", "plan 15.4 items 1–16 plus 10a/10b/15a" | Missing: the exhaustive batch (43/43 and 53/53 at 16:28–17:01) and the onboarding/What's New/chat-list batch (142 tests, b15 130/142). 15.4 now runs to item 31. Add "Batch 5" and "Batch 6" entries with the §1 counts. |
| 40 | M | PROG:10 | "minimal `uitestPrepare` (resolves … and writes `build/uitest/env.properties`)" | `uitestPrepare` no longer writes `env.properties`; `scripts/uitest.sh:58-64` does. |
| 41 | M | PROG:11-14 | Runner: "one pass; no cabal step yet" | The cabal step exists (`uitest.sh:26`), plus the fingerprint skip, peer template, MemAvailable guard and auto forks. It is still one pass. |
| 42 | L | PROG:51 | "Full reset (`Reset.kt`: PrefStores, ModelSnapshot, UiState)" | `ModelSnapshot`, `CommonUiState` and `CommonPrefs` moved to `uiTestShared/.../harness/CommonReset.kt`. `Reset.kt` keeps the desktop `PrefStores` and `UiState`. |
| 43 | L | PROG:69 | "host bridge (`UiBridge`, Gradle `uitestBridge`)" | The runner no longer uses `uitestBridge`. It uses `uitestBridgeClasspath` plus `java -cp` (PROG:78 says so, but line 69 reads as current). |

### ANDR (`plans/ui-tests/2026-10-12-android-ui-test-automation.md`)

| # | Pri | Line | Says | Code / correction |
|---|---|---|---|---|
| 44 | H | ANDR:3, 24-29, 109-117 (§0 "To build (not done)", §3.3 "Not verified") | No instrumentation APK built; stale runner class; `ui-test-junit4` commented out | Done: `android/build.gradle.kts` `testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"`, plus androidTest deps (runner 1.6.1, core 1.6.1, espresso-intents 3.6.1, uiautomator 2.3.0, ui-test-junit4 1.8.2). 123 tests; last run 53/53. Change the status line to "built and running (see §8)", and mark §3.3 as resolved. |
| 45 | H | ANDR:52, 187 (§2 table, §5.1 throughput) | "Multiple instances: **one** per host"; "There is one Waydroid instance, so tests run serially" | `uitest-android.sh:3, 8` defaults to `WD_INSTANCE=2`. A second instance exists (instance 1 shared, 2 for UI tests, PLAN 15.4 item 15a). | "Two instances (1 shared, 2 for UI test runs); runs are serial within an instance." |
| 46 | M | ANDR:147-160 (§5.1 shared driver) | "a shared `UiDriver` over `SemanticsNodeInteractionsProvider`"; source set `androidInstrumentedTest`; typing with `replaceText` **plus `setSelection`**; reads via a `ViewAction`, including `.hint` (placeholder) | No shared driver: each platform has its own `UiScope` with the same method names, and the shared code compiles against each. The shared dir is added to `:android`'s `androidTest` (`android/build.gradle.kts:96`). The message field uses Espresso `replaceText` with retries (`UiTest.kt:210-222`), no `setSelection`; reads use a `ViewAssertion` (`:257`); the hint/placeholder is not read. |
| 47 | H | ANDR:167-182 (§5.1 peer bridge) | Protocol `start bob current reactions=0`, `sends`, `types`, `contactId`, `expect … <timeoutMs>` → `mismatch`, `servers reset smp1`, `snapshot bob`; `start` replies at once; comparison host-side | `UiBridge.kt:339-380`: one client at a time on 127.0.0.1:7777. Commands: `servers` → `ok <smp1> <xftp1>`; `start <name> <flags,comma>` → `ok` **after the peer has started** (synchronous); `type <name> <text>`; `drain <name>` → `lines N` + lines; `stop <name>`. Errors are `err <msg>`; on disconnect all peers stop. Comparison (`expects`, `eventually`, `contactId`) runs **device-side** on drained lines (`android/.../CliPeer.kt`). The Android `Bridge.PORT` is a hard-coded 7777, so `-PuiBridgePort` on the unused `uitestBridge` task can't change it. |
| 48 | M | ANDR:183-185, 247-248 (§5.1 runner, §6.3.c) | `connectedFossDebugAndroidTest` or `am instrument -e class`; "collect JUnit XML"; `wd.sh up → wd.sh net isolate → …` | `scripts/uitest-android.sh <app.apk> <test.apk> [am args]`; APKs come from `~/build/android.sh` with `ANDROID_TEST=1`. Results: `results.txt`/`instrument*.txt`/`logcat.txt`/`timings.txt`/`failures/`, not JUnit XML. **`wd.sh net isolate` is never called**, so the container keeps internet access during runs. Add it, or note that isolation relies on `configureServers` and onboarding opening no connection. |
| 49 | M | ANDR:241-243 (§6.3.a), ANDR:270 (§7 decided) | `debugImplementation ui-test-manifest`; "`testTagsAsResourceId = true` is set on the root in debug builds only" | Neither exists (grep finds no `testTagsAsResourceId`, and there is no `ui-test-manifest` dependency). The §7 "decided" line states as done something that is not built. | Mark "not built (not needed for the instrumented layer)" or schedule it. |
| 50 | L | ANDR:98 vs ANDR:296 | "`run-as chat.simplex.app.debug` works" vs "`run-as` is blocked" | They contradict; PLAN 15.4 item 13 says blocked. Fix §3.2. |
| 51 | L | ANDR:11, 93, 128 | First DB creation after `pm clear` "~45 s" | Measured in the harness: 15.4 s (PLAN 15.2:1075). Add the measured value. |
| 52 | L | ANDR:277-283 (§7 open 1, 3) | Q1: runner script vs Gradle | Resolved: script (`uitest-android.sh`). Q3 (libndk vs libhoudini) is not addressed by the code; leave it open. |
| 53 | L | ANDR:294 (§8) | "A simple test now takes 15–20 s"; reset "about 4 s" | Last run: the `reset` phase median was 5.1 s (3.7–7.7 s, n = 53, from logcat). PROG:78 says 10–30 s per test. Align the numbers. |

---

## (b) Implemented behaviour the docs do not mention

Harness rules and API:
1. **`described(contentDescription)` pseudo-key** (`uiTestShared/.../Helpers.kt`: `DESCRIBED = "@described"`). `matcher()` turns it into `hasContentDescription(…) and hasClickAction()` on both platforms. It replaces the planned `Ui.Modal.Back` key: `goBack()` clicks `described("Back")` on desktop and uses `escape()` on Android. No doc mentions it.
2. **Shared helpers**, none of them in the docs:
   - `Helpers.kt`: `connect(peer)`, `connectPeers(a, b)`, `hostGroup(host, group, members…)`, `quiesce()` (30 s), `goBack()`, `CONNECT_TIMEOUT_MS = 30_000`, `QUIET_MS = 1_000`;
   - `Screens.kt`: `ownGroup(app, group, members…)`;
   - `CliPeer.createAddress(business)`, `CliPeer.createGroupLink(group)`, `UiScope.joinLink(link)` (`ChatListFilterScreens.kt`);
   - the other `ChatList*Screens.kt` files.

   PLAN 9.1 lists screen classes that are different from those built: `App`, `ChatListScreen`, `NewChatScreen`, `UserPickerScreen`, `ProfilesScreen`, `AddressScreen`, `AppearanceScreen`, `ProfileScreen`, `ChatScreen`, `ContactInfoScreen`, `GroupInfoScreen`, `MemberInfoScreen`.
3. **Known-issue handling details** (desktop `UiTest.kt:42-51, 339-343`). PLAN 15.4 item 28 does not say:
   - the `UncaughtExceptionsBeforeTest` and `DiagnosticCoroutineContextException` wrappers are dropped before matching;
   - a known issue **after the body started passes the test** (it is logged, not retried);
   - the allowlist is matched against the whole `cause` chain;
   - the patterns are "Controller is not initialized" and "database closed".
4. **End-of-test recovery** (`UiTest.kt:55-60`): two EDT flushes, then `drain()`. If the test left no chat controller, the harness runs `resetForTest(null)` to bring up an empty core. The test directory `t<n>` is deleted only after a pass.
5. **`TestServers` slot/port details**: 120 slots, `slot-<k>.lock` in the run dir, base 20000 + 100k. These match 8.2. The `xftp1` address is fabricated, with nothing listening (row 29).
6. **Desktop `scrollIntoView`** (`UiTest.kt:176-193`): 10 attempts, a 0.6-page step for unplaced nodes, a 50 px margin, 40 animation frames per step. **Desktop `scrollTo`** pages up to 20 times per list. PLAN 15.4 item 20 mentions paging, but not the constants or that `click` and `setSlider` call `scrollIntoView` first.
7. **`replaceText` on desktop**: select-all, wait for the selection, `performTextInput`, then after 2 s fall back to `performTextReplacement` (`UiTest.kt:236-251`). PROG:68 mentions "select-all, settle, then input" but not the fallback.
8. **`escape()` on desktop advances 3 frames before invoking the back stack** (`UiTest.kt:195-199`).
9. **Click log line** `UITEST click <tag> at <point> of <rect> popups=<n>`, on both platforms. This is the "click-geometry log" in PROG:79; the plan doesn't name it.
10. **`TapPoint` widens nested buttons to a 48 dp minimum touch target** before choosing the free span (`TapPoint.kt:207-213`). PLAN 15.4 item 8 omits this.
11. **Android-specific harness rules** absent from PLAN and ANDR:
    - `rightClick` is a `longClick`;
    - `pressEnter` on the message field clicks Send, and elsewhere sends an IME action;
    - `clipboardText` reads the last recorded intent's `EXTRA_TEXT` (also inside `ACTION_CHOOSER`), then the system clipboard;
    - `settle()` is one 500 ms advance;
    - `Timing.phase`/`Timing.wait` log `UITEST phase N ms: …` and `wait N ms: …` (waits ≥ 500 ms), which the runner extracts to `timings.txt`;
    - `bootOnce` waits up to 180 s for the app's own startup;
    - Android preset prefs (`presetAndroidPrefs`, `UiTestApp.kt:64-71`): blur 0, `notificationsMode = OFF`, both background-service notices shown, `canAskToEnableNotifications = false`, `privacyProtectScreen = false`;
    - the Android template lives in `dataDir/uitest-db-template`, and the key file is empty when decryption succeeded.
12. **Android BackKeyTest helpers** (`back/BackKeyHelpers.kt`): `backFromRoot()` (`pressBackUnconditionally`, then wait for `MainActivity` to finish), `relaunch {}`, `keyboardShown()` via IME insets, `hideKeyboard()`. The 14 Back tests are not listed anywhere (PLAN 15.3 gives only the count).
13. **Android runner options and behaviour** (`uitest-android.sh`), not documented:
    - usage `<app.apk> <test.apk> [am instrument -e …]`;
    - env `WD_INSTANCE` (default 2) and `WD_OWNER` (default `uitest-$USER`);
    - hard-coded `adb` and JDK 17 paths;
    - permissions granted after each `pm clear`: POST_NOTIFICATIONS, CAMERA, RECORD_AUDIO;
    - `logcat -G 16M`;
    - the bridge rebuilds only when one of 7 listed sources is newer than the classpath snapshot;
    - the app-APK skip compares the APK sha256 plus `lastUpdateTime` in `installed-app-i<N>`;
    - the exit status fails on any `-2` or `crashed`, and skipped (-3/-4) counts as success.
14. **Desktop runner details** (`uitest.sh`), not documented:
    - in-process Kotlin compilation on every Gradle call;
    - the peer template creation command (`--maintenance -e /v -t 0`, accepted only if the output contains "no active user in maintenance mode");
    - the fingerprint inputs: all `*.gradle.kts`, `gradle.properties`, `*.versions.toml`, the `cpp` sources excluding `libs`, and an `ls -l` of the staged libs except `libapp-lib.so`;
    - `-PuiForkEvery` (Gradle).
15. **The bridge's peer directories** (`<bridge jvmDir>/bridge-peers/<nanoTime>-<name>`) are never deleted, unlike desktop `t<n>`.

---

## (c) Open/done status mismatches

| Item | Where listed | Actual state |
|---|---|---|
| Android `PrefStores` reset | PLAN:1122 open | **Done** (`android/.../PrefStores.kt`, used at `UiTestApp.kt:76`) |
| Android Back tests | PLAN:1122 open; PLAN:1081 "remaining" | **Done**: `BackKeyTest`, 14 tests (passed in the 53/53 run) |
| Android notification tests | PLAN:1081, 1122 open | Still open (no test exists) |
| Android 10-run exit check | PLAN:1082, 1122 open | Still open (no loop or log found) |
| Android "remaining" items 1–4 (boot/reset/template, `uiTestShared`, bridge, port of the 5 P1a tests) | PLAN:1076-1080 "Remaining for Android P1a" | **Done**, but item 2 is built without the `UiDriver`/`Peer` interface the text names, and item 1's class names (`AndroidUiTestApp`, `AndroidPrefStores`, `AndroidUiState`) don't exist (actual: `UiTestApp`, `PrefStores`, shared `CommonUiState`) |
| `UiIdle` hooks | PLAN:919-920 under "Open"; PLAN:862 "still open" | **Done** and approved |
| Measure render cost | PLAN:972 open | **Done** (15.1:989) |
| "Batch 1 of the exhaustive pass is being drafted" | PLAN:1113 | **Written**: 29 desktop-only + many shared tests; b15 130/142 |
| ANDR "To build" list (§0, §6.3.a–d) | ANDR:24-29, 239-250 open | a/b/c/d **done**, except `testTagsAsResourceId`, `ui-test-manifest` and `net isolate` |
| ANDR §3.3 "Not verified (needs a build)" | open | **Resolved** |
| ANDR open Q1 (runner) | open | **Resolved**: script |
| ANDR open Q2 (in-process reset enough?) | §7 open, answered in §8 | Answered in §8; mark it closed in §7 |
| `testTagsAsResourceId` in debug builds | ANDR:270 "Decided" (reads as done) | **Not built** |
| Template-encryption fix on Android | PLAN 15.4 item 25 "now stopped first" (reads as fixed) | Not shown to work. Last run: still "database is locked"; the newer code is unrun (row 9) |
| `UiUncaught` drops the gsettings NPE | PLAN 15.1:990 "Confirmed as predicted" | **Not built**: `shuttingDown` is set but never read |
| P1a "UiIdle with the hard timeout (no other watchdog parts)" | PLAN:891 P1a row | **Done** as specified (240 s halt) |
| "Nothing is committed" | PLAN:1124, PROG:3 | Still true (`git log`: HEAD is `eda94a505`; all harness files untracked) |
| xftp1 | PROG:38 "adds smp1/xftp1"; PLAN 8.2/11.1 | Configured but **not served**. Not done. Any XFTP-using test will fail or hang until P1b |
| Peer `--mute` | PLAN 8.1 (reads as done) | **Not done** |

---

## Suggested minimal doc changes before the PR

1. In PLAN §15, add a dated "Status 2026-10-14 (code snapshot 19:45)" block with the §1 counts and the b15 result. Update the 15.3 "Open" list as in §(c).
2. At the top of PLAN §6, §7.1, §7.1.1, §7.2, §7.3, §7.4, §8.1, §8.2 and §9.2, add a one-line "As built in P1a" note. Rows 16–31 give the content.
3. Record in §13 the decision that row 32 needs: typed semantics, keys everywhere and key lint vs the minimal-app-code rule.
4. ANDR: status line, §2 instance count, §3.3 resolved, §5.1 bridge protocol as built, §6.3 checkboxes, §7 Q1/Q2 closed, and the `net isolate` gap.
5. PROG: add batch 5 and batch 6 entries, and fix lines 10–14.
6. START: add a one-line "historical, P1a complete" header.
