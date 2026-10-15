# P1a progress (started 2026-10-13, time box 3 h this session)

Worktree `~/wt-uitest`, branch `nd/desktop-ui-tests`, based on origin/master eda94a505. Nothing is committed yet.

## Done
- `common/build.gradle.kts`:
  - desktopTest opt-ins and dependencies (`uiTestJUnit4`, `currentOs`, `pty4j` 0.13.13);
  - `desktopTest` excludes `chat/simplex/app/ui/**`;
  - `desktopUiTest` (`KotlinJvmTest`; no Kover or fonts yet; testLogging shows stdout for P1a);
  - minimal `uitestPrepare` (resolves `desktopTestRuntimeClasspath` and writes `build/uitest/env.properties`; later `env.properties` moved to `scripts/uitest.sh`, and `uitestPrepare` only resolves the classpath).
- `scripts/uitest.sh`, a P1a cut of plan §7.1.1:
  - outside, it runs cmake, the test classes and `uitestPrepare`;
  - inside `unshare -rn --pid`, it runs the offline `desktopUiTest`;
  - one pass; no cabal step yet. (Later added: the cabal step, the fingerprint skip of the outside Gradle step, the peer DB template, the MemAvailable guard and automatic forks; still one pass. Plan 0.5.9.)
- Harness `common/src/desktopTest/kotlin/chat/simplex/app/ui/harness/`:
  - `UiEnv`, `UiJvm` (per-JVM directory, `user.home`, tmpdir, shutdown hook);
  - `UiTestApp` (`bootJvm`, `resetForTest` with an approximate stop-and-delete reset);
  - `UiTest`/`UiScope` (`app`, `poll`, `texts`);
  - `HarnessBootTest`, `HarnessMeasureTest` (20 resets, frame cost, `captureToImage`).
- The namespace works on this host: lo only, uid 0, no DNS.

## Results (session 1)
- Both `HarnessBootTest` and `HarnessMeasureTest` PASS inside the namespace, offline. The measurements are in plan section 15.1.
- Fixes made on the way:
  - `import java.time.Duration` (the plan snippet is corrected);
  - submodule init;
  - the explicit `platform.NtfManager` import;
  - `TestPrefs.preset()`;
  - a stopgap: `conditionsAction` cleared until `TestServers.configure` lands.
- Libs: hardlinked from `~/build/slots/s3` (`libapp-lib.so` is a real copy). `stage-libs.sh` refuses on master (it expects the reviewed `build-lib-linux.sh` hash).
- Run: `cd apps/multiplatform && scripts/uitest.sh --tests '*Harness*'`. Logs are in `~/uitest-review-tmp/p1a-run*.log`.

## Done after session 1
- Template DB (`DbTemplate.kt`, `<run>/db-template` with the key). Reset 7.2 s → 0.94 s median. Both harness tests pass (run 11).

## Done (session 2)
- `uitest-servers` (`tests/UITestServers.hs`; cabal flag `uitest`, executable stanza): smp1 only, `ready smp1 <addr>`, EOF stop. Built with `cabal build -O0 -f uitest --builddir=dist-uitest` (25 min cold); the runner builds it and passes the paths through `env.properties`.
- `CliPeer` + `VtScreen` (pty4j, `stty -echo -icanon` before exec, verified typing, echo wait, `expects`/`eventually`/`nextLine`/`contactId`); `TestServers` (slot lock, port base 20000+100k, `configure()` disables the operator groups and adds smp1/xftp1, refreshes conditions).
- `HarnessCliPeerTest` PASS: the CLI connects to the GUI's one-time short link over the local SMP; `contactId` = 3. All 3 harness tests pass together (150 s); no leftover processes.
- Plan fixes: `configure` must run after `startChat` (`chatNotStarted`), and `chatModel.currentUser` must be set first.

## Done (session 2, continued)
- Keys in app code (`ui/Ui.kt`, `uiTagsEnabled` internal): alerts (by manager), chat-list rows (both platforms), new-chat buttons and actions, `SectionItemView`/`SettingsActionItem`/`UserPickerOptionRow`/`ExposedDropDownSetting` `uiKey` params, `LinkTextView`, the message field (both platforms), user picker/address/settings/appearance.
- `TestRoot` (in-memory clipboard, recording URI handler); screen objects (`App`, `ChatListScreen`, `NewChatScreen`, `UserPickerScreen`, `AddressScreen`, `AppearanceScreen`); `restart()` per plan 7.3.
- Tests: main/OneTimeLinkCompatTest, main/AddressCompatTest, chat/SendCompatTest, groups/JoiningCompatTest, settings/AppearanceTest. All 5 PASS individually.
- Exit check loop: `~/uitest-review-tmp/p1a-exit/loop.sh`, summary in `summary.txt`.

## Done (session 2, end)
- `UiIdle` hooks plus harness use (drain, deadline extension, timeout messages); `idle = false` at PopChatCollector, ProcessedErrors and createUpdateJob.
- Peer start is async with a 60 s deadline (first start 8–10 s, over 30 s under load).
- Full reset (`Reset.kt`: PrefStores, ModelSnapshot, UiState) and `HarnessResetTest`. All 10 tests pass (run 33). (Later `ModelSnapshot`, `CommonUiState` and `CommonPrefs` moved to `uiTestShared/.../harness/CommonReset.kt`; `Reset.kt` keeps the desktop `PrefStores` and `UiState`.)
- Final exit check: `~/uitest-review-tmp/p1a-exit2/` (20 runs).

## P1a COMPLETE (2026-10-13)
- Final exit check: 20/20 runs (`~/uitest-review-tmp/p1a-exit2/summary.txt`), 98–329 s per run.
- Also done: outside-Gradle skip by fingerprint; CLI peer DB template (first start 11–14 s → 0.4 s).

## Next
- Report to the user; P1b re-plan; the first-PR question (open after P1a?). Nothing committed yet.

## Batch 1 (2026-10-13, after P1a)
- Desktop: 12 more catalogue tests pass (messaging 5, contacts 3, profile 1, groups 3); 2 parked with @Ignore (`edit_menu_updatesPeer`, `deleteContactAndNotify`, reasons in plan 15.2). Driver rules and keys are in plan 15.2.
- Android: `ANDROID_TEST=1` mode in `~/build/android.sh` (backup `android.sh.pre-androidtest`); androidTest dependencies; `initUiTags`; `SmokeTest` PASS on Waydroid instance 2; runner `scripts/uitest-android.sh`. Instance 2 was stopped again afterwards (`wd.sh down`).
- Next for Android: boot/reset/template, `uiTestShared` refactor, peer bridge with adb reverse, port the 5 tests, Back and notification tests, exit check (about 2 days).

## Batch 2 (2026-10-14)
- VM crashed (OOM) during an Android build: lingering Kotlin daemons. Now in-process Kotlin compile in uitest runs, plus a MemAvailable guard (memory note `feedback_kotlin_daemon_ram_crash`).
- Desktop: edit and delete-contact unparked (driver: select-all, settle, then input; `ScrollBy` needs clock frames). Shared folder `common/src/uiTestShared` (screens, helpers, server config, `CommonReset`, tests). All 24 desktop tests pass (19 catalogue + 5 harness).
- Android: host bridge (`UiBridge`, Gradle `uitestBridge`; replaced in batch 4 by `uitestBridgeClasspath` + `java -cp`) plus Android `CliPeer`/`UiTest`/`UiScope`/`UiTestApp`; adb reverse; Espresso-Intents capture for "Share"; Android pre-set prefs; the runner skips an unchanged app APK. 12 of 18 shared tests pass on Android (run-20261014-080106).
- Android open: accept-request timeout, message-field replace flake (edit, group send), unread badge, alias, delete contact.

## Batch 3 (2026-10-14, later)
- Desktop: ChatListMenuTest (4) and OneTimeLinkTest incognito (1) pass. Keys and driver calls are in plan 15.3.
- Android fixes: clock advance in poll, message-field retry, 30 s accept wait, failure screenshots, bridge PID kill. 14/14 executed passed in run-20261014-090056. Claude Code stopped the run on host memory pressure from another session; 10 tests not reached (list in plan 15.3).

## Batch 4 (2026-10-14, afternoon): Android working and faster
- Desktop 29/29; Android 25/25 (all 24 catalogue tests + smoke) in one run, run-20261014-134126.
- Speed: bridge 1–7 min → 1 s (no Gradle); reset 31.6 s → ~4 s (template from the startup DB); close 45 s → 0.4 s (release Intents first); Waydroid boots in parallel; per test 65–90 s → 10–30 s.
- Robustness: watchdog + crash-resume runner, per-test results.txt, failure screenshots, click-geometry log, tap point that avoids nested buttons (no coordinates in tests).
- Findings with causes: plan 15.4 (items 1–16 plus 10a/10b/15a).

## Batch 5 (2026-10-14, evening): exhaustive pass, batch 1 (onboarding, What's New, chat list)
- Starting point: desktop 43/43 twice (`b13-desktop-1/2.log`); Android 53/53 (`run-20261014-164429`: 38 shared + 14 `BackKeyTest` + smoke). The Back-key tests and 14 more shared tests came after batch 4.
- 50 catalogue rows drafted in parallel (`plans/ui-tests/drafts/b1`, `batch1.tsv`): 22 onboarding, 5 What's New, 23 chat list (toolbar, subscription indicator, search, unread and preset filters). Desktop-only rows (onboarding, What's New for a new user) go to `desktopTest/.../ui/desktop`.
- Harness changes found while running it: `ModelSnapshot` unwraps `by lazy` fields (plan 15.4 item 26); per-test `drain()` inside the scene and the known-app-issue allowlist (15.4 items 27–28; 15.5 item 3); desktop failure PNGs (item 29); the CLI typing window 200 ms → 1.5 s (item 30).
- Catalogue corrections from passing tests: plan 15.6.
- First desktop run with the batch: `b14-desktop.log`, 93 tests, 19 failed.

## Batch 6 (2026-10-14, late evening): chat list lists, rows, menus, cards; PR review fixes
- 50 chat-list rows drafted (`plans/ui-tests/drafts/b2`, `batch2.tsv`): the remaining preset filters, user lists, row previews and icons, row menus, pending connections, contact requests and cards. Batches 5 and 6 together added 99 catalogue tests: 74 shared, 25 desktop-only.
- Runs: `b15-desktop.log` 142 completed, 12 failed; `b16-desktop.log` 142 completed, 8 failed. Final results: plan 15.3 status ("RESULTS" line).
- PR review fixes (plan 0.5 describes the result):
  - app code: `UiIdle` is a no-op unless `uiTestHooksEnabled` (renamed from `uiTagsEnabled`); counters removed; `sendCmd` and the receiver use thin `UiIdle.track`/`trackEvent` wrappers; unused keys removed (`Ui.UserPicker.Sheet` kept as the one presence-only key); app rule in plan 13;
  - peers: line matching moved to shared `PeerLines` (15 s line timeout); the platform `CliPeer`s implement only `fill()`/`type()`; the Android bridge socket has a 60 s read timeout;
  - Android `poll` extends past its deadline while the same API calls are in flight (60 s), as desktop does; a condition that throws counts as "not yet", and the last error is reported (both platforms);
  - `described(text)` pseudo-keys and `goBack()`; `enterText` without echo verification; the paste-link search tests are shared now; desktop-only classes renamed `*DesktopTest` (`OnboardingTest` kept);
  - helpers consolidated in `Helpers.kt` (`openContextMenu`, `joinGroupFromList` with `MemConnected` + `quiesce`, `CONNECT_TIMEOUT_MS` = 60 s);
  - the allowlist passes only tests whose body completed and never matches an `AssertionError`;
  - desktop JVM crash logs go to the run directory (`-XX:ErrorFile`), and the shutdown hook stops the core; `HarnessMeasureTest` is `@Ignore`d (manual benchmark); click debug logs removed (the click-geometry log of batch 4 is gone);
  - `scripts/uitest-android.sh` is portable: `UITEST_SERIAL` selects any adb device and skips Waydroid; `ADB`, `JAVA_HOME`, `UITEST_APP_ID`; it requires one `scripts/uitest.sh` run first and says so;
  - What's New tests read the app's `lastVersion` by reflection;
  - Android template decryption calls `apiStopChat` before `apiStorageEncryption`; verified in the final Android run (plan 15.4 item 25).
- Counts now (plan 0.5.1): desktop 142 `@Test` (112 shared + 25 desktop-only + 5 harness, 1 ignored); Android 127 (112 shared + 15).
- Still open: Android notification tests, the Android 10-run exit check, `wd.sh net isolate` in the Android runner, `xftp1` not served (plan 15.3 "Open").

## Batch 7 (2026-10-14, night): PR preparation, second review, first full Android run
- Plan docs synced with the code: plan 0.5 "As built", one-line "as built" notes in §§2–10, §13 decisions (Android in scope, minimal app code, desktop forks from free memory); START marked historical; ANDR doc status, bridge protocol and runner.
- Catalogue corrections from batches 1–2 applied to `01-onboarding-chatlist-connect-profiles.md` (35 full, 5 partial; the skipped parts were stale after the tests became shared).
- Second code review (two concurrent reviewers) and fixes: plan 15.4 items 32–41, 15.4.2 (test strength limits and accepted harness limits).
- Desktop runs: b18 136/141 + 4 failed (pending-connection info has no Back on desktop, item 33); b20 141/141; b21 140/141 (one SQLite "disk I/O error" after a reset, item 41); b22 141/141 (6.6 min).
- First full Android run of the shared tests (127): 121 passed; the 6 failures were Android-only (keyboard eats the first Back, prepared chat covers the list, no camera on Waydroid → app finding 15.5 item 4, one flake 15.4.1a); after the fixes the 4 affected classes passed 49/49 with the camera test skipped.
- App code fix needed for Android compile: `uiKey` moved before trailing lambdas (item 39).

## Batch 8 (2026-10-15): PR #7723 follow-ups
- Rebased on master `000258224` (one `UserPicker.kt` conflict with #7679); desktop 140/141 (ForwardTest flake, plan 15.4.1b), Android 126/127 + 1 skipped.
- Waydroid tool moved into the repo and made portable (`apps/multiplatform/scripts/waydroid/`, plan 15.4 item 42); the Android runner uses it.
- Plans, catalogue, review reports and test drafts (`plans/ui-tests/drafts/`, batch 3 unmerged) are in the PR.
