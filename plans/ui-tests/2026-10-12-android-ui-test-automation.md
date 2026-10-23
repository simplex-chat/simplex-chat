# Android UI test automation on this host

**Date:** 2026-10-12. **Status (2026-10-14):** the instrumented harness (layer (a), 3.1) is built and running in `~/wt-uitest`: 127 Android tests (112 shared with desktop + 14 `BackKeyTest` + `SmokeTest`); what is built is described in the desktop plan, section 0.5 (`plans/ui-tests/2026-10-10-desktop-ui-test-dsl-plan.md`), and run findings are in §8. Sections 0–7 below are the 2026-10-12 investigation, with notes where the build differs.
Running log: `plans/ui-tests/2026-10-12-android-research-progress.md`. Evidence: `~/android-uitest-research/evidence/`. Tool: `apps/multiplatform/scripts/waydroid/` (`wd.sh`, `README.md`, `provision2.sh` for instance 2; state in `WD_HOME`, default `~/waydroid`). It runs on Qubes AppVMs and on other Linux hosts (2026-10-15).

## 0. Summary

- **Runtime: Waydroid works, the emulator does not.** On this KVM-less qube, Waydroid (LXC, LineageOS 20 /
  Android 13 x86_64, libndk arm64 translation) runs the real arm64-only SimpleX debug APK headless:
  - Android boots in 37–56 s; the 266 MB APK installs in 5–8 s; the app cold-starts in 2.2–2.6 s.
  - The Haskell core works under translation. After `pm clear`, the first database creation takes ~45 s (measured later inside the instrumented harness: the app's startup took 15.4 s, plan 15.2).
  - The emulator only runs x86_64 AVDs under TCG with no ARM translation (x27 boots in 149 s). The real app can't run on it.
- **Proof:** a clean-environment script installed, reset and launched the app, took screenshots, dumped the UI
  hierarchy, tapped through onboarding, typed a name and created a profile, all with rc=0
  (`evidence/wd-e2e/30-e2e.log`, PNGs `10-`, `11-`, `12-*.png`).
- **Networking:** Android reaches host servers on the bridge IP (192.168.240.1), and loopback-only host servers
  through `adb reverse`. Both still work with the internet blocked for the container (`wd.sh net isolate`).
- **Automation layer, recommended: instrumented Compose tests** (`createEmptyComposeRule()` + `ActivityScenario.launch(MainActivity)` after the reset, `05-android-only.md` setup item 4; run with
  `am instrument`) behind the desktop plan's `UiDriver` (as built: behind per-platform `UiScope`s with the same methods, no `UiDriver`). Test bodies, screen objects and `Ui` keys are shared, and
  only the driver and the peer transport differ. CLI peers and `uitest-servers` stay on the host and are reached
  through `adb reverse`.
  - A host-driven layer (uiautomator/adb, Maestro, Appium) works today with no app change, which `wd.sh` proves.
    It suits LLM-guided exploratory testing, but it can't see typed semantics, has no idling, and costs 2–3 s per observation.
- **Built (2026-10-13/14):**
  - the `androidTest` source set: `testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"`; added `androidx.test:runner` 1.6.1, `androidx.test:core` 1.6.1, `espresso-intents` 3.6.1 and `ui-test-junit4` 1.8.2 (with the existing `ext:junit` and `espresso-core`); `common/src/uiTestShared/kotlin` added as an `androidTest` source dir;
  - no shared `UiDriver`: each platform has its own `UiScope` with the same method names, and the shared screens and test bodies compile against either;
  - the host peer bridge (§5.1, as built);
  - the runner `scripts/uitest-android.sh` (§5.1, as built).
  - Not built: `testTagsAsResourceId` and `ui-test-manifest` (not needed by the instrumented layer).
  - An x86_64 APK is **not needed** and would be expensive: no x86_64-android GHC cross target exists.

## 1. Host facts

- Qubes AppVM `claude-8tb`: Debian 12, kernel 6.18.51 (qubes), Intel i7-10700, 8 vCPU, 12 GB RAM, no swap,
  **no `/dev/kvm`**, no `/dev/dri`. The root FS is non-persistent (25 GB, reset on qube restart); /home is 1.8 TB.
- The kernel has `CONFIG_ANDROID_BINDER_IPC=y` and `CONFIG_ANDROID_BINDERFS=y`, so Waydroid needs no kernel modules.
- Before today Waydroid was not installed on this qube. The earlier Waydroid use (PR #7570 attach sheet) was in
  another qube; its logcat is `~/QubesIncoming/waydroid/attach-full.txt`, and it showed the same SwiftShader/gralloc stack.

## 2. Runtime: Waydroid vs emulator

| | Waydroid (chosen) | Android emulator |
|---|---|---|
| Real SimpleX APK (arm64-v8a only) | **runs** (libndk translation, `ro.product.cpu.abilist=x86_64,x86,arm64-v8a,…`) | **no**: x86_64 AVDs have no native bridge (`ro.dalvik.vm.native.bridge=0`, `evidence/emulator-x27/boot.txt`); arm64 AVDs can't run without KVM ("QEMU2 emulator does not support arm64") |
| Boot | 37–56 s warm, ~75 s first boot | x27: 149 s (TCG); x30 not tried (its image has no native bridge either); the API 36.1 image needs KVM (earlier note) |
| Install 266 MB APK | 5–8 s | >16 min, never finished (killed) |
| App cold start | 2.2–2.6 s (`am start -W`) | n/a |
| Headless | yes: weston headless backend + pixman, `waydroid show-full-ui` | yes (`-no-window`) |
| Screenshot | `weston-screenshooter` (weston `--debug`); `adb screencap` **fails** (empty file) | `screencap` works |
| UI dump / input | `uiautomator dump` 2–3 s; `input tap/text/keyevent` | same |
| Host servers | bridge IP 192.168.240.1 or `adb reverse` (both verified) | `10.0.2.2` or `adb reverse` |
| Internet isolation | `wd.sh net isolate` (nft forward drop); host still reachable (verified) | not examined |
| Multiple instances | **two** since 2026-10-14 (instance 1 shared, instance 2 for UI test runs; `~/waydroid2`, plan 15.4 item 15a); runs are serial within an instance | several AVDs, each slow |
| RAM | app process ~680 MB RSS / 490 MB PSS (`dumpsys meminfo`); whole container not measured | 2 GB guest RAM (AVD config) + QEMU |

Gotchas found and solved, all handled in `wd.sh`:
1. **Qubes firewall:** the `ip qubes input` chain has policy drop, so the container gets no DHCP lease or IPv4.
   Fix: `nft add rule ip qubes custom-input iifname "waydroid0" accept`.
2. **Container freeze:** Waydroid freezes the container when Android idles, and an `adb install` then "hung" for
   11 min. Fix: `waydroid prop set persist.waydroid.suspend false`, plus stay-awake settings and an unfreeze before
   every device command.
3. **No window:** `waydroid session start` creates no window, so screenshots were black. `waydroid show-full-ui`
   creates one, but weston **kiosk-shell never maps it** (empty scene graph, even though the client attached
   720x1440 shm buffers). Fix: weston desktop-shell with `panel-position=none`.
4. **Stale rootfs mount:** a quick down→up left rootfs mounted, and the next session failed on "umount rootfs".
   `down` and `up` now unmount it.
5. **Lock fd inheritance:** daemons started while `wd.sh` held its flock inherited the fd and blocked every later
   command. Fix: `{LFD}>&-`.
6. **Slow download:** `waydroid init` downloads serially at ~0.4 MB/s here. Replaced by a parallel, resumable range
   download (`scripts/waydroid/fetch-images.sh`) and init from local images (`/etc/waydroid-extra/images`).
   A corrupted part, caused by two downloaders appending to the same file, was repaired block by block (`repair-zip.py`).

## 3. Automation layer

### 3.1 Options evaluated

| Layer | Works here? | Finds nodes by | Sync | Sees typed semantics / chatModel | Speed | App change |
|---|---|---|---|---|---|---|
| (a) Instrumented: `createEmptyComposeRule()` + `ActivityScenario.launch(MainActivity)` after the reset (`05-android-only.md` setup item 4) + Compose test APIs, `adb shell am instrument` | yes (built 2026-10-13; §8) | `testTag` (= `Ui` keys) directly, text, any `SemanticsMatcher` | Compose idling + the plan's polling (§6) | **yes** (same process) | ms per query | androidTest set-up only (keys from the desktop plan apply as they are in commonMain) |
| (b) Host-driven: `uiautomator dump` + `adb shell input` (what `wd.sh` does) | **yes, proven** | text / content-desc / bounds; `resource-id` only with `testTagsAsResourceId` | polling only | no (accessibility tree only) | 2–3 s per dump | none today; `testTagsAsResourceId` for keys |
| (c) Maestro / Appium (UiAutomator2 server) | likely (both just need adb), not tried | same accessibility tree as (b) | built-in waits | no | ~0.3–1 s per query (typical; not measured here) | same as (b) |

Evidence for (b):
- Compose nodes appear as `android.view.View`. The clickable node has an empty text and no resource-id, and the
  text sits on a non-clickable child `TextView` (`evidence/waydroid/03-onboarding.xml`).
- `wd.sh nodes` resolves this by tapping the nearest clickable ancestor.
- `testTagsAsResourceId` exists in the app's Compose (`androidx.compose.ui:ui-android:1.8.2`,
  `SemanticsProperties_androidKt.setTestTagsAsResourceId`). One `Modifier.semantics { testTagsAsResourceId = true }`
  on the root makes every `Ui` key visible as `resource-id` to (b) and (c).

### 3.2 Reset, peers and servers

- **Reset:**
  - `pm clear chat.simplex.app.debug` takes 1 s, then the first launch takes ~45 s to create the database under
    translation. This is too slow to run per test.
  - Instrumented tests should reuse the desktop plan's in-process `resetForTest` (stop the core, delete the
    database, start the core): sub-second apart from the core's own work.
  - Or use AndroidX Test Orchestrator `clearPackageData` for full isolation at ~45 s per test.
  - `run-as` is blocked on this Waydroid image (plan 15.4 item 13; an earlier note here said it works). Fixture database copies (plan `FIXTURE`) need another route; open.
- **CLI peers:** the CLI is a glibc x86_64 binary, so it runs on the host, not in Android. It reaches the same
  servers as the app.
- **Servers:** `uitest-servers` on the host. The app reaches them through `adb reverse tcp:<p> tcp:<p>` per port, so
  the **same server address** (`smp://…@localhost:<p>`) works for both the app and the host peers. The alternative
  is binding them on `192.168.240.1`.
- **Isolation:** the desktop plan's loopback-only netns can't contain the container. The equivalent is
  `wd.sh net isolate`: the container reaches only the host. (As built, the runner does not apply it yet; §6 item 3.c.) Host peers and servers can still run in a netns if the
  adb server and their ports are exposed to it; the simplest form runs them on the host with `-s` pointing only at
  local servers.

### 3.3 Not verified (needs a build) — resolved 2026-10-13

Resolved: the runner class is fixed, `ui-test-junit4` 1.8.2 is added, and the instrumented tests run on Waydroid instance 2 (`SmokeTest` first, plan 15.2). The original text:


- No instrumentation APK was built or run (builds were out of scope). The current
  `android/build.gradle.kts` has `testInstrumentationRunner = "android.support.test.runner.AndroidJUnitRunner"`
  (stale support-library class; must be `androidx.test.runner.AndroidJUnitRunner`), and `ui-test-junit4` is
  commented out (it is not in the Gradle cache either).
- Risk is low: the test APK is dex only and runs inside the app's (already translated) process.
- The first check after a build is `am instrument -w chat.simplex.app.debug.test/androidx.test.runner.AndroidJUnitRunner`
  with one trivial `createEmptyComposeRule()` + `ActivityScenario.launch(MainActivity)` test on Waydroid.

## 4. Proof run (evidence)

`~/android-uitest-research/scripts/30-wd-e2e.sh`, run under `env -i HOME=/home/user PATH=/usr/bin:/bin:/usr/sbin:/sbin`.
Log: `evidence/wd-e2e/30-e2e.log`.

| Step | Result |
|---|---|
| `wd.sh down` / `up` | 2 s / 54 s (boot), serial `192.168.240.112:5555` |
| `install ~/android-arm64-v8a-debug-master-7c2c750.apk` | Success, 8 s (7.1-beta.6, `chat.simplex.app.debug`) |
| `reset-app` (pm clear), `launch` | 1 s, cold start 2.2 s; onboarding after ~45 s (first database) |
| `shot` | `evidence/wd-e2e/10-onboarding.png` (SimpleX "Be free in your network", "Get started") |
| `dump`, `nodes` | `10-onboarding.xml`; "Get started" tap point 360,1284 (= pixel position in the PNG) |
| `tap-text "Get started"` → `tap-text "Enter profile name…"` → `text "alice uitest"` | `11-profile-typed.png` (name in field, IME up) |
| `tap-text "Create profile"` | `12-after-create.png`: "Your network / Setup routers / Continue" (core created the user under translation) |

Also:
- `evidence/waydroid/*.xml`: the first manual run.
- `evidence/net/*.txt`: network probes, open and isolated.
- `evidence/wd-e2e/40-claim-net.txt`: lock/claim and isolate tests.
- `evidence/wd-e2e/50-up-after-simulated-restart.txt`: `up` re-running setup after the root-FS state was removed.

## 5. Same tests as desktop?

**Short answer: yes for test bodies, screen objects and `Ui` keys, if the Android runner is the instrumented
layer (5.1). The host-driven layer (5.2) can share the DSL's surface but not its assertions.**

### 5.1 Instrumented tests behind `UiDriver` (recommended)

- (As built: no shared `UiDriver`; each platform has a `UiScope` with the same methods, and the shared directory is added to `:android`'s `androidTest`, not `androidInstrumentedTest`. The message field is set with Espresso `replaceText` with retries, no `setSelection`, and read with a `ViewAssertion`; its hint (placeholder) is not read. Plan 0.5.4.)
- **Shared code.** The plan's `UiDriver` (§7.4) wraps Compose test APIs that exist in both runners:
  `onNode(hasTestTag(key.tag))`, `performClick`, `performTextInput`, semantics reads, `waitUntil`. On desktop the
  receiver is `SkikoComposeUiTest` (`runInternalSkikoComposeUiTest`, plan §7.2/§14); on Android it is the
  `ComposeTestRule` from `createEmptyComposeRule()`. Both implement `SemanticsNodeInteractionsProvider`.
  - Put the driver, the screen objects (§9.1) and the DSL (§9.2) in a source set compiled for both, e.g. a
    `uiTestShared` directory added to both `desktopTest` and `androidInstrumentedTest`.
  - The test classes (`OneTimeLinkCompatTest`, …) then run unchanged on both, except the tests `05-android-carryover.md` lists.
  - `Ui` keys and typed semantics (§3–4) are in commonMain, so the app code needs nothing Android-specific.
- **Per-platform parts:**
  - `escape()`: desktop invokes `simplexWindowState.backstack` (desktopMain, `D/DesktopApp.kt:32,272`) inside `runOnUiThread` (plan §7.4); on Android it is the system Back from the test thread (`Espresso.pressBack()`, 05 `BACK`, carry-over R9), not wrapped, and the IME and `MainActivity.onBackPressed` also act on it (05 §13);
  - input threading: the desktop driver wraps input injection and the failure capture in `runOnUiThread` (plan §7.4, §6); on Android it must not — `AndroidInputDispatcher.flush` already injects on the main thread (ui-test 1.8.x, AndroidInputDispatcher.android.kt:547-565), and a perform, lookup or capture inside `runOnUiThread` throws "cannot be run from the main thread" (`EspressoLink.runUntilIdle`, EspressoLink.android.kt:75-83) — so the wrapper is a per-platform function that does nothing on Android;
  - the test dispatcher: plan §14 chooses `StandardTestDispatcher` because the default unconfined one runs `snapshotFlow` collectors inside measure; the Android P1 checks what `createEmptyComposeRule` uses and whether a `TestDispatcher` can be passed (e.g. in `effectContext`), and if it can't, the 04 `console_*` rows take the plan's fallback on Android (read `chatModel.terminalItems`, scroll to the newest line);
  - the compose field is an `EditText` inside `AndroidView`, so `performTextInput` can't reach it: typing goes through Espresso `replaceText` plus `setSelection`, reads by getting `EditText.text`/`.hint` in an Espresso `ViewAction` behind `ChatScreen.messageText`/`placeholder` (`05-android-only.md` setup item 5, `05-android-carryover.md` R12);
  - boot/reset (a once-per-process boot step first waits for and stops the core that `SimplexApp.onCreate` started, and sets toolbar blur off in the baseline, as on desktop, `05-android-only.md` setup item 4; `resetForTest` calls the same commonMain core functions; it runs before `MainActivity` is launched with `ActivityScenario.launch`, under `createEmptyComposeRule()`, so the reset's prefs and UI-state clearing are in place before `onCreate`, `05-android-only.md` setup item 4);
  - surfaces outside the scene (§7.6). File choosers become Espresso-Intents stubs instead of `UiFiles`;
    `LocalUriHandler` changes too: it is Compose's `AndroidUriHandler`, which calls `startActivity` with `ACTION_VIEW`,
    so links are recorded and stubbed with Espresso-Intents as well (`uitest-catalog/05-android-carryover.md` rule R13,
    which also covers the clipboard); notifications are read in-process with `NotificationManager.getActiveNotifications()`
    (`SYSNTF` in `05-android-only.md`).
  - Keyboard shortcuts and right-click/hover are desktop-only (§5.3).
- **Peers and servers from inside the device.** The test JVM runs inside Android, so `CliPeer` (pty4j + VT model)
  can't spawn the CLI there: it is a glibc x86_64 binary. Split `CliPeer` behind an interface:
  - On desktop it stays in-process.
  - On Android a host process, the **peer bridge**, owns the real `CliPeer`s, `RelayPeer`s and `uitest-servers`.
    It is a small Kotlin/JVM main in the test tree that reuses the desktop `CliPeer` code.
  - **As built** (`UiBridge.kt` in the desktop harness; client `Bridge` in the Android `CliPeer.kt`): a line protocol on
    `127.0.0.1:7777`, one client at a time; the Android side uses a fixed port 7777 and a 60 s read timeout.
    - `servers` → `ok <smp1> <xftp1>` (`xftp1` is configured but not served yet, plan 0.5.6);
    - `start <name> <flags, comma-separated: reactions, markRead, fullLinks>` → `ok`, sent only after the peer has started
      (synchronous; the peer's database is copied from the peer template of the run directory named in `env.properties`, when present);
    - `type <name> <text>` (verified typing, host-side) → `ok`;
    - `drain <name>` → `lines N`, then the N new output lines;
    - `stop <name>` → `ok`; any failure → `err <message>`; when the client disconnects, the bridge stops all its peers.
  - Comparison (`sends` echo, `expects`, `eventually`, `contactId`) runs **device-side** on the drained lines, in the
    shared `PeerLines` class, so `bob expects "…"` has the same semantics as on desktop.
  - The test connects through `adb reverse tcp:7777 tcp:7777`, and the server ports (base + 1, base + 2) are reversed as well.
  - Proposed earlier and not built: `sends`/`contactId`/`expect … <timeoutMs>` as bridge commands, `servers reset`, `snapshot`.
- **Runner.** (The package name `chat.simplex.app.debug` used throughout assumes `application_id.suffix=.debug` in `local.properties`, as `local.properties.example` sets it; root `build.gradle.kts:23-25`.)
  - **As built:** `scripts/uitest-android.sh <app.apk> <androidTest.apk> [am instrument -e …]`, with APKs from
    `ANDROID_TEST=1 bash ~/build/android.sh`. It needs one earlier `scripts/uitest.sh` run (CLI, `uitest-servers`,
    `env.properties`) and 4500 MB free.
  - Environment: `UITEST_SERIAL` (any adb device; skips Waydroid), `WD_INSTANCE` (default 2), `WD_OWNER`
    (default `uitest-$USER`), `UITEST_APP_ID` (default `chat.simplex.app.debug`), `ADB`, `JAVA_HOME`.
  - It claims and boots Waydroid in the background, starts the bridge with `java -cp`, installs the APKs (the app APK
    only when changed), sets the `adb reverse` ports, and per pass runs `pm clear`, grants permissions and
    `am instrument -w -r`; after a process crash it resumes with `-e notClass` (at most 4 passes).
  - Results are not JUnit XML: `build/uitest-android/run-<timestamp>/results.txt` (`<class>#<test> <code>`: 0 pass,
    −2 fail, −3/−4 skipped, `crashed`), plus `instrument*.txt`, `logcat.txt`, `timings.txt`, `failures/`, `bridge.log`.
    The exit status is non-zero on any failure or crash. Details: plan 0.5.9.
  - It does **not** run `wd.sh net isolate` (open item, §6 item 3.c).
  - Considered and not used: `./gradlew :android:connectedFossDebugAndroidTest` (the app has `google`/`foss` flavors,
    foss the default, `android/build.gradle.kts:40-50`).
- **Throughput limits.**
  - Runs are serial within a Waydroid instance: no parallel JVMs as on desktop (§10.2). UI test runs use instance 2; instance 1 is shared with other sessions.
  - In-process reset avoids the 45 s first-database cost per test.
  - The core runs under arm translation; it measured fine for onboarding, but no figure exists yet for a full Android run (the 1,094 carried-over tests of 01–04, 18 of them `IGNORE`, plus the runnable tests of `05-android-only.md`).

### 5.2 Host-driven over adb/UiAutomator(2) with `testTagsAsResourceId`

- **Reach.**
  - The same DSL surface can run in a host JVM, with peers local exactly as on desktop.
  - The driver maps `Ui.X.Y` → `resource-id` (with `testTagsAsResourceId`), text → `text`, and click → `input tap`
    at the node centre. `wd.sh` already does this from the shell.
- **What is lost.**
  - Typed semantics (§4: unread count, item meta/status, sender, verification, incognito, muted) are custom
    `SemanticsPropertyKey`s that are not exported to accessibility. Assertions on them would need the app to mirror
    them into `stateDescription`/`contentDescription`, which changes what TalkBack reads.
  - Name→id resolution through `chatModel` (§9.1 `row("bob")`) is lost; the alternative is text matching or
    `/_get` via a peer.
  - No Compose idling; every observation is a 2–3 s `uiautomator dump`, or less with a resident UiAutomator2 server (not measured here).
  - Off-screen lazy-list items aren't in the tree, so scrolling is needed.
- **Fit.** Good for smoke tests and LLM-guided exploratory testing (`scripts/waydroid/README.md` "How an agent should
  drive the GUI"). Not a substitute for the catalogue's assertions.

### 5.3 Catalogue carry-over (1,177 names; counts from `plans/ui-tests/catalogue/tools/needs.py`)

| Group | Names | On Android |
|---|---|---|
| Rows needing only peers, servers, restart, fixtures, clipboard, toasts, URI (`P` 763, `—` 241, `RELAY` 90, `XFTP` 72, `RESTART` 48, `FIXTURE` 38, `CB` 90, …; overlapping) | ~1,050 | carry over through 5.1 with the peer bridge; `RESTART` rows only once `restart()` has an Android mechanism (`05-android-only.md` open question 14), `FIXTURE` rows only once fixtures have one (open question 16). Assertions on Android-specific layouts (one-pane navigation instead of the desktop's side-by-side list+chat, ChatListView.kt:612) need different screen-object steps, but test bodies stay the same if screen objects own navigation |
| `FD` file-dialog rows | 42 | port with an Espresso-Intents picker stub instead of `UiFiles` |
| `REC`/`AV` seams | 19 | port only once the §11.2 seams have Android actuals (MediaRecorder/ExoPlayer) |
| 04 §K Remote desktop "Link a mobile" (desktop is the controller) | 17 | **desktop-only**. Android is the *host* side ("Use from desktop"), a different test set with a CLI controller |
| 02 §W right-click and Enter rows, `IMGCB` Ctrl+V | 3 | desktop-only; the other §W rows run with long press or Back (`05-android-carryover.md`) |
| 04 §C SimpleX Lock (desktop passcode) | 22 | partially: Android has system-auth/biometric lock modes in addition to the passcode; 20 rows carry over with "Lock mode" set to "Passcode" first (map R8), and the relock and notice rows are replaced by `05-android-only.md` §10 |
| 04 §D Appearance with `FD` (wallpaper image, theme export/import) | 6 | via picker stub |
| `IGNORE` rows (window chrome, tray, OS drag-and-drop, WebRTC, public servers, `chatHooks`, …) | 28 | still ignored; window/tray/drag-and-drop ones are N/A on Android |

**Android-only areas the catalogue doesn't have:**
- notification modes and the onboarding "Setup notifications" stage (periodic/instant service, battery-optimisation prompts);
- runtime permission prompts (POST_NOTIFICATIONS, camera, microphone);
- share-into-app intents and app-link opening (`am start -a VIEW -d https://simplex.chat/…`);
- back gesture / predictive back and IME-overlap layouts (e.g. PR #7570);
- rotation and split-screen (PR #7497);
- calls in the Android WebView;
- "Use from desktop" (host side);
- Waydroid limits: no camera, so QR scanning is out of reach; the MediaProvider NPE blocks file-send from the
  system picker, so stub the picker in tests.

## 6. Recommendation

1. **Runtime:** Waydroid via `apps/multiplatform/scripts/waydroid/wd.sh` (setup steps in its `README.md`; on Qubes it
   re-provisions itself after qube restarts from `$WD_HOME`, default `~/waydroid`). Use the emulator only for framework-only questions with stub APKs, as before.
2. **Now, no app change:** LLM-guided and smoke GUI testing with `wd.sh nodes/shot/tap-text/text` on the existing
   debug APKs. This is already usable and is shared between sessions with `claim`/`WD_OWNER`.
3. **Desktop plan alignment (after its P1 keys land):** (status 2026-10-14: a, b, c and d are done, except the items marked below; see §0 "Built" and plan 0.5)
   - **a.** Android build set-up (done, except `ui-test-manifest` and `testTagsAsResourceId`, not built):
     - fix `testInstrumentationRunner` to `androidx.test.runner.AndroidJUnitRunner`;
     - add `androidTestImplementation("androidx.compose.ui:ui-test-junit4:<compose 1.8.2>")` and
       `debugImplementation("androidx.compose.ui:ui-test-manifest:…")`;
     - optionally add `testTagsAsResourceId = true` on the root in debug builds, for (b)/(c) and the LLM tool.
   - **b.** Move `UiDriver`, the screen objects and the DSL into a source directory shared by `desktopTest` and
     `androidInstrumentedTest`; keep boot, reset and surfaces per platform. (Done as `common/src/uiTestShared`, shared by
     `desktopTest` and `androidTest`, without a `UiDriver` interface.)
   - **c.** Peer bridge (host JVM main reusing `CliPeer`/`RelayPeer`/`TestServers`) plus a runner script:
     `wd.sh up` → `wd.sh net isolate` → start bridge + servers → `adb reverse` ports → install app + test APK →
     `am instrument` → collect JUnit XML. (Done with `CliPeer` and `TestServers`, no relays; results go to `results.txt`,
     not JUnit XML. **The runner does not call `wd.sh net isolate`** (open item): the container keeps internet access
     during runs, and isolation rests on the local server configuration and on onboarding opening no connection.)
   - **d.** Start with the 3 example tests of plan §9.3 on Android; then measure per-test time under translation
     before planning catalogue-wide runs. (Done: §8.)
4. **x86_64 APK: not worth it now.**
   - `flake.nix` has only `aarch64-android` and `armv7a-android` cross targets, and nix is not installed here.
   - `~/build/android.sh` packages prebuilt arm64 `libsimplex.so`/`libsupport.so` only.
   - An x86_64 build would need a new haskell.nix `x86_64-android` cross toolchain (GHC, plus openssl, libffi,
     gmp and iconv for x86_64-android), a CI job, and `x86_64` in the Gradle `splits`/`abiFilters`.
   - That is days of toolchain work for a speed-up translation doesn't need: onboarding is already responsive.
   - Revisit only if translation proves too slow for catalogue-scale runs.

**Costs/risks:**
- runs are serial within an instance (two instances since 2026-10-14; UI test runs use instance 2);
- arm translation is fast enough for onboarding, but bulk message and file tests are not measured yet;
- Waydroid quirks (freeze, window mapping, stale mounts) are handled in `wd.sh`, though new ones may appear;
- the root FS is reset on restart, so the offline deb reinstall path is only simulated, not tested from a truly clean root;
- Android 13 only (one OS version);
- no camera.

## 7. Decisions and open questions

Decided (user, 2026-10-12):
- Test tags are for testing only, so they exist only in debug builds: `testTagsAsResourceId = true` is set on the root in debug builds only (decided, not built: the instrumented layer doesn't need it), and the shared `Modifier.ui` emits tags only when `uiTestHooksEnabled` (formerly `uiTagsEnabled`) is set: by `initUiTags(context)` from `SimplexApp` when the app is debuggable, and by the desktop harness (desktop plan §3.1, 0.5.2). Release builds carry no tags. Typed semantics are deferred (plan 13, minimal-app-code rule of 2026-10-14).
- Android-only UI gets its own catalogue, `plans/ui-tests/catalogue/05-android-only.md`.

Decided (user, 2026-10-13; recommendations in plan section 15): restart is in-process (05 question 14; built, plan 15.4 item 10); the debug-only network-security-config is approved (15; not built yet); the `FIXTURE` proposal is approved (16; not built, and `run-as` is blocked); Android screenshots are deferred (17); the toast hook is added only if the on-device `UiAutomation` check fails (13). Android work started on 2026-10-13, after desktop P1a.

Open:

1. ~~Instrumented runner: Gradle `connectedFossDebugAndroidTest` against the Waydroid adb device, or a plain
   `am instrument` runner script?~~ Resolved: the script, `scripts/uitest-android.sh` (§5.1).
2. ~~Is in-process `resetForTest` enough isolation on Android, or do some areas need Orchestrator `clearPackageData`?~~
   Answered in §8: in-process reset is enough so far; notification and service areas are not tested yet.
3. libndk vs libhoudini: only libndk was tested. Houdini is Intel's and often faster on Intel CPUs; worth one A/B
   run of a message-heavy test later.

## 8. Findings from running the harness (2026-10-14)

The full list, with causes and fixes, is in plan §15.4 (`plans/ui-tests/2026-10-10-desktop-ui-test-dsl-plan.md`). The ones that change this write-up:
- **Open question 2 (in-process reset):** the reset isolates tests, but the translated core sometimes crashes (SIGSEGV) or hangs in `chatMigrateInit` when the database is re-created. Mitigations:
  - the template database comes from the app's own startup database, so later resets run no migrations;
  - a 5 min per-test watchdog;
  - runner resume after a crash.

  Orchestrator `clearPackageData` is still not needed.
- **Speed, measured:** the reset went from 31.6 s to about 4 s; closing the activity from 45 s to 0.4 s, by releasing the Espresso-Intents stub first; bridge startup from 1–7 min to 1 s, by running it without Gradle. Waydroid now boots alongside the bridge start, so run setup takes 33 s with an unchanged app APK. A simple test now takes 15–20 s, against 65–90 s before. In the last full run (`run-20261014-164429`, 53/53, 16.5 min of instrumentation, about 19 s per test on average) the `reset` phase took 5.1 s median (3.7–7.7 s, n = 53, from logcat), because the template was still encrypted ("database is locked" on decryption; the `apiStopChat` fix is not yet run, plan 15.4 item 25). Final run 2026-10-14 (`run-20261014-214038`, 127 tests, 126 passed, 1 skipped, 18.5 min, about 8.8 s per test): the template is now decrypted, and the `reset` phase took 585 ms median (64 ms–3.2 s, n = 126).
- **Compose test clock:** `poll` must advance `mainClock`, because effect `delay()`s (mark-read 600 ms, alias 500 ms) run on the test clock.
- **Root actions:** `adb root` restarts `adbd` and kills a running `am instrument`. Use `wd.sh rootsh`, for example `kill -3 <pid>` for thread dumps; `run-as` is blocked.

