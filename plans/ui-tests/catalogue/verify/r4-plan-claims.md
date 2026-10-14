**Not CLEAN.** I found 15 wrong claims. Only the first one would actually stop a run. Five more would lead an implementer to the wrong code or to tests that can't be written as specified. The rest are small corrections. All checks were made against `/home/user/wt-uitest-ref` (d72f68e6d), with simplexmq at ee66dd68 from `~/repos/simplexmq`. I re-checked the main findings myself.

1. **Gradle cache locks block the inside run (plan line 323).**
   - Quote: "`./gradlew --stop` (an outside daemon can't be asked to release `~/.gradle` locks from inside), then `./gradlew :desktop:cmakeBuildAndCopy :common:desktopTestClasses :common:uitestPrepare`"
   - Actual: the daemon is on by default (`apps/multiplatform/gradle.properties` has no `org.gradle.daemon=false`, and there is no `~/.gradle/gradle.properties`). The second command starts a new daemon, which stays idle and keeps holding the `~/.gradle` cache locks. As the plan itself says, that daemon can't be reached from inside the namespace.
   - Failure: the `--offline` pass inside fails with "Timeout waiting to lock … in use by another Gradle instance".
   - Fix: "`./gradlew --no-daemon :desktop:cmakeBuildAndCopy :common:desktopTestClasses :common:uitestPrepare`, then `./gradlew --stop` (an outside daemon can't be asked to release `~/.gradle` locks from inside, so none may be left running)"

2. **Drag and drop can't be driven through `onFilesAttached` (line 728).**
   - Quote: "Drag and drop is driven by calling the compose state's `onFilesAttached` from the test (no app change)."
   - Actual: `onFilesAttached` is an extension on `MutableState<ComposeState>` (ComposeView.kt:290). The only such state is a local in ChatView: `val composeState = rememberSaveable(...)` (ChatView.kt:148). No singleton holds it. The drop arrives through `desktopOnExternalDrag` (ChatView.kt:979-987).
   - Failure: the test has nothing to call it on, so the dropped-.webm (AV) rows can't be written.
   - Fix: "Dropped files are attached through the same `onFilesAttached` path by a Ctrl+V of a file list from the in-memory clipboard (PlatformTextField.desktop.kt:144-145 → ComposeView.kt:1516); the drop target itself (`desktopOnExternalDrag`, ChatView.kt:979-987) is not exercised. No app change."

3. **`openFile` has no callers (lines 446, 735, 774).**
   - Quote: "`openFile`, D/platform/Share.desktop.kt:49, called e.g. at CIImageView.kt:154"
   - Actual: `openFile` appears only in its declarations (Share.kt:11, Share.desktop.kt:46, Share.android.kt:116). CIImageView.kt:154 is `imageView(previewBitmap, onClick = onClick)`. The only OS-open path the UI uses is `desktopOpenDir`, via `desktopOpenDatabaseDir` (Files.desktop.kt:33, DatabaseView.kt:243, DatabaseErrorView.kt:330).
   - Failure: the open question proposes a seam for dead code, and any catalogue row IGNOREd for `openFile` describes a feature the app doesn't have.
   - Fix for line 774: "Opening a folder in the OS (`desktopOpenDir`, D/platform/Files.desktop.kt:37-43, reached from the database-folder button via `desktopOpenDatabaseDir`, Files.desktop.kt:33, DatabaseView.kt:243, DatabaseErrorView.kt:330) can't be observed headless. `openFile` (Share.desktop.kt:46) has no callers. A fourth seam (an `internal var` folder opener) would make those rows testable; until it is approved they stay `IGNORE`."
   - Lines 446 and 735 should name only `desktopOpenDir`, and the catalogue rows ignored for `openFile` need re-checking.

4. **Peer received files don't stay in the peer's directory (line 471).**
   - Quote: "received files go to `$HOME/Downloads`, Files.hs:31"
   - Actual: Files.hs:30-31 is `dir <- (`combine` "Downloads") <$> getHomeDirectory; ifM (doesDirectoryExist dir) (pure dir) getChatTempDirectory`.
   - Failure: a fresh `HOME` has no `Downloads`, so every peer writes received files to the shared system temp directory.
   - Fix: "`HOME=<jvmDir>/peers/<name>` with `$HOME/Downloads` created before start (received files go there only if it exists, otherwise to the system temp directory, Files.hs:29-31; …)"

5. **`-s` turns the names role on after a restart (line 480).**
   - Quote: "the names server role, which `-s` doesn't enable (Operators.hs:189-193)"
   - Actual: that holds only on first start. On later starts, Chat.hs:302-304 replaces each user's servers with `presetServerCfg True allRoles Nothing`, and `allRoles = ServerRoles True True True` (simplexmq Agent/Env/SQLite.hs:117).
   - Failure: after a PEERCTL `stop()/start()` or a relay `-e` restart, a peer in a non-NAMES test can resolve names.
   - Fix: "…which `-s` doesn't enable on the first start (Operators.hs:186-190); on later starts `-s` overrides the servers with all roles, names included (Chat.hs:302-304)…"

6. **TTL command names are swapped against their line numbers (line 712).**
   - Quote: "(`APISetChatItemTTL`/`APISetChatTTL`, Library/Commands.hs:1853/1891)"
   - Actual: `APISetChatTTL` is at Commands.hs:1842 and `APISetChatItemTTL` at :1884.
   - Fix: "(`APISetChatTTL`/`APISetChatItemTTL`, Library/Commands.hs:1842/1884)"

7. **Names.hs:39 doesn't type a command (line 480).**
   - Quote: "the harness types `/_servers 1 <json>` for them, as tests/ChatTests/Names.hs:39 does"
   - Actual: Names.hs:39 is `mapM_ enableNamesRole [alice, bob]`. That helper (ChatClient.hs:488-505) calls the API in-process. The typed approach still works for a pty peer.
   - Fix: "…the harness types `/_servers 1` and then `/_servers 1 <json>` with names enabled, as `enableNamesRole` (tests/ChatClient.hs:488-505, used at Names.hs:39) does in-process."

8. **`BadgesPayView` does have a caller (line 661).**
   - Quote: "composables with no caller (e.g. `BadgesPayView`)"
   - Actual: it is called at BadgesYourLevelView.kt:149. That screen is only opened from `ChooseLevelButton` (BadgesSupportSimplexView.kt:77), which has no caller.
   - Fix: "composables not reachable from the UI (`BadgesYourLevelView` and `BadgesPayView`, behind the uncalled `ChooseLevelButton`, BadgesSupportSimplexView.kt:77)"
   - This also means the keys at §3.4 line 184 (BadgesYourLevelView.kt:125, BadgesPayView.kt:148) can't be tested.

9. **CallSettings.kt:131 is not `SharedPreferenceRadioButton` (line 137).**
   - Actual: :131 is a second `SharedPreferenceToggleWithIcon` overload. Only :155 is `SharedPreferenceRadioButton`, which has no callers.
   - Failure: keying only one overload leaves one call site unkeyed.
   - Fix: "`SharedPreferenceToggle`, `SharedPreferenceToggleWithIcon` (2 overloads) | C/views/usersettings/CallSettings.kt:85/101/131 | 1 / 2 | param on both overloads (`SharedPreferenceRadioButton` :155 has no call sites)"

10. **Wrong site count for `ExposedDropDownSettingWithIcon` (line 139).**
    - Quote: "30 + 5"
    - Actual: it has one call site, CallView.android.kt:361, which is Android-only.
    - Fix: "30 + 1 (Android only, CallView.android.kt:361)"

11. **Two cited notification lines are dead code (line 443).**
    - Quote: "D/model/NtfManager.desktop.kt:123/168-185"
    - Actual: lines 168-185 are only reached from the uncalled private `displayNotification(title, text, icon)` at :162. The real path is :97-112 → `displayNotificationViaLib` :114-148.
    - Fix: "D/model/NtfManager.desktop.kt:97-148 (`displayNotification` → `displayNotificationViaLib`, `Toast.builder()` :123)"

12. **`initChatController` doesn't wait (line 363).**
    - Quote: "waits for ctrlInitInProgress"
    - Actual: Core.kt:73 is `if (chatModel.ctrlInitInProgress.value) return`, so it returns at once and does nothing. `stopAndCloseCore` already waits, so nothing breaks today.
    - Fix: "returns immediately without initialising if an init is in progress (Core.kt:73), so `stopAndCloseCore` must have waited first"

13. **The `chatStopped` restore step in restart does nothing (line 396).**
    - Quote: "raw `apiStopChat` … and restore `chatStopped` to its previous value"
    - Actual: `apiStopChat` (SimpleXAPI.kt:1093-1097) doesn't touch `chatStopped`. It also leaves `chatRunning == true`. That is harmless only because `setChatCtrl(new)` restarts the still-running receiver (SimpleXAPI.kt:550-555).
    - Fix: "raw `apiStopChat` (it leaves `chatStopped` unchanged, unlike `stopChatAsync`, Core.kt:61-62), then set `chatModel.chatRunning` to `false`"

14. **The cabal stanza is missing `StrictData` (lines 497-500).**
    - Actual: the test-suite sets `default-extensions: StrictData` (simplex-chat.cabal:780-781). Without it `ChatClient` still builds, but its records are lazy, unlike in the Haskell tests.
    - Fix: add "`default-extensions: StrictData` (as in the test-suite)".

15. **Two of the cited loops are delay loops, not animations (line 254).**
    - Actual: TextItemView.kt:176-183 and ChatView.kt:1264-1268 are `delay(250)` loops. Only SendMsgView.kt:534-544 is an animation. The conclusion stands either way.
    - Fix: "…nor the finite animations restarted in a loop (SendMsgView.kt:534-544) and the `delay` loops (TextItemView.kt:176-183, the call timer ChatView.kt:1264-1268)"

**Verified correct (sections 3–8 and 11, within ±3 lines):**
- **Keys and helpers (§3):** every helper and entity-key line, the signature claims, and the site counts apart from #9–#10.
- **Semantics and sync (§4–6):** the semantics and ad-hoc site lines, `sendCmd`, `startReceiver`, the `log = false` polls and badge redemption, and the `withBGApi`/`withApi`/`withLongRunningApi` lines. Decompiling `SkikoComposeUiTest` 1.8.2 confirms that `waitForIdle` with `autoAdvance = false` ignores idling resources and animations, and that `setContent` can be called again.
- **Gradle (§7.1):** line 149, the compose 1.8.2 artifacts, the `uiTestJUnit4` accessor, the error-level `ExperimentalTestApi` opt-in, the pty4j 0.13.13 pin (0.14 needs stdlib 2.4), the `chat.simplex.app` exclude being safe, `cmakeBuildAndCopy`, and the `client_library` flag.
- **Directories:** `dataDir`, `configPath` and `tmpDir` come from `user.home`/XDG and `java.io.tmpdir/simplex`.
- **Boot, reset and restart (§7.2/7.3):**
  - Steps match `initHaskell`/`initHS`/`runMigrations`/`ntfManager`/`windowState`.
  - On a fresh DB, `useDatabaseKey` gives a random key, then `startChatWithoutUser` → create user → `startChat`.
  - The `deleteStorageAndRestart` order matches, and `stopChatAsync` sets `chatStopped`.
  - Self-destruct: chatCtrl `-1` gates init at LocalAuthView.kt:36.
  - Prefs: `SharedPreference._state`, lazy `appPrefs`, `themeOverridesStore`, and the four prefs `ChatModel` copies once.
  - Every `clearUiState` target exists.
  - End modals expand to 388+590+388 = 1366 dp, and the test density is 1.
- **Driver and surfaces (§7.4–7.6):** list keys, `keyForItem`, Escape backstack, file-text attach, `DialogState` NPE, directories-only save, Ctrl+V headless throw, call link via `LocalUriHandler`, toasts, recorder/player lines, tray and window lines, `SingleInstance`, and `LocalClipboard` used by foundation 1.8.2.
- **Peers and servers (§8):**
  - All CLI options and modes, the relay address output, the `--relay-web-*` pairing, and all seven peer output lines in §9.3.
  - `minRemoteHostVersion`, the `ChatClient.hs` server configs (ports base+1/+2, quota 16, store config, TLS fingerprint), `USEDuplicateServer` comparing host only, no conditions check in `/_servers`, `NameResolver`, the SMP server's SIGINT handler, dual-stack bind, and the missing `embedDir` source.
  - Remote desktop: `getLocalAddress` and loopback, multicast port 5227, the source-equals-host check, `localhost` not parsed as a link, and the 12 h cutoff with the 30 s/30 min expiry.
- **§11:** all 13 pre-set prefs, the notice and AppLock functions, the fixture files, the 510 KiB auto-accept limit, the seam lines, and the IGNORE reasons (import catches only `Error`, the channel role limits, `CIGISExpired` never set, the commented-out features).

**Not verified:** the pty crash messages, the version the stable CLI prints, the "replayed signature stripped" mechanism, the ~20-minute inactive-member timing, and that the updated-conditions notice appears only for preset operators.