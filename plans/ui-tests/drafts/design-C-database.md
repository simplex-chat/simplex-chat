# Design C: database tests (catalogue 04 §G)

Status: design only. Nothing below has been built or run. All file:line references were read in `~/wt-uitest` on 2026-10-20 (branch `nd/desktop-ui-tests`); paths are relative to `apps/multiplatform/common/src/` unless they start with `android/`, `src/` (Haskell core) or `plans/`. UI strings are from `commonMain/moko-resources/base/strings.xml`: the brief's `resources/MR/base` path does not exist, and the legend's path is the right one.

## 0. Summary and decisions that need approval

Selected: **10 tests**: 7 shared, 2 desktop-only and 1 shared known-issue test. Together they cover 25 of the 35 rows. Of the rest, 7 rows are deferred, 2 stay IGNORE and 1 is dropped. The tests aim at data-loss and lock-out regressions: change passphrase, remove and re-save the passphrase, startup passphrase prompt, stop/start, stopped state across restart, export + import (data, settings, real messaging afterwards), invalid import, delete database, "Delete messages after", and the incompatible-database screen.

The work needs these test enablers. **Each one needs user approval** (diff-scope rule):

| # | Change | Where | Needed by |
|---|---|---|---|
| H1 | File-chooser responders `saveFileAs(file) { action }`, `openFile(file) { action }` and `exchangeDir()` on both `UiScope`s. Desktop completes `simplexWindowState.saveDialog/openDialog` (public `DialogState.isAwaiting`/`onResult`, `desktopMain/.../DesktopApp.kt:294-308`). Android stubs `ACTION_CREATE_DOCUMENT`/`ACTION_GET_CONTENT` with Espresso-Intents, answered with a `FileProvider` URI. No app change. | `desktopTest/.../harness/UiTest.kt`, `android/.../harness/UiTest.kt` | T4, T5 |
| H2 | `restart(ready, what)` parameter on both `UiScope.restart`. The defaults keep today's behaviour. | both `UiTest.kt` | T7 on Android, T8 (desktop) |
| H3 | Shared `UiScope.typeSecret(key, text)` in `Helpers.kt`, a copy of the private `HiddenProfilesTest.kt:148-152`. The two existing private copies (also `OnboardingTest.kt:302-307`) stay untouched unless you approve removing them. | `uiTestShared/.../harness/Helpers.kt` | T4–T8 |
| H4 | A new screen file `DatabaseScreens.kt` (navigation and form helpers only). | `uiTestShared/.../harness/` | all |
| K | App keys only (section 4): `Ui.Settings.Database`, `Ui.Database.*` (8), `Ui.DbPassphrase.*` (5), plus a `uiKey` parameter on `TtlOptions` and on the expect/actual `SavePassphraseSetting`. | `commonMain`, `desktopMain`, `androidMain` | all |
| C | Catalogue edits: merged rows get one name, IGNORE → `@KnownAppIssue` for `dbMgmt_importInvalidArchive`, file:line corrections (section 5). Plan §15.5 gets new items 7–9 (section 6). | `plans/ui-tests/catalogue/04-…md`, plan | — |

Without H2, T7 (`dbEncrypt_removeFromSettings`) becomes desktop-only (`DatabaseEncryptionDesktopTest`) and T8 cannot be written. Without H1, T4 and T5 cannot be written; the alternative is calling `exportChatArchive`/`importArchive` directly, which is not allowed because it bypasses the feature under test.

## 1. Harness and app facts that shape the design (verified)

1. **The reset survives anything these tests leave behind.** Desktop `resetForTest` (`desktopTest/.../harness/UiTestApp.kt:60-86`): `stopCore` skips `chatCloseStore` when the controller is null (the state after a DB-error screen, :117-119). It then deletes the DB files, `PrefStores.restore()` rewrites `settings.properties` from the boot capture (StoreDBPassphrase, EncryptedDBPassphrase, InitialRandomDBPassphrase, ChatStopped, PrivacyLinkPreviews), and `DbTemplate.installIfPresent` (`DbTemplate.kt:16-22`) copies the template and writes its key. A changed or removed passphrase, a stopped chat, a deleted or imported DB and a pending DB-error screen therefore do not leak into the next test. If a test ends with no controller, `UiTest.kt:60-63` re-inits an empty core. Android does the same (`android/.../UiTestApp.kt:74-98`, `DbTemplate.install` :141-148 removes the Keystore alias when the template key is empty).
2. **Android template is decrypted** (`android/.../UiTestApp.kt:37-45`). On Android the database passphrase screen therefore starts in the "not encrypted" state: footer "Your chat database is not encrypted - set passphrase to protect it." (`androidMain/.../DatabaseEncryptionView.android.kt:66-67`), and the first passphrase goes through "Encrypt database?" / "Encrypt". Desktop starts from the random key: "Change database passphrase?" / "Update" (`DatabaseEncryptionView.kt:172-182`). Both end in "Database encrypted!" (:485), use the same `Ui.Alert.Confirm` key, and afterwards show the "Current passphrase…" field (:147). A shared helper can therefore set the first passphrase without a platform branch. `DbTemplate.install` sets `initialRandomDBPassphrase=true` on Android too, so export is prohibited on both platforms until a passphrase is set.
3. **Neither `restart()` can wait for a "Start chat?" alert, and Android `restart()` cannot reach a DB-error screen.**
   - Desktop `restart()` (`desktopTest/.../UiTest.kt:93-102`) polls `chatDbStatus != null && !ctrlInitInProgress`. While "Start chat?" is pending, `initChatController` is suspended in `startChat().await()` (`Core.kt:170`, alert at `Core.kt:226-236`) with `ctrlInitInProgress` true, so the poll times out.
   - Android `restart()` (`android/.../UiTest.kt:86-96`) needs `chatRunning == true && currentUser != null`, which never happens on a passphrase prompt.
   - Desktop restart into a DB-error screen already works: `OnboardingTest.kt:156-158` does it. Hence H2.
4. **The desktop file chooser is a plain `CompletableDeferred`.** `FileChooserLauncher.launch` awaits `simplexWindowState.saveDialog/openDialog.awaitResult` (`desktopMain/.../platform/Files.desktop.kt:69-83`). The AWT dialog is composed only by the real window (`DesktopApp.kt:135-165`), never in the test scene, and the harness currently completes pending dialogs empty at reset (`Reset.kt:85-94`).
   - On Android the launcher is `rememberLauncherForActivityResult` with `CreateDocument`/`GetContent` (`androidMain/.../Files.android.kt:37-43`). The harness already stubs every external intent as RESULT_CANCELED (`android/.../UiTest.kt:138`); a later `intending` stub takes precedence.
   - `getFileName(uri)` on Android queries `OpenableColumns.DISPLAY_NAME` (`Utils.android.kt:231-243`), so a `file://` URI gives `null` and the import silently does nothing (`DatabaseView.kt:758-775`). It must be a `content://` URI from the app's own `FileProvider` (`android/src/main/res/xml/file_paths.xml`: `files-path /`, authority `"$APPLICATION_ID.provider"`, used at `Utils.android.kt:167`).
   - The file must live under `context.filesDir`. The import's `apiDeleteStorage` removes only `files/app_files`, `files/assets` and `files/temp_files` (`src/Simplex/Chat/Archive.hs:122-135`), so `files/uitest-exchange/` survives it. The next reset's `deleteChatDatabaseFilesAndState` deletes `filesDir` recursively (`DatabaseView.kt:569`), which cleans it up.
5. **Masked fields.** `PassphraseField` uses `PasswordVisualTransformation` (`DatabaseEncryptionView.kt:382-385`), so `typeInto`/`replaceText`, which compare `EditableText` with the typed text, cannot be used. Tests type with `typeSecret` and compare lengths (H3). To retype, reopen the screen: the fields are `remember`ed per composition (:52-54).
6. **Setting a shorter TTL deletes immediately.** `APISetChatItemTTL` runs `expireChatItems user newTTL True` synchronously when the TTL gets shorter (`src/Simplex/Chat/Library/Commands.hs:1894-1903`). For contacts the deletion is by `chat_items.created_at` only (`src/Simplex/Chat/Store/Messages.hs:3621`). Backdating `created_at` alone keeps the item last by `item_ts`, so it stays the chat-list preview until it is deleted. `/sql chat` runs arbitrary SQL (`Commands.hs:686`); there is a precedent for a `CC.Console("/sql chat …")` precondition at `ChatListRequestsCardsCompatTest.kt:27`.
7. **Export leaves the chat stopped.** `exportArchive` returns false (`DatabaseView.kt:630`), so `stopChatRunBlockStartChat` does not restart the chat (:541).
8. **Import ends with a passphrase prompt and then a "Start chat?" alert.**
   - The import removes the stored key (`DatabaseView.kt:724`). Its stop set `ChatStopped=true` (`stopChatAsync` :509-513), and the follow-up `startChat` fails before `controller.startChat`, which is what resets `ChatStopped` (`SimpleXAPI.kt:724`).
   - So "Save passphrase and open chat" runs `runChat` with `startChat = ::showStartChatAfterRestartAlert` (`DatabaseErrorView.kt:226-228`), and "Start chat?" appears. The catalogue row omits this alert.
9. **Core-side "chat running".** `CC.CheckChatRunning()` (`SimpleXAPI.kt:4002`) answers `CR.ChatRunning` or `CR.ChatStopped`. It reads core state, not `chatModel.chatRunning`, which the app sets itself. It serves as the persisted/core-level evidence where no peer can observe the state.

## 2. Row decisions

KEEP = selected now. MERGE = folded into the named test. DEFER = the row stays, not in this batch. DROP = remove the row. Corrected file:line references are in section 5.

| Catalogue row (line) | Test name(s) | Decision | Reason |
|---|---|---|---|
| 172 Layout | `chatData_layout` | MERGE → T1 | Its only non-trivial content, the footer naming the current profile, is asserted in T1; every item is acted on by some test, so a missing item fails there. |
| 173 TTL change | `chatData_ttlIncrease_noAlert`, `…Decrease_alertConfirm`, `…Decrease_alertCancel` | KEEP as T1 (`chatData_ttlDecrease_alertConfirm`), the other two MERGE | Same screen and picker. The cancel, confirm and increase branches of one condition (`DatabaseView.kt:79-83`) belong in one test. |
| 174 TTL custom seconds | `chatData_ttlCustomSecondsShown` | DEFER | Cosmetic; needs a console command and a restart. |
| 175 TTL per profile | `chatData_ttlPerProfile` | DEFER | Needs a second profile; lower data-loss value than T1. T1 checks the footer names the current user. |
| 176 TTL expiry | `chatData_ttlExpiresMessages` | DEFER (partly covered by T1) | T1 covers the immediate expiry when the TTL is set (`Commands.hs:1899-1901`), which is what the user triggers. Background expiry at start (30 s thread) stays SLOW/FIXTURE. |
| 177 Delete files | `chatData_deleteFilesAndMedia` | KEEP as T9 (lower priority) | Data loss: deleting files must keep messages and restart the chat. |
| 178 Files label | `chatData_deleteFilesLabelForMultipleProfiles` | DEFER | Label only. |
| 179 DB mgmt layout | `dbMgmt_layout` | DROP | Pure layout: every row in it is clicked by T2–T6, and the desktop-only "Open database folder" is IGNORE. |
| 180 Run chat | `dbMgmt_stopChat`, `dbMgmt_startChat` | KEEP merged as T2 `dbMgmt_stopAndStartChat` (rename) | `startChat` is the second half of the same scenario; separate tests would be prefixes of each other. |
| 181 Stopped restart | `dbMgmt_stoppedStatePersistsRestart` | KEEP desktop-only as T8 | Needs H2. On Android, relaunching the activity while init awaits the alert risks the plan §15.4 item 10 freeze. |
| 182 Export blocked | `dbMgmt_exportProhibitedWithRandomPassphrase` | MERGE → T4 | It is the opposite state of T4's export step. |
| 183 Export | `dbMgmt_exportArchive` | MERGE → T4 | "Importable" is proven only by importing; a separate export test would be a prefix of T4. |
| 184 Import | `dbMgmt_importArchive_restoresData` | KEEP as T4 | The data-loss core. |
| 185 Import invalid | `dbMgmt_importInvalidArchive` | KEEP as T5, IGNORE → `@KnownAppIssue` | A known app bug is a visible result per the guide §6, not IGNORE; it also wipes all data (§6 B1). |
| 186 Settings import | `dbMgmt_importRestoresAppSettings` | MERGE → T4 | One precondition and one assertion on T4's import. |
| 187 Delete DB | `dbMgmt_deleteDatabase` | KEEP as T6 | — |
| 188 Open folder | `dbMgmt_openDatabaseFolder` | stays IGNORE | `Desktop.isDesktopSupported()` false headless (`Files.desktop.kt:37`). |
| 189 Remote | `dbMgmt_disconnectMobilesWhenRemote` | DEFER | Needs a mobile host (§K harness not built). |
| 190 First passphrase | `dbEncrypt_setFirstPassphrase` | MERGE → T3 | T3 sets the first passphrase through the UI and asserts its outcome; a separate test would be T3's prefix. |
| 191 Change | `dbEncrypt_changePassphrase`, `dbEncrypt_wrongCurrentPassphrase` | KEEP as T3, wrong-current MERGE | A wrong current passphrase followed by the correct one proves the DB still uses the old key. |
| 192 Store | `dbEncrypt_removeFromSettings`, `dbEncrypt_saveToSettings` | KEEP as T7, save MERGE | One scenario: stop storing the passphrase, then store it again. |
| 193 Footers | `dbEncrypt_footerStates` | MERGE → T7 | The footers are the on-screen evidence of each storage state T7 passes through. |
| 194 Unencrypted | `dbEncrypt_footerUnencrypted` | DEFER | Desktop needs an unencrypted-DB FIXTURE. On Android it is the harness start state (§1.2), but asserting it in a shared test would branch on harness state. |
| 195 Startup prompt | `dbError_enterPassphraseOnStartup_open`, `dbError_wrongPassphrase` | MERGE → T7 | The prompt is reached naturally after "Remove". `OnboardingTest.onboardingPassphrasePersistsAcrossRestart` (desktop only) covers the same view from onboarding; T7 adds the settings path and Android. |
| 196 Save and open | `dbError_saveAndOpen` | MERGE → T7 (also T4) | T7 reaches it with "Save passphrase in settings" on and no stored key, exactly the row's state, without editing settings by hand. |
| 197 Upgrade confirm | `dbError_upgradeConfirmation` | DEFER | Needs a down-SQL fixture and developer tools (SQL fixtures not built, plan §0.5.11). |
| 198 Downgrade | `dbError_incompatibleDowngrade` | KEEP desktop-only as T10 (lower priority) | The fixture is one in-test `/sql chat INSERT INTO migrations`; this is the screen a user sees after installing an older app. It becomes shared if H2 is approved. |
| 199 Restore backup | `dbError_restoreBackup` | DEFER | Needs `.bak` files under another key and EncryptionStartedAt; not reachable through the UI (backups are deleted on the next chat start, `SimpleXAPI.kt:726-729`). |
| 200 Error folder | `dbError_openDatabaseFolder` | stays IGNORE | Same as 188. |

## 3. The tests

Shared tests: package `chat.simplex.app.ui.settings`, classes per the class map: `ChatDataTest` for the `chatData_*`/`dbMgmt_*` names, `DatabaseEncryptionTest` for the `dbEncrypt_*`/`dbError_*` names. Desktop-only tests: `chat.simplex.app.ui.desktop.ChatDataDesktopTest` and `DatabaseEncryptionDesktopTest`.

Existing helpers cited (desktop `UiTest.kt`; Android has the same names):

| Helper | Location | Helper | Location | Helper | Location |
|---|---|---|---|---|---|
| `app` | :86 | `waitFor` | :136 | `cli` | :304 |
| `restart` | :93 | `click` | :206 | `poll` | :308 |
| `textOf` | :104 | `enterText` | :232 | `seesDescription` | :348 |
| `storedSetting` | :112 | `textsOf` | :273 | `seesText` | :351 |
| `exists` | :134 | `escape` | :200 | | |

- Android: `app` :80, `restart` :86, `storedSetting` :331.
- `connect` `Helpers.kt:26`.
- `App.settings()` `Screens.kt:31`; `chatList.open` :81, `.rowTexts` :92, `.row` :52; `ChatScreen.itemTexts` :240, `.send` :274.
- `backToChatList` `ChatListToolbarScreens.kt:60`.
- Peer: `sends` `PeerLines.kt:17`, `expectsMessage` :29.
- `AlertManager.shared.hasAlertsShown()` `AlertManager.kt:54`.
- New helpers are named `NEW` and specified in section 4.

---

### T1 `chatData_ttlDecrease_alertConfirm` (shared, `ChatDataTest`)

Rows: 173 (all three names), 172. Needs: P (bob).

**Scenario.** Alice has an old message and a recent one from bob. She sets "Delete messages after" to 1 day. "Cancel" in the warning changes nothing. "Delete messages" removes the old message and keeps the recent one. Raising the period to 1 week asks nothing.

**This test fails if:**
- the confirmation does not apply the TTL. Mutation: `DatabaseView.kt:339` `onConfirm = { setCiTTL(…) }` → `onConfirm = {}`; the expired message stays;
- Cancel applies it anyway: `:340` `onDismiss = { setCiTTL(m, rhId, selectedChatItemTTL, progressIndicator, appFilesCountAndSize) }`; the core TTL becomes Day after Cancel;
- the increase asks for confirmation: `:79` `if (it < oldValue)` → `if (it != oldValue)`; an alert appears for 1 week.

**Setup.** `val alice = app("alice"); val bob = cli("bob"); connect(bob)`. `bob sends "@alice kept message"`, then `bob sends "@alice expired message"`. `poll { "expired message" in alice.chatList.rowTexts("bob") }`. Then `val expiredId = alice.chatList.chat("bob").chatItems.last().id` and `backdateItemViaSql(expiredId)` (NEW, test-private): `CC.Console("/sql chat UPDATE chat_items SET created_at = '2020-01-01 00:00:00' WHERE chat_item_id = $expiredId")`. The result must not be an `API.Error`. A fixed old literal avoids depending on the timestamp format, and `item_ts` is left alone so the item stays the preview.

**Steps and assertions** (opposite state first):
```kotlin
val data = chatData(alice)                                   // NEW: settings > "Chat data"
check(seesText("This setting applies to messages in your current chat profile alice.", substring = true))
check(data.ttlText() == "never" && coreTtl() == ChatItemTTL.None)   // coreTtl = runBlocking { chatController.getChatItemTTL(null) } (SimpleXAPI.kt:1481)
data.selectTtl(ChatItemTTL.Day)                               // NEW: click(Ui.Database.Ttl); click(Ui.Database.Ttl.option(ChatItemTTL.Day))
poll("the deletion alert") { seesText("Enable automatic message deletion?") && "Delete messages" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
poll("TTL back to never") { !AlertManager.shared.hasAlertsShown() && data.ttlText() == "never" }
check(coreTtl() == ChatItemTTL.None)
data.selectTtl(ChatItemTTL.Day); poll(alert as above); click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll("TTL 1 day in the picker and the core") { data.ttlText() == "1 day" && coreTtl() == ChatItemTTL.Day }
data.selectTtl(ChatItemTTL.Week)
poll("TTL 1 week without an alert") { data.ttlText() == "1 week" && coreTtl() == ChatItemTTL.Week }
check(!AlertManager.shared.hasAlertsShown())
data.close()                                                  // NEW: escape + poll to the chat list
poll("bob's preview is the kept message") { alice.chatList.rowTexts("bob").let { "kept message" in it && "expired message" !in it } }
val texts = alice.chatList.open("bob").itemTexts()
check("kept message" in texts && "expired message" !in texts)
```
The bob row's preview is the screen evidence: `afterSetCiTTL` reloads the chats (`DatabaseView.kt:841-851`).

Strings: "Chat data", "Delete messages after", "never", "1 day", "1 week", "Enable automatic message deletion?", "Delete messages", "Cancel", "This setting applies to messages in your current chat profile".

**Keys:** `Ui.Settings.Database`, `Ui.Database.Ttl` (section 4).

**Harness impact:** the TTL lives in the DB, which the reset deletes; nothing to clean.

**Platform and flakiness.** Shared. On Android the dropdown is a popup; `click` handles popups, as in `AppearanceScreen.colorMode`. The option key's argument is the `ChatItemTTL` object, whose tag uses `toString()`, stable within one process. No timing risk: the deletion is synchronous before `setCiTTL` returns, and the picker poll waits for it.

---

### T2 `dbMgmt_stopAndStartChat` (shared, `ChatDataTest`; replaces `dbMgmt_stopChat` and `dbMgmt_startChat`)

Rows: 180. Needs: P (bob).

**Scenario.** Alice opens "Database passphrase & export" and turns off "Chat is running". "Cancel" keeps it running; "Stop" stops it. While stopped, bob's message is not received. Turning the chat on delivers it, and alice can reply.

**This test fails if:**
- stopping does not stop the core: `DatabaseView.kt:510` `m.controller.apiStopChat()` removed; `CheckChatRunning` still says running;
- starting does not restart it: `:428` `m.controller.startChat(user)` removed; bob's message never arrives and bob never gets the reply;
- the confirmation is skipped: `:396` `stopChatAlert()` → `authStopChat(chatModel)`; no "Stop chat?" alert.

**Steps:**
```kotlin
val alice = app("alice"); val bob = cli("bob"); connect(bob)
val db = chatData(alice).management()                         // NEW: click(Ui.Database.Management)
check(seesText("Chat is running") && coreRunning())           // coreRunning = sendCmd(null, CC.CheckChatRunning()).result is CR.ChatRunning
click(Ui.Database.RunChat)
poll { seesText("Stop chat?") && seesText("Stop chat to export, import or delete chat database. You will not be able to receive and send messages while the chat is stopped.") }
click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
poll { !AlertManager.shared.hasAlertsShown() && seesText("Chat is running") }; check(coreRunning())
click(Ui.Database.RunChat); poll(alert); check("Stop" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)))
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll("chat stopped on screen, on disk and in the core") {
  seesText("Chat is stopped") && seesText("You must use the most recent version of your chat database on one device ONLY", substring = true) &&
  storedSetting("ChatStopped") == "true" && !coreRunning()
}
bob sends "@alice sent while stopped"
check(chatModel.chats.value.first { it.chatInfo.chatViewName == "bob" }.chatItems.none { it.text == "sent while stopped" })
click(Ui.Database.RunChat)
poll { seesText("Chat is running") && storedSetting("ChatStopped") == "false" && coreRunning() }
db.close()
poll(CONNECT_TIMEOUT_MS, "bob's queued message after start") { "sent while stopped" in alice.chatList.rowTexts("bob") }
alice.chatList.open("bob").send("back online")
bob expectsMessage "alice> back online"
```
The model check before the start alone is weak, because delivery may simply not have happened yet. The core-level `!coreRunning()` proves the stop; the arrival after start proves nothing was lost.

**Keys:** `Ui.Database.Management`, `Ui.Database.RunChat`.

**Harness impact:** the test ends running. If it fails while stopped, `stopCore` skips `stopChatAsync` and the reset continues normally.

**Platform:** shared. On desktop `stopChat` closes `ModalManager.end` (center) modals only (`DatabaseView.kt:496-498`); the settings modal is in `ModalManager.start` and stays. Flakiness: the arrival waits on the network, hence `CONNECT_TIMEOUT_MS`.

---

### T3 `dbEncrypt_changePassphrase` (shared, `DatabaseEncryptionTest`)

Rows: 190, 191 (both names). Needs: P (bob), RESTART.

**Scenario.**
1. Alice sets her own database passphrase.
2. She then tries to change it with a wrong current passphrase: "Wrong passphrase!".
3. She reopens the screen and changes it with the correct current one: "Database encrypted!".
4. After an app restart the chat opens without a prompt, bob's earlier message is still there, and messaging works. The agent DB was re-keyed too.

**This test fails if:**
- the new key is not stored: `DatabaseEncryptionView.kt:478` `ksDatabasePassword.set(new)` → `set(currentKey.value)`; the restart lands on the "Wrong database passphrase" screen instead of the chat list;
- the wrong current passphrase is not recognised: `:455` branch removed; the alert shows "Error encrypting database" instead;
- the re-key is not done: `:450` replaced with `null`; the stored key P2 does not open the DB at restart.

**Steps:**
```kotlin
val alice = app("alice"); val bob = cli("bob"); connect(bob)
bob sends "@alice before passphrase change"; poll { "before passphrase change" in alice.chatList.rowTexts("bob") }
val pass = chatData(alice).management().passphrase()          // NEW: click(Ui.Database.Passphrase); waitFor(Ui.DbPassphrase.New)
check(!exists(Ui.DbPassphrase.Current))                        // random or unencrypted DB: no current field (:147)
check(storedSetting("InitialRandomDBPassphrase") == "true")    // precondition only (harness-set)
pass.setFirst(P1)                                              // NEW, section 4
check(exists(Ui.DbPassphrase.Current) && storedSetting("InitialRandomDBPassphrase") == "false")
val storedAfterFirst = storedSetting("EncryptedDBPassphrase")
pass.change(current = "wrong-$P1", new = P2)                   // NEW: types, clicks Update, confirms "Change database passphrase?"/"Update"
poll { seesText("Wrong passphrase!") && seesText("Please enter correct current passphrase.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
check(storedSetting("EncryptedDBPassphrase") == storedAfterFirst)
pass.reopen()                                                  // NEW: escape to "Database passphrase & export", click(Ui.Database.Passphrase); fresh fields
pass.change(current = P1, new = P2)
poll { seesText("Database encrypted!") }; click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll("chat restarted and the new key stored") { chatModel.chatRunning.value == true && storedSetting("EncryptedDBPassphrase").let { it != null && it != storedAfterFirst } }
val after = restart()
poll("chat list without a passphrase prompt") { after.chatList.shown("bob") && !seesText("Encrypted database") && !seesText("Wrong database passphrase") }
val chat = after.chatList.open("bob")
check("before passphrase change" in chat.itemTexts())
chat.send("after passphrase change"); bob expectsMessage "alice> after passphrase change"
```
Strings: "Database passphrase", "New passphrase…", "Confirm new passphrase…", "Current passphrase…", "Update database passphrase", "Change database passphrase?", "Update", "Wrong passphrase!", "Please enter correct current passphrase.", "Database encrypted!".

**Keys:** `Ui.Database.Passphrase`, `Ui.DbPassphrase.Current/New/Confirm/Update`.

**Harness impact:** the reset restores the key (§1.1). The restart uses the stored key, and the default `restart()` works on both platforms.

**Platform:**
- Shared. The first-passphrase alert differs (§1.2); `setFirst` does not assert its title.
- Android stores the key in the Keystore: `EncryptedDBPassphrase` is the Keystore-encrypted blob, which still changes on each set. Desktop stores it as plaintext Base64 (`desktopMain/.../Cryptor.desktop.kt:3-17`).
- After typing on Android, call `hideKeyboard()` before `escape()`.

**Flakiness.** `sqlcipher_export` under arm64 translation can take seconds. The waits are `poll` on states, extended while `apiStorageEncryption` is in flight. Run a Keystore smoke check on Waydroid once.

**Note.** The wrong attempt leaves the chat stopped (§6 B3). The test does not assert the running state between the two attempts, so it stays green; the next `change` stops and restarts the chat itself (`DatabaseEncryptionView.kt:73`).

---

### T4 `dbMgmt_importArchive_restoresData` (shared, `ChatDataTest`)

Rows: 182, 183, 184, 186 (and `dbError_saveAndOpen`'s screen). Needs: P (bob), FD (H1).

**Scenario.**
1. Alice tries to export. "Set passphrase to export" sends her to the passphrase screen, where she sets one.
2. She exports to a file; the chat stops.
3. She turns the chat back on and receives a new message from bob.
4. She imports the archive and enters its passphrase.
5. She is asked "Start chat?" and starts the chat.
6. Bob's message from before the export is there, the newer one is gone, a setting saved in the archive is restored, and messaging bob still works.

**This test fails if:**
- the random-passphrase guard is lost: `DatabaseView.kt:251` → `if (false)`; no alert, and the save dialog opens at once;
- the export never offers a file: `:617` `saveArchiveLauncher.launch(…)` removed;
- the import does not import: `:722` `apiImportArchive(config)` removed; no bob after reopening;
- settings are not restored: `:723` `shouldImportAppSettings.set(true)` removed; PrivacyLinkPreviews stays `true`.

**File choosing.** H1 (section 4). `val zip = File(exchangeDir(), "export.zip")`.

**Steps:**
```kotlin
val alice = app("alice"); val bob = cli("bob"); connect(bob)
bob sends "@alice before export"; poll { "before export" in alice.chatList.rowTexts("bob") }
setLinkPreviewsPrefDirectly(false)                             // NEW test-private: appPrefs.privacyLinkPreviews.set(false); precondition, not the feature
check(storedSetting("PrivacyLinkPreviews") == "false")
val db = chatData(alice).management()
click(Ui.Database.Export)
poll { seesText("Set passphrase to export") && seesText("Database is encrypted using a random passphrase. Please change it before exporting.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
waitFor(Ui.DbPassphrase.New)                                   // the passphrase screen opened (DatabaseView.kt:253-255)
PassphraseScreen(this).setFirst(P)
goBackTo(Ui.Database.Export)                                   // NEW: hideKeyboard(); escape(); poll exists
check(!zip.exists())
saveFileAs(zip) { click(Ui.Database.Export) }                  // H1
poll("the archive written") { zip.length() > 0 && ZipFile(zip).use { z -> z.entries().toList().map { it.name }.containsAll(listOf("simplex_v1_chat.db", "simplex_v1_agent.db")) } }
poll("chat left stopped after export") { seesText("Chat is stopped") }
setLinkPreviewsPrefDirectly(true); check(storedSetting("PrivacyLinkPreviews") == "true")
click(Ui.Database.RunChat); poll { seesText("Chat is running") }
bob sends "@alice after export"
db.close(); poll(CONNECT_TIMEOUT_MS) { "after export" in alice.chatList.rowTexts("bob") }
chatData(alice).management()
openFile(zip) { click(Ui.Database.Import) }                    // H1
poll { seesText("Import chat database?") && "Import" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll(30_000) { seesText("Chat database imported") && seesText("Restart the app to use imported chat database.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll { seesText("Encrypted database") && seesText("Database passphrase is required to open chat.") && "Save passphrase and open chat" in textsOf(Ui.DbError.Open) }
check(storedSetting("EncryptedDBPassphrase") == null)          // removed by the import (:724)
typeSecret(Ui.DbError.Passphrase, P); click(Ui.DbError.Open)
poll(30_000) { seesText("Start chat?") && seesText("Chat is stopped. If you already used this database on another device, you should transfer it back before starting chat.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll(30_000, "chat list with bob from the archive") { chatModel.chatRunning.value == true && alice.chatList.shown("bob") }
check(storedSetting("PrivacyLinkPreviews") == "false" && storedSetting("ShouldImportAppSettings") == "false" && storedSetting("EncryptedDBPassphrase") != null)
val chat = alice.chatList.open("bob")
chat.itemTexts().let { check("before export" in it && "after export" !in it) }
chat.send("after import"); bob expectsMessage "alice> after import"
```
Strings: "Export database", "Set passphrase to export", "Database is encrypted using a random passphrase. Please change it before exporting.", "Chat is stopped", "Chat is running", "Import database", "Import chat database?", "Import", "Chat database imported", "Restart the app to use imported chat database.", "Encrypted database", "Database passphrase is required to open chat.", "Save passphrase and open chat", "Start chat?", "Chat is stopped. If you already used this database on another device, you should transfer it back before starting chat.".

**Keys:** `Ui.Database.Export`, `Ui.Database.Import`, `Ui.Database.RunChat`, and the existing `Ui.DbError.*`.

**Why bob's "after import" works.** Alice sends nothing between the export and the import, so her archived sending ratchet is current. Bob must not send after the import: alice's archived receive state would report a skipped message (an integrity item), and the peer-error rule would not catch that on alice's side, but it would add noise.

**Harness impact:** desktop `exchangeDir` lives in the test directory, deleted after a pass. Android `files/uitest-exchange` is deleted by the next reset (§1.4). Any "Start chat?" left pending by a failure leaks an awaiting job, which the next reset abandons after 30 s (`UiTestApp.drain`, `stopCore` clears `ctrlInitInProgress`): slow, but it recovers.

**Platform:**
- Shared through H1.
- Desktop quirk, consistent here: the desktop import launcher keeps the `stopped` value from the screen's first composition (§6 B2). In this flow it equals the real value (false), because the management screen is recomposed after the passphrase screen closes and the chat is running at import time.
- Android shows "Save passphrase in Keystore" (not acted on).

**Flakiness.** The longest test (connect, re-key, export, import), estimated at 60–90 s on Android, within the 300 s watchdog.

---

### T5 `dbMgmt_importInvalidArchive` (shared, `ChatDataTest`, `@KnownAppIssue("15.5 item 7", "the Error importing chat database alert")`)

Rows: 185. Needs: FD (H1). Replaces the row's IGNORE.

**Scenario.** Alice imports a file that is not a chat archive. She must see "Error importing chat database", and her profile must still be there.

**Today** (§6 B1): no alert. All data is wiped and onboarding appears. The test fails at the first poll and is reported as a skipped known issue. Once the bug is fixed, the mutation is reverting the fix.

**Steps:**
```kotlin
val alice = app("alice")
val notes = alice.chatList.open("Private notes"); notes.send("my note"); backToChatList()
val bad = File(exchangeDir(), "not-an-archive.zip").apply { writeText("not a zip") }
chatData(alice).management()
openFile(bad) { click(Ui.Database.Import) }
poll { seesText("Import chat database?") }; click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll(30_000, "the Error importing chat database alert") { seesText("Error importing chat database") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll("alice and her note still there") { chatModel.currentUser.value?.displayName == "alice" && !exists(Ui.Onboarding.GetStarted) }
check("my note" in alice.chatList.open("Private notes").itemTexts())
```
The second assertion (data kept) requires the fix to validate the archive before `apiDeleteStorage` (`DatabaseView.kt:718`). If maintainers decide the alert alone is the fix, drop the data assertion.

**Harness impact:** today the test ends on onboarding with a fresh random-key DB, which the next reset deletes.

---

### T6 `dbMgmt_deleteDatabase` (shared, `ChatDataTest`)

Rows: 187. Needs: none.

**Scenario.** Alice deletes the database. "Cancel" keeps everything; "Delete" shows "Chat database deleted" and then the welcome screen without a restart. The core has no users.

**This test fails if:** the deletion is skipped: `DatabaseView.kt:799` `deleteChatAsync(m)` removed; `listUsers` still returns alice. Or the app does not return to onboarding: `Core.kt:164` / `:168` `onboardingStage.set(Step1_SimpleXInfo)` removed.

**Steps:**
```kotlin
val alice = app("alice"); alice.chatList.open("Private notes").send("to be deleted"); backToChatList()
chatData(alice).management()
click(Ui.Database.Delete)
poll { seesText("Delete chat profile?") && seesText("This action cannot be undone - your profile, contacts, messages and files will be irreversibly lost.") && "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
poll { !AlertManager.shared.hasAlertsShown() && exists(Ui.Database.Delete) }
check(listUsers().map { it.user.displayName } == listOf("alice"))   // runBlocking { chatController.listUsers(null) }
click(Ui.Database.Delete); poll(alert); click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll(30_000) { seesText("Chat database deleted") && seesText("Restart the app to create a new chat profile.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll(30_000, "the welcome screen") { exists(Ui.Onboarding.GetStarted) && onboarding().stage == OnboardingStage.Step1_SimpleXInfo && chatModel.currentUser.value == null }
check(listUsers().isEmpty())
```
**Keys:** `Ui.Database.Delete`.

**Harness impact:** the app has already re-initialised a fresh DB (`stopChatRunBlockStartChat` → `startChat` → `initChatController`, `DatabaseView.kt:541-542, 414-426`), so the controller exists. The reset deletes it.

**Platform:** shared. Desktop calls `startChatWithoutUser` (`Core.kt:157-166`); Android sets the stage (`Core.kt:168`). Both show `Ui.Onboarding.GetStarted` (as used by `OnboardingAndroidTest`).

---

### T7 `dbEncrypt_removeFromSettings` (shared with H2, else desktop-only; `DatabaseEncryptionTest`)

Rows: 192 (both names), 193, 195 (both names), 196. Needs: RESTART.

**Scenario.**
1. Alice has her own passphrase, stored. She turns off "Save passphrase in settings": "Cancel" keeps it; "Remove" deletes the stored passphrase.
2. On the next start she is asked for it. A wrong one is refused; the right one opens the chat and is not stored.
3. She turns storing back on. On the next start she is asked once, through "Save passphrase and open chat", and after that the app starts without asking.

**This test fails if:**
- "Remove" leaves the passphrase on disk until the next start: `DatabaseEncryptionView.kt:309` `ksDatabasePassword.remove()` removed. The restart still prompts, because `Core.kt:75-77` removes the key then, but the pre-restart `EncryptedDBPassphrase == null` check fails. That is a plaintext-on-disk regression on desktop.
- "Open chat" stores the passphrase: `DatabaseErrorView.kt:104-105` calls `saveAndRunChatOnClick`.
- "Save passphrase and open chat" does not store it: `DatabaseErrorView.kt:55` removed; the last restart prompts again.
- The footers are wrong: e.g. `DatabaseEncryptionView.desktop.kt:69` `if (storedKey.value)` inverted.

**Steps.** Footer helper: `footer(desktop, android) = if (appPlatform.isDesktop) desktop else android` (real per-platform strings, expect/actual).
```kotlin
val alice = app("alice"); alice.chatList.open("Private notes").send("kept across restarts"); backToChatList()
val pass = chatData(alice).management().passphrase(); pass.setFirst(P1)
check(seesText(footer("The passphrase is stored in settings as plaintext.", "is used to securely store passphrase - it allows notification service to work."), substring = true)
  && seesText("you will NOT be able to recover or change passphrase if you lose it.", substring = true))
val stored = storedSetting("EncryptedDBPassphrase"); check(stored != null)
click(Ui.DbPassphrase.Save)
poll { seesText(footer("Remove passphrase from settings?", "Remove passphrase from Keystore?")) && "Remove" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
poll { !AlertManager.shared.hasAlertsShown() }; check(storedSetting("EncryptedDBPassphrase") == stored)
click(Ui.DbPassphrase.Save); poll(alert); click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll { seesText("You have to enter passphrase every time the app starts - it is not stored on the device.") &&
       storedSetting("EncryptedDBPassphrase") == null && storedSetting("StoreDBPassphrase") == "false" }
var app = restart(ready = ::dbPrompted, what = "the passphrase prompt")   // H2; dbPrompted = chatDbStatus is ErrorNotADatabase && !ctrlInitInProgress
poll { seesText("Encrypted database") && seesText("Database passphrase is required to open chat.") && "Open chat" in textsOf(Ui.DbError.Open) }
typeSecret(Ui.DbError.Passphrase, P1.dropLast(1)); click(Ui.DbError.Open)
poll(30_000) { seesText("Wrong passphrase!") && seesText("Enter correct passphrase.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
typeSecret(Ui.DbError.Passphrase, P1.takeLast(1)); click(Ui.DbError.Open)
poll(30_000) { app.chatList.shown("Private notes") }
check(storedSetting("EncryptedDBPassphrase") == null)
check("kept across restarts" in app.chatList.open("Private notes").itemTexts()); backToChatList()
chatData(app).management().passphrase()
click(Ui.DbPassphrase.Save)                                    // switching on: no alert (setUseKeychain(true), :136)
poll { seesText(footer("The passphrase will be stored in settings as plaintext after you change it or restart the app.",
                       "Android Keystore will be used to securely store passphrase after you restart the app or change passphrase - it will allow receiving notifications.")) &&
       storedSetting("StoreDBPassphrase") == "true" && storedSetting("EncryptedDBPassphrase") == null }
app = restart(ready = ::dbPrompted, what = "the passphrase prompt")
poll { seesText("Encrypted database") && "Save passphrase and open chat" in textsOf(Ui.DbError.Open) }
typeSecret(Ui.DbError.Passphrase, P1); click(Ui.DbError.Open)
poll(30_000) { app.chatList.shown("Private notes") && storedSetting("EncryptedDBPassphrase") != null }
app = restart()
poll("no prompt with the stored passphrase") { app.chatList.shown("Private notes") && !seesText("Encrypted database") }
```
Strings: "Save passphrase in settings" / "Save passphrase in Keystore" (not matched; acted on by key), "Remove passphrase from settings?" / "Remove passphrase from Keystore?", "Remove", "You have to enter passphrase every time the app starts - it is not stored on the device.", "The passphrase will be stored in settings as plaintext after you change it or restart the app.", "Android Keystore will be used to securely store passphrase after you restart the app or change passphrase - it will allow receiving notifications.", "Encrypted database", "Database passphrase is required to open chat.", "Open chat", "Wrong passphrase!", "Enter correct passphrase.", "Save passphrase and open chat".

**Keys:** `Ui.DbPassphrase.Save`, and the existing `Ui.DbError.Passphrase/Open`.

**Harness impact:** StoreDBPassphrase, EncryptedDBPassphrase and the Keystore alias are restored by the reset (§1.1). Every restart ends either on the chat list or on the prompt, so no pending init is left behind.

**Platform:** shared only with H2 (Android restart into the prompt).
- Android footer `keychain_is_storing_securely` contains a `%s` storage name that varies by device, so the match is on a substring.
- Android "Remove" also warns "Notifications will be delivered only until the app stops!" (not asserted).

**Flakiness.** Three restarts: about 10 s on desktop, longer on Android. The wrong-then-right typing reuses the `OnboardingTest` trick (the field keeps the text), so there is no field clearing and no retry.

**Overlap.** `OnboardingTest.onboardingPassphrasePersistsAcrossRestart` (desktop) exercises the same prompt and wrong entry from onboarding. T7's path (settings "Remove") and its Android run are new. If you prefer, drop T7's wrong-entry step (2 lines).

---

### T8 `dbMgmt_stoppedStatePersistsRestart` (desktop-only, `desktop/ChatDataDesktopTest`; needs H2)

Rows: 181. Needs: P (bob), RESTART.

**Scenario.** Alice stops the chat and restarts the app. She is asked "Start chat?"; "Cancel" keeps it stopped. On the next start, "OK" starts it and bob's message sent while stopped arrives.

**This test fails if:**
- the stopped state is not honoured at start: `Core.kt:61` condition → `false`; the chat auto-starts with no alert;
- Cancel starts it anyway: `Core.kt:182-185` `else` branch replaced by `chatController.startChat(user)`.

**Steps:**
```kotlin
val alice = app("alice"); val bob = cli("bob"); connect(bob)
chatData(alice).management(); click(Ui.Database.RunChat); poll(stop alert); click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll { seesText("Chat is stopped") && storedSetting("ChatStopped") == "true" }
bob sends "@alice sent before restart"
var app = restart(ready = ::startChatAsked, what = "the Start chat? alert")   // startChatAsked = chatDbStatus == OK && AlertManager.shared.hasAlertsShown()
poll { seesText("Start chat?") && seesText("Chat is stopped. If you already used this database on another device, you should transfer it back before starting chat.") }
click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
poll { !chatModel.ctrlInitInProgress.value && chatModel.chatRunning.value == false }
check(!coreRunning() && storedSetting("ChatStopped") == "true")
app.settings(); poll { seesDescription("Chat is stopped") }    // SettingsView.kt:216-221 red icon on "Chat data"
app = restart(ready = ::startChatAsked, what = "the Start chat? alert again")
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll(CONNECT_TIMEOUT_MS) { coreRunning() && "sent before restart" in app.chatList.rowTexts("bob") }
check(storedSetting("ChatStopped") == "false")
app.chatList.open("bob").send("started again"); bob expectsMessage "alice> started again"
```
**Harness impact:** a failure with the alert pending is handled as in T4.

**Why desktop-only:** the Android restart relaunches the activity while `initChatController` awaits the alert (plan §15.4 item 10 risk). This is a harness limit, not a UI difference.

---

### T9 `chatData_deleteFilesAndMedia` (shared, `ChatDataTest`; lower priority)

Rows: 177. Needs: P (bob), XFTP.

**Scenario.** Alice has received an image and a text from bob. "Delete all files" asks "Delete files and media?"; "Delete" removes the files, keeps the messages, and leaves the chat running.

**This test fails if:** the files are not deleted: `DatabaseView.kt:865` `deleteAppFiles()` removed; the footer still counts 1 file. Or the chat is not restarted afterwards: `:542` `startChat(…)` removed; bob never receives the reply.

**Steps.**
1. `bob sends "/img @alice tests/fixtures/test.jpg"`, then wait for "completed uploading file 1 (test.jpg) for alice" (as in `FilesTest.kt:55-58`). `bob sends "@alice kept text"`.
2. Poll until alice's image is `RcvComplete`, as in `FilesTest`. This needs the chat open: open it, then `backToChatList()`.
3. `chatData(alice)`, then `poll { seesText("1 file(s) with total size of", substring = true) }`.
4. `click(Ui.Database.DeleteFiles)`, then `poll { seesText("Delete files and media?") && seesText("This action cannot be undone - all received and sent files and media will be deleted. Low resolution pictures will remain.") }`, then Confirm ("Delete").
5. `poll(30_000) { seesText("No received or sent files") && appFilesDir.listFiles().isNullOrEmpty() && coreRunning() }`.
6. `close()`; open bob's chat: "kept text" is still in `itemTexts()`. `send("files gone")`, then `bob expectsMessage "alice> files gone"`.

**Keys:** `Ui.Database.DeleteFiles`.

**Harness impact:** none; the reset deletes `filesDir` anyway (`DatabaseView.kt:569`).

---

### T10 `dbError_incompatibleDowngrade` (desktop-only, `desktop/DatabaseEncryptionDesktopTest`; shared if H2; lower priority)

Rows: 198. Needs: FIXTURE (in-test SQL), RESTART.

**Scenario.** The database has a migration the app does not know and that has no down migration, as when a newer app version used it. At start alice sees "Incompatible database version" with the reason, instead of a crash or the chat list.

**This test fails if:** the error branch is wrong: `DatabaseErrorView.kt:148-154` (e.g. `MigrationError.Error` rendered as the upgrade screen), or `mtrErrorDescription` (`:283-289`) loses the migration list.

**Steps:**
1. `val alice = app("alice")`, then `check(chatModel.chatDbStatus.value == DBMigrationResult.OK)`.
2. `addFutureMigrationViaSql()` (NEW, test-private): `CC.Console("/sql chat INSERT INTO migrations (name, ts, down) VALUES ('29991231_uitest_newer', '2999-12-31 00:00:00', NULL)")`. The `migrations` schema is at `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql:1-5`. The name sorts after every app migration, so the check yields `MTRENoDown` (simplexmq `Agent/Store/Migrations.hs:26-28` at the pinned `8bc4d360`).
3. `restart()`. The default works on desktop, because init fails fast.
4. `poll { seesText("Incompatible database version") && seesText("File: simplex_v1_chat.db") && seesText("Error: database version is newer than the app, but no down migration for: 29991231_uitest_newer") && !exists(Ui.DbError.Passphrase) && !exists(Ui.ChatList.Search) }`.

**Harness impact:** controller null at the end; `UiTest.kt:60-63` re-inits, and the reset deletes the DB.

## 4. New helpers and keys

### H1 (both `UiTest.kt`, same names on both `UiScope`s)

Desktop (`desktopTest/.../harness/UiTest.kt`, inside `UiScope`):
```kotlin
fun exchangeDir(): File = File(dir, "exchange").apply { mkdirs() }
fun saveFileAs(file: File, action: () -> Unit) = answerDialog(simplexWindowState.saveDialog, "the save file dialog", file, action)
fun openFile(file: File, action: () -> Unit) = answerDialog(simplexWindowState.openDialog, "the open file dialog", file, action)
private fun answerDialog(d: DialogState<File?>, what: String, file: File, action: () -> Unit) {
  action()
  poll(what = what) { d.isAwaiting }
  onEdt { d.onResult(file) }
  poll(what = "$what to be answered") { !d.isAwaiting }
}
```
Android (`android/.../harness/UiTest.kt`):
```kotlin
fun exchangeDir(): File = File(androidAppContext.filesDir, "uitest-exchange").apply { mkdirs() }
fun saveFileAs(file: File, action: () -> Unit) = answerIntent(Intent.ACTION_CREATE_DOCUMENT, file, action)
fun openFile(file: File, action: () -> Unit) = answerIntent(Intent.ACTION_GET_CONTENT, file, action)
private fun answerIntent(intentAction: String, file: File, action: () -> Unit) {
  val uri = FileProvider.getUriForFile(androidAppContext, "$APPLICATION_ID.provider", file)   // chat.simplex.common.helpers.APPLICATION_ID
  Intents.intending(IntentMatchers.hasAction(intentAction)).respondWith(Instrumentation.ActivityResult(Activity.RESULT_OK, Intent().setData(uri)))
  val before = Intents.getIntents().count { it.action == intentAction }
  action()
  poll(what = "the $intentAction request") { Intents.getIntents().count { it.action == intentAction } > before }
}
```
The most recently added matching stub wins over the generic RESULT_CANCELED stub at :138. Stubs are reset by `Intents.release()` on `restart()`; T4 and T5 do not restart.

### H2 (both `UiTest.kt`)

Desktop:
```kotlin
fun restart(ready: () -> Boolean = { chatModel.chatDbStatus.value != null && !chatModel.ctrlInitInProgress.value }, what: String = "the app to start again"): App
```
The body is unchanged apart from `poll(timeoutMs = 30_000, what = what) { ready() }`.

Android: same signature. The default is today's condition (`android/.../UiTest.kt:89-92`). `ready` replaces the poll between `restartCore` and the activity relaunch.

### H3 (`uiTestShared/.../harness/Helpers.kt`)

```kotlin
fun UiScope.typeSecret(key: UiKey, text: String) {
  val before = editableText(key).orEmpty().length
  enterText(key, text)
  poll(what = "field ${key.tag} to hold ${before + text.length} characters") { editableText(key)?.length == before + text.length }
}
```

### H4 (`uiTestShared/.../harness/DatabaseScreens.kt`, new)

```kotlin
fun UiScope.chatData(app: App): ChatDataScreen { app.settings(); click(Ui.Settings.Database); waitFor(Ui.Database.Management); return ChatDataScreen(this) }
class ChatDataScreen(private val s: UiScope) {
  fun ttlText(): String? = s.textOf(Ui.Database.Ttl)
  fun selectTtl(ttl: ChatItemTTL) { s.click(Ui.Database.Ttl); s.click(Ui.Database.Ttl.option(ttl)) }
  fun management(): DatabaseManagementScreen { s.click(Ui.Database.Management); s.waitFor(Ui.Database.Export); return DatabaseManagementScreen(s) }
  fun close() { s.escape(); s.poll(what = "Chat data to close") { !s.exists(Ui.Database.Management) && s.exists(Ui.Settings.Database) }
                s.escape(); s.poll(what = "settings to close") { !s.exists(Ui.Settings.Database) && s.exists(Ui.ChatList.Search) } }
}
class DatabaseManagementScreen(private val s: UiScope) {
  fun passphrase(): PassphraseScreen { s.click(Ui.Database.Passphrase); s.waitFor(Ui.DbPassphrase.New); return PassphraseScreen(s) }
  fun close() { /* escape ×3 with a poll per level, as in PrivacyTest.closeMorePrivacy */ }
}
class PassphraseScreen(private val s: UiScope) {
  fun setFirst(p: String) {
    s.typeSecret(Ui.DbPassphrase.New, p); s.typeSecret(Ui.DbPassphrase.Confirm, p)
    s.click(Ui.DbPassphrase.Update)
    s.waitFor(Ui.Alert.Confirm(Ui.AlertSource.Shared)); s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    s.poll(timeoutMs = 30_000, what = "Database encrypted!") { s.seesText("Database encrypted!") }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    s.poll(timeoutMs = 30_000, what = "chat running with the current-passphrase field") { chatModel.chatRunning.value == true && s.exists(Ui.DbPassphrase.Current) }
  }
  fun change(current: String, new: String) {
    s.typeSecret(Ui.DbPassphrase.Current, current); s.typeSecret(Ui.DbPassphrase.New, new); s.typeSecret(Ui.DbPassphrase.Confirm, new)
    s.click(Ui.DbPassphrase.Update)
    s.poll(what = "the Change database passphrase? alert") { s.seesText("Change database passphrase?") && "Update" in s.textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }
  fun reopen() { s.hideKeyboard(); s.escape(); s.waitFor(Ui.Database.Passphrase); s.click(Ui.Database.Passphrase); s.waitFor(Ui.DbPassphrase.New) }
}
```

### Keys (`commonMain/.../ui/Ui.kt` plus one-line app edits; all no-ops unless `uiTestHooksEnabled`)

```kotlin
object Settings { …; val Database = UiKey("settings.database") }
object Database {
  val Ttl = UiKey("database.ttl"); val Management = UiKey("database.management"); val DeleteFiles = UiKey("database.deleteFiles")
  val Passphrase = UiKey("database.passphrase"); val Export = UiKey("database.export"); val Import = UiKey("database.import")
  val Delete = UiKey("database.delete"); val RunChat = UiKey("database.runChat")
}
object DbPassphrase {
  val Save = UiKey("dbPassphrase.save"); val Current = UiKey("dbPassphrase.current"); val New = UiKey("dbPassphrase.new")
  val Confirm = UiKey("dbPassphrase.confirm"); val Update = UiKey("dbPassphrase.update")
}
```

| Key | App file:line (acted-on element) | Edit |
|---|---|---|
| `Ui.Settings.Database` | `commonMain/.../views/usersettings/SettingsView.kt:200` (`SectionItemView(openDatabaseView)` in `DatabaseItem` :199-225) | `uiKey = Ui.Settings.Database` (the parameter exists, `Section.kt:209`) |
| `Ui.Database.Ttl` (+ `.option(ttl)`) | `views/database/DatabaseView.kt:128` call; `TtlOptions` :347-375 | Add `uiKey: UiKey? = null` as the **last** parameter of `TtlOptions` (after `default`, so `GroupChatInfoView.kt:950` keeps compiling) and pass `uiKey = uiKey` to `ExposedDropDownSettingRow` (:367; parameter exists, `ExposedDropDownSettingRow.kt:172`; options keyed at :77). The call at :128 passes `uiKey = Ui.Database.Ttl`. |
| `Ui.Database.Management` | `DatabaseView.kt:146-153` | `uiKey =` on `SettingsActionItem` (`SettingsView.kt:386`) |
| `Ui.Database.DeleteFiles` | `DatabaseView.kt:159-162` | `uiKey =` on `SectionItemView` (T9 only) |
| `Ui.Database.Passphrase` | `DatabaseView.kt:231-238` | `uiKey =` |
| `Ui.Database.Export` | `DatabaseView.kt:247-265` | `uiKey =` |
| `Ui.Database.Import` | `DatabaseView.kt:266-273` | `uiKey =` |
| `Ui.Database.Delete` | `DatabaseView.kt:274-288` | `uiKey =` |
| `Ui.Database.RunChat` | `DatabaseView.kt:390-400` (`DefaultSwitch` in `RunChatSetting`) | `modifier = Modifier.ui(Ui.Database.RunChat)` (parameter exists, `DefaultSwitch.kt:17`) |
| `Ui.DbPassphrase.Save` | `DatabaseEncryptionView.kt:129-145` call; expect :253-261; actuals `desktopMain/.../DatabaseEncryptionView.desktop.kt:22-55` (switch :48), `androidMain/.../DatabaseEncryptionView.android.kt:22-55` (switch :48) | Add `uiKey: UiKey? = null` before `onCheckedChange` in the expect and both actuals; the actuals apply `modifier = Modifier.ui(uiKey)` to `DefaultSwitch`; the call at :129 passes `uiKey = Ui.DbPassphrase.Save`. `MigrateToDevice.kt:424` is unchanged (keyless). **Build both platforms** (plan §15.4 item 39). |
| `Ui.DbPassphrase.Current` | `DatabaseEncryptionView.kt:151-157` (modifier :154) | `Modifier.ui(Ui.DbPassphrase.Current).padding(horizontal = DEFAULT_PADDING)` |
| `Ui.DbPassphrase.New` | `:160-168` (modifier :163) | same pattern |
| `Ui.DbPassphrase.Confirm` | `:192-202` (modifier :195) | same pattern |
| `Ui.DbPassphrase.Update` | `:204` `SectionItemViewSpaceBetween` | `uiKey = Ui.DbPassphrase.Update` (parameter exists, `Section.kt:277`) |

Existing keys reused: `Ui.DbError.Passphrase/Open` (`DatabaseErrorView.kt:307,320,339`), `Ui.Alert.Confirm/Dismiss` (`AlertManager.kt:160-205`), `Ui.Onboarding.GetStarted`, `Ui.ChatList.*`.

## 5. Catalogue corrections

Line numbers verified against the current tree.

- Row 182: "`DatabaseEncryptionView.kt:120`" → `:123` (AppBarTitle "Database passphrase").
- Row 185: "`SimpleXAPI.kt:1128`" → `:1160-1164` (throw at :1163). The Needs cell `IGNORE: known app bug…` is not a §11.3 reason; replace it with `FD` and mark the test `@KnownAppIssue` (T5). Add the data-wipe consequence to the note (§6 B1).
- Row 184: after "Save passphrase and open chat", the app shows "Start chat?" / "Chat is stopped. If you already used this database on another device, you should transfer it back before starting chat." (`DatabaseErrorView.kt:226-228`), because the import's stop left `ChatStopped=true` (`DatabaseView.kt:512`).
- Row 190: main file `DatabaseEncryptionView.kt:42-215,423-487` → `:44-224,433-496`. "Update database passphrase (`:197`)" → `:205`. "pre-filled with the random key, `:49`" → `:52`.
- Row 191: `:147-200,444-450` → `:147-207,453-463`.
- Row 192: "switch disabled with the random passphrase, `DatabaseEncryptionView.kt:130`" → `:133`.
- Row 194: `DatabaseEncryptionView.kt:165-175` → `:169-184`; "`:180` (can't set an empty one)" → `:185-190` (`newKey.value.isEmpty()` at :187).
- Row 196: "`ksDatabasePassword.set`, `DatabaseErrorView.kt:53`" → `:55`.
- Row 197: "`DatabaseErrorView.kt:77-78,115`" → `:79-81,117`; `:107-120` → `:109-121`.
- Row 198: `:146-152,281-287` → `:148-154,283-289`.
- Row 199: `:184-210,256-279` → `:184-197,258-281`; "`:45, 256-267`" → `:47, 258-268`; "`:268-279`" → `:270-281`.
- Row 200: "`:322-332`" → `:325-335`.
- Merges: row 180 becomes `dbMgmt_stopAndStartChat`; rows 182/183/186 names leave the table (merged into `dbMgmt_importArchive_restoresData`); 173's increase/cancel names, 190, 191's wrong-current, 192's save, 193, 195, 196 merge as listed in section 2. Recount with `tools/recount.py` and regenerate the class map.
- Legend: UI strings path is `commonMain/moko-resources/base/strings.xml` (correct in the legend; the brief's `resources/MR/base` is wrong).

## 6. Suspected app bugs and harness findings (report, do not fix)

- **B1 (data loss, by code reading; proposed plan §15.5 item 7). Importing an invalid archive silently wipes all data and lands on onboarding.**
  - Mechanism, step by step:
    1. `importArchive` deletes storage first (`DatabaseView.kt:718`).
    2. `apiImportArchive` throws `Exception` (`SimpleXAPI.kt:1163`), but only `Error` is caught (`DatabaseView.kt:740,745`). No alert is shown, and `progressIndicator` stays `true` (set at :714).
    3. `stopChatRunBlockStartChat` catches the throwable and restarts the chat (`:535-542`). `initChatController` creates a fresh random-key DB (`DatabaseUtils.kt:55-59`), and with no user the app shows onboarding (`Core.kt:153-169`).
    4. The core's `.bak` copy is also gone: `deleteStorage` removes backups (`Archive.hs:128,135`) before `importArchive` would make them.
  - Test: T5 (`@KnownAppIssue`). To confirm manually: desktop, "Import database", pick any non-zip file.
- **B2 (desktop, minor; proposed item 8). Desktop file-chooser launchers keep the `onResult` lambda from their first composition** (`Files.desktop.kt:52-53`: `remember(rememberedValue)` with the default `null`; Android uses `rememberLauncherForActivityResult`, which is fresh).
  - In `DatabaseManagementView` the import lambda captures `stopped` (`DatabaseView.kt:202-210`).
  - Failing sequence: open the screen while the chat is stopped, start it with the toggle, then "Import database". The import then runs without stopping the chat, and the core refuses `/_db delete` with `CEChatNotStopped` (`Commands.hs:670,3810-3816`). The resulting `Exception` is swallowed (`DatabaseView.kt:527-529`) and the spinner stays forever.
  - Not covered by a test; a known-issue test is possible if wanted.
- **B3 (UX, apparently by design: comment at `DatabaseEncryptionView.kt:72`; proposed item 9). A failed passphrase change leaves the chat silently stopped.**
  - `encryptDatabase` returns false (`:462,:470`), so `stopChatRunBlockStartChat` does not restart (`DatabaseView.kt:541`).
  - `ChatStopped=true` is also left on disk, so the next app start asks "Start chat?".
  - Messages are not received until the user notices "Chat is stopped" in settings.
  - T3 does not assert the running state between attempts, so it stays green. A separate `@KnownAppIssue` test (set passphrase, wrong current, back, expect "Chat is running") is possible but would be a prefix of T3. Your decision.
- **Harness facts** (for plan §15.4):
  - Desktop `restart()` times out on a pending "Start chat?" alert (`Core.kt:170` keeps `ctrlInitInProgress` true), and Android `restart()` cannot reach any DB-error screen (§1.3). Proposed fix: H2.
  - The Android template is decrypted, so Android passphrase tests start from the "not encrypted" state. The export-prohibited alert still says "encrypted using a random passphrase", only because the harness sets `InitialRandomDBPassphrase=true` (§1.2).
  - Android `getFileName` needs a `content://` URI (§1.4), so file responders must go through the app's `FileProvider`.
- **Not bugs, noted:**
  - Export always leaves the chat stopped (`DatabaseView.kt:630`, documented in the catalogue).
  - Opening "Database passphrase & export" again after a passphrase screen recomposes the management screen, because `ModalManager` composes only the top modal (as in plan §15.5 item 5). That is why B2 does not affect T4.

## 7. Open questions (with proposed answers)

1. Approve H1–H4 and the keys? Proposed: yes. All are test-side, apart from no-op keys and two `uiKey` parameters.
2. T7: shared (needs H2 on Android) or desktop-only? Proposed: shared.
3. T5: assert data kept, or only the alert? Proposed: both; the data wipe is the real harm.
4. B3: add a known-issue test, or only report it? Proposed: report only; the code comment suggests the behaviour is intended.
5. T9 and T10 (lower priority): include in the first batch? Proposed: T9 yes (data loss), T10 optional.
