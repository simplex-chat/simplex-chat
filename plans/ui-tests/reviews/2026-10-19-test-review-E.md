# Test review E: Android BackKeyTest + shared harness masking audit

Repo: /home/user/wt-uitest (branch nd/desktop-ui-tests). I only read code: nothing was built or run.
Paths are relative to `apps/multiplatform/` unless they start with `plans/`.
Abbreviations: BKT = `android/src/androidTest/java/chat/simplex/app/ui/back/BackKeyTest.kt`, BKH = `.../back/BackKeyHelpers.kt`,
AUT = `android/src/androidTest/java/chat/simplex/app/ui/harness/UiTest.kt`, DUT = `common/src/desktopTest/kotlin/chat/simplex/app/ui/harness/UiTest.kt`,
SCR = `common/src/uiTestShared/kotlin/chat/simplex/app/ui/harness/Screens.kt`, HLP = `.../harness/Helpers.kt`, PL = `.../harness/PeerLines.kt`, TP = `.../harness/TapPoint.kt`.

---

## Task 1: BackKeyTest (14 tests)

Context checked in app code:
- `MainActivity.onBackPressed` is at `android/src/main/java/chat/simplex/app/MainActivity.kt:91-118`. With no enabled callbacks it calls `finish()`. It never finishes during a Forward: it drops `sharedContent` and reopens `fromChatInfo.id`.
- On Android, `BackHandler` is `androidx.activity.compose.BackHandler` (`common/src/androidMain/.../platform/Back.android.kt:7-8`).
- `ModalManager.start/end/center/fullscreen` are all one `shared` instance on Android (`ModalView.kt:252-255`), so `ModalManager.end.*` and `ModalManager.start.*` count the same stack.
- Android `escape()` is `Espresso.pressBack()` (AUT:210-213). It sends a real KEYCODE_BACK, and Espresso **throws if the press closes the app**. So every `escape()` already asserts that MainActivity was not killed, and the extra `check(!mainActivityFinished())` lines are redundant (harmless).
- Nothing in the app overrides `onKeyPreIme` or `KEYCODE_BACK` (grep over androidMain and android/src/main). When the IME is shown, the platform's ImeInputStage consumes the first Back before the app sees it.
- **Cross-cutting (CONFIRMED, see H1):** Android `SimplexApp.onCreate` installs `GlobalExceptionsHandler` and a main-Looper wrapper that catches every Throwable (`android/src/main/java/chat/simplex/app/SimplexApp.kt:57-72`). The handler recovers by closing the top modal, or by setting `chatModel.chatId = null`, then recreating the activity (`common/src/androidMain/.../platform/UI.android.kt:70-115`). Those recovery actions are exactly the state changes BackKeyTest asserts after Back. So a crash during the Back transition can turn a test **green**. Every test below inherits this risk until H1 is fixed.

| test | feature / user scenario | real user case? | how it asserts | catches / misses | problems | verdict |
|---|---|---|---|---|---|---|
| `back_chatToChatList` (BKT:17-25) | Back from an open chat returns to the chat list | Yes, the most common Back | `chatId == null && !exists(Bar title)`; the activity is not finished | Catches: ChatView `BackHandler` (`ChatView.kt:1225-1227`) removed or disabled; Back finishing the activity. Misses: Back leaving a **blank screen** (nothing asserts the list shows); losing the draft | BKT:23 asserts only absences plus a model field, so "list visible" is never checked (CONFIRMED). If ChatView crashes, H1 sets `chatId = null`, which satisfies this exact poll (PLAUSIBLE masking). BKT:24 is redundant with Espresso's own check | **STRENGTHEN**: after Back, `waitFor(Ui.ChatList.Row(bobId))`. Absorb `back_hidesKeyboardFirst` (below) and add a draft check: type "draft", Back (IME), Back (list), reopen, field holds "draft" |
| `back_closesChatSearchBeforeChat` (BKT:27-46) | In-chat search: the first Back closes search and restores all messages; the second leaves the chat | Yes | Model `itemTexts()` for the filter results; `exists(search)` and `exists(title)`; model `chatId` | Catches: the `onBackClicked` branch order in `ChatView.kt:1215-1222` (search before back()); the search not reset. Misses: the **UI** not showing "banana" again after the reload; the content filter reset that the catalogue row promises (`contentFilter.value = null`) | BKT:37 and BKT:41 check filtering and restore through `chatModel.chatsContext.chatItems` only (SCR:226). The LazyColumn could still render the filtered list (CONFIRMED model-only). It never asserts that the keyboard was shown, but `hideKeyboard()` tolerates that | **STRENGTHEN**: capture `bananaId = chat.itemId("banana")` (it already does at BKT:34). Then use `!exists(Ui.Chat.Item(bananaId))` during search and `exists(Ui.Chat.Item(bananaId))` after Back. No new keys needed |
| `back_exitsSelectionMode` (BKT:48-62) | Back ends multi-select; the chat stays | Yes | `Ui.Chat.Selection` title holds "Selected 2"; after Back it is gone, the Bar title exists, and `chatId` is unchanged | Catches: `SelectedItemsCounterToolbar`'s `BackHandler` (`SelectableChatItemToolbars.kt:26-27`) removed, or ChatView's handler winning instead (it would leave the chat). Good | English string "Selected 2" (fine) | **KEEP** |
| `back_closesContactAndGroupInfo` (BKT:64-84) | Back closes contact info, then the invite-members sheet after group creation, then group info | Yes (three cases merged) | `ModalManager.end.hasModalsOpen()`/`openModalCount()`, `exists(Ui.Info.Alias)`, model `chatId` | Catches: `ChatInfoView.kt:69`, `AddGroupMembersView.kt:44` and `GroupChatInfoView.kt:74` handlers. Misses: the invite sheet's **UI**. BKT:76 accepts any single modal over the "team" chat | BKT:76 and BKT:79 are model-only for the invite sheet (CONFIRMED). The H1 recovery `ModalManager.start.closeModal()` produces the same end state as a correct Back (PLAUSIBLE masking) | **STRENGTHEN**: at BKT:76 also `exists(Ui.InviteMembers.Invite)`, and at BKT:79 `!exists(Ui.InviteMembers.Invite)`. The key already exists and tests act on it |
| `back_hidesKeyboardFirst` (BKT:86-96) | With the keyboard up, the first Back only hides it | Yes, but this is platform behaviour | `keyboardShown()` from window insets; the chat stays open | Catches: tapping the message field not opening the IME. It can only catch an app regression if the app starts intercepting Back before the IME (no `onKeyPreIme` today, CONFIRMED by grep), so it mostly tests Android itself | Low value on its own. It costs a full `connect(bob)` setup (60 s connect budget) for one platform assertion | **MERGE** into `back_chatToChatList` as its first half (same setup, and the realistic "I was typing, press Back twice" flow) |
| `back_clearsChatListSearch` (BKT:98-106) | Back with text in the chat-list search clears it; the next Back exits | Yes | `editableText(Search) == ""`; `backFromRoot()` proves the handler is gone afterwards | Catches: the `hasText` `BackHandler` (`ChatListView.kt:819-822`) missing, or never removed (the second Back would not finish). Misses: the list staying filtered (there are no chats, so a filter has no visible effect) | None serious | **KEEP** (optionally add one chat to see an unfiltered row come back; that costs a connect) |
| `back_closesUserPicker` (BKT:108-115) | Back hides the profile picker sheet | Yes | `!exists(Ui.UserPicker.Sheet)`; the key is set only while `pickerIsVisible` (`UserPicker.android.kt:150`); no modals; `chatId` null | Catches: the `UserPicker.kt:54-57` handler missing; Back finishing the activity | – | **KEEP** |
| `back_closesNewChatSheet` (BKT:117-124) | Back closes "Start new chat" | Yes | Model `newChatSheetVisible`, no modals, `!exists(CreateLink)` | Catches the handler missing or the sheet staying visible | BKT:123 is redundant | **KEEP** |
| `back_closesModalsOneByOne` (BKT:126-136) | Nested settings: each Back closes one level, and a final Back exits | Yes | Modal count plus a positive UI key at each level (`ColorMode`, then `Settings.Appearance`, then neither) and `backFromRoot()` | Catches: `ModalView.kt:40` closing everything at once, or nothing; a stuck handler preventing exit. Good mix of UI and model | – | **KEEP** |
| `back_onChatListRootFinishesActivity` (BKT:138-148) | Back at root finishes MainActivity; relaunch reuses the running core | Yes. Android-only, high value | `isFinishing`, then relaunch, row visible, `getChatCtrl()` identical, `chatRunning` | Catches: `finish()` replaced by moveTaskToBack or no-op; a core restart or DB re-open on relaunch (very costly for users). Misses: the launcher intent path (`ActivityScenario.launch` uses an explicit intent with no MAIN/LAUNCHER action) | `mainActivityFinished()` (BKH:13) is `all { isFinishing }`, which is vacuously true for an empty list (CONFIRMED). It is harmless here because finishing then destroying is also fine | **KEEP** |
| `back_duringForward_returnsToSourceChat` (BKT:150-160) | Back on the forward picker cancels the forward and returns to the source chat | Yes. Android-only code path | Model `sharedContent == null` and `chatId == bobId`, plus the Bar title UI | Catches: the Forward exclusion in `canFinishActivity` (MainActivity.kt:96) being lost (the app would exit; Espresso throws); the chat not reopened. Very good regression target | Could also assert `!exists(Ui.Share.Row(...))` | **KEEP** |
| `back_onboardingProfile_returnsToWelcome` (BKT:162-172) | Back on "Your profile" during onboarding goes to Welcome, with no profile created | Yes | Model stage, plus `GetStarted` shown and `ProfileName` gone, plus no users | Catches: onboarding back mis-routed or a profile accidentally created. Solid | `hideKeyboard()` swallows "keyboard never showed" (BKH:48), so the 300 ms auto-focus claim in the catalogue is unverified (not this test's job) | **KEEP** |
| `back_onboardingYourNetwork_finishesActivity` (BKT:174-188) | Back on a step with no close button exits; relaunch resumes at the same step | Yes | Model stage, `seesText("Your network")`, finish, relaunch | Catches: Back changing the onboarding stage or restarting onboarding; a handler accidentally added | Text match on "Your network" (English; fine) | **KEEP** |
| `back_closesWhatsNew` (BKT:190-196) | Back closes What's New to Help & support | Yes | `openModalCount()==3 && !exists(Ui.Help.WhatsNew)`, then count 2 and the row exists | **Duplicate**: shared `WhatsNewTest.whatsNewFromSettings` (`common/src/uiTestShared/.../main/WhatsNewTest.kt:15-21`) runs on Android too (`android/build.gradle.kts:96`). There `escape()` is the same Espresso Back, and it asserts more strongly (`whatsNew.shown()`, the title text) | BKT:193 never checks positively that What's New is open; any third modal passes (CONFIRMED weak) | **DROP** |

**Counts:** KEEP 9, STRENGTHEN 3, MERGE 1, DROP 1.

**Catalogue gap:** 05-android-only §13 lists 17 rows, but 3 are not implemented:
- `pendingReview_closeSupportChat_returnsToChatList`
- `back_onProtectedActionPrompt_leavesUnlockScreen`
- `back_onSetPasscode_cancelsAndLeavesLockOff`

The support-chat one is the only §13 row backed by real Android-only app logic beyond MainActivity (`MemberSupportChatView.kt:33-47,64-72`; ChatView skips its handler when `secondaryContextFilter != null`, `ChatView.kt:1225`). It is worth more than four of the tests that do exist.

---

## Task 2: harness masking audit

Severity scale: **H** = can turn a broken app green in common tests, **M** = can hide a realistic class of bugs in some tests, **L** = narrow.

### H1. App crashes are swallowed on both platforms, and Android "recovers" into the asserted state (H, new: the plan does not list it)
- **Android:** `SimplexApp.kt:57-72` registers `GlobalExceptionsHandler` and wraps the main Looper in `try { Looper.loop() } catch (Throwable)`, forwarding to the handler. The handler logs, closes the top modal or sets `chatId = null`, recreates the activity, and posts an "App was crashed" alert (`UI.android.kt:70-115`). The instrumentation runs the real `SimplexApp`. No harness code replaces the handler or looks for the alert (grep for `UncaughtExceptionHandler` / `app_was_crashed` in androidTest: none).
- **Evidence it happens:** plan §15.5 item 4 (`plans/ui-tests/2026-10-10-desktop-ui-test-dsl-plan.md`). A QR-scanner disposal crash was seen only indirectly ("the activity then never reaches DESTROYED"), not as a test failure.
- **Desktop:** `bootJvm` (desktop UiTestApp.kt:27-45) does not register a handler. App coroutines on `Dispatchers.Default` or the `singleThreadDispatcher` (`withBGApi`/`withLongRunningApi`, `Utils.kt:42-52`) that throw go to the JVM default handler, which prints to stderr, and the test keeps running. Only exceptions on the Compose test dispatcher reach the test, and those then meet the allowlist (H5).
- **Masking scenario:** a composable throws during a Back or close transition. The handler closes the modal or clears `chatId`, and BackKeyTest's polls (`!hasModalsOpen`, `chatId == null`) pass. Any test whose next assertion is satisfied after an activity recreate passes with a crash in the log.
- **Affected:** all 157 tests, Android worst (the recovery is state-changing).
- **Fix** (harness only, no app keys):
  1. In `UiTestApp.bootOnce` and `bootJvm`, wrap `Thread.getDefaultUncaughtExceptionHandler()` with a recorder: append the throwable to a list, then delegate so app behaviour is unchanged.
  2. In `UiScope.finish()` (AUT:96) and the desktop `uiTest` finally, fail if the list is non-empty, unless the throwable matches an explicitly allowlisted stack origin.
  3. In `poll()`, check the list on every iteration and fail fast with the stack trace. A recovered crash should never wait out a 10 s timeout and then pass on the recovered state.

### H2. `enterWhenSendable` re-presses Enter / Send, hiding a dropped first input (H/M, acknowledged in plan §15.4.2 but under-rated)
- **Where:** SCR:270-276. Press, wait 5 s, and if the field still has text and Send is enabled, press again. On Android `pressEnter` on the message field is `click(Ui.Chat.Send)` (AUT:259-262), so this re-taps Send.
- **Masking scenario:** the plan itself records a real symptom: "after a context-menu action, the first Enter is dropped (cause still open)" (plan §15.2 item 2; §15.4 item 17 "Popups swallow the next pointer or key event"). For a user that is "I picked Reply, typed, hit Enter, nothing sent". On Android the equivalent is "the first tap on Send only dismisses focus or the IME". The harness reports neither.
- **Affected:** 12 `send(`, 1 `replaceAndSend(` across 8 files.
- **Double-send:** guarded correctly, because `inProgress` disables Send (`ComposeView.kt:182, 632`). Keep that guard.
- **Fix:**
  1. Desktop `pressEnter` already waits for popups to close (DUT:294). Retest without the retry. If the drop no longer reproduces, delete the retry. If it still does, it is an app bug: fail with a clear message ("first Enter dropped").
  2. At minimum, make the retry *visible*: log it and fail the test unless it is annotated `@KnownIssue("first-enter")`, so each masked case is enumerated.

### H3. `quiesce()` / `openContextMenu` deliberately wait out real app races (M, new as a framing)
- **Where:** HLP:20-24 waits for no API in flight and 1 s without events. HLP:63-67 makes every `rowMenu` (34 uses) wait for `UiIdle.inFlight().isEmpty()` plus a 500 ms settle before long-pressing.
- **Masking scenario:** plan §15.5 item 1 (the group mute setting reverts after joining, from an event applied after the command reply) was "fixed" in the test by adding `quiesce()`. That is a confirmed user-visible race, now structurally unreachable. Tests never act while events are flowing, so a whole class of bugs is excluded by design: reordering races, stale-label races, the member-review races in MEMORY.
- **Affected:** 12 `quiesce()` calls, `ownGroup`, `joinGroupFromList`, `switchTo`, all `rowMenu` users.
- **Fix:** keep `quiesce` for setup, but never as the remedy for a seen app race. When a race is found, keep one test that does *not* quiesce, marked `@KnownIssue` (expected-fail), so the bug stays tracked. In `openContextMenu`, drop the in-flight wait. The label poll in `rowMenu` (SCR:66-68) already tolerates late updates. Or keep the wait but assert that the label is right on the **first** frame the menu shows, to catch stale-label bugs.

### H4. Poll deadline extension hides latency regressions; drain silently abandons hung work (M, new)
- **Where:** AUT:132-137 and DUT:316-324. If anything that was in flight at the deadline is still in flight, a poll's nominal timeout (often 10 s) silently grows by up to 60 s (`EXTENSION_MS`). `drain()` (Android UiTestApp:188-196, desktop UiTestApp:49-57) waits 30 s, then *abandons* stuck items with only a log line, and the test still passes.
- **Masking scenario:** an API call regresses from 1 s to 50 s ("Loading chats…" stall, support-chat slowness, reconnect delay, all in MEMORY), and every UI wait on it still passes. A command that never returns (hang) passes if the test's last assertion doesn't depend on it, and is then abandoned at drain.
- **Affected:** every test.
- **Fix:**
  1. Record the extension: if a poll succeeds after its nominal deadline, emit a `UITEST SLOW` line with the in-flight `what`. Fail if the overrun is more than N s, unless the poll passed `slow = true`.
  2. Make `drain()` abandonment a test failure (except for an explicit list of known-long background jobs).

### H5. Desktop known-issue allowlist swallows real "database closed" / "Controller is not initialized" crashes (M, acknowledged in plan §15.4.2 as a limit)
- **Where:** DUT:33-56 and 352-358. Any non-AssertionError whose cause chain contains either substring, reported after the body completed, is swallowed. Errors before the body are retried up to 3 times.
- **Right part:** body-time assertion failures are never swallowed (`bodyStarted && !bodyCompleted` rethrows; `AssertionError` excluded).
- **Masking scenario:** Compose reports uncaught coroutine exceptions only at the end. So a background "database closed" thrown *during* the body (a stop/start race, plan §15.5 item 3, a real app bug) is indistinguishable from teardown noise and passes. Pre-body retries hide flaky boot crashes.
- **Fix:** match on stack origin, not message. Swallow only when a frame is in `UiTestApp.resetForTest`/`stopCore`/`restartCore`, or in `SetupDatabasePassphrase` (the one known app issue). Count swallowed occurrences per run and print a summary. Combine with the H1 recorder so body-time background exceptions are timestamped and attributed.

### H6. `PeerLines.eventually*` discards every line before the match, and nothing checks unconsumed output (M, new)
- **Where:** PL:17-19 (`repeat(i + 1) { lines.removeAt(0) }`); the same in `eventuallyMessage` (PL:22-24) and `eventuallyMatching` (PL:27-32). No end-of-test check of `pending()`.
- **Masking scenario:**
  - The peer receives the app's message **twice** (duplicate send/delivery): the second copy sits in the buffer and is silently dropped by the next `eventually`.
  - The CLI prints an error before the awaited line (e.g. `error: …`, `bad chat command`, a failed proxy) and it is discarded.
  - An extra message the app should not have sent goes unnoticed.
- **Affected:** 21 `eventually`, 8 `eventuallyMessage` across ~10 files, plus `ownGroup` and `joinGroupFromList`.
- **Right part:** `expects`/`expectsMessage` (PL:7-15) are strict next-line checks. They are the strongest assertions in the harness; prefer them where order is deterministic.
- **Fix:**
  1. When `eventually` drops lines, fail if any dropped line matches an error pattern (`^error`, `chat error`, `CRITICAL`) or equals the awaited line (a duplicate).
  2. In `UiScope.finish()` (before `peers.forEach { it.stop() }`), fail if any peer has unconsumed message-shaped lines (`^\d\d:\d\d \S+> `). Tests that expect leftovers can call a `peer.discard()`.

### H7. Android message-field input bypasses the keyed node and retries replacement (M, new)
- **Where:** AUT:217-229 and 267-272. `Espresso.onView(isAssignableFrom(EditText))` targets *any* EditText. The `key` argument is ignored for both write and read. `replaceText` is retried for up to 10 s until the field holds the value. `typeInto` on the message field is `replace(existing + text)` (AUT:238-241), so no keystroke path is exercised.
- **Masking scenario:** a draft-restore or recomposition race overwrites what the user just typed (the classic "text vanished when the chat finished loading" bug). The loop simply writes it again until it sticks, and the test passes. Cursor-position, input-filter and IME-composition bugs are invisible by construction.
- **Affected:** every Android `send`/`replaceAndSend`/`typeInto(Ui.Chat.Message)` (13+ sites, plus everything that sends via `ChatScreen`).
- **Fix:**
  1. Scope the view matcher to the composable: match the EditText whose parent AndroidView is under the `Ui.Chat.Message` node (or assert exactly one EditText).
  2. Write once. If the value does not stick, fail with "field reverted to `X`" instead of retrying.
  3. Keep `replaceText` (setText is the only reliable path on Waydroid), but add a `getText()` re-check after a settle, to catch late overwrites.

### H8. Desktop `escape()` calls the back stack directly (M, acknowledged in plan §15.4.2)
- **Where:** DUT:200-204 vs the real handler in `common/src/desktopMain/.../DesktopApp.kt:126-128` (Window `onKeyEvent`, Escape **KeyUp**, after focused nodes had their chance).
- **Masking scenario:**
  - A focused TextField or an open DropdownMenu/Popup consumes Esc in the real app (the popup closes, the screen stays). The harness instead pops the top screen.
  - A regression in the Window key handler (KeyDown vs KeyUp, a handler order change) is never exercised.
  - Desktop Esc tests (04 `keyboard_escClosesTopModal` and the shared tests using `escape()`, 22 uses in 4 files) verify the BackHandler stack, not Esc.
- **Fix** within the rules (no app keys):
  1. Wrap TestRoot content in `Modifier.onKeyEvent { if (it.key == Key.Escape && it.type == KeyUp) simplexWindowState.backstack.lastOrNull()?.invoke() != null else false }`. Make `escape()` send `ui.onRoot().performKeyInput { pressKey(Key.Escape) }`, so focus and popup consumption happen first.
  2. Sharing the exact app lambda would mean extracting it from DesktopApp.kt. That is an app refactor: get user authorization first.

### H9. Model-only "UI" checks in shared helpers (L/M, partly acknowledged)
- `ChatListScreen.gone()` (SCR:72-74) checks `chatModel.chats` only (acknowledged). A deleted chat still rendered (stale derived list) passes. 4 uses (ContactsTest x2, ChatListRequestsCardsTest, ChatListMenusPendingTest). **Fix:** capture `id = chatId(name)` first, then poll `chats.none{…} && !exists(Ui.ChatList.Row(id))`. `Row` is already an acted-on key.
- `UserPickerScreen.switchTo` (SCR:137-141) checks only model `currentUser` and `switchingUsersAndHosts`, then `quiesce`. The UI could still show the previous user's list or picker. 5 uses. **Fix:** also poll for a row of a chat known to belong to the new user, or `!exists(Ui.UserPicker.Sheet)` on Android.
- `ChatScreen.itemTexts()` (SCR:226) is a model list. It is used for "item appears / disappears" in 3 places (2 in BackKeyTest). **Fix:** use `itemId(text)` (model + `exists(Ui.Chat.Item(id))`), which is already correct, or `!exists(Ui.Chat.Item(id))`.
- `joinGroupFromList` (HLP:69-77) checks model `membership.memberStatus == MemConnected`. That is acceptable as setup, but not as the assertion of a join test.
- `ChatListScreen.open` (SCR:76-79) does not confirm the right chat opened. It is mostly safe, because later `itemId` lookups would fail in the wrong chat. Cheap fix: `poll { chatModel.chatId.value == id && exists(Ui.Chat.Bar.part("title")) }`.
- `exists()` (AUT:144, DUT:134) means "in the semantics tree", not "displayed". Desktop keeps the chat list and chat composed under START/END modals, so a positive `exists` on desktop can be true while the node is covered. Most tests pair it with a negative check of the overlay; review any lone positive `exists` used as "is shown".

### H10. Desktop `focusBeforeTap` and the `replaceText` fallback force state the app might not reach (L, acknowledged for focus)
- DUT:224-230 requests focus through semantics before every tap on a focusable node. It hides "the first tap after typing is lost" (plan §15.5 item 2, still unverified with a real mouse) and "clicking the field doesn't focus it".
- DUT:257-261: if select-all plus type doesn't produce the text within 2 s, it falls back to `performTextReplacement`, which hides a broken typing path in the replaced field (ProfileScreen.rename, replaceAndSend).
- **Fix:** log each fallback. Make the focus pre-step skip text fields, so their tap path is exercised.

### H11. Smaller issues (L)
- **Android `clipboardText()` (AUT:320-324):** it falls back to the device clipboard, which no reset clears (no `clearPrimaryClip` in CommonReset or the Android harness, CONFIRMED). If Share produces no intent, `AddressScreen.create` can return the previous test's link. In current tests that link is from a deleted DB, so the CLI connect fails instead of passing, which keeps the risk low. Fix: `clearPrimaryClip()` in reset, and require the clip to change during the action.
- **`mainActivityFinished()` (BKH:13):** vacuously true when no MainActivity is tracked. Fix: `live.isNotEmpty() && live.all { it.isFinishing } || <finished-and-destroyed observed>`.
- **`relaunch` (BKH:20-26):** uses an explicit-component intent, so the launcher (MAIN/LAUNCHER) handling in MainActivity is not exercised.
- **Android `rightClick` (AUT:202-208):** long-presses the node centre (no `tapPointInNode`), so it can hit a nested link or image inside a bubble. That fails rather than masks; mentioned for consistency.
- **`isEnabled`:** always true for `SectionItemView` keys (acknowledged in the plan). `poll { isEnabled(...) }` before a click on those never proves enablement.

### Helpers that do the RIGHT thing (keep them)
- **`ChatScreen.itemId` (SCR:215-222):** model id **and** `exists(Ui.Chat.Item(id))`. It proves the item is rendered. Use it as the template for any presence check.
- **Android `escape()` = `Espresso.pressBack()` (AUT:210-213):** a real KEYCODE_BACK through `onBackPressed` and the dispatcher, and it throws if the app exits. Exactly right.
- **Non-message `typeInto`/`replaceText` (AUT:243-257, DUT:239-245):** type through Compose text input, then poll for the **exact** expected content. Dropped or extra characters fail.
- **`click()` with `tapPointInNode` (TP; AUT:178-189, DUT:206-218):** a real pointer injection at a free point of the target. Overlays and scrims still intercept through hit-testing, so occlusion bugs are caught. It avoids tapping a nested child button that a user would not aim at.
- **`waitPopupsClosedUnlessInside`:** refuses to act through an open popup. A popup that never closes fails the test.
- **`PeerLines.expects` / `expectsMessage`:** strict next-line checks. `sends` waits for the echo.
- **`connect()` via `apiAddContact` (HLP:26-32):** setup-only, and fine, because dedicated UI tests cover connecting via link/address (OneTimeLinkTest, ContactsTest, ChatListRequestsCards*).
- **The `enterWhenSendable` double-send guard:** relies on the app's `inProgress` disabling Send. Correct.
- **The allowlist never swallows body-time failures or AssertionErrors** (DUT:48).
- **Failure diagnostics:** a screenshot, the visible texts, the in-flight API list and the last condition error on every poll timeout (AUT:135-136, DUT:321-322). Watchdogs dump threads.
- **Desktop per-test `TestClipboard` / `RecordingUriHandler`:** no cross-test leakage.

---

## Summary

**(1) BackKeyTest:** KEEP 9, STRENGTHEN 3, MERGE 1, DROP 1.
- **DROP** `back_closesWhatsNew`: it duplicates shared `WhatsNewTest.whatsNewFromSettings`, which already runs on Android with the same Espresso Back and asserts more.
- **MERGE** `back_hidesKeyboardFirst` into `back_chatToChatList`: it mostly tests the Android IME, not app code.
- **STRENGTHEN** with existing keys:
  - `back_chatToChatList`: assert the list row is visible, and add a draft check.
  - `back_closesChatSearchBeforeChat`: check `Ui.Chat.Item(bananaId)` instead of model `itemTexts()`.
  - `back_closesContactAndGroupInfo`: check `Ui.InviteMembers.Invite` instead of a modal count.

The genuinely Android-only value is concentrated in three tests:
- `back_onChatListRootFinishesActivity` (finish without a core restart)
- `back_duringForward_returnsToSourceChat` (the MainActivity Forward exclusion)
- `back_onboardingYourNetwork_finishesActivity`

The rest re-verify BackHandler wiring that desktop Esc tests also reach. The most valuable Android Back row, `pendingReview_closeSupportChat_returnsToChatList` (Android-only support-chat logic), is not implemented, and neither are the two passcode Back rows.

**(2) Top harness risks, ranked:**
1. **H1, crashes swallowed (new, high).** On Android, `SimplexApp` installs a catch-all Looper wrapper and `GlobalExceptionsHandler` that close the top modal or clear `chatId`, then recreate the activity. Those are the very states BackKeyTest and many others assert, so a crash can make a test pass. On desktop, background-coroutine exceptions only reach stderr. Fix: a recording uncaught-exception handler in boot, failing at `finish()` and inside `poll()`.
2. **H2, `enterWhenSendable` second Enter / Send tap.** It hides a dropped first input that the plan itself observed after context-menu actions (a real "Enter didn't send" bug). Retest without it and delete it, or make each retry a tracked known issue.
3. **H3, `quiesce()` / `openContextMenu` idle waits.** They were used to wait out a confirmed app race (§15.5 item 1, group mute revert), and they keep every test away from event traffic, so the race classes in MEMORY are unreachable. Keep one non-quiesced known-issue test per race found.
4. **H4, poll +60 s silent extension and `drain()` silently abandoning hung calls.** Latency regressions and hangs pass. Log and fail on overruns and abandonment.
5. **H6, `PeerLines.eventually` drops all intermediate lines, and nothing checks leftover peer output.** Duplicate deliveries, CLI errors and extra sends vanish.
6. **H7, the Android message field.** It writes through any EditText (ignoring the key) and retries `replaceText` for 10 s, which hides "typed text overwritten by draft restore" races.
7. **H5, the desktop allowlist.** It matches by message substring, so body-time "database closed" app bugs pass. Match by stack origin.
8. **H8, desktop `escape()` bypasses key dispatch.** Popup and focus consumption of Esc is not tested.
9. **H9, model-only helpers:** `gone()`, `switchTo`, `itemTexts`; plus **H10/H11** (focusBeforeTap, replaceText fallback, Android clipboard fallback, vacuous `mainActivityFinished`).

**(3) Overall judgement:** the harness's *primitives* are honest:
- real pointer injection with hit-testing
- a real Android Back key
- exact-text polling
- strict `expects`
- `itemId` requiring a rendered node
- rich failure output

When a test asserts through those primitives it catches real regressions. But the *helpers layered on top* consistently choose "make the test pass" over "report what the app did":
- retrying lost input
- waiting out races with `quiesce`
- extending deadlines while work is in flight
- abandoning hung calls
- discarding peer lines
- allowlisting crashes by message

Worst of all, on Android the app's own crash handler is left in place. It turns crashes into the very state transitions the tests check. Today the harness smooths over four documented classes of real app behaviour:
- the dropped first Enter
- the mute revert race
- the "database closed" races
- the camera disposal crash

None of them shows up as a red test. Fix H1 through H4 (all harness-only, no new app keys) before treating a green run as evidence that the app works.
