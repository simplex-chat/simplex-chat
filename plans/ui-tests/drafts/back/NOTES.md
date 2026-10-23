# §13 The system Back key: drafted tests

Source: `plans/ui-tests/catalogue/05-android-only.md`, section "### 13. The system Back key (17 tests)".
Drafted: 14 tests. Skipped: 3 (see the end).

## Files

| File | Destination | What |
|---|---|---|
| `BackKeyTest.kt` | `android/src/androidTest/java/chat/simplex/app/ui/back/` | the 14 tests, `class BackKeyTest : UiTest()` |
| `BackKeyHelpers.kt` | same package (or move into the Android harness) | Android-only helpers: `mainActivityFinished()`, `backFromRoot()`, `relaunch {}`, `keyboardShown()`, `hideKeyboard()` |
| `screens.kt` | merge into `common/src/uiTestShared/.../harness/Screens.kt` | `App.settings()`, `ChatScreen.openSearch()`, `ChatScreen.itemTexts()` |
| `keys.diff` | `git apply` from `/home/user/wt-uitest` | new keys in `Ui.kt` and one `.ui(...)`/`uiKey =` per element |

`git apply --check` of `keys.diff` passes against the current worktree. Nothing was compiled or run.

Line numbers below are for the current worktree `/home/user/wt-uitest`, not for the catalogue's `d72f68e6d`. Paths: `C/` = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`, `A/` = `.../androidMain/kotlin/chat/simplex/common/`, `APP/` = `apps/multiplatform/android/src/main/java/chat/simplex/app/`.

## Keys added (`keys.diff`)

| Key | Tag | Attached at |
|---|---|---|
| `Ui.ChatList.Search` | `chatList.search` | `C/views/chatlist/ChatListView.kt:804`, the modifier of the chat list's `SearchTextField` |
| `Ui.UserPicker.Sheet` | `userPicker.sheet` | `A/views/chatlist/UserPicker.android.kt:149`, the picker's full-screen box, **only while the picker is visible** (`pickerIsVisible`) |
| `Ui.Settings.Help` | `settings.help` | `C/views/usersettings/SettingsView.kt:106`, "Help & support" |
| `Ui.Help.WhatsNew` | `help.whatsNew` | `C/views/usersettings/SettingsView.kt:162`, "What's new" |
| `Ui.Onboarding.GetStarted` | `onboarding.getStarted` | `A/views/onboarding/SimpleXInfo.android.kt:13`, "Get started" |
| `Ui.Onboarding.ProfileName` | `onboarding.profileName` | `C/views/WelcomeView.kt:288`, mobile profile name field (`ProfileNameField(uiKey =)`) |
| `Ui.Onboarding.CreateProfile` | `onboarding.createProfile` | `C/views/WelcomeView.kt:294`, mobile "Create profile" button |
| `Ui.Chat.More` | `chat.more` | `C/views/chat/ChatView.kt:1426`, the chat toolbar's ⋮ button |
| `Ui.Chat.Menu(action)` | `chat.menu.search` | `C/views/chat/ChatView.kt:1298,1328`, "Search" in the ⋮ menu (direct and group branches) |
| `Ui.Chat.Bar.part("search")` | `chat.bar.search` | `C/views/helpers/DefaultTopAppBar.kt:87`, the app bar's search field (reuses the bar's existing `uiKey`, as `part("title")` does at :42); no `Ui.kt` change |
| `Ui.Chat.Selection` (read as `.part("title")`) | `chat.selection.title` | `C/views/chat/SelectableChatItemToolbars.kt:43-47`, `DefaultAppBar(uiKey =)` of the "Selected N" toolbar |
| `Ui.Chat.ItemMenu(id, "forward")` | existing function | `C/views/chat/item/ChatItemView.kt:478` |
| `Ui.Chat.ItemMenu(id, "select")` | existing function | `C/views/chat/item/ChatItemView.kt:503` (message-content menu only), via a new `uiKey` parameter of `SelectItemAction` (:941) |

Side effect of the `SelectItemAction` parameter: `GroupChatInfoView.kt:1211` called it with a trailing lambda, which would now bind to `uiKey`; the diff changes that call to `selectItem = { … }`. The onboarding keys are on the mobile composables only (`CreateFirstProfileMobile`), not on `CreateFirstProfileDesktop`.

## Harness facts these drafts depend on

1. **`escape()` cannot be used when Back finishes the activity.** It calls `Espresso.pressBack()` (`UiTest.kt:178-181`), whose `PressBackAction` throws `NoActivityResumedException` when no activity is left resumed (espresso-core 3.6.1, `PressBackAction.perform` → `waitForStageChangeInitialActivity`). `backFromRoot()` uses `Espresso.pressBackUnconditionally()`.
2. **After the activity finishes there is no compose root.** `rule.waitForIdle()` then waits 2 s for a root and goes on (compose ui-test 1.8.2, `ComposeRootRegistry_androidKt.waitForComposeRoots(…, true)`: `CountDownLatch.await(2 s)`), so each `poll` iteration takes about 2 s; node queries (`exists`, `texts`) would throw. `mainActivityFinished()` reads only `ActivityLifecycleMonitorRegistry`.
3. **On Android the chat list stays composed under an open chat.** `AndroidScreen` keeps `StartPartOfScreen` composed and only translates it (`C/App.kt:313-366`), so chat-list keys still exist while a chat is open; the tests use `chatModel.chatId` and `Ui.Chat.Bar.part("title")` (the ChatView leaves composition once the slide-out ends, `C/App.kt:347-366`).
4. **The user picker is always composed on Android** (`A/views/chatlist/UserPicker.android.kt:131-190`, it is only translated off screen), so `Ui.UserPicker.Settings` always exists. Visibility needs the conditional `Ui.UserPicker.Sheet`.
5. **Only the top modal is composed.** On Android all `ModalManager`s are the same instance (`C/views/helpers/ModalView.kt:252-258`), `showInView` composes only `modalViews[count-1]` (:197-226), and `modalCount` drops as soon as `closeModal()` runs (:168-180). The tests read `ModalManager.start.openModalCount()` / `hasModalsOpen()` and wait for the closed modal's keys to disappear before the next Back, so a second Back never reaches a modal that is still sliding out.
6. **Soft keyboard.** No IME reader existed; `keyboardShown()` reads `WindowInsetsCompat.Type.ime()` from the resumed activity's decor view. `hideKeyboard()` waits up to 2 s for the keyboard, then `Espresso.closeSoftKeyboard()`, then waits for it to be hidden; it is used wherever the catalogue says "the keyboard is hidden first and Back reaches the app". The IME consumes Back before the app sees it (ViewRootImpl's IME input stage); Espresso's `pressBack` does not close the keyboard first (checked in the 3.6.1 bytecode).
7. **Focused Compose text fields do not consume Back** unless text is selected (`CoreTextField.previewKeyEventToDeselectOnBack`, foundation 1.8.2), so the chat-list search field keeps working with focus.

## Tests

### 1. `back_chatToChatList`
- Row: Chat → chat list; chat with bob open; `C/views/chat/ChatView.kt:1215-1226`.
- Asserts: after Back, `chatModel.chatId == null` and the chat bar is gone; `MainActivity` is not finishing.
- Code: `ChatInfoToolbar` registers `BackHandler(onBackClicked)` on Android (`ChatView.kt:1216-1227`); `back()` sets `chatModel.chatId = null` (`ChatView.kt:396-400`).

### 2. `back_closesChatSearchBeforeChat`
- Row: Search first; chat search open with text; `ChatView.kt:1215-1222`.
- Asserts: bob sends "apple" and "banana"; ⋮ → Search, type "apple"; the loaded items hold "apple" and not "banana". After hiding the keyboard, the first Back closes the search field (`chat.bar.search` gone, `chat.bar.title` back) and "banana" is loaded again; the chat stays open. The second Back returns to the chat list.
- Code: `onBackClicked` with `showSearch` calls `onSearchValueChanged("")`, sets `showSearch = false` and `contentFilter = null` (`ChatView.kt:1216-1223`); `onSearchValueChanged` reloads through `apiFindMessages` (`ChatView.kt:280-292`); the search field opens with focus and shows the keyboard after 200 ms (`C/views/helpers/SearchTextField.kt:46-51`); the title key is applied only when search is closed (`DefaultTopAppBar.kt:40-44`).
- Uncertainty: the catalogue names "the content filter". The test checks the text filter (the observable result). `contentFilter` is local state with no key; it is cleared on the same line of code.

### 3. `back_exitsSelectionMode`
- Row: Selection mode; select two messages; `C/views/chat/SelectableChatItemToolbars.kt:20-30`.
- Asserts: "Select" on "one", then a tap on "two", gives "Selected 2" (`selected_chat_items_selected_n` = "Selected %d", strings.xml:520) in the selection toolbar. Back removes the toolbar, the normal chat bar returns, and `chatId` is still bob's.
- Code: `SelectedItemsCounterToolbar` registers `BackHandler { selectedItems.value = null }` (`SelectableChatItemToolbars.kt:24-26`); in selection mode a clickable overlay covers each item and toggles it (`ChatView.kt:2237-2241`); text messages have no merge category, so one tap selects one item (`ChatView.kt:3344-3370`).
- Uncertainty: the tap on `Ui.Chat.Item(two)` relies on the overlay being drawn above the item at the node's tap point.

### 4. `back_closesContactAndGroupInfo`
- Row: Chat info; `ChatInfoView.kt:67`, `GroupChatInfoView.kt:73`.
- Asserts: contact info (`Ui.Info.Alias` present) closes on Back with no modal left and `chatId` = bob. Then a new group "team" is created; its invite-members sheet (1 modal) closes on Back; group info (the same `LocalAliasEditor`, `Ui.Info.Alias`) closes on Back to team's chat.
- Code: `ChatInfoView` `BackHandler(onBack = close)` (`C/views/chat/ChatInfoView.kt:69`), `GroupChatInfoView` (`C/views/chat/group/GroupChatInfoView.kt:73`), alias editor in group info (`GroupChatInfoView.kt:591`), info opens in `ModalManager.end` (`ChatView.kt:401-425`); create group opens the chat and then `AddGroupMembersView` (`C/views/newchat/AddGroupView.kt:46-62`) with `BackHandler(onBack = close)` (`C/views/chat/group/AddGroupMembersView.kt:43`).
- Uncertainty: the poll for the invite sheet uses `openModalCount() == 1` with `chatId == teamId` to skip the moment before `closeAll()` (new-chat sheet and AddGroupView still open).

### 5. `back_hidesKeyboardFirst`
- Row: Keyboard first; message field focused; `A/platform/UI.android.kt:46-55`.
- Asserts: tapping the message field shows the IME; the first Back hides it; the chat stays open (`chatId` = bob, chat bar present).
- Code: the message field is an `EditText` in `AndroidView` (`A/platform/PlatformTextField.android.kt:101`); the IME consumes Back (harness fact 6).
- Uncertainty: needs a soft keyboard that actually shows on Waydroid under instrumentation (catalogue Need `IME`). If no IME shows, the test fails at "the soft keyboard to show".

### 6. `back_clearsChatListSearch`
- Row: Chat list search text; `C/views/chatlist/ChatListView.kt:811-815`.
- Asserts: with "zz" typed and the keyboard hidden, Back empties the field and `MainActivity` keeps running; the next Back finishes it.
- Code: `BackHandler(hideSearchOnBack)` only while the text is non-empty (`ChatListView.kt:815-822`); the field loses focus when the keyboard closes (`ChatListView.kt:830-835`); "zz" is shorter than a name-search candidate, so no connect-by-name lookup runs (`ChatListView.kt:1156-1166`).

### 7. `back_closesUserPicker`
- Row: User picker; `C/views/chatlist/UserPicker.kt:48-56`.
- Asserts: `Ui.UserPicker.Sheet` exists after opening and is gone after Back; no chat or modal is open and `MainActivity` keeps running.
- Code: `BackHandler { userPickerState.value = HIDING }` while visible (`UserPicker.kt:53-57`); HIDING → GONE (`UserPicker.kt:75-85`); `isVisible()` is `== VISIBLE` (`C/views/helpers/Enums.kt:24-26`), so the key goes as soon as hiding starts.

### 8. `back_closesNewChatSheet`
- Row: New chat sheet; `ChatListView.kt:106-125`.
- Asserts: after Back, `chatModel.newChatSheetVisible` is false, no modal is open, `Ui.NewChat.CreateLink` is gone, and `MainActivity` keeps running.
- Code: `showNewChatSheet` registers `BackHandler { close() }` on Android, and `close` sets `newChatSheetVisible = false` (`ChatListView.kt:105-130`); the sheet's search field is `alwaysVisible`, so it does not take focus or show the keyboard.

### 9. `back_closesModalsOneByOne`
- Row: Nested settings; Settings > "Appearance"; `C/views/helpers/ModalView.kt:40-42`.
- Asserts: 2 modals (Appearance on Your settings) → Back → 1 modal with the Settings rows visible → Back → no modal, Settings gone, user picker hidden, activity still running → Back → `MainActivity` finishes.
- Code: `ModalView` `BackHandler(enabled = enableClose, onBack = close)` (`ModalView.kt:40-42`); Settings opened with `showModalCloseable` (`UserPicker.kt:384-390`, `ModalView.kt:125-129`), Appearance with `showSettingsModal` → `ModalManager.start.showModal` (`SettingsView.kt:45`); the picker hides itself 500 ms after any modal opens (`UserPicker.kt:87-95, 659-662`).

### 10. `back_onChatListRootFinishesActivity`
- Row: Root finishes the activity; `APP/MainActivity.kt:91-117`.
- Asserts: Back on the bare chat list finishes `MainActivity`; a relaunch shows bob's row, with `chatRunning == true` and the same `ChatCtrl` (no core restart).
- Code: with no enabled callback, API ≥ 30 makes `canFinishActivity` true and `finish()` is called (`MainActivity.kt:92-117`).
- Uncertainty: `relaunch` starts a second `ActivityScenario` with the same launch intent as the harness's own (private) scenario. Both end DESTROYED, but which activity the first scenario tracks after the second launch is not verified. A harness-level `relaunch()` that reuses the private `scenario` would be cleaner (open question for the harness, setup item 4 also asks for finishing every `MainActivity` at test end).

### 11. `back_duringForward_returnsToSourceChat`
- Row: Forward in progress; `MainActivity.kt:92-96,107-116`.
- Asserts: "Forward" on bob's message switches to the forward list (`sharedContent is Forward`, chat view gone); Back clears `sharedContent`, sets `chatId` back to bob, the chat bar shows, and `MainActivity` keeps running.
- Code: `forwardItem` sets `chatId = null` and `sharedContent = Forward(…)` (`ChatView.kt:639-642`); `StartPartOfScreen` shows `ShareListView` instead of the chat list (`C/App.kt:376-386`), whose only `BackHandler` is for its search (`C/views/chatlist/ShareListView.kt:93-98`); `canFinishActivity` is false for `Forward`, and with no callbacks the shared content is dropped and `chatId = fromChatInfo.id` (`MainActivity.kt:92-117`).
- Uncertainty: if `ShareListView`'s search were open, `onBackPressed` would do nothing at all (`super.onBackPressed()` is skipped for `Forward`, and `hasEnabledCallbacks()` is true). The test never opens it.

### 12. `back_onboardingProfile_returnsToWelcome`
- Row: Onboarding profile screen; `C/views/WelcomeView.kt:227-242`.
- Asserts: "Get started" → profile screen; after hiding the keyboard, Back sets `onboardingStage` to `Step1_SimpleXInfo`, "Get started" is back, the name field is gone, and no user exists.
- Code: `CreateFirstProfileMobile` is a `ModalView` whose close is `onboardingBackAction` (`WelcomeView.kt:240-243`), which sets `Step1_SimpleXInfo` when there are no visible users (`WelcomeView.kt:229-235`); the name field is focused after 300 ms (`WelcomeView.kt:302-305`); with no user, init sets `Step1_SimpleXInfo` on Android (`C/platform/Core.kt:166-168`).

### 13. `back_onboardingYourNetwork_finishesActivity`
- Row: Screens without a close button, "Your network"; `C/views/onboarding/YourNetwork.kt:61-62`.
- Asserts: after creating profile "alice", the stage is `Step3_ChooseServerOperators` and "Your network" (strings.xml:1373) shows; Back finishes `MainActivity` and leaves the stage unchanged; a relaunch shows "Your network" again with the same stage.
- Code: on Android profile creation goes straight to `Step3_ChooseServerOperators` (`WelcomeView.kt:399-406`), rendered by `YourNetworkView` (`C/App.kt:194-196`) as `ModalView({}, showClose = false, showAppBar = false)` (`YourNetwork.kt:62`), which registers no `BackHandler` (`ModalView.kt:40-42`).
- Uncertainty: `YourNetworkView` runs `prepareChatBeforeFinishingOnboarding()` (`C/views/onboarding/SetNotificationsMode.kt:121-129`), which starts the chat with the preset servers, not the test's (`configureServers` runs only for `app(name)`); on the isolated network this should only mean failed connections. The "Your network" check uses text because the screen has no key; the stage is checked from the model.

### 14. `back_closesWhatsNew`
- Row: What's New; Settings > "Help & support" > "What's new"; `C/views/onboarding/WhatsNewView.kt:170`.
- Asserts: 3 modals (What's new on Help & support on Your settings) → Back → 2 modals with the "What's new" row visible again.
- Code: `WhatsNewView` is a `ModalView(close = close)` (`WhatsNewView.kt:170`); Help & support via `showSettingsModal` (`SettingsView.kt:45,106`); What's new via `showCustomModal` (`SettingsView.kt:162`, :65).

## Skipped rows

| Test | Reason |
|---|---|
| `pendingReview_closeSupportChat_returnsToChatList` | Needs bob's (CLI) group with member admission review, alice joining by its link through the connect UI (catalogue Need `CB`), the support chat auto-opening over the group (03 `joinerPendingReview_supportChatAutoOpens`), and keys for the support chat and its back arrow. None of that is in the harness yet (no helper for review groups or for joining by link from the UI), and the CLI admission command was not verified. Draft together with 03's review rows. |
| `back_onProtectedActionPrompt_leavesUnlockScreen` | Needs Passcode lock on. Setting it through the UI needs keys for the passcode keypad digits (`C/views/localauth/PasscodeView.kt`), "Your privacy", "SimpleX Lock", lock mode and the enable switch. Pre-setting `performLA`, `laMode` and `ksAppPassword` would leak: `resetForTest` restores none of them (`UiTestApp.kt` `presetAndroidPrefs`, `TestPrefs.preset`), so every later test in the process would start locked. Also needs keys for "Migrate to another device", the full-screen "Unlock" button (`SettingsView.kt:475-511`) and the passcode prompt (`C/views/helpers/LocalAuthentication.kt:41-58`). |
| `back_onSetPasscode_cancelsAndLeavesLockOff` | Same reset gap: switching the lock mode to Passcode writes `laMode`, which `resetForTest` does not restore. Otherwise implementable: Settings > Your privacy > SimpleX Lock (`SettingsView.kt:248`), `LockModeSelector` (its `ExposedDropDownSettingRow` already takes `uiKey`; options get `.option(LAMode.PASSCODE)`), the "Enable lock" switch (`PrivacySettings.kt:779-794`), `SetAppPasscodeView`'s `BackHandler { close(); cancel() }` (`SetAppPasscodeView.kt:28-31`), and asserting `performLA == false` with no modal left. Needs 4 more keys plus lock-pref restoration in the reset, or a `finally` in the test. |

## Top uncertainties
1. The IME on Waydroid under instrumentation (tests 2, 5, 6, 12, 13 and the hide-keyboard step in 4). `hideKeyboard()` tolerates a keyboard that never shows; test 5 does not.
2. Running a second `ActivityScenario` for the relaunch alongside the harness's private one (tests 10, 13).
3. Polls after the activity finishes take about 2 s per iteration because of the compose root wait. They are slow but correct.
4. Nothing was compiled. `screens.kt` must be merged into `Screens.kt`, and `keys.diff` applied, before `BackKeyTest.kt` builds. `androidx.core` (`ViewCompat`) is assumed to be on the androidTest classpath through Compose.
