# Batch 1 / B drafts: What's New and chat list toolbar

11 tests in 3 classes. Nothing has been compiled or run.

Paths:
- `C/` = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`
- `D/` = `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/`
- `A/` = `apps/multiplatform/common/src/androidMain/kotlin/chat/simplex/common/`
- `S` = `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`
- App line numbers are from `/home/user/wt-uitest` at `eda94a505`. The catalogue cites `d72f68e6d` (`~/wt-uitest-ref`), so its `ChatListView.kt` lines are 3-4 lower than the ones below. That offset alone is not listed as a correction.

## Files

| File | Tests |
|---|---|
| `main/WhatsNewTest.kt` (shared) | `whatsNewFromSettings`, `whatsNewShownAfterVersionUpgrade`, `whatsNewOkCloses`, `whatsNewShowsViewUpdatedConditions` |
| `desktop/WhatsNewTest.kt` (desktop only, package `chat.simplex.app.ui.desktop`) | `whatsNewNotShownForNewUser` |
| `main/ChatListToolbarSearchTest.kt` (shared) | `chatListTitleAndProfileButton`, `chatListScrollToTopOnTitleClick`, `chatListUnreadDotOnProfileButtonForOtherUsers`, `chatListStoppedChatIndicator`, `subscriptionIndicatorConnected`, `subscriptionIndicatorPercentageToggle` |
| `screens.kt` | Additions to `harness/Screens.kt` (listed below) |
| `keys.diff` | App-side keys. `git apply --check` passes on `eda94a505` |

The class map in `00-legend.md` names the classes. `ChatListToolbarSearchTest` already exists in the repo with `searchFiltersByDisplayName`, and agent C is drafting the search rows into the same class. My file contains only my six tests, so it has to be merged into that class rather than copied over it.

## Keys (`keys.diff`)

| Key | Tag | Attached at | Platforms |
|---|---|---|---|
| `Ui.ChatList.Bar` | `chatList.bar`; the title area is `chatList.bar.title` through `DefaultAppBar`'s existing `uiKey?.part("title")` (`C/views/helpers/DefaultTopAppBar.kt:41-43`) | `uiKey = Ui.ChatList.Bar` on `ChatListToolbar`'s `DefaultAppBar` (`C/views/chatlist/ChatListView.kt:664`) | both |
| `Ui.ChatList.Stopped` | `chatList.stopped` | the red "Chat is stopped" `IconButton` (`ChatListView.kt:548-562`) | both |
| `Ui.ChatList.Subscriptions` | `chatList.subscriptions` | `SubscriptionStatusIndicator`'s `SimpleButtonFrame` (`ChatListView.kt:699-704`) | both |
| `Ui.WhatsNew.Ok` | `whatsNew.ok` | the clickable "OK" row (`C/views/onboarding/WhatsNewView.kt:218-238`) | both |
| `Ui.WhatsNew.ViewConditions` | `whatsNew.viewConditions` | the "View updated conditions" text (`WhatsNewView.kt:197-216`) | both |
| `Ui.ServersSummary.ShowPercentage` | `serversSummary.showPercentage` | the "Show percentage" switch (`C/views/chatlist/ServersSummaryView.kt:349`). It is threaded through new optional `uiKey` parameters on `SettingsPreferenceItem` (`C/views/usersettings/SettingsView.kt:425`) and `SharedPreferenceToggle` (`C/views/usersettings/CallSettings.kt:85`), which put it on the `DefaultSwitch` | both |
| `Ui.UserPicker.Sheet` (existing key) | `userPicker.sheet` | the desktop picker box (`D/views/chatlist/UserPicker.desktop.kt:99-102`). Android already tags it while the picker is visible (`A/views/chatlist/UserPicker.android.kt:150`) | desktop added |
| `Ui.Onboarding.CreateYourProfile` | `onboarding.createYourProfile` | the desktop welcome button "Create your profile" (`D/views/onboarding/SimpleXInfo.desktop.kt:20`) | desktop |
| `Ui.Onboarding.RandomPassphrase` | `onboarding.randomPassphrase` | "Use random passphrase" (`C/views/onboarding/SetupDatabasePassphrase.kt:206`), through a new optional `uiKey` on `TextButtonBelowOnboardingButton` (`C/views/onboarding/SimpleXInfo.kt:223`) | both |
| `Ui.Onboarding.Continue` | `onboarding.continue` | "Continue" on "Your network", in both the mobile and the desktop layout (`C/views/onboarding/YourNetwork.kt:118,157`) | both |
| `Ui.Onboarding.AcceptConditions` | `onboarding.acceptConditions` | `AcceptConditionsButton` (`C/views/onboarding/ChooseServerOperators.kt:314-324`) | both |

Overlaps with parallel drafts that have to be merged:
- **Onboarding keys.** Agent A (onboarding) will very likely add keys for "Create your profile", "Use random passphrase", "Continue" and "Accept". Keep one name for each and point `desktop/WhatsNewTest.kt`'s `finishOnboarding()` at it. A's own onboarding helper can replace `finishOnboarding()`.
- **`ShowPercentage` and the indicator key.** The §4 "Servers summary" drafter (`serversSummaryOpensFromIndicator`) needs a key for the indicator too. Keep one of them: `Ui.ChatList.Subscriptions` or theirs.

The keys exist only for buttons with no unique text, plus the indicator. Existing keys are reused wherever one fits: `Ui.Settings.Help`, `Ui.Help.WhatsNew`, `Ui.ChatList.UserPicker`, `Ui.UserPicker.Address`, `Ui.UserPicker.Sheet`, `Ui.Onboarding.ProfileName`, `Ui.Onboarding.CreateProfile`, `Ui.Alert.Confirm`, `Ui.ChatList.Row`. No search-field or filter-chip keys were added.

## Helpers added in `screens.kt`

- `ChatListToolbar` (`UiScope.chatListToolbar()`)
  - `titleTexts()`
  - `canScrollToTop()`: whether the title's `clickable` is enabled, which is `canScrollToZero` at `ChatListView.kt:615,664`
  - `scrollToTop()`
  - `profileBadge()`: whether the profile button has any `Text`. The badge is `Text("")` (`ChatListView.kt:736-747`), and `ProfileImage` has no `Text` (`C/views/helpers/ChatInfoImage.kt:70-103`)
  - `percentage()`: the indicator text that ends in `%`
  - `toggleShowPercentage()`: opens Servers info from the indicator, clicks the switch, closes the screen with Back
- `WhatsNewScreen`
  - `shown()`
  - `ok()`
  - constants `VERSION = "v7.0.1"`, `TITLE`, `OLDER_VERSION = "v6.5"`
- `UiScope.whatsNewFromSettings(app)`: Settings → "Help & support" → "What's new"
- `UiScope.backToChatList()`: Back on Android only (see the platform notes)
- `enableOperator(tag)`: the same `getUserServers`/`setUserServers` pattern as `harness/ServersConfig.kt:8-18`, with the tagged operator `enabled = true`. This is the `/_servers <userId> <json>` command (`C/model/SimpleXAPI.kt:4243`)

They use only methods that both `UiScope` implementations have: `click`, `escape`, `exists`, `waitFor`, `poll`, `isEnabled`, `textsOf`, `seesText`.

## Per test

### `whatsNewFromSettings` (catalogue 01 line 46)
- **Steps:** Settings → `Ui.Settings.Help` → `Ui.Help.WhatsNew`, which opens `WhatsNewView(viaSettings = true)` (`C/views/usersettings/SettingsView.kt:162`).
- **Checks:**
  - The title is "New in v7.0.1": `new_in_version` "New in %s" (S:2487), `WhatsNewView.kt:176`, version at `:946`.
  - There is no `Ui.WhatsNew.Ok`, because the OK row only exists when `!viaSettings` (`:218`).
  - Back closes What's New and "What's new" (`Ui.Help.WhatsNew`) is shown again.
- **Platforms:** both. The same steps are already used on Android by `back_closesWhatsNew` (`android/.../back/BackKeyTest.kt:189`).

### `whatsNewShownAfterVersionUpgrade` (line 47)
- **Steps:** sets `appPrefs.whatsNewVersion` to "v6.5", then `restart()`.
- **Checks:**
  - `ChatListView`'s `LaunchedEffect` (`ChatListView.kt:222-232`) calls `shouldShowWhatsNew` (`WhatsNewView.kt:1030-1034`), waits 1 s and opens `WhatsNewView` in `ModalManager.center`. The test waits for the title and `Ui.WhatsNew.Ok`.
  - The stored setting `WhatsNewVersion` (`SimpleXAPI.kt:256,512`) becomes "v7.0.1", because `shouldShowWhatsNew` stores the last version.
- **Why the restart is race-free:** `restart()` keeps prefs (only `ModelSnapshot` is restored, and it excludes `AppPreferences`). The chat list is composed only after `startChat` sets `localUserCreated` (`App.kt:156`, `SimpleXAPI.kt:674`), so the pref is read after the restart.

### `whatsNewOkCloses` (line 47)
- **Steps:** the same setup as `whatsNewShownAfterVersionUpgrade`, then a click on `Ui.WhatsNew.Ok` (text `ok` "OK", S:789).
- **Checks:** the OK key and the title are gone.

### `whatsNewShowsViewUpdatedConditions` (line 50)
- **Steps:**
  - `enableOperator(OperatorTag.SimpleX)`, then `restart()`.
  - The harness's `configureServers` had disabled both operators. With SimpleX enabled and its conditions neither accepted nor notified, the core returns `UCAReview` with `showNotice = isNothing notifiedAt` (`src/Simplex/Chat/Operators.hs:117-129`). Fresh conditions are inserted with `notifiedAt = Nothing` (`Operators.hs:386`, `Store/Profiles.hs:820-827`).
  - `initChatController` loads `chatModel.conditions` before `startChat` (`C/platform/Core.kt:143`), so the chat list sees `shouldShowNotice`.
- **Checks:**
  - What's New shows `Ui.WhatsNew.ViewConditions` with "View updated conditions" (S:2703).
  - The link opens `UsageConditionsView` in the Review state: "Updated conditions" (S:2154, `NetworkAndServers.kt:791`) and "Accept conditions" (S:2152).
  - The core's `currentConditions.notifiedAt` becomes non-null. `WhatsNewView.kt:72-81` calls `setConditionsNotified`, which is `/_conditions_notified`, when the modal opens.
- **Note:** enabling the operator makes the preset servers usable for new queues. This test creates no queues, so nothing tries to reach them.

### `whatsNewNotShownForNewUser` (line 49), desktop only
- **Steps:**
  - `app(null)`, then a check that no `WhatsNewVersion` is stored yet.
  - "Create your profile", then type "alice". The desktop profile screen's `LaunchedEffect` calls `setLastVersionDefault` (`C/views/WelcomeView.kt:349-351`), and the test polls until the stored version is "v7.0.1".
  - Onboarding is finished in `finishOnboarding()`: Create profile, "Use random passphrase", alert "OK", "Continue", "Accept". `acceptConditions` → `completeOnboarding` (`ChooseServerOperators.kt:293-312`).
- **Checks:** after five `settle()`s (about 2.4 s of app clock; the modal would open 1 s after the chat list is composed), neither the title nor `Ui.WhatsNew.Ok` is present.
- **Why desktop only:** the brief treats onboarding as desktop-only, and `Ui.Onboarding.CreateYourProfile` only exists on the desktop welcome screen. The mobile profile screen sets the version the same way (`WelcomeView.kt:309-311`); catalogue 05 owns Android onboarding.

### `chatListTitleAndProfileButton` (line 56)
- **Checks:**
  - "Chats" (`your_chats`, S:483; `ChatListView.kt:638`) is among the texts of `chatList.bar.title`.
  - `Ui.UserPicker.Sheet` is absent before the profile button is clicked.
  - After the click (`Ui.ChatList.UserPicker`), the sheet is present and `Ui.UserPicker.Address` reads "Create SimpleX address" (S:1177, `C/views/chatlist/UserPicker.kt:222-227`).
- **Platforms:** the sheet key is used instead of the address row because the Android picker content stays composed off-screen (see Catalogue corrections).

### `chatListScrollToTopOnTitleClick` (line 56)
- **Steps:**
  - 30 groups are created through `apiNewGroup` (the pattern of the existing draft `userPickerSwitchProfile`), then `restart()` loads them into the model.
  - The list order is `chatModel.chats` order (`filteredChats`, `ChatListView.kt:1545-1558`).
- **Checks:**
  - The title is not clickable at the top.
  - `scrollTo(last row)` makes it clickable (`canScrollToZero`, `:615`).
  - Clicking it runs `scrollToBottom` → `animateScrollToItem(0)` (`:1628-1630`). The title becomes non-clickable again and the first row exists.

### `chatListUnreadDotOnProfileButtonForOtherUsers` (line 57)
- **Steps:** alice adds the profile "work"; bob connects to work (`connect` uses the active user); the test switches back to alice.
- **Checks:**
  - There is no badge at first. A new contact's internal items are created read: `ciCreateStatus` (`src/Simplex/Chat/Messages.hs:1082-1088`).
  - `bob sends "@work hello"` raises the badge. `increaseUnreadCounter` → `users[i].unreadCount + 1` (`C/model/ChatModel.kt:1019-1035`), and `allRead` excludes the active and hidden users (`ChatListView.kt:626-629`).
  - The test switches to work, opens bob's chat, polls until its unread count is 0, calls `quiesce()` so that the read is stored before the users list is reloaded, and goes back to the list (Android only). After switching back to alice the badge is gone.

### `chatListStoppedChatIndicator` (line 58)
- **Steps:** chat is stopped with `stopChatAsync(chatModel)` (`C/views/database/DatabaseView.kt:509-513`). `App.kt:375` passes `stopped = chatRunning == false`. The Settings › Database toggle is catalogue 04's.
- **Checks:**
  - `Ui.ChatList.Stopped` is absent before the stop.
  - After the stop it has the description "Chat is stopped" (S:1735).
  - Clicking it shows the alert with title "Chat is stopped" and text S:1736, verbatim.
  - The alert's OK (`Ui.Alert.Confirm(Shared)`, `AlertManager.kt:214-258`) closes it.
- **Not checked:** the red tint.

### `subscriptionIndicatorConnected` (line 59)
- **Steps:**
  - `networkShowSubscriptionPercentage` is set directly (default false, `SimpleXAPI.kt:156`). The toggle has its own test.
  - The first check is `%`: no subscriptions and no session (`ServersSummaryView.kt:120`).
  - Then `apiAddContact`, as in `harness/Helpers.kt` `connect`, creates an invitation queue on the local SMP server. The catalogue's GUI 1-time link is avoided because closing the new-chat sheet asks "Keep unused invitation?", and its confirm button deletes the connection (`C/views/newchat/NewChatView.kt:73-98`). No start modal stays open to pause the 1 s poll either (`ChatListView.kt:691-692`).
- **Checks:** the text becomes `100%` (`ServersSummaryView.kt:121`; `shareOfActive` = 1 with a session). The poll uses the default 10 s, not the catalogue's 2 s.

### `subscriptionIndicatorPercentageToggle` (line 59)
- **Steps:** the test turns "Show percentage" (S:1044) on and then off. Each time it clicks the indicator, which opens Servers info in `ModalManager.start` (`ChatListView.kt:642-660`), clicks the switch on the default Messages tab, Subscriptions section (`ServersSummaryView.kt:325-351`), and closes the screen.
- **Checks:**
  - There is no `%` text at first.
  - After turning it on, the text is `%` and the stored setting `ShowSubscriptionPercentage` (`SimpleXAPI.kt:450`) is "true".
  - After turning it off, there is no text and the setting is "false".

## Platform notes

- **`backToChatList()`** has the only platform branch: Back on Android only. On Android `ChatView` covers the list and closes only on Back (`BackHandler` exists only when `appPlatform.isAndroid`, `C/views/chat/ChatView.kt:1226-1227`). On desktop the chat is a side panel with no Back handler, and the user picker stays reachable.
- **The What's New version is fixed at "v7.0.1".** The label is `if (isInUs()) "v7.0.1" else "v7.0"` (`WhatsNewView.kt:946`) in a file-level list. It is evaluated when `WhatsNewView.kt` is first loaded, which is `setLastVersionDefault` in `resetForTest`. At that point `androidPlayStoreCountry` is still null (`C/platform/AppCommon.kt:36`; it is loaded only in `ChatListView.kt:227` or `crowdfundingAvailable`), so the label is "v7.0.1" on Android too. On a Play-flavour build where an earlier test loaded a non-US country it could be "v7.0".
- **`isAppVisibleAndFocused()`** gates the indicator poll. It is `windowFocused` on desktop, which the harness sets to true (`Reset.kt:62`), and `isAppOnForeground` on Android.

## Uncertainties

1. **`profileBadge()` relies on the badge being an empty `Text`.** Testing a tag on the badge was not possible: `testTag` does not survive merging into the `IconButton`, because TestTag's merge policy keeps the parent's value. If the button ever gains a text, this check breaks. The alternative is an app change: give the badge its own semantics node, or a content description.
2. **Desktop `scrollTo` on the chat list.** With "Reachable app toolbars" on (default true, `SimpleXAPI.kt:276`) the list uses `reverseLayout` (`ChatListView.kt:998`). The desktop harness pages with `ScrollBy` (+y). If Compose does not invert the semantic scroll for reversed lists, `scrollTo(last row)` will not page there, and the harness needs a direction fallback. Android uses `performScrollToNode` and is unaffected.
3. **What `!exists(Row(first))` would mean in a lazy list.** Prefetched rows may still appear in semantics, so the scroll test asserts the title's enabled state (`canScrollToZero`) rather than the first row's absence.
4. **Possible pager double composition in Servers info.** If the `HorizontalPager` composes both pages, `Ui.ServersSummary.ShowPercentage` is still unique, because only the Messages tab has the switch.
5. **The 1 s What's New delay runs on the test clock.** `poll` advances the clock: 16 ms per loop on desktop, 48 ms on Android. Waits use the default 10 s.
6. **`whatsNewNotShownForNewUser` assumes the template database turns on the passphrase step.** `DbTemplate.installIfPresent` sets `initialRandomDBPassphrase`, which leads to "Setup database passphrase" (`WelcomeView.kt:393-410`). If the template is ever saved unencrypted, `click(Ui.Onboarding.RandomPassphrase)` times out.

## Catalogue corrections

1. **01 line 56, `chatListTitleAndProfileButton`.**
   - *Problem:* "whose 'Create SimpleX address' row (`UserPicker.kt:222`) is absent before the click" is true only on desktop. The desktop picker is inside `AnimatedVisibility` (`D/views/chatlist/UserPicker.desktop.kt:94-110`). Android keeps the picker content composed and moves it off-screen with `translationY` (`A/views/chatlist/UserPicker.android.kt:136-197`), so the row is always in the tree.
   - *Replacement Verification:* `Title "Chats" (`your_chats`, `ChatListView.kt:634`); clicking the profile image opens the user picker (the picker sheet, tagged only while visible: `UserPicker.android.kt:150`, and on desktop the `AnimatedVisibility` content, `UserPicker.desktop.kt:94-110`), whose first option row reads "Create SimpleX address" (`UserPicker.kt:222`); with enough chats to scroll (e.g. 30) scrolled to the last one, clicking the title brings the first chat row back into view (the title is clickable only while the list is off the top, `canScrollToZero`, `ChatListView.kt:611,660`)`
2. **01 line 57, `chatListUnreadDotOnProfileButtonForOtherUsers`, Notes column.**
   - *Problem:* "Badge has no text" is imprecise. The badge is `Text("")` (`ChatListView.kt:732-743` at the ref), so it has an empty text node.
   - *Replacement Notes:* `Badge is an empty Text (`ChatListView.kt:732-743`) inside the profile IconButton; hidden and active users are excluded from the dot (`ChatListView.kt:622-625`)`
3. **01 line 59, `subscriptionIndicatorConnected`, Verification column.**
   - *Problem:* the row says the text changes after "the 1-time link is created and the new-chat modal is closed". It omits that closing the sheet with an unused link shows "Keep unused invitation?", whose confirm button is "Delete" (`NewChatView.kt:73-98`). Choosing it deletes the queue, and the indicator goes back to "%".
   - *Replacement Verification:* `With "Show percentage" on (Servers info, `ServersSummaryView.kt:349`; off by default, `SimpleXAPI.kt:155`), the text next to the icon goes from "%" (no subscriptions) to "100%" within 2 s after the 1-time link is created and the new-chat modal is closed with "Keep" in the "Keep unused invitation?" alert (`NewChatView.kt:73-98`; "Delete" removes the queue) — or after an invitation is created through the API with no modal open (`ServersSummaryView.kt:121,141`; the colour alone can't tell connected from a fresh install, :87-104; the 1 s poll skips while a start or fullscreen modal is open, `ChatListView.kt:686-690`); the toggle shows and hides the text`
4. **01 line 59, `subscriptionIndicatorPercentageToggle`, same row.**
   - *Problem:* the row doesn't say where the toggle is or that the Servers info screen has to be closed to see the toolbar. On desktop it opens in `ModalManager.start` over the chat list, and the poll is paused while it is open.
   - *Addition to the Verification text:* `The toggle is "Show percentage" in Servers info (opened from the indicator, `ChatListView.kt:642-660`), Messages tab, Subscriptions section; close Servers info to see the toolbar text; the pref key is `ShowSubscriptionPercentage` (`SimpleXAPI.kt:450`)`
5. **01 lines 47-48, What's New entry path.**
   - *Problem:* "when `whatsNewVersion` is older" is narrower than the code. The code compares for inequality: `v != lastVersion` (`WhatsNewView.kt:1033`), so a null value, as on a fresh install, also opens it. That is why the profile screen stores the version (`WelcomeView.kt:309-311,349-351`).
   - *Replacement Entry path:* `Automatic 1 s after the chat list when the stored `whatsNewVersion` differs from the last version (including unset)`
6. **01 line 50, `whatsNewShowsViewUpdatedConditions`, Verification column.**
   - *Problem:* "Link opens `UsageConditionsView`; `setConditionsNotified` is called" suggests that the link makes the call. The call actually runs when What's New opens with `updatedConditions`. The opened screen's title is not given either.
   - *Replacement Verification:* `What's New shows "View updated conditions"; opening it already calls `setConditionsNotified` (`/_conditions_notified`, `WhatsNewView.kt:71-79`), so the core's `currentConditions.notifiedAt` is set; the link opens `UsageConditionsView` in the Review state, titled "Updated conditions" with "Accept conditions" (`NetworkAndServers.kt:790-805`)`

## Rows skipped

None. `whatsNewPaginationPrevNext` (line 48) and `subscriptionIndicatorServerDown` (line 60) are in the same sections but were not assigned to this batch.
