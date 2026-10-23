# UI test catalogue 5a: what changes on Android for the rows of 01–04

Companion to `05-android-only.md` (paths `A/`, `APP/`, `MAN`, `C/`, `D/`, `US/`, `NS/` as defined there and in `00-legend.md`; line numbers of master `d72f68e6d` at `~/wt-uitest-ref`). Every test of catalogues 01–04 runs on Android unless this map lists it. Each of the 1,155 test names of 01–04 was checked against its row's entry path, expected cells and the code it touches; a name is listed only when its Android behaviour or the harness mechanism differs from desktop.

## Summary

| Catalogue | Tests | Listed | REPLACED | DESKTOP-ONLY | ADAPT | Unchanged on Android |
|---|---|---|---|---|---|---|
| 01 | 331 | 83 | 7 | 6 | 70 | 248 |
| 02 | 280 | 104 | 12 | 11 | 81 | 176 |
| 03 | 312 | 59 | 0 | 0 | 59 | 253 |
| 04 | 232 | 102 | 11 | 32 | 59 | 130 |
| **Total** | **1,155** | **348** | **30** | **49** | **269** | **807** |

The Android run of 01–04 is therefore the 807 unchanged tests plus the 269 ADAPT tests; the 30 REPLACED and 49 DESKTOP-ONLY tests don't run there (9 of the DESKTOP-ONLY tests and the REPLACED `call_webrtcSession_connects` are `IGNORE` on desktop too: `dragDrop_files_attach`, `dbMgmt_openDatabaseFolder`, `dbError_openDatabaseFolder`, the five tray and window tests of 04 §M and `updates_checkDownloadInstall`). Each REPLACED row names the 05 test that runs instead. The first cell gives the line of the test's row in its catalogue.

## Classes

- **REPLACED**: the desktop assertion is wrong on Android and a 05 test covers the Android behaviour instead; the named 05 test replaces it in the Android run.
- **DESKTOP-ONLY**: the feature or the asserted state does not exist on Android, or cannot be reached there; the test does not run on Android.
- **ADAPT**: the test runs on Android with the change stated (driver action, label, path, count, layout or assertion).

## General rules

Rules used by many rows; the tables name every row they apply to, as `R<n>` plus what is specific to the row.

- **R1 Right-click.** `Modifier.onRightClick` does nothing on Android (`A/platform/Modifier.android.kt:16`; desktop `D/platform/Modifier.desktop.kt:80`). Every `onRightClick` in shared code sits next to a `combinedClickable(onLongClick = …)` with the same action, so a "right-click" entry is driven with `longClick()` (01 setup item 8). Rows whose entry only says "row menu" or "long-press" already use the long press and are not listed.
- **R2 Message field keys.** The Android message field is an `EditText` with no key listener: `onDone`, `onUpArrow` and `onFilesPasted` are never called (`A/platform/PlatformTextField.android.kt:60-63`). Enter inserts a line break, and only the send button ("Send Message", `C/views/chat/SendMsgView.kt:452-490`) sends; Shift+Enter, Up, Ctrl+V and typed file paths (`D/platform/PlatformTextField.desktop.kt:97-110,123-150`) don't exist. A file is attached through the 05 §6 "File" stub (`attachFile_getContent_sendsFile`, `A/views/chat/ComposeView.android.kt:44,74-77`).
- **R3 File dialogs.** `FD` becomes `INTENTS`: open dialogs are `ACTION_GET_CONTENT`, save dialogs `ACTION_CREATE_DOCUMENT` (`A/platform/Files.android.kt:36-52`); "Image"/"Video" attachments are `ACTION_PICK` on the gallery (`A/views/chat/ComposeView.android.kt:40-73`). There is no directory chooser and no typed path.
- **R4 Onboarding.** The welcome screen has only "Get started" (`A/views/onboarding/SimpleXInfo.android.kt:11-16`); "Your profile" is the mobile layout with "Migrate" in its app bar (`C/views/WelcomeView.kt:200-300`); there is no passphrase step (`C/views/WelcomeView.kt:400`); "Your network" also has "Setup notifications", and its "Continue" applies the notification mode (default Instant), which starts the service and shows the "Instant notifications" alert (onboarding boots keep the notice prefs at their defaults), dismissed with "OK" (`C/views/onboarding/YourNetwork.kt:58-125`; `APP/SimplexApp.kt:217-244`; `APP/SimplexService.kt:430-448`). After onboarding, while the user has no conversations and `addressCreationCardShown` is false, the chat-list area shows the onboarding cards instead of the rows (`C/views/chatlist/ChatListView.kt:449-454`; `C/views/newchat/OnboardingCards.kt:52-56`); the toolbar with "Chats" stays.
- **R5 Share.** `shareText` opens the system chooser (`ACTION_CHOOSER` wrapping `ACTION_SEND`, `EXTRA_TEXT`) instead of copying and showing "Copied" (`A/platform/Share.android.kt:30-49`; desktop `D/platform/Share.desktop.kt:20-23`); `shareFile` opens the chooser with `EXTRA_STREAM` (`A/platform/Share.android.kt:51-89,112-114`). The assertion is the recorded intent (`INTENTS`), as in 05 §6.
- **R6 New chat entry.** The entry is labelled "Paste link / Scan" and opens the connect screen with the camera section at once ("Or scan QR code"; `CAMERA` is pre-granted by the runner, 05 setup item 2) (`C/views/newchat/NewChatSheet.kt:61,194`; `C/views/chatlist/ChatListView.kt:417,437,440`; `C/views/newchat/NewChatView.kt:652-668`). Pasting a link hides the scanner (`NewChatView.kt:679,684`).
- **R7 Notifications.** `NTF` assertions read the real notifications (`SYSNTF`, 05 §3).
- **R8 SimpleX Lock.** "Lock mode" is set to "Passcode" first (System is the default, `C/views/usersettings/PrivacySettings.kt:38-52`; 05 §10); the passcodes are stored Keystore-encrypted, so a pref check reads `DatabaseUtils.ksAppPassword.get()` / `ksSelfDestructPassword.get()` instead of Base64-decoding the pref (`C/views/helpers/DatabaseUtils.kt:15-19`; `A/platform/Cryptor.android.kt`; desktop passthrough `D/platform/Cryptor.desktop.kt:3-17`); backgrounding is the real activity lifecycle (`BG`, `APP/MainActivity.kt:70-89`) instead of the simulated focus events of 04 harness fact 7.
- **R9 Esc.** There is no Esc key on Android; the same `BackHandler`s run on the system Back key (`BACK`, `A/platform/Back.android.kt:5-8`; desktop window `onKeyEvent`, `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt:125-130`).
- **R10 Recorder and player seams.** `REC` and `AV` rows (recording, voice and video playback, video previews, the incoming-call ringtone) need Android actuals of the plan §11.2 seams: `A/platform/RecAndPlay.android.kt` (`MediaRecorder`/`MediaPlayer`), `A/platform/VideoPlayer.android.kt` (ExoPlayer) and `A/helpers/SoundPlayer.kt`, as the desktop seams replace VLC (`D/platform/RecAndPlay.desktop.kt`, `D/platform/VideoPlayer.desktop.kt`). The test steps are unchanged.
- **R11 Toasts.** Android `showToast` is the system `Toast.makeText(…).show()` (`A/platform/UI.android.kt:23`; every Android toast goes through it), so there is no in-process list to read as on desktop (`simplexWindowState.toasts`, `D/platform/UI.desktop.kt:7-9`). Without app changes, the instrumented test registers `UiAutomation.setOnAccessibilityEventListener` and records the `TYPE_NOTIFICATION_STATE_CHANGED` events whose package is the app's and whose class is `android.widget.Toast`, with the toast text from the event's text; a `TOAST` step ("toast X is recorded", "no toast is recorded") reads this recorder. Text toasts of an app targeting API 30+ (the app targets 36, `android/build.gradle.kts:18`) are drawn by System UI, which sends the event with the app's package (`ToastPresenter.trySendAccessibilityEvent`, SDK `android/widget/ToastPresenter.java:311-335`; before Android 11 the class was `android.widget.Toast$TN`). To be confirmed in the Android P1 (05 open question 13); the fallback is an app-side `showToast` hook, which needs the user's approval like the desktop seams.
- **R12 Typing leaves the cursor at the end.** `EditText.setText`, which Espresso `replaceText` calls, resets the selection to 0 (`TextView.setText` runs `mMovement.initialize` before `sendOnTextChanged`, SDK `android/widget/TextView.java:7504,7519`; `ArrowKeyMovementMethod.initialize` → `Selection.setSelection(text, 0)`, `android/text/method/ArrowKeyMovementMethod.java:364`), and the app forwards that selection into the compose state (`onSelectionChanged` and `doOnTextChanged`, `A/platform/PlatformTextField.android.kt:123-128,160-167`). The mention picker reads the selection (`C/views/chat/group/GroupMentions.kt:104-137,291-310`), so with the cursor at 0, 02 `mention_editAddsMention` would show no picker and `mention_maxReached_showsLimit` would select the first mention instead of the limit state. The Android `type()` therefore puts the cursor at the end after setting the text (05 setup item 5), as desktop `performTextInput` does; with that, compose and mention rows carry over unchanged.
- **R13 Links and clipboard.** The Android test composes the real root (`MainActivity.onCreate` → `setContent { AppScreen() }`, `APP/MainActivity.kt:57-59`; 05 setup item 1), so there is no `TestRoot` and none of its composition-local stubs: nothing in `android/` or `androidMain` provides `LocalUriHandler` or `LocalClipboardManager`. `URI` therefore becomes `INTENTS`: `LocalUriHandler` is Compose's `AndroidUriHandler`, whose `openUri` calls `startActivity(Intent(ACTION_VIEW, Uri.parse(uri)))` and rethrows `ActivityNotFoundException` as `IllegalArgumentException` (Compose `ui-android` 1.8.2, `gradle.properties:35`); the test records the outgoing `ACTION_VIEW` with Espresso-Intents and stubs it with `respondWith`, so nothing launches, and "the URI stub records X" becomes "an `ACTION_VIEW` intent with data X is recorded". `CB` is the real system `ClipboardManager` behind Compose's `AndroidClipboardManager`, read and written in-process while the app is in the foreground; the clip persists across tests (and apps), so the Android reset clears it (`ClipboardManager.clearPrimaryClip()`, 05 setup item 4), and "the test puts X on the clipboard stub" means setting the system clip. The 90 `CB` tests of 01–04 carry over with this mechanism and are not listed for it; every `URI` test of 01–04 is listed, as R13 or, for the call rows, with its own Android behaviour.

**Not listed (carry over unchanged at test level):**
- One pane: Android shows either the chat list or a chat, and info screens, settings and modals cover the screen (`C/App.kt:313-360`; `C/views/helpers/ModalView.kt:253-258`). Screen objects own the navigation (05 setup item 6), so a row that asserts a chat-list row while a chat or its info is open, or opens a modal over a chat, is not listed for that reason alone.
- The message field is typed with Espresso `replaceText` on the `EditText`, followed by `setSelection(editText.length())` (05 setup item 5, R12); only key-driven steps are listed (R2). The clear-text (x) button shows at `EditText.lineCount >= 4`, which counts wrapped lines, where desktop counts newlines (`A/platform/PlatformTextField.android.kt:196`; `D/platform/PlatformTextField.desktop.kt:204`); 02 `compose_clearTextButton_clears` carries over as long as the lines it types are short enough not to wrap. Reads of the field (its text and placeholder) go through `ChatScreen.messageText`/`placeholder`, which read the `EditText` in an Espresso `ViewAction` (05 setup item 5), so rows that read it are not listed for that reason alone.
- Rows whose entry is "row menu" or "long-press" (R1 note), and rows that only need a runtime permission the runner pre-grants (05 setup item 2: `POST_NOTIFICATIONS`, `CAMERA`, `RECORD_AUDIO`).
- `CB` tests (90 in 01–04): the clipboard is the real system one, cleared by the reset (R13), so a `CB` test is not listed for that reason alone.
- 04 `privacy_linkPreviewsOff_noPreview` (04:53) runs unchanged, but until the debug-only network-security-config of the 02:43 rows is approved it passes vacuously: with the setting on, no preview could be fetched either.
- `RESTART` tests (51 in 01–04; 47 run on Android, 4 are DESKTOP-ONLY or REPLACED) carry over once `restart()` has an Android mechanism (05 open question 14); they are listed only where the Android startup differs.
- `FIXTURE` tests (37 in 01–04) carry over once fixtures have an Android mechanism (05 open question 16); listed only where the fixture differs on Android (04:199, the key).
- Screenshot-only checks (01 `createListDuplicateNameError`, `planBotAddressShowsBotIcon`, 04 `morePrivacy_verifySimplexNamesPref`): their semantic assertions run on Android unchanged, and the part checked only in the screenshot waits for 05 open question 17, since Android writes no §7.7 shots until then; they are not listed for that reason alone (01:187 is listed for R6).
- Layout-only differences that no row asserts (paddings, the tags row scrolling horizontally instead of wrapping, `A/views/chatlist/ChatListView.android.kt:33-43`; the user picker drawn as a bottom sheet).

---

## 01 onboarding, chat list, connecting, profiles, address, contact info

| Catalogue row (file:line) | Test name | Class | Android behaviour / replacement | Code ref |
|---|---|---|---|---|
| 01:27 | `onboardingWhySimplexIsBuiltOpensHowItWorks` | ADAPT | R4: the welcome screen has only "Get started" and "Why SimpleX is built.", so the checks of "Link a mobile" and "Create your profile" are dropped (05 §1 `androidOnboardingWelcomeShowsGetStarted` checks the Android buttons); HowItWorks and Back are unchanged | `A/views/onboarding/SimpleXInfo.android.kt:11-16`; `D/views/onboarding/SimpleXInfo.desktop.kt:16-23` |
| 01:28 | `onboardingCreateProfileButtonDisabledForEmptyName` | ADAPT | R4: entry "Get started" | `A/views/onboarding/SimpleXInfo.android.kt:13` |
| 01:28 | `onboardingCreateProfileInvalidNameShowsCorrectionAlert` | ADAPT | R4: entry "Get started" | `A/views/onboarding/SimpleXInfo.android.kt:13` |
| 01:28 | `onboardingCreateProfileBackReturnsToWelcome` | ADAPT | R4: entry "Get started"; the back arrow of the mobile screen returns to the welcome screen (system Back: 05 §13 `back_onboardingProfile_returnsToWelcome`) | `C/views/WelcomeView.kt:227-242` |
| 01:29 | `onboardingMigrateFromAnotherDeviceOpensMigrationScreen` | REPLACED | 05 §1 `androidOnboardingMigrateButtonOpensScanAndPaste`: the button reads "Migrate" and the screen leads with "Scan QR code" | `C/views/WelcomeView.kt:221`; `C/views/migration/MigrateToDevice.kt:206-216` |
| 01:30 | `onboardingSetPassphraseMismatchDisablesButton` | DESKTOP-ONLY | The passphrase step exists only on desktop; Android stores a random passphrase in the Keystore (05 §1 `androidOnboardingNoPassphraseStepAfterProfile`) | `C/views/WelcomeView.kt:400` |
| 01:31 | `onboardingUseRandomPassphraseAlert` | DESKTOP-ONLY | As above | `C/views/WelcomeView.kt:400` |
| 01:32 | `onboardingPassphrasePersistsAcrossRestart` | DESKTOP-ONLY | As above; the Android path to the startup passphrase prompt is 05 §11 `keystore_removed_restartAsksPassphrase` | `C/views/WelcomeView.kt:400` |
| 01:33 | `onboardingYourNetworkShowsSetupRouters` | ADAPT | R4: reached right after the profile; the screen also shows "Setup notifications" (05 §1 `androidYourNetworkShowsSetupNotifications`), so the check that it is absent is dropped | `C/views/onboarding/YourNetwork.kt:58-133` |
| 01:34 | `onboardingChooseOperatorsCannotDeselectAll` | ADAPT | R4: reached without the passphrase step ("Setup routers" on the mobile "Your network") | `C/views/onboarding/YourNetwork.kt:98-103` |
| 01:34 | `onboardingHowItHelpsPrivacyInfo` | ADAPT | R4: as above | `C/views/onboarding/YourNetwork.kt:98-103` |
| 01:35 | `onboardingAcceptConditionsCompletesOnboarding` | ADAPT | R4: no passphrase step; after "Continue", "OK" on the "Instant notifications" alert, then "Network commitments"; after "Accept" the toolbar shows "Chats" and the list area shows the onboarding cards | `C/views/onboarding/YourNetwork.kt:118-125`; `C/views/chatlist/ChatListView.kt:449-454` |
| 01:35 | `onboardingConditionsLinkOpensConditionsText` | ADAPT | R4: as above | `C/views/onboarding/YourNetwork.kt:118-125` |
| 01:36 | `onboardingAcceptButtonDisabledWithNoOperators` | ADAPT | R4: as above | `C/views/onboarding/YourNetwork.kt:118-125` |
| 01:37 | `onboardingThenConnectToCliPeerViaLocalServers` | ADAPT | R4: no passphrase step, "OK" on the notifications alert; the chat-list area then shows the onboarding cards, so the 1-time link is created from "Start new chat" › "Create 1-time link" (or the card "Let someone connect to you" › "Invite someone privately") | `C/views/newchat/OnboardingCards.kt:357-404` |
| 01:38 | `onboardingLinkAMobileShowsQrAndDeviceName` | DESKTOP-ONLY | No "Link a mobile" on the Android welcome screen (05 §1 `androidOnboardingWelcomeShowsGetStarted`) | `A/views/onboarding/SimpleXInfo.android.kt:11-16` |
| 01:39 | `onboardingLinkAMobileBackReturnsToWelcome` | DESKTOP-ONLY | As above | `A/views/onboarding/SimpleXInfo.android.kt:11-16` |
| 01:40 | `onboardingCreateProfileWhenOnlyHiddenUsersExist` | ADAPT | R4: "Get started"; the next screen is always "Your network" (no passphrase step); the no-user startup sets the welcome stage without `startChatWithoutUser` | `C/views/WelcomeView.kt:398-405`; `C/platform/Core.kt:153-167` |
| 01:48 | `whatsNewPaginationPrevNext` | ADAPT | R13: "Open" on the "Open external link?" alert of "Read more" fires `ACTION_VIEW` with the URL, recorded and stubbed with `INTENTS`, instead of the URI stub | `C/views/onboarding/WhatsNewView.kt:261`; `C/views/helpers/Utils.kt:613-624` |
| 01:49 | `whatsNewNotShownForNewUser` | ADAPT | R4: onboarding path ("Get started", no passphrase step, notifications alert) | `C/views/WelcomeView.kt:399-405` |
| 01:73 | `editListNameAndEmoji` | ADAPT | R1 (long-press the chip); the emoji, both when the list is created and when it is changed, comes from the `EmojiPickerView` (`VIEW`, as in 05 §14 `tagListAndroid_emojiPickerViewSetsEmoji`) | `C/views/chatlist/ChatListView.kt:1331`; `A/views/chatlist/TagListView.android.kt:22-81` |
| 01:73 | `deleteListConfirm` | ADAPT | R1 (long-press the chip) | `C/views/chatlist/ChatListView.kt:1331` |
| 01:73 | `reorderListsDrag` | ADAPT | R1 (long-press the chip); the drag is a touch drag | `C/views/chatlist/ChatListView.kt:1331` |
| 01:84 | `contactMenuMarkRead` | ADAPT | R1 (long-press the row) | `A/views/chatlist/ChatListNavLinkView.android.kt:26-27` |
| 01:84 | `contactMenuMarkUnread` | ADAPT | R1 | `A/views/chatlist/ChatListNavLinkView.android.kt:26-27` |
| 01:84 | `contactMenuClearChat` | ADAPT | R1 | `A/views/chatlist/ChatListNavLinkView.android.kt:26-27` |
| 01:84 | `contactMenuRightClickOpens` | ADAPT | R1: tests the long press of the same handler | `A/views/chatlist/ChatListNavLinkView.android.kt:26-27` |
| 01:90 | `pendingAcceptedRowIcon` | ADAPT | R6: the full link is pasted through the connect screen ("Paste link / Scan") | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:90 | `pendingConnectionMenuDeleteAccepted` | ADAPT | R6: as above | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:98 | `connectBannerCardOpensPasteLink` | ADAPT | R6: the banner's second half reads "Paste link / Scan" and opens the connect screen with the camera section | `C/views/chatlist/ChatListView.kt:414-440` |
| 01:101 | `centerConnectOnboardingCardsWithNoConversations` | REPLACED | 05 §1 `androidOnboardingCards_inChatList`, `androidOnboardingCards_swipeBetweenPages`: the cards fill the chat-list area (no centre pane) | `C/views/chatlist/ChatListView.kt:449-475`; `C/App.kt:389-418` |
| 01:101 | `centerCardsInviteSomeone` | ADAPT | The cards are in the chat-list area, not a centre pane; "Let someone connect to you" animates the pager to "Create your link"; "Invite someone privately" opens NewChatView full screen | `C/views/chatlist/ChatListView.kt:453,465-475`; `C/views/newchat/OnboardingCards.kt:343-349,394-404` |
| 01:101 | `centerCardsCreatePublicAddress` | ADAPT | As above: the card is in the chat-list area; the address screen opens full screen | `C/views/newchat/OnboardingCards.kt:405-418` |
| 01:101 | `centerCardsConnectViaLink` | REPLACED | 05 §1 `androidOnboardingCards_connectViaLinkShowsScanner`: the connect screen shows the camera section (desktop asserts no scanner) | `C/views/newchat/OnboardingCards.kt:376-381` |
| 01:102 | `noSelectedChatPlaceholder` | DESKTOP-ONLY | "No selected chat" is drawn only in the desktop centre pane; Android's one-pane screen has no placeholder | `C/App.kt:313-360,407-416` |
| 01:111 | `serversSummaryFilesTabShowsXftp` | ADAPT | R2: no typed-path attach; alice attaches the file through the 05 §6 "File" stub (`INTENTS`) | `A/platform/PlatformTextField.android.kt:60-63`; `A/views/chat/ComposeView.android.kt:44,74-77` |
| 01:112 | `serversSummaryShareJson` | ADAPT | R5: the share button opens the chooser with `EXTRA_TEXT` = the JSON (`INTENTS`), not the clipboard | `C/views/chatlist/ChatListView.kt:641-652`; `A/platform/Share.android.kt:30-49` |
| 01:118 | `userPickerOpensAndShowsCurrentUser` | ADAPT | The picker is a bottom sheet whose first section is one horizontally scrolling row of all visible profiles; the current user is the box with the bold name (no separate current-profile row); it closes on a tap on the dimmed area or a swipe down | `C/views/chatlist/UserPicker.kt:201-215`; `A/views/chatlist/UserPicker.android.kt:34-106,127-201` |
| 01:118 | `userPickerCurrentUserOpensProfileEditor` | ADAPT | Tapping the current user's box opens "Your current profile" | `C/views/chatlist/UserPicker.kt:161-175`; `A/views/chatlist/UserPicker.android.kt:72-106` |
| 01:119 | `userPickerSwitchProfile` | ADAPT | Inactive profiles are boxes in the same scrolling row (desktop: a grid of avatars under the divider); 05 §14 `userPickerAndroid_profilesRowSwitchesUser` covers the row itself | `A/views/chatlist/UserPicker.android.kt:34-70`; `D/views/chatlist/UserPicker.desktop.kt:27-87` |
| 01:120 | `userPickerUnreadBadgePerUser` | ADAPT | The unread badge is on the avatar inside each inactive profile's box of the scrolling profiles row (desktop: on the avatars of the grid); the count is the same `userUnreadBadge` | `A/views/chatlist/UserPicker.android.kt:90-96`; `C/views/chatlist/UserPicker.kt:639` |
| 01:120 | `userPickerMutedUserGrayBadge` | ADAPT | The muted user's grey badge is likewise on the avatar inside its box of the scrolling profiles row (desktop: on the avatars of the grid) | `A/views/chatlist/UserPicker.android.kt:90-96`; `C/views/chatlist/UserPicker.kt:639` |
| 01:121 | `userPickerLinkAMobileRow` | REPLACED | 05 §12 `useFromDesktop_pickerRowOpensConnectScreen`: the row is "Use from desktop" | `C/views/chatlist/UserPicker.kt:354-366` |
| 01:121 | `userPickerDisabledWhenChatStopped` | ADAPT | The disabled items are the profile boxes, the address row, "Chat preferences", "Your chat profiles" and "Use from desktop" (no "Link a mobile"); "Settings" stays enabled | `C/views/chatlist/UserPicker.kt:201-210,354-366`; `A/views/chatlist/UserPicker.android.kt:45-48,81-84` |
| 01:124 | `profilesHideProfileWithPassword` | ADAPT | R1 (long-press the profile row) | `C/views/usersettings/UserProfilesView.kt:220`; `C/views/chatlist/UserPicker.kt:410-418` |
| 01:124 | `profilesHidePasswordMismatchDisabled` | ADAPT | R1 | `C/views/usersettings/UserProfilesView.kt:220`; `C/views/chatlist/UserPicker.kt:410-418` |
| 01:124 | `profilesRevealHiddenByPasswordInSearch` | ADAPT | R1 | `C/views/usersettings/UserProfilesView.kt:220`; `C/views/chatlist/UserPicker.kt:410-418` |
| 01:124 | `profilesActivateHiddenViaSearchPassword` | ADAPT | R1 | `C/views/usersettings/UserProfilesView.kt:220`; `C/views/chatlist/UserPicker.kt:410-418` |
| 01:124 | `profilesUnhideActiveRequiresPassword` | ADAPT | R1 | `C/views/usersettings/UserProfilesView.kt:220`; `C/views/chatlist/UserPicker.kt:410-418` |
| 01:124 | `profilesHideOnlyWhenMoreThanOneVisible` | ADAPT | R1 | `C/views/usersettings/UserProfilesView.kt:220`; `C/views/chatlist/UserPicker.kt:410-418` |
| 01:128 | `profilesDeleteLastVisibleShowsCreateChatProfile` | REPLACED | 05 §15 `profilesAndroid_deleteLastVisibleReturnsToOnboarding`: the core stops and the welcome screen shows | `C/views/usersettings/UserProfilesView.kt:353-361` |
| 01:131 | `editProfileImageSetAndDelete` | ADAPT | R3: "Edit image" opens the bottom sheet "Camera" / "From Gallery"; the image comes from the gallery stub (as 05 §6 `profileImage_galleryStub_setsImage`, `INTENTS`) instead of a file dialog; "Delete image" is unchanged | `A/views/helpers/GetImageView.android.kt:142-216`; `C/views/usersettings/UserProfileView.kt:178-186` |
| 01:138 | `newChatSheetShowsActions` | ADAPT | R6: the second action reads "Paste link / Scan" | `C/views/newchat/NewChatSheet.kt:194` |
| 01:140 | `newChatSheetContactMenuDelete` | ADAPT | R1 (long-press the contact row) | `C/views/contacts/ContactListNavView.kt:31-140` |
| 01:152 | `oneTimeLinkShareButton` | REPLACED | 05 §6 `shareText_oneTimeLink_firesChooser` (R5) | `C/views/newchat/NewChatView.kt:758-763`; `A/platform/Share.android.kt:30-49` |
| 01:158 | `oneTimeLinkUsedNoAlert` | ADAPT | R5: use the copy icon, or assert the chooser when the link is shared; closing then shows no alert as on desktop | `C/views/newchat/NewChatView.kt:719-721,751-763` |
| 01:159 | `oneTimeLinkQrClickSavesPng` | ADAPT | R5: tapping the QR code fires the chooser with `ACTION_SEND`, `EXTRA_STREAM` = a `FileProvider` URI of `IMG_<ts>.png`, type `image/png`, in place of the two save dialogs; closing the sheet still shows no "Keep unused invitation?" alert | `C/views/newchat/QRCode.kt:89-99`; `A/platform/Share.android.kt:51-89,112-114` |
| 01:170 | `pasteLinkInvalidTextAlert` | ADAPT | R6: entry "Paste link / Scan" | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:170 | `pasteLinkClipboardEmptyNoop` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:172 | `planInvitationShortLinkOpenNewChat` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:173 | `planInvitationFullLinkUseCurrentProfile` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:173 | `planInvitationFullLinkUseIncognito` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:173 | `planInvitationFullLinkCancel` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:174 | `planOwnInvitationConnectToYourself` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:175 | `planInvitationAlreadyConnectingFullLink` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:176 | `planInvitationKnownContactOpenChat` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:177 | `planInvitationConnectingPreparedOpensKnown` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:178 | `planInvitationDeletedLinkError` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:179 | `planAddressShortLinkOpenNewChat` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:180 | `planAddressFullLinkConnectAlert` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:181 | `planOwnAddressConnectToYourself` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:182 | `planAddressRepeatConnectionRequest` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:183 | `planAddressConnectingProhibitOpensKnown` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:184 | `planAddressKnownContactOpenChat` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:185 | `planAddressContactViaAddressOpensChat` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:186 | `planAddressDeletedContactReconnect` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:187 | `planBotAddressShowsBotIcon` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 01:211 | `pendingInfoInvitedShowsLinkQrShare` | ADAPT | R5: "Share 1-time link" opens the chooser (05 §6 `shareText_pendingConnection_firesChooser`) instead of filling the clipboard | `C/views/newchat/ContactConnectionInfoView.kt:159-166` |
| 01:224 | `addressQrAndShareAddress` | ADAPT | R5: "Share address" opens the chooser (05 §6 `shareText_address_firesChooser`) | `C/views/usersettings/UserAddressView.kt:115,343-366` |
| 01:240 | `simplexNameRegisterLinkOpensBrowser` | ADAPT | R13: opening the link from the "Open web link?" alert fires `ACTION_VIEW` with `https://simplex.domains/#testing` (`INTENTS`) | `C/views/usersettings/SetSimplexNameView.kt:178`; `C/views/chat/item/TextItemView.kt:533-585` |
| 01:249 | `contactInfoHeaderShowsNameBioCopy` | ADAPT | R11: "Copied to clipboard" is a system toast, asserted through the toast recorder; the clipboard check is unchanged | `C/views/chat/ChatInfoView.kt:745-748`; `A/platform/UI.android.kt:23` |
| 01:250 | `contactInfoSearchButton` | ADAPT | "search" closes the info screen first, then opens the chat's search field (05 §14 `chatInfo_searchButtonClosesInfoAndSearches`) | `C/views/chat/ChatInfoView.kt:883-899` |
| 01:251 | `contactInfoCallButtonsStubbed` | REPLACED | 05 §9 `call_outgoingAudio_invitationReachesPeer` (video: as far as 05 §9 `callPermissions_videoCallNeedsCamera`): calls start `CallActivity`, not the URI handler | `C/App.kt:227-235`; `APP/SimplexApp.kt:319-330`; `D/views/call/CallView.desktop.kt:158-167` |

## 02 chat view, messages, chat item info

| Catalogue row (file:line) | Test name | Class | Android behaviour / replacement | Code ref |
|---|---|---|---|---|
| 02:25 | `notes_opens_showsSearchAndContentFilterIcons` | REPLACED | 05 §14 `toolbar_notes_filterInMenuAndroid`: notes show "Search" and the ⋮ button; the content filters are in the ⋮ menu, not a bar icon | `C/views/chat/ChatView.kt:1339-1340,1377-1400` |
| 02:28 | `toolbarMenu_direct_items` | ADAPT | With calls available the direct chat's ⋮ menu also lists the content filters after a divider (at least "Images" and "Links"; 05 §14 `toolbar_directChat_filterInMenuWhenCallsAvailable`) | `C/views/chat/ChatView.kt:1377-1400` |
| 02:30 | `toolbarCall_audio_opensCallLink` | REPLACED | 05 §9 `call_outgoingAudio_invitationReachesPeer`: the call opens `CallActivity`, not the URI handler | `C/App.kt:227-235`; `APP/SimplexApp.kt:319-330` |
| 02:31 | `callStart_browserUnavailable_showsAlertAndEnds` | DESKTOP-ONLY | No browser is opened on Android, so "Error opening browser" can't appear | `D/views/call/CallView.desktop.kt:157-174`; `C/App.kt:227-235` |
| 02:38 | `multiline_shiftEnter_peerReceivesTwoLines` | REPLACED | 05 §14 `compose_enterKeyInsertsNewline_sendButtonSends`: Enter inserts the line break, the send button sends | `A/platform/PlatformTextField.android.kt:60-63,99-132` |
| 02:39 | `groupSend_allMembersReceive` | ADAPT | R2: sends with the send button | `A/platform/PlatformTextField.android.kt:60-63` |
| 02:42 | `link_click_showsOpenWebLinkAlert` | ADAPT | R13: "Open" fires `ACTION_VIEW` with `http://127.0.0.1/page`, recorded and stubbed with `INTENTS` so no browser launches | `C/views/chat/item/TextItemView.kt:533-585` |
| 02:42 | `link_openClean_opensSanitizedUri` | ADAPT | R13: "Open clean link" fires `ACTION_VIEW` whose data is the URL without `utm_source` (`INTENTS`) | `C/views/chat/item/TextItemView.kt:533-585` |
| 02:43 | `linkPreview_firstTimeAlert_enable_fetchesPreview` | ADAPT | Cleartext HTTP to 127.0.0.1 is blocked: the app targets API 36 and its manifest sets neither `usesCleartextTraffic` nor `networkSecurityConfig`, so the platform default forbids cleartext, with no loopback exemption; jsoup's `HttpURLConnection` fetch fails with "Cleartext HTTP traffic to 127.0.0.1 not permitted" and `getLinkPreview` catches it and returns null (no preview card, the content type stays text). The test needs a debug-only network-security-config allowing cleartext to 127.0.0.1, a build change that needs the user's approval (05 open question 15), and `adb reverse` of the HTTP stub's port like the other servers; until it is approved the test fails | `android/build.gradle.kts:18`; `MAN:40-50`; `common/build.gradle.kts:74`; SDK `com/android/okhttp/HttpHandler.java:121-128`, `android/security/net/config/NetworkSecurityConfig.java:200-202`, `android/security/net/config/ManifestConfigSource.java:73-79`; `C/views/helpers/LinkPreviews.kt:96-140` |
| 02:43 | `linkPreview_sent_peerReceivesLinkContent` | ADAPT | As above: without the debug-only config no preview is attached, so bob's content types print `text`, not `link` | `C/views/helpers/LinkPreviews.kt:96-140` |
| 02:43 | `linkPreview_cancel_sendsPlainText` | ADAPT | As above: without the debug-only config there is no preview to cancel | `C/views/helpers/LinkPreviews.kt:96-140` |
| 02:56 | `liveMessage_menuStart_alertConfirm_peerSeesStarted` | ADAPT | R1: long-press the send button for its menu | `C/views/chat/SendMsgView.kt:452-490` |
| 02:56 | `liveMessage_typingUpdates_peerSeesUpdates` | ADAPT | R1 | `C/views/chat/SendMsgView.kt:452-490` |
| 02:56 | `liveMessage_finish_peerSeesEnded` | ADAPT | R1 | `C/views/chat/SendMsgView.kt:452-490` |
| 02:60 | `liveMessage_group_membersSeeLive` | ADAPT | R1: "Same, in a group" starts from the send-button menu, so the send button is long-pressed | `C/views/chat/SendMsgView.kt:452-490` |
| 02:67 | `timed_send30s_bothSidesDelete` | ADAPT | R1 | `C/views/chat/SendMsgView.kt:452-490` |
| 02:68 | `timed_menuOptions_listed` | ADAPT | R1 | `C/views/chat/SendMsgView.kt:452-490` |
| 02:68 | `timed_customTime_sendsWithChosenTtl` | REPLACED | 05 §14 `timedCustom_androidWheelPickerSetsTtl`: "Delete after" shows two wheels instead of two dropdowns | `A/views/helpers/CustomTimePicker.android.kt:14-106`; `D/views/helpers/CustomTimePicker.desktop.kt:11-81` |
| 02:69 | `timed_menuHidden_whenFeatureOff` | ADAPT | R1 | `C/views/chat/SendMsgView.kt:452-490` |
| 02:70 | `timed_chatPref_receivedMessagesDisappear` | ADAPT | R1 (long-press the feature row → "Expand") | `C/views/chat/item/CIChatFeatureView.kt:30-35` |
| 02:70 | `timed_meta_showsTimerAndTtl` | ADAPT | R1 (long-press the feature row → "Expand") | `C/views/chat/item/CIChatFeatureView.kt:30-35` |
| 02:75 | `voice_record_send_peerReceivesVoice` | ADAPT | R10 (`REC`) | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:76 | `voice_tapToRecord_stopButton_addsPreview` | ADAPT | R10 (`REC`) | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:76 | `voice_tooShort_discarded` | ADAPT | R10 (`REC`) | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:76 | `voice_preview_cancel_deletesTmpFile` | ADAPT | R10 (`REC`) | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:79 | `voice_disabledWhileRecording_cancels` | ADAPT | R10 (`REC`) | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:81 | `voice_playback_playPauseProgress` | ADAPT | R10 (`AV`) | `A/platform/RecAndPlay.android.kt:101-296` |
| 02:82 | `voice_preview_playPauseAndSeek` | ADAPT | R10 (`REC`, `AV`) | `A/platform/RecAndPlay.android.kt` |
| 02:83 | `voice_notes_recordAndSave` | ADAPT | R10 (`REC`) | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:88 | `attach_sheet_showsImageVideoFile` | REPLACED | 05 §6 `attachSheet_androidShowsCameraImageVideoFile`: four buttons, "Camera" first | `A/views/helpers/ChooseAttachmentView.android.kt:13-30`; `D/views/helpers/ChooseAttachmentView.desktop.kt:13-26` |
| 02:89 | `attachFile_send_peerReceivesOffer` | ADAPT | R3: "File" fires `ACTION_GET_CONTENT` `*/*`, answered by the stub (as 05 §6 `attachFile_getContent_sendsFile`) | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:89 | `attachFile_withText_peerReceivesTextAndFile` | ADAPT | R3: as above | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:90 | `attachFile_cancelPreview_nothingSent` | ADAPT | R3: as above | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:91 | `attachImage_send_peerReceivesCompressedImage` | ADAPT | R3: "Image" fires `ACTION_PICK` on the gallery, answered by the stub (as 05 §6 `attachGallery_twoImages_sendsEach`) | `A/views/chat/ComposeView.android.kt:40,58-65` |
| 02:91 | `attachImages_multiple_sentAsSeparateMessages` | ADAPT | R3: the stub returns several URIs in `ClipData` | `A/views/helpers/GetImageView.android.kt:218-242` |
| 02:91 | `attachImage_textOnLastImageOnly` | ADAPT | R3: as above | `A/views/helpers/GetImageView.android.kt:218-242` |
| 02:92 | `attachImage_cancelPreview` | ADAPT | R3 | `A/views/chat/ComposeView.android.kt:40,58-65` |
| 02:93 | `attachGif_sentUncompressedAnimated` | ADAPT | R3: the gallery stub returns the GIF; the coil animation runs in real time even under blur and modals (`A/views/chat/item/CIImageView.android.kt:22-57`), so the image node is volatile in screenshots | `A/views/chat/ComposeView.android.kt:40,58-65` |
| 02:94 | `attachVideo_send_peerReceivesVideo` | ADAPT | R3: "Video" fires `ACTION_PICK` for videos (as 05 §6 `attachGallery_video_sendsVideo`); R10 (`AV`) | `A/views/chat/ComposeView.android.kt:42,66-73` |
| 02:95 | `attachVideo_undecodable_showsAlert` | ADAPT | R3 and R10, as above | `A/views/chat/ComposeView.android.kt:42,66-73` |
| 02:96 | `paste_filePathText_attachesFile` | DESKTOP-ONLY | R2: the Android field doesn't turn typed paths into attachments | `A/platform/PlatformTextField.android.kt:60-63`; `D/platform/PlatformTextField.desktop.kt:97-110` |
| 02:97 | `ctrlV_clipboardFiles_attaches` | DESKTOP-ONLY | R2: no Ctrl+V handling | `D/platform/PlatformTextField.desktop.kt:141-145` |
| 02:98 | `pasteFileList_webmAudio_sentAsFile` | DESKTOP-ONLY | R2: no Ctrl+V handling | `D/platform/PlatformTextField.desktop.kt:144-145` |
| 02:99 | `ctrlV_clipboardImage_attachesPng` | DESKTOP-ONLY | R2: no Ctrl+V handling (`IMGCB`); a keyboard-committed image is 05 §6 `compose_imeCommitImage_attachesToOpenChat` | `D/platform/PlatformTextField.desktop.kt:146-150`; `A/platform/PlatformTextField.android.kt:108-121` |
| 02:100 | `dragDrop_files_attach` | DESKTOP-ONLY | `desktopOnExternalDrag` returns the modifier unchanged on Android (`IGNORE` on desktop too) | `A/platform/Modifier.android.kt:8-14` |
| 02:101 | `attachFile_tooLarge_showsLargeFileAlert` | ADAPT | R3: the `GET_CONTENT` stub returns the sparse file | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:103 | `attachImage_withQuote_peerSeesQuote` | ADAPT | R3: the image comes from the gallery stub | `A/views/chat/ComposeView.android.kt:40,58-65` |
| 02:104 | `attachImage_group_allMembersReceive` | ADAPT | R3: as above | `A/views/chat/ComposeView.android.kt:40,58-65` |
| 02:114 | `sndFile_revoke_peerSeesCancelled` | ADAPT | R2: alice attaches `test.pdf` through the "File" stub, not a typed path (`rcvFile_senderRevoked_showsCancelled` of the same row is bob's `/f` + `/fc` and is unchanged) | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:115 | `sndFile_stopDuringUpload` | ADAPT | R2: as above | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:117 | `sndFile_xftpResetDuringUpload_showsFileError` | ADAPT | R2: the 150 MB file is attached through the "File" stub | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:121 | `rcvFile_clickCompleted_opensSaveDialog` | REPLACED | 05 §6 `rcvFile_tapLoaded_showsOpenWithAndSave` and `rcvFile_save_createDocumentWritesDecrypted`: a click on a loaded file opens the "Open with…" / "Save" menu, and "Save" fires `ACTION_CREATE_DOCUMENT` | `C/views/chat/item/CIFileView.kt:183-200`; `A/views/chat/item/CIFileView.android.kt:14-57` |
| 02:122 | `rcvFile_missingOnDisk_clickDoesNothing` | ADAPT | R3: "no save dialog awaiting" becomes "no intent recorded" (`INTENTS`); with the file missing, `getLoadedFilePath` is null, so no menu opens either | `C/views/chat/item/CIFileView.kt:183-200` |
| 02:124 | `rcvVideo_previewAndDownload` | ADAPT | R10 (`AV`) | `A/platform/VideoPlayer.android.kt` |
| 02:130 | `reply_menu_sendQuoted_peerSeesQuote` | ADAPT | R1 (long-press the item) | `C/views/chat/item/ChatItemView.kt:327-335` |
| 02:130 | `reply_cancelContext` | ADAPT | R1 | `C/views/chat/item/ChatItemView.kt:327-335` |
| 02:138 | `edit_menu_updatesPeer` | ADAPT | R2: the edit is sent with the send button | `A/platform/PlatformTextField.android.kt:60-63` |
| 02:138 | `edit_unchanged_noUpdate` | ADAPT | R2: as above | `A/platform/PlatformTextField.android.kt:60-63` |
| 02:138 | `edit_metaShowsEditedIcon` | ADAPT | R2: as above | `A/platform/PlatformTextField.android.kt:60-63` |
| 02:139 | `edit_upArrow_editsLastMessage` | DESKTOP-ONLY | R2: no Up-arrow handling | `D/platform/PlatformTextField.desktop.kt:138-140` |
| 02:142 | `edit_ownImage_previewWithoutCancel` | ADAPT | R3: the image comes from the gallery stub; R2: the edit is sent with the send button | `A/views/chat/ComposeView.android.kt:40,58-65` |
| 02:142 | `edit_ownFile_keepsFileWithoutPreview` | ADAPT | R3: the file comes from the "File" stub; R2: as above | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:143 | `edit_notOffered_forReceivedVoiceForwarded` | ADAPT | R10 (`REC`) for alice's recorded voice message | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:167 | `forwarded_goToSource_opensOriginalChat` | ADAPT | No hover: the arrow button is always composed (faded until activated) and is tapped directly; tapping the forwarded bubble toggles it to full visibility | `C/views/chat/item/ChatItemView.kt:243-302,330-332` |
| 02:178 | `reaction_chipMenu_direct_showsNames` | ADAPT | R1 (long-press the chip) | `C/views/chat/item/ChatItemView.kt:200-207` |
| 02:179 | `reaction_chipMenu_group_membersAndOpenInfo` | ADAPT | R1 | `C/views/chat/item/ChatItemView.kt:200-207` |
| 02:185 | `copy_text_toClipboardAndToast` | REPLACED | 05 §6 `copyAndroid_text_noToast`: the clipboard gets the text with no toast | `A/views/chat/item/ChatItemView.android.kt:47-49`; `D/views/chat/item/ChatItemView.desktop.kt:54-71` |
| 02:186 | `copy_fileItem_copiesPath` | REPLACED | 05 §6 `copyAndroid_fileItem_copiesCaptionOnly`: the item's text, not the path, and no toast | `A/views/chat/item/ChatItemView.android.kt:47-49` |
| 02:187 | `share_text_copiesToClipboard` | REPLACED | 05 §6 `shareText_itemMenu_firesChooser` (R5) | `C/views/chat/item/ChatItemView.kt:425-434`; `A/platform/Share.android.kt:30-49` |
| 02:188 | `share_file_savesViaDialog` | REPLACED | 05 §6 `shareFile_itemMenu_firesChooserWithStream`: "Share" of a file fires the chooser with `EXTRA_STREAM`, no save dialog | `A/platform/Share.android.kt:112-114`; `D/platform/Share.desktop.kt:24-42` |
| 02:189 | `save_image_writesFile` | REPLACED | 05 §6 `rcvImage_save_insertsIntoGallery`: "Save" of an image inserts it into MediaStore with the toast "Image saved to Gallery", no picker | `A/views/chat/item/ChatItemView.android.kt:28-45`; `A/platform/Share.android.kt:145-181` |
| 02:189 | `save_encryptedFile_decrypts` | ADAPT | R3: "Save" fires `ACTION_CREATE_DOCUMENT`; the stub's URI receives the decrypted bytes (as 05 §6 `rcvFile_save_createDocumentWritesDecrypted`) | `A/views/chat/item/ChatItemView.android.kt:40`; `A/platform/Files.android.kt:36-43` |
| 02:191 | `textSelection_dragAcrossItems_copyButton` | DESKTOP-ONLY | `SelectionManager` exists only on desktop | `C/views/chat/ChatView.kt:166,1056,2383` |
| 02:191 | `textSelection_ctrlC_copies` | DESKTOP-ONLY | As above | `C/views/chat/ChatView.kt:166,2383` |
| 02:208 | `info_historyBubble_copy` | ADAPT | R1 (long-press the version) | `C/views/chat/ChatItemInfoView.kt:80-85,132-137` |
| 02:210 | `info_delivery_liveUpdatesOnReceipt` | ADAPT | R1 (long-press the item → "Info" → "Delivery") | `C/views/chat/item/ChatItemView.kt:327-335` |
| 02:213 | `info_share_copiesText` | ADAPT | R5: the info screen's share button opens the chooser with `EXTRA_TEXT` = the info text (`INTENTS`), not the clipboard | `C/views/chat/ChatView.kt:731-734`; `A/platform/Share.android.kt:30-49` |
| 02:214 | `info_closesOnChatSwitch` | ADAPT | The info covers the whole screen, so the other chat is opened from outside it, preferably with `openChat` from test code, which exercises the info's own close on the chat change (`KeyChangeEffect`, `ChatView.kt:753-755`); tapping cath's message notification (`SYSNTF`) closes it earlier through `clearOverlays` → `closeAllModalsEverywhere` instead; Back would close the info itself | `C/views/chat/ChatView.kt:731,753-755`; `C/platform/NtfManager.kt:80`; `C/App.kt:111-115`; `C/views/helpers/ModalView.kt:253-258` |
| 02:220 | `unreadFab_top_rightClick_markRead` | ADAPT | R1: tests the long press of the top button | `C/views/chat/ChatView.kt:2852` |
| 02:222 | `pagination_scrollUp_loadsOlder` | ADAPT | The chat opens with the newest 75 messages (`INITIAL_COUNT` is 100 only on desktop); older pages are still 100 | `C/model/SimpleXAPI.kt:4609-4610` |
| 02:231 | `contentFilter_menu_listsAvailableTypes` | ADAPT | In the direct chat with bob (calls available) there is no filter icon in the bar: the filters are opened from the ⋮ menu (after the divider), or from the filter icon of the search field once search is open | `C/views/chat/ChatView.kt:1339-1340,1377-1442` |
| 02:231 | `contentFilter_files` | ADAPT | As above | `C/views/chat/ChatView.kt:1377-1400` |
| 02:231 | `contentFilter_images` | ADAPT | As above | `C/views/chat/ChatView.kt:1377-1400` |
| 02:231 | `contentFilter_allMessages_resets` | ADAPT | As above; "All messages" is the last ⋮ item while a filter is active | `C/views/chat/ChatView.kt:1401-1418` |
| 02:232 | `contentFilter_links_showsOnlyLinks` | ADAPT | As above | `C/views/chat/ChatView.kt:1377-1400` |
| 02:232 | `contentFilter_withSearchText` | ADAPT | As above | `C/views/chat/ChatView.kt:1377-1442` |
| 02:238 | `draft_withAttachment_restored` | ADAPT | R2: the file is attached through the "File" stub instead of Ctrl+V | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:239 | `draft_voiceRecording_finishedOnSwitch` | ADAPT | R10 (`REC`) | `A/platform/RecAndPlay.android.kt:17-98` |
| 02:259 | `featureItem_contactChangesPref_shown` | ADAPT | R1 (long-press the feature row → "Expand") | `C/views/chat/item/CIChatFeatureView.kt:28-48` |
| 02:260 | `featureItems_merged_iconRowExpandHide` | ADAPT | R1 | `C/views/chat/item/CIChatFeatureView.kt:28-48` |
| 02:262 | `callItem_incoming_answerButton` | ADAPT | "Answer call" starts `CallActivity` instead of the URI handler: the test asserts that `CallActivity` opens (found through `ComposeRootRegistry`) with bob's name and that `chatModel.activeCall` holds bob's call, before any media, as 05 §9 `call_outgoingAudio_invitationReachesPeer` does for an outgoing call (media is 05 §9 `call_answer_mediaConnects`, `IGNORE`); R10 (`AV`) for the ringtone | `C/views/call/CallManager.kt:27-58`; `C/App.kt:227-235`; `APP/SimplexApp.kt:319-330` |
| 02:262 | `callItem_endedStates` | ADAPT | R10 (`AV`, the ringtone through `SoundPlayer`) | `A/helpers/SoundPlayer.kt` |
| 02:263 | `callAlert_incoming_acceptRejectIgnore` | ADAPT | "Accept" closes the alert and starts `CallActivity` instead of recording the call link with the URI stub; it is asserted before any media, as in `callItem_incoming_answerButton` (`C/App.kt:227-235`; media is 05 §9 `call_answer_mediaConnects`, `IGNORE`); "Reject" and "Ignore" are unchanged (the in-app alert shows in the foreground, 05 §9 `call_incomingForeground_inAppAlertNoSystemNotification`); R10 for the ringtone | `C/views/call/IncomingCallAlertView.kt:27-46`; `APP/model/NtfManager.android.kt:160-169` |
| 02:264 | `callItem_answerEnded_showsAlert` | ADAPT | R10 (`AV`); the "Call already ended!" alert comes before any call screen, as on desktop | `C/views/chat/ChatView.kt:616-627` |
| 02:265 | `events_merged_deleteRange` | ADAPT | R1 (long-press the collapsed events) | `C/views/chat/item/ChatItemView.kt:327-335` |
| 02:277 | `notes_attachImage_storedLocally` | ADAPT | R3: the gallery stub | `A/views/chat/ComposeView.android.kt:40,58-65` |
| 02:277 | `notes_attachFile_storedLocally` | ADAPT | R3: the "File" stub | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:277 | `notes_delete_removesFile` | ADAPT | R3: the file is attached through the "File" stub | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 02:277 | `notes_contentFilterImages` | ADAPT | R3 for the attachments; "Images" is chosen from the ⋮ menu, as in 05 §14 `toolbar_notes_filterInMenuAndroid` | `C/views/chat/ChatView.kt:1377-1400` |
| 02:282 | `contextMenu_rightClick_opens` | DESKTOP-ONLY | R1: right-click does nothing; the long press is `contextMenu_longPress_opens` | `A/platform/Modifier.android.kt:16` |
| 02:283 | `contextMenu_itemsMatrix_byItemType` | ADAPT | R1: each menu is opened by long press ("Right-click each item type"); R10 (`AV`) for the ringtone of the incoming call that produces the call item | `C/views/chat/item/ChatItemView.kt:327-335` |
| 02:284 | `enter_whenSendDisabled_noop` | DESKTOP-ONLY | R2: no Enter handling | `D/platform/PlatformTextField.desktop.kt:123-137` |
| 02:284 | `escape_closesItemInfoModal` | ADAPT | R9: Back closes the item-info modal (`BACK`) | `C/views/helpers/ModalView.kt:40-42`; `A/platform/Back.android.kt:5-8` |
| 02:289 | `call_webrtcSession_connects` | REPLACED | 05 §9 `call_answer_mediaConnects`, `call_connectedControls_micSpeakerVideoFlip` (`IGNORE` on Android too: the call page runs in a `WebView`) | `A/views/call/CallView.android.kt:691-790` |

## 03 groups, channels, business chats

| Catalogue row (file:line) | Test name | Class | Android behaviour / replacement | Code ref |
|---|---|---|---|---|
| 03:33 | `createGroup_withProfileImage` | ADAPT | R3: the edit-image button opens the sheet "Camera" / "From Gallery"; the image comes from the gallery stub (`INTENTS`) | `C/views/newchat/AddGroupView.kt:92-125`; `A/views/helpers/GetImageView.android.kt:142-216` |
| 03:33 | `createGroup_removeImageBeforeCreate` | ADAPT | R3: as above, before the delete-image button is used | `C/views/newchat/AddGroupView.kt:117-124` |
| 03:57 | `groupInfo_headerShowsProfileAndCopiesName` | ADAPT | R11: the toast «Copied to clipboard» is asserted through the toast recorder; the clipboard check is unchanged | `C/views/chat/group/GroupChatInfoView.kt:970-973`; `A/platform/UI.android.kt:23` |
| 03:60 | `groupInfo_searchButton` | ADAPT | «search» closes Group info first, then opens the chat search (05 §14 `chatInfo_searchButtonClosesInfoAndSearches`) | `C/views/chat/group/GroupChatInfoView.kt:404-414` |
| 03:75 | `editGroupProfile_changeImage` | ADAPT | R3: the image comes from the gallery stub through the "Camera" / "From Gallery" sheet | `C/views/chat/group/GroupProfileView.kt:93-100`; `A/views/helpers/GetImageView.android.kt:142-216` |
| 03:91 | `groupPrefs_disappearingOnWithTtl` | ADAPT | The short TTL is set in the custom time picker, which on Android is two wheels (number, unit) instead of two dropdowns (as 05 §14 `timedCustom_androidWheelPickerSetsTtl`) | `C/views/helpers/CustomTimePicker.kt:62`; `A/views/helpers/CustomTimePicker.android.kt:14-106` |
| 03:91 | `groupPrefs_disappearingMessageExpires` | ADAPT | As above | `A/views/helpers/CustomTimePicker.android.kt:14-106` |
| 03:91 | `groupPrefs_disappearingOff` | ADAPT | As above | `A/views/helpers/CustomTimePicker.android.kt:14-106` |
| 03:112 | `review_acceptFromLongPressMenu` | ADAPT | R1 (long-press the pending row) | `C/views/helpers/Section.kt:270-278` |
| 03:114 | `joinerPendingReview_supportChatAutoOpens` | ADAPT | R6 entry; the support chat covers the group chat on Android and closing it returns to the chat list (05 §13 `pendingReview_closeSupportChat_returnsToChatList`), so «reviewed by admins» in the group's own compose is not displayed: it is asserted in the semantics tree under the support chat, or left to 05 §13 | `C/views/chat/group/MemberSupportChatView.kt:33-47`; `C/views/chat/ChatView.kt:246-256` |
| 03:114 | `joinerPendingReview_messagesAdmins` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:114 | `joinerPendingReview_acceptedByAdmin` | ADAPT | R6; after the accept the support chat stays over the group chat, and Back now returns to the group's chat (`memberPending` is false), where the history and the enabled compose are asserted | `C/views/newchat/NewChatSheet.kt:61,194`; `C/views/chat/group/MemberSupportChatView.kt:33-47` |
| 03:114 | `joinerPendingReview_rejected` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:124 | `groupLink_shareCopiesShortLink` | ADAPT | R5: «Share link» opens the chooser with the short link (`INTENTS`), not the clipboard | `C/views/chat/group/GroupLinkView.kt:250-262`; `A/platform/Share.android.kt:30-49` |
| 03:132 | `groupLink_shareOldLink` | ADAPT | R5: «Share old link» opens the chooser with the full link | `C/views/chat/group/GroupLinkView.kt:255-258` |
| 03:138 | `joinGroup_pasteShortLinkPrepareAndJoin` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:138 | `joinGroup_preparedShowsWelcomeBeforeJoin` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:138 | `joinGroup_preparedJoinIncognito` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:138 | `joinGroup_preparedChangeProfile` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:139 | `joinGroup_fullLinkCurrentProfile` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:139 | `joinGroup_fullLinkIncognito` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:139 | `joinGroup_cancelAlert` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:140 | `joinGroup_retryAfterServerError` | ADAPT | R6 (the prepared group comes from a pasted link) | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:141 | `connectPlan_knownGroupOpensGroup` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:141 | `connectPlan_knownGroupShowsRole` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:142 | `connectPlan_ownGroupLinkOpenGroup` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:142 | `connectPlan_ownGroupLinkJoinAsSecondMember` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:143 | `connectPlan_connectingAllowsReconnect` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:143 | `connectPlan_connectingProhibit` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:144 | `connectPlan_rejoinAfterLeaving` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:159 | `memberList_contextMenuAsAdmin` | ADAPT | R1 (long-press the member row) | `C/views/helpers/Section.kt:270-278` |
| 03:160 | `memberList_contextMenuAsMember` | ADAPT | R1 | `C/views/helpers/Section.kt:270-278` |
| 03:201 | `chatsWithMembers_bannerInGroupChat` | ADAPT | The moderator's banner reads «%d chat(s)» ("1 chat(s)"), not «1 chat with a member» (05 §14 `toolbar_supportBanner_shortCountOnAndroid`) | `C/views/chat/ChatView.kt:1721-1730` |
| 03:212 | `reports_tapReportJumpsToReportedMessage` | ADAPT | «Member reports» closes when the quoted message is clicked (it stays open only on desktop); the group chat then scrolls to the reported message | `C/views/chat/ChatView.kt:1900-1909`; `C/views/chat/item/ChatItemView.kt:1372-1376` |
| 03:227 | `mentions_mentionsOnlyMuteStillNotifies` | ADAPT | R7: the notification is read with `SYSNTF` (app in the background, or another chat open) | `C/platform/NtfManager.kt:39-50`; `A/platform/Notifications.android.kt:3` |
| 03:227 | `mentions_blockedMemberMentionIgnored` | ADAPT | R7: no system notification is posted | `C/platform/NtfManager.kt:39-50` |
| 03:249 | `createChannel_singleRelayCompletesAndShowsLink` | ADAPT | When the relay is active the creation screen turns into the «Channel link» step itself (no modal over the opened channel); its close button opens the channel chat | `C/views/newchat/AddChannelView.kt:59-77,557-586` |
| 03:251 | `createChannel_withImage` | ADAPT | R3: the image comes from the gallery stub through the "Camera" / "From Gallery" sheet | `C/views/newchat/AddChannelView.kt:252-260`; `A/views/helpers/GetImageView.android.kt:142-216` |
| 03:252 | `createChannel_twoRelaysProgress` | ADAPT | As `createChannel_singleRelayCompletesAndShowsLink`: the link step replaces the progress step; «Chat relays» is checked after closing it | `C/views/newchat/AddChannelView.kt:59-77,557-586` |
| 03:253 | `createChannel_continueWithPartialRelays` | ADAPT | As above: continuing shows the link step, then the channel | `C/views/newchat/AddChannelView.kt:59-77,495-524` |
| 03:256 | `channelLink_shareCopiesLink` | ADAPT | R5: «Share link» opens the chooser with the channel link (`INTENTS`) | `C/views/chat/group/GroupLinkView.kt:250-262` |
| 03:259 | `channel_broadcastFile` | ADAPT | R3: the file comes from the "File" stub | `A/views/chat/ComposeView.android.kt:44,74-77` |
| 03:260 | `channel_ownerRelayBarStates` | ADAPT | R1: «Remove relay» is in the relay row's long-press menu | `C/views/chat/group/ChannelRelaysView.kt:83-94` |
| 03:261 | `channelRelays_removeRelay` | ADAPT | R1 | `C/views/chat/group/ChannelRelaysView.kt:83-94` |
| 03:261 | `channelRelays_removeLastRelayWarning` | ADAPT | R1 (the same menu) | `C/views/chat/group/ChannelRelaysView.kt:83-94` |
| 03:262 | `channelRelays_rejectedRelayStatus` | ADAPT | R1: the relay that left is removed through the long-press menu | `C/views/chat/group/ChannelRelaysView.kt:83-94` |
| 03:278 | `joinChannel_pasteLinkOpenChannel` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:278 | `joinChannel_joinViaRelay` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:278 | `joinChannel_incognito` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:278 | `joinChannel_noActiveRelays` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:278 | `joinChannel_receivesHistory` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:279 | `joinChannel_twoRelaysBothConnect` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:285 | `connectPlan_knownChannelSubscriber` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:286 | `connectPlan_ownChannelLink` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:286 | `connectPlan_relayAddressRejected` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:296 | `customer_connectToBusinessAddress` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:296 | `customer_connectWithRequestMessage` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:297 | `customer_knownBusinessPlan` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |
| 03:298 | `connectPlan_businessPendingOpensKnownChat` | ADAPT | R6 | `C/views/newchat/NewChatSheet.kt:61,194` |

## 04 settings, database, migration, remote desktop, misc

| Catalogue row (file:line) | Test name | Class | Android behaviour / replacement | Code ref |
|---|---|---|---|---|
| 04:38 | `settingsRoot_listsDesktopItems` | REPLACED | 05 §15 `settingsAndroid_rootListsAndroidItems`: Android lists "Notifications" and "Shutdown" and its own version row | `C/views/usersettings/SettingsView.kt:121-128`; `A/views/usersettings/SettingsView.android.kt:31-34` |
| 04:39 | `settingsRoot_itemsDisabledWhenChatStopped` | ADAPT | "Notifications" is disabled too (05 §2 `ntfSettings_rootItemIconAndStoppedState`); "Shutdown" stays enabled | `C/views/usersettings/SettingsView.kt:121-128` |
| 04:40 | `settingsRoot_crowdfundingOpensGetStake` | ADAPT | R13: "Learn more on Wefunder" fires `ACTION_VIEW` with `https://wefunder.com/simplex.chat?utm_source=app` (`INTENTS`) | `C/views/onboarding/WhatsNewView.kt:1232-1236` |
| 04:42 | `userPicker_colorModeSwitcherRightClickSystem` | ADAPT | R1: tests the long press of the switch | `C/views/usersettings/Appearance.kt:738-768` |
| 04:43 | `keyboard_escClosesTopModal` | REPLACED | 05 §13 `back_closesModalsOneByOne`: the same `BackHandler`s run on the system Back key (R9) | `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt:125-130`; `A/platform/Back.android.kt:5-8` |
| 04:44 | `keyboard_escDismissesAlert` | ADAPT | R9: the system Back key (`BACK`) goes to the alert's dialog window, whose `onDismissRequest` hides only the alert; the switch stays off and "Network & servers" stays open | `C/views/helpers/AlertManager.kt:139-141` |
| 04:52 | `privacy_sectionsAndFooter` | ADAPT | "Device" has "SimpleX Lock" and "Protect app screen" (05 §10 `androidPrivacy_deviceSectionShowsLockAndProtectScreen`) | `A/views/usersettings/PrivacySettings.android.kt:13-31` |
| 04:65 | `morePrivacy_notificationPreviewModes` | ADAPT | R7: per mode the real notification's title and text are asserted (as 05 §3 `sysNtf_previewContactName`, `sysNtf_previewHidden`); the Android notification manager builds them | `APP/model/NtfManager.android.kt:102-158` |
| 04:76 | `lock_enable_setsPasscode` | ADAPT | R8: "Lock mode" > "Passcode" first; `EncryptedAppPassphrase` holds Keystore ciphertext, so the check is `DatabaseUtils.ksAppPassword.get()` = the digits | `C/views/helpers/DatabaseUtils.kt:17-28`; `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:77 | `lock_enable_cancelKeepsOff` | ADAPT | R8: "Passcode" mode first | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:78 | `lock_enable_confirmMismatch` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:79 | `lock_lockAfterOptions` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:80 | `lock_changePasscode_success` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:80 | `lock_changePasscode_wrongCurrent` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:81 | `lock_disable_requiresPasscode` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:82 | `lock_screenOnStartup_correct` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:82 | `lock_screenOnStartup_wrong_showsUnlock` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:83 | `lock_relocksAfterDelay` | REPLACED | 05 §10 `androidLock_passcodeRelocksAfterRealBackground`: the real background (`BG`) replaces the simulated `appWasHidden()`, with the same 10 s delay | `APP/MainActivity.kt:70-89` |
| 04:84 | `lock_protectedActionsPrompt` | ADAPT | R8: "Passcode" mode first, then the four prompts as on desktop | `C/views/usersettings/SettingsView.kt:106,473-520` |
| 04:84 | `profiles_actionsRequireAuthOnceWhenLocked` | ADAPT | R8 | `C/views/chatlist/UserPicker.kt:268-297` |
| 04:85 | `lock_noticeOfferedWithManyChats` | REPLACED | 05 §10 `androidLock_noticeOffersModeChoice`, `androidLock_noticePasscodeEntry_setsPasscodeMode`: "Turn on" first offers "Passcode entry" / "System authentication" | `C/AppLock.kt:34-78` |
| 04:86 | `lock_passcodeKeyboardKeys` | ADAPT | R8; the keys are injected into the Compose passcode view as on desktop (a hardware keyboard; the soft keyboard is not used there) | `C/views/localauth/PasscodeView.kt:34-62` |
| 04:87 | `lock_passcodeKeypad_digitsBackspaceClearReveal` | ADAPT | R8; portrait shows the same vertical keypad (landscape: 05 §10 `passcodeAndroid_landscapeShowsSubmit`) | `C/views/localauth/PasscodeView.kt:130-134` |
| 04:88 | `selfDestruct_enable` | ADAPT | R8; Enter is injected as in 04:86 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:88 | `selfDestruct_cannotEqualAppPasscode` | ADAPT | R8; Enter is injected as in 04:86 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:89 | `selfDestruct_infoView` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:90 | `selfDestruct_displayNamePersisted` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:91 | `selfDestruct_changePasscode` | ADAPT | R8 | `C/views/usersettings/PrivacySettings.kt:38-52` |
| 04:92 | `selfDestruct_triggeredOnLockScreen` | ADAPT | R8: "the app passcode becomes the self-destruct code" is read with `ksAppPassword.get()` | `C/views/helpers/DatabaseUtils.kt:17-28` |
| 04:93 | `selfDestruct_disable` | ADAPT | R8 ("Passcode" mode first); the prefs-unset check runs as on desktop, and unlike desktop the Keystore entry `selfDestructPassword` is deleted too | `C/views/helpers/DatabaseUtils.kt:19,36-40`; `A/platform/Cryptor.android.kt:69-72` |
| 04:99 | `appearance_language_switch` | REPLACED | 05 §14 `appearanceAndroid_languageRecreatesActivity`: the language selector is in the "Interface" section and the activity is recreated in the new language | `A/views/usersettings/Appearance.android.kt:81-105`; `D/views/usersettings/Appearance.desktop.kt:49-63` |
| 04:99 | `appearance_language_backToSystem` | REPLACED | 05 §14 `appearanceAndroid_languageRecreatesActivity` ("System" restores English and recreates the activity again) | `A/views/usersettings/Appearance.android.kt:91-105` |
| 04:100 | `appearance_reachableAppToolbars` | ADAPT | The switch is in the "Interface" section; turning "Reachable app toolbars" on also sets `chatBottomBar = true` | `A/views/usersettings/Appearance.android.kt:108-110` |
| 04:100 | `appearance_reachableChatToolbarVisibility` | REPLACED | 05 §14 `appearanceAndroid_interfaceSectionToolbars`: "Reachable chat toolbar" shows whenever "Reachable app toolbars" is on, whatever `chatBottomBar` is | `A/views/usersettings/Appearance.android.kt:111-113`; `D/views/usersettings/Appearance.desktop.kt:65-67` |
| 04:103 | `appearance_wallpaperPreset_select` | ADAPT | The presets are one horizontally scrolling row (81 dp tiles), not a 4-column grid; the driver scrolls the row to the preset | `C/views/usersettings/Appearance.kt:416-440` |
| 04:104 | `appearance_wallpaperImage_import` | ADAPT | R3: the "+" tile (at the end of the scrolling row) fires `ACTION_GET_CONTENT`, answered by the stub | `C/views/usersettings/Appearance.kt:534`; `A/platform/Files.android.kt:36-43` |
| 04:104 | `appearance_wallpaperImage_remove` | ADAPT | R3: the image is imported through the stub first | `C/views/usersettings/Appearance.kt:534,596` |
| 04:105 | `appearance_themeDestination_perUser` | ADAPT | The wallpaper preset is chosen in the horizontally scrolling preset row (81 dp tiles), which the driver scrolls to the preset, as in `appearance_wallpaperPreset_select` | `C/views/usersettings/Appearance.kt:416-440` |
| 04:107 | `customizeTheme_wallpaperScaleAndChatColors` | ADAPT | R3: the wallpaper image comes from the `GET_CONTENT` stub; the "+" tile is the last tile of the horizontally scrolling preset row (81 dp tiles), which the driver scrolls to, as in `appearance_wallpaperImage_import` | `C/views/usersettings/Appearance.kt:416-440,522,596,635`; `A/platform/Files.android.kt:36-43` |
| 04:108 | `customizeTheme_exportTheme` | ADAPT | R3: "Export theme" fires `ACTION_CREATE_DOCUMENT` with the title "simplex.theme"; the YAML is read back from the stub's URI | `C/views/usersettings/Appearance.kt:702-718` |
| 04:108 | `customizeTheme_importTheme` | ADAPT | R3: "Import theme" fires `ACTION_GET_CONTENT`, answered by the stub | `C/views/usersettings/Appearance.kt:719-732` |
| 04:108 | `customizeTheme_importInvalidTheme` | ADAPT | R3: as above, with the invalid YAML | `C/views/usersettings/Appearance.kt:719-732` |
| 04:109 | `appearance_closeToTray_hiddenWithoutTray` | DESKTOP-ONLY | The tray section exists only in the desktop layout; Android has no tray | `D/views/usersettings/Appearance.desktop.kt:72-75`; `A/views/usersettings/Appearance.android.kt:73-159` |
| 04:113 | `appearance_fontSize_sliderAndReset` | ADAPT | The slider runs from 0.75 to 1.25 with `steps = 11`, rounded to 0.05 (desktop 0.7–1.5, `steps = 9`, rounded to 0.1); "Aa" still resets to 1 (05 §14 `appearanceAndroid_fontSizeSlider`) | `C/views/usersettings/Appearance.kt:231-265` |
| 04:114 | `appearance_zoom_sliderAndReset` | DESKTOP-ONLY | "Zoom" (`DensityScaleSection`) is only in the desktop layout; `densityScale` is read only on desktop | `D/views/usersettings/Appearance.desktop.kt:90,112-153`; `C/views/helpers/Utils.kt:661` |
| 04:115 | `appearance_settingsPersistAcrossRestart` | ADAPT | Theme and font only: there is no zoom on Android | `C/views/helpers/Utils.kt:661` |
| 04:121 | `network_rootLayout` | ADAPT | The screen ends with the section "Network connection" (network type and status dot; 05 §15 `networkAndroid_connectionStatusOnline`) | `C/views/usersettings/networkAndServers/NetworkAndServers.kt:306-316` |
| 04:157 | `network_remoteHostHidesLocalSettings` | DESKTOP-ONLY | Needs the app connected to a mobile host as controller (§K), which only the desktop is | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:163 | `calls_screenLayout_noLockScreenOption` | REPLACED | 05 §9 `callSettings_androidLockScreenOptions`: Android shows "Calls on lock screen:" | `C/views/usersettings/CallSettings.kt:45-49` |
| 04:179 | `dbMgmt_layout` | ADAPT | No "Open database folder" row | `C/views/database/DatabaseView.kt:239-246` |
| 04:183 | `dbMgmt_exportArchive` | ADAPT | R3: after export `ACTION_CREATE_DOCUMENT` fires with `simplex-chat.<timestamp>.zip`; the zip is read from the stub's URI and the toast "File saved" is recorded through the toast recorder (R11; 05 §11 `dbExport_androidCreateDocument`); the Log, prefs and stopped state as on desktop | `C/views/database/DatabaseView.kt:190-201,606-660`; `A/platform/Files.android.kt:36-43` |
| 04:184 | `dbMgmt_importArchive_restoresData` | ADAPT | R3: "Import database" fires `ACTION_GET_CONTENT` (`application/zip`), answered by the stub (05 §11 `dbImport_androidGetContent`); the rest as on desktop | `C/views/database/DatabaseView.kt:202-214` |
| 04:186 | `dbMgmt_importRestoresAppSettings` | ADAPT | R3: as above | `C/views/database/DatabaseView.kt:202-214` |
| 04:187 | `dbMgmt_deleteDatabase` | ADAPT | R4: the welcome screen that appears is the Android one ("Get started", no "Create your profile" or "Link a mobile"); with no user the Android branch only sets the welcome stage, without `startChatWithoutUser` | `C/platform/Core.kt:153-167`; `A/views/onboarding/SimpleXInfo.android.kt:11-16` |
| 04:188 | `dbMgmt_openDatabaseFolder` | DESKTOP-ONLY | No "Open database folder" on Android (`desktopOpenDatabaseDir` is empty there) | `C/views/database/DatabaseView.kt:239-246`; `A/platform/Files.android.kt:32` |
| 04:189 | `dbMgmt_disconnectMobilesWhenRemote` | DESKTOP-ONLY | Needs a connected mobile host (§K) | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:190 | `dbEncrypt_setFirstPassphrase` | ADAPT | The confirmation is the Keystore variant: "Database encryption passphrase will be updated and stored in the Keystore." (05 §11 `keystore_changePassphrase_storedAlert`); "Database encrypted!" and `InitialRandomDBPassphrase = false` as on desktop | `A/views/database/DatabaseEncryptionView.android.kt:95-103` |
| 04:192 | `dbEncrypt_removeFromSettings` | REPLACED | 05 §11 `keystore_removePassphrase_alertAndFooter`: the switch is "Save passphrase in Keystore" and the alert "Remove passphrase from Keystore?" | `A/views/database/DatabaseEncryptionView.android.kt:22-55,105-113` |
| 04:192 | `dbEncrypt_saveToSettings` | REPLACED | 05 §11 `keystore_reEnable_footerUntilRestart` | `A/views/database/DatabaseEncryptionView.android.kt:76-78` |
| 04:193 | `dbEncrypt_footerStates` | REPLACED | 05 §11 `keystore_freshDb_randomPassphraseFooter` (random), `keystore_changePassphrase_storedAlert` (stored), `keystore_removePassphrase_alertAndFooter` (not stored), `keystore_reEnable_footerUntilRestart`: the footers name the Keystore | `A/views/database/DatabaseEncryptionView.android.kt:57-83`; `D/views/database/DatabaseEncryptionView.desktop.kt:57-83` |
| 04:196 | `dbError_saveAndOpen` | ADAPT | "EncryptedDBPassphrase non-empty again" is checked with `DatabaseUtils.ksDatabasePassword.get()` returning the typed passphrase (the pref holds Keystore ciphertext) | `C/views/helpers/DatabaseUtils.kt:17-28` |
| 04:199 | `dbError_restoreBackup` | ADAPT | The fixture's `*.bak` files are encrypted with the key `ksDatabasePassword.get()` returns (the Keystore holds the stored key) | `C/views/helpers/DatabaseUtils.kt:17-28,51-60` |
| 04:200 | `dbError_openDatabaseFolder` | DESKTOP-ONLY | The error screens show the folder button only on desktop | `C/views/database/DatabaseErrorView.kt:323-332` |
| 04:216 | `migrateTo_fromCliArchive_chatMigrated` | ADAPT | R4: "Get started" → "Your profile" → "Migrate" (app bar); "Migrate here" shows "Scan QR code" (camera section) above "Or paste archive link", where the link is pasted | `C/views/WelcomeView.kt:200-225`; `C/views/migration/MigrateToDevice.kt:200-216` |
| 04:217 | `migrateTo_invalidLink` | ADAPT | As above | `C/views/migration/MigrateToDevice.kt:200-216` |
| 04:218 | `migrateTo_deletedFile` | ADAPT | As above | `C/views/migration/MigrateToDevice.kt:200-216` |
| 04:219 | `migrateTo_wrongPassphrase` | ADAPT | As above | `C/views/migration/MigrateToDevice.kt:200-216` |
| 04:220 | `migrateTo_networkSettingsConfirm` | ADAPT | As above | `C/views/migration/MigrateToDevice.kt:200-216` |
| 04:221 | `migrateTo_resumeAfterRestart` | ADAPT | As above ("Migrate" reopens "Migrate here" at "Enter passphrase") | `C/views/WelcomeView.kt:200-225` |
| 04:228 | `version_screenShowsVersions` | ADAPT | "App version: v%s" / "App build: %s" carry `ANDROID_VERSION_NAME` / `ANDROID_VERSION_CODE` (05 §15 `versionAndroid_showsAndroidBuildAndDevTools`) | `C/views/usersettings/VersionInfoView.kt:36-42` |
| 04:229 | `updates_channelDropdown` | DESKTOP-ONLY | "Check for updates" exists only in the desktop `AdvancedSettingsAppSection` | `D/views/usersettings/SettingsView.desktop.kt:14-26`; `A/views/usersettings/SettingsView.android.kt:22-29` |
| 04:230 | `updates_noticeDialog` | DESKTOP-ONLY | The update notice is shown by the desktop `Main.kt` platform object | `apps/multiplatform/desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt:67-89` |
| 04:237 | `dev_showConsoleInNewWindow_requiresAuth` | DESKTOP-ONLY | "Show console in new window" is desktop-only (05 §15 `devToolsAndroid_noConsoleWindowOption`) | `C/views/usersettings/DeveloperView.kt:50-60` |
| 04:240 | `dev_installTerminalLink` | ADAPT | R13: "Open" fires `ACTION_VIEW` with `https://github.com/simplex-chat/simplex-chat` (`INTENTS`) | `C/views/usersettings/SettingsView.kt:316-317`; `C/views/helpers/Utils.kt:613-624` |
| 04:247 | `help_layout` | ADAPT | "Support the project" also lists "Rate the app" (05 §15 `helpAndroid_rateTheAppOpensMarket`) | `C/views/usersettings/SettingsView.kt:173-181` |
| 04:251 | `help_sendEmail_mailto` | ADAPT | R13: "Send us email" fires `ACTION_VIEW` with `mailto:chat@simplex.chat` (`INTENTS`) | `C/views/usersettings/SettingsView.kt:170` |
| 04:252 | `help_contributeAndStar` | ADAPT | R13: each "Open" fires `ACTION_VIEW` with `https://github.com/simplex-chat/simplex-chat#contribute` / `https://github.com/simplex-chat/simplex-chat` (`INTENTS`) | `C/views/usersettings/SettingsView.kt:255-256,283-284`; `C/views/helpers/Utils.kt:613-624` |
| 04:266 | `linkMobile_layout` | DESKTOP-ONLY | Android is never the controller: its picker row is "Use from desktop" (05 §12 is the host side) | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:267 | `linkMobile_deviceNameChange` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:268 | `linkMobile_multicastToggle` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:269 | `linkMobile_newPairing_verifyAndConnect` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:270 | `linkMobile_remoteChatsVisible_sendMessage` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:271 | `linkMobile_switchLocalRemote` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:272 | `linkMobile_disconnectFromDesktop` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:273 | `linkMobile_mobileStopsSession` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:274 | `linkMobile_reconnectStoredPairing` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:275 | `linkMobile_deleteHost` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:276 | `linkMobile_customPortRefreshQr` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:276 | `linkMobile_openPortInfo` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:277 | `linkMobile_multicastDiscovery` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:278 | `linkMobile_closePairingStopsHost` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:279 | `linkMobile_remoteFiles` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:280 | `linkMobile_remoteFileNotFound` | DESKTOP-ONLY | As above | `C/views/chatlist/UserPicker.kt:354-366` |
| 04:281 | `linkMobile_onboardingLinkAMobile` | DESKTOP-ONLY | No "Link a mobile" on the Android welcome screen | `A/views/onboarding/SimpleXInfo.android.kt:11-16` |
| 04:288 | `badges_howItWorks` | ADAPT | R13: "Open" fires `ACTION_VIEW` with `https://simplex.chat/blog/20260919-simplex-supporter-badges.html` (`INTENTS`) | `C/views/badges/BadgesHowItWorksView.kt:33`; `C/views/onboarding/HowItWorks.kt:80`; `C/views/helpers/Utils.kt:613-624` |
| 04:289 | `badges_getYourCodeLink` | ADAPT | R13: "Open" fires `ACTION_VIEW` with `https://simplex.chat/badges/` (`INTENTS`) | `C/views/badges/BadgesSupportSimplexView.kt:130`; `C/views/helpers/Utils.kt:613-624` |
| 04:301 | `tray_closeToTrayBehaviour` | DESKTOP-ONLY | No tray on Android (`IGNORE` on desktop too) | `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt:33-121` |
| 04:301 | `tray_showQuitItems` | DESKTOP-ONLY | As above | `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt:33-121` |
| 04:302 | `window_statePersistence` | DESKTOP-ONLY | No desktop window on Android (`IGNORE` on desktop too) | `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt:34-120` |
| 04:302 | `window_crashRestartLoop` | DESKTOP-ONLY | As above | `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt:34-120,237-257` |
| 04:303 | `window_floatingConsole` | DESKTOP-ONLY | As above | `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt:210-232` |
| 04:304 | `updates_checkDownloadInstall` | DESKTOP-ONLY | The app updater is desktop-only (`IGNORE` on desktop too) | `D/views/helpers/AppUpdater.kt:179-490` |
