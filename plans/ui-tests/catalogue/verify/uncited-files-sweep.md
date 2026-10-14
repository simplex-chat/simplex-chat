# Uncited desktop UI files: coverage sweep

Report only; no catalogue edits. Code: ~/wt-uitest-ref (master d72f68e6d). Paths `C/` = commonMain `chat/simplex/common/`, `D/` = desktopMain.

## Remaining

(none: all 21 files classified; drafted rows below)

## Results

Class key: **(a)** covered indirectly, **(b)** desktop-unreachable / Android-only, **(c)** out of reach in-process (§11.3), **(d)** real gap (rows drafted at the end, §Drafted rows).

### call/CallView.kt — (a) + (c), plus one adjacent gap (D1)
- `expect fun ActiveCallView()` (CallView.kt:11) is rendered on desktop by `App.kt:226-238` when `chatModel.showCallView` is true and the platform is not Android (`App.kt:236`). The desktop actual (`D/views/call/CallView.desktop.kt:32-136`) starts the local call server and opens the call page through `LocalUriHandler` (`WebRTCController`, CallView.desktop.kt:153-171).
- Reached by 02:30 `toolbarCall_audio_opensCallLink` (outgoing: the URI stub records the call link) and 02:256 `callAlert_incoming_acceptRejectIgnore` ("Accept" opens the call view, link recorded; it cites CallView.desktop.kt:158-167).
- `activeCallWaitDeliveryReceipt` (CallView.kt:13-28) runs only from the `WCallResponse.Capabilities` branch (CallView.desktop.kt:40-46), which the browser page sends; the stub doesn't run the page → (c) "Real WebRTC calls", already the IGNORE row 02:279 `call_webrtcSession_connects`.
- Adjacent gap (in the desktop actual, not in CallView.kt): when `uriHandler.openUri` throws, the app shows "Error opening browser" / "The default web browser is required for calls. …" (`unable_to_open_browser_title/desc`, strings.xml:1291-1292) and ends the call (CallView.desktop.kt:161-170). No row asserts it. Drafted as **D1**; it needs the URI stub to be able to throw (a harness option, not a new Needs code).

### chat/ComposeChatLinkView.kt — (a)
- Rendered by `ComposeView.previewView` for `ComposePreview.ChatLinkPreview` (ComposeView.kt:1202-1206), which is set only from `SharedContent.ChatLink` / `SharedContent.MyAddress` after picking a destination in the share list (ComposeView.kt:1634-1665; set at GroupLinkView.kt:271, GroupChatInfoView.kt:649, UserAddressView.kt:360).
- Covered by 01:223 `addressShareViaChat` ("bob's chat opens with the address card in the compose preview (`ComposeView.kt:1651-1665`); click send") and 03:257 `channelLink_shareViaChatToContactSigned`, `channelLink_shareViaChatToGroup`, 03:283 `subscriberInfo_shareViaChat`.
- Not asserted: the preview's (x) (ComposeChatLinkView.kt:47-51 → `NoPreview`, nothing sent). Minor; could be added to `addressShareViaChat`'s verification rather than a new test. Not drafted.

### chat/ComposeFileView.kt — (a), plus an edit-mode gap (D2)
- Rendered for `ComposePreview.FilePreview` (ComposeView.kt:1219-1223). Covered by 02:88 `attachFile_send_peerReceivesOffer`, `attachFile_withText_peerReceivesTextAndFile`; 02:89 `attachFile_cancelPreview_nothingSent` (clicks "Cancel file preview", `icon_descr_cancel_file_preview`, ComposeFileView.kt:38-45); 02:95-96 `paste_filePathText_attachesFile`, `ctrlV_clipboardFiles_attaches`; 02:233 `draft_withAttachment_restored`.
- Gap: `cancelEnabled = !editing && !inProgress` (ComposeView.kt:1222). Editing an own file/image message puts the file into the preview via `chatItemPreview` (ComposeView.kt:264-278) with no (x). No row edits alice's own file or image message (02 §I edits text; `edit_imageCaption_updates` is bob's edit). Drafted as **D2** (shared with ComposeImageView).

### chat/ComposeImageView.kt — (a), edit-mode gap shared with D2
- Rendered for `ComposePreview.MediaPreview` (ComposeView.kt:1207-1211). Covered by 02:90 `attachImage_send_peerReceivesCompressedImage`, `attachImages_multiple_sentAsSeparateMessages`, `attachImage_textOnLastImageOnly`; 02:91 `attachImage_cancelPreview` ("Cancel image preview", ComposeImageView.kt:63-70); 02:92 `attachGif_sentUncompressedAnimated`; the video branch (ComposeImageView.kt:37-52) by 02:93 `attachVideo_send_peerReceivesVideo` (preview frame from the AV seam); 02:98 `ctrlV_clipboardImage_attachesPng`.
- Edit mode (no (x)) → **D2**.

### chat/ComposeVoiceView.kt — (a), plus a playback gap (D3)
- Rendered for `ComposePreview.VoicePreview` (ComposeView.kt:1212-1218). Covered by 02 §E (lines 75-76) `voice_record_send_peerReceivesVoice`, `voice_tapToRecord_stopButton_addsPreview`, `voice_preview_cancel_deletesTmpFile` (the (x) at ComposeVoiceView.kt:87-100), 02:79 `voice_disabledWhileRecording_cancels`, 02:82 `voice_notes_recordAndSave`, 02:234 `draft_voiceRecording_finishedOnSwitch`. `RecordingInProgressSlider` (:137-163) is on screen during every REC test.
- Gap: playing the finished preview before sending — play/pause button enabled only when `finishedRecording` (ComposeVoiceView.kt:53-70, `AudioPlayer.play`/`pause`), the duration text switching to playback progress (:71-84), and seeking on `FinishedRecordingSlider` (:115-134, `AudioPlayer.seekTo`). 02:81 `voice_playback_playPauseProgress` covers playback of a *received* item (CIVoiceView) only. Drafted as **D3**. Note: the play button's content description is "File" (`icon_descr_file`, ComposeVoiceView.kt:66), so K2 needs a key for it.

### chat/item/CIBrokenComposableView.kt — (c)
- Rendered by `ChatItemViewShortHand` as the `tryOrShowError` fallback (ChatView.kt:1954-1956) only when the item's key is in `failedComposables` (Utils.kt:636-644). Keys are added only by `includeMoreFailedComposables()` from the desktop window exception handler (DesktopApp.kt:42-53), i.e. after a composition crash inside a real `Window` and the restart loop of `showApp()` (DesktopApp.kt:37-40).
- That is §11.3 "the crash-restart loop and crash alert"; already the IGNORE row 04:301 `window_crashRestartLoop`. No new row. (A unit test could cover `tryOrShowError` by seeding `failedComposables`, but that is not a UI feature path.)

### chat/item/CIChatFeatureView.kt — (a) for single items; gap for the merged icon row (D4)
- Rendered for Rcv/SndChatFeature, SndChatPreference, Rcv/SndGroupFeature and Rcv*FeatureRejected (ChatItemView.kt:757-788).
- Single items (`FullFeatureView`, CIChatFeatureView.kt:117-135) are covered by 02:253 `featureItem_contactChangesPref_shown` (cites CIV:757-789), 03:104 `groupPrefs_receivedUpdateShownAsEvent`, 03:31 `createGroup_opensChatAndInviteSheet` ("with its feature items"), 02:70 `timed_chatPref_receivedMessagesDisappear` ("Disappearing messages: 3 sec").
- Gap: consecutive feature items share `CIMergeCategory.ChatFeature` (ChatModel.kt:3457-3461) and, unless revealed, collapse into one row of feature icons (`mergedFeatures`, CIChatFeatureView.kt:75-92; `FeatureIconView` :104-114, the timed feature shows its TTL text); the item menu then offers "Expand"/"Hide" (ChatItemView.kt:529-541, `expand_verb`/`hide_verb`). Every new contact gets one such run: on connection the core creates a `CIRcvChatFeature` item for each of the five UI features (`createFeatureEnabledItems_`, Library/Internal.hs:3253-3257, called at Library/Subscriber.hs:679; `allChatFeatures`, Types/Preferences.hs:82-91). No row asserts the icon row or expanding it (03:233 `groupEvents_expandCollapsed` covers group *events*, a different category). Drafted as **D4**.
- Risk for an existing row (not changed here): in 02:253 `featureItem_contactChangesPref_shown`, bob's `/set voice @alice no` adds a feature item; if it directly follows the connection's feature run it is merged into the icon row, and the text "Voice messages: …" is visible only after "Expand". The row should either expand first or put a message between.

### chat/item/CIChatLinkHeader.kt — (a); card contents not asserted (D5)
- Rendered for `MsgContent.MCChat` items (FramedItemView.kt:402-416); a click runs `planAndConnect` with the owner signature.
- Reached by 01:193 `planViaChatItemLinkCard` (taps the card in a message; verification "Plan alert"), by the sent items of 01:223 `addressShareViaChat` and 03:257 `channelLink_shareViaChatToContactSigned` / `channelLink_shareViaChatToGroup`.
- Not asserted anywhere: the card itself — display name, the info line (`MsgChatLink.infoLine`, ChatModel.kt:5111-5121: "Contact address" / "Business address" / "Group link" / "Channel link" / "One-time link", plus " (from owner)" when signed, strings.xml:577-582) and "Tap to open" (strings.xml:576). Drafted as **D5** (one test; it could instead be folded into `planViaChatItemLinkCard`'s verification).

### chat/item/CIEventView.kt — (a)
- `EventItemView()` (ChatItemView.kt:640-643) renders it for group events, direct connection events and the merged-event text (`mergedGroupEventText`, :605-622).
- Covered by 03:233 `groupEvents_memberConnectedMerged`, `groupEvents_roleChangeItems`, `groupEvents_memberLeftAndRemoved`, `groupEvents_groupProfileUpdated`, `groupEvents_expandCollapsed` (the row cites "CIEventView"), 02:258 `events_merged_deleteRange`, 02:157 `select_mergedEvents_selectsWholeRange`, 02:260 decryption/sync event rows, 03:112 review rows (pending-review events).

### chat/item/DeletedItemView.kt — (d), low priority (D6)
- Real caller only `LegacyDeletedItem()` (ChatItemView.kt:588-598) for `CIContent.SndDeleted`/`RcvDeleted` (:705-706); MarkedDeletedItemView.kt:120 is a `@Preview`.
- The core never creates these contents any more ("legacy - since v4.3.0 item_deleted field is used", Messages/CIContent.hs:145-146; no producer under `src/Simplex/Chat/`), but still decodes them from the database (`DBJCISndDeleted`/`DBJCIRcvDeleted`, CIContent.hs:591-592, `singleFieldJSON`, :748), so pre-4.3 databases render them. Reachable only from a fixture row, like 02:262 `invalidJson_item_rendered`. Drafted as **D6**; drop it if pre-4.3 data is considered out of scope.

### chat/item/EmojiItemView.kt — (a)
- Rendered for a plain short-emoji `MCText` item with no quote, forward, deletion or live flag (ChatItemView.kt:574-576; `isShortEmoji` EmojiItemView.kt:58).
- Covered by 02:41 `emojiOnly_rendersLargeEmoji` (cites CIV:575). `EmojiText` is also used by FramedItemView.kt:349-357 (emoji-only text with a quote/forward), reached by 02 §H/§L rows only when their text is an emoji; not separately needed.

### chatlist/ShareListNavLinkView.kt — (a); prohibited destination not asserted (D7)
- One row per destination in `ShareListView` (ShareListView.kt:214). Reached by every forward and share-via-chat test: 02 §L `forward_contactToContact_peerSeesForwarded`, `forward_contactToGroup`, `forward_toNotes_showsSavedFrom`, … (02:162-168), 01:223 `addressShareViaChat`, 03:257, 03:283 `subscriberInfo_shareViaChat`.
- Gap: a destination whose preferences prohibit the content (group with "Files and media" off for a file/image, "SimpleX links" off for a message with a SimpleX link, voice prohibited for a voice message) is shown greyed and its click shows "Cannot send message" / "Selected chat preferences prohibit this message." (ShareListNavLinkView.kt:30-58, 70-75; strings.xml:573-574). No row covers it. Drafted as **D7**. (The rows are also disabled while the chat is stopped, :26, 82 — not drafted: the share list can't be opened with the chat stopped in a normal flow.)

### chat/ScanCodeView.kt — (b)
- Only caller VerifyCodeView.kt:109-115, inside `if (appPlatform.isAndroid)`; the desktop `QRCodeScanner` actual is empty (`D/views/newchat/QRCodeScanner.desktop.kt`). Desktop rows for the same screen assert only "Mark verified"/"Clear verification" (01:251, 03:178); 01:283 lists "Scan code" under "Not on desktop".
- 05 covers it: 05:165 `verifyCode_androidScanCodeButtonOpensScanner`, 05:166 `qrScan_verifySecurityCode` (IGNORE, no camera).

### localauth/PasswordEntry.kt — (a); on-screen keypad not asserted (D8)
- `PasscodeEntry` (PasswordEntry.kt:22-37) is the digit display plus keypad of every passcode screen: `PasscodeView.kt:77` (vertical, the only layout on desktop: `PasscodeView.kt:130-131`), used by `LocalAuthView.kt:26` (lock screen) and `SetAppPasscodeView.kt:32` (new / confirm / change / self-destruct passcodes).
- Rendered by every 04 §C row: 04:76 `lock_enable_setsPasscode`, 04:80 `lock_changePasscode_success`, `lock_changePasscode_wrongCurrent`, 04:81 `lock_disable_requiresPasscode`, 04:82 `lock_screenOnStartup_correct`, `lock_screenOnStartup_wrong_showsUnlock`, 04:87-93 self-destruct rows. Those rows type the digits from the keyboard ("digits + Enter"); 04:86 `lock_passcodeKeyboardKeys` covers only `PasscodeView.handleKeyboard` (PasscodeView.kt:34-62).
- Gap: the keypad itself — digit buttons (`PasswordDigit`, PasswordEntry.kt:118-136, max 16 digits), clear (x) and backspace (`PasswordEdit`, :59-67), and the click on the masked display that toggles "●" vs digits, grouped by 8 below 8 digits (`PasscodeView`/`splitPassword`, :40-48, :101-104). Drafted as **D8**. The clear and backspace icons have no content description (`Icon(image, null, …)`, :140), and the digits are plain `Text`, so K2 needs keys for them.
- `VerticalDivider` (PasswordEntry.kt:157-181) is also the pane divider of the desktop layout (App.kt:445, :464; UserPicker.kt:323), on screen in every test.

### newchat/QRCode.kt — (a); click-to-save not covered (D9)
- `SimpleXCreatedLinkQRCode` / `SimpleXLinkQRCode` / `QRCode` are drawn by: NewChatView.kt:498/510 → 01:151 `createOneTimeLinkShowsLinkAndQr`; ContactConnectionInfoView.kt:137 → 01:209 `pendingInfoInvitedShowsLinkQrShare`; UserAddressView.kt:338/347 → 01:222 `addressQrAndShareAddress`; GroupLinkView.kt:239 → 03:122 `groupLink_createdAutomatically`, 03:123 `groupLink_toggleFullShortLink`, 03:35 `createGroupIncognito_opensGroupLinkStep`; GroupMemberInfoView.kt:578 → 03:184 `memberInfo_memberAddressConnect`; GroupChatInfoView.kt:1276 → 03:283 `subscriberInfo_rows`; VerifyCodeView.kt:75 → 01:251 `contactInfoVerifyCodeMatchesCli`, 03:178; ProtocolServerView.kt:207 → 04:133 `serverView_addToAnotherDeviceQr`; ConnectMobileView.kt:195 → 01:38 `onboardingLinkAMobileShowsQrAndDeviceName`, 04:268 `linkMobile_newPairing_verifyAndConnect`, 04:273, 04:275; MigrateFromDevice.kt:370 → 04:210 (IGNORE). `ImageBitmap.replaceColor` has its desktop actual in `D/views/newchat/QRCode.desktop.kt` and runs on every render.
- Gap: clicking any QR code (QRCode.kt:89-99): `saveTempImageUncompressed(image, asPng = true)` opens a directories-only save dialog and writes `IMG_<ts>.png` there (`D/views/helpers/Utils.desktop.kt:239-252`; DefaultDialog.desktop.kt:98-101), then `shareFile` opens a second save dialog for a copy (`D/platform/Share.desktop.kt:25-43` → `FileChooserLauncher(false)`, Files.desktop.kt:77-81), and `onShare` marks the 1-time invitation used (NewChatView.kt:498/510 → ChatModel.kt:1249-1251), which suppresses "Keep unused invitation?" on close (NewChatView.kt:76-96). `saveTempImageUncompressed` has no other caller. Plan §7.6 already names this dialog; no row drives it. Drafted as **D9** (in 01 §7; the click is the same on every screen).

### newchat/QRCodeScanner.kt — (b)
- The desktop actual is empty (`D/views/newchat/QRCodeScanner.desktop.kt`, body `//LALAL`), and every caller is behind `appPlatform.isAndroid`: NewChatView.kt:652-656, MigrateToDevice.kt:206-208, BadgesRedeemCodeView.kt:143-144, ScanCodeView (VerifyCodeView.kt:109), ScanProtocolServer (ProtocolServersView.kt:281), ConnectDesktopView (UserPicker.kt:354).
- 05 §5 covers it (05:148-150 permission/hidden states; 05:164-171 scan rows, IGNORE "no camera on the test runtime"). Desktop absence is stated at 01:283.

### onboarding/OnboardingLayout.kt — (a)
- `DesktopOnboardingShell` (OnboardingLayout.kt:131-161) wraps every desktop onboarding stage except LinkAMobile (App.kt:274-292), with a per-stage image; `OnboardingShrinkingLayout` (:39-84) lays out SimpleXInfo.kt:114, YourNetwork.kt:63, ChooseServerOperators.kt:59; `OnboardingImage` (:86) also WelcomeView.kt:254.
- Covered by 01 §1: 01:27 `onboardingWelcomeShowsDesktopActions`, 01:28 `onboardingCreateProfileValidName` …, 01:30 `onboardingSetDatabasePassphrase`, 01:32 `onboardingYourNetworkShowsSetupRouters`, 01:33 `onboardingChooseOperatorsToggle`, 01:34 `onboardingAcceptConditionsCompletesOnboarding`; and 04 §J `help_aboutSimplex` (SimpleXInfo outside onboarding). The images and shrink layout are visual only (screenshots, plan §7.7); nothing else to assert.

### remote/ConnectDesktopView.kt — (b)
- Its only entry is the user-picker row "Use from desktop", shown only on Android (UserPicker.kt:354-366); desktop shows "Link a mobile"/"Linked mobiles" instead (:367-370), which is 04 §K.
- 05 §12 covers it fully (05:315-329, 16 tests), plus 05:169 `qrScan_desktopAddress` (IGNORE). 05:415 notes `showUnlinkDesktopAlert` has no caller.

### SplashView.kt — (a)
- `SplashView()` (App.kt:156) is the blank surface while `chatDbEncrypted`/`localUserCreated` are still unknown at boot — every test passes through it, and the harness's boot wait (plan §7.2) ends after it. `SplashView(true)` (App.kt:220-223) is the opaque cover behind the passcode while the app is locked.
- Covered by 04:82 `lock_screenOnStartup_correct`, `lock_screenOnStartup_wrong_showsUnlock` and 04:83 `lock_relocksAfterDelay` (lock screen shown). It has no text or controls.
- Suggestion (not a new row): the main screen stays composed beneath the cover (App.kt:142-173 runs before the `unauthorized` block), so a semantics query still finds the chat list while locked. If a lock row should prove that content is hidden, it has to use the screenshot (plan §7.7) or a click at a chat row's position that must not open the chat (the `Surface` consumes pointer input), not node absence.

### usersettings/MarkdownHelpView.kt — (a)
- Only real caller ChatHelpView.kt:72 (section "Markdown in messages"), shown by `HelpView` (HelpView.kt:18-28) from Settings > "How to use it" (SettingsView.kt:159).
- Covered by 04:247 `help_howToUse` ("Welcome <name>!" + help text). Static text only (`*bold*`, `_italic_`, `~strike~`, `` `a + b` ``, `!1 colored!`, `#secret#`, MarkdownHelpView.kt:24-57). Optionally add these six syntax lines to `help_howToUse`'s verification; no new test.

### usersettings/networkAndServers/ScanProtocolServer.kt — (b)
- `ScanProtocolServer` is opened only from the "Add server" alert's "Scan server QR code" item, inside `if (appPlatform.isAndroid)` (ProtocolServersView.kt:281-299). The desktop actual (`ScanProtocolServer.desktop.kt:8`) delegates to the layout but has no entry point, and its scanner is the empty desktop `QRCodeScanner`.
- 04:307 lists "Scan server QR code" as Android-only; 05:153 `scanServer_requestsCameraPermission` and 05:167 `qrScan_serverAddress` (IGNORE) cover it.

## Summary

| Class | Files |
|---|---|
| (a) covered indirectly | ComposeChatLinkView, ComposeFileView, ComposeImageView, ComposeVoiceView, CIChatFeatureView, CIChatLinkHeader, CIEventView, EmojiItemView, ShareListNavLinkView, PasswordEntry, QRCode, OnboardingLayout, SplashView, MarkdownHelpView, CallView (ActiveCallView) — 15 |
| (b) Android-only, covered by 05 | ScanCodeView, QRCodeScanner, ConnectDesktopView, ScanProtocolServer — 4 |
| (c) out of reach (§11.3, existing IGNORE rows) | CIBrokenComposableView (crash-restart loop, 04:301), CallView's `activeCallWaitDeliveryReceipt` (WebRTC, 02:279) — 1 file + 1 function |
| (d) real gap with no existing coverage | DeletedItemView (legacy items) — 1 |

Partial gaps inside (a) files: D1–D5, D7–D9. Possible problem in an existing row: 02:253 `featureItem_contactChangesPref_shown` (merged feature items, see CIChatFeatureView).

## Drafted rows

Each row uses its target catalogue's columns and naming style. Names were checked to be unused across 01–05. Recount and class map need regenerating when they are added (`tools/recount.py`, `tools/classmap.py table`).

**D1 → 02 §A (Chat view shell, toolbar, banner), after `toolbarCall_audio_opensCallLink`** (calls are owned by 02)

| Area | Feature | Trigger | File:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Toolbar | Call when no browser can be opened | The `LocalUriHandler` stub is set to throw; call icon → "Audio call" | `D/views/call/CallView.desktop.kt:153-171`, `C/views/call/CallManager.kt:75-97`, `C/App.kt:226-238` | `callStart_browserUnavailable_showsAlertAndEnds` | Alert "Error opening browser" / "The default web browser is required for calls. Please configure the default browser in the system, and share more information with the developers." (`unable_to_open_browser_title`/`_desc`, strings.xml:1291-1292); the call ends at once (`endCall` sets `showCallView` false and `activeCall` null, CallManager.kt:81-86), so no outgoing-call waiting state remains under the alert. bob prints nothing: the invitation is sent only from the `Capabilities` response (CallView.desktop.kt:40-46), and `apiEndCall` fails silently with no current call (`withCurrentCall`, `Library/Commands.hs:1579-1584`; `apiEndCall` returns false, `C/model/SimpleXAPI.kt:2160-2163`) | P, URI | Needs a throwing mode in the recording `LocalUriHandler` (harness option, no new code). `endCall` also calls `AppLock.appWasHidden()` (CallManager.kt:82); irrelevant with the lock off |

**D2 → 02 §I (Edit and history), after `edit_imageCaption_updates`**

| Area | Feature | Trigger | File:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Edit | Edit the caption of alice's own image or file | alice sends `test.jpg` with the text "hi" ("Image" → dialog), and `test.pdf` with "doc" ("File" → dialog); "Edit" on each → change the text → Enter | CIV:464-469, CMP:123-136, CMP:264-278, CMP:1207-1223, `C/views/chat/ComposeImageView.kt:63-70`, `C/views/chat/ComposeFileView.kt:37-46` | `edit_ownImage_previewWithoutCancel`, `edit_ownFile_previewWithoutCancel` | While editing, the compose area shows the image thumbnail / the file name "test.pdf" with no "Cancel image preview" / "Cancel file preview" button (`cancelEnabled = !editing && !inProgress`, CMP:1210, 1222) and the field holds "hi" / "doc"; after Enter bob prints `bob <# "alice> [edited] hello"` (resp. the new file text) — `ttyFromContactEdited` + `ttyMsgContent` (`src/Simplex/Chat/View.hs:3066-3067`, :2431-2438); the image/file itself is not re-sent | P, XFTP, FD | Voice items are not editable (CIV:464), so ComposeVoiceView never appears in edit mode; MCChat items get no preview while editing (CMP:276) |

**D3 → 02 §E (Voice messages), after `voice_playback_playPauseProgress`**

| Area | Feature | Trigger | File:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Record | Play the recorded preview before sending | In notes: "Record voice message" (tap), stop → preview; play, pause, drag the slider | CMP:1212-1218, `C/views/chat/ComposeVoiceView.kt:53-84, 104-134` | `voice_preview_playPauseAndSeek` | While recording the play button is disabled and the duration counts up (`RecordingInProgressSlider`); after stop the button is enabled and shows the total duration; play → pause icon and the text follows playback progress; pause keeps the position; dragging the slider seeks (`AudioPlayer.seekTo`); the item is not sent | REC, AV | The play button's content description is "File" (`icon_descr_file`, ComposeVoiceView.kt:66): K2 key needed. Notes chat avoids a peer (as `voice_notes_recordAndSave`) |

**D4 → 02 §U (Special items), next to `featureItem_contactChangesPref_shown`**

| Area | Feature | Trigger | File:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Features | Merged feature items: icon row, Expand, Hide | alice connects with bob; open the chat; right-click the feature row → "Expand", then right-click an item → "Hide" | CIV:529-541, CIV:757-788, `C/views/chat/item/CIChatFeatureView.kt:28-48, 75-114`, `C/model/ChatModel.kt:3453-3461` | `featureItems_merged_iconRowExpandHide` | After connecting, alice's chat shows one row of five feature icons (one `CIRcvChatFeature` per UI feature, `createFeatureEnabledItems_`, `Library/Internal.hs:3253-3257`, `Types/Preferences.hs:82-91`); "Expand" (`expand_verb`) shows five items "Disappearing messages: off", "Delete for everyone: off", "Message reactions: enabled", "Voice messages: enabled", "Audio/video calls: enabled" (`featureText`, ChatModel.kt:4329-4339; defaults `defaultChatPrefs`, Types/Preferences.hs:553-564); "Hide" (`hide_verb`) collapses them back to the icon row | P | The icons have no content description (CIChatFeatureView.kt:105): K2 key needed. A later feature item that directly follows this run joins the row, which affects `featureItem_contactChangesPref_shown` |

**D5 → 01 §8 (Paste link and connection plans), after `planViaChatItemLinkCard`** (the link card is owned by 01)

| Area | Feature | Entry path | Main file:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Card | Link card contents | CLI `/ad`, then `/share address @alice`; open the chat with bob | `C/views/chat/item/CIChatLinkHeader.kt:18-78`; `C/views/chat/item/FramedItemView.kt:402-416`; `C/model/ChatModel.kt:5111-5121` | `linkCardInMessageShowsNameAndSignedInfoLine` | The received item shows the card: "bob", the info line "Contact address (from owner)" (`chat_link_contact_address` + `chat_link_from_owner`, strings.xml:580-582) and "Tap to open" (strings.xml:576); no text below the card (the message text equals the link). bob printed `@alice contact address of @bob (signed):` and the link (as in `testShareAddressViaChat`, tests/ChatTests/Profiles.hs:3944-3946) | P | Same setup as `planSignedAddressCardVerified`; could instead extend `planViaChatItemLinkCard`'s verification |

**D6 → 02 §U (Special items), after `invalidJson_item_rendered`** (low priority)

| Area | Feature | Trigger | File:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Other | Legacy deleted item | alice sends "hi" to bob; between two `restart()` steps alice's row is rewritten: `UPDATE chat_items SET item_content = '{"sndDeleted":{"deleteMode":"broadcast"}}', item_content_tag = 'sndDeleted' WHERE item_text = 'hi'` | CIV:588-598, 705-706; `C/views/chat/item/DeletedItemView.kt:19-43` | `legacyDeletedItem_rendered` | The item shows italic "deleted" (`deleted_description`, strings.xml:49; ChatModel.kt:4261) in a sent bubble with its meta; its menu has only "Info", "Delete" and "Select" | P, RESTART, FIXTURE (legacy deleted item) | The core no longer creates these contents (`Messages/CIContent.hs:145-146`) but still decodes them (`DBJCISndDeleted`, :591, `singleFieldJSON`, :748), so only pre-4.3 databases have them |

**D7 → 02 §L (Forwarding), after `forward_notOffered_forDeletedOrLive`**

| Area | Feature | Trigger | File:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Rules | Destination whose preferences prohibit the content | bob hosts group `team` with alice and runs `/set files #team off`; bob `/img @alice tests/fixtures/test.jpg`; alice: "Forward" on the image → pick "team" | `C/views/chatlist/ShareListView.kt:30-56`; `C/views/chatlist/ShareListNavLinkView.kt:43-58, 70-75` | `forward_destinationProhibitsContent_alert` | "team" is listed but disabled-looking; clicking it shows "Cannot send message" / "Selected chat preferences prohibit this message." (strings.xml:573-574); the group chat doesn't open and no forwarding bar appears; bob prints nothing in `#team` | P, XFTP | After bob's `/set files #team off` (CLI `Files and media: off`, `testProhibitFiles`, tests/ChatTests/Files.hs:1248-1259) alice's group gets a "Files and media: off" feature item; wait for it before forwarding. The same alert covers voice to a contact that prohibits voice and SimpleX links (also "Share via chat" of an address or channel link, which always counts as a SimpleX link, ShareListView.kt:54-56) to a group with "SimpleX links" off |

**D8 → 04 §C (SimpleX Lock), after `lock_passcodeKeyboardKeys`**

| Area | Feature | Entry path | Main file:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Lock | On-screen keypad | Settings > Your privacy > "SimpleX Lock" > "Enable lock" → "New Passcode": click 1 2 3 4 5, backspace, the dots, clear (x), then 1 2 3 4 → "Save" → "Confirm passcode": 1 2 3 4 → "Confirm" | `C/views/localauth/PasswordEntry.kt:22-48, 50-69, 101-145`; `C/views/localauth/PasscodeView.kt:64-84, 130-131` | `lock_passcodeKeypad_digitsBackspaceClearReveal` | Five clicks show "●●●●●"; backspace leaves "●●●●"; clicking the dots shows "1234" and clicking again masks it; clear (x) empties the field and "Save" is disabled (fewer than 4 digits, PasscodeView.kt:82); after confirming, "Passcode set!" and PerformLA=true as in `lock_enable_setsPasscode` | — | Desktop always uses the vertical keypad (PasscodeView.kt:130-131). Clear and backspace have no content description and digits are plain text: K2 keys needed |

**D9 → 01 §7 (1-time link), after `oneTimeLinkUsedNoAlert`**

| Area | Feature | Entry path | Main file:line | Test case(s) | Verification | Needs | Notes |
|---|---|---|---|---|---|---|---|
| Invite | Save the QR code by clicking it | "Create 1-time link" → click the QR code ("QR Code", `image_descr_qr_code`, strings.xml:897) → choose a folder → choose a file for the copy → close the sheet | `C/views/newchat/QRCode.kt:89-99`; `D/views/helpers/Utils.desktop.kt:239-252`; `D/platform/Share.desktop.kt:25-43`; `NewChatView.kt:498,510,76-96` | `oneTimeLinkQrClickSavesPng` | The first save dialog is directories-only; after `respondSave(<dir>)` the folder holds one `IMG_<ts>.png`; a second save dialog awaits and `respondSave(<file>)` writes a copy there; closing the sheet shows no "Keep unused invitation?" alert (`onShare` → `markShowingInvitationUsed`, ChatModel.kt:1249-1251). Optionally decode the PNG with BoofCV (already an app dependency, QRCode.kt) and compare it with `chatModel.showingInvitation`'s short link | FD | The same click exists on every QR code (address, group link, member address, server, Link a mobile); one row covers the code path |
