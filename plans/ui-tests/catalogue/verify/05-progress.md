# 05-android-only.md progress

## Done (survey notes, 2026-10-12)
- read legend, 01 model, research doc §5.3, plan §11.3, 04 §A/C/F/K/M
- surveyed: manifests, MainActivity (intents, back, FLAG_SECURE), SimplexApp (lifecycle, service start, platform iface), SimplexService (bg notices, battery), NtfManager.android, CallActivity, CallView.android (permissions, PiP, overlay), CallService, CallSettings, CallManager, IncomingCallAlertView, ConnectDesktopView, UserPicker(android), ChatListView android branches (SetNotificationsModeAdditions in chat list!), YourNetwork + SetNotificationsMode, NotificationsSettingsView, SettingsView (+android shutdown, rate app), PrivacySettings android (protect screen, lock mode), AppLock, LocalAuthentication.android, Cryptor/Keystore + DatabaseEncryptionView.android, QRCodeScanner.android (camera_not_available branch unreachable), ScanProtocolServer, ComposeView.android (attachments), GetImageView (camera/gallery, 10 limit), Share.android (chooser, open with), CIFileView android menu, ChatItemView.android (save image MediaStore), PlatformTextField.android (EditText! IME image commit), Appearance.android (icon, lang recreate), CustomTimePicker wheel, ChatView android branches (swipe reply, back handler, filter in menu, ConnectInProgressView)
- key facts: no FCM (INSTANT commented out); compose field is an EditText in AndroidView; Android onboarding has no passphrase step; Waydroid Android 13 → POST_NOTIFICATIONS runtime perm

- WRITTEN: header, Needs codes, IGNORE reasons, setup (8 items), §1 Onboarding (14), §2 ntf modes (20), §3 system ntfs (18), §4 permissions (13), §5 camera/QR (12, 7 IGNORE), §6 attachments (19), §7 share-in (15), §8 app links (8), §9 calls (19, 6 IGNORE), §10 lock/protect (19, 1 IGNORE), §11 keystore/archive (11, 1 IGNORE), §12 use-from-desktop (15), §13 back (16), §14 layout/gestures (25), §15 settings/lifecycle (14, 3 IGNORE), Notes (carry-over, excluded, bugs, open questions), Count; recount.py run: 243 tests / 20 IGNORE / 15 sections; header filled; no duplicate names across 01-05; all IGNORE cells match the defined reasons line numbers of androidMain files re-checked against real files (earlier stripped-import listings were wrong)

## Remaining
- nothing; file complete (243 tests, 20 IGNORE, 15 sections). Full read-through review done 2026-10-12.

## Open questions collected so far
1 legend/plan: new Needs codes + IGNORE reasons + class-map rows need adding to 00/plan (fenced)
2 captive_portal_mode 0 → network validated under net isolate?
3 rotation on Waydroid
4 FLAG_SECURE vs captureToImage
5 PendingIntent.send of system intents vs Espresso-Intents
6 MediaProvider NPE for MediaStore insert (save image)
7 WebView loads call page on Waydroid?
8 BUG? lock-screen 'Open' OpenChatAction ignored by onNewIntent (MainActivity.kt:64-68)
9 Use-from-desktop controller CLI must listen on the waydroid bridge (host netns); multicast over bridge unverified
