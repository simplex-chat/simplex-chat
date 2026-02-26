# SimpleX Chat Android & Desktop -- Impact Graph

> Source file to product concept mapping. Use this to identify which product documents must be updated when a source file changes.
>
> Covers Kotlin Multiplatform (Compose) sources: commonMain, androidMain, desktopMain, and the Android and Desktop app modules. Also covers the shared Haskell core.

---

## Product Concept Legend

| ID | Concept |
|----|---------|
| PC1 | Chat List |
| PC2 | Direct Chat |
| PC3 | Group Chat |
| PC4 | Message Composition |
| PC5 | Message Reactions |
| PC6 | Message Editing |
| PC7 | Message Deletion |
| PC8 | Timed Messages |
| PC9 | Voice Messages |
| PC10 | File Transfer |
| PC11 | Link Previews |
| PC12 | Contact Connection |
| PC13 | Contact Verification |
| PC14 | Group Management |
| PC15 | Group Links |
| PC16 | Member Roles |
| PC17 | Audio/Video Calls |
| PC18 | Notifications |
| PC19 | User Profiles |
| PC20 | Incognito Mode |
| PC21 | Hidden Profiles |
| PC22 | Local Authentication |
| PC23 | Database Encryption |
| PC24 | Theme System |
| PC25 | Network Configuration |
| PC26 | Device Migration |
| PC27 | Remote Desktop |
| PC28 | Chat Tags |
| PC29 | User Address |
| PC30 | Member Support Chat |

---

## 1. Common Sources (commonMain)

Path prefix: `common/src/commonMain/kotlin/chat/simplex/common/`

### 1.1 Core Model & Platform

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `App.kt` | PC1 through PC30 | High | Root composable — navigation scaffold for all features |
| `AppLock.kt` | PC22 | Medium | App lock state and authorization lifecycle |
| `model/ChatModel.kt` | PC1 through PC30 | High | Central state object — every feature reads or writes here |
| `model/SimpleXAPI.kt` | PC1 through PC30 | High | FFI bridge to Haskell core — all commands and responses |
| `model/CryptoFile.kt` | PC10, PC23 | Medium | Encrypted file read/write helpers |
| `platform/Core.kt` | PC1 through PC30 | High | Native FFI declarations (`chatMigrateInit`, `chatSendCmd`, etc.) — all API traffic |
| `platform/AppCommon.kt` | PC1 through PC30 | Medium | Shared app initialization logic |
| `platform/Files.kt` | PC10, PC23, PC26 | Medium | File path resolution, temp dirs, encryption utilities |
| `platform/NtfManager.kt` | PC18 | High | Notification manager expect declarations |
| `platform/Notifications.kt` | PC18 | Medium | Notification channel and permission abstractions |
| `platform/SimplexService.kt` | PC18 | Medium | Background service expect declarations |
| `platform/RecAndPlay.kt` | PC9 | Medium | Audio recording and playback abstractions |
| `platform/VideoPlayer.kt` | PC10, PC17 | Low | Video playback abstractions |
| `platform/Cryptor.kt` | PC23 | Medium | Keystore encryption expect declarations |
| `platform/Share.kt` | PC10, PC12 | Low | Share sheet abstractions |
| `platform/Images.kt` | PC10, PC19 | Low | Image processing utilities |
| `platform/Platform.kt` | PC1 through PC30 | Low | Platform detection and capability flags |
| `platform/PlatformTextField.kt` | PC4 | Low | Native text input expect declarations |
| `platform/Back.kt` | PC1 | Low | Back navigation handling |
| `platform/UI.kt` | PC24 | Low | UI density and locale helpers |
| `platform/ScrollableColumn.kt` | PC1 | Low | Scrollable list abstractions |
| `platform/Log.kt` | — | Low | Logging utility — no direct product impact |
| `platform/Modifier.kt` | PC24 | Low | Compose modifier extensions |
| `platform/Resources.kt` | PC24 | Low | Resource loading helpers |

### 1.2 Theme

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `ui/theme/ThemeManager.kt` | PC24 | Medium | Theme resolution engine — all color and wallpaper logic |
| `ui/theme/Theme.kt` | PC24 | Medium | Theme composables and `SimpleXTheme` |
| `ui/theme/Color.kt` | PC24 | Low | Color palette definitions |
| `ui/theme/Shape.kt` | PC24 | Low | Shape token definitions |
| `ui/theme/Type.kt` | PC24 | Low | Typography definitions |

### 1.3 Views — Chat List

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/chatlist/ChatListView.kt` | PC1, PC28 | High | Main screen — chat list rendering and search |
| `views/chatlist/ChatListNavLinkView.kt` | PC1, PC2, PC3 | Medium | Navigation from chat list item to chat |
| `views/chatlist/ChatPreviewView.kt` | PC1, PC2, PC3, PC11 | Medium | Chat row preview rendering |
| `views/chatlist/TagListView.kt` | PC28 | Medium | Chat tag filter UI |
| `views/chatlist/UserPicker.kt` | PC19, PC21 | Medium | Multi-profile switcher overlay |
| `views/chatlist/ShareListView.kt` | PC10 | Low | Share target list |
| `views/chatlist/ShareListNavLinkView.kt` | PC10 | Low | Share target navigation |
| `views/chatlist/ChatHelpView.kt` | PC1 | Low | Empty-state help content |
| `views/chatlist/ContactRequestView.kt` | PC12 | Medium | Incoming contact request row |
| `views/chatlist/ContactConnectionView.kt` | PC12 | Low | Pending connection row |
| `views/chatlist/ServersSummaryView.kt` | PC25 | Low | Server status summary |

### 1.4 Views — Chat & Messaging

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/chat/ChatView.kt` | PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC11 | High | Core conversation UI — most messaging features |
| `views/chat/ComposeView.kt` | PC4, PC6, PC9, PC10, PC11 | High | Message composition — send path for all messages |
| `views/chat/SendMsgView.kt` | PC4, PC9 | Medium | Send button and voice record toggle |
| `views/chat/ComposeVoiceView.kt` | PC9 | Medium | Voice message recording UI |
| `views/chat/ComposeFileView.kt` | PC10 | Low | File attachment preview in compose area |
| `views/chat/ComposeImageView.kt` | PC10 | Low | Image attachment preview in compose area |
| `views/chat/ContextItemView.kt` | PC6 | Low | Reply/edit quote preview |
| `views/chat/SelectableChatItemToolbars.kt` | PC7, PC10 | Medium | Multi-select toolbar (delete, forward) |
| `views/chat/ChatInfoView.kt` | PC2, PC13, PC20 | Medium | Contact details and verification |
| `views/chat/ContactPreferences.kt` | PC2, PC8 | Medium | Per-contact feature preferences |
| `views/chat/ChatItemInfoView.kt` | PC2, PC3 | Low | Message delivery detail |
| `views/chat/ChatItemsLoader.kt` | PC2, PC3 | Medium | Pagination and message loading logic |
| `views/chat/ChatItemsMerger.kt` | PC2, PC3 | Medium | Merges incremental message updates |
| `views/chat/VerifyCodeView.kt` | PC13 | Medium | Contact security code verification |
| `views/chat/ScanCodeView.kt` | PC13 | Low | QR code scanning for verification |
| `views/chat/CommandsMenuView.kt` | PC4 | Low | Slash-command menu |
| `views/chat/ComposeContextProfilePickerView.kt` | PC20 | Low | Incognito profile picker in compose |
| `views/chat/ComposeContextPendingMemberActionsView.kt` | PC14, PC30 | Low | Pending member action buttons in compose |
| `views/chat/ComposeContextGroupDirectInvitationActionsView.kt` | PC14 | Low | Direct invitation action buttons in compose |
| `views/chat/ComposeContextContactRequestActionsView.kt` | PC12 | Low | Contact request action buttons in compose |

### 1.5 Views — Chat Items

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/chat/item/ChatItemView.kt` | PC2, PC3, PC5, PC6, PC7, PC8 | High | Root chat item renderer with context menus |
| `views/chat/item/TextItemView.kt` | PC2, PC3, PC4 | Medium | Text message bubble rendering |
| `views/chat/item/FramedItemView.kt` | PC4, PC6, PC10, PC11 | Medium | Framed (quoted/forwarded) message container |
| `views/chat/item/CIImageView.kt` | PC10 | Medium | Image message rendering |
| `views/chat/item/CIVideoView.kt` | PC10 | Medium | Video message rendering |
| `views/chat/item/CIFileView.kt` | PC10 | Medium | File message rendering |
| `views/chat/item/CIVoiceView.kt` | PC9 | Medium | Voice message rendering and playback |
| `views/chat/item/EmojiItemView.kt` | PC5 | Low | Emoji reaction display |
| `views/chat/item/CIMetaView.kt` | PC2, PC3, PC8 | Low | Timestamp, delivery status, timed message indicator |
| `views/chat/item/CICallItemView.kt` | PC17 | Low | Call event item rendering |
| `views/chat/item/CIEventView.kt` | PC3, PC14, PC16 | Low | Group event item rendering |
| `views/chat/item/CIGroupInvitationView.kt` | PC3, PC14 | Low | Group invitation item rendering |
| `views/chat/item/CIMemberCreatedContactView.kt` | PC3, PC12 | Low | Member-created contact event |
| `views/chat/item/CIChatFeatureView.kt` | PC8 | Low | Feature change event rendering |
| `views/chat/item/CIFeaturePreferenceView.kt` | PC8 | Low | Feature preference change rendering |
| `views/chat/item/CIRcvDecryptionError.kt` | PC2, PC3 | Low | Decryption error display |
| `views/chat/item/DeletedItemView.kt` | PC7 | Low | Deleted message placeholder |
| `views/chat/item/MarkedDeletedItemView.kt` | PC7 | Low | Moderated/marked-deleted placeholder |
| `views/chat/item/ImageFullScreenView.kt` | PC10 | Low | Full-screen image viewer |
| `views/chat/item/CIBrokenComposableView.kt` | — | Low | Fallback for render failures |
| `views/chat/item/CIInvalidJSONView.kt` | — | Low | Fallback for malformed items |
| `views/chat/item/IntegrityErrorItemView.kt` | PC2, PC3 | Low | Message integrity error display |

### 1.6 Views — Groups

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/chat/group/GroupChatInfoView.kt` | PC3, PC14, PC15, PC16, PC30 | High | Group management hub |
| `views/chat/group/AddGroupMembersView.kt` | PC14, PC16 | Medium | Member invitation flow |
| `views/chat/group/GroupMemberInfoView.kt` | PC3, PC14, PC16, PC30 | Medium | Member details and role management |
| `views/chat/group/GroupProfileView.kt` | PC3, PC14 | Medium | Group profile editing |
| `views/chat/group/GroupLinkView.kt` | PC15 | Low | Group link creation and sharing |
| `views/chat/group/GroupPreferences.kt` | PC3, PC8, PC14 | Medium | Group feature toggles |
| `views/chat/group/GroupMentions.kt` | PC3, PC4 | Medium | @mention resolution and display |
| `views/chat/group/GroupMembersToolbar.kt` | PC3, PC14 | Low | Member list toolbar |
| `views/chat/group/GroupReportsView.kt` | PC3, PC14 | Low | Group content reports |
| `views/chat/group/MemberAdmission.kt` | PC14, PC16 | Medium | Member admission settings |
| `views/chat/group/MemberSupportView.kt` | PC30 | Medium | Member support chat toggle |
| `views/chat/group/MemberSupportChatView.kt` | PC30 | Medium | Member support chat conversation |
| `views/chat/group/WelcomeMessageView.kt` | PC3, PC14 | Low | Group welcome message editor |

### 1.7 Views — Calls

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/call/CallView.kt` | PC17 | High | Call UI and WebRTC composable |
| `views/call/CallManager.kt` | PC17 | High | Call lifecycle management |
| `views/call/WebRTC.kt` | PC17 | High | WebRTC types and signaling |
| `views/call/IncomingCallAlertView.kt` | PC17, PC18 | Medium | Incoming call overlay |

### 1.8 Views — New Chat & Contacts

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/newchat/NewChatView.kt` | PC12, PC29 | High | New connection creation — onramp for all contacts |
| `views/newchat/NewChatSheet.kt` | PC12 | Medium | Bottom sheet with connection options |
| `views/newchat/ConnectPlan.kt` | PC12, PC15 | Medium | Link parsing and connection plan resolution |
| `views/newchat/AddGroupView.kt` | PC3, PC14 | Medium | New group creation flow |
| `views/newchat/ContactConnectionInfoView.kt` | PC12 | Low | Pending connection details |
| `views/newchat/AddContactLearnMore.kt` | PC12 | Low | Educational content |
| `views/newchat/QRCode.kt` | PC12 | Low | QR code display |
| `views/newchat/QRCodeScanner.kt` | PC12 | Low | QR code camera scanner |
| `views/contacts/ContactListNavView.kt` | PC1, PC12 | Medium | Contact list navigation |
| `views/contacts/ContactPreviewView.kt` | PC12 | Low | Contact row preview |

### 1.9 Views — User Settings

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/usersettings/SettingsView.kt` | PC18, PC22, PC23, PC24, PC25, PC29 | Medium | Settings navigation hub |
| `views/usersettings/Appearance.kt` | PC24 | Low | Theme and appearance customization |
| `views/usersettings/PrivacySettings.kt` | PC20, PC22 | Medium | Privacy and lock settings |
| `views/usersettings/UserProfileView.kt` | PC19 | Medium | Profile display name and image editing |
| `views/usersettings/UserProfilesView.kt` | PC19, PC21 | Medium | Multi-profile management |
| `views/usersettings/HiddenProfileView.kt` | PC21 | Medium | Hidden profile access |
| `views/usersettings/IncognitoView.kt` | PC20 | Low | Incognito mode explanation |
| `views/usersettings/UserAddressView.kt` | PC29 | Medium | User SimpleX address management |
| `views/usersettings/UserAddressLearnMore.kt` | PC29 | Low | Address educational content |
| `views/usersettings/NotificationsSettingsView.kt` | PC18 | Medium | Notification mode configuration |
| `views/usersettings/CallSettings.kt` | PC17 | Low | Call-related settings |
| `views/usersettings/Preferences.kt` | PC2, PC3, PC8 | Medium | Chat feature preferences UI |
| `views/usersettings/SetDeliveryReceiptsView.kt` | PC2 | Low | Delivery receipts toggle |
| `views/usersettings/RTCServers.kt` | PC17, PC25 | Medium | WebRTC ICE server configuration |
| `views/usersettings/DeveloperView.kt` | — | Low | Developer/debug settings |
| `views/usersettings/HelpView.kt` | — | Low | Help and support links |
| `views/usersettings/MarkdownHelpView.kt` | PC4 | Low | Markdown formatting guide |
| `views/usersettings/VersionInfoView.kt` | — | Low | Version display |
| `views/usersettings/networkAndServers/NetworkAndServers.kt` | PC25 | High | Server and network configuration hub |
| `views/usersettings/networkAndServers/AdvancedNetworkSettings.kt` | PC25 | Medium | SOCKS proxy, timeouts, etc. |
| `views/usersettings/networkAndServers/OperatorView.kt` | PC25 | Medium | Server operator management |
| `views/usersettings/networkAndServers/ProtocolServersView.kt` | PC25 | Medium | SMP/XFTP server list |
| `views/usersettings/networkAndServers/ProtocolServerView.kt` | PC25 | Low | Individual server editing |
| `views/usersettings/networkAndServers/NewServerView.kt` | PC25 | Low | Add new server |
| `views/usersettings/networkAndServers/ScanProtocolServer.kt` | PC25 | Low | QR scan for server address |

### 1.10 Views — Database & Migration

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/database/DatabaseView.kt` | PC23, PC26 | High | Database management — export, import, passphrase |
| `views/database/DatabaseEncryptionView.kt` | PC23 | High | Database encryption passphrase change |
| `views/database/DatabaseErrorView.kt` | PC23 | Medium | Database open error recovery |
| `views/migration/MigrateFromDevice.kt` | PC26 | High | Outbound device migration |
| `views/migration/MigrateToDevice.kt` | PC26 | High | Inbound device migration |

### 1.11 Views — Local Auth & Onboarding

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/localauth/LocalAuthView.kt` | PC22 | Medium | App lock authentication flow |
| `views/localauth/SetAppPasscodeView.kt` | PC22 | Medium | Passcode creation and change |
| `views/localauth/PasscodeView.kt` | PC22 | Medium | Passcode entry UI |
| `views/localauth/PasswordEntry.kt` | PC22 | Low | Password input field |
| `views/onboarding/OnboardingView.kt` | PC1 | Medium | Onboarding flow navigation |
| `views/onboarding/SimpleXInfo.kt` | PC1 | Low | Welcome screen |
| `views/onboarding/SetNotificationsMode.kt` | PC18 | Medium | Notification permission and mode setup |
| `views/onboarding/SetupDatabasePassphrase.kt` | PC23 | Medium | Initial database passphrase setup |
| `views/onboarding/ChooseServerOperators.kt` | PC25 | Medium | Initial server operator selection |
| `views/onboarding/WhatsNewView.kt` | — | Low | Release notes display |
| `views/onboarding/HowItWorks.kt` | — | Low | Educational content |
| `views/onboarding/LinkAMobileView.kt` | PC27 | Low | Mobile linking onboarding |

### 1.12 Views — Remote Desktop

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/remote/ConnectDesktopView.kt` | PC27 | Medium | Connect-to-desktop flow (from mobile) |
| `views/remote/ConnectMobileView.kt` | PC27 | Medium | Connect-to-mobile flow (from desktop) |

### 1.13 Views — Helpers

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/helpers/AlertManager.kt` | PC1 through PC30 | Medium | Modal alert system used across all features |
| `views/helpers/ModalView.kt` | PC1 through PC30 | Medium | Modal navigation stack |
| `views/helpers/Utils.kt` | PC1 through PC30 | Low | Shared formatting, clipboard, and utility functions |
| `views/helpers/DatabaseUtils.kt` | PC23 | Medium | Keystore passphrase and database helpers |
| `views/helpers/LinkPreviews.kt` | PC11 | Medium | Link preview fetching and rendering |
| `views/helpers/LocalAuthentication.kt` | PC22 | Medium | Biometric/passcode authentication expect |
| `views/helpers/ChatWallpaper.kt` | PC24 | Low | Chat wallpaper rendering |
| `views/helpers/ChatInfoImage.kt` | PC19 | Low | Profile image composable |
| `views/helpers/ThemeModeEditor.kt` | PC24 | Low | Theme mode toggle |
| `views/helpers/ChooseAttachmentView.kt` | PC10 | Low | Attachment picker |
| `views/helpers/GetImageView.kt` | PC10, PC19 | Low | Image capture and crop |
| `views/helpers/TextEditor.kt` | PC4 | Low | Rich text editor helpers |
| `views/helpers/SearchTextField.kt` | PC1 | Low | Search bar composable |
| `views/helpers/CustomTimePicker.kt` | PC8 | Low | Time picker for timed messages |
| `views/helpers/DragAndDrop.kt` | PC10 | Low | Drag-and-drop file handling |
| `views/helpers/ProcessedErrors.kt` | — | Low | Error aggregation |
| `views/helpers/AnimationUtils.kt` | PC24 | Low | Animation helpers |
| `views/helpers/DefaultDialog.kt` | — | Low | Dialog composable primitives |
| `views/helpers/DefaultDropdownMenu.kt` | — | Low | Dropdown menu composable |
| `views/helpers/Section.kt` | — | Low | Settings section composable |
| `views/helpers/SimpleButton.kt` | — | Low | Button composable |
| `views/helpers/DefaultTopAppBar.kt` | — | Low | App bar composable |
| `views/helpers/DefaultBasicTextField.kt` | PC4 | Low | Text field composable |
| `views/helpers/AppBarTitle.kt` | — | Low | App bar title composable |
| `views/helpers/BlurModifier.kt` | PC22 | Low | Blur modifier for app lock |
| `views/helpers/CollapsingAppBar.kt` | — | Low | Collapsing toolbar composable |
| `views/helpers/CustomIcons.kt` | — | Low | Custom icon definitions |
| `views/helpers/DataClasses.kt` | — | Low | Shared data class utilities |
| `views/helpers/DefaultProgressBar.kt` | — | Low | Progress bar composable |
| `views/helpers/DefaultSwitch.kt` | — | Low | Switch composable |
| `views/helpers/Enums.kt` | — | Low | Enum utility extensions |
| `views/helpers/ExposedDropDownSettingRow.kt` | — | Low | Dropdown setting row composable |
| `views/helpers/GestureDetector.kt` | — | Low | Touch gesture utilities |
| `views/helpers/Modifiers.kt` | — | Low | Compose modifier extensions |
| `views/helpers/SubscriptionStatusIcon.kt` | PC25 | Low | Server connection status icon |

### 1.14 Views — Other

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `views/TerminalView.kt` | — | Low | Developer chat console |
| `views/SplashView.kt` | — | Low | Splash screen |
| `views/WelcomeView.kt` | PC1 | Low | Empty-state welcome |
| `views/Preview.kt` | — | Low | Compose preview utilities |

---

## 2. Android Sources

### 2.1 Android App Module

Path prefix: `android/src/main/java/chat/simplex/app/`

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `SimplexApp.kt` | PC1 through PC30 | High | Application class — initializes core, preferences, and notification channels |
| `MainActivity.kt` | PC1 through PC30 | High | Single-activity host — intent handling, lifecycle, deep links |
| `SimplexService.kt` | PC18 | High | Foreground service — keeps message receiver alive |
| `CallService.kt` | PC17 | Medium | Foreground service for active calls |
| `MessagesFetcherWorker.kt` | PC18 | Medium | WorkManager periodic message fetch |
| `model/NtfManager.android.kt` | PC18 | High | Android notification channels, display, and actions |
| `views/call/CallActivity.kt` | PC17 | Medium | Dedicated activity for full-screen call UI |
| `views/helpers/Util.kt` | — | Low | Android-specific utility extensions |

### 2.2 Android Platform Implementations (androidMain)

Path prefix: `common/src/androidMain/kotlin/chat/simplex/common/`

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `platform/AppCommon.android.kt` | PC1 through PC30 | Medium | Android app initialization actual declarations |
| `platform/SimplexService.android.kt` | PC18 | Medium | Android foreground service actual implementation |
| `platform/Files.android.kt` | PC10, PC23, PC26 | Medium | Android file paths and content-URI resolution |
| `platform/Cryptor.android.kt` | PC23 | Medium | Android Keystore encryption actual implementation |
| `platform/RecAndPlay.android.kt` | PC9 | Medium | Android MediaRecorder/MediaPlayer actual implementation |
| `platform/VideoPlayer.android.kt` | PC10 | Low | Android ExoPlayer actual implementation |
| `platform/Notifications.android.kt` | PC18 | Medium | Android notification channel creation |
| `platform/Images.android.kt` | PC10, PC19 | Low | Android bitmap processing |
| `platform/PlatformTextField.android.kt` | PC4 | Low | Android native text field actual implementation |
| `platform/Share.android.kt` | PC10 | Low | Android share intent actual implementation |
| `platform/Back.android.kt` | PC1 | Low | Android back press handler |
| `platform/UI.android.kt` | PC24 | Low | Android density and locale |
| `platform/ScrollableColumn.android.kt` | PC1 | Low | Android lazy list actual implementation |
| `platform/Log.android.kt` | — | Low | Android Log wrapper |
| `platform/Modifier.android.kt` | — | Low | Android modifier extensions |
| `platform/Resources.android.kt` | — | Low | Android resource loading |
| `helpers/NetworkObserver.kt` | PC25 | Medium | Android ConnectivityManager observer |
| `helpers/Permissions.kt` | PC9, PC10, PC17, PC18 | Medium | Android runtime permission requests |
| `helpers/SoundPlayer.kt` | PC17, PC18 | Low | Android sound playback for calls and notifications |
| `helpers/Extensions.kt` | — | Low | Kotlin extension utilities |
| `helpers/Locale.kt` | — | Low | Locale helpers |
| `views/call/CallView.android.kt` | PC17 | Medium | Android WebView-based WebRTC call |
| `views/call/CallAudioDeviceManager.kt` | PC17 | Medium | Android audio routing (speaker, earpiece, bluetooth) |
| `views/chat/ComposeView.android.kt` | PC4, PC10 | Low | Android compose view extensions |
| `views/chat/SendMsgView.android.kt` | PC4 | Low | Android send button extensions |
| `views/chat/item/ChatItemView.android.kt` | PC2, PC3 | Low | Android chat item extensions |
| `views/chat/item/CIImageView.android.kt` | PC10 | Low | Android image rendering extensions |
| `views/chat/item/CIVideoView.android.kt` | PC10 | Low | Android video rendering extensions |
| `views/chat/item/CIFileView.android.kt` | PC10 | Low | Android file view extensions |
| `views/chat/item/EmojiItemView.android.kt` | PC5 | Low | Android emoji rendering extensions |
| `views/chat/item/ImageFullScreenView.android.kt` | PC10 | Low | Android full-screen image viewer |
| `views/chatlist/ChatListView.android.kt` | PC1 | Low | Android chat list extensions |
| `views/chatlist/ChatListNavLinkView.android.kt` | PC1 | Low | Android chat list navigation extensions |
| `views/chatlist/TagListView.android.kt` | PC28 | Low | Android tag list extensions |
| `views/chatlist/UserPicker.android.kt` | PC19 | Low | Android profile picker extensions |
| `views/database/DatabaseView.android.kt` | PC23, PC26 | Low | Android database view extensions |
| `views/database/DatabaseEncryptionView.android.kt` | PC23 | Low | Android encryption view extensions |
| `views/helpers/LocalAuthentication.android.kt` | PC22 | Medium | Android BiometricPrompt actual implementation |
| `views/helpers/ChooseAttachmentView.android.kt` | PC10 | Low | Android file/camera chooser |
| `views/helpers/GetImageView.android.kt` | PC10, PC19 | Low | Android image capture |
| `views/helpers/CustomTimePicker.android.kt` | PC8 | Low | Android time picker |
| `views/helpers/Utils.android.kt` | — | Low | Android utility extensions |
| `views/helpers/DefaultDialog.android.kt` | — | Low | Android dialog extensions |
| `views/helpers/WorkaroundFocusSearchLayout.kt` | — | Low | Android focus workaround |
| `views/newchat/QRCode.android.kt` | PC12 | Low | Android QR code rendering |
| `views/newchat/QRCodeScanner.android.kt` | PC12 | Low | Android camera QR scanner |
| `views/onboarding/SimpleXInfo.android.kt` | PC1 | Low | Android onboarding extensions |
| `views/onboarding/SetNotificationsMode.android.kt` | PC18 | Low | Android notification mode extensions |
| `views/usersettings/Appearance.android.kt` | PC24 | Low | Android appearance extensions |
| `views/usersettings/PrivacySettings.android.kt` | PC20, PC22 | Low | Android privacy settings extensions |
| `views/usersettings/SettingsView.android.kt` | — | Low | Android settings extensions |
| `views/usersettings/networkAndServers/OperatorView.android.kt` | PC25 | Low | Android operator view extensions |
| `views/usersettings/networkAndServers/ScanProtocolServer.android.kt` | PC25 | Low | Android server QR scan |
| `ui/theme/Theme.android.kt` | PC24 | Low | Android dynamic color / system theme |
| `ui/theme/Type.android.kt` | PC24 | Low | Android typography |

---

## 3. Desktop Sources

### 3.1 Desktop App Module

Path prefix: `desktop/src/jvmMain/kotlin/chat/simplex/desktop/`

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `Main.kt` | PC1 through PC30 | High | JVM entry point — Haskell init, migrations, app launch |

### 3.2 Desktop Platform Implementations (desktopMain)

Path prefix: `common/src/desktopMain/kotlin/chat/simplex/common/`

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `DesktopApp.kt` | PC1, PC2, PC3 | High | Desktop Compose window — window lifecycle, crash recovery |
| `StoreWindowState.kt` | — | Low | Window position/size persistence |
| `model/NtfManager.desktop.kt` | PC18 | Medium | Desktop system tray notification display |
| `platform/AppCommon.desktop.kt` | PC1 through PC30 | Medium | Desktop app initialization actual declarations |
| `platform/SimplexService.desktop.kt` | PC18 | Low | Desktop background receiver (no foreground service) |
| `platform/Files.desktop.kt` | PC10, PC23, PC26 | Medium | Desktop file path resolution |
| `platform/Cryptor.desktop.kt` | PC23 | Medium | Desktop keystore encryption actual implementation |
| `platform/RecAndPlay.desktop.kt` | PC9 | Medium | Desktop audio recording/playback actual implementation |
| `platform/VideoPlayer.desktop.kt` | PC10 | Low | Desktop VLC-based video player |
| `platform/Videos.desktop.kt` | PC10 | Low | Desktop video utilities |
| `platform/Notifications.desktop.kt` | PC18 | Low | Desktop notification setup |
| `platform/Images.desktop.kt` | PC10 | Low | Desktop image processing |
| `platform/PlatformTextField.desktop.kt` | PC4 | Low | Desktop text field actual implementation |
| `platform/Share.desktop.kt` | PC10 | Low | Desktop clipboard/share |
| `platform/Back.desktop.kt` | PC1 | Low | Desktop back navigation |
| `platform/UI.desktop.kt` | PC24 | Low | Desktop density and locale |
| `platform/ScrollableColumn.desktop.kt` | PC1 | Low | Desktop lazy list |
| `platform/Platform.desktop.kt` | — | Low | Platform detection |
| `platform/Log.desktop.kt` | — | Low | Desktop log output |
| `platform/Modifier.desktop.kt` | — | Low | Desktop modifier extensions |
| `platform/Resources.desktop.kt` | — | Low | Desktop resource loading |
| `views/call/CallView.desktop.kt` | PC17 | Medium | Desktop WebView-based WebRTC call |
| `views/chat/ComposeView.desktop.kt` | PC4, PC10 | Low | Desktop compose view (drag-and-drop, paste) |
| `views/chat/SendMsgView.desktop.kt` | PC4 | Low | Desktop send shortcut (Enter key handling) |
| `views/chat/item/ChatItemView.desktop.kt` | PC2, PC3 | Low | Desktop chat item extensions |
| `views/chat/item/CIImageView.desktop.kt` | PC10 | Low | Desktop image rendering |
| `views/chat/item/CIVideoView.desktop.kt` | PC10 | Low | Desktop video rendering |
| `views/chat/item/CIFileView.desktop.kt` | PC10 | Low | Desktop file open/save |
| `views/chat/item/EmojiItemView.desktop.kt` | PC5 | Low | Desktop emoji rendering |
| `views/chat/item/ImageFullScreenView.desktop.kt` | PC10 | Low | Desktop full-screen image |
| `views/chatlist/ChatListView.desktop.kt` | PC1 | Low | Desktop chat list extensions |
| `views/chatlist/ChatListNavLinkView.desktop.kt` | PC1 | Low | Desktop chat list navigation |
| `views/chatlist/TagListView.desktop.kt` | PC28 | Low | Desktop tag list extensions |
| `views/chatlist/UserPicker.desktop.kt` | PC19 | Low | Desktop profile picker |
| `views/database/DatabaseView.desktop.kt` | PC23, PC26 | Low | Desktop database view extensions |
| `views/database/DatabaseEncryptionView.desktop.kt` | PC23 | Low | Desktop encryption view extensions |
| `views/helpers/AppUpdater.kt` | — | Low | Desktop auto-update checker and installer |
| `views/helpers/OkHttpProgressListener.kt` | — | Low | Download progress tracking for updates |
| `views/helpers/LocalAuthentication.desktop.kt` | PC22 | Low | Desktop passcode-only auth (no biometrics) |
| `views/helpers/ChooseAttachmentView.desktop.kt` | PC10 | Low | Desktop file chooser dialog |
| `views/helpers/GetImageView.desktop.kt` | PC10, PC19 | Low | Desktop image file picker |
| `views/helpers/CustomTimePicker.desktop.kt` | PC8 | Low | Desktop time picker |
| `views/helpers/Utils.desktop.kt` | — | Low | Desktop utility extensions |
| `views/helpers/DefaultDialog.desktop.kt` | — | Low | Desktop dialog extensions |
| `views/newchat/QRCode.desktop.kt` | PC12 | Low | Desktop QR code rendering |
| `views/newchat/QRCodeScanner.desktop.kt` | PC12 | Low | Desktop QR code scanner (screen/clipboard) |
| `views/onboarding/SimpleXInfo.desktop.kt` | PC1 | Low | Desktop onboarding extensions |
| `views/onboarding/SetNotificationsMode.desktop.kt` | PC18 | Low | Desktop notification mode extensions |
| `views/usersettings/Appearance.desktop.kt` | PC24 | Low | Desktop appearance extensions |
| `views/usersettings/PrivacySettings.desktop.kt` | PC20, PC22 | Low | Desktop privacy settings extensions |
| `views/usersettings/SettingsView.desktop.kt` | — | Low | Desktop settings extensions |
| `views/usersettings/networkAndServers/OperatorView.desktop.kt` | PC25 | Low | Desktop operator view extensions |
| `views/usersettings/networkAndServers/ScanProtocolServer.desktop.kt` | PC25 | Low | Desktop server address scan |
| `ui/theme/Theme.desktop.kt` | PC24 | Low | Desktop system theme detection |
| `ui/theme/Type.desktop.kt` | PC24 | Low | Desktop typography |
| `other/videoplayer/SkiaBitmapVideoSurface.kt` | PC10 | Low | Desktop Skia video surface for VLC |

---

## 4. Haskell Core Impact

The Haskell core is compiled as a shared native library (`libsimplex.so` / `libsimplex.dylib`) and linked via JNI through `Core.kt`. Changes here affect both Android and Desktop identically.

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| `src/Simplex/Chat.hs` | PC1 through PC30 | High | Main chat module — top-level orchestration |
| `src/Simplex/Chat/Controller.hs` | PC1 through PC30 | High | Command processor — all API commands dispatched here |
| `src/Simplex/Chat/Types.hs` | PC1 through PC30 | High | Core data types shared across all features |
| `src/Simplex/Chat/Core.hs` | PC1 through PC30 | High | Chat engine lifecycle (start, stop, subscribe) |
| `src/Simplex/Chat/Library/Commands.hs` | PC1 through PC30 | High | API command handler implementations |
| `src/Simplex/Chat/Library/Internal.hs` | PC1 through PC30 | High | Internal helpers for command processing |
| `src/Simplex/Chat/Library/Subscriber.hs` | PC1 through PC30 | High | Event subscriber — incoming message routing |
| `src/Simplex/Chat/Protocol.hs` | PC2, PC3, PC4, PC5, PC6, PC7 | High | Chat-level message protocol (x-events) |
| `src/Simplex/Chat/Messages.hs` | PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9 | High | Message types and content |
| `src/Simplex/Chat/Messages/CIContent.hs` | PC4, PC5, PC6, PC7, PC8, PC9, PC11 | Medium | Chat item content variants |
| `src/Simplex/Chat/Messages/CIContent/Events.hs` | PC3, PC14, PC16 | Medium | Group event content types |
| `src/Simplex/Chat/Messages/Batch.hs` | PC2, PC3, PC4 | Medium | Message batching for efficient delivery |
| `src/Simplex/Chat/Call.hs` | PC17 | Medium | Call signaling types |
| `src/Simplex/Chat/Files.hs` | PC10 | Medium | File transfer orchestration |
| `src/Simplex/Chat/Delivery.hs` | PC2, PC3 | Medium | Message delivery engine |
| `src/Simplex/Chat/Markdown.hs` | PC4 | Low | Markdown parsing for message formatting |
| `src/Simplex/Chat/Store.hs` | PC1 through PC30 | High | Database store interface |
| `src/Simplex/Chat/Store/Shared.hs` | PC1 through PC30 | Medium | Shared store utilities |
| `src/Simplex/Chat/Store/Messages.hs` | PC4, PC5, PC6, PC7, PC8 | High | Message persistence |
| `src/Simplex/Chat/Store/Groups.hs` | PC3, PC14, PC15, PC16, PC30 | High | Group persistence |
| `src/Simplex/Chat/Store/Direct.hs` | PC2, PC12, PC13 | High | Contact persistence |
| `src/Simplex/Chat/Store/Files.hs` | PC10 | Medium | File transfer persistence |
| `src/Simplex/Chat/Store/Profiles.hs` | PC19, PC21 | Medium | User profile persistence |
| `src/Simplex/Chat/Store/Connections.hs` | PC2, PC12 | High | Connection persistence and entity resolution |
| `src/Simplex/Chat/Store/ContactRequest.hs` | PC12 | Medium | Contact request persistence |
| `src/Simplex/Chat/Store/NoteFolders.hs` | PC1 | Low | Note folder (self-chat) persistence |
| `src/Simplex/Chat/Store/Delivery.hs` | PC2, PC3 | Medium | Delivery task persistence |
| `src/Simplex/Chat/Store/AppSettings.hs` | PC25 | Low | App settings persistence |
| `src/Simplex/Chat/Store/Remote.hs` | PC27 | Low | Remote desktop session persistence |
| `src/Simplex/Chat/Archive.hs` | PC26 | Medium | Database export/import for migration |
| `src/Simplex/Chat/Options.hs` | PC23, PC25 | Low | Startup options (DB path, key, etc.) |
| `src/Simplex/Chat/Remote.hs` | PC27 | Medium | Remote desktop protocol handler |
| `src/Simplex/Chat/Remote/Types.hs` | PC27 | Low | Remote desktop data types |
| `src/Simplex/Chat/Remote/Protocol.hs` | PC27 | Medium | Remote desktop wire protocol |
| `src/Simplex/Chat/Remote/Transport.hs` | PC27 | Low | Remote desktop transport layer |
| `src/Simplex/Chat/Remote/RevHTTP.hs` | PC27 | Low | Reverse HTTP for remote desktop |
| `src/Simplex/Chat/Remote/AppVersion.hs` | PC27 | Low | Remote version negotiation |
| `src/Simplex/Chat/ProfileGenerator.hs` | PC20 | Low | Random profile generation for incognito |
| `src/Simplex/Chat/Types/UITheme.hs` | PC24 | Low | Theme data types for UI customization |
| `src/Simplex/Chat/Types/Preferences.hs` | PC2, PC3, PC8 | Medium | Chat feature preferences (timed messages, etc.) |
| `src/Simplex/Chat/Types/Shared.hs` | PC3, PC16 | Medium | Shared types including GroupMemberRole |
| `src/Simplex/Chat/Types/MemberRelations.hs` | PC3, PC16, PC30 | Medium | Member relationship state machine |
| `src/Simplex/Chat/Operators.hs` | PC25 | Medium | Server operator management |
| `src/Simplex/Chat/Operators/Presets.hs` | PC25 | Low | Preset server operators |
| `src/Simplex/Chat/Operators/Conditions.hs` | PC25 | Low | Operator usage conditions |
| `src/Simplex/Chat/AppSettings.hs` | PC25 | Low | App settings sync types |
| `src/Simplex/Chat/Mobile.hs` | PC1 through PC30 | High | C FFI exports — JNI bridge target |
| `src/Simplex/Chat/Mobile/File.hs` | PC10 | Medium | Mobile file read/write FFI |
| `src/Simplex/Chat/Mobile/Shared.hs` | PC1 through PC30 | Medium | Shared FFI helpers |
| `src/Simplex/Chat/Mobile/WebRTC.hs` | PC17 | Low | WebRTC FFI helpers |
| `src/Simplex/Chat/View.hs` | PC1 through PC30 | Low | Terminal view rendering (not used by mobile/desktop UI) |
| `src/Simplex/Chat/Stats.hs` | PC25 | Low | Server statistics tracking |
| `src/Simplex/Chat/Util.hs` | — | Low | General Haskell utilities |
| `src/Simplex/Chat/Styled.hs` | — | Low | Terminal styled text (not used by mobile/desktop UI) |
| `src/Simplex/Chat/Help.hs` | — | Low | Terminal help text |
| `src/Simplex/Chat/Bot.hs` | — | Low | Chat bot framework |
| `src/Simplex/Chat/Bot/KnownContacts.hs` | — | Low | Bot known contacts |
