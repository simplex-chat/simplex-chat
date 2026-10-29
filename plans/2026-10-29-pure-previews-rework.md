# Pure previews: rework

Date: 2026-10-29.

Pull request: https://github.com/simplex-chat/simplex-chat/pull/7938. Branch: ep/pure-previews at c91d90cf3.

The branch has 109 preview-only parameters, each marked `// do not pass`, in 65 files. All 109 take outcome (a): each parameter goes, and the tests or one of two new environment values, S1 and S2, take its place.

Paths:

```
iOS      apps/ios/Shared/
Kotlin   apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/
```

## 1. Outcomes

```
(a) CI    the parameter goes; the preview runs master's code; the scene is rendered on master and compared on every ios-sim or desktop-linux run
(a) CL    the parameter goes; the preview draws previewDate (2025-01-15 10:15 UTC); from 2026-01-01 the clock-dependent text is "1/15/25" and "Wed, 15 Jan 2025", as in master's references
(a) M     the parameter is master's; the comment and the `.task` guard added by the branch go
(a) X     the parameter goes; the scene is excluded as on master
(a) XV    the parameter goes; VersionView_Previews joins EXCLUDED
(a) S1    the parameter goes; the snapshot test sets seam S1
(a) S2    the parameter goes; the snapshot test sets seam S2
(a) SIM   the parameter goes; the simulator build of CodeScanner draws a fixed text and requests no camera access
```

Calls kept in previews, as on master:

```
db        core API reading or writing the CI database
srv       core API that contacts a server when its object exists
agent     agent API
fn        pure core function: markdown parse, name validation, JSON length, server address parse
store     StoreKit
keychain  keychain read
files     file read or delete
defaults  UserDefaults write
paste     pasteboard read
global    write to a shared singleton
```

## 2. Seams

### S1: QR picture

iOS, `Views/NewChat/QRCode.swift`:

```swift
extension EnvironmentValues {
    struct QRCodeImage: EnvironmentKey {
        static let defaultValue: UIImage? = nil
    }

    var qrCodeImage: UIImage? {
        get { self[QRCodeImage.self] }
        set { self[QRCodeImage.self] = newValue }
    }
}
```

`QRCode`:

```
added     @Environment(\.qrCodeImage) private var qrCodeImage
changed   if let image = qrCodeImage ?? image {
changed   .task { if qrCodeImage == nil { image = await generateImage(uri, tintColor: tintColor, errorLevel: small ? "M" : "L") } }
```

Kotlin, `views/newchat/QRCode.kt`:

```
added     val LocalQRCodeImage = staticCompositionLocalOf<ImageBitmap?> { null }
added     val qrCodeImage = LocalQRCodeImage.current                                  (QRCode, before remember)
changed   (qrCodeImage ?: qrCodeBitmap(connReq, 1024, errorLevel)).replaceColor(...)   (QRCode, in remember)
```

Full lines by `git diff -w --word-diff`, blank lines aside: iOS 10 added and 2 changed; Kotlin 2 added and 1 changed.

### S2: camera status

iOS, `Views/NewChat/NewChatView.swift`, beside `ScannerInView`:

```swift
extension EnvironmentValues {
    struct CameraAuthorization: EnvironmentKey {
        static let defaultValue: AVAuthorizationStatus? = nil
    }

    var cameraAuthorization: AVAuthorizationStatus? {
        get { self[CameraAuthorization.self] }
        set { self[CameraAuthorization.self] = newValue }
    }
}
```

`ScannerInView`:

```
added     @Environment(\.cameraAuthorization) private var cameraAuthorization
changed   let status = cameraAuthorization ?? AVCaptureDevice.authorizationStatus(for: .video)   (.task)
```

Full lines by `git diff -w --word-diff`, blank lines aside: 10 added and 1 changed.

### Snapshot tests

```
iOS       renderScene root: .environment(\.qrCodeImage, UIImage(named: "preview-qr-code", in: Bundle(for: SnapshotTests.self), compatibleWith: nil))
iOS       renderScene root: .environment(\.cameraAuthorization, .denied)
iOS       preview-qr-code.png moves to apps/ios/SimpleX Snapshots/, a resource of the SimpleX Snapshots target
Kotlin    renderSnapshot root: LocalQRCodeImage provides previewQRCodeImage()
Kotlin    previewQRCodeImage() moves to SnapshotTest.kt; it reads preview-qr-code-light.png or preview-qr-code-dark.png by the theme of the pass
```

## 3. Parameters

| # | File | Parameter | Outcome | Measure | Calls kept |
|---|---|---|---|---|---|
| 1 | Model/ChatModel.swift | `ChatModel.init(migrationState:)` | (a) CI | previews call `ChatModel()`, as master's 12 preview sites | files |
| 2 | Views/Badges/BadgesCheckOrderView.swift | `priceList` | (a) M | master's preview passes it | store |
| 3 | Views/Badges/BadgesCheckOrderView.swift | `previewCanBuy` | (a) CI | `store.canBuy` after the launch sweep `presentUnfinished` of the CI app | — |
| 4 | Views/Badges/BadgesCheckOrderView.swift | `previewCanMakePayments` | (a) CI | `AppStore.canMakePayments` of the simulator | store |
| 5 | Views/Badges/BadgesChooseBadgeView.swift | `previewMonthlyPrices` | (a) X | the price text is "loading" or "unavailable" by when StoreKit answers | — |
| 6 | Views/Badges/BadgesChooseBadgeView.swift | `previewBrowserAllowed` | (a) X | as row 5 | — |
| 7 | Views/Badges/BadgesHowLongView.swift | `priceList` | (a) M | master's preview passes it | store |
| 8 | Views/Badges/BadgesRedeemCodeView.swift | `previewCameraStatus` | (a) S2 | "Enable camera access" placeholder | — |
| 9 | Views/Call/IncomingCallView.swift | `previewInvitation` | (a) CI | master's preview sets `CallController.shared.activeCallInvitation` | global |
| 10 | Views/Chat/ChatInfoView.swift | `ChatInfoView.previewContactInfo` | (a) CI | `apiContactInfo`, `apiGetContactCode` for the sample contact | db |
| 11 | Views/Chat/ChatInfoView.swift | `ChatInfoView.previewChatItemTTL` | (a) CI | `ChatModel.shared.chatItemTTL` of the CI database | — |
| 12 | Views/Chat/ChatInfoView.swift | `ChatTTLOption.previewChatItemTTL` | (a) CI | as row 11 | — |
| 13 | Views/Chat/ChatItemForwardingView.swift | `previewChats` | (a) CI | `ChatModel.shared.chats` of the CI profile | — |
| 14 | Views/Chat/ChatItemForwardingView.swift | `previewHasSimplexLink` | (a) CI | `hasSimplexLink("hello")` | fn |
| 15 | Views/Chat/ChatView.swift | `previewAvailableContent` | (a) CI | `initChatView` for the sample contact | db |
| 16 | Views/Chat/Group/AddGroupMembersView.swift | `AddGroupMembersView.previewContacts` | (a) CI | `filterMembersToAdd`: contacts of `ChatModel.shared.chats` | — |
| 17 | Views/Chat/Group/AddGroupMembersView.swift | `AddGroupMembersViewCommon.previewContacts` | (a) CI | as row 16 | — |
| 18 | Views/Chat/Group/ChannelRelaysView.swift | `previewMembers` | (a) CI | `loadGroupMembers` for the sample group | db |
| 19 | Views/Chat/Group/GroupChatInfoView.swift | `previewGroupLink` | (a) CI | `apiGetGroupLink` for the sample group | db |
| 20 | Views/Chat/Group/GroupChatInfoView.swift | `previewChatItemTTL` | (a) CI | as row 11 | — |
| 21 | Views/Chat/Group/GroupChatInfoView.swift | `sendReceiptsUserDefault` | (a) CI | `onAppear` reads `User.sampleData`, which the test sets as `ChatModel.shared.currentUser` | — |
| 22 | Views/Chat/Group/GroupLinkView.swift | `previewQRImage` | (a) S1 | fixed QR picture | — |
| 23 | Views/Chat/Group/GroupLinkView.swift | `shouldCreate` | (a) CI | `apiCreateGroupLink` for group 1 fails on the CI database | srv |
| 24 | Views/Chat/Group/GroupMemberInfoView.swift | `previewMemberInfo` | (a) CI | `apiGroupMemberInfo`, `apiGetGroupMemberCode` for the sample member | db |
| 25 | Views/Chat/Group/GroupProfileView.swift | `previewShortDescrLength` | (a) CI | `chatJsonLength` of the sample group's short description; GroupWelcomeView_Previews as well | fn |
| 26 | Views/Chat/Group/MemberSupportView.swift | `previewMembers` | (a) CI | `loadGroupMembers` for the sample group | db |
| 27 | Views/Chat/Group/SecondaryChatView.swift | `previewAvailableContent` | (a) CI | `initChatView` of ChatView for the sample group scope | db |
| 28 | Views/Chat/ScanCodeView.swift | `previewCameraStatus` | (a) SIM | `CodeScannerView` | — |
| 29 | Views/Chat/VerifyCodeView.swift | `previewQRImage` | (a) S1 | fixed QR picture | — |
| 30 | Views/ChatList/ChatListView.swift | `badgeModel` | (a) CI | `BadgeModel.shared` of the CI profile | — |
| 31 | Views/ChatList/ChatListView.swift | `previewSubsTotal` | (a) CI | `getAgentSubsTotal` of the CI profile | agent |
| 32 | Views/ChatList/ChatListView.swift | `previewInUS` | (a) CI | `isInUS` of the simulator's storefront | store |
| 33 | Views/ChatList/ChatListView.swift | `previewNoShownBadge` | (a) CI | as row 30 | — |
| 34 | Views/ChatList/ChatListView.swift | `chatTagsModel` | (a) CI | `ChatTagsModel.shared` of the CI profile | — |
| 35 | Views/ChatList/ChatListView.swift | `SubsStatusIndicator.previewSubsTotal` | (a) CI | as row 31 | agent |
| 36 | Views/ChatList/ServersSummaryView.swift | `previewServersSummary` | (a) X | `onAppear` loads the agent's statistics of the run and starts a timer | — |
| 37 | Views/ChatList/ServersSummaryView.swift | `previewServerAddresses` | (a) X | as row 36 | — |
| 38 | Views/ChatList/UserPicker.swift | `previewUsers` | (a) CI | `listUsersAsync` of the CI database | db |
| 39 | Views/Database/DatabaseEncryptionView.swift | `initialRandomDBPassphrase` | (a) CI | `initialRandomDBPassphraseGroupDefault` of the CI app | — |
| 40 | Views/Database/DatabaseEncryptionView.swift | `storedKey` | (a) CI | `kcDatabasePassword` of the CI app | keychain |
| 41 | Views/Database/DatabaseEncryptionView.swift | `previewDatabasePassword` | (a) CI | as row 40 | keychain |
| 42 | Views/Database/DatabaseErrorView.swift | `storedDBKey` | (a) CI | as row 40 | keychain |
| 43 | Views/Database/DatabaseErrorView.swift | `previewShowRestoreDbButton` | (a) CI | `shouldShowRestoreDbButton`: `encryptionStartedDefault` of the CI app is false | — |
| 44 | Views/Database/DatabaseView.swift | `previewUserName` | (a) CI | `User.sampleData`, which the test sets | — |
| 45 | Views/Database/DatabaseView.swift | `previewAppFilesCountAndSize` | (a) CI | the CI app's files folder | files |
| 46 | Views/LocalAuth/SetAppPasscodeView.swift | `previewProhibitedPasscode` | (a) CI | `kcSelfDestructPassword` of the CI app | keychain |
| 47 | Views/Migration/MigrateFromDevice.swift | `previewMigrationState` | (a) X | `onAppear` stops the chat | — |
| 48 | Views/Migration/MigrateToDevice.swift | `pasteboardHasStrings` | (a) CI | the CI simulator's pasteboard, empty after the smoke test | paste |
| 49 | Views/Migration/MigrateToDevice.swift | `previewCameraStatus` | (a) S2 | "Enable camera access" placeholder | — |
| 50 | Views/NewChat/AddChannelView.swift | `previewValidName` | (a) CI | `mkValidName("")` | fn |
| 51 | Views/NewChat/AddChannelView.swift | `previewHasRelays` | (a) CI | `checkHasRelays` of the CI profile | db |
| 52 | Views/NewChat/AddGroupView.swift | `previewValidName` | (a) CI | `mkValidName("")` | fn |
| 53 | Views/NewChat/NewChatView.swift | `NewChatView.previewQRImage` | (a) X | `onAppear` creates a one-time link | — |
| 54 | Views/NewChat/NewChatView.swift | `connLinkInvitation` | (a) X | as row 53 | — |
| 55 | Views/NewChat/NewChatView.swift | `InviteView.previewQRImage` | (a) X | as row 53 | — |
| 56 | Views/NewChat/NewChatView.swift | `ScannerInView.previewCameraStatus` | (a) S2 | "Enable camera access" placeholder | — |
| 57 | Views/NewChat/QRCode.swift | `MutableQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 58 | Views/NewChat/QRCode.swift | `SimpleXCreatedLinkQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 59 | Views/NewChat/QRCode.swift | `SimpleXLinkQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 60 | Views/NewChat/QRCode.swift | `QRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 61 | Views/Onboarding/CreateProfile.swift | `previewValidName` | (a) CI | `mkValidName("")` | fn |
| 62 | Views/Onboarding/CreateSimpleXAddress.swift | `previewQRImage` | (a) S1 | master's preview draws the address QR code only when `ChatModel.shared.userAddress` is set; it is nil for the CI profile | — |
| 63 | Views/Onboarding/OnboardingView.swift | `previewVersionSaved` | (a) CI | `setLastVersionDefault` | defaults |
| 64 | Views/Onboarding/SimpleXInfo.swift | `previewVersionSaved` | (a) CI | as row 63 | defaults |
| 65 | Views/Onboarding/WhatsNewView.swift | `previewInUS` | (a) CI | as row 32 | store |
| 66 | Views/RemoteAccess/ConnectDesktopView.swift | `previewRemoteCtrls` | (a) CI | `setDeviceName`, `listRemoteCtrls` of the CI database | db |
| 67 | Views/RemoteAccess/ConnectDesktopView.swift | `previewCameraStatus` | (a) S2 | "Enable camera access" placeholder | — |
| 68 | Views/TerminalView.swift | `authorized` | (a) X | master's parameter; `onAppear` loads `TerminalItems.shared`, the commands of the test run | — |
| 69 | Views/TerminalView.swift | `showing` | (a) X | as row 68 | — |
| 70 | Views/UserSettings/CallSettings.swift | `previewInChina` | (a) CI | `CallController.isInChina` of the simulator's storefront | store |
| 71 | Views/UserSettings/NetworkAndServers/NetworkAndServers.swift | `justOpened` | (a) CI | `getUserServers` of the CI database | db |
| 72 | Views/UserSettings/NetworkAndServers/NewServerView.swift | `previewServerAddress` | (a) CI | `parseServerAddress("")` | fn |
| 73 | Views/UserSettings/NetworkAndServers/ProtocolServerView.swift | `previewServerAddress` | (a) CI | `parseServerAddress` of the sample server | fn |
| 74 | Views/UserSettings/NetworkAndServers/ProtocolServerView.swift | `previewQRImage` | (a) S1 | fixed QR picture | — |
| 75 | Views/UserSettings/NetworkAndServers/ProtocolServersView.swift | `YourServersView.previewServerAddresses` | (a) CI | `parseServerAddress` of the sample servers | fn |
| 76 | Views/UserSettings/NetworkAndServers/ProtocolServersView.swift | `ProtocolServerViewLink.previewServerAddress` | (a) CI | as row 75 | fn |
| 77 | Views/UserSettings/NetworkAndServers/ScanProtocolServer.swift | `previewCameraStatus` | (a) SIM | `CodeScannerView` | — |
| 78 | Views/UserSettings/NotificationsView.swift | `previewNtfToken` | (a) CI | `apiGetNtfToken` of the CI database | db |
| 79 | Views/UserSettings/PrivacySettings.swift | `previewUserName` | (a) CI | as row 44 | — |
| 80 | Views/UserSettings/SettingsView.swift | `previewInUS` | (a) CI | as row 32 | store |
| 81 | Views/UserSettings/UserAddressView.swift | `previewQRImage` | (a) S1 | fixed QR picture | — |
| 82 | Views/UserSettings/VersionView.swift | `previewVersionInfo` | (a) XV | the app build number and the core version change with each release | — |
| 83 | model/ChatModel.kt | `getTimestampDateText.previewNow` | (a) CL | DateSeparator text | — |
| 84 | model/ChatModel.kt | `getTimestampText.previewNow` | (a) CL | chat list time | — |
| 85 | views/chat/ChatView.kt | `ChatLayout.previewNow` | (a) CL | as row 83 | — |
| 86 | views/chat/ChatView.kt | `ChatItemsList.previewNow` | (a) CL | as row 83 | — |
| 87 | views/chat/ChatView.kt | `DateSeparator.previewNow` | (a) CL | as row 83 | — |
| 88 | views/chat/group/GroupMemberInfoView.kt | `previewContactConnectionStats` | (a) CI | `apiContactInfo` in `withBGApi` throws "Controller is not initialized" in the test JVM | db |
| 89 | views/chat/group/GroupProfileView.kt | `previewShortDescrFits` | (a) X | `previewsCallingCore`: `chatJsonLength` from composition; the test JVM does not load the core library | — |
| 90 | views/chatlist/ChatPreviewView.kt | `previewNow` | (a) CL | as row 84 | — |
| 91 | views/chatlist/ContactRequestView.kt | `previewNow` | (a) CL | as row 84 | — |
| 92 | views/database/DatabaseErrorView.kt | `previewStoredDBKey` | (a) CI | `ksDatabasePassword.get()` returns null with the CI test's settings | — |
| 93 | views/database/DatabaseErrorView.kt | `previewShowRestoreDbButton` | (a) CI | `shouldShowRestoreDbButton` returns false: `encryptionStartedAt` is null in the CI test's settings | — |
| 94 | views/database/DatabaseErrorView.kt | `previewUseKeychain` | (a) CI | `storeDBPassphrase` default, true | — |
| 95 | views/newchat/AddChannelView.kt | `AddChannelView.previewNameValid` | (a) X | `previewsCallingCore`: `chatValidName` from composition | — |
| 96 | views/newchat/AddChannelView.kt | `AddChannelView.previewHasRelays` | (a) X | as row 95 | — |
| 97 | views/newchat/AddChannelView.kt | `ProfileStepView.previewNameValid` | (a) X | as row 95 | — |
| 98 | views/newchat/AddChannelView.kt | `ProfileStepView.previewHasRelays` | (a) X | as row 95 | — |
| 99 | views/newchat/AddGroupView.kt | `previewNameValid` | (a) X | as row 95 | — |
| 100 | views/newchat/ContactConnectionInfoView.kt | `previewQRImage` | (a) S1 | fixed QR picture | — |
| 101 | views/newchat/NewChatSheet.kt | `NewChatSheet.previewConnectTarget` | (a) CI | `chatParseMarkdown` in a `LaunchedEffect` throws in the test JVM, which does not load the core library; the scene renders as in master's reference | fn |
| 102 | views/newchat/NewChatSheet.kt | `NewChatSheetLayout.previewConnectTarget` | (a) CI | as row 101 | fn |
| 103 | views/newchat/NewChatSheet.kt | `ContactsSearchBar.previewConnectTarget` | (a) CI | as row 101 | fn |
| 104 | views/newchat/QRCode.kt | `SimpleXCreatedLinkQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 105 | views/newchat/QRCode.kt | `SimpleXLinkQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 106 | views/newchat/QRCode.kt | `QRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 107 | views/onboarding/SimpleXInfo.kt | `previewMigrationState` | (a) CI | `chatModel.migrationState` is null with the CI test's settings | files |
| 108 | views/usersettings/UserAddressView.kt | `previewQRImage` | (a) S1 | fixed QR picture | — |
| 109 | views/usersettings/UserProfileView.kt | `previewShortDescrFits` | (a) X | as row 89; EditOff and EditOn | — |

## 4. Counts

```
(a)      109
  CI      62
  CL       7
  M        2
  X       17
  XV       1
  S1      14
  S2       4
  SIM      2
(b)        0
(c)        0
```

Rows keeping calls to the CI database, a server, the agent or StoreKit, by scene:

```
BadgesCheckOrderView      2       store
BadgesHowLongView         7       store
ChatInfoView              10      db
ChatView                  15      db
ChannelRelaysView         18      db
GroupChatInfoView         19      db
GroupLinkView             23      srv
GroupMemberInfoView       24      db
MemberSupportView         26      db
SecondaryChatView         27      db
ChatListView              31 35   agent
UserPicker                38      db
AddChannelView            51      db
ConnectDesktopView        66      db
NetworkServersView        71      db
NotificationsView         78      db
GroupMemberInfoView.kt    88      db, throws
```

## 5. Other branch changes

iOS:

```
ChatModel.init                                   revert
Preview Content asset catalog                    revert; the PNG moves to the test target
project.pbxproj, app target                      revert
project.pbxproj, test target                     preview-qr-code.png in the SimpleX Snapshots resources
SimpleXChat/APITypes.swift, ConnectionStats.sampleData   revert
MigrateFromDevice.swift, MigrationFromState visibility    revert
NewServerView.swift, serverProtocolAndOperator parameter  revert
WhatsNewView.swift, makeVersionDescriptions      revert
ScanCodeView.swift, scanner branch              revert
ScanProtocolServer.swift, scanner branch         revert
preview bodies                                   revert to master
ActiveCallView.swift, ActiveCallOverlay_Previews deleted   keep
SnapshotTests.swift, 5 added scenes              revert
SnapshotTests.swift, ActiveCallOverlay scene     removed: keep
SnapshotTests.swift, VersionView scene           remove
SnapshotTests.swift, SnapshotFixtures markdown table       keep
SnapshotTests.swift, renderScene environment     add S1, S2
check-previews.py, EXCLUDED                      master's 7 and VersionView_Previews
scripts/ci/ios-sim/README.md                     QR picture and camera status sentences; VersionView reason
```

Kotlin:

```
ChatModel.kt, clock parameters                  revert
ChatView.kt, clock parameters                    revert
ChatPreviewView.kt, clock parameter              revert
ContactRequestView.kt, clock parameter           revert
QRCode.kt, previewQRCodeImage                    moves to SnapshotTest.kt
desktopTest/resources/preview-qr-code-light.png  keep
desktopTest/resources/preview-qr-code-dark.png   keep
NetworkAndServers.kt preview servers             revert
preview bodies                                   revert to master
DesktopApp.kt, AppPreview deleted                keep
SnapshotTest.kt, setting pins and appBarMaxHeightPx         keep
SnapshotTest.kt, previewsCallingCore             restore master's 5
SnapshotTest.kt, 5 added scenes                  revert
SnapshotTest.kt, AppPreview scene removed        keep
SnapshotTest.kt, renderSnapshot                  provide LocalQRCodeImage
scripts/ci/desktop-linux/README.md               restore the previewsCallingCore paragraph; QR picture sentence names LocalQRCodeImage
```

## 6. App code after the rework

```
iOS      Views/NewChat/QRCode.swift          S1: 10 lines added, 2 changed
iOS      Views/NewChat/NewChatView.swift     S2: 10 lines added, 1 changed
iOS      Views/Call/ActiveCallView.swift     preview deleted
Kotlin   views/newchat/QRCode.kt             S1: 2 lines added, 1 changed
Kotlin   desktopMain DesktopApp.kt           preview deleted
```
