# Pure previews: rework

Date: 2026-10-29.

Pull request: https://github.com/simplex-chat/simplex-chat/pull/7938. Branch: ep/pure-previews.

The branch had 109 preview-only parameters, each marked `// do not pass`, in 65 files. All are removed. Previews run master's code, except three views split into a loader and a layout, and one environment value for the QR picture.

Rules, from the owner:

```
1   previews keep master's core calls that send no network request: database reads, parsing, validation
2   a preview whose rendering in CI can send a network request draws a layout split cleanly from its view, or its scene is excluded
3   VersionView is split, so that its scene does not depend on the release
4   QR codes: one environment value on iOS, one CompositionLocal on Kotlin, set by the snapshot tests
5   camera: master's code
```

Paths:

```
iOS      apps/ios/Shared/
Kotlin   apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/
```

Line numbers of app code are those of master at 3bdecd63f. Core lines are those of `src/Simplex/Chat/Library/Commands.hs` and `src/Simplex/Chat/Remote.hs` at the same commit; agent lines are those of simplexmq c0c416daf, the commit `cabal.project` pins.

## 1. Outcomes

```
(a) CI    the parameter goes; the preview runs master's code; the scene is rendered on master and compared on every ios-sim or desktop-linux run
(a) CL    the parameter goes; the preview draws previewDate (2025-01-15 10:15 UTC); from 2026-01-01 the clock-dependent text is "1/15/25" and "Wed, 15 Jan 2025", as in master's references
(a) X     the parameter goes; the scene stays excluded, as on master
(a) S1    the parameter goes; the snapshot test sets the QR picture of seam S1
(a) SIM   the parameter goes; the simulator build of CodeScanner draws a fixed text and requests no camera access
(b)       the parameter goes; the preview draws the layout of a loader/layout split (section 3)
```

Calls kept in previews, as on master:

```
db        core API reading or writing the CI database, without network (section 4)
fn        pure core function: markdown parse, name validation, JSON length, server address parse
agent     agent API reading agent memory
store     StoreKit read without network: canMakePayments, storefront country code
keychain  keychain read
files     file read or delete
defaults  UserDefaults write
paste     pasteboard read
camera    camera authorization status, and the permission request of ScannerInView
global    write to a shared singleton
```

## 2. Parameters

| # | File | Parameter | Outcome | Measure | Calls kept |
|---|---|---|---|---|---|
| 1 | Model/ChatModel.swift | `ChatModel.init(migrationState:)` | (a) CI | previews call `ChatModel()`, as master's 12 preview sites | files |
| 2 | Views/Badges/BadgesCheckOrderView.swift | `priceList` | (b) | `BadgesCheckOrderLayout(priceList:)` | — |
| 3 | Views/Badges/BadgesCheckOrderView.swift | `previewCanBuy` | (b) | the layout reads `store.canBuy` | — |
| 4 | Views/Badges/BadgesCheckOrderView.swift | `previewCanMakePayments` | (b) | the layout reads `AppStore.canMakePayments` | store |
| 5 | Views/Badges/BadgesChooseBadgeView.swift | `previewMonthlyPrices` | (a) X | master | — |
| 6 | Views/Badges/BadgesChooseBadgeView.swift | `previewBrowserAllowed` | (a) X | master | — |
| 7 | Views/Badges/BadgesHowLongView.swift | `priceList` | (b) | `BadgesHowLongLayout(priceList:)` | — |
| 8 | Views/Badges/BadgesRedeemCodeView.swift | `previewCameraStatus` | (a) CI | ScannerInView draws "Tap to scan" | camera |
| 9 | Views/Call/IncomingCallView.swift | `previewInvitation` | (a) CI | master's preview sets `CallController.shared.activeCallInvitation` | global |
| 10 | Views/Chat/ChatInfoView.swift | `ChatInfoView.previewContactInfo` | (a) CI | `apiContactInfo`, `apiGetContactCode` for the sample contact | db |
| 11 | Views/Chat/ChatInfoView.swift | `ChatInfoView.previewChatItemTTL` | (a) CI | `ChatModel.shared.chatItemTTL` of the CI database | — |
| 12 | Views/Chat/ChatInfoView.swift | `ChatTTLOption.previewChatItemTTL` | (a) CI | as row 11 | — |
| 13 | Views/Chat/ChatItemForwardingView.swift | `previewChats` | (a) CI | `ChatModel.shared.chats` of the CI profile | — |
| 14 | Views/Chat/ChatItemForwardingView.swift | `previewHasSimplexLink` | (a) CI | `hasSimplexLink("hello")` | fn |
| 15 | Views/Chat/ChatView.swift | `previewAvailableContent` | (a) CI | `initChatView` for the sample contact | db |
| 16 | Views/Chat/Group/AddGroupMembersView.swift | `AddGroupMembersView.previewContacts` | (a) CI | `filterMembersToAdd`: contacts of `ChatModel.shared.chats` | — |
| 17 | Views/Chat/Group/AddGroupMembersView.swift | `AddGroupMembersViewCommon.previewContacts` | (a) CI | as row 16 | — |
| 18 | Views/Chat/Group/ChannelRelaysView.swift | `previewMembers` | (a) CI | `loadGroupMembers`, `apiGetGroupRelays` for the sample group | db |
| 19 | Views/Chat/Group/GroupChatInfoView.swift | `previewGroupLink` | (a) CI | `apiGetGroupLink` for the sample group | db |
| 20 | Views/Chat/Group/GroupChatInfoView.swift | `previewChatItemTTL` | (a) CI | as row 11 | — |
| 21 | Views/Chat/Group/GroupChatInfoView.swift | `sendReceiptsUserDefault` | (a) CI | `onAppear` reads `User.sampleData`, which the test sets as `ChatModel.shared.currentUser` | — |
| 22 | Views/Chat/Group/GroupLinkView.swift | `previewQRImage` | (a) S1 | fixed QR picture | — |
| 23 | Views/Chat/Group/GroupLinkView.swift | `shouldCreate` | (a) CI | `apiCreateGroupLink` for group 1 fails on the CI database | db |
| 24 | Views/Chat/Group/GroupMemberInfoView.swift | `previewMemberInfo` | (a) CI | `apiGroupMemberInfo`, `apiGetGroupMemberCode`, `apiContactInfo` for the sample member | db |
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
| 36 | Views/ChatList/ServersSummaryView.swift | `previewServersSummary` | (a) X | master | — |
| 37 | Views/ChatList/ServersSummaryView.swift | `previewServerAddresses` | (a) X | master | — |
| 38 | Views/ChatList/UserPicker.swift | `previewUsers` | (a) CI | `listUsersAsync` of the CI database | db |
| 39 | Views/Database/DatabaseEncryptionView.swift | `initialRandomDBPassphrase` | (a) CI | `initialRandomDBPassphraseGroupDefault` of the CI app | — |
| 40 | Views/Database/DatabaseEncryptionView.swift | `storedKey` | (a) CI | `kcDatabasePassword` of the CI app | keychain |
| 41 | Views/Database/DatabaseEncryptionView.swift | `previewDatabasePassword` | (a) CI | as row 40 | keychain |
| 42 | Views/Database/DatabaseErrorView.swift | `storedDBKey` | (a) CI | as row 40 | keychain |
| 43 | Views/Database/DatabaseErrorView.swift | `previewShowRestoreDbButton` | (a) CI | `shouldShowRestoreDbButton`: `encryptionStartedDefault` of the CI app is false | — |
| 44 | Views/Database/DatabaseView.swift | `previewUserName` | (a) CI | `User.sampleData`, which the test sets | — |
| 45 | Views/Database/DatabaseView.swift | `previewAppFilesCountAndSize` | (a) CI | the CI app's files folder | files |
| 46 | Views/LocalAuth/SetAppPasscodeView.swift | `previewProhibitedPasscode` | (a) CI | `kcSelfDestructPassword` of the CI app | keychain |
| 47 | Views/Migration/MigrateFromDevice.swift | `previewMigrationState` | (a) X | master | — |
| 48 | Views/Migration/MigrateToDevice.swift | `pasteboardHasStrings` | (a) CI | the CI simulator's pasteboard, empty after the smoke test | paste |
| 49 | Views/Migration/MigrateToDevice.swift | `previewCameraStatus` | (a) CI | ScannerInView draws "Tap to scan" | camera |
| 50 | Views/NewChat/AddChannelView.swift | `previewValidName` | (a) CI | `mkValidName("")` | fn |
| 51 | Views/NewChat/AddChannelView.swift | `previewHasRelays` | (a) CI | `checkHasRelays` of the CI profile | db |
| 52 | Views/NewChat/AddGroupView.swift | `previewValidName` | (a) CI | `mkValidName("")` | fn |
| 53 | Views/NewChat/NewChatView.swift | `NewChatView.previewQRImage` | (a) X | master | — |
| 54 | Views/NewChat/NewChatView.swift | `connLinkInvitation` | (a) X | master | — |
| 55 | Views/NewChat/NewChatView.swift | `InviteView.previewQRImage` | (a) X | master | — |
| 56 | Views/NewChat/NewChatView.swift | `ScannerInView.previewCameraStatus` | (a) CI | rows 8, 49, 67 | camera |
| 57 | Views/NewChat/QRCode.swift | `MutableQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 58 | Views/NewChat/QRCode.swift | `SimpleXCreatedLinkQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 59 | Views/NewChat/QRCode.swift | `SimpleXLinkQRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 60 | Views/NewChat/QRCode.swift | `QRCode.previewImage` | (a) S1 | fixed QR picture | — |
| 61 | Views/Onboarding/CreateProfile.swift | `previewValidName` | (a) CI | `mkValidName("")` | fn |
| 62 | Views/Onboarding/CreateSimpleXAddress.swift | `previewQRImage` | (a) S1 | master's preview draws the address QR code only when `ChatModel.shared.userAddress` is set; it is nil for the CI profile | — |
| 63 | Views/Onboarding/OnboardingView.swift | `previewVersionSaved` | (a) CI | `setLastVersionDefault` | defaults |
| 64 | Views/Onboarding/SimpleXInfo.swift | `previewVersionSaved` | (a) CI | as row 63 | defaults |
| 65 | Views/Onboarding/WhatsNewView.swift | `previewInUS` | (a) CI | as row 32 | store |
| 66 | Views/RemoteAccess/ConnectDesktopView.swift | `previewRemoteCtrls` | (a) CI | `setLocalDeviceName`, `listRemoteCtrls` of the CI database | db |
| 67 | Views/RemoteAccess/ConnectDesktopView.swift | `previewCameraStatus` | (a) CI | ScannerInView draws "Tap to scan" | camera |
| 68 | Views/TerminalView.swift | `authorized` | (a) X | master's parameter; master | — |
| 69 | Views/TerminalView.swift | `showing` | (a) X | master | — |
| 70 | Views/UserSettings/CallSettings.swift | `previewInChina` | (a) CI | `CallController.isInChina` of the simulator's storefront | store |
| 71 | Views/UserSettings/NetworkAndServers/NetworkAndServers.swift | `justOpened` | (a) CI | `getUserServers`, `validateServers_` of the CI database | db |
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
| 82 | Views/UserSettings/VersionView.swift | `previewVersionInfo` | (b) | `VersionLayout(appVersion:appBuild:versionInfo:)` | — |
| 83 | model/ChatModel.kt | `getTimestampDateText.previewNow` | (a) CL | DateSeparator text | — |
| 84 | model/ChatModel.kt | `getTimestampText.previewNow` | (a) CL | chat list time | — |
| 85 | views/chat/ChatView.kt | `ChatLayout.previewNow` | (a) CL | as row 83 | — |
| 86 | views/chat/ChatView.kt | `ChatItemsList.previewNow` | (a) CL | as row 83 | — |
| 87 | views/chat/ChatView.kt | `DateSeparator.previewNow` | (a) CL | as row 83 | — |
| 88 | views/chat/group/GroupMemberInfoView.kt | `previewContactConnectionStats` | (a) CI | `apiContactInfo` in `withBGApi` throws "Controller is not initialized" in the test JVM | db |
| 89 | views/chat/group/GroupProfileView.kt | `previewShortDescrFits` | (a) X | `previewsCallingCore`, as master | — |
| 90 | views/chatlist/ChatPreviewView.kt | `previewNow` | (a) CL | as row 84 | — |
| 91 | views/chatlist/ContactRequestView.kt | `previewNow` | (a) CL | as row 84 | — |
| 92 | views/database/DatabaseErrorView.kt | `previewStoredDBKey` | (a) CI | `ksDatabasePassword.get()` returns null with the CI test's settings | — |
| 93 | views/database/DatabaseErrorView.kt | `previewShowRestoreDbButton` | (a) CI | `shouldShowRestoreDbButton` returns false: `encryptionStartedAt` is null in the CI test's settings | — |
| 94 | views/database/DatabaseErrorView.kt | `previewUseKeychain` | (a) CI | `storeDBPassphrase` default, true | — |
| 95 | views/newchat/AddChannelView.kt | `AddChannelView.previewNameValid` | (a) X | `previewsCallingCore`, as master | — |
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

Counts:

```
(a)      104
  CI      64
  CL       7
  X       17
  S1      14
  SIM      2
(b)        5
(c)        0
```

## 3. Splits

Each loader keeps its view's name and parameters, so its callers are unchanged. The loader runs the call it ran before, at the same moment: the view's `.task` or `.onAppear` moves from the end of the layout's modifiers to the layout in the loader's body. State stays where it was: the loader owns the loaded value, the layout owns its UI state. The preview draws the layout with fixed values.

```
BadgesCheckOrderView   Views/Badges/BadgesCheckOrderView.swift
  loader               BadgesCheckOrderView(level, period, startDate, priceList): .task { await BadgeStore.shared.load() }
  layout               BadgesCheckOrderLayout(level, period, startDate, priceList)
  moved                .task { await store.load() } (:107)
  preview              BadgesCheckOrderLayout
  full lines           10 added, 1 removed, 1 changed

BadgesHowLongView      Views/Badges/BadgesHowLongView.swift
  loader               BadgesHowLongView(level, startDate, priceList): .task { await BadgeStore.shared.load() }
  layout               BadgesHowLongLayout(level, startDate, priceList)
  moved                .task { await store.load() } (:145)
  preview              BadgesHowLongLayout
  full lines           9 added, 1 removed, 1 changed

VersionView            Views/UserSettings/VersionView.swift
  loader               VersionView: @State versionInfo, .onAppear { versionInfo = try apiGetVersion() }
  layout               VersionLayout(appVersion, appBuild, versionInfo)
  moved                .onAppear (:40-46), @EnvironmentObject theme (:13) into the layout
  preview              VersionLayout(appVersion: "7.1", appBuild: "363", versionInfo: CoreVersionInfo(version: "7.1.0.14", ...))
  full lines           16 added, 8 removed, 1 changed
```

`store` of both badge layouts is `BadgeStore.shared` (`@ObservedObject private var store = BadgeStore.shared`), the object the loaders call.

## 4. Network evidence

Previews that kept a core, agent or StoreKit call:

| Rows | Scene | Code path | Network in CI |
|---|---|---|---|
| 2, 3, 4 | BadgesCheckOrderView | BadgesCheckOrderView.swift:107 `store.load()` → BadgeStore.swift:219 `Product.products(for:)`, :228 `subscriptionOriginalIds()` | StoreKit: yes; split |
| 7 | BadgesHowLongView | BadgesHowLongView.swift:145 `store.load()` → BadgeStore.swift:219 | StoreKit: yes; split |
| 10 | ChatInfoView | ChatInfoView.swift:305-326: :313 `apiContactInfo` → Commands.hs:1956-1964 (`getContact` :1958, agent `getConnectionServers` :1963 → Agent.hs:3128-3131 store read, :3141-3180 agent memory); :314 `apiGetContactCode` → Commands.hs:2040-2053, :3838-3839 → Agent.hs:3133-3138 store read | no |
| 15 | ChatView | ChatView.swift:333-338 → `initChatView` :743-803: :750 `apiContactInfo` (row 10) for the sample contact; :791 → :805-818 `apiGetChatContentTypes` → Commands.hs:738-739 store read; :772-790, with :783 `apiGetUpdatedGroupLinkData`, needs a group with `useRelays`; :797-801 needs an unread chat | no |
| 18 | ChannelRelaysView | ChannelRelaysView.swift:45-55: `loadGroupMembers` → ChatModel.swift:592-602 `apiListMembers` → Commands.hs:3293-3294 store read; :49 `apiGetGroupRelays` → Commands.hs:2790-2795 store read | no |
| 19 | GroupChatInfoView | GroupChatInfoView.swift:298-312: :305 `apiGetGroupLink` → Commands.hs:3392-3395 store read | no |
| 23 | GroupLinkView | GroupLinkView.swift:164-169 → :179-196 `apiCreateGroupLink(1)` → Commands.hs:3364-3377: `getGroupInfo` :3365 fails before `createConnection` :3373; the CI database has no group: smoke-test.sh:93-126 creates only the profile | no |
| 24 | GroupMemberInfoView | GroupMemberInfoView.swift:293-328: :301 `apiGroupMemberInfo` → Commands.hs:1986-1989 store read and agent memory; :303 `apiGetGroupMemberCode` → Commands.hs:2054-2078 store read; :320 `apiContactInfo` (row 10) | no |
| 26 | MemberSupportView | MemberSupportView.swift:22-26 `loadGroupMembers` (row 18) | no |
| 27 | SecondaryChatView | ChatView `initChatView` for a group: `GroupInfo.sampleData.useRelays` is false (SimpleXChat/ChatTypes.swift:3103), so :772-790 is skipped; :791 (row 15) | no |
| 31, 35 | ChatListView | ChatListView.swift:736-760 `startTask` :753 `getAgentSubsTotal` → Commands.hs:3760-3764 → Agent/Client.hs:2627-2645 agent memory | no |
| 38 | UserPicker | UserPicker.swift:100-116: :106 `listUsersAsync` → Commands.hs:517 store read | no |
| 51 | AddChannelView | AddChannelView.swift:112-113 → `checkHasRelays` :272-278: :273 `getUserServers` → Commands.hs:1816-1817 store read | no |
| 66 | ConnectDesktopView | ConnectDesktopView.swift:87-104: :88 `setLocalDeviceName` → Commands.hs:3704 variable write; :89 `listRemoteCtrls` → Commands.hs:3723 → Remote.hs:606-611 store read; :92 `disconnectDesktop` needs a desktop session; :94 `findKnownDesktop` needs `useMulticast` (:29-31), which needs a linked desktop | no |
| 71 | NetworkServersView | NetworkAndServers.swift:136-155: :140 `getUserServers` (row 51); :144 `validateServers_` → Commands.hs:1833-1834, :4322-4326 store read | no |
| 78 | NotificationsView | NotificationsView.swift:83-85 `apiGetNtfToken` → Commands.hs:1681 → Agent.hs:3332-3336 store read | no |
| 88 | GroupMemberInfoView.kt | GroupMemberInfoView.kt:502-509 `withBGApi { apiContactInfo }` → model/SimpleXAPI.kt:989 throws "Controller is not initialized" | no |
| — | ActiveCallOverlay | `WebRTCClient` of the preview: WebRTCClient.swift:25 `getChatCtrl()` → SimpleXChat/API.swift:19-22 reads the controller pointer; :82-89 sets two flags of `RTCAudioSession.sharedInstance()` | no |

## 5. QR picture (S1)

iOS, `Views/NewChat/QRCode.swift`:

```swift
extension EnvironmentValues {
    struct FixedQRCode: EnvironmentKey {
        static let defaultValue: UIImage? = nil
    }

    var fixedQRCode: UIImage? {
        get { self[FixedQRCode.self] }
        set { self[FixedQRCode.self] = newValue }
    }
}
```

```
QRCode   added     @Environment(\.fixedQRCode) private var fixedQRCode
QRCode   changed   if let image = fixedQRCode ?? image {
QRCode   changed   .task { if fixedQRCode == nil { image = await generateImage(...) } }
```

Kotlin, `views/newchat/QRCode.kt`:

```
added     val LocalFixedQRCode = staticCompositionLocalOf<ImageBitmap?> { null }
added     val fixedQRCode = LocalFixedQRCode.current                                  (QRCode, before remember)
changed   (fixedQRCode ?: qrCodeBitmap(connReq, 1024, errorLevel)).replaceColor(...)   (QRCode, in remember)
```

Snapshot tests:

```
link      https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw, the address of tests/MarkdownTests.hs:324
iOS       setUp: fixedQRCode = await generateImage(link, tintColor: QRCode's default tint, errorLevel: "M")
iOS       renderScene root: .environment(\.fixedQRCode, fixedQRCode)
iOS       QRCode.swift: generateImage is internal, so the test calls it
Kotlin    renderSnapshot root: LocalFixedQRCode provides qrCodeBitmap(link, 1024, QrCode.ErrorLevel.M), in the theme of the render
```

The views add the logo, and on desktop the tint, to the picture, as to a generated one. Short links are drawn `small`, with error level M, in both apps.

Previews with links pass the short form of a preset server, as the core returns it (shortenShortLink): host only, without the key hash.

```
iOS       GroupLinkView_Previews                   connShortLink https://smp6.simplex.im/g#cL3iklYlt_Tws1B_1aGylbT_3HowPMQhuCFoBiCjZ1M; shortLinkDataSet, shortLinkLargeDataSet true
iOS       UserAddressView_Previews                 connShortLink the address link
iOS       QRCode_Previews                          QRCode(uri: the address link, small: true)
Kotlin    PreviewUserAddressLayoutAddressCreated   connShortLink the address link; shortLinkDataSet, shortLinkLargeDataSet true
Kotlin    PreviewContactConnectionInfoView         connFullLink the invitation of master's QRCode_Previews; connShortLink https://smp4.simplex.im/i#Wu_NH3_F5YphQrIMSCSfWhvqz7tjpZsu/A2dCDGGVkh0-doRPIZVTM-TflHes2uYMVRgA_tK_uCs
Kotlin    PreviewQRCode                            QRCode(connReq = the address link, small = true)
```

Each short link decodes as AConnShortLink and encodes back to the same text.

## 6. Other branch changes

```
ActiveCallView.swift, ActiveCallOverlay_Previews                    master
DesktopApp.kt, AppPreview deleted                                   keep
SnapshotTests.swift, ActiveCallOverlay scene                        master
SnapshotTests.swift, SnapshotFixtures markdown table                keep, values built with initializers
SnapshotTest.kt, setting pins and appBarMaxHeightPx                 keep
SnapshotTest.kt, AppPreview scene removed                           keep
check-previews.py, EXCLUDED                                         master
SnapshotTest.kt, previewsCallingCore                                master
every other file                                                    master
```

## 7. App code

Full lines by `git diff -w --word-diff`, blank lines aside:

```
iOS      Views/Badges/BadgesCheckOrderView.swift      10 added, 1 removed, 1 changed
iOS      Views/Badges/BadgesHowLongView.swift         9 added, 1 removed, 1 changed
iOS      Views/UserSettings/VersionView.swift         16 added, 8 removed, 1 changed
iOS      Views/NewChat/QRCode.swift                   10 added, 4 changed: 1 of them the preview
iOS      Views/Chat/Group/GroupLinkView.swift         3 changed: preview
iOS      Views/UserSettings/UserAddressView.swift     1 changed: preview
Kotlin   views/newchat/QRCode.kt                      2 added, 2 changed: 1 of them the preview
Kotlin   views/newchat/ContactConnectionInfoView.kt   1 changed: preview
Kotlin   views/usersettings/UserAddressView.kt        3 changed: preview
Kotlin   desktopMain DesktopApp.kt                    6 removed: preview and its import
```
