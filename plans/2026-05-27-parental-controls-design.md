# Parental controls: design

Platforms: iOS first, then the same screens in Kotlin for Android. Not on desktop *(provisional, P10)*. No protocol, message or database format changes.

Answers marked *(provisional, Pn)* are recommendations the user has not confirmed yet; Pn names the choice on the decision page. Code references are to master at 85ba221d7. Kotlin paths that start with `views/`, `model/` or `platform/` are under `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.

## 1. What it does

A parent setting up a phone for a child opens Settings → Privacy & security → Parental controls, sets a parent PIN and chooses what to switch off. By default that is connecting to new people, and creating or joining groups and channels; calls, sending files and voice messages, and receiving images, videos and files can be switched off too *(provisional, P11)*. Then the parent hands the phone over.

The child keeps chatting with everyone already in their chats. The top of the chat list says "Parental controls on" *(provisional, P7)*. When the child tries to connect to someone new, by scanning a code, opening or pasting a link, or accepting a request, the app says connecting is turned off by parental controls and offers "Allow once": the parent enters the PIN, sees who is being added, and that one connection goes through. Everything else that is off stays off until the parent changes it on the PIN-protected screen.

Nothing in this feature reads, keeps, shows or reports the child's messages, contacts or groups, to the parent or to anyone *(provisional, P13)*.

This answers issue #4482's second request (a parental PIN and approval before adding contacts). Its first request, forwarding the child's messages to a parent, is monitoring and is not built.

## 2. Principles

1. **Switch off, never watch.** A control removes a capability. No control keeps history, lists contacts or groups to the parent, or sends anything off the device.
2. **Visible on the phone, invisible to contacts.** The restricted phone always shows that controls are on. Contacts learn nothing new: no control sends a message or changes the profile, except that switching calls off sets the ordinary "calls: no" preference (sections 7.4 and 9).
3. **The app prevents casual use; the phone's own parental settings hold the device boundary.** Deleting and reinstalling the app, or clearing its data, removes the controls. Screen Time and Family Link can stop that; the app tells the parent how *(provisional, P9)*.
4. **One check per capability.** Each switch is checked once, in the app's API function that performs the action, so every button that reaches it is covered, including buttons added later *(provisional, P1)*. Buttons are also hidden, for a clean screen; hiding is presentation, not enforcement.
5. **Fail closed.** A missing or unreadable PIN never opens the gate; an action the check can't classify counts as restricted.

## 3. Limits

The child holds the phone, so a determined, technically able child gets around the controls: by reinstalling the app or clearing its data where the phone's settings allow it, by a factory reset, by watching the parent type the PIN, or with another device. Screen Time has the same limits. The setup screen says so in one sentence: these controls stop casual use and state the parent's intent; they are not a lock.

## 4. Who could misuse it

- **The child getting around it.** Covered: every connecting path, including addresses and links made before the controls (section 7.2), the chat console (locked, section 8), importing a database made elsewhere (locked). Not covered: reinstalling or clearing data unless the phone's settings stop it; another device.
- **The parent sliding into watching.** The PIN opens only the parental screen, the locked settings and allow-once. It does not open the child's chats: app lock, self-destruct and profiles, including hidden ones, stay the child's *(provisional, P5)*. Allow-once shows the parent only the name or link of the one connection being approved, which the child is asking for.
- **One adult imposing it on another.** Someone who keeps the PIN can stop an adult reaching anyone new. Safeguards: the indicator is always visible; deleting the app or its data always removes the controls, because the app does not make them survive a reinstall *(provisional, P9)*; nothing is controlled remotely; the PIN reveals nothing about the chats.
- **Strangers contacting the child.** They must not learn the phone is restricted, since that would mark a child. Section 9 lists what a contact can see; none of it is specific to these controls.

## 5. Stored state

- **Switches:** the set of capabilities switched off, from `enum ParentalCapability { connect, groups, calls, sendFiles, voice, receiveMedia }`; an empty set means controls are off. iOS: a group default in `apps/ios/SimpleXChat/AppGroup.swift` (alongside `GROUP_DEFAULT_ALLOW_SHARE_EXTENSION`, line 27), so the notification and share extensions read it. Kotlin: an app preference in `AppPreferences` (`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`).
- **Parent PIN:** stored like the app passcode. iOS: a new keychain item `kcParentPassword` in `apps/ios/SimpleXChat/KeyChain.swift` (same access policy, `kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly`, line 12). Kotlin: a new `KeyStoreItem` in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt` (lines 17-19 hold the existing three).
- **Kept out of the settings archive.** Importing a database or migrating to this device also imports the app settings packed with it (iOS `apps/ios/Shared/Model/SimpleXAPI.swift:2309-2317`, `apps/ios/Shared/Views/UserSettings/AppSettings.swift:14`; Kotlin `AppSettings`, `SimpleXAPI.kt:8711`). The switches and the PIN are not part of `AppSettings`, so no archive can change them.
- **Both live outside the chat database.** Deleting the database and the self-destruct passcode leave them in place (iOS `apps/ios/Shared/Views/LocalAuth/LocalAuthView.swift:65-79`; Kotlin `views/localauth/LocalAuthView.kt:57-75` and `views/database/DatabaseView.kt:556-578` remove only database files and their own preferences). Deleting the app or its data removes them.
- **Reinstall on iOS:** iOS can keep keychain items after the app is deleted; the app already removes its passcodes on the first launch without a database (`apps/ios/Shared/AppDelegate.swift:111-122`). `kcParentPassword` is removed there too, so a reinstalled app never asks for a PIN nobody remembers.
- **Before calls were switched off:** each profile's previous calls preference, so switching calls back on restores it (section 7.4).

## 6. The parent gate and the parental screen

**Setting the PIN** reuses `SetAppPasscodeView` with `passcodeKeychain: kcParentPassword` (iOS `apps/ios/Shared/Views/LocalAuth/SetAppPasscodeView.swift:13-18`; Kotlin `views/localauth/SetAppPasscodeView.kt:14-15`). It must not be compared with the app passcode or the self-destruct passcode, in either direction: the view's default `prohibitedPasscodeKeychain` is the self-destruct item (line 15), and rejecting a matching PIN would tell the parent the child's passcode by trial, and the reverse. The parental use passes no prohibited item.

**Asking for the PIN** reuses the PIN pad: a new `authenticateParent(reason:completed:)` sets `ChatModel.laRequest` with the parent PIN as `password` and `selfDestruct: false` (iOS `LocalAuthRequest`, `apps/ios/Shared/Views/Helpers/LocalAuthenticationUtils.swift:29-35`, checked in `LocalAuthView.swift:31`; Kotlin `LocalAuthView.kt:26-43`). It never uses system authentication, which the child can pass. It does not reuse `authenticate` or `authorize`: those follow the app-lock mode and treat "unavailable" as success (`LocalAuthenticationUtils.swift:19-26,39-58`). No PIN stored means the gate stays shut.

**The parental screen** is a new row in Settings → Privacy & security (iOS `apps/ios/Shared/Views/UserSettings/PrivacySettings.swift`; Kotlin `views/usersettings/PrivacySettings.kt`). Opening it asks for the PIN once controls are on. In order:
1. What this does and doesn't do (section 3), and how to stop app deletion with the phone's own settings: Screen Time → Content & Privacy Restrictions → iTunes & App Store Purchases → Deleting Apps: Don't Allow on iOS; app approval in Family Link on Android *(provisional, P9)*.
2. Set or change the parent PIN.
3. The switches, with the defaults of section 1.
4. When connecting is switched off: one confirmation that switches the auto-accepts off and removes pending links (section 7.2).
5. "Turn off parental controls": PIN, then everything returns to how it was, including the calls preference.

**The indicator:** while any switch is off, a line "Parental controls on" at the top of the chat list (iOS `apps/ios/Shared/Views/ChatList/ChatListView.swift`; Kotlin `views/chatlist/ChatListView.kt`) and on the Settings screen, opening the parental screen *(provisional, P7)*.

## 7. The controls

Each control lists the API functions where its check runs, on both platforms, and the screens where its buttons are hidden. A check that fails throws a new error, "Turned off by parental controls", which the existing error alerts show; for connecting, the alert offers "Allow once" (section 7.1).

### 7.1 Connecting to new people (`connect`)

Off: no new contact can be added from this phone, in either direction.

Checks (iOS `apps/ios/Shared/Model/SimpleXAPI.swift`; Kotlin `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`):

| Action | iOS | Kotlin |
|---|---|---|
| Plan a link (first step of every link path; the alert comes before the preview). Not the local name lookup while typing, which plans with `resolveMode` never and connects nothing (iOS `apps/ios/Shared/Views/ChatList/ChatListView.swift:865`, Kotlin `views/chatlist/ChatListView.kt:889`) | `apiConnectPlan` 1043 | `apiConnectPlan` 1681 |
| Connect via a link | `apiConnect` 1058 | `apiConnect` 1691 |
| Connect a prepared contact (short links) | `apiConnectPreparedContact` 1231 | `apiConnectPreparedContact` 1864 |
| Connect via a contact's address card | `apiConnectContactViaAddress` 1245 | `apiConnectContactViaAddress` 1884 |
| Create a one-time invitation | `apiAddContact` 1020 | `apiAddContact` 1651 |
| Create an address | `apiCreateUserAddress` 1463 | `apiCreateUserAddress` 2069 |
| Turn address auto-accept or the business address on (turning them off stays allowed: the setup does it) | `apiSetUserAddressSettings` 1506 | `apiSetUserAddressSettings` 2118 |
| Turn auto-accept of member contacts on | `apiSetUserAutoAcceptMemberContacts` 305 | `apiSetUserAutoAcceptMemberContacts` 1079 |
| Turn auto-accept of group invitations on | `apiSetUserAutoAcceptGroupInvitations` 309 | `apiSetUserAutoAcceptGroupInvitations` 1085 |
| Accept a contact request | `apiAcceptContactRequest` 1516 | `apiAcceptContactRequest` 2132 |
| Contact a group member directly | `apiCreateMemberContact` 2129, `apiSendMemberContactInvitation` 2135 | `apiCreateMemberContact` 2636, `apiSendMemberContactInvitation` 2645 |
| Accept a member's direct-contact invitation | `apiAcceptMemberContact` 2141 | `apiAcceptMemberContact` 2654 |

A link is checked against `connect` or `groups` by what its plan says it is; a link the app couldn't plan counts as both.

Buttons hidden:
- iOS (`apps/ios/Shared/Views/`): the new-chat sheet's "Create 1-time link", "Scan / Paste link", paste field and "Connect to @name" rows (`NewChat/NewChatMenuButton.swift:96-129,355-406`); the chat-list search's paste and name rows (`ChatList/ChatListView.swift:751-918`); the connect cards after onboarding and the banner (`NewChat/OnboardingCards.swift:150-302`, `Onboarding/ConnectBannerCard.swift:33-58`); creating an address (`UserSettings/UserAddressView.swift:295-336`); "Accept" on contact requests in the chat list, the contacts list, the chat and the notification (`ChatList/ChatListNavLink.swift:134-153,495-527`, `Contacts/ContactListNavLink.swift:78-106,281-314`, `Chat/ComposeMessage/ContextContactRequestActionsView.swift:21-35`, `Model/NtfManager.swift:64-70`); contact cards (`ChatList/ChatListNavLink.swift:97-119`, `Contacts/ContactListNavLink.swift:217-261`); direct contact with a group member (`Chat/Group/GroupMemberInfoView.swift:364-377,420-430`, `Chat/ComposeMessage/ContextMemberContactActionsView.swift:30-40`).
- Kotlin: the new-chat sheet's "1-time link", "Scan / paste link", paste field and name rows (`views/newchat/NewChatSheet.kt:57-62,188-197,322,413,539-554`); the chat-list banner, search paste and name rows (`views/chatlist/ChatListView.kt:384-419,868-874,1236-1262`); the connect cards (`views/newchat/OnboardingCards.kt:376-417`); creating an address (`views/usersettings/UserAddressView.kt:74-121,310`); "Accept" on contact requests in the chat list, the chat, the contacts list and the Android notification (`views/chatlist/ChatListNavLinkView.kt:132-135,279-283,527-557,698-728`, `views/chat/ComposeContextContactRequestActionsView.kt:69-96,136-165`, `views/contacts/ContactListNavView.kt:78-130`, `apps/multiplatform/android/src/main/java/chat/simplex/app/model/NtfManager.android.kt:130-141`); contact cards (`views/chatlist/ChatListNavLinkView.kt:824-899`, `views/contacts/ContactListNavView.kt:62-70`); direct contact with a group member (`views/chat/group/GroupMemberInfoView.kt:518-522`, `views/chat/ComposeContextGroupDirectInvitationActionsView.kt:100,151-171`).
- Links in messages, in the app's own help and news rows, and opened from outside the app stay tappable: they reach the check and get the "Allow once" alert. Every link path on both platforms passes `planAndConnect` (iOS `apps/ios/Shared/Views/NewChat/NewChatView.swift:1319`; Kotlin `views/newchat/ConnectPlan.kt:24`) before any connecting call.

**Allow once.** The alert offers "Allow once". The parent enters the PIN and sees what is about to happen: the name from the link's plan, or the name on the request. The app then repeats the same action with a one-time pass held in memory, bound to that link or request, used by that one action and never stored. Only connecting has this exception; any other switch is changed on the parental screen *(provisional, P14)*.

### 7.2 Connections the core makes without the app

Three settings let the core connect new people with no app call:
- an address with auto-accept (`src/Simplex/Chat/Library/Subscriber.hs:1535-1541`; `AutoAccept` in `src/Simplex/Chat/Store/Profiles.hs:511-522`), which a business address forces on (`Subscriber.hs:1542-1543`);
- auto-accepting direct-contact requests from group members (iOS `apiSetUserAutoAcceptMemberContacts`, `SimpleXAPI.swift:305`);
- auto-accepting group invitations (iOS `apiSetUserAutoAcceptGroupInvitations`, `SimpleXAPI.swift:309`), which joins groups under `groups`.

And a one-time link or a group link made earlier completes when someone uses it.

So when the parent switches connecting off, the app, after the parent confirms *(provisional, P2)*:
1. turns all three auto-accepts off and the business address off, on every profile, hidden ones included. The address commands take a user id (`apiSetAddressSettings(userId:)`, `SimpleXAPI.swift:1508`), so this needs no profile switching;
2. deletes the pending one-time links and the links of groups the child owns, on every profile;
3. keeps these settings behind the PIN while connecting is off (section 8).

Nothing is listed by name, so the parent learns nothing about the child's profiles, including hidden ones, or their groups. Requests to an address without auto-accept wait, as today; accepting one goes through the check above. Turning connecting back on restores none of it: the child creates new links as needed.

### 7.3 Groups and channels (`groups`)

Off: the child can't create or join groups or channels, create group links, or admit members waiting for review. Existing groups keep working, including adding the child's existing contacts to them.

| Action | iOS | Kotlin |
|---|---|---|
| Create a group | `apiNewGroup` 1949 | `apiNewGroup` 2415 |
| Create a channel | `apiNewPublicGroup` 1961 | `apiNewPublicGroup` 2428 |
| Accept a group invitation | `apiJoinGroup` 2007 | `apiJoinGroup` 2465 |
| Join via a group link (short links) | `apiConnectPreparedGroup` 1238 | `apiConnectPreparedGroup` 1874 |
| Join via a link (older links) | `apiConnect` 1058, by the plan | `apiConnect` 1691, by the plan |
| Create a group link | `apiCreateGroupLink` 2090 | `apiCreateGroupLink` 2585 |
| Admit a member waiting for review | `apiAcceptMember` 2017 | `apiAcceptMember` 2491 |

Buttons hidden:
- iOS (`apps/ios/Shared/Views/`): "Create group" and "Create public channel" (`NewChat/NewChatMenuButton.swift:130-145`); "Join" on group invitations in the chat list and the chat (`ChatList/ChatListNavLink.swift:217-236,320-330`, `Chat/ChatItem/CIGroupInvitationView.swift:95-101`); "Join group" / "Join channel" in a prepared group (`Chat/ComposeMessage/ComposeView.swift:486-496`); creating a group link (`Chat/Group/GroupLinkView.swift:122-125,164-195`).
- Kotlin: "Create group" and "Create channel" (`views/newchat/NewChatSheet.kt:63-68,198-205`); joining from an invitation in the chat list and the chat (`views/chatlist/ChatListNavLinkView.kt:86,196-201,314-316,488-511,902-919`, `views/chat/item/CIGroupInvitationView.kt:90-93`); "Join group" / "Join channel" in a prepared group (`views/chat/ComposeView.kt:1780-1787`); creating a group link (`views/chat/group/GroupLinkView.kt:45-56,119-123,213`).

### 7.4 Calls (`calls`)

Off: no calls in or out.

Incoming calls use the preference the core already enforces *(provisional, P3)*. The app sets the calls preference to "no" on every profile (`apiUpdateProfile`, iOS 1356, Kotlin 1966), on every start and after a profile is created, and keeps each profile's previous value to restore. A user's "no" turns calls off in both directions (`prefEnabled`, `src/Simplex/Chat/Types/Preferences.hs:1148-1153`), and the core turns an incoming call into a "feature rejected" item that never rings (`Subscriber.hs:3051-3057,3078-3081`), in the main app and the notification extension alike. While calls are off, the calls row of the user's and each contact's preferences is locked, and the functions behind them refuse turning calls on (`apiUpdateProfile`, iOS 1356, Kotlin 1966; `apiSetContactPrefs`, iOS 1418, Kotlin 2027); the app's own change to "no" passes.

Outgoing calls are also checked in the app, because a contact's "always" still lets the user call (`asymmetric`, `src/Simplex/Chat/Types.hs:721-728`):

| Action | iOS | Kotlin |
|---|---|---|
| Start a call | `apiSendCallInvitation` 1834 | `apiSendCallInvitation` 2172 |

Buttons hidden:
- iOS (`apps/ios/Shared/Views/`): the chat toolbar's call menu (`Chat/ChatView.swift:562-583`), contact info's call buttons (`Chat/ChatInfoView.swift:160-161`), member info's call buttons (`Chat/Group/GroupMemberInfoView.swift:370-371`). Calls started by Siri or from the phone's recents (`apps/ios/Shared/ContentView.swift:325-355`) reach `apiSendCallInvitation` and are refused there.
- Kotlin: the chat toolbar's call button and menu (`views/chat/ChatView.kt:1296-1300,1552-1561`), contact info's call buttons (`views/chat/ChatInfoView.kt:576-577`), member info's call buttons (`views/chat/group/GroupMemberInfoView.kt:512-513`).

### 7.5 Sending files and voice messages (`sendFiles`, `voice`)

Off: the composer offers no attachments (photos, camera, files) or no voice button; forwarding a message with a file is refused; the share extension does not send files.

| Action | iOS | Kotlin |
|---|---|---|
| Send messages; refused when any carries a file (`sendFiles`) or is voice (`voice`) | `apiSendMessages` 559 | `apiSendMessages` 1253 |
| Forward messages; refused when any carries a file | `apiForwardChatItems` 526 | `apiForwardChatItems` 1312 |

Share extension (iOS): it starts its own core and sends through its own `apiSendMessages` (`apps/ios/SimpleX SE/ShareAPI.swift:57-85`, called at `apps/ios/SimpleX SE/ShareModel.swift:149`), so the check runs there too: with `sendFiles` off it refuses images, videos and files and still shares text and links. It reads the switches from group defaults, as it reads "Allow sharing" today (`ShareModel.swift:77-80`).

Buttons hidden:
- iOS (`apps/ios/Shared/Views/Chat/`): the attachment button and its menu (`ComposeMessage/ComposeView.swift:1127-1147`, items 608-689), pasting images into the text field (`ComposeMessage/NativeTextEditor.swift:186-256`), the voice button (`ComposeMessage/SendMessageView.swift:120-128`). Forwarding stays available for text and is refused for files at `apiForwardChatItems`.
- Kotlin: the attachment button (`views/chat/ComposeView.kt:1311-1345`), the voice button (`views/chat/SendMsgView.kt:137-155`), images inserted from the Android keyboard (`apps/multiplatform/common/src/androidMain/kotlin/chat/simplex/common/platform/PlatformTextField.android.kt:108-120`). Sharing into the app from other Android apps (`apps/multiplatform/android/src/main/java/chat/simplex/app/MainActivity.kt:157-209`) ends in `apiSendMessages` and is refused there for files.

### 7.6 Receiving images, videos and files (`receiveMedia`)

Off: nothing is downloaded or opened, and image and video previews are replaced by a plain placeholder *(provisional, P4)*, because image and video messages carry their own preview inside the message (`MCImage`, `MCVideo`, `src/Simplex/Chat/Protocol.hs:747-748`), independent of the file.

| Action | iOS | Kotlin |
|---|---|---|
| Receive a file (manual and automatic) | `receiveFile` 1598, `receiveFiles` 1607 | `receiveFile` 2385, `receiveFiles` 2269 |
| Decide what to auto-receive (main app and notification extension) | `ChatItem.autoReceiveFile()`, `apps/ios/SimpleXChat/ChatTypes.swift:4003-4013`, used by the notification extension at `apps/ios/SimpleX NSE/NotificationService.swift:1031` | the new-message handler decides and calls `receiveFile` (checked), `SimpleXAPI.kt:3070-3079`; Android has no separate extension |

Previews replaced and download buttons hidden:
- iOS (`apps/ios/Shared/Views/`): image, video, voice and file items (`Chat/ChatItem/CIImageView.swift:50-60`, `Chat/ChatItem/CIVideoView.swift:70,101`, `Chat/ChatItem/CIVoiceView.swift:302-316`, `Chat/ChatItem/CIFileView.swift:88-100`), the "Download" actions (`Chat/ChatView.swift:2328-2329,1597-1608`), and chat-link preview thumbnails in the chat list (`ChatList/ChatPreviewView.swift:444-453`).
- Kotlin: image, video, file and voice items (`views/chat/item/CIImageView.kt:225-236`, `views/chat/item/CIVideoView.kt:102,132`, `views/chat/item/CIFileView.kt:76-86`, `views/chat/item/CIVoiceView.kt:424-428`), the "Download" menu item (`views/chat/item/ChatItemView.kt:458-467`), media previews in the chat list (`views/chatlist/ChatPreviewView.kt:335-353`). Android's background service receives through the same `receiveFile` and is covered by its check.

Received files the child already has stay as they are.

## 8. Settings that need the PIN

While any switch is off, these open only with the parent PIN *(provisional, P5)*:

| Setting | Why |
|---|---|
| Parental controls screen | The controls themselves. |
| Database: export, import, and migration to or from another device | Import and migration bring in contacts made elsewhere; export and migration copy everything to an unrestricted install. |
| Developer tools | They open the chat console, which sends raw commands past the app's checks (iOS `apps/ios/Shared/Views/UserSettings/DeveloperView.swift:35-39`, `apps/ios/Shared/Views/TerminalView.swift:147-160`; Kotlin `views/usersettings/DeveloperView.kt:32`). |
| Address settings, auto-accept of member contacts and of group invitations, while connecting is off | They would connect new people (section 7.2). iOS `apps/ios/Shared/Views/UserSettings/UserAddressView.swift:164-173,758-774`, `apps/ios/Shared/Views/UserSettings/PrivacySettings.swift:122-127`; Kotlin `views/usersettings/UserAddressView.kt:625-677`, `views/usersettings/PrivacySettings.kt:86-104`. |
| The calls preference, while calls are off | It would let calls ring again (section 7.4). |

**Turned off entirely** while any switch is off, even for the parent: "Use from desktop" (iOS `apps/ios/Shared/Views/ChatList/UserPicker.swift:73` → `apps/ios/Shared/Views/RemoteAccess/ConnectDesktopView.swift`, `connectRemoteCtrl` `SimpleXAPI.swift:1740`; Kotlin, Android only: `views/chatlist/UserPicker.kt:378-389` → `views/remote/ConnectDesktopView.kt`, `connectRemoteCtrl` `SimpleXAPI.kt:2727`; the core runs the desktop's commands with its own filter, `src/Simplex/Chat/Remote.hs:508-556`). A linked computer drives the phone's chats directly, past every check; locking it behind the PIN instead would give the parent a way to read the chats from their own computer.

Left to the child: app lock and its passcode, self-destruct, chat profiles including hidden ones, network and servers, call settings, appearance, notifications. None of them gets around a switch, since the switches apply to the whole app; locking app lock or hidden profiles would let the person holding the PIN open the child's chats.

## 9. What contacts see

- A contact request sent to the child stays pending, as with anyone who doesn't accept.
- A file the child can't receive stays not received, as with anyone who doesn't open it.
- With calls off, the child's preferences show "calls: off", and each contact's chat shows the usual note that the contact turned calls off (`createRcvFeatureItems`, `src/Simplex/Chat/Library/Subscriber.hs:2798`), as for any adult who does; their app doesn't offer the call button. Declining in the app instead would send nothing (`APIRejectCall`, `src/Simplex/Chat/Library/Commands.hs:1541-1552`), so callers would see an unanswered call; it is not chosen because every ringing path (in-app, CallKit, VoIP push, the notification extension, Android's call notification) would have to decline, and one missed path rings *(provisional, P3)*.
- Address auto-accept and the other auto-accepts are not announced to anyone, so switching them off shows nothing (`contactShortLinkData`, `Commands.hs:4840-4846`).
- Nothing else: no control sends a message or adds anything to the profile.

## 10. Onboarding

No onboarding step *(provisional, P8)*. A question about children asked of every new user puts parental controls in front of adults who have none; a parent sets the controls up in Settings before handing the phone over. The website's FAQ points to the setting.

Onboarding today (since #6888, 2026-04-27) is: introduction, first profile, "Your network" (routers and notifications), conditions (iOS `apps/ios/Shared/Views/Onboarding/OnboardingView.swift:18-57`; Kotlin `views/onboarding/OnboardingView.kt:3-13`). Making connections moved out of onboarding into cards at the top of the chat list (#6753; iOS `apps/ios/Shared/Views/NewChat/OnboardingCards.swift:135-308`, Kotlin `views/newchat/OnboardingCards.kt:332`). With connecting off, those cards are hidden (section 7.1).

## 11. Not built

- Forwarding or showing the child's messages, contacts or groups to anyone.
- Preventing the child from deleting messages or chats, or forcing disappearing messages off *(provisional, P13)*: they exist only to keep history for reading.
- An "incognito off" switch *(provisional, P6)*: incognito hides the child's real profile from new contacts, and with connecting off every new contact is one the parent approved.
- Making the controls survive deleting the app *(provisional, P9)*: the app removes its keychain passcodes after a reinstall on purpose (`apps/ios/Shared/AppDelegate.swift:111-122`, #3841), Apple's engineers call keychain survival undocumented (https://developer.apple.com/forums/thread/36442), and it would take away the way out for an adult on whom the controls were imposed. The phone's own settings do it with the parent's own passcode.
- Desktop *(provisional, P10)*: the desktop app stores passcodes unencrypted (`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt:3-17`) in `settings.properties` (`platform/Resources.desktop.kt:40,64`), so every switch could be edited and the PIN read. The parental screen is hidden on desktop.
- Approval from the parent's own phone. iOS 26's PermissionKit can send a child's request to the parent through Messages, but it shows the new contact's name to Apple's Messages and needs the app to know the user is a child (WWDC25 session 293, https://developer.apple.com/videos/play/wwdc2025/293/) *(provisional, P12)*.
- Time limits and app blocking: Screen Time and Family Link do these.
- Enforcement in the core *(provisional, P1)*: the app passing the switches to the core, which refuses the commands, would also cover the chat console and every client. It is the next step if the app checks prove too easy to get around.

## 12. Build order

Each step ships on its own, iOS first, then Kotlin for Android.

1. Stored state, the parent PIN, the parent gate, the parental screen, the indicator and the settings that need the PIN.
2. Connecting: the checks of 7.1, allow-once, and the clean-up of auto-accepts and links of 7.2.
3. Groups and channels (7.3).
4. Calls (7.4), sending files and voice (7.5), receiving media (7.6).

## 13. Open choices

The provisional answers above are the recommended options of the decision page; each Pn there has the options, the costs and the evidence. Until the user answers, the recommendations stand.
