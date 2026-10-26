# Parental controls: design

Platforms: iOS first, then the same screens in Kotlin for Android. Not on desktop *(provisional, P10)*. No protocol, message or database format changes; one new local API command in the chat core (section 7.1).

Answers marked *(provisional, Pn)* are recommendations the user has not confirmed yet; Pn names the choice on the decision page. Code references are to master at 85ba221d7. Kotlin paths that start with `views/`, `model/` or `platform/` are under `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.

## 1. What it does

A parent setting up a phone for a child opens the privacy settings (iOS "Your privacy", Android "Privacy & security") → Parental controls, sets a parent PIN and chooses what to switch off. By default that is connecting to new people, and creating or joining groups and channels; calls, sending files, voice messages, and receiving images, videos and files can be switched off too *(provisional, P11)*. Then the parent hands the phone over.

The child keeps chatting with everyone already in their chats. The top of the chat list says "Parental controls on" *(provisional, P7)*. When the child tries to connect to someone new, by scanning a code, opening or pasting a link, or accepting a request, the app says connecting is turned off by parental controls and offers "Allow once": the parent enters the PIN, sees who is being added, and that one connection goes through. Everything else that is off stays off until the parent changes it on the PIN-protected screen.

Nothing in this feature reads, keeps, shows or reports the child's messages, contacts or groups, to the parent or to anyone *(provisional, P13)*.

This answers issue #4482's second request (a parental PIN and approval before adding contacts). Its first request, forwarding the child's messages to a parent, is monitoring and is not built.

## 2. Principles

1. **Switch off, never watch.** A control removes a capability. No control keeps history, lists contacts or groups to the parent, or sends anything off the device.
2. **The PIN never opens a path to the chats.** It opens only the parental screen and allow-once. Anything that would hand over the chats (database export and migration, the chat console, "Use from desktop") is off while controls are on, even for the parent; to use it, the parent turns the controls off, which the child sees *(provisional, P5)*.
3. **Visible on the phone, invisible to contacts.** The restricted phone always shows that controls are on. No control sends a message or changes the profile.
4. **Check every action where it goes to the core.** Each action the child starts is checked in the app's function that sends it to the chat core, so every button that reaches it is covered, including buttons added later. What the core does on its own (auto-accepting, ringing for a call, receiving inline files) is stopped by a small switch in the core *(provisional, P1)*. Buttons are also hidden, for a clean screen; hiding is presentation, not enforcement.
5. **Fail closed.** A missing or unreadable PIN never opens the gate; an action the check can't classify counts as restricted.

## 3. Limits

The child holds the phone, so a determined, technically able child gets around the controls. The setup screen says in one sentence that the controls stop casual use and state the parent's intent; they are not a lock.

- **Reinstalling the app or clearing its data removes the controls.** On iPhone, Screen Time can stop the app being deleted (Settings → Screen Time → Content & Privacy Restrictions → iTunes & App Store Purchases → Deleting Apps, https://support.apple.com/en-gb/126807); the setup screen says how *(provisional, P9)*. On Android, Family Link approves new downloads but not an app it approved before (https://support.google.com/googleplay/answer/7103028: previously approved apps "will not require additional approval to download"), so reinstalling stays open there.
- **A factory reset, "Erase All Content and Settings", watching the parent type the PIN, or another device.**
- **Connections the child started on a hidden profile** before the controls can still complete: the app can't open a hidden profile without its password (section 7.3).
- **Groups and channels the child is already in stay, and can gain new members** through other members' links *(provisional, P15)*.

## 4. Who could misuse it

- **The child getting around it.** Covered: every connecting path the child can start (section 7.2); what the core would do on its own, on every profile, hidden ones included, and in an imported database (section 7.1); connections started before the controls, on profiles the app can open (section 7.3); the chat console, database import and migration, including the "Migrate" offered by onboarding after the database is deleted, and "Use from desktop" (all off, section 8). Not covered: section 3.
- **The parent sliding into watching.** The PIN opens only the parental screen and allow-once (principle 2). App lock, self-destruct and profiles, including hidden ones, stay the child's *(provisional, P5)*. Allow-once shows the parent only the name of the one connection being approved, which the child is asking for. The clean-up of section 7.3 lists nothing by name. Turning the controls off to export the chats removes the indicator, so it can't be done unseen.
- **One adult imposing it on another.** Someone who keeps the PIN can stop an adult reaching anyone new. Safeguards: the indicator is always visible; deleting the app or its data always removes the controls, because the app does not make them survive a reinstall *(provisional, P9)*; nothing is controlled remotely; the PIN opens nothing of the chats.
- **Strangers contacting the child.** They must not learn the phone is restricted, since that would mark a child. Section 9 lists what a contact can see; none of it is specific to these controls.

## 5. Stored state

- **Switches:** the set of capabilities switched off, from `enum ParentalCapability { connect, groups, calls, sendFiles, voice, receiveMedia }`; an empty set means controls are off. iOS: a group default in `apps/ios/SimpleXChat/AppGroup.swift` (alongside `GROUP_DEFAULT_ALLOW_SHARE_EXTENSION`, line 27), so the notification and share extensions read it. Kotlin: an app preference in `AppPreferences` (`model/SimpleXAPI.kt`).
- **Parent PIN:** stored like the app passcode. iOS: a new keychain item `kcParentPassword` in `apps/ios/SimpleXChat/KeyChain.swift` (same access policy, `kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly`, line 12). Kotlin: a new `KeyStoreItem` in `views/helpers/DatabaseUtils.kt` (lines 17-19 hold the existing three).
- **Kept out of the settings archive.** Importing a database or migrating to a device also imports the app settings packed with it (iOS `apps/ios/Shared/Model/SimpleXAPI.swift:2309-2317`, `apps/ios/Shared/Views/UserSettings/AppSettings.swift:14`; Kotlin `AppSettings`, `model/SimpleXAPI.kt:8711`). The switches and the PIN are not part of `AppSettings`.
- **Outside the chat database.** Deleting the database and the self-destruct passcode leave them in place (iOS `apps/ios/Shared/Views/LocalAuth/LocalAuthView.swift:65-79`; Kotlin `views/localauth/LocalAuthView.kt:57-75` and `views/database/DatabaseView.kt:556-578` remove only database files and their own preferences). Deleting the app or its data removes them.
- **Reinstall on iOS:** iOS can keep keychain items after the app is deleted; the app removes its passcodes on the first launch without a database (`apps/ios/Shared/AppDelegate.swift:111-122`). That clean-up runs only when the app or self-destruct passcode exists (line 116); its condition gains `kcParentPassword`, and it removes it, so a reinstalled app never asks for a PIN nobody remembers.
- **The core switch** (section 7.1) is held in memory and sent by the app at every chat start; nothing is stored in the chat database.

## 6. The parent gate and the parental screen

**Setting the PIN** reuses `SetAppPasscodeView` with `passcodeKeychain: kcParentPassword` (iOS `apps/ios/Shared/Views/LocalAuth/SetAppPasscodeView.swift:13-18`; Kotlin `views/localauth/SetAppPasscodeView.kt:14-15`). The PIN must not be compared with the app passcode or the self-destruct passcode, in either direction: rejecting a matching PIN would tell the parent the child's passcode by trial, and the reverse. The view's `prohibitedPasscodeKeychain` defaults to the self-destruct item (line 15 on both platforms), so it becomes optional and the parental use passes none.

**Asking for the PIN** reuses the PIN pad: a new `authenticateParent(reason:completed:)` sets `ChatModel.laRequest` with the parent PIN as `password` and `selfDestruct: false` (iOS `LocalAuthRequest`, `apps/ios/Shared/Views/Helpers/LocalAuthenticationUtils.swift:29-35`, checked in `LocalAuthView.swift:31`; Kotlin `views/localauth/LocalAuthView.kt:26-43`). It never uses system authentication, which the child can pass. It does not reuse `authenticate` or `authorize`: those follow the app-lock mode and treat "unavailable" as success (`LocalAuthenticationUtils.swift:19-26,39-58`). No PIN stored means the gate stays shut.

**The parental screen** is a new row in the privacy settings (iOS `apps/ios/Shared/Views/UserSettings/PrivacySettings.swift`, shown as "Your privacy"; Kotlin `views/usersettings/PrivacySettings.kt`, "Privacy & security"). Opening it asks for the PIN once controls are on. In order:
1. What this does and doesn't do (section 3), and, on iOS, how to stop app deletion in Screen Time *(provisional, P9)*.
2. Set or change the parent PIN.
3. The switches, with the defaults of section 1.
4. When connecting or groups is switched off: one confirmation for the clean-up of section 7.3, and a note that groups and channels the child is already in stay *(provisional, P15)*.
5. "Turn off parental controls": PIN, then the switches clear and the core switch is cleared. Links and pending connections removed by the clean-up are not restored; the child makes new ones as needed.

**The indicator:** while any switch is off, a line "Parental controls on" at the top of the chat list (iOS `apps/ios/Shared/Views/ChatList/ChatListView.swift`; Kotlin `views/chatlist/ChatListView.kt`) and on the Settings screen, opening the parental screen *(provisional, P7)*.

## 7. The controls

Each control lists where its check runs, on both platforms, and the screens where its buttons are hidden. A check that fails throws a new error, "Turned off by parental controls", which the existing error alerts show; for connecting, the alert offers "Allow once" (section 7.2).

### 7.1 The core switch: what the core does on its own

Some connections, calls and files happen in the chat core with no app call: auto-accepting on an address, a business address, member contacts and group invitations; ringing for an incoming call; receiving small inline files. The app can't reach them on profiles other than the active one: address settings and the profile act only on the active user (`withUserId` → `checkSameUser`, `src/Simplex/Chat/Library/Internal.hs:3451-3457`), and the auto-accept setters need a hidden profile's password (`src/Simplex/Chat/Library/Commands.hs:541-551`). And the calls preference doesn't reach incognito contacts (`src/Simplex/Chat/Types.hs:695-698`) or contacts with their own calls setting (`src/Simplex/Chat/Types/Preferences.hs:1078-1080`).

So the core gets a switch *(provisional, P1)*: a new local API command that sets which of `connect`, `groups`, `calls`, `receiveMedia` are off, held in memory like `APISetEncryptLocalFiles` (`Commands.hs:659`, `chatWriteVar encryptLocalFiles`). The app sends it at every chat start and whenever the switches change: in the main app, in the notification extension and in the share extension (as the share extension sends `apiSetEncryptLocalFiles`, `apps/ios/SimpleX SE/ShareAPI.swift:44-47`). While it is set:

| Off | The core | Where |
|---|---|---|
| `connect` | does not auto-accept requests to an address, including a business address; they wait as with auto-accept off | `src/Simplex/Chat/Library/Subscriber.hs:1535-1543` |
| `connect` | does not auto-accept a group member's direct-contact request | `Subscriber.hs:3944-3950` (`autoAcceptMemberContacts`) |
| `groups` | does not auto-accept group invitations | `Subscriber.hs:2723-2727`, `3907-3910` (`autoAcceptGroupInvitations`) |
| `calls` | turns every incoming call into the existing local "feature rejected" item, whatever the preferences; it never rings | `Subscriber.hs:3051-3057,3078-3081` |
| `receiveMedia` | does not receive inline files on arrival | `receiveInlineMode`, `Subscriber.hs:2545-2551` |

It covers every profile, hidden ones included, the notification extension's core, and a database imported later. Nothing is sent to anyone and nothing is stored; the address and preference settings themselves are untouched, so turning the controls off brings them back as they were. Cost: one command, one variable, the five checks above, and core tests for each. A join to the child's own group link (`Subscriber.hs:2721-2722`) is a join the child started and is left to section 7.3.

### 7.2 Connecting to new people (`connect`)

Off: no new contact can be added from this phone, in either direction.

Checks (iOS `apps/ios/Shared/Model/SimpleXAPI.swift`; Kotlin `model/SimpleXAPI.kt`):

| Action | iOS | Kotlin |
|---|---|---|
| Plan a link (first step of every link path; the alert comes before the preview). Not the local name lookup while typing, which plans with `resolveMode` never and connects nothing (iOS `apps/ios/Shared/Views/ChatList/ChatListView.swift:865`, Kotlin `views/chatlist/ChatListView.kt:889`) | `apiConnectPlan` 1043 | `apiConnectPlan` 1681 |
| Connect via a link | `apiConnect` 1058 | `apiConnect` 1691 |
| Connect a prepared contact (short links) | `apiConnectPreparedContact` 1231 | `apiConnectPreparedContact` 1864 |
| Connect a prepared business chat (it is a group, `src/Simplex/Chat/Library/Commands.hs:2200-2214`): counted as `connect`, not `groups` | `apiConnectPreparedGroup` 1238 | `apiConnectPreparedGroup` 1874 |
| Connect via a contact's address card | `apiConnectContactViaAddress` 1245 | `apiConnectContactViaAddress` 1884 |
| Create a one-time invitation | `apiAddContact` 1020 | `apiAddContact` 1651 |
| Create an address | `apiCreateUserAddress` 1463 | `apiCreateUserAddress` 2069 |
| Accept a contact request | `apiAcceptContactRequest` 1516 | `apiAcceptContactRequest` 2132 |
| Contact a group member directly | `apiCreateMemberContact` 2129, `apiSendMemberContactInvitation` 2135 | `apiCreateMemberContact` 2636, `apiSendMemberContactInvitation` 2645 |
| Accept a member's direct-contact invitation | `apiAcceptMemberContact` 2141 | `apiAcceptMemberContact` 2654 |

A link is checked against `connect` or `groups` by what its plan says it is; a link the app couldn't plan counts as both. Auto-accept settings can still be changed; while connecting is off the core ignores them (section 7.1).

Buttons hidden:
- iOS (`apps/ios/Shared/Views/`): the new-chat sheet's "Create 1-time link", "Scan / Paste link", paste field and "Connect to @name" rows (`NewChat/NewChatMenuButton.swift:96-129,355-406`); the chat-list search's paste and name rows (`ChatList/ChatListView.swift:751-918`); the connect cards after onboarding and the banner (`NewChat/OnboardingCards.swift:150-302`, `Onboarding/ConnectBannerCard.swift:33-58`); creating an address (`UserSettings/UserAddressView.swift:295-336`); "Accept" on contact requests in the chat list, the contacts list, the chat and the notification (`ChatList/ChatListNavLink.swift:134-153,495-527`, `Contacts/ContactListNavLink.swift:78-106,281-314`, `Chat/ComposeMessage/ContextContactRequestActionsView.swift:21-35`, `Model/NtfManager.swift:64-70`); contact cards (`ChatList/ChatListNavLink.swift:97-119`, `Contacts/ContactListNavLink.swift:217-261`); direct contact with a group member (`Chat/Group/GroupMemberInfoView.swift:364-377,420-430`, `Chat/ComposeMessage/ContextMemberContactActionsView.swift:30-40`).
- Kotlin: the new-chat sheet's "1-time link", "Scan / paste link", paste field and name rows (`views/newchat/NewChatSheet.kt:57-62,188-197,322,413,539-554`); the chat-list banner, search paste and name rows (`views/chatlist/ChatListView.kt:384-419,868-874,1236-1262`); the connect cards (`views/newchat/OnboardingCards.kt:376-417`); creating an address (`views/usersettings/UserAddressView.kt:74-121,310`); "Accept" on contact requests in the chat list, the chat, the contacts list and the Android notification (`views/chatlist/ChatListNavLinkView.kt:132-135,279-283,527-557,698-728`, `views/chat/ComposeContextContactRequestActionsView.kt:69-96,136-165`, `views/contacts/ContactListNavView.kt:78-130`, `apps/multiplatform/android/src/main/java/chat/simplex/app/model/NtfManager.android.kt:130-141`); contact cards (`views/chatlist/ChatListNavLinkView.kt:824-899`, `views/contacts/ContactListNavView.kt:62-70`); direct contact with a group member (`views/chat/group/GroupMemberInfoView.kt:518-522`, `views/chat/ComposeContextGroupDirectInvitationActionsView.kt:100,151-171`).
- Links in messages, in the app's own help and news rows, and opened from outside the app stay tappable: they reach the check and get the "Allow once" alert. Every link path on both platforms passes `planAndConnect` (iOS `apps/ios/Shared/Views/NewChat/NewChatView.swift:1319`; Kotlin `views/newchat/ConnectPlan.kt:24`) before any connecting call.

**Allow once.** The alert offers "Allow once". The parent enters the PIN and sees what is about to happen: the name from the link's plan, the name on the request, or the member's name. The app then continues with a one-time pass held in memory and never stored, bound to that link, request or member:
- a short link connects in two steps, the plan and later the connect from the prepared chat's composer (iOS `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift:1195,1212`; Kotlin `views/chat/ComposeView.kt:657,698`), so the pass approved at the plan is carried to the prepared contact or business chat and used by its connect;
- contacting a group member is two calls (create, then send the invitation); one pass covers both;
- creating a one-time link or an address offers no allow-once, since there is no one to show the parent: the child scans the other person's link instead, or the parent switches connecting on and off.

Only connecting has this exception; any other switch is changed on the parental screen *(provisional, P14)*.

### 7.3 Connections started before the controls

Connections the child started before the controls complete in the core when the other side answers: an unused one-time link, a request sent to an address and accepted later, a prepared chat, a group being joined (`Subscriber.hs:484-518,619-631`; own group link `2721-2722`). Blocking these in the core would also block allowed-once connections, which complete the same way.

So when connecting or groups is switched off, after one confirmation, the app deletes *(provisional, P2)*:
1. with connecting off: unused one-time links, requests the child sent that are still pending, and prepared chats not yet connected;
2. with groups off: groups the child is still joining;
3. with either off: every group link the child created, at any role (`apiCreateGroupLink` needs an admin, `Commands.hs:3362-3364`).

It does this on every profile it can open, switching the active profile and back for the visible ones (address and chat commands act on the active user, section 7.1). A hidden profile can't be opened without its password, so its pending connections stay (section 3). Nothing is listed by name, so the parent learns nothing about the child's profiles or groups.

### 7.4 Groups and channels (`groups`)

Off: the child can't create or join groups or channels, create group links, or admit members waiting for review. Existing groups keep working, including adding the child's existing contacts to them, and they can gain members through other members' links *(provisional, P15)*.

| Action | iOS | Kotlin |
|---|---|---|
| Create a group | `apiNewGroup` 1949 | `apiNewGroup` 2415 |
| Create a channel | `apiNewPublicGroup` 1961 | `apiNewPublicGroup` 2428 |
| Accept a group invitation | `apiJoinGroup` 2007 | `apiJoinGroup` 2465 |
| Join via a group link (short links; a business chat counts as `connect`, section 7.2) | `apiConnectPreparedGroup` 1238 | `apiConnectPreparedGroup` 1874 |
| Join via a link (older links) | `apiConnect` 1058, by the plan | `apiConnect` 1691, by the plan |
| Create a group link | `apiCreateGroupLink` 2090 | `apiCreateGroupLink` 2585 |
| Admit a member waiting for review | `apiAcceptMember` 2017 | `apiAcceptMember` 2491 |

Group invitations are not auto-accepted while groups are off (section 7.1).

Buttons hidden:
- iOS (`apps/ios/Shared/Views/`): "Create group" and "Create public channel" (`NewChat/NewChatMenuButton.swift:130-145`); "Join" on group invitations in the chat list and the chat (`ChatList/ChatListNavLink.swift:217-236,320-330`, `Chat/ChatItem/CIGroupInvitationView.swift:95-101`); "Join group" / "Join channel" in a prepared group (`Chat/ComposeMessage/ComposeView.swift:486-496`); creating a group link (`Chat/Group/GroupLinkView.swift:122-125,164-195`).
- Kotlin: "Create group" and "Create channel" (`views/newchat/NewChatSheet.kt:63-68,198-205`); joining from an invitation in the chat list and the chat (`views/chatlist/ChatListNavLinkView.kt:86,196-201,314-316,488-511,902-919`, `views/chat/item/CIGroupInvitationView.kt:90-93`); "Join group" / "Join channel" in a prepared group (`views/chat/ComposeView.kt:1780-1787`); creating a group link (`views/chat/group/GroupLinkView.kt:45-56,119-123,213`).

### 7.5 Calls (`calls`)

Off: no calls in or out.

Incoming calls are turned away by the core switch (section 7.1) *(provisional, P3)*: no ringing in the app, on the iPhone call screen, through the notification extension or on Android's call screen, for every contact, incognito ones included. Rejecting sends nothing (`APIRejectCall`, `Commands.hs:1541-1552`; the local item at `Subscriber.hs:3078-3081`), so the caller sees a call nobody answers. No preference changes.

Outgoing and answering, checked in the app:

| Action | iOS | Kotlin |
|---|---|---|
| Start a call: checked before CallKit and the call view start, since a refusal at the API leaves the call screen hanging (`apps/ios/Shared/Views/Call/ActiveCallView.swift:121-131`) | `CallController.startCall`, `apps/ios/Shared/Views/Call/CallController.swift:351`, and `apiSendCallInvitation` 1834 | `startChatCall`, `views/chat/ChatView.kt:911`, and `apiSendCallInvitation` 2172 |
| Answer a call (an invitation received before calls were switched off) | `apiSendCallOffer` 1842 | `apiSendCallOffer` 2182 |

Buttons hidden:
- iOS (`apps/ios/Shared/Views/`): the chat toolbar's call menu (`Chat/ChatView.swift:562-583`), contact info's call buttons (`Chat/ChatInfoView.swift:160-161`), member info's call buttons (`Chat/Group/GroupMemberInfoView.swift:370-371`). Calls started by Siri or from the phone's recents (`apps/ios/Shared/ContentView.swift:325-355`) reach `CallController.startCall` and are refused there.
- Kotlin: the chat toolbar's call button and menu (`views/chat/ChatView.kt:1296-1300,1552-1561`), contact info's call buttons (`views/chat/ChatInfoView.kt:576-577`), member info's call buttons (`views/chat/group/GroupMemberInfoView.kt:512-513`).

### 7.6 Sending files and voice messages (`sendFiles`, `voice`)

Off: the composer offers no attachments (photos, camera, files) or no voice button; forwarding a message with a file is refused; the share extension does not send files.

| Action | iOS | Kotlin |
|---|---|---|
| Send messages; refused when any carries a file (`sendFiles`) or is voice (`voice`) | `apiSendMessages` 559 | `apiSendMessages` 1253 |
| Forward messages; refused when any carries a file | `apiForwardChatItems` 526 | `apiForwardChatItems` 1312 |

Share extension (iOS): it starts its own core and sends through its own `apiSendMessages` (`apps/ios/SimpleX SE/ShareAPI.swift:57-85`, called at `apps/ios/SimpleX SE/ShareModel.swift:149`), so the check runs there too: with `sendFiles` off it refuses images, videos and files and still shares text and links. It reads the switches from group defaults, as it reads "Allow sharing" today (`ShareModel.swift:77-80`).

Buttons hidden:
- iOS (`apps/ios/Shared/Views/Chat/`): the attachment button and its menu (`ComposeMessage/ComposeView.swift:1127-1147`, items 608-689), pasting images into the text field (`ComposeMessage/NativeTextEditor.swift:186-256`), the voice button (`ComposeMessage/SendMessageView.swift:120-128`). Forwarding stays available for text and is refused for files at `apiForwardChatItems`.
- Kotlin: the attachment button (`views/chat/ComposeView.kt:1311-1345`), the voice button (`views/chat/SendMsgView.kt:137-155`), images inserted from the Android keyboard (`apps/multiplatform/common/src/androidMain/kotlin/chat/simplex/common/platform/PlatformTextField.android.kt:108-120`). Sharing into the app from other Android apps (`apps/multiplatform/android/src/main/java/chat/simplex/app/MainActivity.kt:157-209`) ends in `apiSendMessages` and is refused there for files.

### 7.7 Receiving images, videos and files (`receiveMedia`)

Off: nothing is downloaded or opened, and image, video and link previews are replaced by a plain placeholder *(provisional, P4)*, because image and video messages carry their own preview inside the message (`MCImage`, `MCVideo`, `src/Simplex/Chat/Protocol.hs:747-748`), and link messages carry a picture the sender chose (`MCLink`, `LinkPreview`, `Protocol.hs:746`). Notifications carry no message images on either platform, so they need nothing. The core stops receiving inline files (section 7.1).

| Action | iOS | Kotlin |
|---|---|---|
| Receive a file (manual and automatic) | `receiveFile` 1598, `receiveFiles` 1607 | `receiveFile` 2385, `receiveFiles` 2269 |
| Decide what to auto-receive (main app and notification extension) | `ChatItem.autoReceiveFile()`, `apps/ios/SimpleXChat/ChatTypes.swift:4003-4013`, used by the notification extension at `apps/ios/SimpleX NSE/NotificationService.swift:1031` | the new-message handler decides and calls `receiveFile` (checked), `model/SimpleXAPI.kt:3070-3079`; Android has no separate extension |

Previews replaced and download buttons hidden:
- iOS (`apps/ios/Shared/Views/`): image, video, voice and file items (`Chat/ChatItem/CIImageView.swift:50-60`, `Chat/ChatItem/CIVideoView.swift:70,101`, `Chat/ChatItem/CIVoiceView.swift:302-316`, `Chat/ChatItem/CIFileView.swift:88-100`), link previews (`Chat/ChatItem/CILinkView.swift:22-23`), quoted media (`Chat/ChatItem/FramedItemView.swift:263-276`), the compose bar's media preview of an item being forwarded or edited (`Chat/ComposeMessage/ComposeView.swift:285-287`), the "Download" actions (`Chat/ChatView.swift:2328-2329,1597-1608`), and media previews in the chat list (`ChatList/ChatPreviewView.swift:427-453`).
- Kotlin: image, video, file and voice items (`views/chat/item/CIImageView.kt:225-236`, `views/chat/item/CIVideoView.kt:102,132`, `views/chat/item/CIFileView.kt:76-86`, `views/chat/item/CIVoiceView.kt:424-428`), link previews and quoted media (`views/chat/item/FramedItemView.kt`), the compose bar's context item (`views/chat/ContextItemView.kt`), the "Download" menu item (`views/chat/item/ChatItemView.kt:458-467`), media previews in the chat list (`views/chatlist/ChatPreviewView.kt:335-353`). Android's background service receives through the same `receiveFile` and is covered by its check.

Received files the child already has stay as they are.

## 8. Settings while controls are on

**Need the PIN:** only the parental controls screen.

**Off entirely, even for the parent** *(provisional, P5)*, because each would hand the chats to whoever uses it or skip every check:

| Setting | Why | iOS | Kotlin |
|---|---|---|---|
| Database export, import and migration to or from another device | Export and migration copy the whole database out; with the default random passphrase the app sets a new passphrase without asking for the old one (`apps/ios/Shared/Views/Database/DatabaseView.swift:209-220`, `DatabaseEncryptionView.swift:86,139`; Kotlin `views/database/DatabaseEncryptionView.kt:52`). Import and migration bring in contacts made elsewhere. | `apps/ios/Shared/Views/Database/DatabaseView.swift:210-227`; migration from the device, `apps/ios/Shared/Views/UserSettings/SettingsView.swift:352-364` → `apps/ios/Shared/Views/Migration/MigrateFromDevice.swift`; migration to the device in onboarding, `apps/ios/Shared/Views/Onboarding/CreateProfile.swift:282-295` and `apps/ios/Shared/Views/Onboarding/SimpleXInfo.swift:91-104` | `views/database/DatabaseView.kt:247-272`; `views/usersettings/SettingsView.kt:106` → `views/migration/MigrateFromDevice.kt`; onboarding `views/onboarding/SimpleXInfo.kt:98,169`, `views/WelcomeView.kt:201-225` |
| Developer tools, with the chat console | The console sends any command but `/sql`, past every check in the app, and reads the active profile's chats | Settings → version row (`apps/ios/Shared/Views/UserSettings/SettingsView.swift:414-421`) → `VersionView.swift:30-36` → `DeveloperView.swift:35-39` → `apps/ios/Shared/Views/TerminalView.swift:147-160` | `views/usersettings/VersionInfoView.kt:52` (the advanced settings section, per platform) → `views/usersettings/DeveloperView.kt:21,32` → `views/TerminalView.kt:59` |
| "Use from desktop" | A linked computer runs the phone's chats past every check (`src/Simplex/Chat/Remote.hs:508-556`); a desktop paired earlier reconnects by itself | `apps/ios/Shared/Views/ChatList/UserPicker.swift:73`; `connectRemoteCtrl` 1740, `findKnownRemoteCtrl` 1747, `confirmRemoteCtrl` 1751 refused; `stopRemoteCtrl` 1769 when the first switch goes off | Android only: `views/chatlist/UserPicker.kt:378-389`; `connectRemoteCtrl` 2727, `findKnownRemoteCtrl` 2739, `confirmRemoteCtrl` 2741 refused; `stopRemoteCtrl` 2767 |

Deleting the database stays available: the switches survive it (section 5), and onboarding's "Migrate" is off while controls are on.

Left to the child: app lock and its passcode, self-destruct, chat profiles including hidden ones, auto-accept settings (ignored by the core while the matching switch is off, section 7.1), preferences, network and servers, call settings, appearance, notifications. None of them gets around a switch; locking app lock or hidden profiles would let the person holding the PIN open the child's chats.

## 9. What contacts see

- A contact request sent to the child stays pending, as with anyone who doesn't accept.
- A file the child can't receive stays not received, as with anyone who doesn't open it.
- A call to the child is never answered: the caller sees "calling…" and then a missed call, as with anyone who doesn't pick up.
- Nothing else: no control sends a message or changes the profile or any preference.

## 10. Onboarding

No onboarding step *(provisional, P8)*. A question about children asked of every new user puts parental controls in front of adults who have none, which one commenter in the issue objects to; a parent sets the controls up in Settings before handing the phone over. The website's FAQ points to the setting.

Onboarding today (since #6888, 2026-04-27) is: introduction, first profile, "Your network" (routers and notifications), conditions (iOS `apps/ios/Shared/Views/Onboarding/OnboardingView.swift:18-57`; Kotlin `views/onboarding/OnboardingView.kt:3-13`). Making connections moved out of onboarding into cards at the top of the chat list (#6753; iOS `apps/ios/Shared/Views/NewChat/OnboardingCards.swift:135-308`, Kotlin `views/newchat/OnboardingCards.kt:332`). With connecting off, those cards are hidden (section 7.2); with any switch off, onboarding's "Migrate" is off (section 8).

## 11. Not built

- Forwarding or showing the child's messages, contacts or groups to anyone.
- Preventing the child from deleting messages or chats, or forcing disappearing messages off *(provisional, P13)*: they exist only to keep history for reading.
- An "incognito off" switch *(provisional, P6)*: it doesn't stop anyone reaching the child; it only makes the child show their real name and photo to everyone they connect with.
- Making the controls survive deleting the app *(provisional, P9)*. On iOS the keychain can outlive the app, but the app removes its keychain passcodes after a reinstall on purpose (`apps/ios/Shared/AppDelegate.swift:111-122`, #3841), and Apple's engineers call keychain survival undocumented (https://developer.apple.com/forums/thread/36442). On Android, Block Store keeps up to 16 small entries across a reinstall when the user's backup is on, in builds with Google Play services (https://developer.android.com/identity/block-store), so not in the F-Droid build. Either would take away the simple way out for an adult on whom the controls were imposed; on iOS, Screen Time does it with the parent's own passcode.
- Desktop *(provisional, P10)*: the desktop app stores passcodes unencrypted (`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt:3-17`) in `settings.properties` (`platform/Resources.desktop.kt:40,64`), so every switch could be edited and the PIN read. The parental screen is hidden on desktop.
- Approval from the parent's own phone. iOS 26's PermissionKit can send a child's request to the parent through Messages, but it shows the new contact's name to Apple's Messages, and Apple asks apps to use it only when they know the user is a child (WWDC25 session 293, https://developer.apple.com/videos/play/wwdc2025/293/) *(provisional, P12)*.
- Time limits and app blocking: Screen Time and Family Link do these.
- Checking every command in the core, so the console and every client are covered without locking them: the next step after the core switch of section 7.1, if the app checks prove too easy to get around *(provisional, P1)*.

## 12. Build order

Each step ships on its own, iOS first, then Kotlin for Android.

1. The core switch (7.1) with its tests; stored state, the parent PIN, the parent gate, the parental screen, the indicator, and the settings that are off (8).
2. Connecting: the checks and allow-once of 7.2, and the clean-up of 7.3.
3. Groups and channels (7.4).
4. Calls (7.5), sending files and voice (7.6), receiving media (7.7).

## 13. Open choices

The provisional answers above are the recommended options of the decision page; each Pn there has the options, the costs and the evidence. Until the user answers, the recommendations stand.
