# Parental controls: design

Platforms: iOS first, then the same screens in Kotlin for Android. Not on desktop *(provisional, P10)*. No protocol, message or database format changes; one new local API command in the chat core (section 7.1).

Answers marked *(provisional, Pn)* wait for a decision (section 13); Pn names the choice on the decision page. Code references are to master at 85ba221d7, except directory search (section 7.8), which is cited at e90838ea9, the head commit of pull request #7881. Kotlin paths that start with `views/`, `model/` or `platform/` are under `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.

## 1. What it does

A parent setting up a phone for a child opens the privacy settings ("Your privacy" on both platforms) → Parental controls, sets a parent PIN and chooses what to switch off. By default that is connecting to new people, creating or joining groups and channels, and searching SimpleX Directory; calls, sending files, voice messages, and receiving images, videos and files can be switched off too, while contacts the parent marks as always allowed, such as the parents, keep them. Then the parent hands the phone over.

The child keeps chatting with everyone already in their chats. Settings shows "Parental controls on". When the child tries to connect to someone new, by scanning a code, opening or pasting a link, or accepting a request, or to join a group or channel, the app says this is turned off by parental controls and offers "Allow once": the parent enters the PIN, sees the name of the contact or group, and that one connection goes through. Everything else that is off stays off until the parent changes it on the PIN-protected screen.

Nothing in this feature reads, keeps, shows or reports the child's messages, contacts or groups, to the parent or to anyone.

This answers issue #4482's second request (a parental PIN and approval before adding contacts). Its first request, forwarding the child's messages to a parent, is monitoring and is not built.

![How the parental controls work](2026-05-27-parental-controls-design/how-it-works.svg)

## 2. Principles

1. **Switch off, never watch.** A control removes a capability. No control keeps history, lists contacts or groups to the parent, or sends anything off the device.
2. **The PIN never opens a path to the chats.** It opens only the parental screen and allow-once. Anything that would hand over the chats (database export and migration, the chat console, "Use from desktop") is off while controls are on, even for the parent; to use it, the parent turns the controls off, which removes the indicator from Settings *(provisional, P5)*.
3. **Visible on the phone, invisible to contacts.** Settings shows that controls are on, and an action the controls refuse says "Turned off by parental controls". No control sends a message or changes the profile.
4. **Check every action where it goes to the core.** Each action the child starts is checked in the app's function that sends it to the chat core, so every button that reaches it is covered, including buttons added later. What the core does on its own (auto-accepting, ringing for a call, receiving inline files) is stopped by a parental mode in the core. Buttons are also hidden or disabled, for a clean screen; that is presentation, not enforcement.
5. **Fail closed.** A missing or unreadable PIN never opens the gate; an action the check can't classify counts as restricted.

## 3. Limits

The child holds the phone, so a determined, technically able child gets around the controls. The setup screen says in one sentence that the controls stop casual use and state the parent's intent; they are not a lock.

- **Reinstalling the app or clearing its data removes the controls.** On iPhone, Screen Time can stop the app being deleted (Settings → Screen Time → Content & Privacy Restrictions → iTunes & App Store Purchases → Deleting Apps, https://support.apple.com/en-gb/126807); the setup screen says how. On Android, Family Link approves new downloads but not an app it approved before (https://support.google.com/googleplay/answer/7103028: previously approved apps "will not require additional approval to download"), so reinstalling stays open there.
- **A factory reset, "Erase All Content and Settings", watching the parent type the PIN, or another device.**
- **One-off connections the child started on a hidden profile** before the controls (an unused one-time link, a request sent, a group being joined) can still complete: the app can't open a hidden profile without its password (section 7.3). Addresses and links of groups the child hosts, on hidden profiles too, are covered by the parental mode.
- **A channel the child owns keeps admitting subscribers through its relays**, since their joins never reach the child's core (`src/Simplex/Chat/Library/Subscriber.hs:1637-1638,1695-1717`), and a subscriber can write to the channel's admins (`src/Simplex/Chat/Library/Commands.hs:4964-4968`; that this reaches the child in the app is not traced). The parental screen says so when groups is switched off.
- **Groups and channels the child is already in stay, and can gain new members** through other members' links.

## 4. Who could misuse it

- **The child getting around it.** Covered: every connecting path the child can start (section 7.2); what the core would do on its own, on every profile, hidden ones included, and in an imported database (section 7.1); connections started before the controls, on profiles the app can open (section 7.3); the chat console, database import and migration, including the "Migrate" offered by onboarding after the database is deleted, and "Use from desktop" (all off, section 8). Not covered: section 3.
- **The parent sliding into watching.** The PIN opens only the parental screen and allow-once (principle 2), so it gives no access to the chats beyond holding the unlocked app, which shows them anyway. App lock, self-destruct and profiles, including hidden ones, stay the child's *(provisional, P5)*, so the child decides who can open the app. Allow-once shows the parent only the name of the one contact or group being approved, which the child is asking for. Always-allowed contacts are added one at a time on that contact's page (section 7.9), so the parent sees only the contacts the child shows them. The clean-up of section 7.3 lists nothing by name.
- **One adult imposing it on another.** Someone who keeps the PIN can stop an adult reaching anyone new. The chat list shows nothing, so the adult learns of the controls from the "Parental controls on" row at the top of Settings, from the alert "Turned off by parental controls" on every action the checks refuse, from the disabled attachment and voice buttons, which say the same when tapped, from the placeholders that replace pictures, and from the "Audio/video calls: received, prohibited" item that each turned-away call leaves in the chat (`apps/ios/SimpleXChat/ChatTypes.swift:4778`). Hidden buttons give no sign, so an adult who doesn't open Settings and doesn't try a refused action may not notice the controls for some time. Safeguards: deleting the app or its data always removes the controls, because the app does not make them survive a reinstall; nothing is controlled remotely; the PIN opens nothing of the chats.
- **Strangers contacting the child.** They must not learn the phone is restricted, since that would mark a child. Section 9 lists what a contact can see; none of it is specific to these controls.

## 5. Stored state

- **Switches:** the set of capabilities switched off, from `enum ParentalCapability { connect, groups, directorySearch, calls, sendFiles, voice, receiveMedia }`; an empty set means controls are off. iOS: a group default in `apps/ios/SimpleXChat/AppGroup.swift` (alongside `GROUP_DEFAULT_ALLOW_SHARE_EXTENSION`, line 27), so the notification and share extensions read it. Kotlin: an app preference in `AppPreferences` (`model/SimpleXAPI.kt`).
- **Parent PIN:** stored like the app passcode. iOS: a new keychain item `kcParentPassword` in `apps/ios/SimpleXChat/KeyChain.swift` (same access policy, `kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly`, line 12). Kotlin: a new `KeyStoreItem` in `views/helpers/DatabaseUtils.kt` (lines 17-19 hold the existing three).
- **Always-allowed contacts:** the connection IDs (`agentConnId` of the contact's `activeConn`; iOS `apps/ios/SimpleXChat/ChatTypes.swift:2675`, core `src/Simplex/Chat/Types.hs:1945`) of the contacts the parent marked, stored beside the switches in the same group default and app preference (section 7.9). A connection ID is random, so an entry left from a deleted database never matches a contact of a new one, and an entry that matches no contact allows nothing (principle 5). Turning the controls off clears it.
- **Kept out of the settings archive.** Importing a database or migrating to a device also imports the app settings packed with it (iOS `apps/ios/Shared/Model/SimpleXAPI.swift:2309-2317`, `apps/ios/Shared/Views/UserSettings/AppSettings.swift:14`; Kotlin `AppSettings`, `model/SimpleXAPI.kt:8711`). The switches, the always-allowed contacts and the PIN are not part of `AppSettings`.
- **Outside the chat database.** Deleting the database and the self-destruct passcode leave them in place (iOS `apps/ios/Shared/Views/LocalAuth/LocalAuthView.swift:65-79`; Kotlin `views/localauth/LocalAuthView.kt:57-75` and `views/database/DatabaseView.kt:556-578` remove only database files and their own preferences). Deleting the app or its data removes them.
- **Reinstall on iOS:** iOS can keep keychain items after the app is deleted, while the app-group container that holds the switches goes with the app. So on launch, if `kcParentPassword` exists and no switches are stored, the app removes it; a reinstalled app never asks for a PIN nobody remembers. The existing clean-up (`apps/ios/Shared/AppDelegate.swift:111-122`) stays as it is: its test, "no app-lock flag and no database", also passes after an in-app "Delete database", so adding the PIN to it would delete the PIN while the switches stay, leaving the controls stuck on. (That the app-group container goes on uninstall is iOS behaviour to confirm once on a device.)
- **The parental mode** (section 7.1) is held in memory and sent by the app at every chat start; nothing is stored in the chat database.

## 6. The parent gate and the parental screen

**Setting the PIN** reuses `SetAppPasscodeView` with `passcodeKeychain: kcParentPassword` (iOS `apps/ios/Shared/Views/LocalAuth/SetAppPasscodeView.swift:13-18`; Kotlin `views/localauth/SetAppPasscodeView.kt:14-15`). The PIN must not be compared with the app passcode or the self-destruct passcode, in either direction: rejecting a matching PIN would tell the parent the child's passcode by trial, and the reverse. The view's `prohibitedPasscodeKeychain` defaults to the self-destruct item (line 15 on both platforms), so it becomes optional and the parental use passes none. Two more parameters get today's values as defaults: the confirming step's title, "Confirm Passcode" today (iOS `SetAppPasscodeView.swift:30`, Kotlin `SetAppPasscodeView.kt:40`), which the parental use sets to "Confirm parent PIN"; and the shortest code accepted, 4 digits today (iOS `PasscodeView.swift:80`, Kotlin `views/localauth/PasscodeView.kt:81,123`), which is 6 for the parent PIN, so guessing it means trying up to a million codes rather than ten thousand.

**Asking for the PIN** reuses the PIN pad: a new `authenticateParent(reason:completed:)` sets `ChatModel.laRequest` with the parent PIN as `password` and `selfDestruct: false` (iOS `LocalAuthRequest`, `apps/ios/Shared/Views/Helpers/LocalAuthenticationUtils.swift:29-35`, checked in `LocalAuthView.swift:31`; Kotlin `views/localauth/LocalAuthView.kt:26-43`). It never uses system authentication, which the child can pass. It does not reuse `authenticate` or `authorize`: those follow the app-lock mode and treat "unavailable" as success (`LocalAuthenticationUtils.swift:19-26,39-58`). No PIN stored means the gate stays shut.

**The parental screen** is a new row in the privacy settings, "Your privacy" on both platforms (iOS `apps/ios/Shared/Views/UserSettings/PrivacySettings.swift:57-76`, the "Device" section; Android `PrivacyDeviceSection`, `apps/multiplatform/common/src/androidMain/kotlin/chat/simplex/common/views/usersettings/PrivacySettings.android.kt:18-31`, titled at `views/usersettings/PrivacySettings.kt:61`). Opening it asks for the PIN once controls are on. In order:
1. What this does and doesn't do (section 3), and, on iOS, how to stop app deletion in Screen Time.
2. Set or change the parent PIN.
3. The switches, with the defaults of section 1, and the always-allowed contacts (section 7.9). During setup, "Turn on parental controls" saves the switches, and only while the PIN is stored. Leaving the screen without turning the controls on removes the new PIN, and a PIN left by an interrupted setup is removed at the next start by the reinstall rule of section 5, so the switches are never stored without a PIN.
4. When connecting or groups is switched off: one confirmation for the clean-up of section 7.3, and a note that groups and channels the child is already in stay.
5. "Turn off parental controls": PIN, then the switches, the always-allowed contacts and the PIN are removed, and the parental mode ends, so addresses and group links work as before. Pending connections removed by the clean-up are not restored; the child starts new ones as needed.

**The indicator:** while any switch is off, a row "Parental controls on" at the top of the Settings screen, before its first section, opening the parental screen (iOS `apps/ios/Shared/Views/UserSettings/SettingsView.swift:322-323`; Kotlin `views/usersettings/SettingsView.kt:99-101`). The chat list shows nothing.

## 7. The controls

Each control lists where its check runs, on both platforms, and the screens where its buttons are hidden or disabled. A check that fails throws a new error, "Turned off by parental controls", which the existing error alerts show; for connecting and joining groups, the alert offers "Allow once" (sections 7.2, 7.4).

### 7.1 The parental mode: what the core does on its own

Some connections, calls and files happen in the chat core with no app call: auto-accepting on an address, a business address, member contacts and group invitations; ringing for an incoming call; receiving small inline files. The app can't reach them on profiles other than the active one: address settings and the profile act only on the active user (`withUserId` → `checkSameUser`, `src/Simplex/Chat/Library/Internal.hs:3451-3457`), and the auto-accept setters need a hidden profile's password (`src/Simplex/Chat/Library/Commands.hs:541-551`). And the calls preference doesn't reach incognito contacts (`src/Simplex/Chat/Types.hs:695-698`) or contacts with their own calls setting (`src/Simplex/Chat/Types/Preferences.hs:1078-1080`).

So the core gets a parental mode: a new local API command that sets which of `connect`, `groups`, `calls`, `receiveMedia` are off and the connection IDs of the always-allowed contacts (section 7.9), held in memory like `APISetEncryptLocalFiles` (`Commands.hs:659`, `chatWriteVar encryptLocalFiles`). Each auto-accept setting is read through one effective value, "the setting is on and its switch is not off", computed once and used at every read listed below. While the parental mode is on:

| Off | The core | Where |
|---|---|---|
| `connect` | does not auto-accept requests to an ordinary address; they wait as with auto-accept off, and the child can accept one with allow-once ("Accept" stays visible for that, section 7.2) | `src/Simplex/Chat/Library/Subscriber.hs:1529-1541` |
| `connect` | ignores requests to a business address before storing them: nothing stored, nothing sent, the requester stays pending; in the notification extension the push then shows nothing rather than the generic "new contact request". (A business address has no waiting state: the store creates the business chat inside `createOrUpdateContactRequest`, `Subscriber.hs:1503`, `src/Simplex/Chat/Store/ContactRequest.hs:196-199`, and the subscriber then accepts it, `Subscriber.hs:1542-1543`.) No allow-once, since there is no request to show | `Subscriber.hs:1503` |
| `connect` | does not auto-accept a group member's direct-contact request; the stored invitation records no started connection | `autoAcceptMemberContacts` at `Subscriber.hs:3946,3949,3968` |
| `groups` | does not auto-accept group invitations; they stay as pending invitations | `autoAcceptGroupInvitations` at `Subscriber.hs:2723,3907` |
| `groups` | ignores join requests to the links of groups the child hosts, on every profile, sending nothing (not the existing rejection, which sends `XGrpLinkReject`). Channels are joined through their relays and are not covered (section 3) | `Subscriber.hs:1633-1648` (`acceptGroupJoinRequestAsync`, 1646) |
| `groups` | treats a group invitation that arrives on a group-link connection the child made earlier as a pending invitation instead of joining | `Subscriber.hs:2721-2722` → the pending branch at 2728-2730 |
| `calls` | turns every incoming call, except from an always-allowed contact, into the existing local "feature rejected" item, whatever the preferences; it never rings | `Subscriber.hs:3051-3057,3078-3081` |
| `receiveMedia` | does not receive inline files on arrival, except in a direct chat with an always-allowed contact; a file sent inline that arrives while receiving is off can't be received later (`src/Simplex/Chat/Library/Internal.hs:826`) | `receiveInlineMode`, `Subscriber.hs:2545-2551`, called at 2040 (direct and group messages), 2511 (direct) and 2527 (group) |
| `receiveMedia` | does not receive, at start or activation, files the notification extension marked to receive earlier, except files of always-allowed contacts (`files.contact_id`, `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql:287`); the marks stay, so marks under two days old are received at the next start after the controls are off (`src/Simplex/Chat/Store/Files.hs:809-823`); older ones wait for a manual download | `startFilesToReceive`, `src/Simplex/Chat/Library/Commands.hs:265,347-361,617` |

**How each process gets it.** The main app, the notification extension and the share extension each run their own core, and both extensions reuse theirs across pushes and shares, so each sends the command at these sites, and doesn't start or continue if sending fails (principle 5):
- iOS main app: in `initializeChat`, next to file encryption (`apps/ios/Shared/Model/SimpleXAPI.swift:2305`; every controller creation goes through it; `/_start` at 2376), and whenever the switches change.
- Notification extension: in `doStartChat` before `/_start` (`apps/ios/SimpleX NSE/NotificationService.swift:888-894`), and on every push after `startChat()` returns (line 425) and before it fetches the push's connections, because later pushes return early or only reactivate the core (849, 855-860, 917-925). It reads the switches and the always-allowed contacts from group defaults each time; if the resend fails, it skips the push and shows the generic notification.
- Share extension: in `initChat` on both branches, before `apiStartChat` and before `apiActivateChat` (`apps/ios/SimpleX SE/ShareModel.swift:179-201`; its file-encryption call at 198 runs after start, so it is not the pattern to copy).
- Android: in `initChatController`, next to file encryption (`platform/Core.kt:138`, before the chat starts at 166 or 180); the app and its background service share one process and one core.

It covers every profile, hidden ones included, the notification extension's core, and a database imported later. Nothing is sent to anyone and nothing is stored for the parental mode; the waiting records are the ones the setting-off paths already store. The address, group-link and preference settings are untouched, so turning the controls off brings them back as they were. Cost: one command, one variable, the checks above at about ten read sites, the per-push resend in the notification extension, and core tests for each.

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
- iOS (`apps/ios/Shared/Views/`): the new-chat sheet's "Create 1-time link", "Scan / Paste link", paste field and "Connect to @name" rows (`NewChat/NewChatMenuButton.swift:96-129,355-406`); the chat-list search's paste and name rows (`ChatList/ChatListView.swift:751-918`); the connect cards after onboarding and the banner (`NewChat/OnboardingCards.swift:150-302`, `Onboarding/ConnectBannerCard.swift:33-58`); creating an address (`UserSettings/UserAddressView.swift:295-336`); the "Accept" action on contact-request notifications (`Model/NtfManager.swift:64-70`), where no PIN prompt can run; contact cards (`ChatList/ChatListNavLink.swift:97-119`, `Contacts/ContactListNavLink.swift:217-261`); direct contact with a group member (`Chat/Group/GroupMemberInfoView.swift:364-377,420-430`, `Chat/ComposeMessage/ContextMemberContactActionsView.swift:30-40`).
- Kotlin: the new-chat sheet's "1-time link", "Scan / paste link", paste field and name rows (`views/newchat/NewChatSheet.kt:57-62,188-197,322,413,539-554`); the chat-list banner, search paste and name rows (`views/chatlist/ChatListView.kt:384-419,868-874,1236-1262`); the connect cards (`views/newchat/OnboardingCards.kt:376-417`); creating an address (`views/usersettings/UserAddressView.kt:74-121,310`); the "Accept" action on contact-request notifications (`apps/multiplatform/android/src/main/java/chat/simplex/app/model/NtfManager.android.kt:130-141`); contact cards (`views/chatlist/ChatListNavLinkView.kt:824-899`, `views/contacts/ContactListNavView.kt:62-70`); direct contact with a group member (`views/chat/group/GroupMemberInfoView.kt:518-522`, `views/chat/ComposeContextGroupDirectInvitationActionsView.kt:100,151-171`).
- Links in messages, in the app's own help and news rows, and opened from outside the app stay tappable, and so does "Accept" on contact requests in the chat list, the contacts list and the chat (iOS `ChatList/ChatListNavLink.swift:134-153,495-527`, `Contacts/ContactListNavLink.swift:78-106,281-314`, `Chat/ComposeMessage/ContextContactRequestActionsView.swift:21-35`; Kotlin `views/chatlist/ChatListNavLinkView.kt:132-135,279-283,527-557,698-728`, `views/chat/ComposeContextContactRequestActionsView.kt:69-96,136-165`, `views/contacts/ContactListNavView.kt:78-130`): they reach the check and get the "Allow once" alert. Every link path on both platforms passes `planAndConnect` (iOS `apps/ios/Shared/Views/NewChat/NewChatView.swift:1319`; Kotlin `views/newchat/ConnectPlan.kt:24`) before any connecting call.

**Allow once.** The alert offers "Allow once". The parent enters the PIN and sees what is about to happen: the name from the link's plan, the name on the request, or the member's name. The app then continues with a one-time pass held in memory and never stored, bound to that link, request or member:
- a short link connects in two steps, the plan and later the connect from the prepared chat's composer (iOS `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift:1195,1212`; Kotlin `views/chat/ComposeView.kt:657,698`), so the pass approved at the plan is carried to the prepared contact or business chat and used by its connect;
- contacting a group member is two calls (create, then send the invitation); one pass covers both;
- creating a one-time link or an address offers no allow-once, since there is no one to show the parent: the child scans the other person's link instead, or the parent switches connecting on and off.

Joining a group or channel has the same exception (section 7.4); any other switch is changed on the parental screen.

### 7.3 Connections started before the controls

Connections the child started before the controls complete in the core when the other side answers: an unused one-time link, a request sent to an address and accepted later, a prepared chat, a direct-contact invitation sent to a group member, a group being joined (`Subscriber.hs:484-518,619-636`). Blocking these in the core would also block allowed-once connections, which complete the same way. The child's own address and the links of groups the child hosts need nothing here: the parental mode ignores what arrives on them (section 7.1), and they work again when the controls are off. Channels are the exception (section 3).

So when connecting or groups is switched off, after one confirmation, the app deletes *(provisional, P2)*:
1. with connecting off: unused one-time links, requests the child sent that are still pending, direct-contact invitations the child sent to group members, and prepared chats not yet connected;
2. with groups off: groups the child is still joining.

It does this on every profile it can open, switching the active profile and back for the visible ones (chat commands act on the active user, section 7.1). The switches are stored and sent to the core first, with a flag that marks the clean-up as due until it ends, so an interrupted clean-up runs again at the next start; deleting again deletes nothing more. A hidden profile can't be opened without its password, so its one-off pending connections stay (section 3). Nothing is listed by name, so the parent learns nothing about the child's profiles or groups.

### 7.4 Groups and channels (`groups`)

Off: the child can't create or join groups or channels, create group links, or admit members waiting for review; joining offers "Allow once". Existing groups keep working, including adding the child's existing contacts to them, and they can gain members through other members' links.

| Action | iOS | Kotlin |
|---|---|---|
| Create a group | `apiNewGroup` 1949 | `apiNewGroup` 2415 |
| Create a channel | `apiNewPublicGroup` 1961 | `apiNewPublicGroup` 2428 |
| Accept a group invitation | `apiJoinGroup` 2007 | `apiJoinGroup` 2465 |
| Join via a group link (short links; a business chat counts as `connect`, section 7.2) | `apiConnectPreparedGroup` 1238 | `apiConnectPreparedGroup` 1874 |
| Join via a link (older links) | `apiConnect` 1058, by the plan | `apiConnect` 1691, by the plan |
| Create a group link | `apiCreateGroupLink` 2090 | `apiCreateGroupLink` 2585 |
| Admit a member waiting for review | `apiAcceptMember` 2017 | `apiAcceptMember` 2491 |

While groups are off, the parental mode keeps group invitations pending and ignores join requests to the links of groups the child hosts (section 7.1); a channel the child owns keeps admitting subscribers through its relays (section 3).

**Allow once.** As for connecting (section 7.2), the alert offers "Allow once", and after the PIN the parent sees the group's or channel's name before approving: from the invitation, for "Join" on a group invitation (`apiJoinGroup`); from the link's plan, for a group link, whose pass is carried to the prepared group and used by its "Join group" or "Join channel" (`apiConnectPreparedGroup`). An older full group link (`apiConnect`, by the plan) may contain no name, and the approval says so; once the parent allows it, the host's invitation arrives on that connection and waits as a pending invitation (section 7.1), so its "Join" asks for the PIN a second time, now with the group's name. Creating groups, channels and group links, and admitting members, have no allow-once.

Buttons hidden:
- iOS (`apps/ios/Shared/Views/`): "Create group" and "Create public channel" (`NewChat/NewChatMenuButton.swift:130-145`); creating a group link (`Chat/Group/GroupLinkView.swift:122-125,164-195`).
- Kotlin: "Create group" and "Create channel" (`views/newchat/NewChatSheet.kt:63-68,198-205`); creating a group link (`views/chat/group/GroupLinkView.kt:45-56,119-123,213`).
- "Join" on group invitations and "Join group" / "Join channel" in a prepared group stay visible, so they reach the check and its "Allow once": iOS `ChatList/ChatListNavLink.swift:217-236,320-330`, `Chat/ChatItem/CIGroupInvitationView.swift:95-101`, `Chat/ComposeMessage/ComposeView.swift:486-496`; Kotlin `views/chatlist/ChatListNavLinkView.kt:86,196-201,314-316,488-511,902-919`, `views/chat/item/CIGroupInvitationView.kt:90-93`, `views/chat/ComposeView.kt:1780-1787`.

### 7.5 Calls (`calls`)

Off: no calls in or out, except with always-allowed contacts (section 7.9).

Incoming calls are turned away by the parental mode (section 7.1): no ringing in the app, on the iPhone call screen, through the notification extension or on Android's call screen, for every contact, incognito ones included, except always-allowed ones. Rejecting sends nothing (`APIRejectCall`, `Commands.hs:1541-1552`; the local item at `Subscriber.hs:3078-3081`), so the caller sees a call nobody answers. No preference changes.

Outgoing and answering, checked in the app, pass for always-allowed contacts:

| Action | iOS | Kotlin |
|---|---|---|
| Start a call: checked before CallKit and the call view start, since a refusal at the API leaves the call screen hanging (`apps/ios/Shared/Views/Call/ActiveCallView.swift:121-131`) | `CallController.startCall`, `apps/ios/Shared/Views/Call/CallController.swift:351`, and `apiSendCallInvitation` 1834 | `startChatCall`, `views/chat/ChatView.kt:911`, and `apiSendCallInvitation` 2172 |
| Answer a call (an invitation received before calls were switched off) | `apiSendCallOffer` 1842 | `apiSendCallOffer` 2182 |

For contacts that are not always allowed, the chat toolbar's call button is hidden, and the call buttons of contact and member info keep the disabled look they have when calls are prohibited, saying "Turned off by parental controls" when tapped:
- iOS (`apps/ios/Shared/Views/`): the chat toolbar's call menu (`Chat/ChatView.swift:562-583`; the filter button takes its place, as when calls are unavailable, 584-587); contact info's call buttons (`Chat/ChatInfoView.swift:160-161`) and member info's (`Chat/Group/GroupMemberInfoView.swift:370-371`), both `CallButton` (`Chat/ChatInfoView.swift:783-896`). Calls started by Siri or from the phone's recents (`apps/ios/Shared/ContentView.swift:325-355`) reach `CallController.startCall` and are refused there.
- Kotlin: the chat toolbar's call button and menu (`views/chat/ChatView.kt:1296-1300,1552-1561`); contact info's call buttons (`views/chat/ChatInfoView.kt:576-577`) and member info's (`views/chat/group/GroupMemberInfoView.kt:512-513`), both `CallButton` (`views/chat/ChatInfoView.kt:965`).

### 7.6 Sending files and voice messages (`sendFiles`, `voice`)

Off: in chats other than with always-allowed contacts (section 7.9), the composer's attachment (photos, camera, files) or voice button is disabled and says "Turned off by parental controls" when tapped; forwarding a message with a file is refused; the share extension does not send files.

| Action | iOS | Kotlin |
|---|---|---|
| Send messages; refused when any carries a file (`sendFiles`) or is voice (`voice`), unless the chat is with an always-allowed contact | `apiSendMessages` 559 | `apiSendMessages` 1253 |
| Forward messages; refused when any carries a file, unless the chat they go to is with an always-allowed contact | `apiForwardChatItems` 526 | `apiForwardChatItems` 1312 |

Share extension (iOS): it starts its own core and sends through its own `apiSendMessages` (`apps/ios/SimpleX SE/ShareAPI.swift:57-85`, called at `apps/ios/SimpleX SE/ShareModel.swift:149`), so the check runs there too: with `sendFiles` off it refuses images, videos and files, except to always-allowed contacts, and still shares text and links. It reads the switches and the always-allowed contacts from group defaults, as it reads "Allow sharing" today (`ShareModel.swift:77-80`).

Buttons disabled, as when a contact's preferences prohibit files or voice, with the parental alert in place of "Files and media prohibited!" and "Voice messages prohibited!":
- iOS (`apps/ios/Shared/Views/Chat/`): the attachment button and its menu (`ComposeMessage/ComposeView.swift:1127-1147`, items 608-689; the disabled pattern at 1137-1144), the voice button (`ComposeMessage/SendMessageView.swift:120-128`; the disabled pattern, `voiceMessageNotAllowedButton`, at 295-330). Pasting images into the text field is refused (`ComposeMessage/NativeTextEditor.swift:186-256`). Forwarding stays available for text and is refused for files at `apiForwardChatItems`.
- Kotlin: the attachment button (`views/chat/ComposeView.kt:1311-1345`), the voice button (`views/chat/SendMsgView.kt:137-155`). Images inserted from the Android keyboard are refused (`apps/multiplatform/common/src/androidMain/kotlin/chat/simplex/common/platform/PlatformTextField.android.kt:108-120`). Sharing into the app from other Android apps (`apps/multiplatform/android/src/main/java/chat/simplex/app/MainActivity.kt:157-209`) ends in `apiSendMessages` and is refused there for files.

### 7.7 Receiving images, videos and files (`receiveMedia`)

Off: except in direct chats with always-allowed contacts (section 7.9), nothing is downloaded or opened, and image, video and link previews are replaced by a plain placeholder, because image and video messages contain their own preview (`MCImage`, `MCVideo`, `src/Simplex/Chat/Protocol.hs:747-748`), and link messages contain a picture the sender chose (`MCLink`, `LinkPreview`, `Protocol.hs:746`). Notifications contain no message images on either platform, so they need nothing. The core stops receiving inline files (section 7.1).

| Action | iOS | Kotlin |
|---|---|---|
| Receive a file (manual and automatic), unless it is in a direct chat with an always-allowed contact | `receiveFile` 1598, `receiveFiles` 1607 | `receiveFile` 2385, `receiveFiles` 2269 |
| Decide what to auto-receive (main app and notification extension) | `ChatItem.autoReceiveFile()`, `apps/ios/SimpleXChat/ChatTypes.swift:4003-4013`, used by the notification extension at `apps/ios/SimpleX NSE/NotificationService.swift:1031` | the new-message handler decides and calls `receiveFile` (checked), `model/SimpleXAPI.kt:3070-3079`; Android has no separate extension |

Previews replaced and download buttons hidden:
- iOS (`apps/ios/Shared/Views/`): image, video, voice and file items (`Chat/ChatItem/CIImageView.swift:50-60`, `Chat/ChatItem/CIVideoView.swift:70,101`, `Chat/ChatItem/CIVoiceView.swift:302-316`, `Chat/ChatItem/CIFileView.swift:88-100`), link previews (`Chat/ChatItem/CILinkView.swift:22-23`), quoted media (`Chat/ChatItem/FramedItemView.swift:263-276`), the compose bar's media preview of an item being forwarded or edited (`Chat/ComposeMessage/ComposeView.swift:285-287`), the "Download" actions (`Chat/ChatView.swift:2328-2329,1597-1608`), and media previews in the chat list (`ChatList/ChatPreviewView.swift:427-453`).
- Kotlin: image, video, file and voice items (`views/chat/item/CIImageView.kt:225-236`, `views/chat/item/CIVideoView.kt:102,132`, `views/chat/item/CIFileView.kt:76-86`, `views/chat/item/CIVoiceView.kt:424-428`), link previews and quoted media (`views/chat/item/FramedItemView.kt`), the compose bar's context item (`views/chat/ContextItemView.kt`), the "Download" menu item (`views/chat/item/ChatItemView.kt:458-467`), media previews in the chat list (`views/chatlist/ChatPreviewView.kt:335-353`). Android's background service receives through the same `receiveFile` and is covered by its check.

Received files the child already has stay as they are.

### 7.8 Directory search (`directorySearch`)

Off: the app doesn't search SimpleX Directory. A search sends the typed text to the directory and lists public groups and channels; joining one of them is checked as in section 7.4. The switch is separate from connecting and groups, so a parent can allow searching while joining still needs "Allow once". Directory search is in pull request #7881 (https://github.com/simplex-chat/simplex-chat/pull/7881), not yet on master; the references below are to its head commit e90838ea9.

| Action | iOS | Kotlin |
|---|---|---|
| Search the directory: the "Search in Directory" row, the keyboard's Search key, "Load more" and retry all reach it | `apiSearchDirectory`, `apps/ios/Shared/Model/SimpleXAPI.swift:1044` | `apiSearchDirectory`, `model/SimpleXAPI.kt:1685` |

Hidden:
- iOS: the "Search in Directory" row in the chat list's search (`apps/ios/Shared/Views/ChatList/ChatListView.swift:860,863,914-915`, defined at 1015-1017) and in New chat (`apps/ios/Shared/Views/NewChat/NewChatMenuButton.swift:108-110`); the keyboard's Search key runs only the local filter (`ChatListView.swift:877-878`, `NewChatMenuButton.swift:448-449`).
- Kotlin: the same row (`views/chatlist/ChatListView.kt:1308`, `views/newchat/NewChatSheet.kt:364,462`) and Search key (`views/chatlist/ChatListView.kt:889`, `views/newchat/NewChatSheet.kt:355,453`).

The "Directory search" privacy setting that the pull request adds (iOS `apps/ios/Shared/Views/UserSettings/PrivacySettings.swift:87-89`, Kotlin `views/usersettings/PrivacySettings.kt:72-76`) is shown disabled, with "Turned off by parental controls" under it. The core needs nothing: a search is a request the app sends (`APISendServiceRequest`), never one the core starts.

### 7.9 Contacts always allowed

The parent can mark contacts, such as the parents themselves, as always allowed: calls, sending files, voice messages and receiving media then work in the direct chat with that contact whatever those four switches say. Connecting, groups and directory search have no such list: they concern people the child isn't in contact with yet. Groups are not covered: files that an always-allowed contact sends in a group follow the switches.

**Adding a contact.** On the contact's page, a row "Always allowed" in a section of its own after the one with "Contact preferences" (iOS `apps/ios/Shared/Views/Chat/ChatInfoView.swift:185-206`; Kotlin `views/chat/ChatInfoView.kt:594-616`), shown while any of the four switches is off; turning it on or off asks for the PIN. The child opens that page and hands the phone over, so the parent sees only that contact. The parental screen lists the always-allowed contacts of the current profile by name, each removable there; it lists no other contacts. Two other ways were weighed: choosing from a list of all contacts on the parental screen, which shows the parent every contact; and offering it with "Allow once", which would apply to a contact that exists only after the other side accepts.

**Where it is checked:**
- The core: the calls and receiving-media rows of section 7.1 skip always-allowed contacts, matched by the connection ID of the contact's active connection (section 5); the app sends the list with the switches, at the same sites.
- The app, at the checks of sections 7.5–7.7: starting and answering a call, sending and forwarding files and voice messages to the contact, receiving its files, manually and automatically (`ChatItem.autoReceiveFile()` included, so the notification extension receives them), and its previews. The share extension sends files to an always-allowed contact.

Removing the contact's entry, deleting the contact, or turning the controls off ends it.

## 8. Settings while controls are on

**Need the PIN:** only the parental controls screen.

**Off entirely, even for the parent** *(provisional, P5)*, shown disabled where the setting is listed, because each would hand the chats to whoever uses it or skip every check:

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
- A call to the child from a contact that is not always allowed is never answered: the caller sees "calling…" and then a missed call, as with anyone who doesn't pick up.
- Nothing else: no control sends a message or changes the profile or any preference.

## 10. Onboarding

No onboarding step. A question about children asked of every new user puts parental controls in front of adults who have none, which one commenter in the issue objects to; a parent sets the controls up in Settings before handing the phone over. The website's FAQ points to the setting.

Onboarding today (since #6888, 2026-04-27) is: introduction, first profile, "Your network" (routers and notifications), conditions (iOS `apps/ios/Shared/Views/Onboarding/OnboardingView.swift:18-57`; Kotlin `views/onboarding/OnboardingView.kt:3-13`). Making connections moved out of onboarding into cards at the top of the chat list (#6753; iOS `apps/ios/Shared/Views/NewChat/OnboardingCards.swift:135-308`, Kotlin `views/newchat/OnboardingCards.kt:332`). With connecting off, those cards are hidden (section 7.2); with any switch off, onboarding's "Migrate" is off (section 8).

## 11. Not built

- Forwarding or showing the child's messages, contacts or groups to anyone.
- Preventing the child from deleting messages or chats, or forcing disappearing messages off: they exist only to keep history for reading.
- An "incognito off" switch: it doesn't stop anyone reaching the child; it only makes the child show their real name and photo to everyone they connect with.
- Making the controls survive deleting the app. On iOS the keychain can outlive the app, but the app removes its keychain passcodes after a reinstall on purpose (`apps/ios/Shared/AppDelegate.swift:111-122`, #3841), and Apple's engineers call keychain survival undocumented (https://developer.apple.com/forums/thread/36442). On Android, Block Store keeps up to 16 small entries across a reinstall when the user's backup is on, in builds with Google Play services (https://developer.android.com/identity/block-store), so not in the F-Droid build. Either would take away the simple way out for an adult on whom the controls were imposed; on iOS, Screen Time does it with the parent's own passcode.
- Desktop *(provisional, P10)*: the desktop app stores passcodes unencrypted (`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt:3-17`) in `settings.properties` (`platform/Resources.desktop.kt:40,64`), so every switch could be edited and the PIN read. The parental screen is hidden on desktop.
- Approval from the parent's own phone. iOS 26's PermissionKit can send a child's request to the parent through Messages, but it shows the new contact's name to Apple's Messages, and Apple asks apps to use it only when they know the user is a child (WWDC25 session 293, https://developer.apple.com/videos/play/wwdc2025/293/).
- Time limits and app blocking: Screen Time and Family Link do these.
- Checking every command in the core, so the console and every client are covered without locking them: the next step after the parental mode of section 7.1, if the app checks prove too easy to get around.

## 12. Build order

Each step ships on its own, iOS first, then Kotlin for Android.

1. The parental mode (7.1) with its tests; stored state, the parent PIN, the parent gate, the parental screen, the indicator, and the settings that are off (8).
2. Connecting: the checks and allow-once of 7.2, the clean-up of 7.3, and directory search (7.8).
3. Groups and channels (7.4), with allow-once.
4. Calls (7.5), sending files and voice (7.6), receiving media (7.7), and the always-allowed contacts (7.9).

## 13. Open choices

The answers on the decision page are applied above. Three choices stay provisional:

1. **Connections started before the controls (P2), for the project owner.** The design deletes them on every profile the app can open, without listing them (section 7.3). The other options are listing them for the parent to keep or delete, which shows the parent the child's profiles and group names, or leaving them with a sentence on the setup screen.
2. **Desktop (P10), for the project owner.** The design leaves the desktop app out, since its settings file keeps passcodes in plain text (section 11). The user leans the same way.
3. **The parent unlocking the app (P5).** The user suggests that the parent should be able to unlock the app, which conflicts with principle 2: the PIN never opens a path to the chats. The options:
   - A. As designed: the PIN opens only the parental screen and allow-once; when the child has locked the app, the parent asks the child to unlock it. **Recommended:** it adds nothing to what a PIN holder can do, so an adult on whom the controls are imposed keeps a phone whose chats the PIN can't open (section 4).
   - B. The lock screen offers "Parental controls", which opens only the parental screen with the PIN: the parent changes the controls without the child's passcode and still sees no chats. It costs a way past the lock screen, so whoever holds the PIN can change the controls without the phone's owner unlocking it.
   - C. The PIN also unlocks the app: whoever holds the PIN reads the chats whenever they hold the phone. It drops principle 2 and the safeguard of section 4.
