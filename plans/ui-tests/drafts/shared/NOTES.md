# Shared UI test drafts: notes

14 tests in 11 classes. Nothing here has been compiled or run.

Paths:
- `C/` means `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`
- `H/` means `tests/ChatTests/`
- `S` means `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`
- Line numbers are from `/home/user/wt-uitest` at `eda94a505`.

## Files

| File | What it is |
|---|---|
| `main/ChatListToolbarSearchTest.kt` | `searchFiltersByDisplayName` |
| `main/ChatListMenusPendingTest.kt` | `groupMenuMuteModesAllMentionsNone` |
| `main/UserPickerTest.kt` | `userPickerSwitchProfile` |
| `main/ChatProfilesTest.kt` | `profilesAddProfile` |
| `main/ContactInfoTest.kt` | `contactInfoVerifyCodeMatchesCli`, `contactInfoMarkVerified` |
| `chat/EditMessageTest.kt` | `edit_received_updatesBubble` |
| `chat/DeleteMessageTest.kt` | `delete_received_onlyForMe` |
| `chat/ForwardTest.kt` | `forward_contactToContact_peerSeesForwarded`, `forward_toNotes_showsSavedFrom` |
| `chat/ChatSearchTest.kt` | `search_direct_filtersResults` |
| `chat/NotesTest.kt` | `notes_sendText_saved` |
| `groups/MemberInfoTest.kt` | `memberInfo_changeRoleToAdmin`, `memberInfo_removeMember` |
| `screens.kt` | A drop-in replacement for `harness/Screens.kt`. It keeps all of the current content unchanged and adds helpers (listed below). |
| `keys.diff` | The app-side keys. Paths are relative to `/home/user/wt-uitest`. `git apply --check` passes on `eda94a505`. |

Class names follow the catalogue class map in `00-legend.md`, for example `ChatListMenusPendingTest` and `DeleteMessageTest`. The existing repo files use their own names instead: `MessagingTest` holds reply, edit, delete and reaction tests, and `ChatListMenuTest` and `ContactsTest` are used too. Some existing test names are not in the catalogue at all: `contactMenuMarkReadAndUnread` stands in for the catalogue's `contactMenuMarkRead` and `contactMenuMarkUnread`. If the traceability check is enforced, either the drafts get moved into the repo's classes or the repo's classes get renamed. That decision is left open.

### Helpers added in `screens.kt`

- **`ChatListScreen`**
  - `rowMenu(name, action, label)` asserts the menu item's label before clicking it.
  - `search(text)`
  - `shown(name)`
  - `shareTo(name)`
- **`NewChatScreen`**
  - `invite(vararg names)`
- **`UserPickerScreen`**
  - `profiles()`
  - `switchTo(name)`
- **New screens**
  - `ProfilesScreen`: `add(name)`, `close()`
  - `ContactInfoScreen`: `securityCode()`, `markVerified()`, `verifyRowText()`
  - `GroupInfoScreen`: `member(name)`
  - `MemberInfoScreen`: `changeRole(role)`, `remove()`
- **`ChatScreen`**
  - `contactInfo()`, `groupInfo()`
  - `item(id)`, `itemTexts()`
  - `forwardTo(text, chatName)`
  - `search(text)`
- **`UiScope.ownGroup(app, group, vararg peers): ChatScreen`**
  - alice creates the group in the GUI and invites the peers from the invite sheet.
  - Each peer runs `/j`.
  - It returns the open group chat.
  - It could move to `Helpers.kt`, next to `hostGroup`.

Every helper uses only methods that both `UiScope` implementations have: `click`, `rightClick`, `typeInto`, `escape`, `exists`, `waitFor`, `poll`, `textOf`, `textsOf`, `isEnabled`, `seesText`, `descriptionsOf`, `editableText` and `pressEnter`. They also use `CliPeer` `sends`, `eventually`, `eventuallyMessage`, `expects`, `expectsMessage` and `nextLine`.

## Keys added (`keys.diff`)

All keys are in commonMain. None of the touched composables has a desktopMain or androidMain actual, so both platforms get every key.

| Key | Tag | Attached at |
|---|---|---|
| `Ui.ChatList.Search` | `chatList.search` | `C/views/chatlist/ChatListView.kt:803-804`, the `SearchTextField` modifier |
| `Ui.Chat.Bar.part("search")` (no new constant) | `chat.bar.search` | `C/views/helpers/DefaultTopAppBar.kt:87`, the search field of any `DefaultAppBar` that has a `uiKey` (only `ChatView` passes one) |
| `Ui.Chat.BarMenu` | `chat.bar.menu` | `C/views/chat/ChatView.kt:1426`, the toolbar ⋮ `IconButton`. Also adds `import chat.simplex.common.ui.ui`. |
| `Ui.Chat.BarMenuItem("search")` | `chat.bar.menu.search` | `ChatView.kt:1298` (direct) and `:1328` (group) |
| `Ui.Chat.ItemMenu(id, "forward")` | `chat.item.menu.forward[id]` | `C/views/chat/item/ChatItemView.kt:478` |
| `Ui.Share.Row(chatId)` | `share.row[chatId]` | `C/views/chatlist/ShareListNavLinkView.kt`. `ShareListNavLinkLayout` gets a `uiKey` parameter, and the Direct, Group and Local cases pass `chat.id`. |
| `Ui.Info.VerifyCode` | `info.verifyCode` | `C/views/chat/ChatInfoView.kt:1210-1218`, inside `VerifyCodeButton`. The member-info screen shares this key; the two screens are never visible together. |
| `Ui.VerifyCode.Code` | `verifyCode.code` | `C/views/chat/VerifyCodeView.kt:80`, the code `Text` |
| `Ui.VerifyCode.Mark` | `verifyCode.mark` | `VerifyCodeView.kt:117`. `SimpleButton` (`C/views/helpers/SimpleButton.kt:26`) gets an optional `uiKey` parameter before `click`. I checked all 13 call sites and none of them is ambiguous. |
| `Ui.UserPicker.Profiles` | `userPicker.profiles` | `C/views/chatlist/UserPicker.kt:271-304`, the "Your chat profiles" row |
| `Ui.Profiles.Add` | `profiles.add` | `C/views/usersettings/UserProfilesView.kt:182` |
| `Ui.NewProfile.Name` | `newProfile.name` | `C/views/WelcomeView.kt:139`, `CreateProfile` |
| `Ui.NewProfile.Create` | `newProfile.create` | `C/views/WelcomeView.kt` `CreateProfile`, the "Create profile" `SettingsActionItem` |
| `Ui.InviteMembers.Contact(contactId)` | `inviteMembers.contact[id]` | `C/views/chat/group/AddGroupMembersView.kt:342`, `ContactCheckRow`. `SectionItemViewWithoutMinPadding` gets a `uiKey` parameter. |
| `Ui.InviteMembers.Invite` | `inviteMembers.invite` | `AddGroupMembersView.kt:243`, inside `InviteMembersButton` |
| `Ui.GroupInfo.Member(groupMemberId)` | `groupInfo.member[id]` | `C/views/chat/group/GroupChatInfoView.kt:785`, the member row. `SectionItemViewLongClickable` (`C/views/helpers/Section.kt:240`) gets a `uiKey` parameter. |
| `Ui.MemberInfo.Role` (+ `.option(GroupMemberRole)`) | `memberInfo.role`, `memberInfo.role.option[Admin]` | `C/views/chat/group/GroupMemberInfoView.kt:918`, through the existing `ExposedDropDownSettingRow` `uiKey` |
| `Ui.MemberInfo.Remove` | `memberInfo.remove` | `GroupMemberInfoView.kt:843`, `RemoveMemberButton` |
| `Ui.Alert.Option(Shared, "remove")` | `alert.option.remove[Shared]` | `GroupMemberInfoView.kt:314`, the "Remove" button. It is keyed only in the group branch of `removeMemberDialog`; the relay and channel branches are not keyed. |

Kotlin naming: `Ui.GroupInfo` and `Ui.Share` are nested objects, so they never clash with the model class `GroupInfo` unless a file imports `Ui.*`. No file does that.

---

## Per test

### 1. `searchFiltersByDisplayName`

**Catalogue:** 01:62. Needs P.

**What the test asserts:** with contact bob and the "Private notes" chat, typing "bo" into the chat-list search keeps bob's row and hides notes.

**Why this is the expected behaviour:**
- `filteredChats` (`C/views/chatlist/ChatListView.kt:1545-1573`) keeps chats for which `anyNameContains` is true (`C/model/ChatModel.kt:1420-1423`, `1921-1924`).
- "bo" has fewer than `MIN_NAME_LENGTH` = 5 characters (`ChatListView.kt:1145`), so `nameSearchCandidate` returns null. That means no name lookup and no "connect to name" row.
- The notes chat's name is the string `note_folder_local_display_name` = "Private notes" (`S:90`). It does not contain "bo".
- Haskell: `testFilterSearch` (`H/ChatList.hs:82-95`, search "bob" matches `@bob`).

**Platform differences:** none. The search bar is the same composable on both platforms; it sits at the bottom on Android when one-hand UI is on, but it is still composed.

**Uncertainty:**
- None in the behaviour.
- I used notes as the non-matching chat so that the row's Needs stays a single peer (P).

### 2. `groupMenuMuteModesAllMentionsNone`

**Catalogue:** 01:86. Needs P.

**What the test asserts:**
- The row menu item `ntfs.Mentions` is labelled "Mute", and choosing it sets `enableNtfs` to `Mentions`.
- Then `ntfs.None` is labelled "Mute all" and sets `None`.
- Then `ntfs.All` is labelled "Unmute" and sets `All`.

**Why this is the expected behaviour:**
- `GroupMenuItems` else-branch (`C/views/chatlist/ChatListNavLinkView.kt:312-358`) and `ToggleNotificationsChatAction` (`:435-446`).
- `MsgFilter.nextMode(true)` and `text()` (`C/model/SimpleXAPI.kt:5456-5470`).
- `hasMentions` is true for every group (`ChatModel.kt:1830`).
- Strings: `mute_chat`, `mute_all_chat` and `unmute_chat` (`S:855-857`).
- Setup: the same join flow as the existing `leaveGroup_fromChatListMenu`.
- The test then polls until membership is no longer `MemInvited` or `MemAccepted`, because only after that does the menu contain the ntfs item (`ChatListNavLinkView.kt:320-335`).
- Haskell: `testMuteGroup` (`H/Direct.hs:1854`). The CLI side prints nothing, so no CLI line is asserted.

**Platform differences:** none. Accepting the join alert does not open the chat (`groupChatAction`, `:204-210`), so the row stays visible on Android too.

**Uncertainty:** the catalogue also says "the row icon follows the mode". I did not assert the icon, for two reasons:
- The mentions icon and the muted icon have the same content description, "Notifications" (`C/views/chatlist/ChatPreviewView.kt:477-507`), so the description cannot tell the modes apart.
- If the joined group has unread items, the icon is replaced by the unread badge.

### 3. `userPickerSwitchProfile`

**Catalogue:** 01:119. Needs —.

**What the test asserts:**
- A group "team" exists for alice.
- alice creates the profile "work" in the GUI; "work" becomes the active profile and its chats load.
- alice opens the user picker and taps alice's avatar (`Ui.UserPicker.User(aliceId)`).
- alice is active again and the chat list shows "team".

**Why this is the expected behaviour:**
- `onUserClicked` calls `changeActiveUser` (`C/views/chatlist/UserPicker.kt:160-176`).
- `changeActiveUser_` reloads users and chats (`SimpleXAPI.kt:748-778`).
- Avatars are keyed on desktop (`UserPicker.desktop.kt`, `UserPickerUsersSection`) and on Android (`UserPicker.android.kt:83`).
- Haskell: `testCreateSecondUser` (`H/Direct.hs:1968`).

**Platform differences:**
- On desktop the inactive users appear in the picker's second section. On Android all users appear in the top section. The key is the same in both.
- `ProfilesScreen.close()` uses `escape()`, which is the desktop backstack and the system back button on Android.

**Uncertainty:**
- "team" is created with `chatController.apiNewGroup` before any UI step. This is deliberate: the group then reaches the model only through the reload that switching users triggers, so the final `row("team")` proves that the chat list was reloaded.
- If the team prefers no direct API setup here, the alternative is `newChat().createGroup("team")` followed by closing the invite sheet. That path needs a key or an `escape()` for the sheet.

### 4. `profilesAddProfile`

**Catalogue:** 01:122. Needs —.

**What the test asserts:**
- User picker → "Your chat profiles" → "Add profile" → name "work" → "Create profile".
- "work" becomes the current user, and the users list holds both "alice" and "work".

**Why this is the expected behaviour:**
- `addUser` → `CreateProfile` (`C/views/usersettings/UserProfilesView.kt:49-55`, `C/views/WelcomeView.kt:63-170`).
- `createProfileInProfiles` sets `currentUser` and refreshes `users` (`WelcomeView.kt:371-388`).
- `withAuth` runs the block at once when `performLA` is off (`C/views/usersettings/SettingsView.kt:475-478`).
- Strings: `your_chat_profiles` (`S:1011`), `users_add` (`S:2198`), `create_another_profile_button` = "Create profile" (`S:1256`).

**Platform differences:** none in the test code. `CreateProfile` opens in `ModalManager.center`, which works on both platforms.

**Uncertainty:**
- "Create profile" is a `SettingsActionItem`. When it is disabled it has no clickable modifier, so the `Disabled` semantics never appear and `isEnabled` cannot be polled. The test relies on `typeInto` having waited until the field holds the name.
- The catalogue's "lists it" is checked through `chatModel.users`, not the profile rows, because those rows have no key.

### 5. `contactInfoVerifyCodeMatchesCli`

**Catalogue:** 01:254. Needs P.

**What the test asserts:** the code shown on "Security code", with all whitespace removed, equals the line after `alice security code:` in bob's `/code alice` output, also with whitespace removed.

**Why this is the expected behaviour:**
- `VerifyCodeLayout` splits the code into 24-character lines (`C/views/chat/VerifyCodeView.kt:64-85`).
- The real CLI prints the label line and then the code (`src/Simplex/Chat/View.hs:2020-2023`, `viewSecurityCode` with the test view off).
- Haskell: `testMarkContactVerified` (`H/Direct.hs:3116-3126`, `bCode` = `aCode`).

**Platform differences:** none. Android also shows a "Scan code" button, which the test does not use.

**Uncertainty:**
- I used `eventually("alice security code:")` rather than `expects`, because `sends` removes only the echo and leaves any earlier lines in the queue (`CliPeer.takeFirst`).
- The CLI's third line (the "pass this code…" hint) is not asserted.

### 6. `contactInfoMarkVerified`

**Catalogue:** 01:254. Needs P.

**What the test asserts:**
- "Mark verified" closes the screen.
- The model marks bob's contact as verified.
- The info row reads "View security code".
- Reopening the screen shows "bob is verified".

**Why this is the expected behaviour:**
- `VerifyCodeView` closes on success (`VerifyCodeView.kt:40-45`).
- The model is updated (`C/views/chat/ChatInfoView.kt:166-180`), and `Contact.verified` is `activeConn.connectionCode != null` (`ChatModel.kt:1919`).
- The row label is chosen by `VerifyCodeButton` (`ChatInfoView.kt:1210-1218`).
- Strings: `view_security_code` (`S:747`), `is_verified` = "%s is verified" (`S:1005`).

**Platform differences:** none.

**Uncertainty:**
- The info screen must recompose from the model to show "View security code". The catalogue asserts this, but I did not trace how the `contact` argument of `ChatInfoLayout` is refreshed.
- The final check finds "bob is verified" by text through `seesText`; that line has no key.

### 7. `edit_received_updatesBubble`

**Catalogue:** 02:141. Needs P.

**What the test asserts:**
- bob sends "hello", then runs `! @alice hey`, which edits his last message to alice.
- The same item id in the GUI now has text "hey" and `meta.itemEdited` set, and the bubble text contains "hey".

**Why this is the expected behaviour:**
- `EditMessage` and its parser `("! " <|> "!")` (`src/Simplex/Chat/Library/Commands.hs:2693`, `:6407`).
- Haskell: `testDirectMessageUpdate` (`H/Direct.hs:676-678`, `bob <# "alice> [edited] hey 👋"`). Also `H/Groups.hs:5926`: `##>` shows that the CLI echoes `!` commands, so `sends` finds the echo.
- The edited mark is an icon with no text or description (`C/views/chat/item/CIMetaView.kt:83-85`), so it is checked through the model.

**Platform differences:** none.

**Uncertainty:** low. ASCII text avoids emoji input through the pty.

### 8. `delete_received_onlyForMe`

**Catalogue:** 02:151. Needs P.

**What the test asserts:**
- "Delete message?" offers `forMe` but not `forEveryone`.
- "Delete for me" removes the item from the model and from the screen.
- bob prints nothing.

**Why this is the expected behaviour:**
- `canDeleteForEveryone = meta.deletable && …` (`C/views/chat/item/ChatItemView.kt:1396-1427`).
- `deletable'` is false for anything that is not `CISndMsgContent` (`src/Simplex/Chat/Messages.hs:542-549`).
- `cidmInternal` with no `toChatItem` → `removeChatItem` (`C/views/chat/ChatView.kt:548-581`).
- "bob prints nothing" is checked with a sentinel: alice then sends "ping", and bob's very next line must be `alice> ping`.

**Platform differences:** `rightClick` is a long press on Android. The item's `combinedClickable` `onLongClick` opens the same menu (`ChatItemView.kt:326-336`).

**Uncertainty:** the sentinel assumes bob has no other queued lines after his own send. The existing `reply_menu_sendQuoted_peerSeesQuote` relies on the same assumption.

### 9. `forward_contactToContact_peerSeesForwarded`

**Catalogue:** 02:165. Needs P ×2.

**What the test asserts:**
- bob sends "hey"; alice forwards it to cath (menu "forward" → share row cath → Send).
- cath prints `alice> -> forwarded` and then `      hey`.

**Why this is the expected behaviour:**
- `forwardItem` sets `chatId = null` and `sharedContent = Forward` (`ChatView.kt:639-642`).
- The share list shows ready chats where sending is allowed (`C/views/chatlist/ShareListView.kt:196-202`).
- The compose area gets `ForwardingItems` (`C/views/chat/ComposeView.kt:1631-1634`). Send is enabled while forwarding (`ComposeView.kt:172-182`), and the voice button is hidden (`C/views/chat/SendMsgView.kt:73-74`).
- Sending forwards the item (`ComposeView.kt:838-850`).
- Haskell: `testForwardContactToContact` (`H/Forward.hs:167-177`: bob's "hey" forwarded to cath prints `alice> -> forwarded` / `      hey`).

**Platform differences:** none. On both platforms the share list replaces the chat list while `sharedContent` is set. Send is clicked rather than Enter, because the message field is empty.

**Uncertainty:** low.

### 10. `forward_toNotes_showsSavedFrom`

**Catalogue:** 02:167. Needs P.

**What the test asserts:**
- After forwarding bob's "hey" to "Private notes", the notes chat is open.
- It holds an item whose `itemForwarded` is `Contact("bob")`.
- The bubble shows "saved from" and "bob".

**Why this is the expected behaviour:**
- The header has two rows when the chat type is Local and `chatTypeApiIdMsgId` is not null (`C/views/chat/item/FramedItemView.kt:314-343`).
- Caption string `saved_from` = "saved from" (`S:73`); the second row shows `forwarded.chatName`.
- The core creates the note folder for every new user (`Commands.hs:471`), so it exists for alice without a fixture.
- Haskell: `testForwardContactToNotes` (`H/Forward.hs:229-247`).

**Platform differences:** none.

**Uncertainty:** the header is matched as text inside `Ui.Chat.Item(id)`. Both header rows are plain `Text` elements, so the match should hold, but it was not run.

### 11. `search_direct_filtersResults`

**Catalogue:** 02:230. Needs P.

**What the test asserts:**
- alice sends "hello there 🙂"; bob sends "hello there" and "how are you?".
- Searching "ello ther" leaves exactly {"hello there 🙂", "hello there"} in the model, and the "how are you?" item is no longer on screen.

**Why this is the expected behaviour:**
- `onSearchValueChanged` → `apiFindMessages`, which clears the items and loads the search results (`ChatView.kt:280-291`, `C/views/chatlist/ChatListNavLinkView.kt:254-260`).
- The ⋮ "Search" item is in `ChatView.kt:1296-1301`.
- Haskell: `testAddContact` (`H/Direct.hs:211-237`, `search=ello ther` → `[(1,"hello there 🙂"),(0,"hello there")]`).

**Platform differences:** none. The ⋮ menu and the "Search" entry are common code for direct chats.

**Uncertainty:** the results are compared as a set, so their order is not asserted.

### 12. `notes_sendText_saved`

**Catalogue:** 02:276. Needs —.

**What the test asserts:** sending "keep in mind" in "Private notes" creates an item that exists on screen with direction `LocalSnd`.

**Why this is the expected behaviour:**
- `createNoteFolderContentItems` (`Commands.hs:5109`).
- Haskell: `testNotes` (`H/Local.hs:39-41`).

**Platform differences:** none.

**Uncertainty:** none. The chat-list preview is not asserted, because on Android the list is not composed while a chat is open.

### 13. `memberInfo_changeRoleToAdmin`

**Catalogue:** 03:171. Needs P.

**What the test asserts:**
- alice creates #team and invites bob, who joins with `/j team`.
- alice opens Group info → bob → Change role → Admin → "Change".
- bob prints `#team: alice changed your role from member to admin (signed)`, and the model shows bob as Admin.

**Why this is the expected behaviour:**
- `onRoleSelected` → `updateMemberRoleDialog` (confirm `change_verb`) → `updateMembersRole` (`C/views/chat/group/GroupMemberInfoView.kt:149-158`, `:968-991`).
- The role picker is shown for an owner (`canChangeRoleTo`, `ChatModel.kt:2944-2955`).
- The invite lines come from `fullAddMember` (`H/Utils.hs:917-928`) and `View.hs:1407` (`ttyFullGroup`: no full name, because the GUI creates groups with `fullName = ""`, `C/views/newchat/AddGroupView.kt:159-161`).
- The join line comes from `createGroup2'` (`H/Utils.hs:822-825`).
- The role line comes from `testUpdateMemberRole` (`H/Groups.hs:1698-1701`).

**Platform differences:** none. The group chat is still open after `createGroup` on both platforms, so `ownGroup` returns that chat screen instead of reopening it from the list.

**Uncertainty:**
- I assume the `ExposedDropdownMenuBox` popup can be clicked on Android, as it already is in `appearance_settingsPersistAcrossRestart`.
- I assume `option(GroupMemberRole.Admin)` produces the tag `[Admin]` on both sides; enum `toString` returns the name.

### 14. `memberInfo_removeMember`

**Catalogue:** 03:175. Needs P ×2.

**What the test asserts:**
- With bob and cath in alice's #team, Remove member → "Remove".
- bob prints `#team: alice removed you from the group (signed)`.
- cath prints `#team: alice removed bob from the group (signed)`.
- bob is no longer a current member in the model.

**Why this is the expected behaviour:**
- `removeMemberDialog` group branch (`GroupMemberInfoView.kt:304-333`), then `removeMember` (`:345-363`).
- String `remove_member_confirmation` = "Remove" (`S:2022`).
- Haskell: `testGroupRemoveAdd` (`H/Groups.hs:1148-1155`).

**Platform differences:** none.

**Uncertainty:** cath's line depends on alice's broadcast reaching cath. Cath connects to alice when she joins, so the line does not depend on the bob–cath introduction finishing.

---

## Rows considered and skipped

| Row | Reason |
|---|---|
| `delete_ownForMe_localOnly` (02:150) | The row's Needs is `P, S` (stable-compat class), which is outside the "—"/"P" scope. |
| `copy_text_toClipboardAndToast` and all of 02 §N | Every row needs `CB` and/or `TOAST`. |
| 02 §G receive-file rows | They are marked `XFTP`; the task's filter was strictly "—"/"P". |
| `contactInfoClearVerification` (01:254) | In scope, and the next one to add. It needs one more key, `Ui.VerifyCode.Clear`, on the "Clear verification" `SimpleButton`. |
| `forward_withComment_sendsCommentQuoted`, `forward_groupToContact`, … (02:165-166) | In scope and lower priority. They would reuse `forwardTo` plus `typeInto(Ui.Chat.Message)` before Send. |
| `search_group_filtersResults`, `search_close_restoresAll`, `search_notes_filtersResults` (02:230-231) | In scope. They reuse `ChatScreen.search`. The close test also needs a key on the search back button (`NavigationButtonBack` in `ChatView.kt:1445`). |
| `profilesActivateByTap`, `profilesSearchByName` (01:122) | In scope. They need a key on the `UserProfilePickerItem` rows (`C/views/chatlist/UserPicker.kt:403`). |
| `memberInfo_removeMemberWithMessages` (03:175) | Same setup as the remove test. It needs `Ui.Alert.Option(Shared, "removeWithMessages")` on the second button of `removeMemberDialog`. |
| `groupInfo_membersSectionCountAndSelfRow` (03:68) | **The catalogue is wrong here.** It says the self row reads «you: %1$s» "formatted with alice's name". The code formats it with `member.memberStatus.shortText` (`C/views/chat/group/GroupChatInfoView.kt:1150-1151`), so for the creator it reads "you: creator" (`group_member_status_creator`, `S:1880`). The catalogue row needs fixing before the test is written. |
| `toolbarMenu_muteUnmute_togglesNtfMode` (02:28), `groupInfo_muteCycleWithMentions` (03:61) | The mute modes are covered by test 2. These need keys on the toolbar ntfs item and on the group-info mute button. |
| `contactMenuMarkRead`, `contactMenuMarkUnread` (01:84) | Already covered in the repo by `contactMenuMarkReadAndUnread`, under a different name. |
| `searchFiltersByAlias`, `searchNoChatsFound`, `searchClearsWhenFilterChanges` (01:62) | In scope and cheap to add with `ChatListScreen.search`. `searchNoChatsFound` needs the "No filtered chats" text (`S:504`) or a key. |
| `userPickerUnreadBadgePerUser` (01:120), `profilesHide*` (01:124) | Lower value; the hide flows also need password-field keys. |
| Reports, moderation, support chats, channels, business (03 §12-14, §18-20) | They need ×2-×3 peers or `RELAY`; lower priority than the core flows. |
