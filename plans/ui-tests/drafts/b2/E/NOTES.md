# Draft E: chat-list rows (catalogue 01 §3, rows 75–82)

## Deliverables

| File | Goes to | Contents |
|---|---|---|
| `main/ChatListRowsTest.kt` | `common/src/uiTestShared/kotlin/chat/simplex/app/ui/main/ChatListRowsTest.kt` | 16 shared tests, class `ChatListRowsTest` (class map: `previewShowsLastTextMessage` … up to `contactMenuMarkRead`) |
| `screens.kt` | append to `harness/Screens.kt` (or a new harness file) | `hasRowMark`, `switchAwayFromChat`, `closeInfoToChatList`, `joinGroupFromList`, `connectIncognito` |
| `keys.diff` | applies with `git apply` on `/home/user/wt-uitest` (checked with `git apply --check`) | `Modifier.uiMarker`, `Ui.Chat.Connect`, row marks `draft`/`verified`/`incognito`/`link` |

All 16 tests are shared (desktop and Android), with no `appPlatform` branches. Nothing was compiled or run.

Paths: `C/` = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`, `CPV` = `C/views/chatlist/ChatPreviewView.kt` (identical in `~/wt-uitest` and `~/wt-uitest-ref`), `S` = `strings.xml`, `Hs/` = `src/Simplex/Chat/`, `H/` = `tests/ChatTests/`.

## Keys and how rows are read

The row key `Ui.ChatList.Row(chatId)` sits on the row's `combinedClickable`, so the row is one merged node (`D/views/chatlist/ChatListNavLinkView.desktop.kt:40-43`, the Android actual at `:26-30`).
- `rowTexts` = `textsOf(Row)`: the merged node's own texts plus every unmerged descendant's text (desktop `UiTest.kt:238-245`, Android `:261-267`). So every `Text` in the row is visible. That includes the unread badge, the timestamp and the preview text.
- `rowDescriptions` = `descriptionsOf(Row)`: content descriptions of the row and its unmerged descendants (desktop `:247-252`, Android `:271-275`). Icons with a description can be checked this way: the mute icon and both mention icons read "Notifications" (`CPV:479,489,505`, `S:704`) and the star reads "Favorite" (`CPV:515`, `S:858`).
- `exists(key)` searches the **merged** tree (desktop `UiTest.kt:106`, Android `:133`). A plain `testTag` on a child of the row is merged away, because TestTag's merge policy keeps the parent's value (plan §3.2). So a `Modifier.ui(key)` on a non-clickable icon inside a row can never be found.

Icons with no content description: the verified shield (`CPV:98`), the incognito mask (`CPV:561`) and the draft edit icon (`CPV:126`). Each gets a key through a new modifier in `Ui.kt`:

```kotlin
fun Modifier.uiMarker(key: UiKey?): Modifier =
  if (key == null || !uiTagsEnabled) this else semantics(mergeDescendants = true) { set(SemanticsProperties.TestTag, key.tag) }
```

The `mergeDescendants` node stays its own node in the merged tree, the same way nested `IconButton`s do (b1/A relies on this too), so `exists` finds it. Like `ui`, it does nothing unless tags are enabled, so release builds are unchanged. The icons have no other semantics, so neither the row's merged texts and descriptions nor `tapPointInNode` (which only avoids `OnClick` descendants, `TapPoint.kt:18-19`) are affected.

Keys are parts of the row key, `Ui.ChatList.Row(chat.id, chat.remoteHostId).part(x)`, giving the tags `chatList.row.<x>[<chatId>]`:

| Part | Where | Modifier |
|---|---|---|
| `verified` | `VerifiedIcon`, `CPV:97-99` | `uiMarker` |
| `draft` | the draft's inline edit icon, `CPV:123-127` | `uiMarker` |
| `incognito` | `IncognitoIcon(incognito, uiKey)`, `CPV:557-568`; both calls in `chatStatusImage`, `CPV:395,398` (the `ContactConnectionView.kt:62` call keeps the default `null`) | `uiMarker` |
| `link` | the link-preview `IconButton`, `CPV:321-324` (clickable, so plain `ui`) | `ui` |

Also new: `Ui.Chat.Connect` on the "Connect" button of `SendContactRequestView` (`C/views/chat/ComposeView.kt:1543-1550`). It goes through a new `uiKey` parameter of `SimpleButtonIconEnded`, which passes `Modifier.ui(uiKey)` to `SimpleButtonFrame`, ahead of its `clickable` (`C/views/helpers/SimpleButton.kt:73-99`). This mirrors the existing local change to `SimpleButton`. `ConnectButtonView` (`ComposeView.kt:1556`, invitation links and bots) is not keyed here.

## Helpers (`screens.kt`)

- `hasRowMark(name, mark)`: `exists(Ui.ChatList.Row(chatId(name)).part(mark))`.
- `switchAwayFromChat(app)`: `backToChatList()`, open "Private notes" (`S:90`, the notes folder every new user has, as in `NotesTest`), wait for it to be the open chat, then `backToChatList()` again. A draft is saved only when the open chat changes (`KeyChangeEffect(chatModel.chatId.value)`, `ComposeView.kt:1381-1420`). Desktop has no way to close a chat: the chat's `BackHandler` is Android-only (`ChatView.kt:1226-1228`). Switching to the notes folder changes the chat on both platforms. On Android the first `backToChatList()` already closes the chat.
- `closeInfoToChatList()`: `escape()` closes the info modal (`ModalView.kt:41`; on desktop through the window back stack), then `backToChatList()`.
- `joinGroupFromList(app, host, group)`: clicks the invitation row (`groupChatAction`, `ChatListNavLinkView.kt:204-207`), then "Join" (`Ui.Alert.Confirm(Shared)`, `:920-931`). Waits for the host's `#g: alice joined the group` and for an active membership (as in `ChatListMenusPendingTest`), then calls `quiesce()`. Joining from the list does not open the chat, so incoming messages stay unread.
- `connectIncognito(peer)`: `connect()` with `apiAddContact(incognito = true)`. Returns alice's random name as the CLI prints it, `<name>: contact is connected` (`H/Profiles.hs:702`).

## Tests

### `previewShowsLastTextMessage` (01:75)
- bob sends two messages. The row shows the second, and the first is gone. alice replies from the chat; after `backToChatList()` the row shows her reply in place of bob's message.
- Code: `chatPreviewText`, `CPV:251-304` (`ci.text`, last item); CLI `alice> my reply` (`eventuallyMessage`).

### `previewShowsDraft` (01:75)
- alice opens bob's chat, types "unsent draft" and switches away (`switchAwayFromChat`). Then:
  - `chatModel.draftChatId` is bob's chat.
  - The row text ends with " unsent draft". The text is `"� unsent draft"`: `appendInlineContent` puts the default alternate text U+FFFD and then " " in front of it (`CPV:111-113`).
  - bob's "hello" is gone from the row (the draft branch replaces the preview, `CPV:229-248`).
  - The `draft` mark exists.

### `previewLink` (01:75)
- bob sends a link item with `/_send @<id> json [{"msgContent":{"type":"link","text":…,"preview":{uri,title,description,image}}}]`. The JSON shape is the one in `tests/ProtocolTests.hs:231`; `/_send … json` is in `H/Direct.hs:902`. The image is a valid 1×1 PNG, because `base64ToBitmap` decodes it (`Images.desktop.kt:30-38`).
- Asserts: the row text "https://simplex.chat" and the `link` mark (`MCLink` → `SmallContentPreview` with an `IconButton`, `CPV:318-330`).

### `previewDeletedMessage` (01:75)
- bob sends a message, then deletes it with `\@alice to be deleted` (`DeleteMessage`, parser `Hs/Library/Commands.hs:6405`, handler :2685-2688, broadcast).
  - There is no space after `\`: typing `\` and then a space auto-inserts the current chat (`Hs/Terminal/Input.hs:356-357`).
  - The CLI prints `message marked deleted` (`Hs/View.hs:938`, `chatItemDeletedText` :643; also `H/Direct.hs:827`).
- The row reads "marked deleted" (`CPV:253`, `MarkedDeletedItemView.kt:109-110`, `S:50`), and the original text is gone.

### `previewContactCardMessage` (01:75)
- bob sets his bio with `/p bob Coffee lover`, which prints `user bio changed to Coffee lover (your 0 contacts are notified)` (`Hs/View.hs:2042`; parser `Commands.hs:6448,6624-6626`).
- bob creates an address (`CliPeer.createAddress`), connects, and runs `/share address @alice` (`Commands.hs:2572-2583,1247-1259`). His view prints `@alice contact address of @bob[ (signed)]:` (`View.hs:2435-2447`; `H/Profiles.hs:3945-3946`).
- The row text is `"bob\nCoffee lover"`, the name plus the short description (`CPV:254-258`, `MsgChatLink.shortDescription`, `C/model/ChatModel.kt:5087-5095`).

### `previewGroupSenderPrefix` (01:77)
- bob hosts "team"; alice joins from the list.
- alice's own message "mine" shows with no prefix. bob's "#team hello all" shows as "bob: hello all": `sender` is set only for received, non-channel-sender group items (`CPV:280-283`), and `appendSender` adds `"<name>: "` (`TextItemView.kt:38-44`).

### `unreadCounterGrayWhenMuted` (01:79)
- alice mutes bob from the row menu (`ntfs.None`; contacts cycle All↔None). bob sends two messages.
- Asserts:
  - The badge reads "1" and then "2".
  - `chatSettings.enableNtfs == None`.
  - There is no "Notifications" icon: with unread messages the badge replaces the mute icon (`CPV:464,502`).
- The grey colour (`CPV:497`) shows only in the screenshot.

### `mentionBadgeSingleUnread` (01:80)
- bob hosts "team", alice joins, and the row has no "Notifications" description. bob sends `#team hello @alice` (`H/Groups.hs:8198-8200`).
- Asserts:
  - `unreadCount == 1` and `unreadMentions == 1` (`ChatModel.kt:597`).
  - The row has "Notifications" (the round @ badge, `CPV:485-493`) and no "1" text.
- Join events do not make the group unread (`ciRequiresAttention`, `Hs/Messages/CIContent.hs:230-247`).

### `mentionIconWithMultipleUnread` (01:80)
- Same setup and mention, then one more message. Asserts `unreadCount == 2` and `unreadMentions == 1`, the @ icon "Notifications" (`CPV:476-483`) and the badge "2" (`:494-499`).

### `mutedIconWhenNoUnread` (01:81)
- After muting, the row has "Notifications". A message from bob replaces it with the badge "1". "Mark read" from the row menu brings the icon back, with no badge (`CPV:464,502-511`).

### `favoriteStarWhenNoUnread` (01:81)
- The same cycle with "Favorite": the star shows, a message replaces it with the badge, and "Mark read" brings it back (`CPV:512-521`).

### `incognitoIconOnRow` (01:81)
- alice connects to bob through an incognito 1-time link (`connectIncognito`).
- Asserts:
  - `contactConnIncognito` is true (`ChatModel.kt:1938`).
  - bob's row has the `incognito` mark (`chatStatusImage` → `IncognitoIcon`, `CPV:398,557-568`).
  - The "Private notes" row does not have it.

### `verifiedIconOnRow` (01:81)
- At first there is no `verified` mark. alice verifies bob from his contact info (`ContactInfoScreen.markVerified`), and the model shows him verified (`Contact.verified`, `ChatModel.kt:1919`).
- After `closeInfoToChatList()`, the row has the `verified` mark (`CPV:143-145`).

### `timestampShown` (01:81)
- bob sends a message. The row texts contain `getTimestampText(item.meta.itemTs)` (`CPV:417-418`, `ChatModel.kt:3955`) for the last item in the model.
- The test calls the app's own formatter, so locale and time zone match on both platforms.

### `previewContactShouldAcceptForAddress` (01:82)
- bob creates an address with no auto-accept (`/ad`; lines from `View.hs:1139-1149`). alice opens it through `planAndConnect(null, address, close = null)`. This is the function the search field and the paste sheet call, called directly because typing a link into the search field cannot use `typeInto` (b1/C notes).
- Then:
  - "Open new chat" (`Ui.Alert.Confirm(PrivacySensitive)`, `ConnectPlan.kt:155-168,699-741`).
  - "Connect" (`Ui.Chat.Connect`, new; `ComposeView.kt:1806-1819,1524-1553`).
  - "Send request without message" (`Ui.Alert.Confirm(Shared)`, `ComposeView.kt:672-691`).
- bob prints `alice wants to connect to you!` (`View.hs:1263-1265`). The row reads "contact should accept…" (`CPV:194-196`, `S:513`). It needs a prepared contact with `uiConnLinkType == Con` that is not a bot and is not `sndReady`.
- The request is sent without a message on purpose: a message would become the last item and replace this text (`CPV:249`).

### `previewGroupInvitationJoinAs` (01:82)
- bob (a normal contact) invites alice to "team", and the row reads "You are invited to group" (`S:488`).
- cath is connected incognito (`connectIncognito`) and invites alice's random name to "club" (`/a club <name>`, `H/Groups.hs:2895-2900`). The row reads "Join as <name>" (`S:489`; `groupInvitationPreviewText`, `CPV:583-588`; `memberIncognito`, `ChatModel.kt:2972`).
- The incognito invitation uses a second peer, so that bob is not connected to alice twice, where contact probes could interfere.

## Uncertainties (top first)

1. **`uiMarker` is new app-side API in `Ui.kt`.**
   - It relies on a `mergeDescendants` child surviving as its own node inside the row's merged node. Compose handles nested `IconButton`s the same way, but I have not seen it confirmed for a node with only a tag.
   - In debug Android builds it adds an empty a11y-mergeable node per marked icon.
   - Alternative: plan §4 typed semantics on the row node (`VerifiedKey`, `IncognitoKey`), which is a larger change.
2. **The draft icon is inline content.** `InlineTextContent` children are composed under the `Text`'s layout node, so their semantics should be children of the text node. If they are not, drop `hasRowMark("bob", "draft")` and rely on the `"� "` prefix plus `draftChatId`.
3. **Android draft saving on Back** relies on `KeyChangeEffect(chatModel.chatId.value)` running before `ComposeView` leaves composition (the comment at `ComposeView.kt:1446` says Android uses it).
4. **`previewContactShouldAcceptForAddress`:**
   - It assumes the CLI's short link for the local SMP server parses in `strConnectTarget` (the same assumption as b1/C).
   - `planAndConnect` runs in `runBlocking` on the test thread. The app normally calls it from `withBGApi`.
5. **`previewLink`** assumes the API JSON for `MsgContent` is the wire shape of `ProtocolTests.hs:231`.
6. **Overlaps with other drafts:**
   - `joinGroupFromList` duplicates the inline join in `ChatListMenusPendingTest`.
   - F (`contactMenuForNotReadyContactOnlyDelete`) may add its own prepared-contact helper and a key for the same "Connect" button. Pick one key name.
7. **`unreadCounterIncrementsAndClears`** (same catalogue row 79) already exists in `chat/MessagingTest.kt`, but the class map puts it in `main/ChatListRowsTest`. Move it, or the traceability check will report it in the wrong class.

## Catalogue corrections

1. **`01-onboarding-chatlist-connect-profiles.md:75`, Verification cell** ("Text, draft (edit icon), link image, "marked deleted", contact-card name and description").
   - Problem: it is incomplete.
     - A draft shows only after the chat is left (saved on chat change; desktop has no way to close a chat), and it replaces the last-message preview.
     - The edit icon, the verified shield and the incognito mask have no content description.
     - The "link image" is a button, and it exists only for link-preview messages (`MCLink`), which the CLI sends only as JSON.
     - The card's description line appears only when the profile has a bio.
   - Replacement:
     > `previewShowsLastTextMessage`: the last item's text, received or sent; `previewShowsDraft`: after alice types in bob's chat and switches to another chat (`ComposeView.kt:1381-1420`), the row shows the draft text after the edit icon (inline, no content description, `ChatPreviewView.kt:123-127`) in place of the last message (`:229-248`); `previewLink`: a link message (CLI `/_send @<id> json` with a `link` content and preview image) shows the link text and a thumbnail button that opens the link alert (`:318-330`); `previewDeletedMessage`: a message deleted by bob reads "marked deleted" (`:253`); `previewContactCardMessage`: bob's `/share address @alice` reads his name and, on a second line, his bio (`/p bob <bio>`; `:254-258`)
2. **`…:77`, Verification cell** ("“bob: …” sender").
   - Problem: it is incomplete. alice's own messages, and items shown as sent by the group, have no prefix (`ChatPreviewView.kt:280-283`).
   - Replacement:
     > A received group message reads "bob: <text>" (`appendSender`, `TextItemView.kt:38-44`); alice's own message has no prefix
3. **`…:79`, Verification cell, the clause** "the row's typed `MutedKey` (plan §4) reads ntfsMode `None` (`MsgFilter.None`)".
   - Problem: the harness has no typed semantics (plan §4 is not implemented).
   - Replacement:
     > the chat's `chatSettings.enableNtfs` is `MsgFilter.None` and the row shows no mute icon ("Notifications") while the badge is shown (`ChatPreviewView.kt:464,502`)
4. **`…:81`, Verification cell** ("Mute icon, star, incognito mask, verified shield, timestamp").
   - Problem: it is incomplete.
     - The mute icon and the star show only when the row has no unread badge, and the mute icon wins over the star.
     - On group rows the incognito mask gives way to the report, support and pending flags.
     - The mute icon's description is the generic "Notifications" (the same as the mention icons).
     - The shield and the mask have no description.
   - Replacement:
     > `mutedIconWhenNoUnread`: a muted chat without unread messages shows the mute icon ("Notifications", `ChatPreviewView.kt:502-511`); a message replaces it with the unread badge, and "Mark read" brings it back; `favoriteStarWhenNoUnread`: the same with the star ("Favorite", `:512-521`; not shown when muted); `incognitoIconOnRow`: an incognito contact's row shows the mask (no description, `:557-568`; on group rows only without a report/support/pending flag, `:385-396`); `verifiedIconOnRow`: a verified contact shows the shield before the name (no description, `:97-99,143-145`); `timestampShown`: the row shows `getTimestampText` of the last item (`:417`)
5. **`…:82`, Verification cell** ("“contact should accept…”, “You are invited to group” / “Join as %s”").
   - Problem: the condition for "contact should accept…" is missing. It needs a prepared contact from an address (not a bot) whose request was sent without a message, or a member contact (`ChatPreviewView.kt:194-196`). With a request message, the message is the preview (`:249`), and other pending contacts read "connecting…" (`:197-198`).
   - Replacement:
     > `previewContactShouldAcceptForAddress`: alice opens bob's address (no auto-accept), "Open new chat", "Connect", "Send request without message": the row reads "contact should accept…" until bob accepts; `previewGroupInvitationJoinAs`: an invitation reads "You are invited to group", and one received through an incognito contact reads "Join as <incognito name>" (`:583-588`)

No rows were skipped.
