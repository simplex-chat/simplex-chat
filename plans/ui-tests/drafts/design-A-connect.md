# Design A: connecting to people (catalogue 01 §8 and §9)

Status: design only, nothing implemented. Written 2026-10-20 against `~/wt-uitest` (branch `nd/desktop-ui-tests`,
HEAD `8927c1892`). Every `file:line` below was read in this tree; paths are relative to `apps/multiplatform/common/src/`
unless they start with `src/` or `tests/` (Haskell, repo root). UI strings are from
`commonMain/moko-resources/base/strings.xml` (the brief said `MR/base`; that path does not exist).

Rules followed: `uiTestShared/README.md` (whole guide), plan §0.5, legend `catalogue/00-legend.md`.
`status.py`: none of the 40 names in §8/§9 has a test yet.

## 0. Summary

| # | Test (file / class) | Catalogue rows covered | Peers | Route |
|---|---|---|---|---|
| 1 | `pasteLinkInvalidTextAlert` (main/ConnectPlanTest) | §8 Paste (`pasteLinkInvalidTextAlert`, `pasteLinkClipboardEmptyNoop`) | 0 | New chat › Paste link |
| 2 | `planOwnInvitationConnectToYourself` (ConnectPlanTest) | §8 Invitation own link, Address own address | 0 | search |
| 3 | `planInvitationAlreadyConnectingFullLink` (ConnectPlanTest) | §8 Invitation full link (`…UseCurrentProfile`, `…Cancel`), Already connecting | 1 (full links) | search |
| 4 | `planAddressRepeatConnectionRequest` (ConnectPlanTest) | §8 Address full link (`planAddressFullLinkConnectAlert`, incognito button), Repeat request | 1 (full links) | search |
| 5 | `planViaChatItemLinkCard` (ConnectPlanTest) | §8 Card in message, Signed address card (chat list), Card contents, Address Known, Invitation Known (same alert code) | 2 | chat-item card, chat-list card, search |
| 6 | extend existing `searchPasteLinkTriggersConnectPlan` (ChatListToolbarSearchTest) | §8 Deleted link → error (re-targeted to a deleted address) | (existing 1) | search |
| 7 | `preparedContactInvitationConnectButton` (main/PreparedChatTest) | §8 Invitation short link OK; §9 Invitation (3 names) | 1 | search |
| 8 | `preparedContactAddressSendRequestWithMessage` (PreparedChatTest) | §8 Address short link OK; §9 Address request (`…WithMessage`, `…AlertAddMessage`, `…AfterConnectShowsContactShouldAccept`) | 1 | search |
| 9 | `preparedBotConnect` (PreparedChatTest) | §8 Bot address; §9 Bot | 1 | search |
| 10 | `preparedContactIncognitoBeforeConnect` (PreparedChatTest) | §9 Profile/incognito (incognito half) | 1 | search |
| 11 | `preparedContactChangeProfileBeforeConnect` (PreparedChatTest) | §9 Profile/incognito (profile half) | 1 | search |

10 new tests plus one extension of an existing test. Each plan alert kind (Ok short with link data, Ok full without it,
OwnLink ×2, Connecting, ConnectingConfirmReconnect, Known, error) and each compose-area variant (invitation Connect,
address request with/without message, bot Connect, profile picker: incognito / other profile) is exercised once, and
every alert button (`Use current profile`, `Use new incognito profile`, `Cancel`, `Open new chat`, `Open chat`) is
clicked at least once.

### Why the chat-list search is the main entry

- New chat › "Paste link" on Android opens the QR scanner at once (`newchat/NewChatSheet.kt:62-64`,
  `showQRCodeScanner = appPlatform.isAndroid`); Waydroid has no camera and leaving the scanner crashes the app (plan
  §15.5 item 4). A paste-route test must start with `assumeCamera()` and is skipped on Waydroid. So only test 1 (the
  paste row's own behaviour) uses that route.
- The search field (`Ui.ChatList.Search`, existing key, `chatlist/ChatListView.kt:837`) runs the same `planAndConnect`
  (`ChatListView.kt:873-881,924-934`) on both platforms with no camera and no clipboard. Its one difference: Known
  contacts are filtered into the list instead of an alert (`filterKnownContact`, `ChatListView.kt:929`), which an
  existing test already covers (`searchPasteKnownContactLinkFiltersToContact`). The "Open chat" alert of Known plans is
  therefore tested through the chat-item card (test 5), which calls `planAndConnect` with no filter
  (`chat/item/FramedItemView.kt:409-411`).

## 1. App changes (keys only)

All are no-ops unless `uiTestHooksEnabled` (`Modifier.ui`, `common/ui/Ui.kt:18`). Every key below is clicked by a test.

| Key | Ui.kt change | Element (file:line) | Used by |
|---|---|---|---|
| `Ui.NewChat.PasteLink` | new: `val PasteLink = UiKey("newChat.pasteLink")` in `object NewChat` (`Ui.kt:53-56`) | the "Paste link" / "Paste link / Scan" action: third argument `null` → `Ui.NewChat.PasteLink` at `commonMain/kotlin/chat/simplex/common/views/newchat/NewChatSheet.kt:193` | 1 |
| `Ui.Link.Paste` | new: `val Paste = UiKey("link.paste")` in `object Link` (`Ui.kt:142-146`) | "Tap to paste link" row: `SectionItemView({ … }, uiKey = Ui.Link.Paste) {` at `views/newchat/NewChatView.kt:681` | 1 |
| `Ui.Alert.Option(Ui.AlertSource.PrivacySensitive, "useCurrentProfile")`, `…"useIncognito"`, `…"cancel"` | none (existing `Option` family, `Ui.kt:202`) | the three `SectionItemView`s of `askCurrentOrIncognitoProfileAlert`: `views/newchat/ConnectPlan.kt:469` ("Use current profile"), `:477` ("Use new incognito profile"), `:493` ("Cancel"); add `uiKey = …` (SectionItemView takes `uiKey` before `content`, `views/helpers/Section.kt:200-210`, same call shape as `usersettings/UserProfilesView.kt:85`). The alert is always `AlertManager.privacySensitive` (`ConnectPlan.kt:463`). | 1, 2, 3, 4 |
| `Ui.Chat.Connect` | none (existing, `Ui.kt:151`) | also on `ConnectButtonView`: `var modifier = Modifier.height(57.dp).fillMaxWidth().ui(Ui.Chat.Connect)` at `views/chat/ComposeView.kt:1576`. Today the key is only on the address-request button (`ComposeView.kt:1563`); the invitation and bot "Connect" (`:1816-1827`) and "Join group" (`:1785`) have none. Same meaning, mutually exclusive branches (`:1782-1835`). | 7, 9, 10, 11 |
| `Ui.Chat.PickerUser(userId)` | new: `fun PickerUser(userId: Long) = UiKey("chat.picker.user", userId)` in `object Chat` | `ProfilePickerUserOption` row: `.ui(Ui.Chat.PickerUser(user.userId))` in the `Row` modifier at `views/chat/ComposeContextProfilePickerView.kt:124-127` | 10, 11 |
| `Ui.Chat.PickerIncognito` | new: `val PickerIncognito = UiKey("chat.picker.incognito")` | `IncognitoOption` row modifier at `ComposeContextProfilePickerView.kt:167-170` (the nested ⓘ at `:194-205` is avoided by `tapPointInNode`) | 10 |
| `Ui.Chat.Item(id).part("chatLink")` | none (`part` is generic, as `.part("quote")` at `FramedItemView.kt:297`) | chat-link card: `Modifier.ui(Ui.Chat.Item(ci.id).part("chatLink")).combinedClickable(` at `views/chat/item/FramedItemView.kt:409` | 5 |
| `Ui.ChatList.Row(id, rh).part("link")` | none (existing, already on the link-preview thumbnail `chatlist/ChatPreviewView.kt:326`) | chat-link card thumbnail on the row: `Modifier.fillMaxSize().ui(Ui.ChatList.Row(chat.id, chat.remoteHostId).part("link")).clickable { … }` at `chatlist/ChatPreviewView.kt:357` | 5 |

Not added: no key on the alert's profile image, the bot icon, banners or info lines (read by text), and none on
`showOpenChatAlert`'s "connect other" button (names need `NAMES`).

## 2. Harness additions

| Helper | File | Signature and body sketch |
|---|---|---|
| `setClipboardText` | desktop `desktopTest/…/harness/UiTest.kt` and Android `android/…/harness/UiTest.kt` (same name on both `UiScope`s, plan §0.5.1) | desktop: `fun setClipboardText(text: String?) { clipboard.content = text?.let { AnnotatedString(it) } }` (`TestClipboard`, `TestRoot.kt:11-15`); Android: `fun setClipboardText(text: String?) = UiTestApp.onMain { val cm = androidAppContext.getSystemService(ClipboardManager::class.java); if (text == null) cm.clearPrimaryClip() else cm.setPrimaryClip(ClipData.newPlainText("uitest", text)) }` (`clearPrimaryClip` needs API 28; an empty clip is not "no text": `getText()` would return `""`, which the app treats as invalid, `NewChatView.kt:682-694`) |
| `planOption(name)`, `planDismiss` | new `uiTestShared/…/harness/ConnectScreens.kt` | `fun planOption(name: String) = Ui.Alert.Option(Ui.AlertSource.PrivacySensitive, name)`; `val planDismiss = Ui.Alert.Dismiss(Ui.AlertSource.PrivacySensitive)` (next to the existing `planConfirm`, `ChatListFilterScreens.kt:98`, which moves here) |
| `UiScope.openPasteLink(app)` | `ConnectScreens.kt` | `app.newChat(); click(Ui.NewChat.PasteLink); poll(what = "the `Paste the link you received` section with `Tap to paste link`") { seesText("Paste the link you received") && "Tap to paste link" in textsOf(Ui.Link.Paste) }` |
| `UiScope.openNewChatFromSearch(link, name): ChatScreen` | `ConnectScreens.kt` | `enterLinkInSearch(link)`; `poll(CONNECT_TIMEOUT_MS, "the plan alert for `$name` with `Open new chat`") { exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText(name) }`; `click(planConfirm)`; `poll(what = "`$name`'s prepared chat open") { chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == name && it.chatInfo.nextConnectPrepared }?.id?.let { it == chatModel.chatId.value } == true }`; `return ChatScreen(this)`. Used by 7–11 (the plan alert itself is asserted inside the poll). |
| `ChatScreen.chatLinkItemId(): Long` | `Screens.kt` (`ChatScreen`, next to `itemId`, `Screens.kt:222-229`) | `poll(what = "a chat link card") { id = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.msgContent is MsgContent.MCChat }?.id; id != null && s.exists(Ui.Chat.Item(id!!)) }` |
| `CliPeer.createAddress(business = false, autoAccept = true)` | change `ChatListFilterScreens.kt:80-88` | add `autoAccept`; when `false`, skip `/auto_accept` and consume the rest of the `/ad` output with `eventually("to delete it: /da (accepted contacts will remain connected)")` (`src/Simplex/Chat/View.hs:1144-1154`). Existing callers unchanged. |
| `CliPeer.createFullAddress(): String` | `ChatListFilterScreens.kt` | peer started with `PeerFlags(fullLinks = true)`: `this sends "/ad"; this expects "Your new chat address is created!"; return eventuallyMatching(Regex("The contact link for old clients: (\\S+)")).groupValues[1]` (`View.hs:1154`) |
| `CliPeer.createFullOneTimeLink(): String` | `Helpers.kt` (next to `createOneTimeLink`, `Helpers.kt:56-61`) | `this types "/c"; eventually("The invitation link for old clients:"); return nextLine()` (`View.hs:1071-1086`) |
| `CliPeer.becomeBot()` | `ChatListFilterScreens.kt` | `this sends """/_profile 1 {"displayName":"$name","fullName":"","peerType":"bot"}"""; this sends "/u"; eventually("user profile: $name (bot)"); this expects "use /p <name> [<bio>] to change it"` (`/_profile` parser `Library/Commands.hs:6399`; `peerType` text `"bot"`, `Types.hs:766-778`; `/u` lines `View.hs:1579-1592`, suffix ` (bot)` `View.hs:1597`). The `/u` round trip flushes whatever `/_profile` printed (preference lines, if any). Must run before `/ad` so the address link data carries the bot profile. |
| `PeerLines.eventuallyAll(lines, messages = emptyList(), timeoutMs)` | `PeerLines.kt` (next to `eventuallyMatching`, `PeerLines.kt:46-52`) | waits until every `lines` entry is present exactly and every `messages` entry matches after `stripTime`, then `dropThrough` the last matched index. Needed because the CLI prints a contact request and its message in either order (Haskell uses `<###`, `tests/ChatTests/Profiles.hs:4184-4187`). |

No `quiesce()` is added to hide a race; existing uses after setup only.

## 3. Tests

Conventions in the pseudo-code: `alice = app("alice")`; `waitAlert(title)` = `poll(CONNECT_TIMEOUT_MS, "…") { seesText(title) }`
when the alert follows a network plan, plain `poll` otherwise. Alert bodies of `askCurrentOrIncognitoProfileAlert`
end with the link (`ConnectPlan.kt:83,108`), so they are matched with `seesText(text, substring = true)`.

---

### Test 1. `pasteLinkInvalidTextAlert` — `main/ConnectPlanTest`

Rows: §8 "Paste: Invalid or empty clipboard" (both names; `pasteLinkClipboardEmptyNoop` merged).

**Scenario.** The user opens New chat › Paste link and taps "Tap to paste link": with nothing copied nothing happens;
with a web link copied they are told it is not a SimpleX link; with a SimpleX link copied the connection plan starts.

**Fails if** the paste row stops reading the clipboard correctly or stops rejecting non-SimpleX text. Mutations:
- `views/newchat/NewChatView.kt:682` `?: return@SectionItemView` → `?: AnnotatedString("")`: the empty clipboard shows
  "Invalid link!" → step 2 fails;
- delete the `null -> AlertManager.shared.showAlertMsg(…)` branch at `NewChatView.kt:694-697` (make it `null -> {}`) →
  step 3 fails;
- `NewChatView.kt:684-688` drop the `withBGApi { connect(…) }` call → step 4 fails (no plan alert).

**Setup.** No peer. `val address = alice.userPicker().createAddress()` (`Screens.kt:127-130`; returns alice's own
address; on desktop it also lands on the clipboard stub, so step 2 clears it).

**Steps and assertions.**
```kotlin
assumeCamera()                                   // Android: the paste screen starts the scanner (§15.5 item 4)
val address = alice.userPicker().createAddress()
openPasteLink(alice)                              // NEW, ConnectScreens.kt
// 2. empty clipboard: nothing happens
setClipboardText(null)                            // NEW
click(Ui.Link.Paste)
settle()
assert(exists(Ui.Link.Paste) && "Tap to paste link" in textsOf(Ui.Link.Paste))    // presence first
assert(!seesText("Invalid link!") && !exists(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && !exists(planConfirm))
// 3. web link: rejected
setClipboardText("https://example.com/invite")
click(Ui.Link.Paste)
poll { seesText("Invalid link!") && seesText("The text you pasted is not a SimpleX link.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll { !seesText("Invalid link!") && exists(Ui.Link.Paste) }                         // row still offered
// 4. positive control: a SimpleX link starts the plan
setClipboardText(address)
click(Ui.Link.Paste)
poll { seesText("Connect to yourself?") && !exists(Ui.Link.Paste) }                   // row replaced by the link
click(planOption("cancel"))
poll { !seesText("Connect to yourself?") && exists(Ui.Link.Paste) }                   // cleanup resets the row
```
Strings: "Paste the link you received" (`paste_the_link_you_received`), "Tap to paste link", "Invalid link!"
(`invalid_contact_link`), "The text you pasted is not a SimpleX link.", "Connect to yourself?"
(`connect_plan_connect_to_yourself`). The positive control is what makes steps 2–3 meaningful (a dead row would also
show no alert); it uses alice's own address so no peer is needed. Opposite-first: step 2 asserts the row and the
absence of alerts before step 3 expects an alert.

**Keys:** `Ui.NewChat.PasteLink`, `Ui.Link.Paste`, `planOption("cancel")` (new); `Ui.Alert.Confirm(Shared)`,
`planConfirm` (existing).
**Peer:** none.
**Platform:** desktop runs it; Android skips on devices without a camera (Waydroid) via `assumeCamera()` (guide §3
table), and needs `setClipboardText` with `clearPrimaryClip`. **Flakiness:** none expected; the empty-clipboard check
is a `settle()` followed by presence + absence, as the guide requires.

---

### Test 2. `planOwnInvitationConnectToYourself` — `ConnectPlanTest`

Rows: §8 "Invitation: Own link" and "Address: Own address" (`planOwnAddressConnectToYourself` merged; same alert
function, different text and outcome).

**Scenario.** A user pastes their own SimpleX address and then their own 1-time link. Each time the app warns
"Connect to yourself?"; choosing "Use current profile" really connects (address: a request from yourself; 1-time link:
two contacts both named after you).

**Fails if** own links stop being recognised or the current-profile button connects incognito. Mutations:
- `ConnectPlan.kt:182-193`: replace the `ContactAddressPlan.OwnLink` body by the Ok-without-data branch (title
  `connect_via_contact_link`) → "Connect to yourself?" never shows → fails;
- `ConnectPlan.kt:114-123`: same for `InvitationLinkPlan.OwnLink` (title `connect_via_invitation_link`) → fails;
- `ConnectPlan.kt:472` `incognito = false` → `true`: the joining side shares a random profile, so only one of the two
  new contacts is named "alice" → the final count fails.

**Setup.** No peer. Address via `alice.userPicker().createAddress()` (`Screens.kt:127-130`); 1-time link kept as a
pending connection via `keepOneTimeLink(alice)` (`ChatListFilterScreens.kt:29-39`).

**Steps and assertions.** Address first, because after the 1-time link part three chats are named "alice".
```kotlin
val address = alice.userPicker().createAddress()
alice.chatList.row("Private notes")
assert(chatModel.chats.value.none { it.chatInfo.chatViewName == "alice" })                // opposite state
enterLinkInSearch(address)                                                                 // ChatListFilterScreens.kt:100
poll(CONNECT_TIMEOUT_MS) { seesText("Connect to yourself?") && seesText("This is your own SimpleX address!", substring = true) &&
  exists(planOption("useCurrentProfile")) && exists(planOption("useIncognito")) && exists(planOption("cancel")) }
click(planOption("useCurrentProfile"))
poll { seesText("Connection request sent!") &&
  seesText("You will be connected when your connection request is accepted, please wait or check later!") }
click(planConfirm)
alice.chatList.request("alice")                         // Screens.kt:57-62: row "alice" with "Open to accept"

val link = keepOneTimeLink(alice)
val contactsNamedAlice = { chatModel.chats.value.filter { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.profile.displayName == "alice" && c.sndReady } == true } }
assert(contactsNamedAlice().isEmpty())
enterLinkInSearch(link)
poll(CONNECT_TIMEOUT_MS) { seesText("Connect to yourself?") && seesText("This is your own one-time link!", substring = true) }
click(planOption("useCurrentProfile"))
poll { seesText("Connection request sent!") &&
  seesText("You will be connected when your contact's device is online, please wait or check later!") }
click(planConfirm)
poll(CONNECT_TIMEOUT_MS, "two contacts named alice, connected to each other") {
  contactsNamedAlice().size == 2 && contactsNamedAlice().all { exists(Ui.ChatList.Row(it.id)) } &&
    chatModel.chats.value.none { it.chatInfo is ChatInfo.ContactConnection } }
```
Strings: "This is your own SimpleX address!", "This is your own one-time link!", "Connection request sent!", both
"You will be connected when …" texts (`strings.xml:925-928,2824-2829`). The two "You will be connected" texts also
check the link-type mapping in `connectViaUri` (`ConnectPlan.kt:418-431`). Expected core outcome: two contacts
`alice_1`/`alice_2` (`tests/ChatTests/Direct.hs:507-525`, `testPlanInvitationLinkOwn`) and a request `alice_1`
(`testPlanAddressOwn`, `Profiles.hs:2183`).

**Keys:** `planOption("useCurrentProfile")`, `…("useIncognito")`, `…("cancel")` (only `useCurrentProfile` clicked here;
presence of the other two shows the three-button alert). **Peer:** none.
**Platform:** shared, no differences (alerts and the list are visible on both; `request()` waits for the row).
**Flakiness:** the self-connection crosses the server → `CONNECT_TIMEOUT_MS`.

---

### Test 3. `planInvitationAlreadyConnectingFullLink` — `ConnectPlanTest`

Rows: §8 "Invitation: Full link: OK" (`planInvitationFullLinkUseCurrentProfile`, `planInvitationFullLinkCancel`) and
"Invitation: Already connecting".

**Scenario.** Someone with an old client sends a full 1-time link. The user pastes it: "Connect via one-time link?".
"Cancel" does nothing; "Use current profile" sends the request. Pasting the same link again while the contact is offline
says "Already connecting!". When the contact comes online, they are connected.

**Fails if** a second paste of a link in progress offers to connect again, or "Cancel" connects. Mutations:
- `ConnectPlan.kt:134-140`: show the "Connect via one-time link?" alert (call `askCurrentOrIncognitoProfileAlert` as in
  `:105-112`) instead of `connect_plan_already_connecting` → third paste fails;
- `ConnectPlan.kt:493-496` "Cancel" calls `connectViaUri(…, incognito = false, …)` → a pending connection appears after
  step 1 → fails;
- `ConnectPlan.kt:429` swap the INVITATION/CONTACT texts → "contact's device is online" text missing → fails.

**Setup.** `val bob = cli("bob", PeerFlags(fullLinks = true))` (`UiTest.kt:304-306`, `CliPeer.kt:391`).
`val full = bob.createFullOneTimeLink()` (NEW). CLI output parsed: `pass this invitation link to your contact (via another
channel): `, ``, short link, ``, `and ask them to connect: /c <invitation_link_above>`, `The invitation link for old
clients:`, full link (`View.hs:1071-1086`). Then `bob sends "/_stop"; bob.eventually("chat stopped")` (`Commands.hs:6310`,
`View.hs:123`): bob stays offline, so alice's connection stays in progress (the core forgets `conn_req_inv` once
connected, `tests/ChatTests/Direct.hs:502-503`, which would turn the plan back to "ok"; Haskell gets the same state by
closing the inviter, `testPlanInvitationLinkConnecting`, `Direct.hs:543-556`). `/_stop` is a CLI command, not PEERCTL.

**Steps and assertions.**
```kotlin
alice.chatList.row("Private notes")
val pending = { chatModel.chats.value.count { it.chatInfo is ChatInfo.ContactConnection } }
// 1. Cancel
enterLinkInSearch(full)
poll(what = "the full-link alert") { searchShowsLink("SimpleX one-time invitation") && seesText("Connect via one-time link?") &&
  seesText("Your profile will be sent to the contact that you received this link from.", substring = true) }
click(planOption("cancel"))
poll { !seesText("Connect via one-time link?") && editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search) }
quiesce(); assert(pending() == 0)                             // presence: the list and an editable search are shown
// 2. Use current profile
enterLinkInSearch(full)
poll { seesText("Connect via one-time link?") }
click(planOption("useCurrentProfile"))
poll { seesText("Connection request sent!") && seesText("You will be connected when your contact's device is online, please wait or check later!") }
click(planConfirm)
poll { pending() == 1 && editableText(Ui.ChatList.Search).isNullOrEmpty() }
// 3. Again while bob is offline
enterLinkInSearch(full)
poll { seesText("Already connecting!") && seesText("You are already connecting via this one-time link!", substring = true) &&
  !seesText("Connect via one-time link?") }
click(planConfirm)
poll { !seesText("Already connecting!") && pending() == 1 }    // no second connection
// 4. bob comes online
bob sends "/_start"; bob.eventually("chat started")
bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
poll(CONNECT_TIMEOUT_MS) { alice.chatList.shown("bob") && pending() == 0 }
```
Opposite-first: step 1 and 2 show the "ok" alert for the same link that step 3 must show as "Already connecting!".

**Expected peer output.** `chat stopped`; after `/_start`: `chat started`, possibly subscription lines, then
`alice: contact is connected` (`View.hs:1508`). Nothing alice did in steps 1 and 3 reaches bob: step 1 sends nothing
and step 3 only shows an alert; an extra connection from step 3 would be visible as `pending() == 2`.
**Keys:** `planOption("cancel")`, `planOption("useCurrentProfile")`, `planConfirm`.
**Platform:** shared; no differences. **Flakiness:** bob being stopped removes the race between alice's second paste
and the handshake; network waits use `CONNECT_TIMEOUT_MS`.
**Class:** the name's row has no `S`, so `ConnectPlanTest`. The merged full-link rows have `S`; a stable-CLI run cannot
pass `--show-full-links` (unknown option) and prints the full link as its only link, so a compat variant needs
`CliVersion` (plan §0.5.11, not built). See question Q2.

---

### Test 4. `planAddressRepeatConnectionRequest` — `ConnectPlanTest`

Rows: §8 "Address: Full link: OK" (`planAddressFullLinkConnectAlert`, here through "Use new incognito profile") and
"Address: Repeat request".

**Scenario.** The user pastes an old-format contact address and chooses an incognito profile; the owner sees a request
from a random name. Pasting the address again while the request is pending asks "Repeat connection request?"; the user
cancels; when the owner accepts, they are connected under the random name.

**Fails if** the incognito button sends the real profile, or a pending request is not recognised. Mutations:
- `ConnectPlan.kt:480` `incognito = true` → `false` → bob prints `alice wants to connect to you!` → `assertNotEquals`
  fails;
- `ConnectPlan.kt:194-205`: show `connect_via_contact_link` instead of `connect_plan_repeat_connection_request` → fails;
- `ConnectPlan.kt:428` swap CONTACT/INVITATION text → fails.

**Setup.** `val bob = cli("bob", PeerFlags(fullLinks = true))`; `val full = bob.createFullAddress()` (NEW; no
auto-accept). Parsed line: `The contact link for old clients: <link>` (`View.hs:1154`). A full address has no link data,
so the plan is `CAPOk Nothing` → `askCurrentOrIncognitoProfileAlert` (`ConnectPlan.kt:169-181`).

**Steps and assertions.**
```kotlin
alice.chatList.row("Private notes")
enterLinkInSearch(full)
poll(CONNECT_TIMEOUT_MS) { searchShowsLink("SimpleX contact address") && seesText("Connect via contact address?") &&
  seesText("Your profile will be sent to the contact that you received this link from.", substring = true) }
click(planOption("useIncognito"))
poll { seesText("Connection request sent!") && seesText("You will be connected when your connection request is accepted, please wait or check later!") }
click(planConfirm)
val rand = bob.eventuallyMatching(Regex("(\\S+) wants to connect to you!"), CONNECT_TIMEOUT_MS).groupValues[1]
assertNotEquals("alice", rand)
bob expects "to accept: /ac $rand"
bob expects "to reject: /rc $rand (the sender will NOT be notified)"
// repeat
enterLinkInSearch(full)
poll { seesText("Repeat connection request?") && seesText("You have already requested connection via this address!", substring = true) &&
  !seesText("Connect via contact address?") }
click(planOption("cancel"))
poll { !seesText("Repeat connection request?") && editableText(Ui.ChatList.Search).isNullOrEmpty() }
// accept
bob sends "/ac $rand"
assertTrue(bob.nextLine().startsWith("$rand: accepting contact request"))   // nothing (a second request) came before it
bob.eventually("$rand: contact is connected", CONNECT_TIMEOUT_MS)
poll(CONNECT_TIMEOUT_MS) { (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.let { it.contactConnIncognito && it.sndReady } == true }
```
**Expected peer output.** `<rand> wants to connect to you!`, `to accept: /ac <rand>`, `to reject: /rc <rand> (the sender
will NOT be notified)` (`View.hs:1267-1272`); after `/ac`: `<rand>: accepting contact request...` (or `…, you can send
messages to contact`, `View.hs:1259-1262`), then `<rand>: contact is connected`. `nextLine()` right after `/ac` proves
no second request was printed. The final model check is secondary; the peer line is the source of truth.
**Keys:** `planOption("useIncognito")`, `planOption("cancel")`, `planConfirm`.
**Platform/flakiness:** shared, none; `CONNECT_TIMEOUT_MS` for the request and the connection. Same `S`/stable remark
as test 3.

---

### Test 5. `planViaChatItemLinkCard` — `ConnectPlanTest`

Rows: §8 "Card: Link card in a message", "Signature: Signed address card" (`planSignedAddressCardVerified`, chat-list
card), "Card: Link card contents" (`linkCardInMessageShowsNameAndSignedInfoLine`), "Address: Known"
(`planAddressKnownContactOpenChat`), and "Invitation: Known" (`planInvitationKnownContactOpenChat`; same
`showOpenKnownContactAlert`, `ConnectPlan.kt:519-541`).

**Scenario.** Two contacts share their addresses in chat. cath (connected by 1-time link) is not yet known by her
address: tapping her card shows her profile, "Link signature verified." and "Open new chat", which opens a new chat to
request. bob (connected through that address) is known: tapping his card shows "Open chat" and returns to his chat,
creating nothing.

**Fails if** the card loses its signature or the known-contact alert offers a new chat. Mutations:
- `FramedItemView.kt:411` drop `linkOwnerSig = mc.ownerSig` → no "Link signature verified." in step d → fails;
- `ChatPreviewView.kt:357` drop `linkOwnerSig = mc.ownerSig` → step b fails;
- `ConnectPlan.kt:725` `information = null` → steps b, d fail;
- `ConnectPlan.kt:533` always `connect_plan_open_new_chat` → step c fails;
- `chat/item/CIChatLinkHeader.kt:68` `signed = false` → "Contact address (from owner)" missing → fails.

**Setup.**
```kotlin
val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
connect(cath)                                                     // Helpers.kt:26-32 (1-time link)
val bobAddress = bob.createAddress()                              // auto_accept on (ChatListFilterScreens.kt:80-88)
joinLink(bobAddress)                                              // ChatListFilterScreens.kt:62-67, API precondition
bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
val cathAddress = cath.createAddress()
cath sends "/share address @alice"; cath.eventuallyMessage("@alice contact address of @cath (signed):"); cath expects cathAddress
bob sends "/share address @alice";  bob.eventuallyMessage("@alice contact address of @bob (signed):");   bob expects bobAddress
val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
quiesce()
```
CLI lines: `ttyMsgContent` for `MCChat` prints `contact address of @<name> (signed):` then the link
(`View.hs:2443-2459`); the sent item gets `@alice ` and a time prefix (handled by `eventuallyMessage`). `joinLink` makes
bob a Known contact by address exactly as `searchPasteKnownContactLinkFiltersToContact` relies on. The signature is kept
because the binding matches alice's connection (`src/Simplex/Chat/Library/Subscriber.hs:1931-1938`,
`testShareAddressViaChat` `Profiles.hs:3912-3929`).

**Steps and assertions.**
```kotlin
// a. negative control: the same address pasted without the card's signature
enterLinkInSearch(cathAddress)
poll(CONNECT_TIMEOUT_MS) { exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText("cath") }
assert(!seesText("Link signature verified."))
click(planDismiss); poll { !exists(planConfirm) && editableText(Ui.ChatList.Search).isNullOrEmpty() }
// b. chat-list card thumbnail
backToChatList()
click(Ui.ChatList.Row(cathId).part("link"))
poll(CONNECT_TIMEOUT_MS) { seesText("cath") && seesText("Link signature verified.") && "Open new chat" in textsOf(planConfirm) }
click(planDismiss)
// c. known contact: bob's card in bob's chat
val bobChat = alice.chatList.open("bob")
val bobCard = bobChat.chatLinkItemId()                                       // NEW
poll { textsOf(Ui.Chat.Item(bobCard)).containsAll(listOf("bob", "Contact address (from owner)", "Tap to open")) }
click(Ui.Chat.Item(bobCard).part("chatLink"))
poll(CONNECT_TIMEOUT_MS) { "Open chat" in textsOf(planConfirm) && seesText("bob") && !seesText("Link signature verified.") }
click(planConfirm)
poll { chatModel.chatId.value == bobId && chatModel.chats.value.count { it.chatInfo.chatViewName == "bob" } == 1 }
bobChat.send("still here")                                                   // Screens.kt:274-277
bob.expectsMessage("alice> still here", CONNECT_TIMEOUT_MS)                  // nothing else reached bob
// d. cath's card in cath's chat: verified, "Open new chat"
backToChatList()
val cathChat = alice.chatList.open("cath")
val cathCard = cathChat.chatLinkItemId()
poll { textsOf(Ui.Chat.Item(cathCard)).let { t -> t.containsAll(listOf("cath", "Contact address (from owner)", "Tap to open")) && cathAddress !in t } }
click(Ui.Chat.Item(cathCard).part("chatLink"))
poll(CONNECT_TIMEOUT_MS) { seesText("Link signature verified.") && "Open new chat" in textsOf(planConfirm) }
click(planConfirm)
poll { chatModel.chatId.value != cathId && seesText("Tap Connect to send request") &&
  chatModel.chats.value.count { it.chatInfo.chatViewName == "cath" } == 2 }
```
Strings: "Open new chat" / "Open chat" (`strings.xml:18,15`), "Link signature verified." (`:586`), "Contact address"
+ " (from owner)" (`:581,583`, joined in `common/model/ChatModel.kt:5144-5154`), "Tap to open" (`:577`), "Tap Connect to
send request" (`:552`). Opposite pairs inside the test: unsigned (a) vs signed (b, d); Known "Open chat" (c) vs Ok "Open
new chat" (d).

**Expected peer output.** Plans read the link data from the server only; no peer is contacted. Step c's
`expectsMessage` as the next bob line proves "Open chat" sent no connection request (a request would print
`alice wants to connect…`/accepting lines first). cath prints nothing after her share; any error line fails the test
(harness §0.5.8).
**Keys:** `Ui.Chat.Item(id).part("chatLink")`, `Ui.ChatList.Row(id).part("link")` (new uses), `planConfirm`,
`planDismiss`.
**Platform.** Shared. `backToChatList()` before the row thumbnail and between chats (guide §3: on Android the chat
covers the list). **Flakiness:** two peers; `connect`/`joinLink` waits are network-bound (`CONNECT_TIMEOUT_MS`); the
cards are found by content type, not by position.

---

### Test 6. Extend `searchPasteLinkTriggersConnectPlan` (`main/ChatListToolbarSearchTest.kt:186-197`)

Row: §8 "Invitation: Deleted link → error" (`planInvitationDeletedLinkError`), re-targeted to a deleted address and
merged into the existing test.

**Why an extension.** The existing test already does the opposite state: paste bob's address in search → profile alert
with "Open new chat" → dismiss → search cleared. A new test would start with exactly those steps (guide §5: no
prefixes; extend the existing test). The catalogue's invitation setup needs async server deletion and a peer polling
loop (`repeatUntil`, plan §0.5.11, not built); an address deleted with `/da` is gone on the server at once
(`tests/ChatTests/Profiles.hs:4273-4277`, the next connect fails with AUTH).

**Fails if** the AUTH error of a plan stops being explained. Mutation: delete the `SMPErrorType.AUTH` branch at
`common/model/SimpleXAPI.kt:1757-1764` → no "Connection link removed" → fails.

**Added steps** (after the existing last line):
```kotlin
bob sends "/da"
bob expects "Your chat address is deleted - accepted contacts will remain connected."
bob expects "To create a new chat address use /ad"
enterLinkInSearch(address)
poll(CONNECT_TIMEOUT_MS) { seesText("Connection link removed") && seesText("Your contact removed this link", substring = true) }
assert(!exists(planConfirm))
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll { !seesText("Connection link removed") && editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search) &&
  chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" } }
```
CLI output: `View.hs:1123-1127` `viewUserContactLinkDeleted` (the two lines above). The plan goes through `sendCmdWithRetry`; AUTH is
not retryable (`SimpleXAPI.kt:926-945`), so `apiConnectResponseAlert` shows `connection_error_auth` /
`connection_error_auth_desc` (`strings.xml:215-216`). No keys. Shared, no platform difference.

---

### Test 7. `preparedContactInvitationConnectButton` — `main/PreparedChatTest`

Rows: §8 "Invitation: Short link: OK" (`planInvitationShortLinkOpenNewChat`); §9 "Prepared: Invitation"
(`preparedContactInvitationConnectButton`, `preparedContactBannerTexts`, `preparedContactRowOpenToConnect`).

**Scenario.** The user pastes a 1-time link, sees the sender's profile, taps "Open new chat", sees a chat that says "Tap
Connect to chat" with a single "Connect" button (the row reads "Open to connect"); taps it; they are connected and can
message.

**Fails if** the invitation compose area shows the wrong control or Connect doesn't connect. Mutations:
- `ComposeView.kt:1815-1820`: use `SendContactRequestView` for `ConnectionMode.Inv` → a message field appears →
  `!exists(Ui.Chat.Message)` fails;
- `chat/ChatView.kt:2262` banner text → fails; `chatlist/ChatPreviewView.kt:192-193` → row text fails;
- `ConnectPlan.kt:93` invert the `contactSLinkData_ != null` test → "Connect via one-time link?" instead of the
  profile alert → `openNewChatFromSearch` fails;
- `ComposeView.kt:657` `incognito = true` → bob prints a random name → fails.

**Setup.** `val bob = cli("bob"); val link = bob.createOneTimeLink()` (`Helpers.kt:56-61`, short link); then
`bob.eventually("and ask them to connect: /c <invitation_link_above>")` so the rest of the `/c` output is consumed and
the next bob line is the connection (`View.hs:1073-1078`).

**Steps and assertions.**
```kotlin
alice.chatList.row("Private notes")
assert(chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" })
val chat = openNewChatFromSearch(link, "bob")                                 // NEW
poll { seesText("Tap Connect to chat") && "Connect" in textsOf(Ui.Chat.Connect) && !exists(Ui.Chat.Message) }
backToChatList()
poll { "Open to connect" in alice.chatList.rowTexts("bob") }
alice.chatList.open("bob")
click(Ui.Chat.Connect)
assertEquals("alice: contact is connected", bob.nextLine(CONNECT_TIMEOUT_MS))  // first and only line
poll(CONNECT_TIMEOUT_MS) { exists(Ui.Chat.Message) && !exists(Ui.Chat.Connect) && !seesText("Tap Connect to chat") &&
  (alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.sndReady }
chat.send("hi")
bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
backToChatList(); poll { "Open to connect" !in alice.chatList.rowTexts("bob") }
```
Strings: "Open new chat", "Tap Connect to chat" (`:551`), "Connect" (`compose_view_connect`, `:636`), "Open to connect"
(`:511`). The "Your contact" banner (`ChatView.kt:2270`) is not asserted: after connecting, feature and e2e items are
added above the composer and on a small Android screen the banner can scroll out of composition, so its absence would
not mean much and its presence is not guaranteed; the composer change and the peer exchange are the outcome.
**Expected peer output.** `alice: contact is connected` (`View.hs:1508`), then `alice> hi`; `nextLine` proves nothing
came before (e.g. a message).
**Keys:** `Ui.Chat.Connect` on `ConnectButtonView` (new placement); `Ui.ChatList.Search`, `planConfirm`,
`Ui.Chat.Message`, `Ui.ChatList.Row` (existing).
**Platform:** `backToChatList()` + `open()` to read the row (Android covers the list); no other difference.
**Flakiness:** plan alert and connection wait `CONNECT_TIMEOUT_MS`.

---

### Test 8. `preparedContactAddressSendRequestWithMessage` — `PreparedChatTest`

Rows: §8 "Address: Short link: OK" (`planAddressShortLinkOpenNewChat`); §9 "Prepared: Address request"
(`preparedContactAddressSendRequestWithMessage`, `preparedContactSendRequestAlertAddMessage`,
`preparedContactAfterConnectShowsContactShouldAccept`). `preparedContactAddressSendRequestWithoutMessage` is already
done by `ChatListRowsTest.previewContactShouldAcceptForAddress` (`ChatListRowsTest.kt:171-188`: Connect → "Send request
without message" → `alice wants to connect to you!` → row "contact should accept…"); this test covers the other two
branches of the same alert.

**Scenario.** The user opens a prepared chat from a contact address ("Tap Connect to send request", a message field and
a "Connect" button). Tapping Connect with no text asks "Send contact request?"; "Add message" closes the alert without
sending. They type "hello": the Connect button hides, the send button asks again with "Send request" / "Cancel"; they
send. The owner sees the request with the message; the chat says "contact should accept…" until the owner accepts.

**Fails if** the message is not sent with the request, or "Add message" sends. Mutations:
- `ComposeView.kt:662` `msg = mc` → `msg = null` → bob never prints `alice> hello` → fails;
- `ComposeView.kt:686-691` dismiss runs `sendConnectPreparedContact()` → the request goes out without text; step B then
  has no prepared chat (no alert) → fails;
- `ComposeView.kt:1556` drop `if (composeState.value.whitespaceOnly)` → Connect stays with text → fails;
- `ComposeView.kt:1822-1834` invert `isBot` → no message field → fails; `ChatView.kt:2263`, `ChatView.kt:882-883` → fail.

**Setup.** `val bob = cli("bob"); val address = bob.createAddress(autoAccept = false)` (changed helper).

**Steps and assertions.**
```kotlin
alice.chatList.row("Private notes")
val chat = openNewChatFromSearch(address, "bob")
poll { seesText("Tap Connect to send request") && exists(Ui.Chat.Message) && "Connect" in textsOf(Ui.Chat.Connect) }
// A. empty: Add message
click(Ui.Chat.Connect)
poll { seesText("Send contact request?") && "Send request without message" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) &&
  "Add message" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared)) }
click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
poll { !seesText("Send contact request?") && exists(Ui.Chat.Connect) }
// B. with text: Send request
typeInto(Ui.Chat.Message, "hello")
poll { !exists(Ui.Chat.Connect) && isEnabled(Ui.Chat.Send) }                  // both branches of whitespaceOnly
pressEnter(Ui.Chat.Message)                                                    // desktop Enter / Android taps Send
hideKeyboard()
poll { seesText("Send contact request?") && "Send request" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) &&
  "Cancel" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared)) }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
bob.eventuallyAll(listOf("alice wants to connect to you!", "to accept: /ac alice", "to reject: /rc alice (the sender will NOT be notified)"),
  messages = listOf("alice> hello"), timeoutMs = CONNECT_TIMEOUT_MS)            // NEW
poll { editableText(Ui.Chat.Message).isNullOrEmpty() && chat.itemId("hello") > 0 && seesText("contact should accept…") }
// C. owner accepts
bob sends "/ac alice"
assertTrue(bob.nextLine().startsWith("alice: accepting contact request"))     // one request only
bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
poll(CONNECT_TIMEOUT_MS) { !seesText("contact should accept…") && (alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.sndReady }
chat.send("hi"); bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
```
Strings: "Tap Connect to send request", "Send contact request?", "Send request without message", "Add message", "Send
request", "Cancel", "contact should accept…" (`strings.xml:552,635-640,801,514`). "contact should accept…" is the line
above the composer (`ChatView.kt:309-317,873-890`), so it is on screen on both platforms. The alert text with `<b>`
(`:638`) is not matched.
**Expected peer output.** As `testShortLinkAddressPrepareContact` (`Profiles.hs:4176-4192`) with roles swapped: the
request line and `alice> hello` in either order, the two hint lines; after `/ac alice`:
`alice: accepting contact request, you can send messages to contact` (or `...`), `alice: contact is connected`.
**Keys:** existing only (`Ui.Chat.Connect` on `ComposeView.kt:1563`, `Ui.Chat.Message`, `Ui.Chat.Send`, alert keys).
**Platform:** `hideKeyboard()` after sending (Android IME may cover the stacked alert's buttons); `pressEnter` already
maps to the Send tap on Android. **Flakiness:** the request/message order is handled by `eventuallyAll`.

---

### Test 9. `preparedBotConnect` — `PreparedChatTest`

Rows: §8 "Bot: Bot address" (`planBotAddressShowsBotIcon`: its name claims the alert's cube icon, which has only the
generic description, so the name is not used; its checkable parts are merged here); §9 "Prepared: Bot".

**Scenario.** The user opens a bot's address: the chat says "Tap Connect to use bot" and offers only "Connect" (no
message field, no request alert); the row reads "Open to use bot". One tap connects; the chat then has a message field
and the "Bot" banner.

**Fails if** a bot address is treated as a person's address. Mutations: `ComposeView.kt:1822` `isBot` → `false` →
`SendContactRequestView` with a message field → `!exists(Ui.Chat.Message)` fails; `ChatView.kt:2263` → banner fails;
`chatlist/ChatPreviewView.kt:190` → row fails.

**Setup.** `val bob = cli("bob"); bob.becomeBot(); val address = bob.createAddress()` (auto_accept on, so the
connection completes without `/ac`; the acceptance comes from `/auto_accept`, not from being a bot).

**Steps and assertions.**
```kotlin
alice.chatList.row("Private notes")
openNewChatFromSearch(address, "bob")
poll { seesText("Tap Connect to use bot") && "Connect" in textsOf(Ui.Chat.Connect) && !exists(Ui.Chat.Message) &&
  !seesText("Tap Connect to send request") }
backToChatList(); poll { alice.chatList.rowTexts("bob").let { "Open to use bot" in it && "Open to connect" !in it } }
val chat = alice.chatList.open("bob")
click(Ui.Chat.Connect)
poll { !seesText("Send contact request?") }                                    // no request alert for a bot
bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
poll(CONNECT_TIMEOUT_MS) { exists(Ui.Chat.Message) && !exists(Ui.Chat.Connect) && seesText("Bot") && !seesText("Tap Connect to use bot") }
chat.send("hi"); bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
```
Opposite pair across tests 8/9: same prepared-address chat type, message field present (8) vs absent (9).
Strings: "Tap Connect to use bot" (`:553`), "Open to use bot" (`:512`), "Bot" (`chat_banner_bot`, `:556`), "Connect".
The "Bot" banner is shown right after connecting with few items; if Android scrolls it away (see test 7 note), drop that
one assertion rather than wait for it.
**Expected peer output.** `alice: accepting contact request...`, `alice: you can send messages to contact`,
`alice: contact is connected` (shape from `testShortLinkAddressChangeAutoReply`, `Profiles.hs:5217-5222`; skipped by
`eventually`), then `alice> hi` as the next line.
**Keys:** `Ui.Chat.Connect` on `ConnectButtonView` (new placement). **Platform:** list via `backToChatList()`.
**Flakiness:** none beyond network waits.

---

### Test 10. `preparedContactIncognitoBeforeConnect` — `PreparedChatTest`

Row: §9 "Profile and incognito before connecting" (incognito half).

**Scenario.** In a prepared chat the user opens "Your profile", picks "Incognito" and taps Connect: the contact sees a
random name; afterwards the picker is gone.

**Fails if** the picker's choice is not used when connecting. Mutations: `ComposeView.kt:657` →
`val incognito = false` → bob prints `alice: contact is connected` → fails;
`ComposeContextProfilePickerView.kt:175` drop `appPrefs.incognito.set(true)` → fails; `ComposeView.kt:1701` drop
`chat.chatInfo.nextConnectPrepared &&` → picker still shown after connecting → fails.

**Setup.** `val bob = cli("bob"); val link = bob.createOneTimeLink(); bob.eventually("and ask them to connect: /c <invitation_link_above>")`.

**Steps and assertions.**
```kotlin
val aliceId = chatModel.currentUser.value!!.userId
openNewChatFromSearch(link, "bob")
poll { seesText("Your profile") && exists(Ui.Chat.PickerUser(aliceId)) && !exists(Ui.Chat.PickerIncognito) }   // current: alice
click(Ui.Chat.PickerUser(aliceId))                                            // expands the list
poll { exists(Ui.Chat.PickerIncognito) }
click(Ui.Chat.PickerIncognito)
poll { exists(Ui.Chat.PickerIncognito) && !exists(Ui.Chat.PickerUser(aliceId)) }   // collapsed, Incognito selected
click(Ui.Chat.Connect)
val shared = bob.eventuallyMatching(Regex("(\\S+): contact is connected"), CONNECT_TIMEOUT_MS).groupValues[1]
assertNotEquals("alice", shared)
poll(CONNECT_TIMEOUT_MS) { !seesText("Your profile") && !exists(Ui.Chat.PickerIncognito) && exists(Ui.Chat.Message) }
```
Picker states from `ComposeContextProfilePickerView.kt:121-302`: collapsed shows only the selected option
(`CurrentSelection`, `:272-296`); a click on the selected user expands (`:129-131`); Incognito collapses and sets the
preference (`:170-177`). Strings: "Your profile" (`:1005`), "Incognito" (`:2238`).
**Expected peer output.** `<random>: contact is connected` (incognito joiner: `Profiles.hs:4692-4699`).
**Keys:** `Ui.Chat.PickerUser`, `Ui.Chat.PickerIncognito` (new), `Ui.Chat.Connect`.
**Platform:** none; the picker sits above the composer on both. The incognito preference is global
(`appPrefs.incognito`) and restored by the reset (`PrefStores.restore`, plan §0.5.3). **Flakiness:** none.

---

### Test 11. `preparedContactChangeProfileBeforeConnect` — `PreparedChatTest`

Row: §9 "Profile and incognito before connecting" (profile half).

**Scenario.** With two profiles, the user opens a link under "alice", switches the prepared chat to "alice2" in the
picker (the app switches the active profile and keeps the chat open) and connects: the contact sees "alice2".

**Fails if** changing the profile doesn't move the prepared contact. Mutations:
`ComposeContextProfilePickerView.kt:83` skip `apiChangePreparedContactUser` (treat as success) → bob prints `alice:` →
fails; `:99-104` drop `changeActiveUser_` → active user stays alice → fails.

**Setup.**
```kotlin
val alice = app("alice"); val bob = cli("bob")
alice.userPicker().profiles().apply { add("alice2"); close() }      // Screens.kt:137-164 (as UserPickerTest.kt:16)
alice.userPicker().switchTo("alice")                                // Screens.kt:142-148
val link = bob.createOneTimeLink(); bob.eventually("and ask them to connect: /c <invitation_link_above>")
```
alice2 gets alice's servers (new users copy the current user's servers, `Library/Commands.hs:488-500`), so it uses the
local `smp1`.

**Steps and assertions.**
```kotlin
val aliceId = chatModel.users.first { it.user.displayName == "alice" }.user.userId
val alice2Id = chatModel.users.first { it.user.displayName == "alice2" }.user.userId
openNewChatFromSearch(link, "bob")
val bobChatId = chatModel.chatId.value
poll { exists(Ui.Chat.PickerUser(aliceId)) && !exists(Ui.Chat.PickerUser(alice2Id)) }
click(Ui.Chat.PickerUser(aliceId))
poll { exists(Ui.Chat.PickerUser(alice2Id)) }
click(Ui.Chat.PickerUser(alice2Id))
poll { chatModel.currentUser.value?.userId == alice2Id && chatModel.chatId.value == bobChatId &&
  exists(Ui.Chat.PickerUser(alice2Id)) && !exists(Ui.Chat.PickerUser(aliceId)) }
click(Ui.Chat.Connect)
assertEquals("alice2: contact is connected", bob.nextLine(CONNECT_TIMEOUT_MS))
```
**Expected peer output.** `alice2: contact is connected` (as `robert: contact is connected`,
`testShortLinkChangePreparedContactUser`, `Profiles.hs:4737-4770`), as bob's next line.
**Keys:** `Ui.Chat.PickerUser` (new), `Ui.Chat.Connect`. **Platform:** none. **Flakiness:** the user switch is awaited
on the model and the picker state before Connect.

## 4. Rows not kept

| Row (name) | Decision | Reason |
|---|---|---|
| `pasteLinkClipboardEmptyNoop` | merged → test 1 | same screen and action |
| `planProgressIndicatorAndCancel` | drop | needs `SRVCTL` (stop the SMP server), not built (plan §0.5.11, 8.2/11) |
| `planInvitationShortLinkOpenNewChat` | merged → test 7 | its steps are test 7's first half |
| `planInvitationFullLinkUseCurrentProfile`, `planInvitationFullLinkCancel` | merged → test 3 | same alert, both buttons clicked there |
| `planInvitationFullLinkUseIncognito` | merged (button) → test 4 | the incognito button of the same alert function is clicked in test 4 with a peer-visible result; a second peer test for the same button adds cost only |
| `planOwnAddressConnectToYourself` | merged → test 2 | same alert function |
| `planInvitationKnownContactOpenChat` | merged → test 5 | same `showOpenKnownContactAlert` (`ConnectPlan.kt:519-541`); Known reaches an alert only via paste/URI/card (search filters it, `ChatListView.kt:929`), and the paste route is camera-gated on Android; invitation Known in the core is covered by Haskell (`Profiles.hs:4078-4080`) |
| `planInvitationConnectingPreparedOpensKnown` | drop | the alert exists only on the paste route (camera-gated) before Connect; the search route filters, and clearing a link from the read-only search field needs Back while a chat is open (unverified on desktop). Only `ConnectPlan.kt:533`'s `nextConnectPrepared` branch would be added; low value |
| `planInvitationDeletedLinkError` | merged → test 6 (extension) | re-targeted to a deleted address (deterministic, `/da`); the invitation variant needs async deletion + `repeatUntil` (not built). Catalogue row to be corrected |
| `planAddressShortLinkOpenNewChat` | merged → test 8 | first half of test 8 |
| `planAddressFullLinkConnectAlert` | merged → test 4 | |
| `planAddressConnectingProhibitOpensKnown` | drop | the plan comes only from a full-address lookup after Connect on a prepared contact (`Library/Commands.hs:4761`); search filters it and the alert is the same `showOpenKnownContactAlert` as test 5; core behaviour is Haskell-covered |
| `planAddressKnownContactOpenChat` | merged → test 5 | |
| `planAddressContactViaAddressOpensChat` | drop | needs `FIXTURE` + `RESTART` (contact card without connection), not built |
| `planAddressDeletedContactReconnect` | drop | the UI branch is the ordinary Ok/Known alert already tested; the deletion logic is core (`testPlanAddressContactDeletedReconnected`, `testShortLinkAddressDeleteContact`); low UI regression value for a 1-peer test |
| `planBotAddressShowsBotIcon` | merged → test 9 | the icon is not observable (generic description); the checkable parts are in test 9 |
| `planSignedAddressCardVerified`, `linkCardInMessageShowsNameAndSignedInfoLine` | merged → test 5 | |
| `planOwnerSignatureFailed` | drop for now (high-value follow-up) | needs the genuine `ownerSig`/`chatBinding` JSON read back from `/_get chat` (one very long CLI line) and re-sent; fragile without a JSON peer helper. Recommended once a `CliPeer.json(cmd)` reader exists |
| `planConnectByNameContact`, `planConnectByNameNotFound` | drop | need `NAMES` (names-mode SMP + resolver), not built |
| `planConnectByNameOtherKindButton` | drop | needs `NAMES` + `RELAY` |
| `planOpenedViaSimplexUriHandler` | drop | clicking a link inside message text needs a text-range click (`clickAtTextRange`, plan §0.5.11 7.4); the card and search routes run the same `planAndConnect` |
| `planViaChatItemLinkCard` | kept as test 5 | |
| `preparedContactBannerTexts`, `preparedContactRowOpenToConnect` | merged → test 7 | |
| `preparedContactAddressSendRequestWithoutMessage` | already covered | `ChatListRowsTest.previewContactShouldAcceptForAddress` |
| `preparedContactSendRequestAlertAddMessage`, `preparedContactAfterConnectShowsContactShouldAccept` | merged → test 8 | |
| `preparedContactConnectRetryAfterServerError` | drop | needs `SRVCTL` |

Group-link plans are owned by catalogue 03 §8 (legend ownership table) and are not in area A.

## 5. Catalogue corrections (file:line drift, checked 2026-10-20)

| Catalogue says | Actual |
|---|---|
| §8 intro `NewChatView.kt:628,671` | `ConnectView` `NewChatView.kt:635`, `PasteLinkView` `:678-720` (click handler `:681-698`) |
| Paste row `NewChatView.kt:671-699`, empty check `:675` | `:681-698`, empty check `:682` |
| Progress row `SimpleXAPI.kt:845-874` | `sendCmdWithRetry` `SimpleXAPI.kt:877-913` |
| Invitation OK `ChatView.kt:2257-2259` | banner `ChatView.kt:2262` |
| Own link `ConnectPlan.kt:470-476` | `:469-476` (button), `connectViaUri` alert `:424-433` |
| Known `ChatView.kt:2267` | `ChatView.kt:2270` |
| Deleted link `SimpleXAPI.kt:1722-1728` | AUTH alert `SimpleXAPI.kt:1757-1764` |
| Address OK `ChatView.kt:2260` | `ChatView.kt:2263` |
| Own address `ConnectPlan.kt:463-484` | alert `:463-503` |
| Address Known `ConnectPlan.kt:216-246` | Known `:216-231`, ContactViaAddress `:232-246` |
| Bot `ComposeView.kt:1807`, `:1806-1812`; `ChatPreviewView.kt:189` | `ComposeView.kt:1822-1827` (bot), `:1821-1834`; `ChatPreviewView.kt:190-191` |
| Failed signature `FramedItemView.kt:407` | card `FramedItemView.kt:406-417` (click `:409-411`) |
| Names `ChatListView.kt:1207`; `SimpleXAPI.kt:1707-1710` | `ChatListView.kt:1248` (key) / `:1253` (`planAndConnect`); NOT_FOUND alert `SimpleXAPI.kt:1743-1746` |
| URI `TextItemView.kt:373`; `Utils.kt:599-600`; `ChatListView.kt:775-784` | `TextItemView.kt:474`; `Utils.kt:607-609`; `connectIfOpenedViaUri` `ChatListView.kt:782-796` |
| Card `FramedItemView.kt:400-410`; `ChatModel.kt:5111-5121` | `FramedItemView.kt:406-417`; `infoLine` `ChatModel.kt:5144-5154` |
| §9 `ComposeView.kt:652,1799-1805`; `ChatView.kt:2251-2268`; `ChatPreviewView.kt:190-191` | `sendConnectPreparedContact` `ComposeView.kt:654-672`, Inv branch `:1815-1820`; banner `ChatView.kt:2254-2271`; row `ChatPreviewView.kt:192-193` |
| §9 address `ComposeView.kt:672-691,1806-1819` | alert `:674-693`; `SendContactRequestView` `:1538-1568`; branch `:1821-1834` |
| §9 bot `ComposeView.kt:1807-1812` | `:1822-1827` |
| §9 picker `ComposeContextProfilePickerView.kt:80-94`; `ComposeView.kt:1686` | `changeProfile` `:80-112`; picker shown at `ComposeView.kt:1701-1707` |
| §9 retry `ComposeView.kt:667-669`; picker alert `:128-140` | `:669-671`; "Can't change profile" alert `:114-119`, used at `:139-140,178-179` |
| §9 bot row cites `testShortLinkAddressChangeAutoReply` as the bot reference | that test has no bot; it only shows auto-accept output |
| Legend strings path | `commonMain/moko-resources/base/strings.xml` (correct in the legend; the brief's `MR/base` is not a path) |

Row edits needed after implementation: rename/re-describe `planInvitationDeletedLinkError` (address, `/da`, merged into
`searchPasteLinkTriggersConnectPlan`), mark merged names, and drop `planBotAddressShowsBotIcon`'s icon claim.

## 6. Suspected app issues (report only)

1. **Double `hideAlert()` on "Open new chat".** `showOpenChatAlert`'s confirm button runs `onConfirm()` then
   `hideAlert()` (`views/helpers/AlertManager.kt:402-405`), and `showPrepareContactAlert`'s `onConfirm` first calls
   `AlertManager.privacySensitive.hideAlert()` itself (`ConnectPlan.kt:727-729`; same in `showPrepareGroupAlert`
   `:777-778`). Two alerts are popped; if another privacy-sensitive alert is under it, it disappears unseen. Low impact;
   not reproduced.
2. **Profile change on API failure still switches the active user.** `ComposeContextProfilePickerView.kt:80-111`: when
   `apiChangePreparedContactUser` returns `null`, `changeActiveUser_(…, keepingChatId = chat.id)` still runs, so the app
   switches to the new user while the prepared contact stays with the old one, and the kept chat belongs to the other
   user. Error path only; not reproduced.
3. **Empty clipboard text vs no clipboard text.** `NewChatView.kt:682` returns silently for no text but an empty string
   reaches `strConnectTarget("")` and shows "Invalid link!". Cosmetic; noted because test 1's Android helper must clear
   the clip, not set it to `""`.
4. Known: the Android paste screen starts the camera and crashes without one (plan §15.5 item 4) — the reason only test
   1 uses that route.

## 7. Questions (with suggested answers)

- **Q1. Merge test 6 into the existing `searchPasteLinkTriggersConnectPlan`?** Suggested: yes (no new reset or peer;
  the existing body is the required opposite state). Alternative: a new `planDeletedAddressLinkError` that repeats the
  first five lines (violates the no-prefix rule).
- **Q2. Full-link rows marked `S`:** keep tests 3/4 current-CLI only (`ConnectPlanTest`) and leave the stable-peer
  variant for when `CliVersion` exists? Suggested: yes; the stable CLI rejects `--show-full-links`.
- **Q3. Test 1 on Android:** keep it shared with `assumeCamera()` (skipped on Waydroid, like
  `connectBannerCardOpensPasteLink`), rather than desktop-only? Suggested: shared; it runs on real devices.
- **Q4. `ConnectScreens.kt`:** a new harness file for the connect helpers (and moving `planConfirm` there) vs adding
  them to `ChatListFilterScreens.kt`? Suggested: new file; `ChatListFilterScreens.kt` already mixes filters, links and
  peers.
- **Q5. `planOwnerSignatureFailed`:** worth adding a `CliPeer` JSON-reading helper now (security-relevant warning), or
  defer? Suggested: defer to a follow-up with that helper.
