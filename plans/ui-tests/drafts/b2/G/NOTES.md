# Chat list requests and cards (catalogue 01, section 3): notes

9 tests in 2 classes. None of them has been compiled or run.

Paths:
- `C/` means `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.
- `CLV` means `C/views/chatlist/ChatListView.kt`, `NAV` means `C/views/chatlist/ChatListNavLinkView.kt`, `CCR` means `C/views/chat/ComposeContextContactRequestActionsView.kt`, `NCV` means `C/views/newchat/NewChatView.kt`.
- `S` means `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`.
- `H/` means `tests/ChatTests/`, and `src/` means `src/Simplex/Chat/`.
- App line numbers are from `/home/user/wt-uitest` (`eda94a505` plus its uncommitted harness changes), before `keys.diff` is applied. In these files they are 3 to 6 lines below the catalogue's numbers (`~/wt-uitest-ref`, `d72f68e6d`).
- Catalogue line numbers are from `plans/ui-tests/catalogue/01-onboarding-chatlist-connect-profiles.md`.

## Files

| File | Tests |
|---|---|
| `main/ChatListRequestsCardsCompatTest.kt` (shared, `open class`) | `contactRequestIncognitoHiddenWithShortLinkData` |
| `main/ChatListRequestsCardsTest.kt` (shared) | `contactWithRequestOpenToAccept`, `contactWithRequestComposeAccept`, `contactWithRequestComposeReject`, `oneHandToolbarCardSelectBottomBar`, `oneHandToolbarCardDismissShowsAlert`, `oneHandCardAutoHiddenAfterThreeChats`, `connectBannerCardOpensNewLink`, `connectBannerCardOpensPasteLink` |
| `screens.kt` | One new helper, `openRowMenu` (below) |
| `keys.diff` | App-side keys. `git apply --check` passes on `/home/user/wt-uitest` as it is now. |

Class names come from the class map: `contactRequestMenuAccept` starts `ChatListRequestsCards*`, and the row at :91 has `S`, so its test goes to the `open class ChatListRequestsCardsCompatTest`. The other rows (:93, :97, :98) have no `S` and go to `ChatListRequestsCardsTest`.

About `S`: the legend (`00-legend.md`, Needs codes) defines `S` as "also run with the stable-release CLI peer". A second SMP server is `SMP2`. So `S` does not need a second server, and nothing was skipped for it. The legend also asks for `@Category(StableCompat::class) class ChatListRequestsCardsCompatStableTest : ChatListRequestsCardsCompatTest()`. That subclass is not written: the harness has no `StableCompat` category and no stable peer yet. The existing compat classes (`AddressCompatTest`, `OneTimeLinkCompatTest`, `SendCompatTest`) do the same, with only the open class.

The existing `contactRequestMenuAccept` and `contactRequestMenuReject` (same catalogue row :91, also `S`) are in `main/ContactsTest.kt`, not in `ChatListRequestsCardsCompatTest` as the class map says. I did not move them, because that is outside this draft.

## Keys added (`keys.diff`)

All composables are in commonMain, so desktop and Android both get the keys. Each card has one base key, and its parts use `UiKey.part(...)`, as `Ui.Chat.Bar.part("title")` does. This adds only 4 vals to `Ui.kt`.

| Key | Attached at | Why |
|---|---|---|
| `Ui.ChatList.OneHandCard` | the card `Row`, `CLV:146` | Shows whether the card is present. `textsOf` reads both labels. |
| `OneHandCard.part("bottom")` / `.part("top")` | the `ToolbarSegment` modifiers, `CLV:160,169` (the segment applies `modifier` first, then `clickable`, at `CLV:197-200`) | The segments can only be found by their text, and the text also appears in Appearance settings. |
| `OneHandCard.part("close")` | the close `Icon`, `CLV:171-179` | The icon has no description (catalogue setup item 5). |
| `Ui.ChatList.ConnectBanner` | the banner `Column`, `CLV:364` | Shows whether the banner is present. No test here uses it, but `connectBannerCardDismiss` will. |
| `ConnectBanner.part("newLink")` / `.part("pasteLink")` | the two clickable `Column`s, `CLV:386,417` | Without `SIMPLEX_ASSETS` the halves are a gradient and a text, and the paste label differs by platform. |
| `Ui.Chat.RequestAccept` / `Ui.Chat.RequestReject` | the two button rows of the accept bar, `CCR:66,86` (both imports added) | Neither row has a description. "Accept"/"Reject" also appear in alerts and menus. |

No close key is added for the connect banner, because none of these tests uses it. `connectBannerCardDismiss` can find its `IconButton` by the description "Close button" (`CLV:371`, `icon_descr_close_button`, `S:893`), or add `ConnectBanner.part("close")`.

## Helper added (`screens.kt`)

- `UiScope.openRowMenu(app, name): String` waits for API calls to finish and right-clicks (long-presses on Android) the row, the same way `ChatListScreen.rowMenu` does. It returns the chat id but does not click an item, so a test can check which items are absent. `rowMenu` cannot do this, because it always clicks an item.

## Per test

### `contactRequestIncognitoHiddenWithShortLinkData` (catalogue :91)
- **Setup:** alice creates her address in the UI (`UserPickerScreen.createAddress`). The test then waits for `chatModel.userAddress.value?.shortLinkDataSet == true`. It checks the field directly: `ChatModel.addressShortLinkDataSet()` returns true when there is no address at all (`C/model/ChatModel.kt:276`). bob sends `/c <address>` and gets `connection request sent!` (`src/View.hs:1231`). alice sees the "Open to accept" row (`ChatPreviewView.kt:192-193`).
- **Asserts:** the row menu has `accept` and `reject` (`NAV:541-572`), and has neither the key `acceptIncognito` nor the text "Accept incognito" (`NAV:552`). The core refuses the same thing: `src/Library/Commands.hs:1453` "incognito not allowed for address with short link data", Haskell `testShortLinkAddressPrepareContact` (`H/Profiles.hs:4212-4213`).
- **Platforms:** shared. On Android, `rightClick` is a long press.
- **Stable peer:** `/c` with a short address link and the `connection request sent!` line are also what a stable CLI prints (that is assumed, not checked against a stable build).

### `contactWithRequestOpenToAccept` (:93)
- **Asserts:** opening the "Open to accept" row opens bob's chat (`chatModel.chatId`; a click on a direct contact calls `openDirectChat`, `NAV:197-200`). The compose area shows the bar with "Reject" and "Accept" (`S:832,830`), and there is no message field. The bar replaces `SendMsgView_` when `nextAcceptContactRequest && contactRequestId != null` (`C/views/chat/ComposeView.kt:1821-1829`).
- **Not covered:** "the request message (if any)". A request made with `/c <address>` has no message, because the CLI `Connect` command takes no message (`src/Library/Commands.hs:2407-2413`, parser :6390). See the correction for :93.

### `contactWithRequestComposeAccept` (:93)
- **Asserts:** clicking "Accept" in the bar accepts at once, with no alert, because the GUI address has short-link data (`CCR:91-95`). bob prints `alice: contact is connected` (Haskell `testShortLinkAddressPrepareContact`, `H/Profiles.hs:4214-4218`). Then the contact no longer has `nextAcceptContactRequest` (`C/model/ChatModel.kt:1897-1900`), the bar is gone, the message field is back, and the "Accept contact request" alert was never shown.
- The check uses `nextAcceptContactRequest`, not `contactRequestId`. The accepted contact keeps its `contactRequestId` (`NAV:756-759`).

### `contactWithRequestComposeReject` (:93)
- **Asserts:** "Reject" opens "Reject contact request" / "The sender will NOT be notified." (`CCR:122-134`, `S:834-835`), and its confirm button reads "Reject". Confirming removes bob's chat and sets `chatModel.chatId` to null (`NAV:777-789`, `dismissToChatList = true`). The chat list search field is there again.
- **Not asserted:** "the CLI prints nothing". The harness has no "no new line within N ms" check. `contactRequestMenuReject` does not assert it either.

### `oneHandToolbarCardSelectBottomBar` (:97)
- **Setup:** `appPrefs.oneHandUICardShown.set(false)` after `app()`. The pref is read through `.state` (`CLV:956`), and `SharedPreference.set` updates the state (`C/model/SimpleXAPI.kt:3928-3931`), so no `restart()` is needed. A fresh alice lists one chat (the notes folder). The "Ask SimpleX Team" card is filtered out (`CLV:1564`), so the card is not hidden by the 3-chat rule.
- **Asserts:** the card shows "Bottom bar" and "Top bar" (`S:1721-1722`). "Bottom bar" is selected by default, and the selected segment is not clickable (`CLV:200`), so the test clicks "Top bar" first. Then `OneHandUI` is false in the model and in storage, the floating new-chat button (`NewChatFab`, `CLV:266,489`) exists, and the toolbar button (`NewChat`, `CLV:572-593`) does not. Clicking "Bottom bar" reverses all of that. The card stays shown.
- **Deviation from the catalogue:** the row checks that the toolbar is above the first row or below the last row. That needs bounds, which the brief forbids. Which new-chat button is present is the key-based effect of the same pref. See the correction for :97.

### `oneHandToolbarCardDismissShowsAlert` (:97)
- **Asserts:** the close icon opens "Reachable app toolbars" / "You can change it in Appearance settings." (`CLV:136-142`, `S:1719,1723`). The card disappears and `OneHandUICardShown` is stored as `true` (`TestPrefs` presets it to true, but the test's own `set(false)` writes false first, so a stored `true` means the close set it). "OK" closes the alert.

### `oneHandCardAutoHiddenAfterThreeChats` (:97)
- **Setup:** bob connects. The test waits until exactly two chats are listed (bob and the notes folder, no contact cards or deleted chats), then turns the card back on, waits for it, and checks it is still shown after `settle()`.
- **Asserts:** after cath connects, the filtered list has 3 chats, and `LaunchedEffect(chats.size)` sets `OneHandUICardShown` (`CLV:1130-1136`). The card is gone, the stored value is `true`, and no "Reachable app toolbars" alert was shown (only the close icon shows it).
- `connect()` uses `apiAddContact` directly, so no pending-connection row is added before the contact. `ContactConnected` removes the connection id (`C/model/SimpleXAPI.kt:2927-2935`), so the count is exactly 2 and then 3.

### `connectBannerCardOpensNewLink` (:98)
- **Setup:** bob connects, so `hasConversations` is true (`C/views/newchat/OnboardingCards.kt:69-79`). Then `addressCreationCardShown` is set to false. It is read through `.state` (`CLV:1276`), so no restart is needed.
- **Asserts:** the banner half reads "New 1-time link" (`S:1197`). Clicking it opens `NewChatView(INVITE)` (`CLV:386-389`). The test then sees "New chat", the tab "1-time link" (`S:951`, `NCV:105-111`), "Share this 1-time invite link" (`NCV:507`, `S:976`), no "Paste the link you received", `chatModel.showingInvitation` set, and the link text in `Ui.Link.Text`. The invitation is created by `LaunchedEffect(selection)` (`NCV:63-72`). `HorizontalPager` composes only the current page, so these texts show which tab is selected.

### `connectBannerCardOpensPasteLink` (:98)
- **Asserts:** the banner half reads "Paste link" on desktop and "Paste link / Scan" on Android (`CLV:440,443`, `S:948-949`). Clicking it opens `NewChatView(CONNECT, showQRCodeScanner = isAndroid)` (`CLV:417-420`). The test then sees "New chat", "Connect via link", "Paste the link you received" and "Tap to paste link" (`NCV:656,702`), no 1-time link section, and no invitation. "Or scan QR code" is shown only on Android (`NCV:661-663`, `S:970`).
- **Platform branch:** `appPlatform.isAndroid` picks the label and decides whether "Or scan QR code" is expected. The app itself branches on the platform at these lines, and `05-android-carryover.md:95` lists this test as ADAPT for the same reason.
- **Android risk:** with `showQRCodeScanner = true` and the camera permission not granted, `QRCodeScanner` asks for the permission at once (`C/../androidMain/.../QRCodeScanner.android.kt:137-142`). That opens the system permission dialog over `MainActivity`. The Android harness does not grant `CAMERA` (no `GrantPermissionRule` or `pm grant` in `android/src/androidTest`). The Compose assertions should still pass, but the dialog may outlive the test. The Android harness should grant or deny `CAMERA` before the run.

## Uncertainties
1. `OneHandCard.part("top")`: the close icon sits over the right end of the "Top bar" segment as a sibling, not a descendant, so `tapPointInNode` does not avoid it. The segment centre should be clear of the 36 dp icon. This is not measured.
2. `contactWithRequestOpenToAccept` does not check the request message (see the correction for :93).
3. Stable-peer output for `/c` with a short address link is assumed to match the current build.
4. On Android, `connectBannerCardOpensPasteLink` depends on how the camera permission dialog is handled (above).
5. The other b2 drafts may add keys to the same `Ui.ChatList` block, so `keys.diff` may need a rebase when they are merged.

## Catalogue corrections

1. **:97 (one-hand card), Verification.** Problem: "the toolbar title "Chats" is above the first row … below the last row" can only be checked with bounds, which the tests may not use. The pref has a key-based effect: the new-chat button moves between the toolbar and a floating button. Replace "then "Top bar": OneHandUI=false and the toolbar title "Chats" is above the first row; then "Bottom bar": OneHandUI=true and the title is below the last row (`ChatListView.kt:158,167,241-260`)" with:
   > then "Top bar": Pref OneHandUI=false, the floating "Start new chat" button appears and the toolbar one is gone; then "Bottom bar": Pref OneHandUI=true, the toolbar button is back and the floating one is gone (`ChatListView.kt:158,167,263,569-590`; the list's `reverseLayout` follows the same pref, :994)

2. **:93 (accept bar), Verification.** Problem: "the request message (if any)" cannot come from a CLI `/c <address>`. `Connect` takes no message (`src/Simplex/Chat/Library/Commands.hs:6390`). A message needs the prepared-contact flow. Replace "Opening the row shows the chat with the request message (if any) and the accept bar." with:
   > Opening the row shows the chat with the accept bar in place of the message field (`ComposeView.kt:1821-1829`); a request made by CLI `/c <address>` carries no message (a request message needs `/_connect plan 1 <link>` → `/_prepare contact 1 <full> <short> <data>` → `/_connect contact @<id> text <msg>`, H/Profiles.hs:4194-4201)

3. **:98 (connect banner), Entry path.** Problem: "Shown when conversations exist" leaves out that a contact request also counts. Its contact is a non-card `Direct` chat, so it shows the banner. Group invitations count too. Replace "Shown when conversations exist and `addressCreationCardShown` is false" with:
   > Shown when `addressCreationCardShown` is false and there is a non-card, not-deleted direct contact (a contact still waiting to be accepted counts) or any group (`OnboardingCards.kt:69-79`); with chats but none of these, the onboarding cards show instead (`OnboardingCards.kt:52-55`)

4. **:98, Verification.** Problem: the line reference `ChatListView.kt:363,1270` points at a blank line (1270). The condition is at 1271. Replace `(ChatListView.kt:363,1270)` with `(ChatListView.kt:363,1271)`.

## Rows skipped
None. All 9 assigned tests are drafted. `connectBannerCardDismiss` (same row, :98) was not assigned and is not drafted.
