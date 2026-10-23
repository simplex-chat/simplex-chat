# UI test harness PR: adversarial review, pass 2 (reviewer B)

Scope: /home/user/wt-uitest (git diff + untracked uiTestShared, desktopTest/ui, androidTest/ui, commonMain/ui, scripts, tests/UITestServers.hs).
Paths below are relative to apps/multiplatform/common/src/ unless they start with android/, scripts/ or apps/.
I traced each finding in the code and marked it CONFIRMED. PLAUSIBLE means I did not trace it all the way.

## A. Tests that pass while the feature they name is broken

A1. ChatListRowsTest.incognitoIconOnRow (uiTestShared/.../main/ChatListRowsTest.kt:178-185). CONFIRMED.
The test name says "icon on row", but it only asserts the model (`contactConnIncognito == true`, notes not incognito). The row icon is `IncognitoIcon` (commonMain/.../chatlist/ChatPreviewView.kt:557-561). It has `contentDescription = null` and no key, so the test cannot see it.
Scenario: delete the `IncognitoIcon(...)` calls at ChatPreviewView.kt:395/398. The test still passes.

A2. ChatListRowsTest.verifiedIconOnRow (ChatListRowsTest.kt:187-195). CONFIRMED.
The test only asserts `contact.verified` in the model and then closes the info screen. `VerifiedIcon()` (ChatPreviewView.kt:97-98) has `contentDescription = null` and no key.
Scenario: remove `VerifiedIcon()` at ChatPreviewView.kt:144. The test still passes.

A3. ChatListRowsTest.previewLink (ChatListRowsTest.kt:64-66). CONFIRMED.
The poll is titled "the link text and the link image on the row", but the image half is a model check (`msgContent is MsgContent.MCLink`).
Scenario: drop the link preview thumbnail from the row (the MCLink branch of the small content preview in ChatPreviewView). The test still passes.

A4. ChatListRowsTest.unreadCounterGrayWhenMuted (ChatListRowsTest.kt:107-119). CONFIRMED.
The name claims a gray badge. The test checks that the count is "2", that there is no mute icon, and that the model has `MsgFilter.None`. It never checks the colour.
Scenario: change `backgroundColor = if (disabled || showNtfsIcon) secondary else primaryVariant` (ChatPreviewView.kt:496) to always use primaryVariant. The test still passes.

A5. GroupsTest.createGroup_opensChatAndInviteSheet (uiTestShared/.../groups/GroupsTest.kt:13-18). CONFIRMED.
The name says the chat opens. The test checks that the row exists in the list (`row("team")`) and that the text "Invite members" is visible. It never checks `chatModel.chatId` or the chat view. The Android BackKeyTest does assert `chatModel.chatId.value == teamId`.
Scenario: remove `openGroupChat(rhId, groupInfo.groupId)` (commonMain/.../newchat/AddGroupView.kt:51). The group is still created, the row still appears and the AddGroupMembersView modal still opens, so the test passes.

A6. ChatSearchTest.search_direct_filtersResults (uiTestShared/.../chat/ChatSearchTest.kt:19-21). CONFIRMED.
The only UI assertion is a negative one (`!exists(Item(other))`). The two matches are checked only in `chatsContext.chatItems` (model).
Scenario: the search results list renders empty, or never recomposes the matches. The test still passes.

A7. Removal is checked in the model only, through `ChatListScreen.gone()` (uiTestShared/.../harness/Screens.kt:72-74). CONFIRMED.
`gone()` polls `chatModel.chats` and never checks the `Ui.ChatList.Row` node. Tests affected:
- ContactsTest.contactRequestMenuReject (main/ContactsTest.kt:26)
- ContactsTest.deleteContactAndNotify (main/ContactsTest.kt:37)
- ChatListRequestsCardsTest.contactWithRequestComposeReject (main/ChatListRequestsCardsTest.kt:50)
- ChatListMenusPendingTest.pendingConnectionMenuDeleteInitiated (main/ChatListMenusPendingTest.kt:139)

Scenario: the chat list keeps showing a stale row after the model entry is removed (for example a LazyColumn key or derived-state regression). All four tests pass. `!shown(name)` cannot be used after removal because `chatId(name)` polls the model, so these tests need a row-id captured before the removal.

A8. Mark-unread is checked in the model only. CONFIRMED.
- ChatListMenuTest.contactMenuMarkReadAndUnread (main/ChatListMenuTest.kt:16-19): both the "marked unread" step and the "unread mark cleared" step assert `chatStats.unreadChat` only.
- ChatListMenusPendingTest.notesMenuMarkUnreadAndClear (main/ChatListMenusPendingTest.kt:82-83): same.

Scenario: the empty unread badge (the `n > 0 || unreadChat` branch, ChatPreviewView.kt:464) stops rendering. Both tests pass. Only ChatListMenusPendingTest.contactMenuMarkUnread checks the badge.

A9. ChatProfilesTest.profilesAddProfile (main/ChatProfilesTest.kt:8-12). CONFIRMED.
The poll "alice and work listed" reads `chatModel.users` only. Neither the profiles list nor the user picker UI is checked.

A10. ChatListFiltersListsTest.createListDuplicateNameError (main/ChatListFiltersListsTest.kt:205-218). CONFIRMED.
The test never checks the duplicate-name error. The error row (`duplicated_list_error` plus the error icon, TagListView.kt:273-288) is always composed and only switches between `Color.Transparent` and visible, so `seesText` could not tell the difference anyway.
Scenario: break `showErrorMessage` (TagListView.kt:273), so the message never becomes visible. The test passes.

A11. The Enter retry hides a dropped first Enter: ChatScreen.enterWhenSendable (harness/Screens.kt:270-276). CONFIRMED.
If the field does not clear within 5 s after the first Enter, the helper sends a second Enter and passes.
Scenario: a regression where the first Enter after typing is ignored (focus or key routing). SendCompatTest.sendText_enter_peerReceives, MessagingTest.* and every other test that calls `send` still pass.

A12. "enter" on Android is a tap on Send. CONFIRMED.
Android `UiScope.pressEnter` (android/src/androidTest/.../harness/UiTest.kt:253-255) calls `click(Ui.Chat.Send)` for the message field. SendCompatTest.sendText_enter_peerReceives (chat/SendCompatTest.kt:8) therefore never presses Enter on Android, and the test name is wrong there.

A13. Desktop "Back" tests skip the real Back input. CONFIRMED, though limited by the headless harness.
Desktop `escape()` (desktopTest/.../harness/UiTest.kt:199-203) calls `simplexWindowState.backstack.lastOrNull()?.invoke()` directly. It copies the body of the Window `onKeyEvent` (desktopMain/.../DesktopApp.kt:125-127) instead of sending Esc, and it never taps the visible back arrow.
Scenario: drop or misroute the Esc branch in DesktopApp.kt. OnboardingTest.onboardingCreateProfileBackReturnsToWelcome (desktop/OnboardingTest.kt:76-85) and onboardingLinkAMobileBackReturnsToWelcome (:272-283) still pass. So do the shared tests that use `escape()` on desktop.

A14. Mention badge vs mute icon cannot be told apart. PLAUSIBLE.
mentionBadgeSingleUnread and mentionIconWithMultipleUnread (ChatListRowsTest.kt:121-146) identify the "@" badge by the content description "Notifications". The mute icon uses the same description (ChatPreviewView.kt:479/489/505), and the round single-mention badge and the inline "@" icon have identical semantics. The single-unread test therefore only tells them apart by the missing "1".
Scenario: the n==1 path renders the small inline icon without the round background. The test passes.

A15. previewShowsDraft does not check the edit icon (ChatListRowsTest.kt:52-55). CONFIRMED, minor.
The poll title says "the draft with the edit icon". The edit icon is inline content with a null description, and the assertion only checks that the text ends with " unsent draft".

A16. Negative `!isEnabled` checks pass when the node is missing. PLAUSIBLE, low.
`isEnabled()` returns `false` when the node is missing (desktop harness/UiTest.kt:288-290; Android UiTest.kt:258-259). So `check(!isEnabled(X))` passes when X is not rendered at all.
- desktop/OnboardingTest.kt:67: no `waitFor(CreateProfile)` before the check.
- desktop/OnboardingTest.kt:120: no `waitFor(SetPassphrase)` before the check.

Line 55 does wait first, so it is safe.

A17. `isEnabled` is meaningless for keys on SectionItemView. CONFIRMED (harness weakness).
`SectionItemView(disabled = true)` (helpers/Section.kt:212-218) just leaves out `clickable` and does not set Disabled semantics. As a result:
- `isEnabled()` is always true for these keys: TagEditor.Save, UserPicker.*, Settings.*, Share.Row, NewChat.*, and alert rows built on SectionItemView.
- The `"node … to be enabled"` wait in `click()` (desktop UiTest.kt:209, Android UiTest.kt:177) is vacuous for them.
- A click on such a disabled row silently does nothing.
- `ListEditorScreen.saveIgnored()` (harness/ChatListListsScreens.kt:78-83) only works because of this.

## B. Assertions vs app code and strings.xml

I checked every literal string asserted by a shared or desktop test against moko-resources/base/strings.xml and the code that renders it. All of them exist and match what the code renders. That covers:
- Chat list: "Chats", "Start new chat", "Favorite", "Notifications", "All", the preset labels, "Add list", "Save list", "Add to list", "Change list", "List name...", "No chats in list %s.", "No unread chats", "No chats found", "Connect to %s".
- Contacts and invitations: "contact should accept…", "You are invited to group", "Join as %s", "marked deleted", "saved from", "Open to accept", "Accept incognito", "Reject contact request", "The sender will NOT be notified.", "Delete pending connection?".
- Onboarding: all strings used by the onboarding tests (the ones that use substring=true have `\n` in the resource).
- Keys and settings: "View security code", "%s is verified", "Dark", "New in %s"; "CurrentTheme", "FontScale", "DensityScale", "ShowSubscriptionPercentage", "OneHandUI", "OneHandUICardShown", "WhatsNewVersion", "StoreDBPassphrase", "EncryptedDBPassphrase".
- Formats and slider values:
  - percentage "%" / "100%": ServersSummaryView.kt:125.
  - "work ●": ChatListView.kt:1358.
  - "bob\nCoffee lover": ChatPreviewView.kt:256-257.
  - font scale 1.2 on Android: the 1.2083 slider tick is snapped to 1.20 in onValueChange.
  - zoom 1.5 on desktop: it is an exact tick.

I found no assertion that contradicts the app code.

B1. contactWithRequestComposeAccept has no short-link-data precondition (main/ChatListRequestsCardsTest.kt:28-39). PLAUSIBLE (flake, not a false pass).
The Accept bar accepts directly only when `chatModel.addressShortLinkDataSet()` is true. Otherwise it opens an unkeyed "Accept contact request" alert (ComposeContextContactRequestActionsView.kt:93-97). ChatListRequestsCardsCompatTest waits for `shortLinkDataSet == true` (CompatTest.kt:13), but this test does not, so if the flag is late `bob.eventually("…connected")` times out.

## C. Shared tests vs platform-specific UiScope API

Public UiScope members:
- Desktop only: `ui`, `clipboard`, `uriHandler`, `scrollIntoView`.
- Android only: `rule`, `finish`.

`grep -nE '(^|[^.A-Za-z_])(ui|rule|clipboard|uriHandler)\.|scrollIntoView|\bfinish\('` over common/src/uiTestShared finds nothing.

Desktop CliPeer has two extra methods, `awaitStart` and `drainLines`. Shared code does not use them.

Shared code uses only these PeerLines methods: `sends`, `types`, `expects`, `expectsMessage`, `eventually*`, `nextLine`, `contactId`.

So no shared test calls API that exists on only one platform. Only the semantics differ:
- `pressEnter` on Android taps Send (A12).
- `escape()` on desktop calls the backstack directly; on Android it is `Espresso.pressBack` (A13).
- `rightClick` on Android is a long-press.

These differences are intended, but they change what some test names mean.

## D. Code style

D1. Keys in Ui.kt that no test uses: none. I grepped every `Ui.<Obj>.<Name>` in Ui.kt across the three test source sets, and every one has at least one test use and one app use.

Per-variant keys:

D2. TagListView.android.kt:28 `.ui(Ui.TagEditor.Emoji)`. CONFIRMED.
Only the desktop-only ChatListFiltersListsDesktopTest acts on TagEditor.Emoji, and `ListEditorScreen.emojiText()` is called only from that desktop test. No Android-run test touches the Android emoji field, so this key sits on an element no test acts on.

D3. ChatItemView.kt `Ui.Chat.ItemMenu(cItem.id, "select")` is applied at 10 menu variants (lines 386, 403, 503, 520, 530, 544, 553, 570, 599, 673). PLAUSIBLE, low.
Only one variant (a received text item, in Android-only BackKeyTest.kt:56) is acted on. The other nine keys are on elements no test reaches.

D4. Hardcoded pixel offset in the desktop harness: `SCROLL_MARGIN = 50f` (desktopTest/.../harness/UiTest.kt:80). PLAUSIBLE: this depends on whether the rule covers the harness as well as tests.
It is used as a raw pixel delta in `scrollIntoView` (:191-192), and is not scaled by density. The tests themselves have no hardcoded coordinates or offsets. TapPoint uses a 48 dp minimum scaled by density, which is fine.

D5. Comments.
- I found no comments in the new Kotlin test, harness or Ui sources, and none on the `+` lines of the app diff.
- scripts/uitest-android.sh:87-88 describes what `results()` prints. LOW: it arguably documents an opaque awk rather than restating code.
- The usage header (:2-8) is CLI documentation.

D6. HarnessMeasureTest (desktopTest/.../harness/HarnessMeasureTest.kt:6). LOW.
The class is permanently `@Ignore`d, with the instruction "run manually … after removing this line". It is dead code in the suite.

## Severity summary

- **Highest value to fix:**
  - A1, A2: tests named "...IconOnRow" that can never see the icon.
  - A5: the "opensChat" half is not asserted.
  - A6: search matches are never shown to be displayed.
  - A7: removal from the chat list is checked in the model only (4 tests).
  - A11: the second-Enter retry hides a dropped Enter.
- **Medium:**
  - A3, A4, A8, A9, A10: the test name claims UI or colour the test does not check.
  - A12, A13: the desktop and Android harnesses skip the real Enter or Back input.
  - A17: SectionItemView keys have no Disabled semantics.
- **Low:** A14, A15, A16, B1, D2-D6.
