# Test review C: chat-list rows, row menus, pending connections, requests, cards

Scope: 39 tests. Repo /home/user/wt-uitest (branch nd/desktop-ui-tests). Read-only review: nothing was run.

Abbreviations (all under `apps/multiplatform/common/src/`):
- Tests: `R` = `uiTestShared/.../ui/main/ChatListRowsTest.kt`, `MP` = `ChatListMenusPendingTest.kt`, `M` = `ChatListMenuTest.kt`, `RC` = `ChatListRequestsCardsTest.kt`, `RCC` = `ChatListRequestsCardsCompatTest.kt`
- Harness: `SCR` = `uiTestShared/.../harness/Screens.kt`, `HLP` = `harness/Helpers.kt`, `RS` = `harness/ChatListRowsScreens.kt`, `FS` = `harness/ChatListFilterScreens.kt`, `DUT` = `desktopTest/.../harness/UiTest.kt`
- App: `CPV` = `commonMain/.../views/chatlist/ChatPreviewView.kt`, `NAV` = `views/chatlist/ChatListNavLinkView.kt`, `CLV` = `views/chatlist/ChatListView.kt`, `CCV` = `views/chatlist/ContactConnectionView.kt`, `CIMG` = `views/helpers/ChatInfoImage.kt`, `CRA` = `views/chat/ComposeContextContactRequestActionsView.kt`
- Catalogue: `CAT01` = `plans/ui-tests/catalogue/01-onboarding-chatlist-connect-profiles.md`

Harness facts used throughout (all CONFIRMED):
- `rowTexts`/`rowDescriptions` read the row's own merged node plus every unmerged descendant (DUT:273-287). Checks like `"1" in rowTexts` are exact matches on one Text element, so they really are UI checks.
- **Screenshots are taken only on failure** (DUT:308-334, `failureShot`). The catalogue says some checks (for example the grey badge in `unreadCounterGrayWhenMuted`, CAT01:79) "show in the screenshot", but no screenshot is taken when a test passes, so **nothing checks colour anywhere**.
- `restart()` exists (DUT:93-104), but no test in this scope uses it. No local state change (favourite, mute, unread, alias) is checked after a restart.
- Icons with `contentDescription = null` cannot be seen by semantics: the verified shield (CPV:98), the incognito mask (CPV:557-567), the link thumbnail (CPV:325) and the flag icons (CPV:571-579). The pending-connection icon has the generic description "profile image placeholder" (CCV:26 → CIMG:92/99), the same as every row without an avatar.
- `markChatRead` clears the model's unread count **before** it calls `apiChatRead` (NAV:668-679). Updates to favourite, mute and the unread mark are applied to the model only after the API succeeds (NAV:994-1011, 711-715).

## Per-test table

| # | test | feature / user scenario | real user case? | how it asserts | regression it CATCHES / MISSES | problems | verdict |
|---|---|---|---|---|---|---|---|
| 1 | R `previewShowsLastTextMessage` (R:27) | The row shows the newest message, received or sent, and replaces the older one | yes, the main purpose of the row | UI text of the row; CLI checks the reply is delivered | CATCHES: the preview stuck on a stale item, or own messages not shown. MISSES: maxLines / ellipsis on long text; markdown formatting in the preview | none | KEEP (fold #14 into it) |
| 2 | R `previewShowsDraft` (R:44) | Unsent draft shown with the edit icon in place of the last message | yes | model `draftChatId` + UI text ending in `" unsent draft"`. The leading space comes from the inline edit-icon placeholder (CPV:111-113), so a draft rendered without the icon fails | CATCHES: the draft not saved when switching chats; the draft branch dropped (CPV:229). MISSES: drafts with attachments (file name, voice duration, CPV:102-120); the draft clearing after send | relies on the inline placeholder rendering as one char + space (PLAUSIBLE: fragile if the Compose inline text changes) | KEEP |
| 3 | R `previewLink` (R:58) | A link message shows its text and a thumbnail that opens the link alert | yes | UI text `https://simplex.chat` + model `msgContent is MCLink` | CATCHES: link text missing from the preview. MISSES: the thumbnail branch removed or broken (CPV:318-330). The URL text would show for a plain text message too, and the MCLink check only shows that core decoded the JSON | the poll message says "the link image on the row" (R:64), but the image is checked only in the model; the image has a null description (CPV:325). CONFIRMED vacuous for the thumbnail | STRENGTHEN: click the thumbnail and assert the open-link alert (catalogue CAT01:75 already describes this). That is an element the test acts on, so a key on the `IconButton` at CPV:321 is allowed |
| 4 | R `previewDeletedMessage` (R:69) | A message the sender deleted reads "marked deleted" | yes | UI text; CLI confirms the delete | CATCHES: `markedDeletedText` branch lost (CPV:253), or the deleted text still shown. MISSES: moderated / blocked variants (`MarkedDeletedItemView.kt:102-108`) | none | KEEP |
| 5 | R `previewContactCardMessage` (R:81) | A shared address card previews as name + bio on two lines | yes | exact UI text `"bob\nCoffee lover"` | CATCHES: the MCChat branch (CPV:254-258) broken or the bio dropped. MISSES: the card image/icon (CPV:355-366); tapping the card to connect (CPV:357) | none | KEEP |
| 6 | R `previewGroupSenderPrefix` (R:92) | Group preview prefixes others' messages with "bob: " but not your own | yes | UI text with exact `"bob: hello all"`; no `": mine"` | CATCHES: sender missing, or the sender added to your own messages (CPV:280-283). MISSES: `showGroupAsSender` (channel) items | none | KEEP |
| 7 | R `unreadCounterGrayWhenMuted` (R:107) | A muted chat's badge is grey | yes | UI text "1"/"2", no "Notifications" description, model `enableNtfs == None` | CATCHES: the mute icon shown alongside the badge. MISSES: **the feature in its name**. The grey colour (CPV:497) is never checked: no success screenshot (DUT:331), no semantics | the name promises colour, but the test checks nothing about it (CONFIRMED). Everything else is the middle step of #10 (R:153-156) | MERGE into #10 (add the "2" step there); drop the colour claim from the catalogue unless a pixel check is added |
| 8 | R `mentionBadgeSingleUnread` (R:121) | One unread mention shows the round @ badge with no count | yes | model `unreadCount==1 && unreadMentions==1` + UI description "Notifications" + no "1" text | CATCHES: the @ badge replaced by the count badge (CPV:485-499). MISSES: mention colour depending on the group's mute mode (CPV:467-475) | "Notifications" is also the mute icon's description (CPV:505). That is safe only because the group is unmuted (CONFIRMED, not a bug) | MERGE with #9 (its first half is exactly this setup) |
| 9 | R `mentionIconWithMultipleUnread` (R:134) | A mention plus more unread messages shows the @ icon next to the count | yes | model counts + description + "2" | CATCHES: the icon dropped when n>1 (CPV:476-483). MISSES: colour; a mention in a muted group | its first step (R:140) checks less than #8. The second step depends on the same setup, so the two tests repeat the group setup (CONFIRMED) | MERGE: one test, single-mention state → send another → multi state |
| 10 | R `mutedIconWhenNoUnread` (R:148) | Precedence in the trailing slot: mute icon ↔ unread badge ↔ mute icon after Mark read | yes | UI description + UI text | CATCHES: wrong precedence (CPV:464/502), the mute icon not coming back. MISSES: persistence of the mute across restart; the Mentions-mode icon | none | KEEP (absorb #7 and #29: add "2" and finish with `ntfs.All`, checking the icon is gone) |
| 11 | R `favoriteStarWhenNoUnread` (R:163) | Same precedence for the favourite star | yes | UI description "Favorite" + text | CATCHES: star/badge precedence (CPV:512). MISSES: star hidden when muted (CPV:502 before 512, not tested); persistence | none | KEEP (absorb #28: end with `unfavorite`) |
| 12 | R `incognitoIconOnRow` (R:178) | An incognito contact's row shows the mask | yes as a feature, but the test does not exercise it | model only: `contactConnIncognito` from an incognito link **created through the API** (RS:20-24, `apiAddContact(incognito=true)`); `assertFalse(notes.incognito)` | CATCHES: core not marking the contact incognito. MISSES: **the mask itself** (null description, CPV:560), e.g. `IncognitoIcon` removed, or `chatStatusImage` (CPV:384-399) broken | no UI assertion at all (CONFIRMED). `NoteFolder.incognito` is hard-coded `false` (`ChatModel.kt:3163`), so the `assertFalse` can never fail (CONFIRMED vacuous). The model fact duplicates `OneTimeLinkTest.oneTimeLinkSwitchToIncognito` (OneTimeLinkTest.kt:21-22), which uses the real UI | DROP. It cannot be strengthened under the keys-only rule unless the icon gets a content description (an app a11y change, not a test key) |
| 13 | R `verifiedIconOnRow` (R:187) | A verified contact shows the shield before the name | yes as a feature, but not exercised | model only: `contact.verified` after Mark verified | CATCHES: only what ContactInfoTest already catches. MISSES: the shield (null description, CPV:98, 143-145) | a strict subset of `ContactInfoTest.contactInfoMarkVerified` (ContactInfoTest.kt:20-29), which checks the same model fact plus UI text (CONFIRMED). Nothing on the row is checked | DROP |
| 14 | R `timestampShown` (R:197) | Row shows the last item's time | yes | UI text compared with `getTimestampText(item.meta.itemTs)`, the same function the row uses (CPV:417) | CATCHES: the timestamp missing. MISSES: formatting regressions (the test reuses the app's formatter, so it agrees with itself); `chatTs` used instead of `itemTs` (same minute here) | tautological for formatting (CONFIRMED). One assertion; the setup duplicates #1 | MERGE into #1 |
| 15 | R `previewContactShouldAcceptForAddress` (R:207) | After you request a contact through their address, the row reads "contact should accept…" | yes | UI text; CLI sees the request | CATCHES: the CPV:194-199 branch picking "connecting…" instead. MISSES: the text clearing once bob accepts (the catalogue says "until bob accepts", CAT01:82) | enters the link through `planAndConnect` directly (R:213), skipping the paste/search UI. Acceptable, because paste is covered elsewhere | STRENGTHEN: `bob sends "/ac alice"`, then poll until the text leaves the row and bob's last message or "connected" state appears |
| 16 | R `previewGroupInvitationJoinAs` (R:222) | Group invite row: "You are invited to group" / "Join as <incognito name>" | yes | UI text | CATCHES: the incognito branch (CPV:583-587) lost, which would leak which profile is used. MISSES: the row menu Join / Join incognito (NAV:318-326); invite row colour | uses API incognito link creation (RS:20). Fine | KEEP |
| 17 | MP `contactMenuMarkRead` (MP:28) | Row menu "Mark read" clears the badge | yes | UI text + model `unreadCount` | CATCHES: menu item missing or not wired. MISSES: `apiChatRead` failing. The model is cleared before the API call (NAV:668-679), so the badge disappears anyway (CONFIRMED) | identical to the first half of #27 (M:10-16) (CONFIRMED) | KEEP (canonical); optionally add `restart()` + badge still absent, to catch the API/persistence failure |
| 18 | MP `contactMenuMarkUnread` (MP:37) | "Mark unread" sets the mark (an empty badge); the menu flips to "Mark read", which clears it | yes | model `unreadChat` + UI (count of empty Text grows) + menu keys | CATCHES: the menu not flipping (NAV:292-296), no badge for `unreadChat` (CPV:464,496). MISSES: the mark not persisting | counting empty strings (MP:41,44) is indirect, though `>` keeps it robust (PLAUSIBLE: fragile if another empty Text is added to the row) | KEEP |
| 19 | MP `contactMenuRightClickOpens` (MP:54) | Right-click (desktop) / long-press (Android) opens the contact menu with its 6 items | yes | menu keys + global label texts | CATCHES: context-menu gesture broken; items or labels missing; "Mute" label wrong. MISSES: item order | labels are matched with a global `seesText`. Low risk | KEEP |
| 20 | MP `groupMenuMuteModesAllMentionsNone` (MP:64) | Group mute cycle All→Mentions→None→All with labels Mute / Mute all / Unmute | yes | menu label text (via `rowMenu` label) + model `enableNtfs` | CATCHES: `nextMode(true)` cycle or labels broken. MISSES: the row's mute icon following the mode (the catalogue claims it does, CAT01:86) | the row effect is not checked (CONFIRMED) | STRENGTHEN: after each step assert `"Notifications" in/!in rowDescriptions("team")` (present for Mentions and None, absent for All, CPV:463,502-511). No new keys needed |
| 21 | MP `notesMenuMarkUnreadAndClear` (MP:78) | Notes: Mark unread, then Clear → "Clear private notes?" → empties notes and resets the mark | yes | model + exact alert texts + row text | CATCHES: wrong dialog (notes vs chat, NAV:463-472); stats not reset. MISSES: little | none | KEEP |
| 22 | MP `pendingInvitationRowIconAndName` (MP:96) | Pending 1-time link row: name, description, add-link icon | partial | UI texts + generic description + model `initiated` | CATCHES: wrong name/description strings. MISSES: the icon (ic_add_link vs ic_link, CCV:26). "profile image placeholder" is on every row without an avatar (CIMG:92,99) | the "icon" assertion is vacuous (CONFIRMED). `initiated` in the model is redundant with the "you shared one-time link" text | MERGE into #24 (same `keepOneTimeLink` setup; assert the texts as the precondition) |
| 23 | MP `pendingConnectionMenuSetName` (MP:106) | "Set contact name" opens the info screen; the alias renames the row | yes | UI texts + Alias key + model + row text "friend" | CATCHES: alias not saved or row not renamed. MISSES: focus of the field (catalogue `focusAlias`) | none | KEEP |
| 24 | MP `pendingConnectionClickOpensInfo` (MP:120) | Clicking the pending row opens "You invited a contact" with the link | yes | model `chatId` + UI texts + key | CATCHES: click handler (NAV:157-159) broken. MISSES: the QR shown | none | KEEP (absorb #22) |
| 25 | MP `pendingConnectionMenuDeleteInitiated` (MP:131) | Deleting a pending link makes it unusable on the server | yes, high value | alert texts + model gone + **CLI proves the link is dead** (`/_connect plan`, `/c` → auth failed) | CATCHES: deletion that is only local (the server queue kept), which is a privacy and security regression. MISSES: nothing major | the poll re-sends a CLI command each iteration (MP:140-143). Intentional, works | KEEP |
| 26 | MP `contactMenuForNotReadyContactOnlyDelete` (MP:148) | A prepared contact (no connection) offers only Delete | yes | model precondition + absence of 7 keys + "Add to list" | CATCHES: actions offered on `activeConn == null` (NAV:296), which would fail or crash. MISSES: the delete flow itself (only the alert title) | none | KEEP |
| 27 | M `contactMenuMarkReadAndUnread` (M:10) | read → unread → read | yes | UI + model | nothing #17 and #18 do not catch | first half is line-for-line #17 (M:10-16 vs MP:28-35). Second half is a weaker #18 (model only). Not named in the catalogue (CONFIRMED, grep) | DROP |
| 28 | M `contactMenuFavoriteUnfavorite` (M:23) | Favorite / Unfavorite toggle | yes | UI description | CATCHES: toggle broken. MISSES: persistence | overlaps #11 (favourite half identical, R:166-167 vs M:26-27) | MERGE into #11 |
| 29 | M `contactMenuMuteUnmute` (M:32) | Mute / Unmute toggle | yes | UI description | CATCHES: toggle broken. MISSES: persistence; labels | overlaps #10 (mute half identical, R:151-152 vs M:35-36) | MERGE into #10 |
| 30 | M `contactMenuClearChat` (M:41) | "Clear" empties the conversation but keeps the contact | yes | UI: preview text gone | CATCHES: clear not wired; clear deleting the chat (`rowTexts` → `chatId` poll fails). MISSES: the "Clear chat?" alert text and its contact name (`ChatInfoView.kt:499-507`); the contact still working afterwards | does not check which dialog was confirmed (CONFIRMED: M:46 clicks a generic Confirm) | STRENGTHEN: assert `seesText("Clear chat?")` before confirming; afterwards have bob send a new message and see it in the row (proves the contact is intact, a real user check) |
| 31 | RC `contactWithRequestOpenToAccept` (RC:15) | Opening an "Open to accept" row shows the Reject/Accept bar instead of the composer | yes | model `chatId` + keys + labels + no Message field | CATCHES: the composer shown for a pending request (`ComposeView.kt:1821`). MISSES: n/a | same setup as #32/#33; #32 implicitly needs the bar | MERGE into #32 (assert the bar state before clicking Accept) |
| 32 | RC `contactWithRequestComposeAccept` (RC:28) | Accept from the bar connects | yes | **CLI** "alice: contact is connected" + model + composer back | CATCHES: accept broken end to end. MISSES: the alert path for addresses without short-link data (CRA:93-96) | `!seesText("Accept contact request")` (RC:37) is redundant: if the alert had appeared, bob would not have connected | KEEP |
| 33 | RC `contactWithRequestComposeReject` (RC:41) | Reject → alert → row removed, back to the list | yes | alert texts + model gone + `chatId == null` | CATCHES: reject alert, `dismissToChatList` (CRA:125-131). MISSES: core keeping the request (no CLI-side effect exists by design) | none | KEEP |
| 34 | RC `oneHandToolbarCardSelectBottomBar` (RC:54) | Reachable-toolbar card switches top/bottom | yes | pref + **stored setting on disk** + which new-chat button exists (layout effect) | CATCHES: the toggle not persisted or the layout not following it. MISSES: `reverseLayout` of the list | sets `oneHandUICardShown=false` through the pref (no first-run path). Acceptable | KEEP |
| 35 | RC `oneHandToolbarCardDismissShowsAlert` (RC:71) | Closing the card shows the "Reachable app toolbars" alert and hides the card for good | yes | UI texts + stored setting | CATCHES: onClose (CLV:136-141) broken | none | KEEP |
| 36 | RC `oneHandCardAutoHiddenAfterThreeChats` (RC:81) | The card disappears by itself once there are 3 chats, without an alert | yes | key + stored setting + no alert | CATCHES: threshold (CLV:1132-1136) changed. MISSES: threshold counting under a filter | none | KEEP |
| 37 | RC `connectBannerCardOpensNewLink` (RC:99) | Banner "New 1-time link" opens New chat on that tab with a link created | yes | UI texts + model `showingInvitation` + link field | CATCHES: wrong tab or no link created. MISSES: banner visibility rules (`hasConversations`, OnboardingCards.kt:69-77) | none | KEEP |
| 38 | RC `connectBannerCardOpensPasteLink` (RC:115) | Banner "Paste link" opens the Connect-via-link tab (Android adds Scan) | yes | UI texts, platform-specific | CATCHES: wrong tab; Android camera section missing | none | KEEP |
| 39 | RCC `contactRequestIncognitoHiddenWithShortLinkData` (RCC:10) | "Accept incognito" is hidden when the address carries short-link data | partial: only one branch | model precondition + menu keys + label absent | CATCHES: "Accept incognito" offered where core rejects it. MISSES: **the other branch**. If "Accept incognito" disappeared for every address (NAV:555 condition inverted or the item deleted), this passes. Every GUI-created address has short-link data, so the `false` branch is never reached | negative-only (CONFIRMED). The legend requires `open class …CompatTest` plus a `…CompatStableTest` that runs with the stable CLI peer (00-legend.md:42). The class is final (RCC:9) and no `CompatStableTest` exists anywhere (CONFIRMED, grep) | STRENGTHEN: pair it with the catalogue's FIXTURE test (address without short-link data → "Accept incognito" shown → CLI sees a random name). Make the class `open` and add the stable subclass, or drop the `S` flag from the catalogue |

## Summary

### (1) Verdict counts (39 tests)
- **KEEP: 23.** R #1, 2, 4, 5, 6, 10, 11, 16; MP #17, 18, 19, 21, 23, 24, 25, 26; RC #32, 33, 34, 35, 36, 37, 38
- **STRENGTHEN: 5.** #3 `previewLink`, #15 `previewContactShouldAcceptForAddress`, #20 `groupMenuMuteModesAllMentionsNone`, #30 `contactMenuClearChat`, #39 `contactRequestIncognitoHiddenWithShortLinkData`
- **MERGE: 8.**
  - #7 → #10
  - #8 + #9 → one test
  - #14 → #1
  - #22 → #24
  - #28 → #11
  - #29 → #10
  - #31 → #32
- **DROP: 3.** #12 `incognitoIconOnRow`, #13 `verifiedIconOnRow`, #27 `contactMenuMarkReadAndUnread`

After the merges and drops: about 28 tests, with no loss of coverage. Each merge removes a full app start plus CLI connection (and for #8/#9 a group setup).

### (2) Five most valuable
1. **`pendingConnectionMenuDeleteInitiated`**: the only test here that checks a privacy-relevant side effect outside the app. The CLI shows the deleted link is refused by the server. It catches a delete that only happens locally.
2. **`contactWithRequestComposeAccept`**: end-to-end accept, with the CLI peer confirming the connection. Together with `…Reject` it covers the main path for address requests.
3. **`contactMenuForNotReadyContactOnlyDelete`**: guards a real state edge (`activeConn == null`, NAV:296) where offering mark read, mute or clear would call the API on a contact with no connection.
4. **`mutedIconWhenNoUnread`** (with `favoriteStarWhenNoUnread`): checks the precedence logic of the trailing slot (CPV:464-521) as UI descriptions and texts in three states. This is real rendering logic that is easy to break.
5. **`previewGroupInvitationJoinAs`** / **`previewContactShouldAcceptForAddress`**: they exercise the `chatPreviewInfoText` branches (CPV:183-223), which decide what a user sees for pending states. The incognito "Join as" text is privacy-relevant.

### (3) Five weakest
1. **`verifiedIconOnRow`**: checks nothing on the row (the icon has a null description, CPV:98) and is a strict subset of `ContactInfoTest.contactInfoMarkVerified`.
2. **`incognitoIconOnRow`**: model only, incognito set up through the API (RS:20-24). Its one other assertion (`notes.incognito == false`) is hard-coded true in the model (`ChatModel.kt:3163`).
3. **`contactMenuMarkReadAndUnread`**: a line-for-line duplicate of `contactMenuMarkRead` plus a weaker `contactMenuMarkUnread`. Not in the catalogue.
4. **`unreadCounterGrayWhenMuted`**: the feature in its name (grey colour, CPV:497) is not checked at all. No success screenshot exists (DUT:331), even though the catalogue says it "shows in the screenshot".
5. **`contactRequestIncognitoHiddenWithShortLinkData`**: tests only the hidden branch, which is the branch every GUI user hits anyway. Removing the feature passes it. The promised stable-peer variant does not exist. (Runner-up: `pendingInvitationRowIconAndName`, whose "icon" check matches the generic placeholder description on any row.)

### (4) Important features with no test, ranked by regression risk
1. **"Accept incognito" for contact requests** on addresses without short-link data: row menu (NAV:555-565) and the accept-bar alert (CRA:93-96, `showAcceptRequestAlert` CRA:138). Privacy-critical: a broken incognito accept shares the real profile. These are catalogue rows `contactRequestMenuAcceptIncognito` and `contactWithRequestComposeAcceptAlertIncognito` (FIXTURE), not implemented.
2. **Legacy request rows** (`ChatInfo.ContactRequest`, "wants to connect to you!", tap → accept/reject alert, NAV:130-147). Users upgrading from old databases see them. Catalogue `contactRequestLegacyRowWantsToConnect` and `contactRequestTapAlert*` are not implemented.
3. **Media previews in rows** (image, video, voice with in-row playback, file; CPV:331-354, 422-455). This is the most complex code in the row, and it includes blur/privacy and an `AudioPlayer` side effect. Catalogue `previewImage`, `previewVoice` and `previewFile` are not implemented.
4. **"Show last messages" off** (`showChatPreviews=false`, CPV:251,427). A privacy setting whose whole effect is on the row. Catalogue 04 `morePrivacy_showLastMessages` is not implemented.
5. **Group row states and menu:**
   - invited-group menu Join / Join incognito / Delete (NAV:318-326)
   - inactive overlay for left, removed or deleted groups (CPV:72-79)
   - "Open to join" / "Open to connect" for prepared groups and business chats (CPV:205-209)
   - "reviewed by admins" (CPV:215-216)
   - report and support flag icons (CPV:388-393)
6. **Member-contact requests** (`MemberContactRequestMenuItems`, NAV:580-600) and **contact-card rows** ("Tap to Connect" → incognito/current profile choice, CPV:186-187; `directChatAction`).
7. **Persistence of row state across restart** (favourite, mute, unread mark, pending alias). `restart()` exists but is unused here. Mark read in particular updates the model before the API (NAV:668-679), so an API failure shows up only after a restart.
8. **Pending "accepted invitation" row and its delete** (`pendingAcceptedRowIcon`, `pendingConnectionMenuDeleteAccepted`), "connecting…" (`previewConnectingForInvitation`), connect-banner dismiss (`connectBannerCardDismiss`), and the GetStake banner. All are in the catalogue and none is implemented.

### (5) Overall judgement
These are real UI tests of real features. Most act through the real context menu, read the row's actual text and description nodes, and where a peer-visible effect exists (accept, pending-link delete, reply delivery) they confirm it through the CLI peer.

The keys-only/model compromise is mostly sound here. The model is usually an extra assertion next to a UI check, not the only one. The exceptions are where an icon has no semantics: verified, incognito, link thumbnail, pending-row icon, badge colour. There the test quietly checks a core or model fact instead and still carries the UI feature's name. Those tests (#3, #7, #12, #13, #22) would pass with the visual feature deleted, and they inflate the coverage count.

The second systemic weakness is duplication. Mark read, mute and favourite are each exercised by 3-4 tests (R, MP and M overlap), while whole branches of the row are untested:
- media previews
- hidden previews
- incognito accept
- legacy requests
- group invitation and inactive states
- persistence

For regression-catching, the suite reliably catches:
- broken menu wiring
- wrong text branches in `chatPreviewInfoText`
- the precedence logic of the trailing slot
- card and banner navigation

It will not catch:
- visual regressions (colour, icons, thumbnails)
- persistence bugs
- anything in the privacy-relevant branches listed in (4)

Spending the effort saved by the merges and drops on items 1-4 of (4) would raise the suite's value much more than more menu-toggle tests.
