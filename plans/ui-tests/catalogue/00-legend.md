# UI test catalogue 0: shared legend and rules

Applies to catalogues 01–04. The plan (`20261010-desktop-ui-test-dsl-plan.md`, §10) points here.

## Paths

All paths are repo-relative:
- `C/` = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`
- `D/` = `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/`
- `H/` = `tests/ChatTests/`; other Haskell tests are given as `tests/<File>.hs`
- Haskell core paths without a prefix (`Library/…`, `Store/…`, `Types/…`, `Messages.hs`, …) are under `src/Simplex/Chat/`; `Agent/…` and other simplexmq paths are in simplexmq at the commit pinned by `cabal.project`
- UI strings: `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`
- **Log** / "the command log" in any catalogue is the app's command log, `chatModel.terminalItems`: `sendCmd` adds `TerminalItem.cmd(obfuscated)` and `.resp` (`C/model/SimpleXAPI.kt:936-954`); commands sent with `log=false` (the subscription total :560, the servers summary :569, badge redemption :580) never appear there. `<uid>` and `<userId>` in logged commands are the same user id; JSON arguments are pretty-printed (`SimpleXAPI.kt:6546-6551`), so assertions match `"key": value` with a space
- **Pref** `<Key>` in catalogues 01–04 is the key in `settings.properties` (key strings at `C/model/SimpleXAPI.kt:390-530`, properties at :103-280); theme overrides live in `themes.yaml` instead; catalogue 05 writes `Pref <property> = <value>` with the AppPreferences property name, since Android has no `settings.properties`

Line numbers are those of master at `d72f68e6d` (2026-10-10), checked out at `~/wt-uitest-ref`.

At P1 the catalogue moves into the repo at `apps/multiplatform/common/src/desktopTest/resources/uitest-catalog/`.

## Canonical row format (parsed by the traceability check)

- Every feature table has a column titled exactly **`Test case(s)`**. It contains only backticked test names separated by `, `. Nothing else goes in that cell.
- A test name is a valid Kotlin identifier, unique across all four catalogues.
- Test names in prose, notes or other columns are not counted.
- Every catalogue ends with a table titled `## Count` with columns `Section | Tests | Ignored`. `Tests` counts the names in that section's `Test case(s)` cells; `Ignored` counts the subset that must be `@Ignore` (see below). The last row is `| **Total** | **N** | **M** |`.
- Features that can't be tested in-process are rows too: their names go in the `Test case(s)` column like any other, and the `Needs` cell says `IGNORE: <reason>`; the reason must fall under a plan §11.3 bullet. The `@Ignore` string is the cell text after `IGNORE: `, verbatim, including any parenthetical. The traceability check requires a `@Test` method for every name; for `IGNORE` rows that method carries `@Ignore("<reason>")`.
- **Needs apply to every name in the row.** A row whose tests need different things is split, so that counts and JUnit categories derived from Needs are exact.
- **Needs cells contain only codes from the table below**, each optionally followed by `×N` (number of peers for `P`, of relays for `RELAY`, e.g. `P ×3`, `RELAY ×2`) and/or a short parenthetical detail (e.g. `FIXTURE (legacy address)`, `FD (wallpaper image)`). Setup states that are not harness capabilities (lock on, two users, four chats, a group) belong in the row's notes, not in Needs.
- Section headings carry the section's test count, e.g. `### B. Composing and sending text, links, mentions, commands (28 tests)`; the file header states the totals; nothing follows the `## Count` table.

## Users and CLI output

- The GUI user is **alice**, created with a display name only (no full name). CLI peers are created with `--user-display-name <name>` and also have no full name.
- So CLI lines never contain a full name: `alice: contact is connected`, `alice wants to connect to you!`, not `alice (Alice): …`.
- When a Haskell test is cited, its expected lines are rewritten for these roles and names. The GUI user's own output is asserted on the GUI, never as a CLI line.

## Needs codes (one shared set)

| Code | Meaning |
|---|---|
| `P` | CLI peer(s), the current build unless the row names a stable one |
| `S` | also run with the stable-release CLI peer. These tests live in their class base's separate `open class <Base>CompatTest` (class map), whose subclass `<Base>CompatStableTest` overrides the peer version; never combined with `RELAY` (channels need current-build peers). S rows currently run only against the current CLI build; the stable-peer run is not wired (plan 0.5.11) |
| `SMP` | local SMP server (always available, so never needed in a cell; stopping or starting it is `SRVCTL`) |
| `SMP2` | a second local SMP server (private routing, manual server add, a peer on another server) |
| `XFTP` | informational: the row transfers files through the local XFTP server. The server is always available (like `SMP`), so the code derives no category and requires nothing; it only marks file rows |
| `XFTP2` | a second local XFTP server (unknown-relay approval) |
| `RELAY` | chat relay CLI process(es) |
| `NAMES` | SMP server in names mode + mock name registry |
| `HTTP` | local HTTP server (link previews, `http://127.0.0.1:<port>`) |
| `SOCKS` | in-JVM SOCKS5 stub |
| `SRVCTL` | stop/start a local server mid-test |
| `PEERCTL` | stop/start a CLI peer or relay, or restore its database copy |
| `RESTART` | core restart inside the test |
| `FIXTURE` | database fixture (named in the row): prepared before boot via `app(fixture = …)`, or between `restart()` steps for data the test produced itself (then also `RESTART`) |
| `ONBOARD` | the test boots with no user (`app(name, onboarded = false)`); tests that reach onboarding later need no `ONBOARD`: through a restart they have `RESTART`, and in-app (e.g. deleting the database, which shows onboarding at once) they need neither |
| `FD` | file-dialog completion, or asserting that no file dialog is awaiting (`UiFiles`, plan §7.6) |
| `CB` | in-memory clipboard (`LocalClipboardManager`/`LocalClipboard`) |
| `IMGCB` | the approved Ctrl+V clipboard seam (plan §11.2): Ctrl+V in the compose field when the clipboard holds plain text or an image (a file list needs only `CB`) |
| `URI` | `LocalUriHandler` stub (links, browser, calls) |
| `NTF` | recording notification manager |
| `TOAST` | toast assertions |
| `DT` | developer tools on |
| `REC` | recorder seam (plan §11.2) |
| `AV` | audio/video player seam (plan §11.2) |
| `MCAST` | multicast on loopback (plan §8.3); runs in the non-parallel `Multicast` category |
| `SLOW` | 30 s or more (timed-message deletion, TTL expiry, large groups) |
| `REALTIME` | needs the real display clock: `uiTest(realTimes = true)`, no screenshots (plan §7.7) |
| `—` | no other code applies: no peer and no harness capability |
| `IGNORE: <reason>` | cannot run in-process; `@Ignore` with this reason |

## Catalogue 05 (Android-only)

`05-android-only.md` covers UI that exists only on Android or behaves materially differently there (added 2026-10-12). It is for the Android harness (`plans/ui-tests/2026-10-12-android-ui-test-automation.md`, Waydroid), not the desktop run: its tests are outside the desktop counts, class map and traceability check above, and the count tools read 01–04 only. It defines its own extra Needs codes (`PERM`, `SYSUI`, `SYSNTF`, `INTENTS`, `INTENT`, `BG`, `BACK`, `IME`, `VIEW`, `SHELL`, `ROT`) and IGNORE reasons at its top; rows of 01–04 carry over to Android unchanged unless `05-android-carryover.md` lists them (REPLACED by a 05 test, DESKTOP-ONLY, or ADAPT with the Android change; 348 of 1,155 listed, 2026-10-19).

## Ownership of overlapping features

A feature is tested once, in the catalogue that owns it. Duplicates are deleted from the other catalogue. Rows that are unique stay where they are.

| Feature | Owner | The other catalogue keeps only |
|---|---|---|
| User picker, chat profiles list, add/activate/search/hide/unhide/mute/delete profile, your-profile editing, user chat preferences | 01 (§5, §14) | 04: settings-root rows, colour-mode switch, Esc keys, lock-protected profile actions |
| Onboarding incl. passphrase setup and the onboarding "Link a mobile" screen (QR, device name, back) | 01 (§1) | — |
| What's New (auto-show, pages, read-more, from settings) | 01 (§2) | 04 Help: none |
| SimpleX Lock notice, delivery-receipts startup prompt, app-update channel notice | 04 | 01: none |
| Remote desktop pairing, use, disconnect, stored pairing, delete, port, multicast — including pairing from onboarding | 04 (§K) | 01: the onboarding "Link a mobile" screen UI only |
| Message draft setting ("Message draft" in More privacy): turning it off | 04 (privacy) | 02 §S: draft behaviour with the setting on (default) |
| Sent-via-proxy setting ("Show message status") and the proxy icon | 04 (network) | 02: none |
| "Remove link tracking" setting and its effect while typing | 04 (privacy) | 02: none |
| "Show encryption" setting and the lock icon on items | 04 (privacy) | 02: none |
| Group invitation item in the direct chat with the host (tap to join, incognito label, sent item), and the join-time "Invitation expired!" alert | 03 (§8) | 02 §U: the expired invitation item label (an `IGNORE` row: never produced by the core) |
| Group event items (merged/collapsed events, expand/hide, role changes) | 03 (§16) | 02: selecting a merged range (§K) and deleting it (§U) |
| Welcome message received as the first item | 03 (§4) | — |
| Group disappearing messages (preference and expiry) | 03 (§5) | 02 §D: direct-chat timed messages |
| Group toolbar ⋮ entries ("Invite members", "Group link") | 02 (§A) | 03: entry points on the group info screen |
| "Send link previews" setting | 04 (privacy): turning it off | 02 §B: previews with the setting on (default), the first-time alert, fetch, cancel, received cards |
| "Auto-accept images" setting | 04 (privacy): turning it off | 02 §G: automatic receipt with the setting on (default) |
| Calls: toolbar call buttons, call items, incoming call alerts, the WebRTC `IGNORE` row | 02 | 01 §13: the contact-info call buttons and their "prohibited"/"allow" alerts |
| Chat-list rows and row menus for every chat type (rendering, flags, favorite, tags, mute modes) | 01 (§3) | 03: group-specific actions (join/delete invitation, leave, delete, archive reports) and the report/support/pending flags asserted in their own sections |
| A contact deleted by the peer (row label, disabled compose and its alert) | 01 (§15) | — |
| Private notes clear | 01 (§3, notes menu) | — |
| Address and business-address settings | 01 (§11) | 03: business-chat flows (customer and business sides, team members) |
| Contact, invitation, address, bot, name, signature, URI and link-card connection plans; prepared direct chats | 01 (§8, §9) | — |
| Group, channel, relay-address and business connection plans; joining groups and channels; prepared groups and business chats | 03 (§8, §19, §20) | 01: search filtering on a pasted known group link (§3), the channel button of a SimpleX-name plan (§8), pending-connection info of a group link (§10) |
| Mentions while composing (picker, filter, limit, received highlight) | 02 (§B) | 03 §15: mentions-only mute notification, blocked member's mention ignored |
| Moderation (single, full delete, multiple, received moderation, not offered) | 03 (§14) | 02: rendering of blocked-by-admin and collapsed marked-deleted items |
| Reports: member side (compose, reasons, sent alert, not offered, preference off, not visible to other members) | 02 (§O) | — |
| Reports: moderator side (banner, reports view, archive, delete, multi-archive, jump to reported message, archive all from chat list) | 03 (§13) | — |
| Member contacts: create, accept, reject, incognito | 03 (§11) | 02: the "requested connection" event item; 01: none |
| Clear chat and chat TTL from contact info | 01 (§13) | — |
| Clear chat from group info | 03 (§17) | — |
| Chat toolbar ⋮ menu (incl. mute) | 02 (§A) | 01: none |
| Contact preferences | 01 (§14) | 02: feature items and offers in the chat, the compose "Allow voice messages?" alert |
| Group preferences, their effects in group chats (incl. reactions or voice off) and the resulting feature items | 03 (§5) | 02: the disabled attach button when group files are off (§F), reports with the preference off (§O) |

## Class map

Each sub-area (a `###` section) is one test class, or several consecutive classes when it has more than 30 tests. A class holds the names from its first test up to the next class's first test, in catalogue order. Packages: catalogue 01 → `chat.simplex.app.ui.main`, 02 → `chat.simplex.app.ui.chat`, 03 → `chat.simplex.app.ui.groups`, 04 → `chat.simplex.app.ui.settings`. Within a class's range, names whose row has `S` go to `open class <Base>CompatTest` (plus `@Category(StableCompat::class) class <Base>CompatStableTest : <Base>CompatTest()`), the others to `<Base>Test`. The traceability check reads this table; regenerate it with the counts after any change.

| Cat | § | Sub-area | First test | Classes | Tests | Ignored |
|---|---|---|---|---|---|---|
| 01 | 1 | Onboarding | `onboardingWhySimplexIsBuiltOpensHowItWorks` | `OnboardingTest` | 18 | 0 |
| 01 | 2 | What's New | `whatsNewFromSettings` | `WhatsNewTest` | 5 | 0 |
| 01 | 3 | Chat list | `chatListScrollToTopOnTitleClick` | `ChatListToolbarSearchTest` | 16 | 0 |
| 01 | 3 | Chat list | `unreadFilterToggle` | `ChatListFiltersListsTest` | 24 | 0 |
| 01 | 3 | Chat list | `previewShowsLastTextMessage` | `ChatListRowsTest` | 17 | 0 |
| 01 | 3 | Chat list | `contactMenuMarkRead` | `ChatListMenusPendingTest` | 11 | 0 |
| 01 | 3 | Chat list | `contactRequestMenuAccept` | `ChatListRequestsCardsTest`, `ChatListRequestsCardsCompatTest`, `ChatListRequestsCardsCompatStableTest` | 24 | 0 |
| 01 | 4 | Servers summary | `serversSummaryShowsLocalSmpConnected` | `ServersSummaryTest` | 7 | 0 |
| 01 | 5 | User picker and profiles | `userPickerOpensAndShowsCurrentUser` | `UserPickerTest` | 5 | 0 |
| 01 | 5 | User picker and profiles | `profilesListShowsAllVisible` | `ChatProfilesTest` | 14 | 0 |
| 01 | 5 | User picker and profiles | `editProfileDisplayName` | `ProfileEditTest` | 7 | 0 |
| 01 | 6 | New chat sheet and contacts | `newChatSheetShowsActions` | `NewChatSheetTest` | 10 | 0 |
| 01 | 7 | 1-time link (create / share / QR / incognito / profile) | `createOneTimeLinkShowsLinkAndQr` | `OneTimeLinkTest`, `OneTimeLinkCompatTest`, `OneTimeLinkCompatStableTest` | 14 | 0 |
| 01 | 8 | Paste link and connection plans (contact, invitation, address, bot, name, signature, URI and link-card links) | `pasteLinkInvalidTextAlert` | `ConnectPlanTest` | 15 | 0 |
| 01 | 9 | Connecting a prepared chat (compose area) | `preparedContactInvitationConnectButton` | `PreparedChatTest` | 6 | 0 |
| 01 | 10 | Pending connection info screen | `pendingInfoInvitedSharesWorkingLink` | `PendingConnectionTest` | 5 | 0 |
| 01 | 11 | Address screens | `addressScreenNoAddressShowsCreate` | `AddressTest` | 12 | 0 |
| 01 | 12 | SimpleX name | `simplexNameEntryShownWithAddress` | `SimplexNameTest` | 8 | 0 |
| 01 | 13 | Contact info screen | `contactInfoSetLocalAlias` | `ContactInfoTest` | 24 | 0 |
| 01 | 14 | Preferences | `contactPrefsSaveNotifiesContactAndEnforces` | `ContactPreferencesTest` | 4 | 0 |
| 01 | 15 | Delete contact variants | `deleteContactOnlyConversation` | `DeleteContactTest` | 7 | 0 |
| 02 | A | Chat view shell, toolbar, banner | `directChat_opens_showsToolbarAndBanner` | `ChatShellTest` | 8 | 0 |
| 02 | B | Composing and sending text, links, mentions, commands | `sendText_sendButton_peerReceives` | `ComposeTest` | 21 | 0 |
| 02 | C | Live messages | `liveMessage_menuStart_alertConfirm_peerSeesStarted` | `LiveMessageTest` | 5 | 0 |
| 02 | D | Timed / disappearing messages | `timed_send30s_bothSidesDelete` | `TimedMessageTest` | 5 | 0 |
| 02 | E | Voice messages | `voice_record_send_peerReceivesVoice` | `VoiceMessageTest` | 11 | 0 |
| 02 | F | Attachments – sending | `attachFile_withText_peerReceivesTextAndFile` | `SendAttachmentTest`, `SendAttachmentCompatTest`, `SendAttachmentCompatStableTest` | 14 | 1 |
| 02 | G | Receiving files: download, cancel, errors, save, viewers | `rcvImage_autoReceived_renders` | `ReceiveFileTest` | 18 | 0 |
| 02 | H | Replies / quotes | `reply_menu_sendQuoted_peerSeesQuote` | `ReplyTest`, `ReplyCompatTest`, `ReplyCompatStableTest` | 6 | 0 |
| 02 | I | Edit and history | `edit_menu_updatesPeer` | `EditMessageTest`, `EditMessageCompatTest`, `EditMessageCompatStableTest` | 5 | 0 |
| 02 | J | Delete / reveal / blocked | `delete_ownForEveryone_peerSeesMarkedDeleted` | `DeleteMessageTest`, `DeleteMessageCompatTest`, `DeleteMessageCompatStableTest` | 4 | 0 |
| 02 | K | Multi-select and selection toolbar | `select_toggle_updatesCounter` | `MultiSelectTest` | 3 | 0 |
| 02 | L | Forwarding | `forward_contactToContact_peerSeesForwarded` | `ForwardTest` | 9 | 0 |
| 02 | M | Reactions | `reaction_chipClickOthers_adds` | `ReactionTest`, `ReactionCompatTest`, `ReactionCompatStableTest` | 4 | 0 |
| 02 | N | Copy / Share / Save / text selection | `copy_text_toClipboardAndToast` | `CopyShareTest` | 5 | 0 |
| 02 | O | Reports, member side | `report_member_spam_moderatorsReceive` | `MemberReportTest` | 2 | 0 |
| 02 | P | Chat item info | `info_sent_showsSentAt` | `ItemInfoTest` | 10 | 0 |
| 02 | Q | Unread handling, scrolling, pagination | `unread_openChat_scrollsToFirstUnread` | `UnreadScrollTest` | 12 | 0 |
| 02 | R | Search and content filters | `search_direct_filtersResults` | `ChatSearchTest` | 6 | 0 |
| 02 | S | Draft persistence | `draft_onlyLastChatKept` | `DraftTest` | 5 | 0 |
| 02 | T | Status icons and delivery | `status_direct_sentThenDelivered` | `ItemStatusTest`, `ItemStatusCompatTest`, `ItemStatusCompatStableTest` | 8 | 0 |
| 02 | U | Special items | `featureOffer_accept_updatesPrefs` | `SpecialItemTest`, `SpecialItemCompatTest`, `SpecialItemCompatStableTest` | 16 | 1 |
| 02 | V | Private notes | `notes_sendText_saved` | `NotesTest` | 5 | 0 |
| 02 | W | Context menus and keyboard | `contextMenu_itemsMatrix_byItemType` | `ContextMenuTest` | 1 | 0 |
| 02 | X | Rare states and out-of-reach rows | `call_webrtcSession_connects` | `ChatEdgeCasesTest` | 4 | 2 |
| 03 | 1 | Create group | `createGroup_buttonDisabledForEmptyOrInvalidName` | `CreateGroupTest` | 8 | 0 |
| 03 | 2 | Invite members | `inviteMembers_listsOnlyEligibleContacts` | `InviteMembersTest`, `InviteMembersCompatTest`, `InviteMembersCompatStableTest` | 8 | 0 |
| 03 | 3 | Group info screen rows | `groupInfo_headerShowsProfileAndCopiesName` | `GroupInfoTest` | 11 | 0 |
| 03 | 4 | Group profile and welcome message | `editGroupProfile_renameNotifiesMembers` | `GroupProfileTest` | 10 | 0 |
| 03 | 5 | Group preferences | `groupPrefs_ownerChangesNotifyAndRestrictMembers` | `GroupPreferencesTest` | 6 | 1 |
| 03 | 6 | Member admission and review | `memberAdmission_unsavedChangesAlert` | `MemberAdmissionTest` | 7 | 2 |
| 03 | 7 | Group link | `groupLink_cliJoinsViaFullLink` | `GroupLinkTest` | 8 | 0 |
| 03 | 8 | Joining (alice as joiner), connect-plan states, invitations | `joinGroup_pasteShortLinkPrepareAndJoin` | `JoiningTest`, `JoiningCompatTest`, `JoiningCompatStableTest` | 16 | 0 |
| 03 | 9 | Member list and multi-select | `memberList_rolesAndStatuses` | `MemberListTest` | 5 | 0 |
| 03 | 10 | Member info | `memberInfo_openShowsSections` | `MemberInfoTest` | 22 | 0 |
| 03 | 11 | Member contacts | `memberContact_createAndSendInvitation` | `MemberContactTest` | 4 | 0 |
| 03 | 12 | Support chats, «Chats with members» / «Chat with admins» | `supportChat_memberSendsToAdmins` | `SupportChatTest` | 6 | 0 |
| 03 | 13 | Reports, moderator side | `reports_moderatorBannerAndCounts` | `ModeratorReportTest` | 5 | 0 |
| 03 | 14 | Moderation | `moderate_fullDeleteWhenEnabled` | `ModerationTest` | 3 | 0 |
| 03 | 15 | Mentions, notification side | `mentions_mentionsOnlyMuteCountsOnlyMentionsForOtherProfile` | `MentionNotificationTest` | 2 | 0 |
| 03 | 16 | Group events and history | `groupEvents_memberConnectedMerged` | `GroupEventsTest` | 6 | 0 |
| 03 | 17 | Leave, delete, clear | `leaveGroup_thenDeleteLocalCopy` | `LeaveDeleteClearTest` | 7 | 0 |
| 03 | 18 | Channels: alice as owner, with relays and CLI subscribers | `createChannel_noRelaysBlocksCreation` | `ChannelOwnerCreateTest` | 16 | 0 |
| 03 | 18 | Channels: alice as owner, with relays and CLI subscribers | `channel_broadcastPlaceholderAndDelivery` | `ChannelOwnerBroadcastTest` | 15 | 0 |
| 03 | 18 | Channels: alice as owner, with relays and CLI subscribers | `channelSubscribers_listAndCount` | `ChannelOwnerManageTest` | 27 | 1 |
| 03 | 19 | Channels: alice as subscriber, with a CLI owner and relays | `joinChannel_pasteLinkOpenChannel` | `ChannelSubscriberTest` | 26 | 0 |
| 03 | 20 | Business chats | `business_autoReplyWelcome` | `BusinessChatTest` | 9 | 0 |
| 04 | A | Settings root and global keys | `settingsRoot_rowsOpenTheirScreens` | `SettingsRootTest` | 5 | 0 |
| 04 | B | Privacy (Your privacy, More privacy, receipts) | `privacy_sectionsAndFooter` | `PrivacyTest` | 22 | 0 |
| 04 | C | SimpleX Lock (desktop passcode) and self-destruct | `lock_enable_setsPasscode` | `LockTest` | 13 | 0 |
| 04 | D | Appearance | `appearance_language_switch` | `AppearanceTest` | 18 | 0 |
| 04 | E | Network & servers | `operators_enableDisableOperator` | `NetworkServersTest` | 18 | 0 |
| 04 | E | Network & servers | `socks_enable_confirm` | `NetworkProxyTest` | 15 | 0 |
| 04 | F | Audio & video calls | `calls_screenLayout_noLockScreenOption` | `CallSettingsTest` | 6 | 0 |
| 04 | G | Chat data, database, encryption, startup DB errors | `chatData_ttlDecrease_alertConfirm` | `ChatDataTest` | 12 | 1 |
| 04 | G | Chat data, database, encryption, startup DB errors | `dbEncrypt_changePassphrase` | `DatabaseEncryptionTest` | 7 | 1 |
| 04 | H | Migrate to/from device | `migrateFrom_stopsChatAndAuth` | `MigrationTest` | 15 | 6 |
| 04 | I | App version, update checker, developer tools | `version_screenShowsVersions` | `VersionDevToolsTest` | 14 | 0 |
| 04 | J | Help & support | `help_layoutHowToUseAndAbout` | `HelpTest` | 4 | 2 |
| 04 | K | Remote desktop: "Link a mobile" (the desktop GUI is the controller, the CLI is the mobile host) | `linkMobile_layout` | `RemoteDesktopTest` | 17 | 0 |
| 04 | L | Supporter perks (badge-display screens excluded) | `badges_supportScreenLayout` | `SupporterPerksTest` | 8 | 1 |
| 04 | M | Out of reach in-process | `tray_closeToTrayBehaviour` | `SettingsOutOfReachTest` | 8 | 8 |

## Count rules

- After any change, recount with `tools/recount.py <file>` (it rewrites the section headings and the `## Count` table from the `Test case(s)` cells), then update the file header and regenerate the class map (`tools/classmap.py table`).
- A class holds at most 30 tests (a sub-area with more is split, class map); the largest class now has 27, the bound the plan quotes for per-JVM leaks (§7.3).
- The plan's numbers derived from the catalogue (Summary, §7.7 screenshot count, §10 table, §10.1, §10.2, §11.1 NAMES/RELAY, §11.2, §12, §14) are regenerated from the four `## Count` tables, the class map and the Needs counts (`tools/catcheck.py`, `tools/classmap.py`, `tools/needs.py`).
