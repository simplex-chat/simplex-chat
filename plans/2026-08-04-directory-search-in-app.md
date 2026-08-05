# SimpleX Directory search in app search

Make the directory searchable from the app's search fields — chat list and New chat — instead of only by chatting with the bot. Transport is the Service RPC from `e1a349b90`: one request, one response, over the directory's contact address.

```
search field ──"Search in Directory" / keyboard Search──▶ APISendServiceRequest(directory short link, JSON)
   ▲                                                          │
result rows ◀── CRServiceResponse ◀── APISendServiceResponse ◀─┴─ directory: CEvtServiceRequest → searchListedGroups
   │
   └─tap─▶ planAndConnect(link)   — existing flow: resolves the link, prepares the chat, opens it
```

Results are not chats and are never persisted; they live as long as the search text does.

## 1. Directory: answer search over RPC

New `Directory/Rpc.hs`, JSON-derived, re-declared by hand in Kotlin and Swift.

```haskell
data DirectorySearchRequest = DirectorySearchRequest
  { searchText :: Text                -- the same string a user sends the bot in chat (STSearch)
  , cursor :: Maybe (Int, GroupId)    -- last row's (membersCount, groupId); Nothing = first page
  }

data DirectorySearchEntry = DirectorySearchEntry
  { entryType, displayName, simplexName, groupLink, activeAt, createdAt  -- as DirectoryEntry, Listing.hs:70
  , shortDescr :: Maybe Text          -- as stored on the profile; the app parses markdown locally
  , image :: Maybe ImageData      -- the group profile picture, as the bot already sends it
  }

data DirectorySearchResponse = DirectorySearchResponse
  { entries :: [DirectorySearchEntry], cursor :: Maybe (Int, GroupId) }  -- Nothing = no more
```

This is the in-conversation text search (`STSearch`, Search.hs:13) and nothing more: no sort parameter — the directory's other two modes are separate commands, `/all` and `/new` (Service.hs:1141-1142), with no UI entry point here — and no page-size parameter, since the page size is the directory's own `searchResults` (Service.hs:1284, default 10 at Directory/Options.hs:195), as in chat. The cursor is the in-chat `SearchRequest.lastGroup` (Search.hs:10) widened to the corrected sort key below, and carried in the request because an RPC caller has no contact to hold server-side state against (`searchRequests :: TMap ContactId SearchRequest`, Service.hs:101). Both directions go in a versioned envelope (`{"v":1,"method":…}` / `{"v":1,"result":…}` or `{"v":1,"error":…}`), so browse or streaming can be added later without breaking clients.

Two projections away from `DirectoryEntry`: drop `welcomeMessage` (it carries the group's whole description plus a link line, Listing.hs:105-119, bounded only by `maxEncodedInfoLength = 14694`, Protocol.hs:937), and send `shortDescr` as `Text` rather than `DirectoryEntry`'s `MarkdownList`. `Text` is the stored form — `GroupProfile.shortDescr :: Maybe Text`, capped at 160 characters (Types.hs:882) — which the directory converts to `MarkdownList` only when generating the web listing (`toFormattedText`, Listing.hs:118); the app can parse it back with its own core through `chat_parse_markdown` (Mobile.hs:130, `parseToMarkdown`, Core.kt:31) if the row renders markdown. Sending `MarkdownList` would gain nothing and break iOS: `Format`'s JSON shape is build-dependent (`sumTypeJSON`, simplexmq Parsers.hs:110-115), so a Linux-built directory emits `{"type":…}` where iOS expects Swift's synthesized `{"caseName":…}` (ChatTypes.swift:5360), and the core passes the payload through unreshaped as a `J.Object` (Commands.hs:1455).

`image` is the same `GroupProfile.image` the bot already sends as `MCImage` (Service.hs:1310), bounded to 12,500 bytes by the apps that set it (GroupProfileView.kt:95, GroupProfileView.swift:116). So the handler queries its normal `searchResults` page as in chat, then fills `entries` until the response nears the envelope and sets the cursor to the last entry it included — with an avatar that is roughly **one entry per response** today, which is why streaming is the follow-up. `entries` is a list regardless, so streaming changes the transport, not the client model; the client must render a one-entry response correctly.

**Handler.** Add `DEServiceRequest AgentInvId (Maybe C.PublicKeyEd25519) J.Object` to `DirectoryEvent` (Events.hs:48) and a `CEvtServiceRequest` case to `crDirectoryEvent_` (:81). In `directoryServiceEvent` (Service.hs:321) decode, search, build entries, reply with `APISendServiceResponse`. Malformed JSON, unknown method or unsupported version return the error envelope, never a chat message. Reply promptly — a late reply is discarded by the client.

Entries need the group link, which `searchListedGroups` does not return (Store.hs:343) and without which `groupDirectoryEntry` yields `Nothing` (Listing.hs:126-130). Extend it to return `Maybe GroupLink` per row as `getAllListedGroups_` does (Store.hs:335-341); entries that still come out `Nothing` are skipped and excluded from paging.

**Do the work off the event loop.** The directory processes every event — registrations, captchas, link checks, owner commands — on one sequential consumer (Service.hs:263-269, :175-181), so answering inline lets unauthenticated callers starve moderation. Fork the handler as `sendFoundGroups` already does (Service.hs:1301) with bounded concurrency, and cap the global request rate. The cap can only be global: a service request carries no caller identity, just `invId`, an optional attacker-chosen `sigKey_` and the payload (Subscriber.hs:1365-1368). Each request also accrues agent state — `APISendServiceResponse` returns an `AgentConnId` (Commands.hs:1472) whose `SSENT` has no chat entity behind it (Subscriber.hs:131) — so size the cap against that too.

**Cursor fix (separate commit, first).** `searchListedGroups` orders by `summary_current_members_count DESC, r.group_reg_id ASC` (Store.hs:359, :388) or `r.created_at DESC, …` (:371) but pages with `AND r.group_id > ?` (:354, :366, :378) — a different column, unrelated to the sort key — so pages skip and repeat rows. Tie-break on `r.group_id` (unique, Directory/Store/SQLite/Migrations.hs:36; `group_reg_id` is not in `groupRegFields`, Store.hs:450) and use the OR form, since the mixed-direction sort rules out a row-value comparison:

```sql
AND (g.summary_current_members_count < ? OR (g.summary_current_members_count = ? AND r.group_id > ?))
AND (r.created_at < ? OR (r.created_at = ? AND r.group_id > ?))          -- recent
```

The count queries take the same corrected predicate, so `n` stays the remainder that in-chat `moreGroups = n - length gs` needs (Service.hs:1289, rendered :1311). This fixes `/next`'s skipping, not its counts, so `testSearchGroups` (DirectoryTests.hs:505) needs only a fixture whose member counts are not monotone in group id — its "for N more result(s)" assertions must still hold.

Listing is gated by admin approval before `GRSActive` (Service.hs:1316-1334); blocked words apply only to joining members' names (:293-294) and nothing else rate-limits the directory today (only captcha attempts, :775).

## 2. Core: bot support and the DR address

`startChatController mainApp enableSndFiles serviceRequests` (Commands.hs:219) already threads the flag; only `Core.hs:93` hardcodes it off. Add `serviceRequests :: Bool` to `CoreChatOpts` with a `--service-requests` switch in `coreChatOptsP`, beside `chatRelay` (Options.hs:71, :244), read it at Core.hs:93, and give it `False` at the two other full-record sites — `mobileChatOpts` (Mobile.hs:251) and `testCoreOpts` (ChatClient.hs:136); `-Wmissing-fields` is not fatal, so a miss is a runtime error on every call. The directory inherits it through `coreChatOptsP` (Directory/Options.hs:52). The requester side needs no flag — it gates only inbound `SREQ` (Subscriber.hs:1366).

**The address must be DR**, or requests fail with `A_SERVICE ASENotDRAddress`; `APICreateMyAddress` enables DR only when `pqRatchet` is `Just` (Commands.hs:2438-2442), and the bot's start-up path passes `Nothing`. Whether an existing address can be upgraded in place is unsettled: `APIRotateAddressRatchetKeys` (:2477) calls `setMyAddressData … (Just IKUsePQ)` on the existing connection, but `useDR` is a distinct parameter passed only to `prepareConnectionLink` at creation (:2442) and has no counterpart in `setConnShortLink` (:4035). Settle it with a test before choosing: create an address with `/ad`, rotate, start a responder with `service_requests=on`, and expect the `{"pong":2}` exchange of `testServiceRequestResponse` (Direct.hs:1903-1926) instead of `ASENotDRAddress`. Do not extend `testServiceRequestNonDRAddress` (:1966) — it has no responder and can only observe failure. If rotation does not work, recreate the address with `pq_ratchet=on`; replacing the published one also invalidates the What's New links (WhatsNewView.swift:293,355 — inlined in the English string that doubles as the localization key, so 35 files under `apps/ios`; WhatsNewView.kt:512,575 — one file).

## 3. Apps: API binding

Kotlin `CC.APISendServiceRequest(userId, target, timeoutSec, request: JsonObject)` with `cmdString` matching the parser (`/_service_request <userId> <target>[ timeout=<s>][ sign_key=<k>] <json>`, Commands.hs:5520), a `CR.ServiceResponse` case, and `apiSearchDirectory(rh, text, cursor)` wrapping the envelope. iOS: the same as a `ChatCommand` case plus a response case in the appropriate split `ChatResponse` enum.

The app ships the directory's **short link** as a constant. A short link costs one agent `getConnShortLink` fetch per request (Internal.hs:1557-1560); the app cannot cache the resolved target, since `APISendServiceRequest` resolves internally and returns only the response, so if that latency hurts on "Show more" the fix belongs in core.

The call blocks until the reply or the timeout — pass `requestTimeout` of **10 s** rather than the agent default. Use `withLongRunningApi` (Utils.kt:43), not the single-threaded `withBGApi` (:38). Guard stale responses with a generation counter bumped on text change, profile switch, remote-host switch and filter reset — a text comparison alone lets a request issued before a profile switch return afterwards and repopulate the list that switch cleared. No remote-host allowlist change is needed (`allowRemoteCommand` is a deny-list, Controller.hs:697-735); when a desktop drives a phone the phone makes the request. The request carries no profile (Commands.hs:1454), so incognito needs nothing.

## 4. Search UI

**Rows.** `SearchInDirectoryRow` goes beside `ConnectByNameRow` in the same slot on all four surfaces (chat list: ChatListView.kt:1109 via `TagsOrConnectByName` :1095, ChatListView.swift:803 via the `oneHandUI` branches :650-704; New chat: NewChatSheet.kt:322,413, NewChatMenuButton.swift:96-104). It shows for any non-empty search text that is not a SimpleX link; `ConnectByNameRow` keeps its 5-character minimum, since names cannot be shorter (ChatListView.kt:1043). The slot is currently an either/or keyed on `connectNameCandidate` (ChatListView.kt:1100-1106; iOS one-hand :651-661, while the top-bar block :696-704 is already additive), so it becomes a small column — the tags hide exactly when they hide for the name row today.

**Keyboard.** Kotlin already sets `ImeAction.Search` (SearchTextField.kt:105) with no `keyboardActions`. Add `onSearch: (() -> Unit)? = null` and `keyboardActions = if (onSearch != null) KeyboardActions(onSearch = { onSearch() }) else KeyboardActions.Default`, so DefaultTopAppBar.kt:84, AddGroupMembersView.kt:217 and GroupChatInfoView.kt:1386 stay unchanged. The New chat field is inside `ContactsSearchBar` (NewChatSheet.kt:476, field :494), shared with Deleted chats (:715, :725), so pass the handler only from the New-chat sites (:312, :403); iOS has the same hazard with `ContactsListSearchBar` (NewChatMenuButton.swift:333, used at :87 and :469). iOS also needs `.submitLabel(.search)` and `.onSubmit` on the search `TextField` (ChatListView.swift:665 and the New chat equivalent). Desktop shares `SearchTextField` and has no soft keyboard — confirm Enter reaches the handler.

**Actions.** The keyboard key runs the local filter, resolves the typed name online when it is a name candidate (≥5 characters), and runs the directory search. The "Search in Directory" row runs the directory search only. Name resolution must be silent: `apiConnectPlan` calls `apiConnectResponseAlert` whenever `inProgress` is true (SimpleXAPI.kt:1540) and pops "SimpleX name not found" for `NOT_FOUND` (:1602), so pass a false `inProgress` as the typing path does (ChatListView.kt:818); iOS calls it unconditionally (SimpleXAPI.swift:1046) and needs the same guard.

## 5. Results

Three sections: local chats (unheadered, as today), then **Names**, then **Directory**. Each new section — header included — renders only when it has rows.

**Name rows** come from the `apiConnectPlan` result for the resolved name (its `connLink` plus short-link profile data) and render like a chat list row: avatar, profile name, description. Append the searched name in parentheses — `(@name)` / `(#name)` — since the profile name may differ. At most two rows: a bare term resolves as both `@name` and `#name`, a prefixed one only as itself (ChatListView.kt:819, ChatListView.swift:741). When a row appears it replaces the "Connect to \<name\>" action row for that text, which carries only the guess.

**Directory rows** render the same way from the response: avatar from `image`, `displayName` with its `#`/`@` prefix, `simplexName` as plain text, `shortDescr`, member count from `DETGroup.summary`. The verb comes from `DETGroup.groupType` — `compose_view_join_channel` / `compose_view_join_group` (strings.xml:625-626; `"Join channel"` / `"Join group"`, ComposeView.swift:478), defaulting to group when absent. Do not reuse `SimplexNameView`: it auto-verifies under `DEFAULT_PRIVACY_VERIFY_SIMPLEX_NAMES` (ChatInfoView.swift:1411), so a list of rows would fire a network verification each. The name shown is the directory's claim; connecting uses the link, exactly as the in-chat bot flow does today, and the name is verified in chat info after joining.

Tap → `planAndConnect(rhId, shortLink ?: fullLink)`; both `PublicLink` fields are optional (Listing.hs:63-66) and the directory drops entries with neither. That link is also the row key, since entries have no id, and "Show more" de-duplicates by it — duplicate keys are a hard error in a `LazyColumn`. Already-joined and already-connecting are handled inside `planAndConnect` (`GroupLinkPlan.Known`, ConnectPlan.kt:322-333; iOS NewChatView.swift:1573-1584), so results need no pre-check. Render directory text as untrusted: no markdown links, no HTML, clamped lengths.

**Placement.** Declaration order is search bar, local matches, Names, Directory in both bar positions — the search bar is declared first regardless of mode (Kotlin `stickyHeader` ChatListView.kt:952 before `itemsIndexed(chats…)` :1001; iOS :399-412 before the `ForEach` :422/:431), and one-hand mode flips only the physical direction. On Kotlin the new items go before `ChatListFeatureCards` (:1007) so the promo card stays last; on iOS each row carries the same per-row `.scaleEffect` flip. On New chat they go after `itemsIndexed(filteredContactChats)` (NewChatSheet.kt:341, :430) and after the `ContactsList(…)` call in `NewChatSheet.viewBody` (NewChatMenuButton.swift:159-168) — not inside `ContactsList` (:223), which Deleted chats also uses (:482).

**Empty states are a condition change, not just wording.** Both platforms centre an overlay over the list whenever the local filter is empty — `chats.isEmpty() && chatModel.chats.value.isNotEmpty()` (ChatListView.kt:1016), `cs.isEmpty && !chatModel.chats.isEmpty` (ChatListView.swift:466), plus "You have no chats" for a zero-chat user (ChatListView.kt:419) — which is exactly the normal successful directory search. Every such condition must also require the new sections to be empty. New chat has its own: `NoFilteredContactsItem`, local to `NewChatSheetLayout` (NewChatSheet.kt:261-272), and the iOS states inside the shared `ContactsList` (:262-266), which need a defaulted parameter or lifting to `NewChatSheet`.

Reachability gates worth knowing: iOS renders the chat-list search bar only when the chat list is non-empty (ChatListView.swift:398), and on both platforms the has-chats-but-no-conversations onboarding state replaces the whole list (ChatListView.swift:384, ChatListView.kt:413).

## 6. Pagination

Directory only — the Names section is bounded at two rows by construction. The view model holds `entries`, `cursor`, `loading`, `error`; no count, since nothing renders one and `cursor = Nothing` already marks the end. "Show more" is the last row of the Directory section: it re-calls with the stored cursor, appends de-duplicated by key, and disappears when the cursor is `Nothing`. On failure it becomes a retry row. An explicit button rather than infinite scroll keeps the "requests only on an explicit action" property the warning promises. Changing the search text discards both sections and the cursor, as do profile and remote-host switches; `activeChatTagFilter` already clears `searchText` (ChatListView.kt:1034) and must clear results with it.

## 7. Warning

The first online search from either trigger shows a three-action alert — **Cancel** / **Search** / **Search and don't show again** — where the third writes `directorySearchAlertShown`, added to the `hintPref` list (SimpleXAPI.kt:273) and `hintDefaults` (SettingsView.swift:145) so "Reset hints" restores it. Kotlin builds it with `showAlertDialogButtonsColumn`; iOS with an equivalent three-button alert.

The copy names both recipients, because the Search key makes two network requests: the search text goes to the directory, and a name-shaped term (≥5 ASCII characters, completed with the default top-level part, ChatListView.kt:1041-1065) is additionally resolved through a name server. It should also be clear that what is sent is whatever is in the box — the same field is the local chat filter, so the user may have typed a contact's name and then pressed Search. Claim no more than that: no contact is created and no profile is sent.

## 8. Strings

Kotlin: `common/src/commonMain/resources/MR/base/strings.xml`, beside `connect_plan_connect_to_name` (:19). iOS: inline `NSLocalizedString` at the use site, as `ConnectByNameRow` does (ChatListView.swift:815). Needed: the row label, two section headers, "Show more", the alert title/body/three buttons, the empty state, the error text.

## 9. Failure modes

| Case | Surfaced as |
| --- | --- |
| Address not DR | `A_SERVICE ASENotDRAddress` — a deployment bug: log, generic failure |
| Directory silent or over its global cap | `A_SERVICE ASETimeout` after ~10 s → retry row |
| Network down | existing offline handling; must not spin |
| Response malformed or oversized | parse failure → error row, log the raw size |
| Response arrives after text changed, or after a profile/host switch | dropped by the generation counter |
| Name resolves to nothing | no row, no alert |
| No directory results | Directory section omitted; the empty state only when every section is empty |
| Result already joined | `planAndConnect` opens it (ConnectPlan.kt:322-333) |

## 10. Order of work

1. Cursor fix + `testSearchGroups` fixture — standalone.
2. Settle the DR-address question (§2), then `--service-requests` through `CoreChatOpts` and the directory enabling it.
3. RPC schema, `Directory/Rpc.hs`, handler off the event loop — CLI-testable with `/_service_request`.
4. Kotlin: API binding, view model, rows, button, keyboard, alert, pagination — chat list, then New chat.
5. iOS: the same.
6. Tests in `tests/Bots/DirectoryTests.hs`, following `tests/ChatTests/Direct.hs:1903-1975`. Three harness changes first: `/ad` → `/ad pq_ratchet=on` (:1771); `mkDirectoryOpts` (:112-147) must set `serviceRequests` in its `coreOptions`, since the bot starts through `runDirectory` → Core.hs:93 with no `/_start` to override it; and the harness must expose the short link, as `getContactLink` (:1772) discards it (Utils.hs:593-596).

## Verify against the pinned agent first

`cabal.project:24` pins simplexmq `ee4dd0d8`, which is not in this checkout, so two things behind the design are unread: the exact **service-response size limit** (which sets the per-response entry budget in §1) and whether per-request **agent state is released** on the responder after `SSENT` or its timeout. The error constructors, the `timeout=` syntax and the short-link target form are pinned by `tests/ChatTests/Direct.hs:1903-1975`. Streaming responses, directory browse, and self-hosted directory configuration are out of scope.
