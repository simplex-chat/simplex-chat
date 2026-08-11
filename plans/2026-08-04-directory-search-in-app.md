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

## 1. Directory: search over RPC

New `Directory/Rpc.hs`, JSON-derived, re-declared by hand in Kotlin and Swift.

```haskell
data DirectorySearchRequest = DirectorySearchRequest
  { searchText :: Text                -- the string a user sends the bot in chat
  , cursor :: Maybe DirectoryCursor   -- opaque to the app; Nothing = first page
  }

data DirectorySearchEntry = DirectorySearchEntry
  { entryType, displayName, simplexName, groupLink, activeAt, createdAt  -- as DirectoryEntry, Listing.hs:70
  , shortDescr :: Maybe Text          -- stored form, not DirectoryEntry's MarkdownList
  , image :: Maybe ImageData          -- group profile picture, as the bot already sends it
  }

data DirectorySearchResponse = DirectorySearchResponse
  { entries :: [DirectorySearchEntry], cursor :: Maybe DirectoryCursor }  -- Nothing = no more
```

This is the bot's existing text search (`STSearch`), a `LIKE '%…%'` over registered group and channel **profiles** — display name, full name, short description, welcome message, and the SimpleX name when the domain is verified (Store.hs:394-401). No conversation content is involved. No sort parameter: the directory's other modes are the separate `/all` and `/new` commands (Service.hs:1141-1142). No page-size parameter: it uses its own `searchResults` (Service.hs:1284, default 10).

Paging is `/next` with the state moved. In chat the directory keys the saved search on the contact (`TM.lookup (contactId' ct) searchRequests`, Service.hs:1128, written :1299, 5-minute expiry :1131). Over RPC it gets only a per-request `invId` (Subscriber.hs:1368) — nothing identifying the requester — so there is no key to look a `next` up against, and the cursor comes back in the request instead. `DirectoryCursor` is opaque to the app — a JSON value it stores and echoes back verbatim, never inspects. It MUST be typed as an opaque blob in the bindings (`JsonObject` / `Data`), not as an id, because the paging fix below changes it from a bare group id to a sort value plus a tie-break id; opacity is what keeps that fix out of the apps.

Entry differences from `DirectoryEntry`: `welcomeMessage` dropped (it carries the whole group description, Listing.hs:105-119); `shortDescr` sent as stored `Text` (Types.hs:882) rather than `MarkdownList`, which would ship a Linux-shaped `Format` iOS cannot decode — apps parse markdown locally via `chat_parse_markdown` (Core.kt:31) if the row needs it; `imageFile` (a web-listing path) replaced by `image`, the profile picture the bot already sends as `MCImage` (Service.hs:1310), bounded to 12,500 characters of data URI (`maxProfileImageSize`, Commands.hs:157-159; matches `resizeImageToStrSize`, GroupProfileView.kt:95).

**Response budget and compression.** A service response travels as a confirmation: `APISendServiceResponse` (Commands.hs:1474) -> `sendServiceReplyAsync'` -> `replyRequest_` -> `prepareReply` -> `storeConfirmation` (agent Agent.hs:4183-4191), padded to `e2eEncConnInfoLength` — **11,106 bytes** with PQ on at agent version >= 5 (agent Protocol.hs:349-353), not the 14,832 of the non-PQ branch. The reply connection inherits `PQSupportOn` from the requester (agent Agent.hs:1730, :1736 -> `newConnToAcceptDR` :1399), so 11,106 is the operative figure. Over-length fails in `pad` (agent Crypto.hs:1054), which surfaces to the app only as `ASETimeout`.

**The response MUST be compressed, exactly as conn info is.** A service response is the same payload class as conn info — both are padded to `e2eEncConnInfoLength` — and the chat layer already compresses that class: `encodeConnInfoPQ` (Internal.hs:2250-2261) compresses with `compressedBatchMsgBody_` when PQ is on and the body exceeds `maxCompressedInfoLength` = **10,968** (Protocol.hs:939-940, defined as `maxEncodedInfoLength - 3726, see e2eEncConnInfoLength in agent`), and fails over the cap after compressing. The service path must do the same:

- directory side: encode the response JSON, compress with `compressedBatchMsgBody_` (Protocol.hs:1005, marker `'X'`) when over `maxCompressedInfoLength`, and treat still-over-cap as an internal error rather than attempting the send;
- requester side: `APISendServiceRequest` currently decodes the reply as raw JSON (`J.eitherDecodeStrict' respData`, Commands.hs:1455) and must first undo the marker and `decompress1`, mirroring the chat parser at Protocol.hs:957 (`'X' -> decodeCompressed`). `compress1`/`decompress1` are agent-side (`Simplex.Messaging.Compression`), so both ends already have them. Apply the same to the request direction (Subscriber.hs:1369) for symmetry, though search requests are far below the cap.

`10968` is the constant to size against, not the raw 11,106 — it is the chat layer's already-correct expression of the same agent limit.

**Entries per page is still bounded by images.** `image` is base64 of already-compressed JPEG/PNG, so compression recovers roughly the base64 expansion and no more: a near-cap 12,500-character image lands at roughly 9,400 bytes, against a 10,968 budget. One image-bearing entry per response; two will not fit whatever the page size. `searchResults` (default 10) is therefore not the binding constraint — the envelope is. The app shows whatever fits and pages the rest manually (§6); streaming the response is the eventual fix and is out of scope here.

**Handler.** Add `DEServiceRequest` to `DirectoryEvent` (Events.hs:48) and a `CEvtServiceRequest` case to `crDirectoryEvent_` (:81); in `directoryServiceEvent` (Service.hs:321) decode, search, reply with `APISendServiceResponse`. Malformed JSON never reaches the bot — the core rejects it before emitting the event (Subscriber.hs:1367-1374), so the handler only ever sees a well-formed object and only has to answer for a wrong shape or unknown method, which returns the error envelope, never a chat message. The response must likewise be a JSON object: the core decodes it into `J.Object` and fails the whole call otherwise (Commands.hs:1455).

**Page size.** `searchListedGroups` takes `pageSize`; the handler passes `searchResults` (Options.hs:195, default 10) as the in-chat path does, then fills `entries` until the compressed response would exceed `maxCompressedInfoLength` and returns a cursor for the rest. Because images bind the page to roughly one entry, the cursor MUST come from the last **included** row, not from `last gs` as the chat path does (Service.hs:1290) — otherwise truncated rows are skipped permanently. Entries need the group link, which `searchListedGroups` does not return (Store.hs:343) and without which `groupDirectoryEntry` yields `Nothing` (Listing.hs:126-130) — extend it as `getAllListedGroups_` does (Store.hs:341).

Run the handler off the event loop (`forkIO`, as `sendFoundGroups` does at Service.hs:1301) with bounded concurrency: the directory processes registrations, captchas and owner commands on one sequential consumer (Service.hs:263-269) fed by an unbounded `TQueue` (:105, :133), so the handler must return to the loop immediately and the concurrency bound must live in the forked side.

**Rate limiting is global, and that is a deliberate privacy trade.** A signed request would carry a caller identity — `CEvtServiceRequest` has `signerKey` (Controller.hs:952), populated end to end (agent Agent.hs:1716-1724; test Direct.hs:1928) — but a stable key would correlate every search a user ever makes, so the app does not sign, and no per-caller key exists. The cost is that the global cap is a denial-of-service surface: anyone can exhaust it and block directory search for everyone. Accepted for now; the mitigations are to make the cap generous relative to legitimate traffic, and to shed load by **rejecting** over-cap requests (`rejectServiceRequest`, a fast error) rather than dropping them, so the victim of a flood gets an immediate failure instead of a 10 s timeout.

Per-request agent state on the responder is released and needs no work here: `replyRequest_` deletes the invitation (agent Agent.hs:1686), `sendReplyAsync` enqueues `ICReplyDel` which sends then deletes the connection (:1695, :2232-2234), and a periodic sweep clears expired requests at `serviceResponseTimeout` = 180 s (:3365-3368).

**Cursor fix — separate commit, first.** All three search modes order by one key and page by an unrelated one, so `/next` skips and repeats rows in every mode. `/next` replays whichever `searchType` was saved (Service.hs:1136), so all three MUST be fixed together:

| mode | `ORDER BY` | cursor predicate today |
| --- | --- | --- |
| `STAll` | `members_count DESC, group_reg_id ASC` (:359) | `r.group_id > ?` (:354) |
| `STRecent` | `r.created_at DESC, group_reg_id ASC` (:371) | `r.group_id > ?` (:366) |
| `STSearch` | `members_count DESC, group_reg_id ASC` (:388) | `r.group_id > ?` (:378) |

`STRecent` is the worst: `created_at DESC` walks newest-first while `group_id > ?` excludes everything below the cursor, so `/next` after `/new` returns almost nothing.

The predicate MUST match the sort key, and **the `ORDER BY` tie-break changes with it** — today it is `r.group_reg_id`, which is not selected (`groupRegFields`, Store.hs:450) and diverges from `group_id` after a deregister/re-register. Tie-break both on `r.group_id` (unique, Migrations.hs:36). Use the OR form; the mixed-direction sort rules out a row-value comparison:

```sql
-- STAll, STSearch
AND (g.summary_current_members_count < ? OR (g.summary_current_members_count = ? AND r.group_id > ?))
ORDER BY g.summary_current_members_count DESC, r.group_id ASC

-- STRecent
AND (r.created_at < ? OR (r.created_at = ? AND r.group_id > ?))
ORDER BY r.created_at DESC, r.group_id ASC
```

The cursor therefore carries the sort value plus `r.group_id`, so `SearchRequest.lastGroup :: GroupId` (Search.hs:10) becomes a pair, and `updateSearchRequest` (Service.hs:1296-1300) takes it from the last row rather than its `groupId` alone. The count queries take the same predicate, so `n` stays the remainder `moreGroups` needs (Service.hs:1289).

`testSearchGroups` (DirectoryTests.hs:505) misses all of this because its fixture registers groups in id order with equal member counts, so `group_reg_id` and `group_id` agree. It needs a fixture whose member counts are not monotone in id, plus a `/new` case.

## 2. Core: bot support and the DR address

`startChatController` already threads a `serviceRequests` flag (Commands.hs:220); only `Core.hs:93` hardcodes it off. Add `serviceRequests :: Bool` to `CoreChatOpts` with a `--service-requests` switch in `coreChatOptsP` (Options.hs:71, :244), read it at Core.hs:93 (adding it to the destructuring at :90), and give it `False` at the two other full-record sites — `mobileChatOpts` (Mobile.hs:251) and `testCoreOpts` (ChatClient.hs:136); the cabal `-Werror=` list covers `incomplete-record-updates` but not `missing-fields`, so the compiler will not point them out. The directory inherits it via `coreChatOptsP` (Directory/Options.hs:52) and `mkChatOpts` (:216-218). The requester needs no flag.

Because the switch is opt-in, a deployment that omits it fails silently — every search times out after 10 s. Add `-- TODO [directory] default service requests on for the directory binary` at the switch, and make the operator-facing failure loud in the meantime.

**The address is upgradable in place; the published link does not change.** Requests need address ratchet keys or fail with `ASENotDRAddress` (agent Agent.hs:1739-1740), and `APICreateMyAddress` generates them only when `pqRatchet` is `Just` (Commands.hs:2439-2443), which the bot's start-up path is not. But `setConnShortLink` **creates keys when there are none** (agent Agent.hs:1171-1178), so `APIRotateAddressRatchetKeys` (Commands.hs:2478, `rotateKeys = True`) or `APIAddMyAddressShortLink userId (Just True)` (:2474) upgrades the existing address, reusing the same `shortLinkKey` and `linkId` (agent Agent.hs:1181-1185) so the link string is unchanged. `keepAddressKeys` retains earlier generations (:1057-1060), so rotating does not break in-flight requests. Nothing needs recreating and no published link is invalidated.

**Only the short link carries the keys.** `setMyAddressData` leaves `connFullLink` untouched (Commands.hs:4039) and that link was built with `useDR = False`, while `serviceRequest_` reads the keys off the resolved URI — so any full-link path yields `ASENotDRAddress` permanently. The app MUST use the short link (§3). Note the links shipped today are full links to the older directory address (WhatsNewView.kt:512, :575; WhatsNewView.swift:293, :355), and no directory short-link constant exists in the codebase yet — obtaining and publishing it is a prerequisite, not a code change.

Cover the upgrade with a test: `/ad`, rotate, start a responder with `service_requests=on`, expect the exchange of `testServiceRequestResponse` (Direct.hs:1903) instead of `ASENotDRAddress`. Do not extend `testServiceRequestNonDRAddress` (:1966) — it has no responder and can only observe failure.

## 3. Apps: API binding

Kotlin `CC.APISendServiceRequest(userId, target, timeoutSec, request: JsonObject)` with `cmdString` matching the parser (Commands.hs:5523 — `/_service_request <userId> <target>[ timeout=<s>][ sign_key=<k>] <json>`; the request is never signed, see §1), a `CR.ServiceResponse` case, and `apiSearchDirectory(rh, text, cursor)` wrapping the envelope. iOS: the same as a `ChatCommand` case plus a response case in `ChatResponse1` (AppAPITypes.swift:819), which already carries the command-result cases.

The app ships the directory's **short link** as a constant; it cannot cache a resolved target, since `APISendServiceRequest` resolves internally and returns only the response.

The call blocks until reply or timeout — pass `requestTimeout` of **10 s**, and use `withLongRunningApi` (Utils.kt:43), not the single-threaded `withBGApi` (:38). Show progress through the existing `ConnectProgressManager` (ChatModel.kt:55-78, ChatModel.swift:303-329): `startConnectProgress(text, onCancel)` when the request goes out, `stopConnectProgress()` when it returns. It already withholds the spinner for 1 s, and the search bar already renders it (ChatListView.kt:522, ChatListView.swift:670); `onCancel` gives the user a way out of the wait.

**The progress manager needs an owner, because a search and a connect can overlap.** Tapping a result while the search is still running is normal: `planAndConnect` calls `cancelConnectProgress()` then `startConnectProgress(...)` (ConnectPlan.kt:48-50, NewChatView.swift:1331-1334), so today the search's `onCancel` would fire and, worse, the search's later `stopConnectProgress()` would kill the *connect* spinner. Add an owner tag to the single slot rather than a second spinner — the search and the connect are the same kind of wait in the same place, and two spinners would be worse UX:

- `startConnectProgress(text, owner, onCancel)` records the owner alongside the text;
- `stopConnectProgress(owner)` is a no-op unless the current owner matches, so a late search result cannot clear a connect spinner;
- `cancelConnectProgress()` keeps its current unconditional behaviour — a user-initiated cancel, and the takeover in `planAndConnect`, should still cancel whatever is running.

Two owners for now (`directorySearch`, `connect`); existing call sites pass `connect` and keep today's semantics. The dispose-time cancels (ChatListView.kt:66, NewChatSheet.kt:46) are correct for a search too and need no change. Guard stale responses with a generation counter bumped on text change, profile switch, remote-host switch and filter reset; a text comparison alone lets a pre-switch request return and repopulate a cleared list. No remote-host allowlist change is needed (`allowRemoteCommand` is a deny-list, Controller.hs:735). The request carries no profile, so incognito needs nothing.

## 4. Search UI

**Rows.** `SearchInDirectoryRow` goes beside `ConnectByNameRow` in the same slot on all four surfaces (ChatListView.kt:1109 via `TagsOrConnectByName` :1095; ChatListView.swift:803 via the `oneHandUI` branches :650-704; NewChatSheet.kt:322,413; NewChatMenuButton.swift:96-104). On a phone:

| search text | slot |
| --- | --- |
| empty | tags |
| non-empty, not a name candidate | Search in Directory |
| name candidate (5+ chars, ChatListView.kt:1043) | Search in Directory + Connect to \<name\> |

The rows replace the tags exactly as `ConnectByNameRow` does today, so the tag rule is unchanged — only the range of text that produces a row widens. Desktop shows tags alongside, also as today. The directory row is hidden for a SimpleX link, which connects on its own.

The slot still needs widening, and it is wider than it looks. `TagsOrConnectByName` has **three** branches, not two — `candidate == null -> TagsView`, mobile -> `ConnectByNameRow`, desktop -> `desktopView(candidate)` (ChatListView.kt:1100-1105) — and the desktop lambda is supplied separately at each of the two call sites, in opposite order because one has the search bar below and the other above (:976-980, :987-991). All four places change: the mobile branch and both lambdas must emit up to two rows, and the `candidate == null` branch must now distinguish empty text (tags) from non-empty text (Search in Directory). On iOS the same widening applies to the one-hand branch (:651-661) and the top-bar branch, which is already additive (:696-704).

**Keyboard.** Kotlin already sets `ImeAction.Search` (SearchTextField.kt:105) with no `keyboardActions`; add `onSearch: (() -> Unit)? = null` feeding `KeyboardActions`, defaulting to `KeyboardActions.Default`. `SearchTextField` has five call sites: pass the handler from **ChatListView.kt:760** (the chat list bar — the primary surface) and from `ContactsSearchBar` (NewChatSheet.kt:494), wired only from the New-chat callers (:312, :403) and not from the Deleted-chats ones (:715, :725) that share it; leave DefaultTopAppBar.kt:84, AddGroupMembersView.kt:217 and GroupChatInfoView.kt:1386 untouched. iOS has the same sharing hazard with `ContactsListSearchBar` (NewChatMenuButton.swift:333, used at :87 and :469). iOS also needs `.submitLabel(.search)` and `.onSubmit` (ChatListView.swift:665 and the New chat equivalent). Desktop shares `SearchTextField` with no soft keyboard — confirm Enter reaches the handler.

**Actions.** The keyboard key runs the local filter, resolves the typed name online when it is ≥5 characters, and runs the directory search; the row runs the directory search only. Name resolution must be silent — pass a false `inProgress` as the typing path does (ChatListView.kt:818), or `apiConnectPlan` pops "SimpleX name not found" (SimpleXAPI.kt:1540); iOS calls the alert unconditionally (SimpleXAPI.swift:1046) and needs the same guard.

## 5. Results

Three sections: local chats (unheadered, as today), then **Names**, then **Directory**. Each new section, header included, renders only when it has rows.

**Name rows** come from the `apiConnectPlan` result for the resolved name and render like a chat list row — avatar, profile name, description — with the searched name appended as `(@name)` / `(#name)`, since the profile name may differ. At most two: a bare term resolves as both `@name` and `#name`, a prefixed one only as itself (ChatListView.kt:819). A name row replaces the "Connect to \<name\>" action row for that text.

Resolve with `PRMUnknown` (Controller.hs:684): it resolves only chats that are not already known, which is exactly the wanted behaviour here — `PRMNever` is the local-only mode the typing path already uses, and `PRMAllGroups` would re-resolve known groups over the network for no gain.

**Nothing is shown twice.** The typing path already resolves the name locally and puts any hit into `searchChatFilteredBySimplexLink`, so a known chat is already rendered in the local section, and the candidate is cleared once every target is known (ChatListView.kt:812-824). So: a name that resolves to a chat already in the local section produces **no** Names row, and a directory entry whose link matches a chat already shown in the local or Names section produces no Directory row. De-duplication is by chat id for local matches and by link for directory entries, applied before render and again when "Show more" appends.

**Directory rows** render the same way from the response: `image`, `displayName` with its `#`/`@` prefix, `simplexName` as plain text, `shortDescr`, member count from `DETGroup.summary`. The verb comes from `DETGroup.groupType` — `compose_view_join_channel` / `compose_view_join_group` (strings.xml:625-626), defaulting to group when absent. Do not reuse `SimplexNameView`: it auto-verifies under `DEFAULT_PRIVACY_VERIFY_SIMPLEX_NAMES` (ChatInfoView.swift:1411), so each row would fire a network call. The name shown is the directory's claim; connecting uses the link, as the in-chat flow does today.

Tap → `planAndConnect(rhId, shortLink ?: fullLink)`; both `PublicLink` fields are optional (Listing.hs:63-66) and entries with neither are dropped. The link is also the row key, and "Show more" de-duplicates by it. Already-joined is handled inside `planAndConnect` (ConnectPlan.kt:322-333), so results need no pre-check. Render directory text as untrusted: no markdown links, no HTML, clamped lengths.

**Placement.** Declaration order is search bar, local matches, Names, Directory in both bar positions — the search bar is declared first regardless of mode (ChatListView.kt:952 before :1001; ChatListView.swift:399-412 before :422/:431), and one-hand mode flips only the physical direction. On Kotlin the new items go before `ChatListFeatureCards` (:1007); on iOS each row carries the same per-row `.scaleEffect` flip. On New chat they go after `itemsIndexed(filteredContactChats)` (NewChatSheet.kt:341, :430) and after the `ContactsList(…)` call (NewChatMenuButton.swift:159-168) — not inside `ContactsList` (:223), which Deleted chats also uses (:482).

**Empty states** are centred overlays gated on the local filter being empty — ChatListView.kt:1016, ChatListView.swift:466, and "You have no chats" at ChatListView.kt:419. They now depend on whether an online search has run:

- **not yet run** (typing) — unchanged, including "You have no chats" when there are no local chats;
- **run, anything found** in either new section — no overlay, results only;
- **run, nothing found anywhere** — "no chats found".

New chat has its own: `NoFilteredContactsItem`, local to `NewChatSheetLayout` (NewChatSheet.kt:261-272), and the iOS states inside the shared `ContactsList` (:262-266), needing a defaulted parameter or lifting to `NewChatSheet`.

**The search bar must always be present**, since it is now a discovery instrument. Two gates hide it today and both have to go: iOS renders it only when the chat list is non-empty (ChatListView.swift:397-398), and the onboarding state replaces the whole list — `ConnectOnboardingView` (ChatListView.swift:383-386) and, on Kotlin, `ChatListWithLoadingScreen`'s onboarding branch (ChatListView.kt:413-414). Keep the onboarding content, but render it below a live search bar rather than instead of it.

On Kotlin that branch is `if (appPlatform.isAndroid) AndroidOnboardingCards()`, so on **desktop** the onboarding state renders nothing at all today — no cards, no list, no search bar. Desktop must show the search bar there too; with no cards to keep, the branch becomes search bar only.

## 6. Pagination

Directory only — the Names section is bounded at two rows. The view model holds `entries`, `cursor`, `loading`, `error`; no count, since nothing renders one.

The app renders exactly what the response carries — as many entries as fit the envelope, which is often a single group when the entry has a full-size image (§1). A short first page is acceptable; the app does not try to disguise it.

Filling the list is manual. **"Show more"** is the last row of the Directory section: it re-calls with the stored cursor, appends de-duplicated by key, and disappears when the cursor is `Nothing`; on failure it becomes a retry row. No auto-paging and no prefetch — each request is a full DR handshake, so filling in the background would multiply handshakes for results the user has not asked for.

Streaming the response is the eventual answer to the one-entry page and is out of scope here (see the closing section). It changes how entries arrive, not this view model, which already accumulates pages behind a cursor.

Changing the search text discards both sections and the cursor, as do profile and remote-host switches — `activeFilter` already clears `searchText` (ChatListView.kt:1034) and must clear results with it.

## 7. Warning

The first online search from either trigger shows a three-action alert — **Cancel** / **Search** / **Search and don't show again** — the third writing `directorySearchAlertShown`, added to `hintPref` (SimpleXAPI.kt:273) and `hintDefaults` (SettingsView.swift:145) so "Reset hints" restores it. Kotlin uses `showAlertDialogButtonsColumn`; iOS an equivalent three-button alert.

The copy names both recipients, since the Search key makes two requests: the search text to the directory, and a name-shaped term additionally to a name server. It should be clear that whatever is in the box is sent — the same field is the local chat filter. Claim no more: no contact is created and no profile is sent.

## 8. Strings

Kotlin: `common/src/commonMain/resources/MR/base/strings.xml`, beside `connect_plan_connect_to_name` (:19). iOS: inline `NSLocalizedString` at the use site (ChatListView.swift:815). Needed: the row label, two section headers, "Show more", the alert title/body/three buttons, the empty state, the error text.

## 9. Failure modes

| Case | Surfaced as |
| --- | --- |
| Address not DR, or a full link used | `ASENotDRAddress` — a deployment bug: log, generic failure |
| Directory over its cap | rejected fast → retry row, not a 10 s wait (§1) |
| Directory silent | `ASETimeout` after ~10 s → retry row |
| Response over `maxCompressedInfoLength` | send fails on the directory; the app sees `ASETimeout` → retry row. Prevented by filling the page against the compressed size (§1) |
| Network down | fails within the 10 s budget → retry row; no indefinite wait |
| Response malformed or oversized | parse failure → error row, log the size |
| Response late, or after a profile/host switch | dropped by the generation counter |
| Name resolves to nothing | no row, no alert |
| No directory results | section omitted; empty state only when every section is empty |
| Result already joined | `planAndConnect` opens it (ConnectPlan.kt:322-333) |

## 10. Order of work

1. Page `searchListedGroups` by its sort key instead of `group_id` in **all three modes**, so `/next` stops repeating and skipping rows (§1), plus the `testSearchGroups` fixture and a `/new` case — standalone, no app dependency.
2. `--service-requests` and the directory enabling it; upgrade the address to DR and confirm with the new test (§2).
3. RPC schema, `Directory/Rpc.hs`, handler off the event loop — CLI-testable with `/_service_request`.
4. iOS: API binding, view model, rows, button, keyboard, alert, pagination — chat list, then New chat.
5. Kotlin: port it verbatim — same function and variable names, same structure and logic, diverging only where the platform forces it.
6. Tests in `tests/Bots/DirectoryTests.hs`, following `Direct.hs:1903-1975`. Three harness changes first: `/ad` → `/ad pq_ratchet=on` (:1771); `mkDirectoryOpts` (:112-147) must set `serviceRequests` in `coreOptions`, as the bot starts through `runDirectory` → Core.hs:93; and the harness must expose the short link, which `getContactLink` (:1772) discards.

## Agent facts this plan depends on

`cabal.project` pins simplexmq `e3d53428`; the checkout at `/workspace/simplexmq-1` contains it, and every agent line cited here was read at that commit. The three facts the design rests on:

- **Response budget is 11,106 bytes** — `e2eEncConnInfoLength` with PQ on at agent version >= 5 (Protocol.hs:349-353), reached via `storeConfirmation` (Agent.hs:4191); over-length fails in `pad` and surfaces only as `ASETimeout`. Service responses are **not** compressed by the chat layer (§1).
- **Responder state is released** — invitation deleted in `replyRequest_` (Agent.hs:1686), connection deleted by `ICReplyDel` (:1695, :2232-2234), expired requests swept at 180 s (:3365-3368).
- **Address ratchet keys can be added to an existing address** without changing the link (Agent.hs:1171-1178, :1181-1185), and only the short link carries them (§2).

Error constructors, `timeout=` syntax and the short-link target form are pinned by `tests/ChatTests/Direct.hs:1903-1975`. Streaming, per-row image fetch, directory browse and self-hosted directory configuration are out of scope.
