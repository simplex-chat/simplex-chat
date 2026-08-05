# SimpleX Directory search in app search UI

Make the SimpleX Directory searchable from the app's own search fields — chat list and New chat — instead of only by chatting with the directory bot. Transport is the Service RPC added in `e1a349b90`: one request, one response, over the directory's contact address.

## Flow

```
search field ──"Search in Directory" row / keyboard Search──▶ APISendServiceRequest(target = directory address, JSON)
   ▲                                                            │  agent: one-shot request to a contact address
   │                                                            ▼
result rows ◀── CRServiceResponse(JSON) ◀── APISendServiceResponse ◀── directory: CEvtServiceRequest → searchListedGroups
   │
   └─tap─▶ planAndConnect(link)   (existing path: resolves the link, prepares the chat, opens it)
```

Results are not chats and are never written to the DB or to disk — they are view-model rows that live as long as the search text does, so a search leaves no history artifact.

## What already exists (verified)

- **Core RPC, requester side.** `APISendServiceRequest {userId, sendTarget :: ConnectTarget 'CMContact, requestTimeout, signKey, request :: J.Object}` (Controller.hs:413), handler and target resolution (Commands.hs:1452-1471), parser `/_service_request` (Commands.hs:5520), response `CRServiceResponse {user, responseData :: J.Object}`. The handler takes no chat lock and writes nothing to the chat store, so a blocking search does not stall other commands. It is already remote-allowed — `allowRemoteCommand` is a deny-list ending in `_ -> True` (Controller.hs:697-735).
- **The RPC contract is pinned by tests.** `tests/ChatTests/Direct.hs:124-127` covers it end to end (:1903-1975): the responder's address is created with `/ad pq_ratchet=on` and it starts with `/_start main=on snd_files=on service_requests=on`; the requester sends `/_service_request 1 <shortLink> {"ping":1}` — **a short link is a valid target** — and receives `service response: {"pong":2}`; a silent responder yields `A_SERVICE {serviceError = ASETimeout}` and a non-DR address `A_SERVICE {serviceError = ASENotDRAddress}`.
- **Directory listing DTO.** `DirectoryEntry` (Listing.hs:70-82) is already JSON-derived for the web listing: `entryType (DETGroup {groupType, admission, summary})`, `displayName`, `simplexName`, `groupLink :: PublicLink {connFullLink, connShortLink}`, `shortDescr`, `welcomeMessage`, `imageFile`, `activeAt`, `createdAt`. The RPC sends a projection of it (§1).
- **Directory search + cursor.** `searchListedGroups :: … -> SearchType -> Maybe GroupId -> Int -> IO (Either String ([(GroupInfo, GroupReg)], Int))` (Store.hs:343); `SearchRequest {searchType, searchTime, lastGroup}` (Search.hs:7-13) is the in-chat cursor; page size `searchResults = 10` (Directory/Options.hs:195).
- **Kotlin's keyboard key is already "Search".** `KeyboardOptions(imeAction = ImeAction.Search)` (SearchTextField.kt:105), with no `keyboardActions` at all — the label needs no change, only an action. iOS's `TextField` (ChatListView.swift:665) has no `submitLabel`, so iOS needs both.
- **The action-row slot, on both surfaces.** `ConnectByNameRow` renders an action row by the search bar in the chat list (ChatListView.kt:1109 via `TagsOrConnectByName` :1095; ChatListView.swift:803 via the two `oneHandUI` branches :650-704) and in New chat (NewChatSheet.kt:322,413; NewChatMenuButton.swift:96-104). The directory row belongs in the same slot on all four — but the slot's gating has to change (§4).
- **Connect from a result.** `planAndConnect(rhId, shortOrFullLink, …)` (ConnectPlan.kt:24) and its iOS twin (NewChatView.swift:1308) take a short **or** full link and run the whole flow; for a channel or group link the already-joined case is `GroupLinkPlan.Known` → `showOpenKnownGroupAlert` (ConnectPlan.kt:248, :322-333; iOS :1510, :1573-1584). No new connect code, and no need to pre-check results against local chats.
- **Typing a name stays offline.** Names resolve with `PlanResolveMode.PRMNever` (ChatListView.kt:821, ChatListView.swift:744), debounced 300 ms, filtering the in-memory list (`filteredChats`, ChatListView.kt:1445). A *pasted link* already connects online with no warning (ChatListView.kt:798-805; ChatListView.swift:714-724), so the new thing is a network request for a plain, non-link search term — that is what the warning covers.

## What does not exist — the work

1. **The directory cannot receive service requests.** `Core.hs:93` starts every CLI-hosted chat with `startChatController True True False`; the third argument is `processServiceRequests`, and when false `SREQ` is actively rejected (Subscriber.hs:1366-1372). `crDirectoryEvent_` (Events.hs:81) has no `CEvtServiceRequest` case.
2. **The directory's address is probably not DR.** `APICreateMyAddress` enables DR only when `pqRatchet` is `Just` (Commands.hs:2438-2442), and whether an existing address can be upgraded in place is unresolved — see §2.
3. No request/response schema, no directory handler, no app API binding, no result rows, no pagination state, no warning, no strings.

---

## Decisions I need from you

**D1 — How the app addresses the directory.** Both apps already embed the directory's old (v1-4) contact URI in What's New (WhatsNewView.swift:293,355; WhatsNewView.kt:512,575) — as literal copy, not as a reusable constant, and those four links break if the address ever changes. Those shipped URIs are `v=1-4` with only `smp=…dh=…` and no ratchet parameters, so they cannot serve as a DR target however §2 resolves. The remaining forms differ in cost: a short link costs one agent `getConnShortLink` fetch per request (Internal.hs:1557-1560), a name costs `resolveSimplexName` **plus** that fetch (Commands.hs:1466-1471) — on every request, including each "Show more". (`CTFullContact` is used as-is with no network step, Commands.hs:1459, but only if a DR-capable full URI form exists — an agent-source question.)
  - (a) ship the directory's short link as a constant — one fetch per request; re-pointing needs a release;
  - (b) address it by SimpleX name — re-pointable without a release, two round trips per request;
  - (c) a settings override for self-hosted directories, on top of (a) or (b).
  **Recommendation: (a) + (c).** The app cannot cache a resolved target — `APISendServiceRequest` resolves internally and returns only the response — so if the per-request fetch proves too slow for "Show more", the fix belongs in core (cache the resolved link for an address), not in the app.

**D2 — Avatars in results.** `DirectoryEntry.imageFile` is a path inside the published web-listing folder (Listing.hs:77,120,133-146) — the bytes are written to disk by `generateListing` (:159) and never serialized — so it is meaningless to the app. Getting avatars means either (i) image bytes in the response, which the size budget makes impractical (§1), or (ii) an HTTPS URL into the directory's web listing, which costs a direct web request from the user's IP to the directory host and breaks the messaging-only privacy model. **Recommendation: no avatars in v1**, initials placeholder, revisit with the streaming response.

**D3 — Tags vs the action rows on a phone.** The two platforms already differ. Android replaces the tag filters with the connect row in both bar positions (the branch keys on `appPlatform.isDesktop`, ChatListView.kt:1100-1106); iOS replaces them only in one-hand mode (ChatListView.swift:651-661) and shows tags and the row together in top-bar mode (:696-704). The directory row appears for far more inputs than a name does, so under the current rules the tags would vanish as soon as an Android user types three characters. Do rows and tags coexist on a phone, or do the tags stay hidden while searching? Either answer changes current behaviour on one platform.

**D4 — Shape of the warning.** Three idioms exist:
  - **link previews** — the only precedent for an action that sends data to a third party: `privacyLinkPreviewsShowAlert` gates an Enable/Disable button column (dismissible, no explicit Cancel) whose choice writes the persistent `privacyLinkPreviews` setting and clears the alert flag (ComposeView.kt:443-448, :1767-1790; ComposeView.swift:1913-1954), copy in the form "Sending a link preview may reveal your IP address to the website. You can change this in Privacy settings.";
  - **"Don't show again" as a button** — six Kotlin sites and their iOS twins (e.g. ComposeView.kt:751, SetDeliveryReceiptsView.kt:56), but all on *informational* alerts, none gating an action;
  - **confirm-once** — Send/Cancel where confirming suppresses (SendMsgView.kt:561-571, SendMessageView.swift:383-393); both instances are local, non-network features and it offers no option at all.
  **Recommendation: the link-previews shape** — Search / Don't search, writing a persistent `privacyDirectorySearch` preference plus the `directorySearchAlertShown` hint flag, with a matching toggle in Privacy settings (beside `send_link_previews`, PrivacySettings.kt:68-70 / PrivacySettings.swift:78-81). It is the closest precedent for what this action does, and it leaves the user a way back. If it is chosen, say also what happens when the preference is **off**: the proposal is that the "Search in Directory" row is hidden and the Search key runs the local filter only, with the toggle as the only way back on.

### Interpretations I made (say if any is wrong)

- **"Search both by name and in the directory"** = the Search key keeps the local filter, additionally resolves the typed name **online** when it is a name candidate not already known locally, and runs the directory search. The "Search in Directory" row does the directory part only. One warning covers both triggers.
- **Ordering**: local chats, then a "Directory" section header, then directory results, then "Show more". Name results sit with the local chats (§5).
- **Scope**: both surfaces, as asked.
- **A result row's SimpleX name is de-emphasised, and the tap connects by name when there is one.** Everywhere else a name carries its verification state — checkmark when verified, red cross when not, a "Verify name" action when unknown (SimplexNameView.kt:76-97; ChatInfoView.swift:1442-1470) — and bare text is used only where re-verification is impossible. A result row cannot use that component (§5), and a hostile directory could attach any name to any link. Connecting by *link* does not check it: a `CTLink` target yields no plan name (Commands.hs:4445-4446) and so no `planDomain` (:4482), leaving the domain check at :4491-4498 dormant. So the row renders the name in secondary colour with no badge and no verify affordance, and the tap targets `simplexName` when present (§5) so that check does fire and a mismatch fails the connect. The cost is not only the extra resolution per tap: it also decides joinability when name resolution is unavailable, so §5 partitions the failure classes. If that is more machinery than the check is worth, drop `simplexName` from the row entirely and tap the link — but drop it, rather than showing a name the tap does not verify.

---

## 1. Directory: answer a search over RPC

**Schema** — a new `Directory/Rpc.hs`, JSON-derived, re-declared by hand in Kotlin and Swift (there is no codegen for bot payloads).

```haskell
data DirectorySearchRequest = DirectorySearchRequest
  { searchText :: Maybe Text      -- Nothing = browse
  , sortBy :: Maybe SearchSort    -- browse only (searchText = Nothing): SSPopular | SSRecent
  , cursor :: Maybe SearchCursor  -- opaque, echoed from a previous response
  , limit :: Maybe Int            -- clamped server-side
  }

data DirectorySearchEntry = DirectorySearchEntry  -- projection of DirectoryEntry, same field names
  { entryType, displayName, simplexName, groupLink, activeAt, createdAt
  , shortDescr :: Maybe Text }  -- plain text, NOT DirectoryEntry's MarkdownList

data DirectorySearchResponse = DirectorySearchResponse
  { entries :: [DirectorySearchEntry]

  , cursor :: Maybe SearchCursor  -- Nothing = no more
  }
```

Send a **projection**, not `DirectoryEntry` verbatim: `welcomeMessage` carries the group's full `description` plus an appended join-link line (Listing.hs:105-110, :119), bounded only by the whole-event budget (`maxEncodedInfoLength = 14694`, `maxCompressedInfoLength = 10968`, Protocol.hs:937-940), so a couple of verbose groups would fill the response on their own. `imageFile` is dropped (D2). `shortDescr` becomes plain `Text` rather than `DirectoryEntry`'s `MarkdownList` (Listing.hs:75, :118): `Format`'s JSON shape is build-dependent — `sumTypeJSON` is the single-field `_owsf` form only under `darwin_HOST_OS && swiftJSON` and the tagged-object form otherwise (simplexmq `Parsers.hs:110-115`, applied at Markdown.hs:530) — so a Linux-built directory emits the tagged `{"type":…}` form, while iOS decodes `Format` with Swift's synthesized single-field enum decoding (ChatTypes.swift:5360, no `init(from:)`), which requires `{"caseName":…}`. Nothing in between fixes it: the core keeps the payload as an opaque `J.Object` (Commands.hs:1455, Controller.hs:834) and never decodes `Format` at all. Plain text also matches §5's untrusted-text rule. Keeping the remaining field names identical keeps web and app recognisably one schema.

Wrap both directions in a versioned envelope (`{"v":1,"method":…,"params":…}` / `{"v":1,"result":…}` or `{"v":1,"error":…}`) so the response can grow — streaming will need it.

`sortBy` is meaningful only for browse: `searchListedGroups` has one ordering per `SearchType` and `STSearch` is always member-count ordered (Store.hs:388), so a text search has no sort choice — consistent with §4-§6 exposing no sort control.

**Page size is a budget, not a constant.** The whole response is one message, so the handler fills `entries` until the encoded response approaches the limit and returns a cursor for the rest. `entries` is a list even while the directory fills it with one entry, so today's one-at-a-time behaviour and tomorrow's streaming change the transport, not the client model — and the client must render a single-entry response correctly.

**Handler.** Add `DEServiceRequest AgentInvId (Maybe C.PublicKeyEd25519) J.Object` to `DirectoryEvent` (Events.hs:48) and a `CEvtServiceRequest` case to `crDirectoryEvent_` (Events.hs:81). In `directoryServiceEvent` (Service.hs:321), decode the envelope, run the search, build entries, and reply with `APISendServiceResponse`. Bad JSON, unknown method or unsupported version return the error envelope, never a chat message. Reply promptly: the client's timeout (§3) covers the whole round trip and a late reply is discarded.

**Building entries needs the group link.** `groupDirectoryEntry` (Listing.hs:100) takes `Maybe GroupLink` and returns `Nothing` when a non-public group has none (:126-130); `searchListedGroups` returns no link at all (Store.hs:343). Either extend `searchListedGroups` to return `Maybe GroupLink` per row the way `getAllListedGroups_` does (Store.hs:335-341), or restrict the RPC to `publicGroup` registrations — and if the latter, filter the count query identically so the in-chat "N more" and the page agree, accepting that in-app search then returns a strict subset of what the bot returns in chat. Entries that still come out `Nothing` are skipped and must be excluded from the page accounting.

**Statelessness.** In-chat search keeps per-contact state (`searchRequests :: TMap ContactId SearchRequest`, Service.hs:101, expiring after 5 minutes at :1131). RPC has no contact, which is why the cursor travels in the request.

**The cursor is broken today and must be fixed for this feature.** `searchListedGroups` orders by `summary_current_members_count DESC, r.group_reg_id ASC` (Store.hs:359 all, :388 search) or `r.created_at DESC, r.group_reg_id ASC` (:371 recent), but pages with `AND r.group_id > ?` (:354, :366, :378) — a different column from the tiebreaker and unrelated to the sort key, so "next page" both skips and repeats rows; the counts at :355, :367, :379 use the same wrong predicate. It is invisible today only because the fixture in `testSearchGroups` (DirectoryTests.hs:505) has ids aligned with member counts. Behind a "Show more" button it is a visible defect.

Make `SearchCursor` the real sort key, tie-breaking on `r.group_id` rather than `r.group_reg_id`: `groupRegFields` does not select `group_reg_id` (Store.hs:450) so the caller cannot build a cursor from it, while `group_id` is unique (`idx_sx_directory_group_regs_group_id`, Directory/Store/SQLite/Migrations.hs:36) and already reaches the caller. The sort is **mixed-direction**, so a row-value comparison does not express "after the cursor" — use the OR form, which works on both backends:

```sql
-- popular / search   ORDER BY g.summary_current_members_count DESC, r.group_id ASC
AND (g.summary_current_members_count < ? OR (g.summary_current_members_count = ? AND r.group_id > ?))
-- recent             ORDER BY r.created_at DESC, r.group_id ASC
AND (r.created_at < ? OR (r.created_at = ? AND r.group_id > ?))
```

The count queries take the **same** corrected predicate as the page query, leaving `n` as "matches at or after the cursor". That is what the in-chat caller needs — `sendFoundListedGroups` computes `moreGroups = n - length gs` (Service.hs:1288-1291) and renders "Send /next for N more result(s)" (:1311), with no sent-count in `SearchRequest` to recompute it from — and on a cursor-less first request it already equals the whole-query count (Store.hs:351), so nothing in the RPC path needs a second count. **Separate commit, ahead of the RPC work**; it fixes `/next`'s skipping and repeating, not its count, so the `testSearchGroups` update is the fixture reordering only — member counts deliberately *not* monotone in group id — and the existing "for N more result(s)" assertions must still hold.

**Abuse surface.** Nothing rate-limits the directory today — the only cap is on captcha attempts by a joining member (`maxCaptchaAttempts`, Service.hs:775) — so the RPC path loses no existing protection, and what keeps results clean is admin approval before `GRSActive` (Service.hs:1316-1334), not blocked words, which only gate a joining member's display name (Service.hs:293-294). What the RPC path does add is an unauthenticated caller with no contact behind it.

Two limits are needed. First, **do the work off the event loop**: the directory processes every event (registrations, captchas, link checks, owner commands) on a single sequential consumer (Service.hs:263-269, and :175-181 for the CLI path), so answering search requests inline lets unauthenticated callers starve bot moderation — fork the handler as `sendFoundGroups` already does (Service.hs:1301-1302) and bound the concurrency. Second, **cap and clamp**: clamp `limit` to `searchResults`, and cap the global request rate with a bounded worker pool. Note what that cannot be: a service request carries no caller identity — the responder sees only `invId`, an optional attacker-chosen `sigKey_` and the payload (Subscriber.hs:1365-1368) — so the cap is necessarily global, and its failure mode under load is the feature being unavailable to everyone rather than one caller being throttled. Sizing also matters because each request accrues agent state on the directory: `APISendServiceResponse` returns an `AgentConnId` (Commands.hs:1472-1475) and its `SSENT` is handled with no chat entity behind it (Subscriber.hs:131-132).

## 2. Core: let a bot process service requests, on a DR address

**The flag.** `startChatController mainApp enableSndFiles serviceRequests` (Commands.hs:219) already threads it; only the CLI entry point hardcodes it off, and there are exactly two callers (Core.hs:93, Commands.hs:555). Add `serviceRequests :: Bool` to `CoreChatOpts` with a `--service-requests` switch in `coreChatOptsP`, beside `chatRelay` (src/Simplex/Chat/Options.hs:71 and :244), and read it at Core.hs:93. Give it a value at the two other full-record construction sites — `mobileChatOpts` (Mobile.hs:251) and `testCoreOpts` (tests/ChatClient.hs:136), both `False`; `-Wmissing-fields` is not fatal here (simplex-chat.cabal:332), so a miss is a runtime error on every call rather than a build failure. The directory inherits the flag through `coreChatOptsP` (Directory/Options.hs:52) — `mkChatOpts` needs no change. A zero-core-change alternative exists — `postStartHook` receives the `ChatController` (Controller.hs:235) and runs after `startChatController` has written the flag (Core.hs:93-95), so `directoryPostStartHook` (Service.hs:218) could set the TVar itself — but prefer the opts flag: a bot hook silently changing chat-controller behaviour is the kind of hidden condition that costs a reader later.

**The DR address: settle this before anything else.** There is a candidate in-place path — `APIRotateAddressRatchetKeys` (Commands.hs:2477-2478, `/_rotate_address_keys <userId>` :5747) and `APIAddMyAddressShortLink … pq_ratchet=on` (:2473-2476, `/_short_link_address <uid> pq_ratchet=on` :5746) both call `setMyAddressData … (Just IKUsePQ)` on the **existing** address connection, keeping the same full link (Commands.hs:4025-4039; test `testRotateAddressRatchetKeys`, Profiles.hs:649). But the signatures argue against it: `useDR` is a parameter distinct from `pqInitKeys`, passed only to `prepareConnectionLink` at creation (Commands.hs:2442), and `setConnShortLink` on the rotation path has no `useDR` argument at all (:4035). So treat this as **unresolved**, not as the expected outcome.

Answer it empirically before planning around either branch, with a new test rather than an edit to `testServiceRequestNonDRAddress` (Direct.hs:1966-1972) — that one has no responder enabled, so it can only observe failure: create the address with `/ad`, rotate, start the responder with `service_requests=on`, and expect the `{"pong":2}` exchange of `testServiceRequestResponse` (:1903-1926) instead of `ASENotDRAddress`. If it passes, this is one command on the live directory. If it does not, the address must be recreated with `pq_ratchet=on` — either as a second, RPC-only address, or by replacing the published one, which breaks the What's New links in D1 at very uneven cost: on iOS the URI sits inside the English description that doubles as the localization key and fans out to 35 files under `apps/ios`, against a single Kotlin source file. Note also that the bot's own start-up address path passes `pqRatchet_ = Nothing`, so a freshly created bot address is non-DR unless that is changed too.

The requester side needs no flag — `processServiceRequests` gates only inbound `SREQ` (Subscriber.hs:1366).

## 3. Apps: the API binding

**Kotlin** — `CC.APISendServiceRequest(userId, target, timeoutSec, request: JsonObject)` with `cmdString` matching the parser (Commands.hs:5520: `/_service_request <userId> <target>[ timeout=<s>][ sign_key=<k>] <json>`), a `CR.ServiceResponse` case, and `suspend fun apiSearchDirectory(rh, text, cursor)` wrapping the envelope. **iOS** — the same as a `ChatCommand` case with `cmdString`, plus a response case in the appropriate split `ChatResponse` enum.

**The call blocks** until the reply or the timeout. Pass an explicit `requestTimeout` — propose **10 s** — so a dead directory fails visibly instead of hanging on the agent default. On Kotlin use `withLongRunningApi` (Utils.kt:43), not the single-threaded `withBGApi` (:38). Guard stale responses with a request generation counter, not a text comparison: bump it on every text change **and** on profile switch, remote-host switch and filter reset, and drop any response whose generation is stale. A text-only check lets a request issued before a profile switch return afterwards, still matching the unchanged text, and repopulate the list that switch had just cleared.

**Remote host and profiles.** No allowlist change is needed. When a desktop drives a phone the *phone* performs the request, so the directory sees the phone's network; results must clear on remote-host switch. The request carries no profile (Commands.hs:1454 sends only the JSON body), so incognito needs no special handling; results must also clear on user switch.

## 4. Search UI

**The row — the slot needs re-gating, this is not a drop-in.** Today the slot is an either/or keyed entirely on `connectNameCandidate`: `when { candidate == null -> TagsView; !isDesktop -> ConnectByNameRow; else -> desktopView(candidate) }` (ChatListView.kt:1100-1106), while iOS has two blocks: an either/or above the field in one-hand mode (ChatListView.swift:651-661) and an additive block below it in top-bar mode (:696-704, no `else`, so the tags drawn at :659-661 stay). Only the one-hand block becomes three-state; the top-bar block just gains the directory row. The directory row's condition is different — trimmed text non-empty, not a SimpleX link (`searchShowingSimplexLink` false), at least 3 characters — far looser than a name candidate (`MIN_NAME_LENGTH = 5` plus ASCII name grammar, ChatListView.kt:1043-1065): "news", "cat pics", "новости" are all valid searches and none is a name. So the slot becomes a three-state column (tags / rows / both), which means changing `TagsOrConnectByName` and both its call sites (ChatListView.kt:976-980, :987-991, including the `desktopView` lambda shape) and making the iOS `if/else` three-state. That restructuring carries the product question in D3.

**The keyboard.** Kotlin: add an optional `onSearch: (() -> Unit)? = null` to `SearchTextField` (SearchTextField.kt:32) and set `keyboardActions = if (onSearch != null) KeyboardActions(onSearch = { onSearch() }) else KeyboardActions.Default`, so unrelated call sites (DefaultTopAppBar.kt:84, AddGroupMembersView.kt:217, GroupChatInfoView.kt:1386) are bit-for-bit unchanged — the component supplies no `keyboardActions` today and their behaviour must stay the platform default. The New chat field goes through `ContactsSearchBar` (NewChatSheet.kt:476, field at :494), which is **also** the Deleted-chats screen's search bar (:715, :725), so the handler must be a parameter passed only by the New-chat call sites (:312, :403) — not added inside `ContactsSearchBar`. iOS has the same hazard: `ContactsListSearchBar` (NewChatMenuButton.swift:333) serves both New chat (:87) and deleted contacts (:469). The chat-list handler hides the keyboard itself with `hideKeyboard(view)`, the idiom already at ChatListView.kt:800. iOS: `.submitLabel(.search)` and `.onSubmit` on the search `TextField` (ChatListView.swift:665 and the New chat equivalent), both iOS 15+ and matching the deployment target. Desktop shares `SearchTextField` and has no soft keyboard, so confirm that Enter reaches the same handler there rather than assuming it.

**What runs**: one function with two entry points — the keyboard key does local filter + online name resolution + directory search; the "Search in Directory" row does the directory search only. The keyboard key takes the same non-empty, ≥3-character, not-a-link gate as the row: without it, Search on an empty field sends `searchText = Nothing`, which the directory answers with a browse of the whole listing. The name resolution MUST be silent: `apiConnectPlan` calls `apiConnectResponseAlert` whenever `inProgress.value` is true (SimpleXAPI.kt:1540) and pops "SimpleX name not found" for `NameErrorType.NOT_FOUND` (:1602-1605), so an ordinary word like "photography" would throw a modal alert — twice, since a bare name is tried as both `@name` and `#name`. Pass a false `inProgress` as the typing path already does ("background search: no spinner, no error alerts", ChatListView.kt:818); iOS calls `apiConnectResponseAlert` unconditionally (SimpleXAPI.swift:1046) and needs the same guard added.

## 5. Rendering results

Two new row types, neither a `Chat`. Both lists already take heterogeneous items, so the change is additive.

**Placement.** Declaration order is the same in both bar positions — search bar, local matches, then the Directory section — because the search bar is declared first on both platforms regardless of mode (Kotlin `stickyHeader` ChatListView.kt:952, before `itemsIndexed(chats…)` :1001; iOS search-bar row ChatListView.swift:399-412, before the chats `ForEach` :422/:431). One-hand mode flips only the physical direction (`reverseLayout = oneHandUI.value`, ChatListView.kt:949, default true at SimpleXAPI.kt:269; a list-level `.scaleEffect(x: 1, y: oneHandUI ? -1 : 1)` at ChatListView.swift:232, default true at :174), so directory results sit past the local matches either way. That is fine because the list is filtered during a search and therefore short — but check both modes against a term that matches many chats.

- **Kotlin** — header item, `items(results, key = …)` and the "Show more" item, after `itemsIndexed(chats, …)` (ChatListView.kt:1001) and **before** `ChatListFeatureCards` (:1007-1009), so the promo card stays at the end of the list; `ToggleChatListCard` (:996-1000) sits above the chats and is unaffected.
- **iOS** — the same sections relative to the chats `ForEach` (ChatListView.swift:422/431), each row carrying the per-row `.scaleEffect` flip the other rows use.

**Directory row.** Display name with its `#`/`@` prefix, the SimpleX name de-emphasised when present (see the interpretation above), `shortDescr`, member count from `DETGroup.summary`, placeholder avatar (D2). Do not reuse `SimplexNameView`: it takes a `verify` closure and auto-verifies under `DEFAULT_PRIVACY_VERIFY_SIMPLEX_NAMES` (ChatInfoView.swift:1411-1419), so a ten-row list could fire ten network verifications — and the name is the directory's claim (it emits one only for a domain it has verified, Listing.hs:116), not ours to badge. The directory lists both channels and groups, so the row picks its verb from `DETGroup.groupType` using the group/channel pair — `compose_view_join_channel` / `compose_view_join_group` (strings.xml:625-626), `"Join channel"` / `"Join group"` on iOS (ComposeView.swift:478) — defaulting to the group wording when `groupType` is absent. Not `connect_plan_join_name` / `connect_plan_connect_to_name`, which is a contact-vs-channel choice (ConnectPlan.kt:87).

**Name row.** When the Search key resolves the typed name online to a contact or channel that is not already a local chat, it renders as the same kind of connectable row, fed from the `apiConnectPlan` result (its `connLink` plus the short-link profile data) rather than from a `DirectoryEntry`: profile name, short description, contact-vs-channel verb, no member count. It sits with the local chats, above the Directory section. A name that resolves to nothing produces no row and — per §4 — no alert. Once the row exists it supersedes the "Connect to \<name\>" action row for that input, which is on screen for exactly the same text: hide the action row, since the result row carries the resolved profile and the action row only carries the guess.

**Links and keys.** `PublicLink` has **both** fields optional (Listing.hs:63-66): a public group yields short-only (:127-128), a group registered by group link may yield full-only (:130-132). Tapping accepts any of the three forms (ConnectPlan.kt:24-26), and the directory drops entries with no link. Preferring the name is what arms the domain check (see the interpretation above), but it cannot be an unconditional preference: a name target resolves **before** any link data is fetched, and `APIConnectPlan` does not degrade a resolution failure into a `CPError` plan (Commands.hs:2151-2152 — contrast `Connect` at :2378, which wraps it in `catchAllErrors`), so the failure reaches `apiConnectResponseAlert` (SimpleXAPI.kt:1540; unconditional on iOS, SimpleXAPI.swift:1046) and the entry cannot be joined at all — with its perfectly good link sitting unused in the row. Partition the rule by error class; a blanket retry-with-the-link would hand a hostile directory the attack back, since it could pair name X with link Y and pick an X that fails to resolve:

- **`NO_NAME_SERVERS`** — the user has no names-role server. Not attacker-inducible, and it makes *every* name uncheckable, so in that configuration render rows without `simplexName` and tap the link. This is reachable by ordinary configuration, not just outage: the role is user-toggleable per operator and per server, and a server with no operator defaults to `names = false` (SimpleXAPI.kt:4653), so a self-hosted-only user would otherwise find every named result unjoinable.
- **Claim failures** (`NameErrorType.NOT_FOUND`, `SDENoValidLink`, `SDEUnknownDomain`) — never fall back. Fail with a message that says the directory's name for this channel did not check out.
- **Resolver or network failures** (`NO_RESOLVER`, `RESOLVER`, timeout) — fail with a retry, not a fallback; the domain is attacker-chosen, so these are influenceable too. That link is also the row key, since the entry has no id — and because paging can repeat rows, "Show more" must de-duplicate by key when appending (duplicate keys are a hard error in a `LazyColumn`).

**Trust boundary.** Everything in a result comes from the directory, not from the channel owner. Render it as untrusted text — no markdown links, no HTML, clamped lengths — and never build the prepared chat from it. Connecting goes through `planAndConnect`, which resolves the link and takes the profile from the link owner, so a lying directory can mis-advertise a channel in the list but cannot forge the chat you land in.

**Empty state — a condition change, not just wording.** Both platforms draw the empty view as an overlay *centred over the list* whenever the local filter is empty: `if (chats.isEmpty() && chatModel.chats.value.isNotEmpty())` (ChatListView.kt:1016-1020) and `if cs.isEmpty && !chatModel.chats.isEmpty` (ChatListView.swift:466-470). That is exactly the normal successful directory search — you searched for a group you are not in — so "No chats found" would float on top of the results. Kotlin has a third such overlay for a user with no chats at all ("You have no chats", ChatListView.kt:419-421), which is the one that fires on a fresh install. Every one of these conditions must also require the new sections to be empty, and a searched-and-found-nothing variant is added on top.

**New chat has its own list and its own empty state**, and the same treatment applies there: results go after `itemsIndexed(filteredContactChats)` (NewChatSheet.kt:341, :430 — two lazy columns) / after the `ContactsList(…)` call in `NewChatSheet.viewBody` (NewChatMenuButton.swift:159-168) — **not** inside `ContactsList` itself (struct at :223), which `DeletedChats` also uses (:482), for the same reason the Search action is gated to the New-chat call sites in §4, and the empty states must also require the new sections to be empty — an in-place edit on Kotlin, where `NoFilteredContactsItem` is local to `NewChatSheetLayout` (NewChatSheet.kt:261-272, shown at :339/:428), but a signature change on iOS, where those states live in `ContactsList` (NewChatMenuButton.swift:223, conditions :262-266) which the Archived-contacts screen also uses (:482): add a defaulted parameter or lift the empty state to `NewChatSheet`. Note that this list filters **contacts only**, so a directory result there sits beside a much narrower local set than in the chat list.

Two gates affect reachability, differently per platform: on iOS the search bar renders only when the chat list is non-empty (ChatListView.swift:398), so a fresh install can only search from New chat; and on both platforms the has-chats-but-no-conversations onboarding state replaces the whole list, search bar included (ChatListView.swift:384 via `shouldShowOnboarding` :366-368; ChatListView.kt:413). If new-user discovery matters, both gates have to change.

## 6. Pagination

The view model holds `entries`, `cursor`, `loading`, `error` — no result count, since nothing in the UI renders one and `cursor = Nothing` already says when the list is complete. "Show more" is the last row of the directory section: it re-calls with the stored cursor, appends (de-duplicating by key), and disappears when the response has `cursor = Nothing`. On failure it becomes a retry row carrying the message. An explicit button rather than infinite scroll also preserves the "requests only on an explicit action" property the warning promises.

Changing the search text discards `entries` and `cursor`; so does switching profile or remote host. `activeChatTagFilter` already clears `searchText` (ChatListView.kt:1034-1036) and must clear results with it.

## 7. Warning and preference

The first online search — from either trigger — shows the alert chosen in D4. The copy must name **both** recipients, because the Search key makes two different network requests: the search text goes to the SimpleX Directory, and a name-shaped term (any ASCII word of 5+ characters, completed with the default top-level part, ChatListView.kt:1041-1065) is additionally resolved through a name server, proxied where possible and direct otherwise. It must also be clear that what gets sent is *whatever is in the box* — the same field is the local chat filter, so the user may have typed a contact's name to find a private chat and then pressed Search. Say only what is verified — no contact is created and no profile is sent — and do not claim anonymity until the responder-side question in §10 is answered.

If two recipients in one warning is too much, the alternative is to drop online name resolution from the Search key and leave it to the existing "connect to <name>" row, which already carries its own tap-to-confirm; that narrows the brief's "search by name" to the local filter. Flagging rather than deciding, because it changes what Search does.

- **Kotlin** — persistent `privacyDirectorySearch` beside `privacyLinkPreviews`, plus `directorySearchAlertShown` in `AppPreferences` (SimpleXAPI.kt ~190) added to the `hintPref` list (:273) so "Reset hints" restores it.
- **iOS** — the same pair: a persistent group default plus `DEFAULT_DIRECTORY_SEARCH_ALERT_SHOWN` in `appDefaults` and `hintDefaults` (SettingsView.swift:122, 145).
- Both: the Privacy settings toggle from D4.

## 8. Strings

Kotlin: new entries in `common/src/commonMain/resources/MR/base/strings.xml`, beside `connect_plan_connect_to_name` (:19) and `search_or_paste_simplex_link` (:519). iOS: inline `NSLocalizedString` at the use site, as `ConnectByNameRow` does (ChatListView.swift:815). Needed: the row label, section header, "Show more", the alert title/body/buttons, the empty state, and the error text.

## 9. Failure modes to handle

| Case | Surfaced as |
| --- | --- |
| Directory address not DR | `A_SERVICE ASENotDRAddress` — a deployment bug, not a user error: log, show a generic failure |
| Directory offline or silent | `A_SERVICE ASETimeout` after ~10 s → retry row |
| Network down | existing offline handling; must not spin |
| Response malformed or oversized | parse failure → error row, log the raw size |
| Response arrives after the text changed | dropped by the cancellation token |
| Name resolves to nothing | no row, no alert (§4) |
| Directory over its global cap | indistinguishable from a timeout to the client (§1) — same retry row |
| Tap: user has no names-role server | rows render without the name; the tap uses the link (§5) |
| Tap: the directory's name does not resolve, or mismatches the link | join refused with a mismatch message — never falls back to the link (§5) |
| No directory results | dedicated empty state |
| Result is a channel already joined | `planAndConnect` opens it (`GroupLinkPlan.Known`, ConnectPlan.kt:322-333) — no pre-check needed |

## 10. Order of work

1. Directory cursor fix (`searchListedGroups`, `SearchRequest`) + `testSearchGroups` fixture update — standalone; it stops `/next` skipping and repeating rows, and leaves its counts as they are.
2. Settle the DR-address question with the new test in §2 — a negative result decides whether the directory gets a second RPC-only address or a replaced published one. Then `--service-requests` through `CoreChatOpts` → Core.hs:93 and the directory enabling it.
3. Confirm against the pinned agent what the responder observes for a service request — reply-queue server, signer key, whether paginated requests are correlatable, and whether per-request agent state is released — before the warning copy is finalised.
4. RPC schema, `Directory/Rpc.hs`, directory handler off the event loop — CLI-testable with `/_service_request`.
5. Kotlin: API binding, view model, rows, button, keyboard, alert, pagination — chat list, then New chat.
6. iOS: the same.
7. Tests: directory-side search RPC (request → response, cursor, clamping, bad envelope) in `tests/Bots/DirectoryTests.hs`, following the shape of `tests/ChatTests/Direct.hs:1903-1975`. Three harness changes come first: the bot's address is created with a plain `/ad` (DirectoryTests.hs:1771) and must become `/ad pq_ratchet=on`; `mkDirectoryOpts` (:112-147) must set the new `serviceRequests` flag in its `coreOptions` — the bot starts through `runDirectory` → Core.hs:93, so there is no `/_start … service_requests=on` to override it; and the harness hands tests only the full URI (`getContactLink`, :1772, which discards the short link, Utils.hs:593-596), so it must expose the short link too, since that is the target form D1(a) ships.

## Not in scope

Streaming responses (separate; the envelope is versioned so they can land without breaking clients), directory browse and recommendations, self-hosted directory configuration beyond D1(c), avatars in results (D2).

## Uncertainties I could not close in this container

- The pinned agent (`cabal.project:24`, `ee4dd0d8`) is not available locally — `simplexmq-2` is 7.0.0.4 and `dist-newstyle` holds 7.0.0.5, neither containing the service-request code. The error constructors, the `timeout=` syntax and the short-link target form are nevertheless pinned by `tests/ChatTests/Direct.hs:1903-1975`. What remains unread is the **exact response size limit** (which sets the page budget in §1; the in-repo `maxEncodedInfoLength = 14694` / `maxCompressedInfoLength = 10968` at Protocol.hs:937-940 is the right order of magnitude, not the number) and the **responder-side state lifecycle** in item 3 above.
- Nothing here has been compiled or run.
