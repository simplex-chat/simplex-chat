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

Paging is `/next` with the state moved. In chat the directory keys the saved search on the contact (`TM.lookup (contactId' ct) searchRequests`, Service.hs:1128, written :1299, 5-minute expiry :1131). Over RPC it gets only a per-request `invId` (Subscriber.hs:1368) — nothing identifying the requester — so there is no key to look a `next` up against, and the cursor comes back in the request instead. `DirectoryCursor` is opaque: it is the `lastGroup` the directory already stores, held by the client, so the paging fix below needs no app change.

Entry differences from `DirectoryEntry`: `welcomeMessage` dropped (it carries the whole group description, Listing.hs:105-119); `shortDescr` sent as stored `Text` (Types.hs:882) rather than `MarkdownList`, which would ship a Linux-shaped `Format` iOS cannot decode — apps parse markdown locally via `chat_parse_markdown` (Core.kt:31) if the row needs it; `imageFile` (a web-listing path) replaced by `image`, the profile picture the bot already sends as `MCImage` (Service.hs:1310), bounded to 12,500 bytes (GroupProfileView.kt:95). Against the ~14.7KB envelope that is about one entry per response today — the handler fills `entries` until the envelope is full and returns a cursor for the rest, so streaming later changes the transport, not the client model.

**Handler.** Add `DEServiceRequest` to `DirectoryEvent` (Events.hs:48) and a `CEvtServiceRequest` case to `crDirectoryEvent_` (:81); in `directoryServiceEvent` (Service.hs:321) decode, search, reply with `APISendServiceResponse`. Bad JSON or unknown method returns the error envelope, never a chat message. Entries need the group link, which `searchListedGroups` does not return (Store.hs:343) and without which `groupDirectoryEntry` yields `Nothing` (Listing.hs:126-130) — extend it as `getAllListedGroups_` does (Store.hs:341).

Run the handler off the event loop (`forkIO`, as `sendFoundGroups` does at Service.hs:1301) with bounded concurrency: the directory processes registrations, captchas and owner commands on one sequential consumer (Service.hs:263-269). Cap the request rate globally — a service request carries no caller identity to key a per-caller limit on — and size the cap knowing each request also leaves agent state on the responder (`APISendServiceResponse` returns an `AgentConnId`, Commands.hs:1472).

**Cursor fix — separate commit, first.** The search orders by `members_count DESC, group_reg_id ASC` (Store.hs:388) but pages with `AND r.group_id > ?` (:378), a column unrelated to the sort key, so pages skip and repeat rows; `testSearchGroups` (DirectoryTests.hs:505) misses it because its fixture aligns counts with ids. "Continue after the last row" here means "fewer members than it had", so the cursor has to carry the member count; the id only breaks ties between equal counts. Tie-break on `r.group_id` (unique, Migrations.hs:36; `group_reg_id` is not in `groupRegFields`, Store.hs:450), and use the OR form, since the mixed-direction sort rules out a row-value comparison:

```sql
AND (g.summary_current_members_count < ? OR (g.summary_current_members_count = ? AND r.group_id > ?))
```

The count queries take the same predicate, so `n` stays the remainder in-chat `moreGroups` needs (Service.hs:1289). This fixes `/next`'s skipping, not its counts — `testSearchGroups` needs only a fixture whose counts are not monotone in id.

## 2. Core: bot support and the DR address

`startChatController` already threads a `serviceRequests` flag (Commands.hs:219); only `Core.hs:93` hardcodes it off. Add `serviceRequests :: Bool` to `CoreChatOpts` with a `--service-requests` switch in `coreChatOptsP` (Options.hs:71, :244), read it at Core.hs:93, and give it `False` at the two other full-record sites — `mobileChatOpts` (Mobile.hs:251) and `testCoreOpts` (ChatClient.hs:136), since `-Wmissing-fields` is not fatal. The directory inherits it via `coreChatOptsP` (Directory/Options.hs:52). The requester needs no flag.

The address must be DR or requests fail with `ASENotDRAddress`; `APICreateMyAddress` enables DR only when `pqRatchet` is `Just` (Commands.hs:2438-2442) and the bot's start-up path passes `Nothing`. Whether an existing address can be upgraded in place is unsettled — `APIRotateAddressRatchetKeys` (:2477) acts on the existing connection, but `useDR` is passed only to `prepareConnectionLink` at creation (:2442) with no counterpart in `setConnShortLink` (:4035). Settle it with a new test: `/ad`, rotate, start a responder with `service_requests=on`, expect the exchange of `testServiceRequestResponse` (Direct.hs:1903) instead of `ASENotDRAddress`. Do not extend `testServiceRequestNonDRAddress` (:1966) — it has no responder and can only observe failure. If rotation does not work, recreate the address with `pq_ratchet=on`; replacing the published one also invalidates the What's New links (WhatsNewView.swift:293,355 — inlined in the localization key, so 35 files under `apps/ios`; WhatsNewView.kt:512,575).

## 3. Apps: API binding

Kotlin `CC.APISendServiceRequest(userId, target, timeoutSec, request: JsonObject)` with `cmdString` matching the parser (Commands.hs:5520), a `CR.ServiceResponse` case, and `apiSearchDirectory(rh, text, cursor)` wrapping the envelope. iOS: the same as a `ChatCommand` case plus a response case in the appropriate split `ChatResponse` enum.

The app ships the directory's **short link** as a constant; it cannot cache a resolved target, since `APISendServiceRequest` resolves internally and returns only the response.

The call blocks until reply or timeout — pass `requestTimeout` of **10 s**, and use `withLongRunningApi` (Utils.kt:43), not the single-threaded `withBGApi` (:38). Show progress through the existing `ConnectProgressManager` (ChatModel.kt:55-78, ChatModel.swift:303-329): `startConnectProgress(text, onCancel)` when the request goes out, `stopConnectProgress()` when it returns. It already withholds the spinner for 1 s, and the search bar already renders it (ChatListView.kt:522, ChatListView.swift:670); `onCancel` gives the user a way out of the wait. Guard stale responses with a generation counter bumped on text change, profile switch, remote-host switch and filter reset; a text comparison alone lets a pre-switch request return and repopulate a cleared list. No remote-host allowlist change is needed (`allowRemoteCommand` is a deny-list, Controller.hs:735). The request carries no profile, so incognito needs nothing.

## 4. Search UI

**Rows.** `SearchInDirectoryRow` goes beside `ConnectByNameRow` in the same slot on all four surfaces (ChatListView.kt:1109 via `TagsOrConnectByName` :1095; ChatListView.swift:803 via the `oneHandUI` branches :650-704; NewChatSheet.kt:322,413; NewChatMenuButton.swift:96-104). On a phone:

| search text | slot |
| --- | --- |
| empty | tags |
| non-empty, not a name candidate | Search in Directory |
| name candidate (5+ chars, ChatListView.kt:1043) | Search in Directory + Connect to \<name\> |

The rows replace the tags exactly as `ConnectByNameRow` does today, so the tag rule is unchanged — only the range of text that produces a row widens. Desktop shows tags alongside, also as today. The slot still needs widening from `when { candidate == null -> TagsView; else -> ConnectByNameRow }` (ChatListView.kt:1100-1106; iOS one-hand :651-661, top-bar already additive :696-704), since it must now emit two rows. The directory row is hidden for a SimpleX link, which connects on its own.

**Keyboard.** Kotlin already sets `ImeAction.Search` (SearchTextField.kt:105) with no `keyboardActions`; add `onSearch: (() -> Unit)? = null` feeding `KeyboardActions`, defaulting to `KeyboardActions.Default` so DefaultTopAppBar.kt:84, AddGroupMembersView.kt:217 and GroupChatInfoView.kt:1386 are untouched. Pass the handler only from the New-chat call sites (NewChatSheet.kt:312, :403), since `ContactsSearchBar` is shared with Deleted chats (:715, :725); iOS has the same hazard with `ContactsListSearchBar` (NewChatMenuButton.swift:333, used at :87 and :469). iOS also needs `.submitLabel(.search)` and `.onSubmit` (ChatListView.swift:665 and the New chat equivalent). Desktop shares `SearchTextField` with no soft keyboard — confirm Enter reaches the handler.

**Actions.** The keyboard key runs the local filter, resolves the typed name online when it is ≥5 characters, and runs the directory search; the row runs the directory search only. Name resolution must be silent — pass a false `inProgress` as the typing path does (ChatListView.kt:818), or `apiConnectPlan` pops "SimpleX name not found" (SimpleXAPI.kt:1540); iOS calls the alert unconditionally (SimpleXAPI.swift:1046) and needs the same guard.

## 5. Results

Three sections: local chats (unheadered, as today), then **Names**, then **Directory**. Each new section, header included, renders only when it has rows.

**Name rows** come from the `apiConnectPlan` result for the resolved name and render like a chat list row — avatar, profile name, description — with the searched name appended as `(@name)` / `(#name)`, since the profile name may differ. At most two: a bare term resolves as both `@name` and `#name`, a prefixed one only as itself (ChatListView.kt:819). A name row replaces the "Connect to \<name\>" action row for that text.

**Directory rows** render the same way from the response: `image`, `displayName` with its `#`/`@` prefix, `simplexName` as plain text, `shortDescr`, member count from `DETGroup.summary`. The verb comes from `DETGroup.groupType` — `compose_view_join_channel` / `compose_view_join_group` (strings.xml:625-626), defaulting to group when absent. Do not reuse `SimplexNameView`: it auto-verifies under `DEFAULT_PRIVACY_VERIFY_SIMPLEX_NAMES` (ChatInfoView.swift:1411), so each row would fire a network call. The name shown is the directory's claim; connecting uses the link, as the in-chat flow does today.

Tap → `planAndConnect(rhId, shortLink ?: fullLink)`; both `PublicLink` fields are optional (Listing.hs:63-66) and entries with neither are dropped. The link is also the row key, and "Show more" de-duplicates by it. Already-joined is handled inside `planAndConnect` (ConnectPlan.kt:322-333), so results need no pre-check. Render directory text as untrusted: no markdown links, no HTML, clamped lengths.

**Placement.** Declaration order is search bar, local matches, Names, Directory in both bar positions — the search bar is declared first regardless of mode (ChatListView.kt:952 before :1001; ChatListView.swift:399-412 before :422/:431), and one-hand mode flips only the physical direction. On Kotlin the new items go before `ChatListFeatureCards` (:1007); on iOS each row carries the same per-row `.scaleEffect` flip. On New chat they go after `itemsIndexed(filteredContactChats)` (NewChatSheet.kt:341, :430) and after the `ContactsList(…)` call (NewChatMenuButton.swift:159-168) — not inside `ContactsList` (:223), which Deleted chats also uses (:482).

**Empty states** are centred overlays gated on the local filter being empty — ChatListView.kt:1016, ChatListView.swift:466, and "You have no chats" at ChatListView.kt:419. They now depend on whether an online search has run:

- **not yet run** (typing) — unchanged, including "You have no chats" when there are no local chats;
- **run, anything found** in either new section — no overlay, results only;
- **run, nothing found anywhere** — "no chats found".

New chat has its own: `NoFilteredContactsItem`, local to `NewChatSheetLayout` (NewChatSheet.kt:261-272), and the iOS states inside the shared `ContactsList` (:262-266), needing a defaulted parameter or lifting to `NewChatSheet`.

**The search bar must always be present**, since it is now a discovery instrument. Two gates hide it today and both have to go: iOS renders it only when the chat list is non-empty (ChatListView.swift:397-398), and on both platforms the has-chats-but-no-conversations onboarding state replaces the whole list — `ConnectOnboardingView` (ChatListView.swift:383-386) and `AndroidOnboardingCards` (ChatListView.kt:412-413). Keep the onboarding content, but render it below a live search bar rather than instead of it.

## 6. Pagination

Directory only — the Names section is bounded at two rows. The view model holds `entries`, `cursor`, `loading`, `error`; no count, since nothing renders one. "Show more" is the last row of the Directory section: it re-calls with the stored cursor, appends de-duplicated by key, and disappears when the cursor is `Nothing`; on failure it becomes a retry row. Changing the search text discards both sections and the cursor, as do profile and remote-host switches — `activeChatTagFilter` already clears `searchText` (ChatListView.kt:1034) and must clear results with it.

## 7. Warning

The first online search from either trigger shows a three-action alert — **Cancel** / **Search** / **Search and don't show again** — the third writing `directorySearchAlertShown`, added to `hintPref` (SimpleXAPI.kt:273) and `hintDefaults` (SettingsView.swift:145) so "Reset hints" restores it. Kotlin uses `showAlertDialogButtonsColumn`; iOS an equivalent three-button alert.

The copy names both recipients, since the Search key makes two requests: the search text to the directory, and a name-shaped term additionally to a name server. It should be clear that whatever is in the box is sent — the same field is the local chat filter. Claim no more: no contact is created and no profile is sent.

## 8. Strings

Kotlin: `common/src/commonMain/resources/MR/base/strings.xml`, beside `connect_plan_connect_to_name` (:19). iOS: inline `NSLocalizedString` at the use site (ChatListView.swift:815). Needed: the row label, two section headers, "Show more", the alert title/body/three buttons, the empty state, the error text.

## 9. Failure modes

| Case | Surfaced as |
| --- | --- |
| Address not DR | `ASENotDRAddress` — a deployment bug: log, generic failure |
| Directory silent or over its cap | `ASETimeout` after ~10 s → retry row |
| Network down | fails within the 10 s budget → retry row; no indefinite wait |
| Response malformed or oversized | parse failure → error row, log the size |
| Response late, or after a profile/host switch | dropped by the generation counter |
| Name resolves to nothing | no row, no alert |
| No directory results | section omitted; empty state only when every section is empty |
| Result already joined | `planAndConnect` opens it (ConnectPlan.kt:322-333) |

## 10. Order of work

1. Cursor fix + `testSearchGroups` fixture — standalone.
2. Settle the DR-address question (§2), then `--service-requests` and the directory enabling it.
3. RPC schema, `Directory/Rpc.hs`, handler off the event loop — CLI-testable with `/_service_request`.
4. iOS: API binding, view model, rows, button, keyboard, alert, pagination — chat list, then New chat.
5. Kotlin: port it verbatim — same function and variable names, same structure and logic, diverging only where the platform forces it.
6. Tests in `tests/Bots/DirectoryTests.hs`, following `Direct.hs:1903-1975`. Three harness changes first: `/ad` → `/ad pq_ratchet=on` (:1771); `mkDirectoryOpts` (:112-147) must set `serviceRequests` in `coreOptions`, as the bot starts through `runDirectory` → Core.hs:93; and the harness must expose the short link, which `getContactLink` (:1772) discards.

## Verify against the pinned agent first

`cabal.project:24` pins simplexmq `ee4dd0d8`, absent from this checkout, so two things are unread: the exact **service-response size limit** (which sets the per-response entry budget) and whether per-request **agent state is released** on the responder. Error constructors, `timeout=` syntax and the short-link target form are pinned by `tests/ChatTests/Direct.hs:1903-1975`. Streaming, directory browse and self-hosted directory configuration are out of scope.
