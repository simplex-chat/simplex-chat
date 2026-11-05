# SimpleX Directory search in app search

The directory is searchable from the app's search fields, in the chat list and New chat, without chatting with the bot. Transport is the service RPC: one request, one response, over the directory's contact address.

```
search field ──"Search in Directory" / keyboard Search──▶ APISendServiceRequest(directory short link, JSON)
   ▲                                                          │
result rows ◀── CRServiceResponse ◀── APISendServiceResponse ◀─┴─ directory: CEvtServiceRequest → searchListedGroups
   │
   └─tap─▶ planAndConnect(link)   — loads the link's profile, prepares the chat, opens it
```

Results are not chats and are never persisted; they exist only as long as the search text and the active profile do.

## 1. Directory: search over RPC

`Directory/Rpc.hs`, JSON-derived, declared by hand in Kotlin and Swift.

```haskell
data DirectoryRequest = DRSearch
  { searchText :: Text,                 -- at most 100 characters
    searchCursor :: Maybe SearchCursor  -- opaque to the app; Nothing = first page
  }

data DirectorySearchEntry = DirectorySearchEntry
  { entryType :: DirectoryEntryType,
    displayName :: Text,
    simplexName :: Maybe Text,
    groupLink :: PublicLink,
    shortDescr :: Maybe Text,
    image :: Maybe ImageData,
    activeAt :: Maybe UTCTime,
    createdAt :: Maybe UTCTime
  }

data DirectoryResponse
  = DRSearchResults
      { entries :: [DirectorySearchEntry],
        searchCursor :: Maybe SearchCursor,  -- Nothing = no more
        moreCount :: Int
      }
  | DRError {errorMessage :: Text}
```

On the wire: `{"type": "search", "searchText": …, "searchCursor": …}`, answered by `{"type": "searchResults", "entries": […], "searchCursor": …, "moreCount": N}` or `{"type": "error", "errorMessage": …}`.

This is the bot's text search (`STSearch`), a case-insensitive substring match over registered group and channel **profiles**: display name, full name, short description, description, and the SimpleX name when the domain is verified. `%` and `_` in the text match literally. No conversation content is involved. No sort parameter: results are sorted by member count, and the directory's other modes are the separate `/all` and `/new` commands. No page-size parameter: `searchListedGroups` reads at most `searchResults` rows (default 10), as the in-chat path does, and the size is not part of the response.

Over RPC the directory gets only a per-request `invId`, with nothing identifying the requester, so there is no saved search to continue and the cursor comes back in the request. `SearchCursor` is opaque to the app: a JSON value it stores and echoes back verbatim and never inspects. It is typed as an opaque blob in the bindings (`JsonObject` / `JSONValue`), not as an id. A cursor of another search mode gets the error response, not the first page.

An entry differs from the web listing's `DirectoryEntry`:

- no `welcomeMessage`, which contains the whole group description;
- `shortDescr` as plain text rather than `MarkdownList`, whose `Format` differs between platforms; it is cut to 160 characters, the limit clients apply when they set one;
- `image` in place of `imageFile` (a web-listing path): the group's profile picture, sent only when it is at most 12,500 characters of data URI (`maxProfileImageSize`) and a JPEG or PNG that `safeImageData` keeps, so a response never contains a blank image; otherwise the entry is sent without it;
- `groupLink` without its full link when it has a short link, since the apps connect through the short link;
- `entryType`, `simplexName`, `activeAt` and `createdAt` from the same functions the web listing uses.

**Response budget and compression.** A service response travels as a confirmation, padded to `e2eEncConnInfoLength`: **11,106 bytes** with PQ on, which the reply connection inherits from the requester. A request also contains its reply queue and an optional signature, so both directions use one bound, `maxServiceBodyLength` = **10,737 bytes**.

Compression is in the **core**, not in the directory: `APISendServiceResponse` takes a `J.Object` and encodes it itself, and any service needs it. `compressServiceBody` (Protocol.hs) compresses a body over the bound with the same `'X'` marker as connection info and refuses one that is still over it, or over the decompressed limit of the receiver, when it is sent. `parseServiceBody` decompresses with the size bounded by `maxDecompressedMsgLength` (64 KB) and refuses JSON nested deeper than 32 levels. They apply symmetrically: request encode and response decode in `APISendServiceRequest`, response encode in `APISendServiceResponse`, and request decode on the `SREQ` path. A JSON payload never starts with `'X'`, so the marker is unambiguous, and payloads under the bound stay uncompressed. The directory handler and the apps see plain JSON.

**Entries per page are bounded by images.** `image` is base64 of already-compressed JPEG or PNG, so compression recovers roughly the base64 expansion and no more: a near-limit image is roughly 9,400 bytes against the 10,737 budget. One such entry fits in a response, two do not, whatever the page size. The envelope is the binding constraint, and the app shows whatever fits and pages the rest on request (§6).

**Filling the page.** `searchResultsPage` drops entries from the end until the compressed response fits, so the cursor comes from the last row **included**, not the last one read, and truncated rows are sent on the next page. A single entry that doesn't fit is sent without its image, and if it still doesn't fit it is skipped, with the cursor moved past it, so paging doesn't stall. `moreCount` is the number of matches after the cursor, 0 without one. `searchListedGroups` returns each group's link, without which an entry has no link to send; rows without a public link are consumed by the cursor and not sent.

**Handler.** `DEServiceRequest` in `DirectoryEvent`, from `CEvtServiceRequest`; `directoryServiceEvent` decodes the request, searches, and replies with `APISendServiceResponse`. A payload that isn't a valid JSON object is dropped by the core and logged, so the handler sees a well-formed object and answers a wrong shape or unknown method with the error response, never a chat message.

The handler runs off the event loop: the directory processes registrations, captchas and owner commands on one sequential consumer, so `forkWithinCap` forks each request with at most `maxServiceRequestsInFlight` (8) running, and frees the place however the request ends.

**The limit is global, and that is a deliberate privacy trade.** A signed request would give a caller identity, but a stable key would correlate every search a user ever makes, so the app doesn't sign and no per-caller key exists. All requesters share one cap, generous relative to legitimate traffic. Over it, a request is **rejected** with a reason (`APIRejectServiceRequest`, "service is busy"), so the requester gets an immediate failure instead of a 10 s timeout. A rejection is enqueued rather than sent from the event loop.

Per-request agent state on the responder is released: `replyRequest_` deletes the invitation, `sendReplyAsync` enqueues `ICReplyDel`, which sends and then deletes the connection, and a periodic sweep clears expired requests at `serviceResponseTimeout` (180 s).

**Paging by the sort key.** Each search mode pages by the key it sorts on, tie-broken on `r.group_id` (unique), so `/next` and the RPC neither skip nor repeat rows. The mixed-direction sort rules out a row-value comparison:

```sql
-- STAll, STSearch
AND (g.summary_current_members_count < ? OR (g.summary_current_members_count = ? AND r.group_id > ?))
ORDER BY g.summary_current_members_count DESC, r.group_id ASC

-- STRecent
AND (r.created_at < ? OR (r.created_at = ? AND r.group_id > ?))
ORDER BY r.created_at DESC, r.group_id ASC
```

`SearchCursor` therefore holds the sort value plus the group id: `SCMembers {lastMembers, lastGroupId}` or `SCRecent {lastCreatedAt, lastGroupId}`. `/next` replays the saved `searchType` with it. The count queries take the same condition, so the count is the remainder. Anyone can send a search, so a text search's cursor has no timestamp: the group id shows only the order the directory joined groups.

`testSearchGroupsPaging` covers `/all`, `/new` and text search with member counts that are not monotone in id: its last registered group has the most members.

## 2. Core: bot support and the DR address

`CoreChatOpts` has `serviceRequests :: Bool` with a `--service-requests` switch, read in `runSimplexChat`, and false by default. The directory binary always sets it. The requester needs no flag. A service request received while the option is off is logged and dropped.

`APIRejectServiceRequest` lets a service refuse a request, with a reason that reaches the requester at once; without a reason the requester waits out its timeout.

**The address is upgradable in place; the published link does not change.** Requests need address ratchet keys or fail with `ASENotDRAddress`, and an address created without `pq_ratchet` has none. `setConnShortLink` creates keys when there are none, so `/_rotate_address_keys <user id>` (`APIRotateAddressRatchetKeys`) upgrades the existing address, reusing the same `shortLinkKey` and `linkId`, so the link string is unchanged. Earlier generations are retained, so rotating doesn't break in-flight requests. `testServiceRequestUpgradedAddress` covers it.

**Only the short link contains the keys.** The address's full link was built without them, while `serviceRequest_` reads the keys from the resolved URI, so any full-link path gives `ASENotDRAddress`. The app uses the short link (§3).

The directory's short link is published in `docs/DIRECTORY.md`:

```
https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok
```

`/a#` is the contact-address short-link form. The links shipped in the apps' What's New cards are unrelated: full links used only as "learn more" targets.

Before the release that uses search, the live address's keys are rotated with the new CLI on the directory's database, then the new directory service is started.

**A store-only plan of a group's short link.** `connectPlan` with `resolve=never` (`PRMNever`) reads only the store. For a group's short link it finds a prepared group, and one joined with `/c` and that link, whose connection stores the full link under the short link. The apps use this to recognise the user's own chats among results (§5) with no network request.

## 3. Apps: API binding

Kotlin `CC.APISendServiceRequest(userId, target, timeoutSec, request: JsonObject)`, with `cmdString` matching the parser (`/_service_request <userId> <target>[ timeout=<s>][ sign_key=<k>] <json>`; the request is never signed, see §1), a `CR.ServiceResponse` case, and `apiSearchDirectory(rh, text, cursor)`. iOS: the same as a `ChatCommand` case plus a response case in `ChatResponse1`.

`APISendServiceRequest` stays general: it takes the target. The directory's **short link** (§2) is a constant in the app, `DIRECTORY_SERVICE_LINK`, declared once per platform and referenced only by `apiSearchDirectory`. Two copies is the accepted cost of keeping the command address-agnostic.

The app sends at most 100 characters of search text, the directory's limit, counted as Unicode scalars on iOS and code points on Kotlin, as the directory counts.

The call blocks until reply or timeout, with `requestTimeout` of **10 s**. On Kotlin it runs with `withApi` and uses plain `sendCmd`, so a timeout gives a retry row rather than the alert `sendCmdWithRetry` would show. Progress shows through `ConnectProgressManager`, whose spinner appears after 1 s in the search field; its `onCancel` is a way out of the wait, and what is already shown stays.

**The progress manager has an owner, because a search and a connection can overlap.** Tapping a result while a search runs is normal: `planAndConnect` calls `cancelConnectProgress()` and then starts its own progress. There is one slot rather than a second spinner:

- `startConnectProgress(text, owner, onCancel)` records the owner, and a search doesn't take the spinner from a running connection;
- `stopConnectProgress(owner)` does nothing unless the current owner matches, so a late search result can't clear a connection's spinner;
- `cancelConnectProgress()` cancels whichever owner has it.

There are two owners, `directorySearch` and `connect`; other call sites use `connect`.

`OnlineSearchModel` guards stale replies with a generation counter, bumped on every text change, profile switch and cancel; a reply whose generation differs writes nothing. The request contains no profile, so incognito needs nothing.

**The response comes from another party**, so `parseDirectorySearchResponse` accepts only a `searchResults` response and decodes entries one at a time: an entry the app can't decode is dropped, and the rest of the page is kept. An entry is kept only when its link is exactly one SimpleX link of type group or channel. Its image is kept only when it is a JPEG or PNG data URI (`isSafeImageData`), as the core sends images; otherwise the entry is kept without it. `moreCount` is kept only when it is a valid count; anything else leaves it out, and the row reads "Load more". Text from the directory is drawn as plain text, never parsed as markdown, with line limits.

## 4. Search UI

**Rows.** `SearchInDirectoryRow` sits with `ConnectByNameRow` on all four surfaces: the chat list with either toolbar, and New chat on each platform.

| Search text | Toolbar at the bottom (slot above the field) | Toolbar at the top |
|---|---|---|
| Empty | Tags | Tags above; nothing below |
| Plain text | "Search in Directory"; nothing while results show or load | Tags above; "Search in Directory" below, nothing while results show or load |
| Name candidate | "Connect to …", and "Search in Directory" until results show | Tags above; "Connect to …" below, and "Search in Directory" until results show |
| Pasted SimpleX link | Tags | Tags above; nothing below |

The directory row is not offered for a SimpleX link, which connects on its own. "Connect to …" stays next to the results. On Android with the toolbar at the top, "Connect to …" takes the tags' place. On desktop the tags always show, with the rows between them and the field.

**Keyboard.** `SearchTextField` has an optional `onSearch`, passed only from the chat list's field and New chat's, and not from Archived contacts, which shares New chat's bar: on Android through `KeyboardActions`, on desktop on the Enter key's release. iOS uses `.submitLabel(.search)` and `.onSubmit`. A field with `onSearch` doesn't autocorrect or capitalize, so the key searches what was typed; other search fields keep autocorrect.

**Actions.** The keyboard key and the row run the same directory search, which never looks up SimpleX names: a name is looked up only by "Connect to …", and the directory is the search's only recipient. Both do nothing for empty or whitespace-only text, which would otherwise match the whole listing, and for a pasted SimpleX link.

**The search bar is always present.** With no conversations yet, the onboarding content shows below a live search bar, on desktop too, and the results take the onboarding pages' place while a search runs or has results. On Android with the toolbar at the bottom, the onboarding cards keep their size when the field opens the keyboard, and the pages above the field scroll.

## 5. Results

One list without headers, below the local chats: first the user's chats among the results that the list doesn't show, as their regular chat rows; then the other results; then the load or retry row. With the toolbar at the bottom the list is mirrored, so the user's own chats stay nearest the field and the load row is farthest from it. On New chat the matching contacts and the results form one card, outside `ContactsList`, which Archived contacts also uses.

**Result rows** are drawn from the response: `image`, `#` and `displayName`, `simplexName` as plain text, `shortDescr` in up to two lines, and the member or subscriber count from `entryType.summary`, a channel when `groupType` says so. Trailing "Open". `SimplexNameView` isn't reused: it verifies automatically, so each row would make a network call. The name shown is the directory's claim, and connecting uses the link.

**Nothing is shown twice.** Each new entry's link is planned store-only (§2, `apiConnectPlan` with `.never`, logging failures without an alert) and mapped by `directoryResultState`:

| State | Plan | Drawn as |
|---|---|---|
| Your chat | names a chat: a known or prepared group, the user's own group, or a pending join with its chat | hidden when the list shows that chat, otherwise its chat row once, which opens it |
| Joining | a pending join without a chat | a result with "Already joining" and no Open |
| Open | anything else, including no plan | a result with Open |

Only chat ids from the store decide this, never a field the directory wrote. An entry repeated across pages is dropped by its link, which is also the row key. A page whose results are all chats already listed adds no row and no note. Plan results are read again when a connection started from a result ends, since that flow creates the chat.

**Opening.** A tap calls `planAndConnect(link)` with `prepareGroup = open`, which makes the online plan, with the spinner in the field, and opens the prepared chat straight away with its Join button. The "Open channel?" question shows only when the plan has something to warn about: a checked signature, or the name also leading to another chat. With the alert skipped, a prepared channel's banner shows its subscriber count from the link data. For a Joining result it uses the default, `confirm`, so the pending join keeps its alert. Prepared chats the user opened but didn't join stay in the chat list, as for a pasted link. A long press (iOS 16 and later, Android) or a right click on desktop shows a menu with Open.

**Cards.** While a search runs or has run, the chat list hides its banner and cards, even after a search that found nothing; filtering by typing keeps them.

**Empty states** depend on whether an online search has run:

- **not run, with text** — no message: "Search in Directory" is offered instead, so "No chats found" and New chat's "No filtered contacts" never sit next to it;
- **running, or anything found** — no message;
- **finished, nothing found** — "No chats found" on the chat list (in onboarding, as a row); in New chat "No filtered contacts", or "No contacts" with no contacts.

"You have no chats" on Android and desktop hides while a search runs or has results.

## 6. Pagination

The model holds the results, `moreCount`, `cursor`, `loading`, `failed` and `searched`.

The app renders exactly what the response contains: as many entries as fit the envelope, which is often one group when the entry has a full-size image (§1). A short first page is acceptable.

Filling the list is manual. The last row is "Load more · N left", or "Load more" when the directory gives no count. It calls with the stored cursor, appends de-duplicated by link, is greyed while the page loads, and goes away when there is no cursor. On failure it becomes "Search failed, tap to retry", which resumes from the cursor or repeats the first page; the results shown stay. No automatic paging and no prefetch: each request is a new connection, so pages load only on a tap. The row says how many are left rather than how many it will load, since the envelope decides that.

Changing the search text or switching profile discards the results and the cursor, at the view that holds the text, since a profile switch can replace the search bar.

## 7. Consent

Search text leaves the device only when the user agrees. The "Directory search" switch in Settings › Privacy is off by default. While it is off, every online search, load and retry shows the alert "Enable Directory search?": "Search text is sent to SimpleX Directory, each search separately and without your profile." **Cancel** sends nothing; **Enable** turns the switch on and searches. Once it is on, nothing asks.

No contact is created and no profile is sent.

## 8. Strings

Kotlin: `common/src/commonMain/moko-resources/base/strings.xml`. iOS: inline `NSLocalizedString` at the use site. Texts: the row label, the load and retry rows, "Already joining", the members count, the alert's title and body, the switch, the spinner, and New chat's "No contacts".

## 9. Failure modes

| Case | Surfaced as |
|---|---|
| Address not DR, or a full link used | `ASENotDRAddress`: logged, retry row |
| Directory over its cap | rejected at once → retry row, not a 10 s wait (§1) |
| Directory silent, or service requests not enabled | timeout after 10 s → retry row |
| Response too large to send | prevented by filling the page against the compressed size (§1) |
| Network down | fails within the 10 s → retry row |
| Error response, or a response that can't be parsed | logged, retry row |
| One entry that can't be decoded, or with another kind of link | that entry dropped, the rest of the page kept |
| A store lookup of a result fails | a result with Open, whose online plan shows the real error; no alert |
| Response late, or after a text change, profile switch or cancel | dropped by the generation counter |
| No results | no rows; the empty state of §5 |
| A chat the user already has | its chat row, or hidden (§5) |

## 10. What the design protects, and its limits

The directory learns the search text and when it was sent, and nothing else about the requester: a request is unsigned, contains no profile, and uses a new connection that is deleted after the reply or the timeout. Requests are end-to-end encrypted from the requester to the directory's address. The pages of one search share their text and cursor, so the directory can link them.

The directory or whoever controls it is treated as untrusted by the apps: they accept only group and channel links, draw its text as plain text and only JPEG and PNG images, and make no network request and show no alert for an entry until the user opens it. Listing owners can't affect the requester beyond their own listing's bounded fields. Anyone can send requests without connecting, so the work of one request is bounded by the text limit and the page size, and the work of all requests by the global cap.

Each request's reply queue is created on one of the user's SMP servers, as for any new connection, so the consent alert promises only that each search is sent separately and without the profile.

Limits of what is built:

- A group the user was added to by invitation, or joined through a link its owner has since replaced, isn't recognised: it shows as a result with Open beside the user's own chat, and Open prepares a second copy. Recognising it needs the group's current link in the member's group record, a protocol change. Channels are always joined through a prepared chat, so they are recognised.
- "N left" counts listings without a public link, which are never sent, so it can promise more than loading brings.
- One directory, fixed in the apps.

Out of scope: streaming the response, per-row image fetch, directory browse, and self-hosted directory configuration.
