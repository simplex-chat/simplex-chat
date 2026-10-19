# Plan: Fix support-bot crash on large databases — use pagination and direct lookup

## Context

The simplex-support-bot crashes during startup against large production
databases:

```
[2026-04-30T15:52:53.498Z] Grok contact from state: ID=142676
[2026-04-30T15:52:53.498Z] Resolving team group...
<anonymous_script>:0
[Error: Unknown failure]
```

The crash happens inside `chat.apiListGroups(mainUser.userId)` at
`apps/simplex-support-bot/src/index.ts:215`. The native binding marshals the
Haskell core's response to a JS string at
`packages/simplex-chat-nodejs/cpp/simplex.cc:255` (`chat_send_cmd`) →
`HandleCResult` (line 157) → `Napi::String::New` in `OnOK`. When the response
exceeds V8's max string length (~512 MB on 64-bit), N-API string allocation
fails. The literal string `"Unknown failure"` does **not** appear anywhere in
this repo — confirmed by full-tree search — so the message originates from V8
or N-API internals rather than the binding's own error path (which would say
`chat_send_cmd failed`). Hypothesis: oversized string allocation throws a JS
exception that propagates up unannotated.

Two distinct misuse patterns drive the payload size:

**A. List-then-find by ID** (most call sites). The bot pulls every contact /
every group with `apiListContacts` / `apiListGroups`, then calls `find(...)`
to locate one record by a known ID. This is gratuitous — there is already
`apiGetChat(chatType, chatId, count=0)` (`packages/simplex-chat-nodejs/src/api.ts:819`)
that returns one `AChat` whose `chatInfo` carries the full `GroupInfo` /
`Contact` (with `customData`) and zero items. The Haskell parser accepts
`count=0` (`src/Simplex/Chat/Library/Commands.hs:5210`), and
`getDirectChatLast_` / `getGroupChatLast_` return empty `chatItems` with full
`chatInfo`.

**B. Genuine multi-record scan** (one site).
`apps/simplex-support-bot/src/cards.ts:131` (`refreshAllCards`) enumerates
groups where `customData.cardItemId && !complete` to refresh in-flight cards
on restart. The Haskell side already supports paginated scans via
`APIGetChats` (`/_get chats {userId} pcc=on|off count=N`,
`src/Simplex/Chat/Library/Commands.hs:4868`). It is currently in
`undocumentedCommands` (`bots/src/API/Docs/Commands.hs:360`), so the codegen
does not emit it for the TypeScript bot library. Confirmed: the chat preview
returned by `getChatPreviews` carries `customData` on `GroupInfo`
(`src/Simplex/Chat/Store/Shared.hs:685`, `toGroupInfo`).

Active card state is already tracked on each group via `customData.cardItemId`
and `customData.complete` (written through `apiSetGroupCustomData` at
`apps/simplex-support-bot/src/cards.ts:103,231`). No `state.json` schema
change is needed — phase 3 reads exactly the same `customData` it already
writes, just via paginated `APIGetChats` instead of a full `apiListGroups`.

Per the constraint, `apiListContacts` / `apiListGroups` stay in the nodejs
library unchanged for other consumers. Audit confirmed no callers outside
support-bot use them today.

## Phase 1 — Plumb `APIGetChats` through the bot library

The codegen pipeline is test-driven: `tests/APIDocs.hs:41–44` invokes
`testGenerate` against the functions in
`bots/src/API/Docs/Generate/TypeScript.hs`, which writes to:

- `packages/simplex-chat-client/types/typescript/src/commands.ts`
- `packages/simplex-chat-client/types/typescript/src/responses.ts`
- `packages/simplex-chat-client/types/typescript/src/types.ts`

Run via `cabal test`. The published `@simplex-chat/types` npm package is
built from this TypeScript source; the copy under
`packages/simplex-chat-nodejs/node_modules/@simplex-chat/types/dist/` is a
downstream build artifact and is **not** edited directly.

Currently missing from generated TS:
`T.PaginationByTime`, `T.ChatListQuery`, `CC.APIGetChats`, and the
`apiChats` response tag on `ChatResponse`.

### 1.1 `bots/src/API/Docs/Commands.hs`

- **Remove** `"APIGetChats",` from `undocumentedCommands` (line 360).
- **Add** an entry under "Chat commands" (next to `APIListContacts` /
  `APIListGroups` at lines 145–146). Match the Haskell parser at
  `src/Simplex/Chat/Library/Commands.hs:4868`:

  ```haskell
  ( "APIGetChats",
    [],
    "Get chat previews. Supports time-based pagination — use this " <>
    "instead of APIListContacts / APIListGroups when scanning at scale.",
    ["CRApiChats", "CRChatCmdError"],
    [],
    Nothing,
    "/_get chats " <> Param "userId"
      <> OnOffParam "pcc" "pendingConnections" (Just False)
      <> Optional "" (" " <> Param "$0") "pagination"
      <> Optional "" (" " <> Json "$0") "query"
  )
  ```

  Note: the `query` segment uses `" " <> Json "$0"` (no `"json "` prefix) —
  the parser accepts `A.space *> jsonP` directly.

### 1.2 `bots/src/API/Docs/Types.hs`

The type universe already references `PaginationByTime` and `ChatListQuery`
in commented form (lines 381, 390 and 592, 602). Uncomment all four lines.
Confirm the constructor-prefix encoding (`STRecord`/`STUnion`, prefix
`""`/`"CLQ"`) matches the existing definitions in
`src/Simplex/Chat/Controller.hs:992,998` and the JSON deriving at line 1661
(`sumTypeJSON $ dropPrefix "CLQ"`).

### 1.3 `bots/src/API/Docs/Responses.hs`

- Uncomment `("CRApiChats", "...")` at line 100.
- Remove `"CRApiChats",` from `undocumentedResponses` at line 123.

### 1.4 Regenerate TypeScript types

Run `cabal test` (the `APIDocs` test suite drives generation). Inspect the
diffs in `packages/simplex-chat-client/types/typescript/src/{commands,responses,types}.ts`.
Verify:

- `T.PaginationByTime` (sum type with `PTLast`/`PTAfter`/`PTBefore`) exists
  with a generated `cmdString`. Compare wire format against the Haskell
  `paginationByTimeP` at `src/Simplex/Chat/Library/Commands.hs:5216`:
  `count=N` | `after=TS count=N` | `before=TS count=N`.
- `T.ChatListQuery` exists with `CLQFilters` / `CLQSearch` JSON-encoded
  variants.
- `CC.APIGetChats.cmdString({userId, pendingConnections, pagination, query})`
  exists and emits the expected wire format.
- `r.type === "apiChats"` with `r.chats: T.AChat[]` exists in the response
  union (drops `CR` prefix per `sumTypeJSON`,
  `src/Simplex/Chat/Controller.hs:1743`).

Bump `@simplex-chat/types` version and re-link / re-build the
`simplex-chat-nodejs` package so the new symbols are available.

### 1.5 `packages/simplex-chat-nodejs/src/api.ts`

Add a single method next to `apiListGroups` (line 761):

```ts
/**
 * Get chat previews (paginated).
 * Network usage: no.
 *
 * Prefer this over apiListContacts / apiListGroups for any scan: those
 * methods load the entire history into memory and will fail on large DBs.
 */
async apiGetChats(
  userId: number,
  pagination: T.PaginationByTime,
  query: T.ChatListQuery = {type: "filters", favorite: false, unread: false},
  pendingConnections = false,
): Promise<T.AChat[]> {
  const r = await this.sendChatCmd(
    CC.APIGetChats.cmdString({userId, pendingConnections, pagination, query})
  )
  if (r.type === "apiChats") return r.chats
  throw new ChatCommandError("error getting chats", r)
}
```

(Exact `T.PaginationByTime` / `T.ChatListQuery` shapes come from the codegen
output of phase 1.4 — verify the discriminator field names before locking
this signature.)

## Phase 2 — Replace list-then-find with direct lookup

For every site below, replace `apiList…().find(…)` with
`apiGetChat(ChatType.X, id, 0)`. Treat "not found" — the chat was deleted —
as a clean missing-record case (log + skip). The wire format
`/_get chat #{id} count=0` is already supported.

### 2.1 Error matcher

The Haskell `SEContactNotFound` / `SEGroupNotFound` (in
`src/Simplex/Chat/Store/Shared.hs:863` and elsewhere) surface to TS as:

```ts
err.chatError?.type === "errorStore"
  && err.chatError.storeError.type === "groupNotFound" // or "contactNotFound"
```

Both discriminators are already present in the generated types
(`types.d.ts:2825` and `:2788`). Add a small helper in
`apps/simplex-support-bot/src/util.ts`:

```ts
export function isChatNotFound(err: unknown, kind: "group" | "contact"): boolean {
  if (!(err instanceof core.ChatAPIError)) return false
  if (err.chatError?.type !== "errorStore") return false
  const seType = err.chatError.storeError.type
  return kind === "group" ? seType === "groupNotFound" : seType === "contactNotFound"
}
```

(Strict — does not swallow other `errorStore` variants.)

### 2.2 Ergonomic wrappers

Add two thin helpers in `apps/simplex-support-bot/src/util.ts` (the constraint
forbids touching `apiListContacts` / `apiListGroups` in the nodejs library;
keeping these helpers in the support-bot util keeps the library surface
unchanged):

```ts
export async function getGroupInfo(chat: api.ChatApi, groupId: number): Promise<T.GroupInfo | null> {
  try {
    const c = await chat.apiGetChat(T.ChatType.Group, groupId, 0)
    return c.chatInfo.type === "group" ? c.chatInfo.groupInfo : null
  } catch (err) {
    if (isChatNotFound(err, "group")) return null
    throw err
  }
}

export async function getContact(chat: api.ChatApi, contactId: number): Promise<T.Contact | null> {
  try {
    const c = await chat.apiGetChat(T.ChatType.Direct, contactId, 0)
    return c.chatInfo.type === "direct" ? c.chatInfo.contact : null
  } catch (err) {
    if (isChatNotFound(err, "contact")) return null
    throw err
  }
}
```

### 2.3 Call-site changes

All sites must keep their existing `withMainProfile` / `profileMutex`
wrapping where present.

- **`apps/simplex-support-bot/src/index.ts:165–180`** (Grok contact
  resolution). Drop the `apiListContacts(mainUser.userId)` call entirely. If
  `state.grokContactId` is set, call `getContact(chat, state.grokContactId)`
  inside `profileMutex.runExclusive`. Preserve the existing log lines.
- **`apps/simplex-support-bot/src/index.ts:306–320`** (team member
  validation). Loop and `getContact(chat, member.id)` per member. Compare
  `displayName` as before. Team rosters are small; N round-trips are fine.
- **`apps/simplex-support-bot/src/index.ts:213–227`** (team group
  resolution). Replace `apiListGroups` + `find` with
  `getGroupInfo(chat, state.teamGroupId)`. Preserve the "create new group"
  fallback when the lookup returns `null`.
- **`apps/simplex-support-bot/src/bot.ts:796–805`** (`handleJoinCommand`).
  Replace with `getGroupInfo(chat, targetGroupId)`; same `businessChat`
  validation.
- **`apps/simplex-support-bot/src/cards.ts:120`** (`flushOne`). Direct
  `getGroupInfo(chat, groupId)` (still inside `withMainProfile`).
- **`apps/simplex-support-bot/src/cards.ts:213`** (`getRawCustomData`).
  Direct lookup. **Hot path** — called on every `mergeCustomData` /
  `clearCustomData`. Largest single win.
- **`apps/simplex-support-bot/src/cards.ts:251`** (`updateCard`). Direct
  lookup. The "Read customData and groupInfo in one apiListGroups call"
  comment goes away.

After phase 2 the bot can boot and operate steadily on a large DB; phase 3
is purely about startup reconciliation.

## Phase 3 — Paginate `refreshAllCards`

`apps/simplex-support-bot/src/cards.ts:131` is the only legitimate
multi-record scan. Convert it to a single bounded `apiGetChats` call:

```ts
async refreshAllCards(): Promise<void> {
  // Scan the most recently active 1000 chats. Active cards live on
  // recently-active customer chats by definition — a card stays open while
  // the conversation is in flight. If the bot has been offline long enough
  // that an active card has fallen outside the recent-1000 window, that
  // card refreshes lazily on the next customer message (which moves the
  // chat back into the recent window).
  const chats = await this.withMainProfile(() =>
    this.chat.apiGetChats(
      this.mainUserId,
      {type: "last", count: 1000},
    )
  )
  const activeCards: {groupId: number; cardItemId: number}[] = []
  for (const c of chats) {
    if (c.chatInfo.type !== "group") continue
    const customData = c.chatInfo.groupInfo.customData as Record<string, unknown> | undefined
    if (customData
      && typeof customData.cardItemId === "number"
      && !customData.complete) {
      activeCards.push({
        groupId: c.chatInfo.groupInfo.groupId,
        cardItemId: customData.cardItemId,
      })
    }
  }
  // (sort and refresh loop unchanged)
}
```

`count = 1000` per the constraint. No `state.json` schema change. Card
status remains entirely on the group's `customData` (`cardItemId`,
`complete`), which is what the bot already reads and writes.

## Phase 4 — Verification

### 4.1 Stress test

Existing tests use `MockChatApi` (`apps/simplex-support-bot/bot.test.ts:24`),
which is in-memory and won't exercise the native binding. A meaningful
stress test needs a real `ChatApi.init` against Postgres.

Add a new test file (e.g.
`apps/simplex-support-bot/test/stress.test.ts`) that:

1. Starts an ephemeral Postgres or uses an existing test DB.
2. Calls `ChatApi.init` and seeds N synthetic groups + contacts via the
   chat API. (No existing seeding helper — write one.) Reasonable N: 20k
   each, large enough to expose the marshaling cliff but not so large that
   the test takes minutes.
3. Boots the support-bot main flow against this DB and asserts: startup
   completes within a wall-clock budget; resident memory stays bounded;
   no native error.

This is new infrastructure — keep scope tight. If standing up Postgres in
CI is too heavy, run as a manual stress harness rather than a CI test.

### 4.2 Production replay

Replay against (a copy of) the affected production DB. Confirm the bot
starts and the team group / Grok contact / team members all resolve.

### 4.3 Smoke tests

Existing functional flows via `bot.test.ts` continue to pass after the
phase-2 changes. Manually exercise:

- Business-request acceptance.
- `/join` validation (the changed `bot.ts:799` path).
- Card create/update/complete cycle (`cards.ts` hot path).
- Restart-time card refresh (`refreshAllCards`).

## Risks and footguns

- **`/_get chats` parser default is `PTLast 5000`**
  (`src/Simplex/Chat/Library/Commands.hs:4872`). Even 5000 previews can be
  heavy. Support-bot now always passes an explicit `count=1000`, but the
  default itself remains a footgun for other callers — flag for follow-up;
  not changed here.
- **`apiListMembers` is per-group, not per-DB.** Used at `bot.ts:629,825`
  and `cards.ts:165`. Bounded by group membership, not history size, so
  out of scope for this fix. Flag if customer groups grow huge (>1000
  members) — would warrant a paginated members API at that point.
- **Codegen output sanity.** Phase 1.4 must be inspected by hand — the
  generated `cmdString` for `T.PaginationByTime` and the `r.type` /
  `r.chats` shape on the response side are the integration points the rest
  of the plan depends on. Do not skip eyeballing the diff.
- **`apiGetChat(..., 0)` semantics on a non-existent chatId.** Verified:
  the error tag is `chatError.type === "errorStore"` with
  `storeError.type === "groupNotFound"` or `"contactNotFound"`. Both
  discriminators already exist in the generated types
  (`types.d.ts:2825,2788`). `isChatNotFound` matches them precisely; do
  not loosen it.
- **Native binding crash hypothesis is unverified.** The literal "Unknown
  failure" string is not in this tree. Most likely V8/N-API surfacing a
  string-allocation or JSON-parse failure. The fix in this plan addresses
  the proximate cause (oversized response payload) regardless of the exact
  surfacing path; if the same error reappears after the fix, dig into the
  binding's `OnOK` handler to add explicit size-check / better diagnostics.
- **`@simplex-chat/types` package version bump.** Phase 1.4 produces
  TypeScript changes in `packages/simplex-chat-client/types/typescript/`.
  Bumping the version and re-publishing (or rebuilding locally) is required
  before phase 1.5 lands. Coordinate the release sequence.

## Out of scope

- Deprecating or paginating `apiListContacts` / `apiListGroups` in the
  nodejs library. They stay as-is; only support-bot stops calling them.
- Lowering the `/_get chats` parser default from `PTLast 5000`.
- Adding a paginated members API.
- Native binding diagnostics for oversized responses.
