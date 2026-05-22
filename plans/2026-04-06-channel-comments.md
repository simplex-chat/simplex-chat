# Plan: Comments on channel messages

## Context

SimpleX Chat introduced channels in PR #6382 (the chat-relays MVP): groups where one node acts as a relay between the owner and many subscribers, posts are signed, and subscribers do not see each other. Channels currently support posts only — there is no way for subscribers to discuss a post.

Channels need Telegram-style **comments**: under each channel post, a tappable affordance shows a count and opens a separate Comments view containing a flat thread of replies, with its own composer. Subscribers can comment, including quoting the parent post and quoting other comments.

Three architectural choices shape the rest of the plan:

1. **Wire format.** `ExtMsgContent` and `MsgContainer` are merged into a single record `MsgContainer` whose shape matches the wire JSON 1:1. The dead `MCComment` constructor is replaced with a `parent :: Maybe MsgRef` field on the merged record. A single message can now carry both a `quote` (referencing any prior message) and a `parent` (referencing the channel post being commented on). The parent reference lives in the message body, so no new `MsgScope` variant is required.

2. **Local model.** Comments are not a `GroupChatScope` variant. A new `ChannelMsgInfo` type is added as a separate field on `ChatInfo.GroupChat` and as a separate parameter to pagination, parallel to `Maybe GroupChatScopeInfo`. This keeps the Comments view as a distinct concept rather than overloading the scope mechanism.

3. **iOS UI.** The Comments view opens via a hidden `NavigationLink` pushed onto the existing chat navigation stack (the same trick used by `NavStackCompat` and `UserSupportChatNavLink` / `GroupReportsChatNavLink`), not as a sheet. The user can navigate forward into a comment, back to the channel, and forward into another post's comments without losing context.

The plan covers Haskell core, the CLI command surface (for tests), and the iOS app. Kotlin Multiplatform (Android/Desktop) is deferred to a follow-up. Profile dissemination between subscribers is also a separate follow-up; for the MVP, comment authors who are unknown to a viewing subscriber appear as unknown members (the existing `unknownMemberRole` mechanism). There is no unread tracking on comments — only a running total comment count per post.

## Solution summary

- Merge `ExtMsgContent` into `MsgContainer` as a single record with optional `quote :: Maybe QuotedMsg`, `parent :: Maybe MsgRef`, and plain `forward :: Bool` fields. Remove the four `MC*` constructors and `mcExtMsgContent`. Rewrite `parseMsgContainer` and `msgContainerJSON` to read/write the three discriminator fields independently rather than as mutually-exclusive arms — the existing wire JSON shape is preserved exactly because every existing message uses at most one of these keys and `forward` is only emitted when `True` (matching the existing `MCForward` serializer arm).
- Add `commentsVersion :: VersionChat = VersionChat 18` and bump `currentChatVersion = VersionChat 18`. Comment-bearing messages (those whose container has `parent = Just _`) and the new `XGrpCommentsDisabled` event are gated for relay forwarding behind this version.
- New SQLite + Postgres migration `M20260501_channel_comments` adding three columns to `chat_items`:
  - `parent_chat_item_id INTEGER REFERENCES chat_items(chat_item_id) ON DELETE CASCADE` — set on the comment row.
  - `comments_total INTEGER NOT NULL DEFAULT 0` — set on the parent post row.
  - `comments_disabled INTEGER NOT NULL DEFAULT 0` — set on the parent post row.
  - Two new indexes: one on `parent_chat_item_id`, one on `(user_id, group_id, parent_chat_item_id, item_ts)` for the per-parent comment-thread pagination.
- New Haskell type `ChannelMsgInfo {channelMsgItem :: CChatItem 'CTGroup, channelMsgSharedId :: SharedMsgId}`. Smart-constructed in `Store/Messages.hs` so the SharedMsgId is extracted from the parent at lookup time and the field is total.
- `ChatInfo.GroupChat` constructor extended from two parameters to three: `GroupInfo -> Maybe GroupChatScopeInfo -> Maybe ChannelMsgInfo -> ChatInfo 'CTGroup`. `ChatRef` gains a parallel `channelMsg_ :: Maybe ChatItemId` field. `getGroupChat`, `getChatItemIDs`, and the `APIGetChat` command path gain a new `Maybe ChatItemId` parent argument that resolves to a `ChannelMsgInfo` inside the store helpers.
- New `CommentsGroupPreference {enable :: GroupFeatureEnabled, closeAfter :: Maybe Int}` (modeled on `TimedMessagesGroupPreference`). `closeAfter` is the duration in seconds since post creation after which a channel post stops accepting new comments; `Nothing` means never close.
- New role `GRCommenter` inserted between `GRObserver` and `GRAuthor` in `GroupMemberRole`. Default `channelSubscriberRole` flips from `GRObserver` to `GRCommenter`.
- New chat command `APISetCommentsDisabled groupId chatItemId disabled` and corresponding wire event `XGrpCommentsDisabled SharedMsgId Bool` so the disabled state is disseminated through the relay to all subscribers.
- iOS: `Chat` (more precisely the `.group` case of `ChatInfo`) carries a new third associated value `channelMsgInfo: ChannelMsgInfo?`. `ChatItem` gains `parentChatItemId`, `commentsTotal`, `commentsDisabled` decoded fields with `omittedField`-style defaults. `ChatView` holds a hidden `NavigationLink(isActive:)` driven by `@State commentsParent: ChatItem?`. The "Comments(N)" button on a channel post bubble sets that state, which pushes `SecondaryChatView` for the parent post's comments thread onto the existing chat navigation stack. The existing `getCIItemsModel` router gains a parallel branch that consults a new `ChatInfo.channelMsgInfo()` helper to route incoming comment items to the open Comments view.
- Tests in `tests/ChatTests/Groups.hs` under a new `describe "channel comments"` block, mirroring existing `testChannelMessage*` patterns.

## Detailed technical design

### 1. Merged `MsgContainer` record

`src/Simplex/Chat/Protocol.hs`

**Type changes** (lines ~634–737 in current source).

Replace the existing sum type and the separate `ExtMsgContent` record with a single record type:

```haskell
data MsgContainer = MsgContainer
  { content :: MsgContent,
    -- Mentions: locally (per-message) unique display name → MsgMention.
    -- Suffixes _1, _2 are appended in the UI to make names locally unique.
    mentions :: Map MemberName MsgMention,
    file :: Maybe FileInvitation,
    ttl :: Maybe Int,
    live :: Maybe Bool,
    scope :: Maybe MsgScope,
    asGroup :: Maybe Bool,
    -- The three discriminator fields below are independent on the wire and
    -- may co-occur (e.g. a comment that quotes another comment carries both
    -- `parent` and `quote`).
    quote :: Maybe QuotedMsg,
    parent :: Maybe MsgRef,
    -- forward is a plain Bool (not Maybe) to match the existing wire
    -- format exactly: the existing parser treats absent/false as "not a
    -- forward" and the existing serializer only ever emits "forward":true.
    -- Modelling it as Maybe Bool would regress round-trips of
    -- `"forward": false` and bloat the serialized output with an
    -- explicit false field.
    forward :: Bool
  }
  deriving (Eq, Show)
```

Remove the entire `data ExtMsgContent = ExtMsgContent {...}` block. Remove `mcExtMsgContent`. Remove `isMCForward`. Replace inlined constructors and pattern matches with field access (see "Mechanical refactor" below).

Add four smart constructors for the common send-side cases, so most call sites stay one line:

```haskell
mcSimple :: MsgContent -> MsgContainer
mcSimple c = mc {content = c}

mcQuote :: QuotedMsg -> MsgContent -> MsgContainer
mcQuote q c = mc {content = c, quote = Just q}

mcComment :: MsgRef -> MsgContent -> MsgContainer
mcComment p c = mc {content = c, parent = Just p}

mcForward :: MsgContent -> MsgContainer
mcForward c = mc {content = c, forward = True}

mc :: MsgContainer
mc =
  MsgContainer
    { content = MCText "",
      mentions = M.empty,
      file = Nothing,
      ttl = Nothing,
      live = Nothing,
      scope = Nothing,
      asGroup = Nothing,
      quote = Nothing,
      parent = Nothing,
      forward = False
    }
```

The base `mc` value is the empty container that callers update via record syntax. Send sites that today build an `ExtMsgContent` record and wrap it in `MCSimple` are rewritten to build `MsgContainer` directly: `mcSimple content` followed by record-update for any optional fields (`mentions = ...`, `file = ...`, `ttl = ...`, `live = ...`, `scope = ...`, `asGroup = ...`).

`MsgScope` stays a single-constructor type `data MsgScope = MSMember {memberId :: MemberId}`. Comments do not need a scope tag because the parent reference lives in `MsgContainer.parent`.

**Parser rewrite** (current `parseMsgContainer` at lines ~834–852).

The current parser uses `<|>` to enforce mutual exclusion between MCQuote / MCComment / MCForward / MCSimple. The new parser reads each discriminator field independently:

```haskell
parseMsgContainer :: J.Object -> JT.Parser MsgContainer
parseMsgContainer v = do
  content <- v .: "content"
  mentions <- fromMaybe M.empty <$> (v .:? "mentions")
  file <- v .:? "file"
  ttl <- v .:? "ttl"
  live <- v .:? "live"
  scope <- v .:? "scope"
  asGroup <- v .:? "asGroup"
  quote <- v .:? "quote"
  parent <- v .:? "parent"
  -- forward parsing matches the existing two-arm form exactly:
  --   J.Bool b   -> b     (modern boolean form, including `false`)
  --   J.Object _ -> True  (legacy object form: forward-compat with group links)
  --   absent     -> False
  forward <- (v .:? "forward") >>= parseForward
  pure MsgContainer {content, mentions, file, ttl, live, scope, asGroup, quote, parent, forward}
  where
    parseForward :: Maybe J.Value -> JT.Parser Bool
    parseForward Nothing = pure False
    parseForward (Just (J.Bool b)) = pure b
    parseForward (Just (J.Object _)) = pure True
    parseForward _ = fail "invalid forward field"
```

Backward compatibility: any client below `commentsVersion` that sent only one of `quote`/`parent`/`forward` is parsed identically by the new code — the other two fields are simply `Nothing`. Clients at or above `commentsVersion` that send both `quote` and `parent` (a comment that quotes another comment) are parsed correctly. A pre-`commentsVersion` client that received a message with both `quote` and `parent` would see only the quote (its parser tries `MCQuote` first and never reads `parent`); the relay's per-recipient version gate (see "Forward compatibility" below) prevents such messages from reaching pre-`commentsVersion` clients in practice. No message currently in flight has a `parent` field, because `MCComment` is a dead constructor in the existing source.

**Serializer rewrite** (`msgContainerJSON` at lines ~900–909).

```haskell
msgContainerJSON :: MsgContainer -> J.Object
msgContainerJSON MsgContainer {content, mentions, file, ttl, live, scope, asGroup, quote, parent, forward} =
  JM.fromList $
    discriminators
      <> ("file" .=? file)
        ( ("ttl" .=? ttl)
            ( ("live" .=? live)
                ( ("mentions" .=? nonEmptyMap mentions)
                    ( ("scope" .=? scope)
                        ( ("asGroup" .=? asGroup)
                            ["content" .= content]
                        )
                    )
                )
            )
        )
  where
    -- At most one discriminator is typically present (the existing wire
    -- format treats them as mutually exclusive). When a comment quotes
    -- another comment, both `quote` and `parent` appear; this combination
    -- is gated by commentsVersion relay forwarding.
    discriminators =
      ["quote" .= q | Just q <- [quote]]
        <> ["parent" .= p | Just p <- [parent]]
        <> ["forward" .= True | forward]
```

Key wire-format invariants preserved:
- `forward` is only ever emitted as `"forward": true` (never `false`), matching the existing serializer.
- The field order inside the content sub-object (file, ttl, live, mentions, scope, asGroup, content) is unchanged from the existing `msgContent` helper — the rewritten serializer wraps the same `("file" .=? file) $ ...` fold to keep every produced `J.Object` byte-identical to the existing output for any pre-existing message.
- `nonEmptyMap` (Protocol.hs:911) is reused verbatim; empty `mentions` maps are omitted exactly as before.
- When both `quote` and `parent` are set (only possible at or above `commentsVersion`), both fields are emitted; no message currently in flight has this shape, and the version gate prevents pre-`commentsVersion` recipients from seeing it.

**Mechanical refactor — call sites.**

Across the 5 files (Protocol.hs, Subscriber.hs, Internal.hs, Commands.hs, Messages.hs) the ~50 occurrences fall into three categories:

1. **Construction sites** (e.g. `MCSimple ec`, `MCQuote q ec`, `MCForward ec`, `MCComment _ ec`): replaced with smart-constructor calls. Construction of the inner `ExtMsgContent` is folded in via record update:
   ```haskell
   -- before
   MCSimple ExtMsgContent {content = c, mentions = M.empty, file = Nothing, ttl = Nothing, live = Nothing, scope = Just sc, asGroup = Just True}
   -- after
   (mcSimple c) {scope = Just sc, asGroup = Just True}
   ```

2. **Pattern matches with `mcExtMsgContent`** (e.g. `let ExtMsgContent {content, file, ttl} = mcExtMsgContent mc in ...`): become direct record patterns on `mc`:
   ```haskell
   -- after
   let MsgContainer {content, file, ttl} = mc in ...
   ```
   `mcExtMsgContent` is removed.

3. **Pattern matches on the four constructors**:
   ```haskell
   case mc of
     MCQuote q ec  -> ...
     MCComment _ _ -> ...
     MCForward ec  -> ...
     MCSimple ec   -> ...
   ```
   become field-based dispatch:
   ```haskell
   case mc of
     MsgContainer {parent = Just p, quote = q, content} -> ...   -- comment, possibly quoting
     MsgContainer {quote = Just q, content}             -> ...   -- pure quote
     MsgContainer {forward = True, content}             -> ...   -- forward
     MsgContainer {content}                             -> ...   -- simple
   ```
   Order of arms matters: comment-arms must come before pure-quote arms, because a quoting comment matches both. `isMCForward` becomes `\MsgContainer {forward} -> forward` — a plain Bool projection, no `Just` wrapper needed.

The refactor blast radius is mechanical but wide. To keep the diff focused, all 50 sites are updated in a single commit (Slice 1), and the smart constructors keep most send sites at one line.

### 2. Wire event for per-message disable

Add a new event constructor to `ChatMsgEvent 'Json`:

```haskell
| XGrpCommentsDisabled SharedMsgId Bool
```

Wire it through:
- `chatMsgEventTag` returns a new tag string `"x.grp.cmnts.dsbl"` (or similar; pick to match the existing `x.grp.*` naming).
- `appJsonToCM` parses the new tag back to the constructor.
- `cmToAppMessage` serializes it to JSON with two named fields `parentMsgId` and `disabled`.
- `processForwardedMsg` (in Subscriber.hs) routes it through the standard relay forwarding path so subscribers learn the new state.
- The event is signed exactly like other channel governance events.

### 3. Chat protocol version

Bump `currentChatVersion` from 17 to 18 and add the new constant in the version block:

```haskell
commentsVersion :: VersionChat
commentsVersion = VersionChat 18

currentChatVersion :: VersionChat
currentChatVersion = commentsVersion
```

The version is consumed in two places only:

1. **Send-side per-recipient relay forwarding**: when fanning out a message whose container has `parent = Just _`, or when fanning out an `XGrpCommentsDisabled` event, the relay must skip recipients whose chat protocol version is below `commentsVersion`. The same gating function used today for `groupKnockingVersion` and `contentReportsVersion` (typically a `VersionRangeChat`-based filter inside `getGroupRelayMembers` / `infoToDeliveryContext` callers) is the place to add the new check.

2. **Send-side preflight in `Commands.hs`**: when the user attempts to post a comment, the chat-version check that ensures every recipient supports the feature uses `commentsVersion`. If any required member's version is below the threshold, the send is rejected at the API boundary with a clear error.

### 4. DB schema migration

`src/Simplex/Chat/Store/SQLite/Migrations/M20260501_channel_comments.hs` (new) and the matching Postgres file `src/Simplex/Chat/Store/Postgres/Migrations/M20260501_channel_comments.hs`. Date is a placeholder; pick the actual date during implementation.

```sql
ALTER TABLE chat_items ADD COLUMN parent_chat_item_id INTEGER REFERENCES chat_items(chat_item_id) ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN comments_total INTEGER NOT NULL DEFAULT 0;
ALTER TABLE chat_items ADD COLUMN comments_disabled INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_parent_chat_item_id ON chat_items(parent_chat_item_id);

CREATE INDEX idx_chat_items_parent_item_ts ON chat_items(
  user_id,
  group_id,
  parent_chat_item_id,
  item_ts
);
```

Down migration drops the columns and indexes. Register the migration in both `Migrations.hs` files and add the new modules to `simplex-chat.cabal` exposed-modules. `chat_schema.sql` is auto-regenerated by tests; do not hand-edit.

The cascade self-FK is safe — SQLite cascades through one level by default, and child→child cascading is not needed because comments do not chain. Tests already cover one-level cascade for the existing self-FK on `chat_items.quoted_item_id`.

### 5. Local types: `ChannelMsgInfo` and `ChatInfo` extension

`src/Simplex/Chat/Messages.hs`

1. **New type** (placed near `GroupChatScopeInfo` at ~line 174):
   ```haskell
   data ChannelMsgInfo = ChannelMsgInfo
     { channelMsgItem :: CChatItem 'CTGroup,
       channelMsgSharedId :: SharedMsgId
     }
     deriving (Show)
   ```
   Only `Show` is derived. `CChatItem` has a hand-written `deriving instance Show (CChatItem c)` at `Messages.hs:338` and no `Eq` instance, so `ChannelMsgInfo` cannot derive `Eq` either; adding an `Eq (CChatItem c)` instance is out of scope and unnecessary because `ChannelMsgInfo` is only ever stored, threaded through, and pattern-matched — never compared. The two fields are co-determined (the SharedMsgId is extracted from `channelMsgItem` at smart-constructor time), so we keep them paired in the type rather than forcing every consumer to re-extract.

2. **`ChatInfo.GroupChat` constructor** (at ~line 164–166): extend from two parameters to three.
   ```haskell
   data ChatInfo (c :: ChatType) where
     DirectChat :: Contact -> ChatInfo 'CTDirect
     GroupChat  :: GroupInfo -> Maybe GroupChatScopeInfo -> Maybe ChannelMsgInfo -> ChatInfo 'CTGroup
     LocalChat  :: NoteFolder -> ChatInfo 'CTLocal
     ...
   ```
   Every pattern match on `GroupChat gInfo scopeInfo` becomes `GroupChat gInfo scopeInfo channelMsgInfo`. The third parameter is `Nothing` for the channel chat itself and `Just ChannelMsgInfo {...}` for the comments thread of a specific post. The two `Maybe` fields are mutually exclusive in practice (the Comments view does not also have member-support scope), but the type does not enforce it — runtime invariant only.

3. **`ChatRef`** (~line 161): add `channelMsg_ :: Maybe ChatItemId`.
   ```haskell
   data ChatRef = ChatRef
     { chatType :: ChatType,
       chatId :: Int64,
       chatScope :: Maybe GroupChatScope,
       channelMsg_ :: Maybe ChatItemId
     }
   ```
   Existing call sites that build `ChatRef` are extended to pass `channelMsg_ = Nothing` by default. The Comments view passes the parent `ChatItemId`.

4. **No changes** to `GroupChatScope`, `GroupChatScopeTag`, `GroupChatScopeInfo`, or `toMsgScope`. They keep their current single `MemberSupport` variant. Comments are represented through `ChannelMsgInfo` rather than as a new scope variant, so the scope ADTs are untouched.

### 6. Smart constructor for `ChannelMsgInfo`

`src/Simplex/Chat/Store/Messages.hs`

A new function near `getGroupChatScopeForItem_` (~line 1477):

```haskell
getChannelMsgInfo :: DB.Connection -> User -> GroupId -> ChatItemId -> ExceptT StoreError IO ChannelMsgInfo
getChannelMsgInfo db user groupId parentChatItemId = do
  parent@(CChatItem _ ChatItem {meta = CIMeta {itemSharedMsgId}}) <-
    getGroupChatItem db user groupId parentChatItemId
  case itemSharedMsgId of
    Just sId -> pure ChannelMsgInfo {channelMsgItem = parent, channelMsgSharedId = sId}
    Nothing -> throwError $ SEChatItemNotFound parentChatItemId
```

The error case is unreachable for valid data — channel posts are always saved with a `SharedMsgId` by construction — and it explicitly drops malformed data on the floor instead of crashing. `getGroupChatItem` is the existing function at `Store/Messages.hs:3014` (NOT `getGroupChatItemById` — that does not exist).

`getCreateGroupChatScopeInfo` (~line 1448) is unchanged. The smart-constructor pattern is parallel but separate: the scope info builder builds `GCSIMemberSupport`, the channel-msg builder builds `ChannelMsgInfo`, and they are composed by the caller.

### 7. `getGroupChat` and pagination signature changes

`src/Simplex/Chat/Store/Messages.hs:1434` `getGroupChat`:

```haskell
getGroupChat
  :: DB.Connection
  -> VersionRangeChat
  -> User
  -> Int64
  -> Maybe GroupChatScope
  -> Maybe ChatItemId            -- NEW: parent chat item id for comments thread
  -> Maybe MsgContentTag
  -> ChatPagination
  -> Maybe Text
  -> ExceptT StoreError IO (Chat 'CTGroup, Maybe NavigationInfo)
```

When `Maybe ChatItemId` is `Just parentId`, the function:
1. Calls `getChannelMsgInfo db user groupId parentId` to build the `ChannelMsgInfo`.
2. Embeds it into the returned `Chat`'s `ChatInfo.GroupChat _ _ (Just channelMsgInfo)`.
3. Passes it through to `getChatItemIDs` for filtering.

The `Maybe GroupChatScope` and `Maybe ChatItemId` parameters are mutually exclusive at the call site — only one is `Just _` for any given query. The function asserts this with a clear error if both are set (defensive).

`getChatItemIDs` at ~line 1506:

```haskell
getChatItemIDs
  :: DB.Connection
  -> User
  -> ChatInfo c
  -> Maybe MsgContentTag
  -> ChatItemIDsRange
  -> Int
  -> Text
  -> ExceptT StoreError IO [ChatItemId]
```

The signature does NOT change — the new `ChannelMsgInfo` is already inside `ChatInfo` (the third constructor parameter on `GroupChat`). The function pattern-matches on it:

```haskell
GroupChat GroupInfo {groupId} scopeInfo_ channelMsgInfo_ -> case (scopeInfo_, channelMsgInfo_, contentFilter) of
  (Nothing, Nothing, Nothing) -> ... existing main-chat arm ...
  (Nothing, Nothing, Just MCReport_) -> ... existing reports arm ...
  (Just GCSIMemberSupport {groupMember_}, Nothing, Nothing) -> ... existing member-support arm ...
  (Nothing, Just ChannelMsgInfo {channelMsgItem = CChatItem _ ChatItem {meta = CIMeta {itemId = parentId}}}, Nothing) ->
    liftIO $ idsQuery
      (grCond <> " AND parent_chat_item_id = ? ")
      (userId, groupId, parentId)
      "item_ts"
  _ -> pure []
```

The new arm filters chat items by `parent_chat_item_id = ?`. Existing arms MUST filter by `parent_chat_item_id IS NULL` — this is NOT automatic; the existing `grCond` does not include this predicate today, so Slice 3 explicitly adds `AND parent_chat_item_id IS NULL` to `grCond` and to every other default-scope SELECT. See "Insert path" section below for the enumeration of sites.

### 8. Insert path: `createNewChatItem_` extension

**Critical: this is the writer that persists every chat item.** Located at `Store/Messages.hs:581-624`.

The current code has a `groupScopeRow` 2-tuple (~line 621) for the existing scope columns. We add a new local binding for the parent-chat-item-id column:

```haskell
parentChatItemId :: Maybe ChatItemId
parentChatItemId = case groupChannelMsg of
  Just ChannelMsgInfo {channelMsgItem = CChatItem _ ChatItem {meta = CIMeta {itemId}}} -> Just itemId
  Nothing -> Nothing
```

`groupChannelMsg` is a new parameter to `createNewChatItem_` of type `Maybe ChannelMsgInfo`, threaded from the two wrappers `createNewSndChatItem` and `createNewRcvChatItem` (which in turn get it from the `ChatDirection`-bearing call sites). Both wrappers gain a new parameter.

**INSERT statement extension** (~line 588): add `parent_chat_item_id` to the column list immediately after `group_scope_group_member_id`. Increment the placeholder count from 39 to 40 `?` marks. Pass `parentChatItemId` in the parameter tuple at the corresponding position.

**Increment the parent's `comments_total`** in the same transaction, immediately after the INSERT:

```haskell
case parentChatItemId of
  Just pId -> adjustChannelMsgCommentCount db pId 1
  Nothing -> pure ()
```

This is a single guarded call site that covers both `createNewSndChatItem` and `createNewRcvChatItem` without duplication.

**`includeInHistory`** (`Store/Messages.hs:625-628`) admits comment items so they appear in joiner history:

```haskell
includeInHistory = case (groupScope, parentChatItemId) of
  (Just Nothing, Nothing) -> isJust mcTag_ && mcTag_ /= Just MCReport_   -- main channel
  (_,            Just _)  -> isJust mcTag_ && mcTag_ /= Just MCReport_   -- comment
  _                       -> False
```

The per-parent comment cap is enforced at query time, not at insert time — see "History playback" below.

**Default-scope SELECT queries** at `Store/Messages.hs:891, 904, 1202, 1512, 1733` etc. currently filter by `group_scope_tag IS NULL AND group_scope_group_member_id IS NULL`. They MUST be extended to also filter by `parent_chat_item_id IS NULL`, otherwise comments would leak into the main channel chat. The required SQL fragment:

```sql
... AND group_scope_tag IS NULL AND group_scope_group_member_id IS NULL AND parent_chat_item_id IS NULL ...
```

Each of these sites must be audited and updated. Comments do not have a scope tag — the dedicated `parent_chat_item_id` column is the sole discriminator — so every default-scope SELECT must add the explicit `IS NULL` predicate.

**Read-side plumbing in `toGroupChatItem`** (`Store/Messages.hs:2334`) and any other chat-item SELECT that reads chat-item rows: extend the row tuple to include `parent_chat_item_id`, `comments_total`, `comments_disabled`. These three fields propagate into the `CIMeta` record as new fields with default values. Parent chat-item id is exposed via a new optional `CIMeta` field; the comment count and disabled flag are read directly into the existing meta record.

### 9. `CIMeta` extensions and JSON forward compat

`src/Simplex/Chat/Messages.hs` — add three new fields to `CIMeta`:

```haskell
data CIMeta c d = CIMeta
  { ... existing fields ...,
    parentChatItemId :: Maybe ChatItemId,   -- non-Nothing on comment rows
    commentsTotal :: Int,                    -- 0 by default; non-zero on parent posts
    commentsDisabled :: Bool                 -- False by default; True on parent posts where commenting is locked
  }
```

JSON forward compatibility (per `docs/CONTRIBUTING.md`): each new field MUST have an `omittedField` default in the `FromJSON` instance:

- `parentChatItemId` → `omittedField = Just Nothing`
- `commentsTotal` → `omittedField = Just 0`
- `commentsDisabled` → `omittedField = Just False`

This ensures older remote-connection clients see chat items normally and do not fall back to `CInfoInvalidJSON`.

### 10. Send path

`src/Simplex/Chat/Library/Commands.hs` — `APISendMessages` already takes `Maybe GroupChatScope`. Add a parallel `Maybe ChatItemId` parameter for the parent post id when sending a comment. The same field also flows through the `composedMessage` tuple if needed. The send command body:

1. Resolves the parent into `Maybe ChannelMsgInfo` via `withFastStore $ \db -> mapM (getChannelMsgInfo db user groupId) parentId_`.
2. Builds the `MsgContainer` via the smart constructors. For a comment, the constructed container has `parent = Just (toMsgRef channelMsgInfo)` — see helper below.
3. Calls `prohibitedGroupContent gInfo m channelMsgInfo_ scopeInfo mc ft file_ True` (see "prohibitedGroupContent extension" below).
4. Calls `commentsClosed gInfo channelMsgInfo_ now` (see "commenting-window helper" below) and rejects with `CECommandError "commenting closed on this post"` if true.
5. Threads the resolved `ChannelMsgInfo` into `saveSndChatItems` so it ends up in `createNewChatItem_` and the comment row gets the parent linkage.

**`toMsgRef` helper** in `Protocol.hs` (or `Messages.hs`):

```haskell
toMsgRef :: ChannelMsgInfo -> MsgRef
toMsgRef ChannelMsgInfo {channelMsgItem = CChatItem _ ChatItem {chatDir, meta = CIMeta {itemTs}}, channelMsgSharedId} =
  MsgRef
    { msgId = Just channelMsgSharedId,
      sentAt = itemTs,
      sent = isSndCIDir chatDir,
      memberId = chatDirMember_ chatDir
    }
```

`isSndCIDir` and `chatDirMember_` are existing helpers (or trivially defined inline for the channel-direction cases).

**`allowedRole`** in `Commands.hs:4170-4176` extension:

```haskell
allowedRole = case (scope, channelMsgInfo_) of
  (Nothing, Nothing) -> Just GRAuthor
  (Just (GCSMemberSupport Nothing), Nothing)
    | memberPending membership -> Nothing
    | otherwise -> Just GRObserver
  (Just (GCSMemberSupport (Just _gmId)), Nothing) -> Just GRModerator
  (Nothing, Just _) -> Just GRCommenter
  _ -> Nothing  -- mutually exclusive: scope and channelMsgInfo cannot both be Just
```

### 11. Receive path

`src/Simplex/Chat/Library/Subscriber.hs` — `newGroupContentMessage` (~line 1943).

The current code resolves a chat scope via `mkGetMessageChatScope vr user gInfo m content msgScope_`. The parent reference for a comment lives in the message container (`mc.parent :: Maybe MsgRef`), not in the scope. The receive flow becomes:

> **Notation note.** `mc.parent`, `mc.quote`, `mc.forward` in this section are prose shorthand for "the `parent`/`quote`/`forward` field of `mc`". `Subscriber.hs` does **not** enable `OverloadedRecordDot` and `DuplicateRecordFields` disables bare field selectors as functions, so the implementation MUST use explicit record patterns (`case mc of MsgContainer {parent = Just p, quote = q, forward = f} -> ...`) or a local `let MsgContainer {parent, quote, forward} = mc in ...` binding. Dot syntax will not compile in `Subscriber.hs`.

1. Existing `mkGetMessageChatScope` resolves `Maybe GroupChatScopeInfo` from `msgScope_` (only `MSMember` arm — unchanged).
2. New step: if `mc.parent = Just MsgRef {msgId = Just parentSharedId}`, look up the parent post in this group:
   ```haskell
   parent <- withStore $ \db -> getGroupChatItemBySharedMsgId db user gInfo Nothing parentSharedId
   pure (Just ChannelMsgInfo {channelMsgItem = parent, channelMsgSharedId = parentSharedId})
   ```
   The `Nothing` for `groupMemberId_` (the second argument) is correct — channel comments reference posts by `SharedMsgId` only; the parent's authoring member is not part of the lookup.
3. If lookup fails (`SEChatItemNotFound`), the message is dropped via `messageError "channel comment parent not found"`.
4. The resolved `Maybe ChannelMsgInfo` is threaded through to `prohibitedGroupContent` (see below) and to `saveRcvChatItem*`, where it ends up in `createNewChatItem_` and the comment row gets the parent linkage.

**Important guards on `mc.parent`**:
- If `mc.parent = Just MsgRef {msgId = Nothing}` — the parent SharedMsgId is missing — the message is dropped (no fallback).
- If the group is not a channel (`not (useRelays' gInfo)`), the message is converted to a feature-rejected item via the `rejected` helper (the new `GFComments` arm in `prohibitedGroupContent` handles this).
- If `mc.parent = Nothing`, the receive path is unchanged from current behavior.

**`memberCanSend`** in `Subscriber.hs:1519-1524` — TWO changes:

1. **Role-comparison fix in the `Nothing` arm**: inserting `GRCommenter` between `GRObserver` and `GRAuthor` makes `GRCommenter > GRObserver` true. The current code grants non-comment send rights based on `memberRole > GRObserver`, which would silently grant commenters non-comment send rights. Change to `memberRole >= GRAuthor`.
2. **New comment arm**: gate by the presence of `mc.parent` in the *content*, not by a scope tag. Since `memberCanSend` currently takes `msgScope :: Maybe MsgScope` as the second argument, change the signature to also accept the parent field — or, simpler, extract a new helper `memberCanComment :: Maybe GroupMember -> CM (Maybe a) -> CM (Maybe a)` and call it from the receive flow when `mc.parent = Just _`.

   Cleanest implementation: leave `memberCanSend` shape unchanged for the existing call sites (which only check the `MSMember` scope), and add a parallel guard `memberCanComment m a = if memberRole m >= GRCommenter then a else messageError "member is not allowed to comment" $> Nothing` invoked from `newGroupContentMessage` immediately after `mc.parent` is unpacked.

```haskell
memberCanSend :: Maybe GroupMember -> Maybe MsgScope -> CM (Maybe a) -> CM (Maybe a)
memberCanSend (Just m@GroupMember {memberRole}) msgScope a = case msgScope of
  Just MSMember {} -> a
  Nothing
    | memberRole >= GRAuthor || memberPending m -> a   -- was: memberRole > GRObserver
    | otherwise -> messageError "member is not allowed to send messages" $> Nothing
memberCanSend Nothing _ a = a

memberCanComment :: Maybe GroupMember -> CM (Maybe a) -> CM (Maybe a)
memberCanComment (Just GroupMember {memberRole}) a
  | memberRole >= GRCommenter = a
  | otherwise = messageError "member is not allowed to comment" $> Nothing
memberCanComment Nothing a = a
```

The receive flow composes them: `memberCanSend author scope (memberCanComment author (newGroupContentMessage ...))` when the message has `parent`, otherwise just `memberCanSend ... newGroupContentMessage`.

### 12. `prohibitedGroupContent` extension

`src/Simplex/Chat/Library/Internal.hs:341` — current signature:

```haskell
prohibitedGroupContent
  :: GroupInfo
  -> GroupMember
  -> Maybe GroupChatScopeInfo
  -> MsgContent
  -> Maybe MarkdownList
  -> Maybe f
  -> Bool
  -> Maybe GroupFeature
```

Add a `Maybe ChannelMsgInfo` parameter (after `Maybe GroupChatScopeInfo`) and a new arm at the bottom:

```haskell
prohibitedGroupContent gInfo m scopeInfo channelMsgInfo mc ft file_ sent
  | ... existing arms unchanged ...
  | otherwise = case channelMsgInfo of
      Just ChannelMsgInfo {channelMsgItem = CChatItem _ ChatItem {meta = CIMeta {itemDeleted, commentsDisabled}}}
        | not (useRelays' gInfo)                      -> Just GFComments
        | not (groupFeatureAllowed SGFComments gInfo) -> Just GFComments
        | isJust itemDeleted                          -> Just GFComments
        | commentsDisabled                            -> Just GFComments
        | otherwise                                   -> Nothing
      Nothing -> Nothing
```

The function uses record-pattern syntax (`channelMsgItem = CChatItem _ ChatItem {meta = CIMeta {...}}`) so the second field `channelMsgSharedId` is bound by wildcard and ignored. `useRelays'` guards against a non-channel group locally constructing a `ChannelMsgInfo`.

The deleted-parent case is now an explicit reject (not a fall-through), so a soft-deleted parent post stops accepting comments.

The commenting-window check (which needs the wall clock) is **NOT** in `prohibitedGroupContent` — that function does not have a `UTCTime` parameter today and adding one would inflate the diff. Instead, a new helper:

```haskell
commentsClosed :: GroupInfo -> Maybe ChannelMsgInfo -> UTCTime -> Bool
commentsClosed
  GroupInfo {fullGroupPreferences = FullGroupPreferences {comments = CommentsGroupPreference {closeAfter}}}
  (Just ChannelMsgInfo {channelMsgItem = CChatItem _ ChatItem {meta = CIMeta {itemTs}}})
  now =
    case closeAfter of
      Just secs -> diffUTCTime now itemTs > fromIntegral secs
      Nothing -> False
commentsClosed _ Nothing _ = False
```

The destructure uses record patterns rather than record-selector function calls, because `Internal.hs` enables `DuplicateRecordFields` but not `OverloadedRecordDot`.

**Send side** (`sendGroupContentMessages_` in `Commands.hs`): after `prohibitedGroupContent`, fetch `now` and reject if `commentsClosed gInfo channelMsgInfo_ now`.

**Receive side** (`newGroupContentMessage` in `Subscriber.hs`): after the parent is resolved into `Maybe ChannelMsgInfo`, fetch `now` and drop the message via `messageError "commenting closed on channel post"` if `commentsClosed gInfo channelMsgInfo_ now`.

### 13. Per-message disable

**New chat command** in `Commands.hs`:

```haskell
APISetCommentsDisabled GroupId ChatItemId Bool
```

**Handler**:

1. Load the channel post via `getGroupChatItem db user groupId chatItemId`. Assert that `useRelays' gInfo` (the group is a channel) and that the item has a `SharedMsgId` (only items with a SharedMsgId can be addressed by comments). There is no separate `CIChannelSnd` constructor in `Messages.hs:291-293` — `CIChannelRcv` is the only channel-specific direction; the owner's own outgoing channel posts reuse `CIGroupSnd`, and `asGroup` is a wire-only field on the message container that is not separately persisted on chat items (see `Protocol.hs:735, 908-909`). For the MVP the simpler `SharedMsgId`-presence check is sufficient: it accepts every legitimate channel post (both subscriber-received `CIChannelRcv` and owner-sent `CIGroupSnd`) and rejects rows with no shared id (e.g. local-only items, system messages).
2. Assert the user role is `>= GRModerator`.
3. Update `chat_items.comments_disabled` for the post row via a new helper `setChannelMsgCommentsDisabled db chatItemId disabled`.
4. Send a new chat event `XGrpCommentsDisabled parentSharedMsgId disabled` via the relay's standard `sendGroupMessages` path.
5. Return `CRChatItemUpdated` so iOS state reconciliation works.

**Handler for incoming `XGrpCommentsDisabled`** in `Subscriber.hs`, alongside `xGrpMsgUpdate` / `xGrpMsgDelete`:

```haskell
xGrpCommentsDisabled
  :: GroupInfo
  -> GroupMember
  -> SharedMsgId
  -> Bool
  -> CM ()
xGrpCommentsDisabled gInfo m@GroupMember {memberRole} parentSharedMsgId disabled =
  if memberRole < GRModerator
    then messageError "member is not allowed to disable comments"
    else do
      parent <- withStore $ \db ->
        getGroupChatItemBySharedMsgId db user gInfo Nothing parentSharedMsgId
      withStore' $ \db ->
        setChannelMsgCommentsDisabled db (cChatItemId parent) disabled
      toView $ CEvtChatItemUpdated user (AChatItem ...)
```

Plumb through `chatMsgEventTag` / `appJsonToCM` / `cmToAppMessage` and through `processForwardedMsg` for relay forwarding.

### 14. Comment count maintenance

A small helper in `Store/Messages.hs`:

```haskell
adjustChannelMsgCommentCount :: DB.Connection -> ChatItemId -> Int -> IO ()
adjustChannelMsgCommentCount db parentChatItemId delta =
  DB.execute db
    "UPDATE chat_items SET comments_total = MAX(0, comments_total + ?) WHERE chat_item_id = ?"
    (delta, parentChatItemId)
```

Call sites:

- **Insert**: inside `createNewChatItem_`, immediately after the INSERT, gated on `parentChatItemId = Just _`. See "Insert path" above.
- **Delete**: in `deleteChatItemBy*` and `markChatItemDeleted`, before issuing the delete/mark, fetch the row's `parent_chat_item_id`. If non-NULL and the row was previously not deleted, call `adjustChannelMsgCommentCount db parentId (-1)`.
- **Edit**: no change to the count.
- **Reactions**: no change to the count.

A second moderator delete on an already-deleted comment must NOT decrement again — wrap the decrement in a check that the row's `item_deleted` was previously NULL.

The `MAX(0, ...)` clamp guards against transient negative counts under concurrent deletes; combined with transactional guarding, the count remains a safe upper bound on live comments at all times.

Cascade behavior: when a parent post is hard-deleted, the FK cascade removes all child comment rows in a single SQLite cascade. The parent's `comments_total` value disappears with the parent row — there is no count drift.

### 15. History playback for new subscribers

`getGroupHistoryItems` (`Store/Messages.hs:3656`) replays recent channel posts to a new joiner. Extend this so that for each replayed post, up to `M` most-recent non-deleted comments under that post are also replayed.

Two pieces:

1. **`includeInHistory`** (covered in "Insert path" above) admits comment items so they are persisted with `include_in_history = 1`.
2. **`getChannelMsgCommentsForHistory`** — new helper next to `getGroupHistoryItems`:
   ```haskell
   getChannelMsgCommentsForHistory
     :: DB.Connection
     -> UserId
     -> GroupId
     -> ChatItemId
     -> Int  -- M, max comments per parent
     -> IO [CChatItem 'CTGroup]
   ```
   Uses the new `idx_chat_items_parent_item_ts` index, ranked by `item_ts` desc, limited to `M`, applying the same soft-delete and report filters that the main history query already applies.

`getGroupHistoryItems` iterates over the post ids it just selected, fetches comments per post, and merges them into the replay stream in `item_ts` order.

`M` and the existing post-window cap `N` are tunable constants chosen at implementation time.

Older joiners on a chat protocol version below `commentsVersion` cannot deserialize a `MsgContainer` with a `parent` field (technically they CAN deserialize it — the field is optional — but they would not understand it, and the relay's per-recipient version gate drops the comment items from their replay stream entirely so they receive the post-only history unchanged from today).

**`commentsTotal` consistency for new joiners.** A new joiner receives the parent post row with its current `commentsTotal`, while only `M` of the comments are included in the initial replay. The Comments view fetches additional items on demand via the existing pagination path; this is documented as expected UX, not a correctness issue.

### 16. CLI / API surface (Haskell)

**`Commands.hs`** changes:

- New command `APISetCommentsDisabled groupId chatItemId disabled` — handler defined above.
- `APISendMessages` gains an optional parent chat item id (parsed from a new prefix syntax — see below).
- `APIGetChat` parser must accept a parent chat item id, mirroring how it accepts a scope. For example, the existing `/_get chat #N count=K` parser currently accepts `scope=member_support` and `scope=member_support/<gmId>`; add a parallel `parent=<chatItemId>` token that maps to the new `Maybe ChatItemId` parameter.
- The `comments` group preference is set/cleared via the existing `APISetGroupPreference` path (`GroupPreferences` already round-trips an arbitrary preference set); no new preference command is needed.

**Pretty-printed CLI shorthand for tests** (added to the existing `chatCommand` parser):

- `bob #!> #team <itemId> hello back` — comment on the channel post `<itemId>`. Pseudo-syntax for `/_send #team(parent=<itemId>) text hello back`.
- `/_get chat #team count=5 parent=<itemId>` — paginate the comments thread for the parent post `<itemId>`.
- `alice #disable_comments #team <itemId>` — owner disables comments on a post.

The exact CLI grammar lives in the existing `chatCommand` parser; this is a small additive parse rule.

### 17. Group preference

`src/Simplex/Chat/Types/Preferences.hs`

Add `CommentsGroupPreference` mirroring `TimedMessagesGroupPreference`:

```haskell
data CommentsGroupPreference = CommentsGroupPreference
  { enable :: GroupFeatureEnabled,
    closeAfter :: Maybe Int  -- seconds since post creation; Nothing = never close
  }
  deriving (Eq, Show)
```

Mechanical extension across the file (~22 sites, the same set every other group preference touches):

- `GroupFeature` and `SGroupFeature` add `GFComments` / `SGFComments`
- `groupFeatureNameText` adds the localized name `"Comments"`
- `allGroupFeatures` includes it
- `groupPrefSel` / `toGroupFeature` map between the singleton and the selector
- `GroupPreferences` and `FullGroupPreferences` add the optional / non-optional field
- `setGroupPreference_` extends the merge logic
- `GroupPreferenceI FullGroupPreferences` instance
- `defaultGroupPrefs` / `emptyGroupPrefs` / `defaultBusinessGroupPrefs` (default `enable = FEOff` for backward-compatible groups; default `FEOn` only for newly created channel groups — see `defaultGroupPrefs` callsites in `Store/Groups.hs`)
- `HasField "enable"` for the singleton
- `GroupFeatureI` (or `GroupFeatureNoRoleI` — comments has no role on the preference; the role gating is the separate `GRCommenter` mechanism)
- `groupParamText_` renders the `closeAfter` duration the same way `TimedMessagesGroupPreference` renders its `ttl`
- `toGroupPreferences` / `mergeGroupPreferences`
- `deriveJSON` and explicit `omittedField` returning `Just CommentsGroupPreference {enable = FEOff, closeAfter = Nothing}` for forward compat

`Store/Groups.hs` — when a channel is created with `useRelays = True`, `defaultGroupPrefs` for that case sets `comments = CommentsGroupPreference {enable = FEOn, closeAfter = Nothing}`. Locate the channel-creation prefs branch (the same place that defaults `directMessages` to off and so on for channels).

### 18. Role

`src/Simplex/Chat/Types/Shared.hs`

```haskell
data GroupMemberRole
  = GRUnknown Text
  | GRRelay
  | GRObserver
  | GRCommenter -- new
  | GRAuthor    -- still reserved, unused
  | GRMember
  | GRModerator
  | GRAdmin
  | GROwner
  deriving (Eq, Show, Ord)
```

Wire encoding via the existing `TextEncoding GroupMemberRole`: add `"commenter"` ↔ `GRCommenter`.

`src/Simplex/Chat/Controller.hs:160` `channelSubscriberRole` default changes from `GRObserver` to `GRCommenter`.

`src/Simplex/Chat/Library/Commands.hs:5193` — the role parser:
```haskell
memberRole =
  A.choice
    [ " owner" $> GROwner,
      " admin" $> GRAdmin,
      " moderator" $> GRModerator,
      " member" $> GRMember,
      " commenter" $> GRCommenter, -- new, between member and observer
      " observer" $> GRObserver
    ]
```

The down migration of `M20260222_chat_relays` already maps `member_role = 'relay'` back to `'observer'`. Add a parallel down step in the new `M20260501_channel_comments` migration that maps `'commenter'` back to `'observer'` to preserve send-permission semantics on downgrade.

### 19. Delivery worker context

`src/Simplex/Chat/Delivery.hs:106` `infoToDeliveryContext` — comments fan out via the existing `DWSGroup` worker (the same path that handles channel posts). The function does not need a new arm because comments live in the main `ChatInfo.GroupChat` (with `channelMsgInfo` set), and the existing main-chat dispatch covers them. The third constructor parameter on `GroupChat` is ignored by `infoToDeliveryContext`. No code change here.

### 20. iOS API types

`apps/ios/SimpleXChat/ChatTypes.swift`:

1. **New struct** `ChannelMsgInfo`:
   ```swift
   public struct ChannelMsgInfo: Decodable, Hashable {
       public var channelMsgItem: ChatItem
       public var channelMsgSharedId: String  // SharedMsgId is base64 string
   }
   ```

2. **`ChatInfo.group` extension** (line 1376):
   ```swift
   case group(groupInfo: GroupInfo, groupChatScope: GroupChatScopeInfo?, channelMsgInfo: ChannelMsgInfo?)
   ```
   Every existing pattern match in this file (~25 sites) gains the new `_` for the third associated value. The mechanical extension follows the same shape as the existing `groupChatScope: _` arms.

3. **`GroupChatScope`** (line 1905) — UNCHANGED. Keep `.memberSupport` and `.reports` only. NO `.channelMsg` case.

4. **`ChatItem` decoding** — add three new optional fields with defaults in the manual `Decodable` init:
   ```swift
   public var parentChatItemId: Int64? = nil
   public var commentsTotal: Int = 0
   public var commentsDisabled: Bool = false
   ```
   Decoded via `try container.decodeIfPresent(...)` with the documented default. Forward compat: older clients that lack these fields see them as nil/0/false.

5. **`ChatItem` computed property**:
   ```swift
   public var isChannelPost: Bool {
       // Subscriber view: chatDir == .channelRcv always identifies a received channel post.
       // Owner-side (viewing their own outgoing channel post) is NOT covered here; call
       // sites must compose `groupInfo.useRelays && chatItem.chatDir == .groupSnd` explicitly.
       if case .channelRcv = chatDir { return true }
       return false
   }
   ```
   The owner-side check (owner viewing their own outgoing channel post) is composed at the call site as `groupInfo.useRelays && chatItem.chatDir == .groupSnd`, NOT inside `isChannelPost`. The property only covers the subscriber's receive side. (`isOwnerSnd` is not an existing property of `ChatItem` — verified absent in `apps/ios/`.) Section 22's qualifier on the Comments button (`parent.chatDir == .groupSnd`) uses the same form.

6. **`ChatInfo.channelMsgInfo()` helper method** — parallel to the existing `ChatInfo.groupChatScope()` method:
   ```swift
   public func channelMsgInfo() -> ChannelMsgInfo? {
       switch self {
       case let .group(_, _, channelMsgInfo): channelMsgInfo
       default: nil
       }
   }
   ```
   Used by `getCIItemsModel` (section 21) and by view-layer composer/toolbar gating (section 22).

`apps/ios/Shared/Model/AppAPITypes.swift`: new `ChatCommand` case `.apiSetCommentsDisabled(groupId:chatItemId:disabled:)` and its serialization. The existing `.apiSendMessages` case gains an optional `parent: Int64?` parameter that serializes to `parent=<id>` in the wire command.

`apps/ios/Shared/Model/SimpleXAPI.swift`:
- `apiSendMessages` (line 539) — extend to take an optional `parent: Int64? = nil`. Serializes as `parent=<id>` in the wire command. Default `nil` keeps existing call sites unchanged.
- `apiGetChat` (or whichever function calls `/_get chat`) — extend to take an optional `parent: Int64? = nil`.
- New: `apiSetCommentsDisabled(_ groupId: Int64, _ chatItemId: Int64, _ disabled: Bool) async throws -> ChatItem` calling the new `ChatCommand` case and decoding the updated parent post.

### 21. iOS state model

`apps/ios/Shared/Model/ChatModel.swift:691` — the existing `getCIItemsModel(_ cInfo: ChatInfo, _ ci: ChatItem) -> ItemsModel?` is the iOS-side router that decides whether an inbound `ChatItem` belongs to the open secondary chat (member support, reports). It currently uses `cInfo.groupChatScope()` (a method on `ChatInfo`, not a stored field) and matches against `secondaryIM?.secondaryIMFilter` in a 2-tuple switch wrapped in `if let cInfoScope = cInfoScope`. The Comments view extends this routing by adding a parallel channel-msg branch.

A new helper method on `ChatInfo` (defined alongside `groupChatScope()` in `ChatTypes.swift`):

```swift
public func channelMsgInfo() -> ChannelMsgInfo? {
    switch self {
    case let .group(_, _, channelMsgInfo): channelMsgInfo
    default: nil
    }
}
```

`getCIItemsModel` is extended to consult both helpers. Because `cInfo.groupChatScope()` and `cInfo.channelMsgInfo()` are mutually exclusive at runtime (a `ChatInfo.group` is either the main channel, a member-support scope, or a comments thread for one parent post), the function checks them in order:

```swift
func getCIItemsModel(_ cInfo: ChatInfo, _ ci: ChatItem) -> ItemsModel? {
    let cInfoScope = cInfo.groupChatScope()
    let cInfoChannelMsg = cInfo.channelMsgInfo()
    if let cInfoScope = cInfoScope {
        // ... existing switch on (cInfoScope, secondaryIM?.secondaryIMFilter) unchanged ...
    } else if let cInfoChannelMsg = cInfoChannelMsg {
        switch secondaryIM?.secondaryIMFilter {
        case let .some(.groupChannelMsgContext(parentFilter)):
            // Comments view open: route the item to the secondary IM iff the chat id
            // matches AND the local parent id matches the open thread's parent id.
            return (cInfo.id == chatId && cInfoChannelMsg.channelMsgItem.id == parentFilter.id) ? secondaryIM : nil
        default:
            return nil
        }
    } else {
        return cInfo.id == chatId ? im : nil
    }
}
```

`SecondaryIMFilter` (the local filter type used by the model) gains a new constructor `groupChannelMsgContext(parent: ChatItem)`. The matcher compares the parent's local `ChatItem.id` (NOT the SharedMsgId — the local id is unambiguous within a single client and is what `cInfo.channelMsgInfo()?.channelMsgItem.id` exposes).

The four parallel call sites at `ChatModel.swift:657`, `679`, `717`, `788` that gate behavior on `cInfo.groupChatScope() == nil` (i.e. "this is the main chat, not a secondary scope") need analogous treatment for comment items: a comment item must NOT update the main channel's chat-list preview, NOT add to its unread count, etc. Each call site is audited and changed from `cInfo.groupChatScope() == nil` to `cInfo.groupChatScope() == nil && cInfo.channelMsgInfo() == nil` so that comment items take the same code path as member-support items (i.e. they do NOT touch the main chat preview).

When a `ChatEvent` updates a parent post (e.g. `commentsTotal` changes after a new comment, or `commentsDisabled` is toggled), the parent post arrives as a normal main-chat item (its own `cInfo.channelMsgInfo() == nil`) and the model's existing `upsertChatItem` path reconciles the parent's fields automatically since they are part of `ChatItem` JSON.

When a new comment `ChatItem` arrives, its containing `ChatInfo.group` carries the `channelMsgInfo` set to the parent post; `cInfo.channelMsgInfo()?.channelMsgItem.id` matches the open Comments view's filter and the comment is appended to the open thread via the secondary IM.

`ItemsModel.loadSecondaryChat(...)` gains a new branch that, for `groupChannelMsgContext(parent)`, calls `apiGetChat(type: .group, id: groupId, scope: nil, parent: parent.id, pagination: ..., search: nil)` and stores the resulting `Chat` in the secondary IM slot.

### 22. iOS view layer

**Comments button on the channel post bubble.**

The entry point is the channel post chat item view. Locate by searching `apps/ios/Shared/Views/Chat/ChatItem/` for `.channelRcv` (the only channel-specific chat-direction case in iOS — `ChatTypes.swift:3593`) or for `chatItem.isChannelPost`. The owner-side path reuses the existing `.groupSnd` case — there is no separate `.channelSnd` enum case in `ChatTypes.swift`, and the wire-format `asGroup` flag is not persisted as a separate column on chat items (see `Protocol.hs:735, 908-909`). The owner is identified by `groupInfo.useRelays && groupInfo.membership.memberRole >= .owner` rather than by inspecting `asGroup` on the persisted item. The most likely host is `FramedItemView.swift` or `CIMetaView.swift`, which already render the per-item meta row.

Add a small `CommentsButton` view that renders only on channel posts:

```swift
struct CommentsButton: View {
    let parent: ChatItem
    let openComments: (ChatItem) -> Void
    var body: some View {
        Button {
            openComments(parent)
        } label: {
            HStack(spacing: 4) {
                Image(systemName: "bubble.left")
                Text(parent.commentsTotal > 0
                     ? String.localizedStringWithFormat(NSLocalizedString("comments %d", comment: ""), parent.commentsTotal)
                     : NSLocalizedString("comments", comment: ""))
            }
            .font(.caption)
            .foregroundStyle(.secondary)
        }
    }
}
```

The qualifier for showing the button: `groupInfo.useRelays && (parent.isChannelPost || (groupInfo.membership.memberRole >= .owner && parent.chatDir == .groupSnd))` — i.e. either the user receives the post as a subscriber OR the user is the channel owner viewing their own outgoing post.

**Hidden NavigationLink in `ChatView`.**

`apps/ios/Shared/Views/Chat/ChatView.swift` gains:

```swift
@State private var commentsParent: ChatItem?
```

In the body, alongside the existing chat scrollview, add a hidden NavigationLink that activates when `commentsParent` is set:

```swift
ZStack {
    NavigationLink(
        isActive: Binding(
            get: { commentsParent != nil },
            set: { active in if !active { commentsParent = nil } }
        )
    ) {
        if let parent = commentsParent,
           let sharedId = parent.meta.itemSharedMsgId,
           case let .group(groupInfo, _, _) = chat.chatInfo
        {
            SecondaryChatView(chat: Chat(
                chatInfo: .group(
                    groupInfo: groupInfo,
                    groupChatScope: nil,
                    channelMsgInfo: ChannelMsgInfo(
                        channelMsgItem: parent,
                        channelMsgSharedId: sharedId
                    )
                ),
                chatItems: [],
                chatStats: ChatStats()
            ))
            .onAppear {
                ItemsModel.loadSecondaryChat(
                    chat.id,
                    parent: parent
                )
            }
        }
    } label: { EmptyView() }
    .opacity(0)
    .frame(width: 0, height: 0)

    chatItemsList(...)  // existing body
}
```

The pattern is identical to `NavStackCompat`'s hidden NavigationLink trick (line 21-30 of `NavStackCompat.swift`) and to the `UserSupportChatNavLink` / `GroupReportsChatNavLink` patterns in `GroupChatInfoView.swift`. The difference: those nav links live on info screens; this one lives directly on `ChatView` so the comments view pushes onto the chat's existing nav stack (the user can navigate Channel → Comments → back → another post's Comments without losing context).

**iOS 16+ deprecation note.** `NavigationLink(isActive:)` is deprecated on iOS 16 in favor of `NavigationStack` + value-based navigation (`.navigationDestination(isPresented:)`). However, the existing codebase uses `NavigationLink(isActive:)` throughout — most visibly in `NavStackCompat.swift`, `UserSupportChatNavLink`, and `GroupReportsChatNavLink`. The project's minimum deployment target still supports the pre-iOS-16 API, and mixing new-style `NavigationStack` into a project whose existing navigation uses the legacy `NavigationView` would break both patterns. For consistency with the rest of the codebase, the Comments view uses the same legacy `NavigationLink(isActive:)` pattern. A later whole-codebase migration to `NavigationStack` is a separate, out-of-scope change. Suppressing the deprecation warning locally with `@available` is preferable to introducing a new navigation style just for this feature.

A closure `openComments: (ChatItem) -> Void` is passed from `ChatView` down through the chat-items list and into each item view. When the user taps the Comments button on a post bubble, the closure sets `commentsParent = parent`, the binding flips to `true`, and the hidden link pushes `SecondaryChatView`.

**Why not a sheet:** sheets break the stack-based navigation model — the user cannot navigate "deeper" from inside a sheet without modal-on-modal complexity, and the back-swipe gesture works differently. The hidden NavigationLink keeps the chat → comments transition consistent with the rest of the app's navigation.

**`SecondaryChatView` requires no change** — it already takes any `Chat` with any scope and renders it via `ChatView`. The only thing that's new is that the wrapped `Chat`'s `chatInfo.group` carries `channelMsgInfo` set.

**Toolbar.** `ChannelMsgChatToolbar.swift` (new, mirror of `MemberSupportChatToolbar.swift`): a small ~40-line view showing "Comments on:" with a 1-line preview of the parent post text. Wire into `ChatView`'s toolbar selector based on the chat's `channelMsgInfo` being non-nil.

**Owner controls.** On the channel post bubble, in the existing item context menu (long-press), add an "Disable comments" / "Enable comments" item, gated to `groupInfo.isOwner || groupInfo.isAdmin || groupInfo.isModerator`. Tapping it calls `apiSetCommentsDisabled` and updates the local state.

**Composer gating.** In `ChatView`'s composer, when the chat has `channelMsgInfo` set, the composer is disabled with a banner "Comments are closed" if any of: the parent's `commentsDisabled` is true; the group's comments preference is off; the post's age has exceeded the group's `closeAfter` window.

### 23. iOS three-layer documentation updates

Per `apps/ios/CODE.md` Change Protocol:

- New `apps/ios/spec/client/comments.md` describing the comments view, the API surface, and the state matching logic.
- Update `apps/ios/spec/client/chat-view.md` to describe the comments button, the hidden NavigationLink, and the navigation push to SecondaryChatView.
- Update `apps/ios/spec/api.md` with the new `apiSetCommentsDisabled` signature, the new `parent:` parameter on `apiSendMessages` and `apiGetChat`, and the `ChannelMsgInfo` type.
- Update `apps/ios/spec/state.md` with the new `getCIItemsModel` `channelMsgInfo()` branch, the new `ChatInfo.channelMsgInfo()` helper method, the audited gating sites at `ChatModel.swift:657, 679, 717, 788`, and the new `SecondaryIMFilter.groupChannelMsgContext` constructor.
- New `apps/ios/product/views/comments.md` describing the user-facing flow.
- Update `apps/ios/product/concepts.md` adding a "Comments" row.
- Update `apps/ios/product/glossary.md` defining "channel post comment".
- Update `apps/ios/product/rules.md` with the gating invariants (channels-only, role >= commenter, parent.commentsDisabled false, post age within `closeAfter` window).
- Update `apps/ios/spec/impact.md` to map the touched source files to the new product concept.
- Run the within-layer + across-layer review until two consecutive passes find zero issues.

## Implementation steps (slices)

Each slice ends with a build + test run that should pass.

### Slice 1 — Merged `MsgContainer` record (refactor only, no new behavior)

This slice is the largest mechanical refactor and is independent of comments. It can be merged before any of the comments work.

1. In `Protocol.hs`, replace the `MsgContainer` sum type and the separate `ExtMsgContent` record with the merged record. Add the `mc` base value and the four smart constructors `mcSimple`, `mcQuote`, `mcComment`, `mcForward`. Remove `mcExtMsgContent` and `isMCForward`.
2. Rewrite `parseMsgContainer` and `msgContainerJSON` per the parser/serializer rewrites above.
3. Update all 50 call sites across `Protocol.hs`, `Subscriber.hs`, `Internal.hs`, `Commands.hs`, `Messages.hs`. Each `MCSimple ec` becomes `(mcSimple content) {...}`; each pattern match becomes record-pattern dispatch.
4. Run `cabal build --ghc-options=-O0`. Fix any warnings about ambiguous fields by using explicit record patterns.
5. Run the JSON round-trip tests (`tests/JSONTests.hs`) and the protocol tests (`tests/ProtocolTests.hs`). Both must pass with zero diffs from the existing wire format.
6. Run the full chat test suite. Existing channel tests must continue to pass — there is no behavior change in this slice.

**Wire-format-preserving acceptance criteria for Slice 1 (ALL must hold before the slice lands):**

- `forward` is `Bool` (not `Maybe Bool`) in the record. Absent or `false` in the input JSON parses to `False`; `true` or any Object parses to `True`. The serializer emits `"forward": true` only when `True`, and emits nothing when `False`. (No `"forward": false` ever appears on the wire, matching the existing `msgContainerJSON` which only ever emits `("forward" .= True)` from the `MCForward` arm.)
- The serializer's content-field ordering (`file, ttl, live, mentions, scope, asGroup, content`) is preserved byte-for-byte — the rewritten implementation wraps the same `("file" .=? file) $ ("ttl" .=? ttl) $ ...` fold that `msgContent` uses today in `msgContainerJSON` at Protocol.hs:908-909.
- `nonEmptyMap` (Protocol.hs:911) is reused verbatim; empty `mentions` maps are omitted exactly as before.
- Discriminator fields are emitted at the front in the order: `quote` first (if present), then `parent` (if present), then `forward: true` (if true). The existing source enforces mutual exclusion via case-arms; after the refactor, at most one is present for any pre-`commentsVersion` message because no in-flight message has both `quote` and `parent` (MCComment is a dead constructor in the existing source).
- Every test in `tests/JSONTests.hs` that round-trips a `MsgContainer`, `ChatMsgEvent`, or `AppMessage` passes without modification. If a test had to be modified, the rewrite is wrong and must be revisited.
- Manual spot-check: take five representative in-tree sample JSON messages (a plain text, a quote, a forward-with-bool, a forward-with-object, a file send) from `tests/ProtocolTests.hs` fixtures and round-trip them through the new parser + serializer. The output bytes must be identical to the input (modulo field ordering inside a single JSON object — Haskell Aeson preserves insertion order, so the fold-based serializer produces identical ordering).
- The refactor does not touch `FromJSON`/`ToJSON` instances of `MsgContent`, `QuotedMsg`, `MsgRef`, `FileInvitation`, or `MsgScope`. Only the outer `MsgContainer` instances are rewritten.

### Slice 2 — DB migration, new types, role, preference (no new behavior)

1. New SQLite + Postgres migrations adding the three columns and two indexes.
2. New Haskell type `ChannelMsgInfo` in `Messages.hs`.
3. `ChatInfo.GroupChat` constructor extended to three parameters; mechanical update of every pattern match in the codebase to add `_` for the new third position. The compiler will list every site.
4. `ChatRef` gains `channelMsg_ :: Maybe ChatItemId`; existing call sites add `channelMsg_ = Nothing`.
5. `CIMeta` gains `parentChatItemId`, `commentsTotal`, `commentsDisabled` fields with `omittedField` JSON defaults. Plumb the three fields through every chat-item SELECT (`toGroupChatItem`, `toDirectChatItem`, etc.) — they're cheap fields and the existing chat-item SELECTs already enumerate every column individually.
6. `GroupMemberRole.GRCommenter` in `Types/Shared.hs` and its `TextEncoding`. Same commit: `Subscriber.hs:1522` `memberCanSend` `memberRole > GRObserver` → `memberRole >= GRAuthor`. This MUST land in the same commit as the role insertion to preserve send-permission semantics; otherwise commenters silently get non-comment send rights.
7. `Commands.hs:5193` role parser arm `" commenter" $> GRCommenter` — same commit so `/_member` and tests can address commenters.
8. `channelSubscriberRole` default change in `Controller.hs:160` from `GRObserver` to `GRCommenter`.
9. `CommentsGroupPreference` in `Types/Preferences.hs` (~22 mechanical sites). Includes the channel-creation default change in `Store/Groups.hs` so new channel groups default to `comments = CommentsGroupPreference {enable = FEOn, closeAfter = Nothing}` while non-channel groups default to `FEOff`.
10. Bump `commentsVersion :: VersionChat` in `Protocol.hs`, bump `currentChatVersion`.
11. Spot-check existing test fixtures: grep `tests/ChatTests/` for `"observer"` string assertions to confirm none of them depend on the default channel-subscriber role being `observer` (verified — the existing matches are all `/mr ... observer` member-role-change tests, not channel default-role tests).

Build: `cabal build --ghc-options=-O0`. Test: `cabal test simplex-chat-test --test-options='-m "channels"'` — must continue to pass.

### Slice 3 — Insert / read / pagination plumbing for `parent_chat_item_id`

1. **Critical insert-path extension** in `createNewChatItem_` (`Store/Messages.hs:581-624`): add the `groupChannelMsg :: Maybe ChannelMsgInfo` parameter, derive `parentChatItemId` from it, add `parent_chat_item_id` to the INSERT column list, increment placeholder count from 39 to 40. Both wrapper functions `createNewSndChatItem` and `createNewRcvChatItem` gain the new parameter.
2. **Increment** `comments_total` immediately after the INSERT, gated on `parentChatItemId = Just _`.
3. **`includeInHistory`** (`Store/Messages.hs:625-628`) admits comment items per the section above.
4. **Read-side plumbing** in `toGroupChatItem` (`Store/Messages.hs:2334`) and any other chat-item SELECT: extend the row tuple to include `parent_chat_item_id`, `comments_total`, `comments_disabled` and propagate into `CIMeta`.
5. **Default-scope SELECT predicates** at `Store/Messages.hs:891, 904, 1202, 1512, 1733` etc. — add `AND parent_chat_item_id IS NULL` to each. Audit every site: missing one would cause comments to leak into the main channel chat. The audit MUST cover not only item-list SELECTs but also:
   - **Unread-count aggregations**: every `SELECT COUNT(*)` or `SUM(...)` over `chat_items` that feeds `chatStats.unreadCount`, `unreadChat`, or chat-preview counters. A missed site would count comments toward the main channel's unread badge.
   - **Last-item / chat-preview queries**: every `ORDER BY item_ts DESC LIMIT 1` that selects the latest item for the chat-list preview. A missed site would show a comment as the channel's last-item preview, revealing comment content on the chat list.
   - **Marked-read queries**: every `UPDATE chat_items SET item_read = 1` WHERE clause that targets a chat — a missed site would silently mark comments as read when the main channel is opened.
   - **Delete-cascade queries**: `deleteChatItemsRange`, `deleteChatItems`, and the group-level clear operations — these must also either include `parent_chat_item_id IS NULL` (to preserve comments when clearing the main chat) or explicitly cascade to comments by the FK (which is already in place for hard deletes). The audit MUST decide per call site whether clearing the main channel chat should also clear all comments; the default behavior should match user expectation (clearing the main channel's view does NOT delete on-disk comments, which remain accessible via the post's Comments view until the post itself is deleted — at which point the FK cascade removes them).
   - **History-replay queries**: `getGroupHistoryItems` (covered separately in Slice 5) and any other helper that builds the joiner-history stream. Comments must be INCLUDED in history only when the `parent_chat_item_id` points to a post that is itself included, and must never be counted as main-chat posts.

   The audit is a grep-assisted read of every SQL string literal in `Store/Messages.hs` that references `chat_items`, plus every join through `chat_items` in `Store/Groups.hs`, `Store/Direct.hs`, and `Store/Files.hs`. Every SELECT/UPDATE/DELETE touching `chat_items` in a group context must be classified as "main-chat-only" (add the predicate), "explicitly includes comments" (leave as-is, add a code comment explaining why), or "irrelevant to groups" (leave as-is).
6. **`getGroupChat`** signature change: add `Maybe ChatItemId` parent argument; resolve via `getChannelMsgInfo` and embed into the returned `ChatInfo.GroupChat`.
7. **`getChatItemIDs`** new arm for `(Nothing, Just ChannelMsgInfo {...}, Nothing)` filtering by `parent_chat_item_id = ?`.
8. **`getChannelMsgInfo`** smart constructor in `Store/Messages.hs`.
9. **`getCreateGroupChatScopeInfo`** (`Store/Messages.hs:1448`) is unchanged.
10. **`getGroupChatScopeForItem_`** (`Store/Messages.hs:1477`) is unchanged — comments do not have a scope tag.
11. **`adjustChannelMsgCommentCount`** helper + guarded −1 in the deletion path (`deleteChatItemBy*` and `markChatItemDeleted`), only when the previous row was not already marked deleted.

Build + test as before.

### Slice 4 — Send / receive / governance plumbing

1. **`toMsgRef` helper** in `Protocol.hs` or `Messages.hs`.
2. **`prohibitedGroupContent` extension** with the new `Maybe ChannelMsgInfo` parameter and the new arm. Audit every call site (~7) and pass the new parameter; `Nothing` for direct chats and main-channel sends.
3. **`commentsClosed` helper** in `Internal.hs`.
4. **`allowedRole` arm** in `Commands.hs:4170-4176`.
5. **`memberCanSend` Nothing-arm fix** has already landed in Slice 2 step 6.
6. **`memberCanComment` helper** in `Subscriber.hs`. Compose with `memberCanSend` in `newGroupContentMessage` when `mc.parent = Just _`.
7. **`newGroupContentMessage` extension**: resolve `mc.parent` into `Maybe ChannelMsgInfo` via `getGroupChatItemBySharedMsgId`. Drop the message on lookup failure or on `commentsClosed`.
8. **`saveSndChatItems` / `saveRcvChatItem*`** thread the new `Maybe ChannelMsgInfo` parameter into `createNewChatItem_`.
9. **Send-side preflight in `sendGroupContentMessages_`**: call `prohibitedGroupContent` and `commentsClosed`, reject with `CECommandError` on either rejection.
10. **`groupMessageDelete` moderation arm** for comments — moderator deletes inside Comments are routed correctly via the existing scope/parent threading. Verify no special case is needed (the comment's `parent_chat_item_id` is already set on the row).
11. **`xGrpMsgForward`** — no change. The existing `processForwardedMsg` already passes the comment-bearing `XMsgNew` through `memberCanSend author_ scope $ newGroupContentMessage`. The new comment guard is composed inside `newGroupContentMessage`.

Build + test as before.

### Slice 5 — History playback and per-message disable

1. **`getChannelMsgCommentsForHistory`** helper in `Store/Messages.hs`.
2. **`getGroupHistoryItems`** extension to merge per-parent comments into the replay stream, capped at `M` per parent.
3. **New `XGrpCommentsDisabled` event** in `Protocol.hs` plumbed through `chatMsgEventTag` / `appJsonToCM` / `cmToAppMessage`.
4. **`xGrpCommentsDisabled` handler** in `Subscriber.hs`.
5. **`APISetCommentsDisabled` command** + handler in `Commands.hs`.
6. **`setChannelMsgCommentsDisabled`** helper in `Store/Messages.hs`.
7. **Wire the event into `processForwardedMsg`** for relay forwarding.

Build + test as before.

### Slice 6 — Haskell tests

`tests/ChatTests/Groups.hs` add `describe "channel comments"` block (after `describe "channel message operations"` at line ~281). Tests:

1. `testChannelCommentSubscriberCanComment` — owner posts a channel message; bob (subscriber) comments; cath, dan, eve all receive bob's comment via the relay.
2. `testChannelCommentRcvFromAnotherSubscriber` — cath comments; bob receives via relay; bob sees cath as unknown member.
3. `testChannelCommentQuoteParent` — cath's comment quotes the original channel post; subscribers see both `parent` and `quote` populated; the quote shape is correct on the wire.
4. `testChannelCommentQuoteAnotherComment` — dan's comment quotes bob's earlier comment AND has the same parent post; both fields are set on the same `MsgContainer`. This is the test that exercises the merged-record's ability to carry both `quote` and `parent`.
5. `testChannelCommentEditDelete` — author edits and deletes their own comment; receivers reconcile.
6. `testChannelCommentModerationDelete` — owner deletes someone else's comment via moderation.
7. `testChannelCommentObserverRejected` — in a channel group, downgrade a subscriber to `GRObserver` via `/mr team bob observer`; the subscriber's comment is rejected by `memberCanComment`.
8. `testChannelCommentDisabledRejected` — owner toggles `comments_disabled` on a post; subscriber's comment is rejected by the relay.
9. `testChannelCommentClosingWindow` — group has `comments.closeAfter = 1`; an old post can no longer accept new comments.
10. `testChannelCommentPrefOff` — group `comments.enable = FEOff`; all comment sends are rejected.
11. `testChannelCommentNotInRegularGroup` — a regular group rejects a message with a `parent` field (server-side and client-side preflight).
12. `testChannelCommentCountIncrement` — `comments_total` increments and decrements with insert/delete.
13. `testChannelCommentReplayedToNewSubscriberWithCap` — eve joins after several comments exist on a post; eve receives the parent post AND up to `M` most recent comments (the per-parent cap is enforced and older comments beyond the cap are NOT replayed).
14. `testChannelCommentOwnerSentAsGroupNoLeak` — owner posts a channel message and adds an own comment with `sentAsGroup = True`; subscribers must not see the owner's member identity in the comment. Mirrors `testChannelOwnerReaction` (`tests/ChatTests/Groups.hs:9482`) and `testChannelOwnerQuote` (`tests/ChatTests/Groups.hs:9510`) — required by threat-model item #7.
15. `testChannelCommentRoundtripJSON` — a `JSONTests.hs` round-trip case for the merged `MsgContainer` carrying `parent` only, `quote` only, both, and neither (Slice 1's round-trip test set covers parts of this; this test specifically asserts the full discriminator combinations).
16. `testChannelCommentMainChatExclusion` — verify the new `AND parent_chat_item_id IS NULL` predicate: a comment exists on a parent post; the main channel pagination MUST NOT include the comment row.

Each test follows the existing `testChannelMessage*` pattern with the new `bob #!> #team <id> reply` shorthand for sending comments.

Build + test: `cabal test simplex-chat-test --test-options='-m "channel comments"'`.

### Slice 7 — iOS API types and state

1. `ChatTypes.swift`: `ChannelMsgInfo` struct, `ChatInfo.group` third associated value, `ChatItem` parentChatItemId/commentsTotal/commentsDisabled fields with defaults, `isChannelPost` computed property.
2. `AppAPITypes.swift`: new `apiSetCommentsDisabled` `ChatCommand` case; `apiSendMessages` and `apiGetChat` extended with `parent: Int64?` parameter.
3. `SimpleXAPI.swift`: new `apiSetCommentsDisabled(...)` function; existing `apiSendMessages` / `apiGetChat` extended.
4. `ChatModel.swift:691` `getCIItemsModel` extension with the `channelMsgInfo()` branch. New `ChatInfo.channelMsgInfo()` helper method on `ChatTypes.swift`. New `SecondaryIMFilter.groupChannelMsgContext` constructor. Audit and update the four `cInfo.groupChatScope() == nil` gating sites at `ChatModel.swift:657, 679, 717, 788` to also gate on `cInfo.channelMsgInfo() == nil`.
5. `ItemsModel.loadSecondaryChat(...)` new branch for the channel msg filter.

iOS build via Xcode (out of scope for the Haskell test environment; built separately).

### Slice 8 — iOS UI

1. `Comments(N)` button on channel post bubble (locate the right `ChatItem` view file for channel posts in `apps/ios/Shared/Views/Chat/ChatItem/`).
2. Hidden `NavigationLink` in `ChatView.swift` driven by `@State commentsParent: ChatItem?` per the section above.
3. Closure `openComments: (ChatItem) -> Void` plumbed from `ChatView` down through the chat-items list to each item view.
4. New `ChannelMsgChatToolbar.swift` mirroring `MemberSupportChatToolbar.swift`.
5. Composer gating banner when comments are disabled / pref off / commenting window closed.
6. Owner-only "Disable comments" / "Enable comments" item in the per-message context menu.
7. iOS three-layer documentation updates per the section above.

### Slice 9 — Adversarial review and final test pass

1. Re-read every changed file once more end-to-end.
2. Run the full `cabal test simplex-chat-test` suite (not just channels).
3. Run `JSONTests.hs` and `ProtocolTests.hs` to catch JSON / protocol regressions.
4. Two consecutive clean adversarial self-review passes.

## Forward compatibility and threat model

**Forward compatibility.** Older clients and relays that have not upgraded past `commentsVersion`:

- Receive a `MsgContainer` with `parent` set: their parser would technically accept the field (since `parseMsgContainer` reads optional fields with `<|>`-style fallthrough), but they would not understand it and might surface it incorrectly.
- Mitigation: relay must NOT forward `XMsgNew` events whose container has `parent = Just _` to subscribers whose chat protocol version is below `commentsVersion`.
- Same gating for `XGrpCommentsDisabled` events.
- The relay's existing per-recipient version check (used for `groupKnockingVersion`, `contentReportsVersion`, etc.) is the place to add the gating.
- New optional `parentChatItemId` / `commentsTotal` / `commentsDisabled` fields on chat-item JSON ship with `omittedField` defaults so older remote-connection clients see chat items normally.
- The merged `MsgContainer` record's wire JSON shape is identical to the existing shape — pre-`commentsVersion` parsers see the same set of optional discriminator fields. No version bump is needed for the refactor itself; only the `parent` field semantics is gated.

**Threat model.**

1. Malicious subscriber tries to flood comments → existing per-relay rate limits apply unchanged. Owner can disable comments per-post or globally via the group preference.
2. Malicious subscriber tries to comment on a non-channel group → `prohibitedGroupContent`'s `not (useRelays' gInfo) -> Just GFComments` arm rejects on both send-side preflight (`Commands.hs sendGroupContentMessages_`) and receive-side (`Subscriber.hs newGroupContentMessage`), saving a `CIRcvGroupFeatureRejected GFComments` item.
3. Malicious subscriber tries to comment on a deleted parent post → cascade FK deletes the comment row when the parent is hard-deleted; the receive path's parent lookup returns `SEChatItemNotFound` and the message is dropped. For soft-deleted parents, the new explicit `isJust itemDeleted -> Just GFComments` arm in `prohibitedGroupContent` rejects.
4. Malicious subscriber tries to disable comments without permission → `xGrpCommentsDisabled` handler asserts `memberRole >= GRModerator`.
5. Malicious relay tries to forward a comment with a forged parent SharedMsgId → resolution fails on receive (`getGroupChatItemBySharedMsgId` returns not-found) and the message is dropped.
6. Malicious owner adversary tries to lie about `comments_disabled` to the relay → no different from any other group governance event; the relay's role check is the trust boundary.
7. Comment author identity leak → comment messages from owners as-channel use the existing `sentAsGroup` path. Comments from owners as-member use the member identity. The existing channel reaction/quote anti-leak tests (`testChannelOwnerReaction`, `testChannelOwnerQuote`) are the template — the corresponding comment leak test is in Slice 6 (#14).
8. Cascade FK and comment count drift → when a channel post is hard-deleted, the new `parent_chat_item_id ON DELETE CASCADE` removes all child comment rows in a single SQLite cascade. Because the parent row itself goes away, the now-orphan `comments_total` value disappears with it — there is no count drift. For interleaved comment-by-comment deletes the count is maintained incrementally via `adjustChannelMsgCommentCount` and clamped at zero with `MAX(0, ...)` so concurrent transactions that race a +1 against a −1 cannot leave a negative value. SQLite cascades through one level only by default, but child→child cascading is not needed (comments do not chain to other comments).
9. Replay attack on `XGrpCommentsDisabled` → if a relay re-broadcasts an old (stale) `XGrpCommentsDisabled false` after a newer `... true` has taken effect, subscribers would silently re-enable a disabled post. Mitigation for MVP: trust the latest delivery. Channel governance has no replay protection today and `XGrpCommentsDisabled` is no different from existing relay-broadcast events. Document as a known limitation in `product/rules.md`.
10. Merged-record parser confusion → an attacker crafts a message with both `quote` and `parent` set, where `parent` references a SharedMsgId that does not exist in the receiver's group. The receiver's parent lookup fails, the message is dropped, and the quote is never surfaced. No leak.
11. Merged-record forward+parent injection → an attacker crafts a message with `forward = True` and `parent = Just _`. The receive path treats the message as a forwarded comment (semantically odd but valid). The forward provenance is preserved per existing forward semantics, and the comment lands on the parent post. No new attack surface compared to the current forward+quote case. Defensive note: `memberCanComment` still runs after `memberCanSend`, so a member without `GRCommenter` role cannot deliver such a message even with the forward flag set.

## Out of scope (deferred)

- Subscriber profile dissemination (the existing "unknown member" path is the MVP).
- Kotlin Multiplatform (Android/Desktop) port.
- Notification routing for comments (no per-comment notifications in MVP).
- Comment count indicator on the channel chat list (no per-chat unread badge for comments).
- A "list of comment threads" view for owners (no analog to `MemberSupportView`).
- A separate `DeliveryWorkerScope` for comments (they share the channel post worker for batching).

## Critical files to be modified

Backend:
- `src/Simplex/Chat/Protocol.hs` — merged `MsgContainer`, smart constructors, `parseMsgContainer`/`msgContainerJSON` rewrite, `commentsVersion`, `XGrpCommentsDisabled`, `toMsgRef`
- `src/Simplex/Chat/Messages.hs` — `ChannelMsgInfo`, `ChatInfo.GroupChat` 3rd parameter, `ChatRef.channelMsg_`, `CIMeta` extensions
- `src/Simplex/Chat/Types/Shared.hs` — `GRCommenter`
- `src/Simplex/Chat/Types/Preferences.hs` — `CommentsGroupPreference` (~22 sites)
- `src/Simplex/Chat/Controller.hs` — `channelSubscriberRole` default
- `src/Simplex/Chat/Library/Internal.hs` — `prohibitedGroupContent` extension, `commentsClosed` helper
- `src/Simplex/Chat/Library/Commands.hs` — `allowedRole`, `APISetCommentsDisabled`, parent-id parsing on `APISendMessages` and `APIGetChat`
- `src/Simplex/Chat/Library/Subscriber.hs` — `memberCanSend` Nothing-arm fix, `memberCanComment` helper, `newGroupContentMessage` parent resolution, `xGrpCommentsDisabled` handler
- `src/Simplex/Chat/Store/Messages.hs` — `createNewChatItem_` extension, `getChannelMsgInfo`, `getGroupChat` parent param, `getChatItemIDs` new arm, `adjustChannelMsgCommentCount`, `setChannelMsgCommentsDisabled`, `getChannelMsgCommentsForHistory`, `getGroupHistoryItems` extension, default-scope SELECT predicates
- `src/Simplex/Chat/Store/Groups.hs` — `defaultGroupPrefs` for new channels
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260501_channel_comments.hs` (new)
- `src/Simplex/Chat/Store/Postgres/Migrations/M20260501_channel_comments.hs` (new)
- `src/Simplex/Chat/Store/SQLite/Migrations.hs`, `src/Simplex/Chat/Store/Postgres/Migrations.hs`
- `simplex-chat.cabal`

Tests:
- `tests/ChatTests/Groups.hs` — `describe "channel comments"`
- `tests/JSONTests.hs` — merged `MsgContainer` round-trip, `XGrpCommentsDisabled` round trip
- `tests/ProtocolTests.hs` — wire compatibility for the merged record (no diff from existing wire shape)

iOS:
- `apps/ios/SimpleXChat/ChatTypes.swift` — `ChannelMsgInfo`, `ChatInfo.group` 3rd associated value, `ChatItem` new fields
- `apps/ios/Shared/Model/AppAPITypes.swift` — new `ChatCommand` case, parent param on existing cases
- `apps/ios/Shared/Model/SimpleXAPI.swift` — `apiSetCommentsDisabled`, parent param on `apiSendMessages` and `apiGetChat`
- `apps/ios/Shared/Model/ChatModel.swift` — `getCIItemsModel` channel-msg branch, audited `groupChatScope() == nil` gating sites (657, 679, 717, 788), `SecondaryIMFilter.groupChannelMsgContext`
- `apps/ios/Shared/Views/Chat/ChatView.swift` — `@State commentsParent`, hidden `NavigationLink`, `openComments` closure plumbing
- `apps/ios/Shared/Views/Chat/ChatItem/<channel post item view>.swift` — Comments button + context-menu items
- `apps/ios/Shared/Views/Chat/Group/ChannelMsgChatToolbar.swift` (new)
- `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift` — composer gating banner
- `apps/ios/spec/client/comments.md` (new), `chat-view.md`, `api.md`, `state.md`, `impact.md`
- `apps/ios/product/views/comments.md` (new), `concepts.md`, `glossary.md`, `rules.md`

## Verification

Backend (run by Claude in this environment):

1. After Slice 1, run `cabal test simplex-chat-test --test-options='-m "JSON"'` and `cabal test simplex-chat-test --test-options='-m "Protocol"'` — the merged `MsgContainer` must round-trip identically. Run the full chat test suite — no behavior change should be observable.
2. After each subsequent slice, run `cabal build --ghc-options=-O0` and `cabal test simplex-chat-test --test-options='-m "channels"'`.
3. After Slice 6, run `cabal test simplex-chat-test --test-options='-m "channel comments"'`.
4. After Slice 9, run the full `cabal test simplex-chat-test` suite.
5. After Slice 2, manually inspect the generated `chat_schema.sql` to confirm the new columns and indexes are present and the column name is `parent_chat_item_id`.

iOS (run in Xcode):

1. Build the iOS app target in Xcode after Slice 7.
2. Manual test in iOS simulator: create a channel as Alice, join from Bob, post a message from Alice, comment from Bob, verify the comments button shows `comments 1`, tap to navigate (NOT a sheet) to the Comments view, post another comment, edit, delete, quote both the parent post and another comment.
3. Verify navigation: from inside Comments, swipe back to the channel; tap a different post's comments; verify the navigation stack behaves consistently with the rest of the app.
4. As Alice, disable comments on the post, verify Bob's composer goes inactive and a banner appears, verify Bob cannot send a comment, re-enable, verify Bob can send again.
5. Set `comments.closeAfter = 60` in the group preference, post a message, wait, verify commenting is rejected after 60 seconds.
6. Run iOS test suite (`xcodebuild test ...`).

A change is complete only after two consecutive adversarial self-review passes find zero issues at the end of Slice 9.
