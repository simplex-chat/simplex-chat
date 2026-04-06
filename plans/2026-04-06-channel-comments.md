# Plan: Comments on channel messages

## Context

SimpleX Chat introduced channels in PR #6382 (the chat-relays MVP): groups where one node acts as a relay between the owner and many subscribers, posts are signed, and subscribers do not see each other. Channels currently support posts only — there is no way for subscribers to discuss a post.

Channels need Telegram-style **comments**: under each channel post, a tappable affordance shows a count and opens a separate Comments view containing a flat thread of replies, with its own composer. Subscribers can comment, including quoting the parent post and quoting other comments.

This plan implements comments by extending the existing **`GroupChatScope` abstraction** that today supports member-support / reports chats. Comments live in a new scope `GCSChannelMsg` whose anchor is the channel post itself. The comment messages are ordinary `MCSimple`/`MCQuote`/`MCForward` messages, so all existing message features (text, files, voice, quotes, forwards, edits, deletes, reactions) work for free.

The plan covers Haskell core, the CLI command surface (for tests), and the iOS app. Kotlin Multiplatform (Android/Desktop) is deferred to a follow-up. Profile dissemination between subscribers is also a separate follow-up; for the MVP, comment authors who are unknown to a viewing subscriber appear as unknown members (the existing `unknownMemberRole` mechanism).

Comments are NOT a `MsgContainer` constructor — `MCComment` is left as dead code for forward compatibility and never constructed. Parent linkage belongs on the **scope**, not the container, which preserves the ability to quote and forward inside comments. There is no unread tracking on comments — only a running total comment count per post.

## Solution summary

- New `GroupChatScope` variant `GCSChannelMsg {channelChatItemId :: ChatItemId}` with corresponding tag `GCSTChannelMsg_` ↔ `"channel_msg"` and runtime info `GCSIChannelMsg {channelChatItem :: CChatItem 'CTGroup, channelMsgSharedId :: SharedMsgId}`. The `SharedMsgId` is stored as a separate field — extracted at smart-constructor time — so `toMsgScope` is total.
- New wire variant `MSChannelMsg {parentMsgId :: SharedMsgId}` on `MsgScope`. Gated behind a chat-protocol version bump so older subscribers/relays do not see broken JSON.
- New SQLite + Postgres migration adding three columns to `chat_items`:
  - `group_scope_chat_item_id INTEGER REFERENCES chat_items(chat_item_id) ON DELETE CASCADE` (the comment row's parent linkage)
  - `comments_total INTEGER NOT NULL DEFAULT 0` (running comment count on the parent post row)
  - `comments_disabled INTEGER NOT NULL DEFAULT 0` (per-post disable flag on the parent post row)
  - Two new indexes mirroring the existing scope-pagination index pattern.
- New `CommentsGroupPreference {enable, closeAfter}` group preference. `enable` controls whether commenting is allowed on the channel at all; `closeAfter` (`Maybe Int`, seconds since post creation; `Nothing` = no limit) is the duration after which a channel post stops accepting new comments. Modeled on `TimedMessagesGroupPreference` but the field name reflects that it closes commenting on a post rather than expiring data.
- New role `GRCommenter` inserted between `GRObserver` and `GRAuthor` in `GroupMemberRole`. Default `channelSubscriberRole` changes from `GRObserver` to `GRCommenter`.
- New chat command `APISetCommentsDisabled groupId chatItemId disabled` and a corresponding wire event `XGrpCommentsDisabled parentSharedMsgId disabled` so the disabled state is disseminated through the relay to all subscribers.
- iOS: extend `GroupChatScope`/`GroupChatScopeInfo`, add the `matchesSecondaryIM` arm, add a small `Comments(N)` button on channel post bubbles that calls `ItemsModel.loadSecondaryChat(...)` and pushes the existing `SecondaryChatView`. Add a small toolbar variant and an owner-only "Disable comments" menu item.
- Test additions in `tests/ChatTests/Groups.hs` under a new `describe "channel comments"` block, mirroring the existing `testChannelMessage*` patterns.
- iOS three-layer documentation updates per `apps/ios/CODE.md` change protocol.

## Detailed technical design

### Wire protocol additions

`src/Simplex/Chat/Protocol.hs`

1. Extend `MsgScope`:
   ```haskell
   data MsgScope
     = MSMember     {memberId :: MemberId}
     | MSChannelMsg {parentMsgId :: SharedMsgId}
     deriving (Eq, Show)
   ```
   The `taggedObjectJSON $ dropPrefix "MS"` derivation auto-generates `"type":"channelMsg"`.

2. Bump chat protocol version. The version constants block in `Protocol.hs:89-155` has `currentChatVersion = VersionChat 17` and the latest constant `memberSupportVoiceVersion = VersionChat 17`. `MsgScope` was introduced at v15 (`groupKnockingVersion`, line 146-147 with comment `support group knocking (MsgScope)`). Add:
   ```haskell
   commentsVersion :: VersionChat
   commentsVersion = VersionChat 18
   ```
   and bump `currentChatVersion = VersionChat 18` (line 90). Use `commentsVersion` to gate every comment-bearing message and event in send-path version assertions, and to gate per-recipient relay forwarding.

3. Add a new chat event `XGrpCommentsDisabled SharedMsgId Bool` to `ChatMsgEvent 'Json`, wired into `chatMsgEventTag` / `appJsonToCM` / `cmToAppMessage`. The event is signed and forwarded via the relay just like other channel governance events.

4. `MCComment`: leave the constructor and parser arm intact. Add a brief code comment noting it is reserved for future shapes and is intentionally never constructed (comments use the scope mechanism instead).

### DB schema migration

`src/Simplex/Chat/Store/SQLite/Migrations/M20260501_channel_comments.hs` (new) and the matching Postgres file `src/Simplex/Chat/Store/Postgres/Migrations/M20260501_channel_comments.hs`. Pick the actual date during implementation; this filename uses 2026-05-01 as a placeholder consistent with the existing `M2026*` cadence.

```sql
ALTER TABLE chat_items ADD COLUMN group_scope_chat_item_id INTEGER REFERENCES chat_items(chat_item_id) ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN comments_total INTEGER NOT NULL DEFAULT 0;
ALTER TABLE chat_items ADD COLUMN comments_disabled INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_group_scope_chat_item_id ON chat_items(group_scope_chat_item_id);

CREATE INDEX idx_chat_items_channel_msg_scope_item_ts ON chat_items(
  user_id,
  group_id,
  group_scope_tag,
  group_scope_chat_item_id,
  item_ts
);
```

Register the migration in `src/Simplex/Chat/Store/SQLite/Migrations.hs` and `src/Simplex/Chat/Store/Postgres/Migrations.hs`. Add the module to `simplex-chat.cabal` exposed-modules.

`chat_schema.sql` is auto-regenerated by tests; do not hand-edit.

The cascade self-FK is safe: SQLite cascades through one level by default. Tests already cover this for the existing self-FK on `chat_items.quoted_item_id`.

### Group scope types (Haskell)

`src/Simplex/Chat/Messages.hs`

1. Extend `GroupChatScope`:
   ```haskell
   data GroupChatScope
     = GCSMemberSupport {groupMemberId_ :: Maybe GroupMemberId}
     | GCSChannelMsg    {channelChatItemId :: ChatItemId}
     deriving (Eq, Show, Ord)
   ```

2. Extend `GroupChatScopeTag` and its `TextEncoding`:
   ```haskell
   data GroupChatScopeTag
     = GCSTMemberSupport_
     | GCSTChannelMsg_

   instance TextEncoding GroupChatScopeTag where
     textDecode = \case
       "member_support" -> Just GCSTMemberSupport_
       "channel_msg"    -> Just GCSTChannelMsg_
       _                -> Nothing
     textEncode = \case
       GCSTMemberSupport_ -> "member_support"
       GCSTChannelMsg_    -> "channel_msg"
   ```

3. Extend `GroupChatScopeInfo`:
   ```haskell
   data GroupChatScopeInfo
     = GCSIMemberSupport {groupMember_ :: Maybe GroupMember}
     | GCSIChannelMsg    {channelChatItem :: CChatItem 'CTGroup, channelMsgSharedId :: SharedMsgId}
   ```
   The `CChatItem` carries the parent post's `itemTs` (needed for the commenting-window check) and `comments_disabled` flag (needed for the gate). The `channelMsgSharedId` is extracted by the smart constructor at lookup time so `toMsgScope` is total — it does not need to deconstruct the parent CChatItem to find the SharedMsgId.

4. Extend `toMsgScope` — total, no `error` arm:
   ```haskell
   toMsgScope GroupInfo {membership} = \case
     GCSIMemberSupport {groupMember_} ->
       MSMember $ memberId' $ fromMaybe membership groupMember_
     GCSIChannelMsg {channelMsgSharedId} ->
       MSChannelMsg channelMsgSharedId
   ```
   The smart constructor that builds `GCSIChannelMsg` (in `Store/Messages.hs` and `Internal.hs`) reads the parent's `itemSharedMsgId` and raises `SEChatItemNotFound` if it is `Nothing`. Channel posts are always saved with a `SharedMsgId` by construction, so this branch is unreachable for valid data and explicitly drops malformed data on the floor instead of crashing.

5. Forward-compat for AChat / ChatItem JSON: per `docs/CONTRIBUTING.md`, any new field that ships in remote-connection JSON must be optional with `omittedField`. The new constructors are additive on tagged JSON, but the new optional `commentsTotal` and `commentsDisabled` fields on chat-item JSON for channel posts must use `omittedField` defaults of `0` and `False` respectively (the JSON field names are derived from the Haskell field names `commentsTotal :: Int` and `commentsDisabled :: Bool` on `CIMeta`).

### Group preference

`src/Simplex/Chat/Types/Preferences.hs`

Add `CommentsGroupPreference` mirroring `TimedMessagesGroupPreference`:

```haskell
data CommentsGroupPreference = CommentsGroupPreference
  { enable :: GroupFeatureEnabled,
    closeAfter :: Maybe Int -- seconds since post creation; Nothing = never close
  }
  deriving (Eq, Show)
```

Mechanical extension across the file (~22 sites — the same set every other group preference touches):

- `GroupFeature` and `SGroupFeature` add `GFComments`/`SGFComments`
- `groupFeatureNameText` adds the localized name
- `allGroupFeatures` includes it
- `groupPrefSel` / `toGroupFeature` map between the singleton and the selector
- `GroupPreferences` and `FullGroupPreferences` add the optional / non-optional field
- `setGroupPreference_` extends the merge logic
- `GroupPreferenceI FullGroupPreferences` instance
- `defaultGroupPrefs` / `emptyGroupPrefs` / `defaultBusinessGroupPrefs` (default to `enable = FEOff` for backward-compatible groups; default to `FEOn` only for newly created channel groups — see `defaultGroupPrefs` callsites in `Store/Groups.hs`)
- `HasField "enable"` for the singleton
- `GroupFeatureI` (or `GroupFeatureNoRoleI` — comments has no role on the preference; the role gating is the separate `GRCommenter` mechanism)
- `groupParamText_` renders the `closeAfter` duration the same way `TimedMessagesGroupPreference` renders its `ttl`
- `toGroupPreferences` / `mergeGroupPreferences`
- `deriveJSON` and explicit `omittedField` returning `Just CommentsGroupPreference {enable = FEOff, closeAfter = Nothing}` for forward compat

When a channel is created with `useRelays = True`, `defaultGroupPrefs` for that case sets `comments = CommentsGroupPreference {enable = FEOn, closeAfter = Nothing}`. Locate the channel-creation prefs branch in `Store/Groups.hs` (the same place that defaults `directMessages` to off and so on for channels).

### Role

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

**Critical role-comparison fix in `Subscriber.hs:1522` `memberCanSend`.** The current code is:
```haskell
memberCanSend (Just m@GroupMember {memberRole}) msgScope a = case msgScope of
  Just MSMember {} -> a
  Nothing
    | memberRole > GRObserver || memberPending m -> a   -- THIS LINE
    | otherwise -> messageError "member is not allowed to send messages" $> Nothing
```
Inserting `GRCommenter` between `GRObserver` and `GRAuthor` makes `GRCommenter > GRObserver` true, so the existing test would silently grant non-comment send rights to commenters. Change to `memberRole >= GRAuthor`, which preserves the documented send-threshold semantic (matches `assertUserGroupRole gInfo GRAuthor` used elsewhere in `Commands.hs` for the regular send path).

Audit every pattern match on `GroupMemberRole` for non-exhaustive matches that the compiler will flag. After grepping all `GRObserver|GRCommenter|GRAuthor|GRMember` sites, the only role-comparison expression that needs adjustment is the `memberCanSend` line above. Existing sites that use `>= GRAuthor`, `>= GRModerator`, `>= GRAdmin`, `>= GROwner`, `< GRModerator`, etc. continue to work without source change.

`down_` migration for `M20260222_chat_relays` already maps `member_role = 'relay'` back to `'observer'`. Add a parallel down step in the new comments migration if any rows store `'commenter'` — map `'commenter'` back to `'observer'` to preserve send-permission semantics on downgrade.

### Send path

`src/Simplex/Chat/Library/Commands.hs:4170-4176` — `allowedRole` insertion:

```haskell
allowedRole = case scope of
  Nothing -> Just GRAuthor
  Just (GCSMemberSupport Nothing)
    | memberPending membership -> Nothing
    | otherwise -> Just GRObserver
  Just (GCSMemberSupport (Just _gmId)) -> Just GRModerator
  Just (GCSChannelMsg _) -> Just GRCommenter
```

`Internal.hs:1572` `getChatScopeInfo` — **signature change** + new arm. The current signature is `getChatScopeInfo :: VersionRangeChat -> User -> GroupChatScope -> CM GroupChatScopeInfo` (verified at Internal.hs:1572). The new arm needs the parent post's `groupId`, so the function gains a `GroupInfo` parameter:

```haskell
getChatScopeInfo :: VersionRangeChat -> User -> GroupInfo -> GroupChatScope -> CM GroupChatScopeInfo
getChatScopeInfo vr user gInfo = \case
  GCSMemberSupport Nothing -> pure $ GCSIMemberSupport Nothing
  GCSMemberSupport (Just gmId) -> do
    supportMem <- withFastStore $ \db -> getGroupMemberById db vr user gmId
    pure $ GCSIMemberSupport (Just supportMem)
  GCSChannelMsg parentChatItemId -> do
    parent@(CChatItem _ ChatItem {meta = CIMeta {itemSharedMsgId}}) <-
      withFastStore $ \db -> getGroupChatItem db user (groupId' gInfo) parentChatItemId
    case itemSharedMsgId of
      Just sId ->
        pure $ GCSIChannelMsg {channelChatItem = parent, channelMsgSharedId = sId}
      Nothing ->
        throwChatError $ ChatErrorStore $ SEChatItemNotFound parentChatItemId
```

The existing parent-lookup function is `getGroupChatItem db user groupId itemId :: ExceptT StoreError IO (CChatItem 'CTGroup)` at `Store/Messages.hs:3014` (NOT `getGroupChatItemById` — that function does not exist).

**Call sites (9) that must pass `gInfo` as the new third argument** — each already has `gInfo` in scope at the call site (verified by reading the surrounding code at each location):

- `Internal.hs:1455` (`deleteTimedItem` — `gInfo` is fetched on line 1453)
- `Commands.hs:702`, `Commands.hs:758`, `Commands.hs:848`, `Commands.hs:1097`, `Commands.hs:1127`, `Commands.hs:3124`, `Commands.hs:4158` — each call site has a corresponding `getGroupInfo` call earlier in the same function body. Implementation must verify and add `gInfo` to each invocation.

The `Nothing` arm of the case guards the smart-constructor invariant that channel posts always have a `SharedMsgId`. The error constructor is the existing `SEChatItemNotFound {itemId :: ChatItemId}` from `Store/Shared.hs:135` (verified by grep), wrapped in `ChatErrorStore` to match the existing chat-error-from-store pattern. The smart constructor only enforces the SharedMsgId invariant; "parent must be a channel post" is enforced separately by the `useRelays' gInfo` guard in `prohibitedGroupContent` and in the receive-side `mkGetMessageChatScope` arm — within a channel group, every chat item is necessarily a channel post (subscribers cannot post in main scope), so a successful `useRelays'` check is sufficient to prove the parent is a channel post.

`Internal.hs:1579` `getGroupRecipients` — **no change**. The first guard `useRelays' gInfo && not (isRelay membership)` already routes ALL channel sends through `getGroupRelayMembers` regardless of scope, so comment send/receive fan-out is handled by the same path that handles channel posts. Verified by reading the function body in full.

`Internal.hs:341` `prohibitedGroupContent` — new arm. The function signature is `GroupInfo -> GroupMember -> Maybe GroupChatScopeInfo -> MsgContent -> Maybe MarkdownList -> Maybe f -> Bool -> Maybe GroupFeature` (verified — no `UTCTime` param), so the commenting-window check that needs the wall clock CANNOT live here. Split into two checks:

1. **Structural checks in `prohibitedGroupContent` (no clock needed)**:
   ```haskell
   prohibitedGroupContent gInfo m scopeInfo mc ft file_ sent
     | ... existing arms ...
     | otherwise = case scopeInfo of
         Just GCSIChannelMsg {channelChatItem = CChatItem _ ChatItem {meta = CIMeta {itemDeleted, commentsDisabled}}}
           | not (useRelays' gInfo)                          -> Just GFComments
           | not (groupFeatureAllowed SGFComments gInfo)     -> Just GFComments
           | isJust itemDeleted                              -> Just GFComments
           | commentsDisabled                                -> Just GFComments
           | otherwise                                       -> Nothing
         _ -> Nothing
   ```
   Note the use of record-pattern syntax `GCSIChannelMsg {channelChatItem = ...}` so the second field `channelMsgSharedId` is irrelevant and ignored. The deleted-parent case is now an explicit reject (not a fall-through), so a soft-deleted parent post stops accepting comments. The `useRelays'` check inside the arm guards against an `MSChannelMsg` scope being constructed against a non-channel group locally.

2. **Time-dependent commenting-window check in a new helper called from send/receive wrappers**:
   ```haskell
   -- in Internal.hs, near prohibitedGroupContent
   commentsClosed :: GroupInfo -> CChatItem 'CTGroup -> UTCTime -> Bool
   commentsClosed
     GroupInfo {fullGroupPreferences = FullGroupPreferences {comments = CommentsGroupPreference {closeAfter}}}
     (CChatItem _ ChatItem {meta = CIMeta {itemTs}})
     now =
       case closeAfter of
         Just secs -> diffUTCTime now itemTs > fromIntegral secs
         Nothing -> False
   ```
   The argument is destructured via record patterns rather than using `fullGroupPreferences gInfo` as a function call, because `Internal.hs` enables `DuplicateRecordFields` but not `OverloadedRecordDot`, and field selectors for duplicated fields are not callable as functions in that mode (verified — `Internal.hs` line 3 has `DuplicateRecordFields` and no `OverloadedRecordDot`).
   - **Send side** (`sendGroupContentMessages_` in `Commands.hs`): after `prohibitedGroupContent` is called, if the scope is `GCSIChannelMsg` and `useRelays' gInfo`, also call `liftIO getCurrentTime >>= \now -> when (commentsClosed gInfo parent now) $ throwChatError $ CECommandError "commenting closed on this post"` (or use a more specific error variant).
   - **Receive side** (`newGroupContentMessage` in `Subscriber.hs`, after `mkGetMessageChatScope` returns `GCSIChannelMsg`): same shape, drop the message via `messageError "commenting closed on channel post"`.

The exact placement of the new arm in `prohibitedGroupContent` is the bottom-most `otherwise` arm that returns `Nothing` today; turn that arm into a `case scopeInfo of` that adds the comments check and falls through to `Nothing` for non-comment scopes. This preserves all existing behavior.

The two new fields `commentsDisabled :: Bool` and `commentsTotal :: Int` MUST be added to the Haskell `CIMeta` record (in `Messages.hs`) with default values `False` and `0` so existing chat-item parsing remains forward-compatible. Both fields are loaded from the new `chat_items` columns in the existing chat-item SELECT queries (one site each in `toGroupChatItem` etc. — locate via `CIMeta {` constructor sites).

`Internal.hs:204` `prepareGroupMsg` — **no change**. The function already threads `msgScope :: Maybe MsgScope` through all three container branches.

`Commands.hs:4196` `saveSndChatItems` — already saves with `CDGroupSnd gInfo chatScopeInfo`. The `comments_total` increment on the parent post row happens in the saver helper (see "Comment count maintenance" below).

### Receive path & relay forwarding

`src/Simplex/Chat/Library/Subscriber.hs:1519` `memberCanSend` — new wire arm AND `Nothing`-arm role-comparison fix (the same fix called out in the Role section above; restated here for completeness):

```haskell
memberCanSend (Just m@GroupMember {memberRole}) msgScope a = case msgScope of
  Just MSMember {}     -> a
  Just MSChannelMsg {} -> if memberRole >= GRCommenter then a
                          else messageError "member is not allowed to comment" $> Nothing
  Nothing
    | memberRole >= GRAuthor || memberPending m -> a    -- was: memberRole > GRObserver
    | otherwise -> messageError "member is not allowed to send messages" $> Nothing
```

`Subscriber.hs:1943` `newGroupContentMessage` — no structural change; the existing `mkGetMessageChatScope vr user gInfo m content msgScope_` call resolves the scope via `Internal.hs:1626` `mkGetMessageChatScope`. Add a new arm there for `Just (MSChannelMsg parentSharedMsgId)`:

```haskell
mkGetMessageChatScope vr user gInfo m mc msgScope_ =
  mkGroupChatScope gInfo m >>= \case
    groupScope@(_, _, Just _) -> pure groupScope
    (_, _, Nothing) -> case msgScope_ of
      Nothing -> ...
      Just (MSMember mId) -> ...
      Just (MSChannelMsg parentSharedMsgId) -> do
        parent <- withStore $ \db ->
          getGroupChatItemBySharedMsgId db user gInfo Nothing parentSharedMsgId
        -- the parent SharedMsgId is the same value we just looked up by, so we can pass it through directly
        pure (gInfo, m, Just GCSIChannelMsg {channelChatItem = parent, channelMsgSharedId = parentSharedMsgId})
```

The `Nothing` for `groupMemberId_` (the second argument to `getGroupChatItemBySharedMsgId`) is correct — channel comments reference posts by `SharedMsgId` only; the parent's authoring member is not part of the lookup.

`mkGetMessageChatScope` does NOT need an explicit `useRelays' gInfo` guard. If the group is not a channel, two outcomes are possible: (a) the parent SharedMsgId does not match any chat item in the group, in which case the lookup raises `SEChatItemNotFound` and the receive path drops the message; (b) the SharedMsgId does match (regular group message accidentally collides), in which case the resulting `GCSIChannelMsg` flows into `prohibitedGroupContent`, whose new `not (useRelays' gInfo) -> Just GFComments` arm produces a `CIRcvGroupFeatureRejected GFComments` item via the existing `rejected` helper at `Subscriber.hs:1965`. Both pathways are clean and reuse existing infrastructure — no new error constructor or throwing logic is needed inside the scope resolver.

`Subscriber.hs:2125` `groupMessageDelete` moderation arm — add the `MSChannelMsg` wire arm so moderator deletes inside Comments are routed through the comment scope, not the main channel chat. Mechanical: copy the existing `MSMember` arm and replace the scope resolver with the comment-scope resolver.

`Subscriber.hs:3248` `xGrpMsgForward` — no change. The existing `processForwardedMsg` already passes the comment-bearing `XMsgNew` through `memberCanSend author_ scope $ newGroupContentMessage`. The new `memberCanSend` arm above handles the `MSChannelMsg` case. The existing `unknownMemberRole gInfo` (which returns `channelSubscriberRole`) now returns `GRCommenter` — meaning unknown subscribers who comment are auto-created with the role that the receive-side gate accepts.

`Delivery.hs:106` `infoToDeliveryContext` — new arm:

```haskell
infoToDeliveryContext GroupInfo {membership} scopeInfo sentAsGroup =
  DeliveryTaskContext {jobScope, sentAsGroup}
  where
    jobScope = case scopeInfo of
      Nothing                                  -> DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}
      Just GCSIMemberSupport {groupMember_}    -> DJSMemberSupport {supportGMId = groupMemberId' $ fromMaybe membership groupMember_}
      Just GCSIChannelMsg {}                   -> DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}
```

Comments fan out to all live subscribers via the existing `DWSGroup` worker. No new `DeliveryWorkerScope` or `DeliveryJobScope` constructor is needed.

### Per-message disable

New chat command in `Commands.hs`:

```haskell
APISetCommentsDisabled GroupId ChatItemId Bool
```

Handler:

1. Load the channel post via `getGroupChatItem db user groupId chatItemId` (Store/Messages.hs:3014). Assert it is a channel post (`CIChannelRcv`/`CIChannelSnd` direction) and that the group `useRelays' gInfo`.
2. Assert the user role is `>= GRModerator`.
3. Update `chat_items.comments_disabled` for the post row.
4. Send a new chat event `XGrpCommentsDisabled parentSharedMsgId disabled` via the relay's standard `sendGroupMessages` path so subscribers learn the new state.
5. Return `CRChatItemUpdated` so iOS state reconciliation works.

New wire event:

```haskell
| XGrpCommentsDisabled SharedMsgId Bool
```

Handler in `Subscriber.hs` (alongside `xGrpMsgUpdate`/`xGrpMsgDelete`):

```haskell
xGrpCommentsDisabled gInfo m@GroupMember {memberRole} parentSharedMsgId disabled = do
  if memberRole < GRModerator
    then messageError "member is not allowed to disable comments"
    else do
      parent <- withStore $ \db ->
        getGroupChatItemBySharedMsgId db user gInfo Nothing parentSharedMsgId
      withStore' $ \db ->
        setChannelMsgCommentsDisabled db (cChatItemId parent) disabled
      toView $ CEvtChatItemUpdated user (AChatItem ...)
```

The destructuring `m@GroupMember {memberRole}` is required because `DuplicateRecordFields` is enabled and field selectors don't work as functions. The `if/else` shape (rather than `unless`) ensures the body short-circuits cleanly when the role check fails — `messageError` returns `()` and the body must also return `()` in that branch.

Plumb through `chatMsgEventTag` / `appJsonToCM` / `cmToAppMessage` and through `processForwardedMsg` for relay forwarding. The event is signed exactly like existing channel governance events.

### Comment count maintenance

The `comments_total` column on the parent post row is incremented:

- On comment **insert** (snd or rcv): +1.
- On comment **delete** (snd, rcv, moderation): −1, only if the previous row was not already marked deleted.
- On comment **edit**: no change to the count.

Implement as a tiny helper in `Store/Messages.hs`:

```haskell
adjustChannelMsgCommentCount :: DB.Connection -> ChatItemId -> Int -> IO ()
adjustChannelMsgCommentCount db parentChatItemId delta =
  DB.execute db
    "UPDATE chat_items SET comments_total = MAX(0, comments_total + ?) WHERE chat_item_id = ?"
    (delta, parentChatItemId)
```

Call sites — guard each with the new `GCSIChannelMsg` arm so non-comment chat items are unaffected and other callers of these store functions see no behavior change:

- **Insert path**: place the increment inside `createNewChatItem_` itself (`Store/Messages.hs:581-624`), immediately after the INSERT. `createNewChatItem_` already has `groupScope :: Maybe (Maybe GroupChatScopeInfo)` in scope (it builds `groupScopeRow` from it in step 4a above), so a single `case groupScope of Just (Just GCSIChannelMsg {channelChatItem}) -> adjustChannelMsgCommentCount db (cChatItemId channelChatItem) 1; _ -> pure ()` covers both `createNewSndChatItem` and `createNewRcvChatItem` without duplication. Both wrappers call `createNewChatItem_` so the increment fires in the same DB transaction as the INSERT.
- **Delete path** (`deleteChatItemBy*` and `markChatItemDeleted`): before issuing the delete/mark, fetch the row's `group_scope_tag` and `group_scope_chat_item_id`. If `group_scope_tag = 'channel_msg'` and the row was previously not deleted, call `adjustChannelMsgCommentCount db parentItemId (-1)`.

Edits and reactions do not change the count. A second moderator delete on an already-deleted comment must NOT decrement again — wrap the decrement in a check that the row's `item_deleted` was previously NULL (or use `markChatItemDeleted` only when the previous state was non-deleted).

The `MAX(0, ...)` clamp in the helper guards against transient negative counts under concurrent deletes; combined with transactional guarding, the count remains a safe upper bound on live comments at all times.

### Pagination

`Store/Messages.hs:1506` `getChatItemIDs` — new arm:

```haskell
GroupChat GroupInfo {groupId} scopeInfo_ -> case (scopeInfo_, contentFilter) of
  ...
  (Just GCSIChannelMsg {channelChatItem = CChatItem _ ChatItem {meta = CIMeta {itemId = parentId}}}, Nothing) ->
    liftIO $ idsQuery
      (grCond <> " AND group_scope_tag = ? AND group_scope_chat_item_id = ? ")
      (userId, groupId, GCSTChannelMsg_, parentId)
      "item_ts"
  ...
```
The pattern uses record syntax `GCSIChannelMsg {channelChatItem = ...}` so the second field `channelMsgSharedId` is bound by wildcard and ignored.

The scope→column round-trip in `getGroupChatScopeForItem_` (Store/Messages.hs:1477) reads three columns now:

```haskell
DB.query db
  [sql|
    SELECT group_scope_tag, group_scope_group_member_id, group_scope_chat_item_id
    FROM chat_items
    WHERE chat_item_id = ?
  |]
  (Only itemId)

toScope (scopeTag, scopeMemberId, scopeChatItemId) = case (scopeTag, scopeMemberId, scopeChatItemId) of
  (Just GCSTMemberSupport_, Just gmId, Nothing)  -> Just $ GCSMemberSupport (Just gmId)
  (Just GCSTMemberSupport_, Nothing,   Nothing)  -> Just $ GCSMemberSupport Nothing
  (Just GCSTChannelMsg_,    Nothing,   Just cId) -> Just $ GCSChannelMsg cId
  (Nothing,                 Nothing,   Nothing)  -> Nothing
  _                                              -> Nothing
```

`getCreateGroupChatScopeInfo` (Store/Messages.hs:1448) gains a parallel arm for `GCSChannelMsg parentChatItemId` that loads the parent `CChatItem` (via `getGroupChatItem db user groupId parentChatItemId`), extracts `itemSharedMsgId` from the parent's `CIMeta`, raises `SEChatItemNotFound parentChatItemId` if it is `Nothing`, and returns `GCSIChannelMsg {channelChatItem = parent, channelMsgSharedId = sId}`. Both `getChatScopeInfo` (in `Internal.hs`) and `getCreateGroupChatScopeInfo` (in `Store/Messages.hs`) MUST use the same smart-construction pattern so the data type's invariant (channel-post parents always carry their SharedMsgId) holds at every construction site.

### Insert path: `createNewChatItem_` extension

**Critical: this is the writer that persists every chat item.** Located at `Store/Messages.hs:581-624`. The current code has a `groupScopeRow` 2-tuple at line 621 that ONLY handles `GCSIMemberSupport`:

```haskell
groupScopeRow :: (Maybe GroupChatScopeTag, Maybe GroupMemberId)
groupScopeRow = case groupScope of
  Just (Just GCSIMemberSupport {groupMember_}) -> (Just GCSTMemberSupport_, groupMemberId' <$> groupMember_)
  _ -> (Nothing, Nothing)
```

Without an explicit arm for `GCSIChannelMsg`, the fall-through `_ -> (Nothing, Nothing)` would write a channel comment with `group_scope_tag = NULL`, making it indistinguishable from a main channel post. This is a **silent data-corruption hazard** — the plan MUST extend this code:

1. **Extend the type** to a 3-tuple including the chat item id:
   ```haskell
   groupScopeRow :: (Maybe GroupChatScopeTag, Maybe GroupMemberId, Maybe ChatItemId)
   groupScopeRow = case groupScope of
     Just (Just GCSIMemberSupport {groupMember_}) ->
       (Just GCSTMemberSupport_, groupMemberId' <$> groupMember_, Nothing)
     Just (Just GCSIChannelMsg {channelChatItem}) ->
       (Just GCSTChannelMsg_, Nothing, Just (cChatItemId channelChatItem))
     _ -> (Nothing, Nothing, Nothing)
   ```
   where `cChatItemId :: CChatItem c -> ChatItemId` is the existing accessor (or pattern-match the constructor inline).

2. **Add the new column to the INSERT statement** at `Store/Messages.hs:586-596`. The current column list at line 588 is:
   ```sql
   user_id, created_by_msg_id, contact_id, group_id, group_member_id, note_folder_id,
   group_scope_tag, group_scope_group_member_id,
   ```
   Add `group_scope_chat_item_id` immediately after `group_scope_group_member_id`. Update the placeholder count at line 596 from 39 to 40 `?` marks.

3. **`includeInHistory` (Store/Messages.hs:625-628) needs a new arm for `GCSIChannelMsg`.** The existing code:
   ```haskell
   includeInHistory = case groupScope of
     Just Nothing -> isJust mcTag_ && mcTag_ /= Just MCReport_
     _ -> False
   ```
   Add an arm that admits channel-comment content items:
   ```haskell
   includeInHistory = case groupScope of
     Just Nothing                  -> isJust mcTag_ && mcTag_ /= Just MCReport_
     Just (Just GCSIChannelMsg {}) -> isJust mcTag_ && mcTag_ /= Just MCReport_
     _                             -> False
   ```
   This persists comments with `include_in_history = 1` so they are visible to `getGroupHistoryItems`. The per-parent comment cap is enforced at query time, not at insert time — see "History playback for new subscribers" below.

4. **Default-scope SELECT queries do NOT need updating.** Existing queries that filter by `group_scope_tag IS NULL AND group_scope_group_member_id IS NULL` (e.g., `Store/Messages.hs:891, 904, 1202, 1512, 1733`) already exclude channel comments correctly because comments have `group_scope_tag = 'channel_msg'`, which fails the `IS NULL` predicate. **No additional `group_scope_chat_item_id IS NULL` clause is needed** in these queries — but verify each one during implementation in case any predicate uses `OR` instead of `AND`.

5. **Read-side scope reconstruction in `toGroupChatItem`** (`Store/Messages.hs:2334`) reads the chat item row including `group_scope_tag` and `group_scope_group_member_id`. After adding `group_scope_chat_item_id`, the row tuple shape grows by one column and `toGroupChatItem` needs to thread it through. This is the same per-SELECT plumbing as for the new `commentsDisabled` / `commentsTotal` columns mentioned in the `prohibitedGroupContent` section.

### History playback for new subscribers

`getGroupHistoryItems` (`Store/Messages.hs:3656`) replays the recent channel posts to a new joiner. Extend this so that for each replayed post, up to `M` most-recent non-deleted comments under that post are also replayed. `M` (and the existing post-window cap `N`) are tunable constants chosen at implementation time.

Two pieces:

1. `includeInHistory` (`Store/Messages.hs:625-628`) admits `GCSIChannelMsg` content items so they are persisted with `include_in_history = 1` (covered in the Insert path section above).
2. `getGroupHistoryItems` runs an additional per-post sub-query against the new `idx_chat_items_channel_msg_scope_item_ts` index, ranked by `item_ts` descending and limited to `M`, applying the same soft-delete and report filters that the main history query already applies. The fetched comments are merged into the replay stream in `item_ts` order. A small helper `getChannelMsgCommentsForHistory db userId groupId parentChatItemId limit :: IO [CChatItem 'CTGroup]` lives next to `getGroupHistoryItems`; the caller iterates over the post ids it just selected.

Older joiners on a chat protocol version below `commentsVersion` cannot deserialize `MSChannelMsg`. The relay's existing per-recipient version gate drops the comment items from their replay stream so they receive the post-only history unchanged from today.

**`commentsTotal` consistency.** A new joiner receives the parent post row with its current `commentsTotal`, while only `M` of the comments are included in the initial replay. The Comments view fetches additional items on demand via the existing scope-pagination path; this is documented as expected UX, not a correctness issue.

### CLI / API surface (Haskell)

`Commands.hs`:

- New command `APISetCommentsDisabled groupId chatItemId disabled` — handler defined above in the "Per-message disable" section.
- Existing `/_get chat #N count=K` parser must accept `scope=channel_msg/<chatItemId>` so the Comments view can paginate.
- The `comments` group preference is set/cleared via the existing `APISetGroupPreference` path (`GroupPreferences` already round-trips an arbitrary preference set); no new preference command is needed.

Sender side: `APISendMessages` already accepts `scope :: Maybe GroupChatScope`. Comments are sent by passing `Just (GCSChannelMsg parentChatItemId)`.

Pretty-printed CLI shorthand for tests:

- `bob #_> #team channel_msg <itemId> hello back` — pseudo-syntax for `/_send #team(channel_msg=<itemId>) text hello back`
- `/_get chat #team count=5 scope=channel_msg/<itemId>` for pagination

The exact CLI grammar lives in the existing `chatCommand` parser; this is a small additive parse rule for the `(channel_msg=<id>)` form, mirroring the existing `(member_support=<id>)` form.

### iOS API types

`apps/ios/SimpleXChat/ChatTypes.swift`:

1. `GroupChatScope` (line 1905):
   ```swift
   public enum GroupChatScope: Decodable {
       case memberSupport(groupMemberId_: Int64?)
       case channelMsg(channelChatItemId: Int64)
   }
   ```

2. `sameChatScope` (line 1910): add the `(.channelMsg, .channelMsg)` arm comparing the chat item ids.

3. `GroupChatScopeInfo` (line 1923):
   ```swift
   public enum GroupChatScopeInfo: Decodable, Hashable {
       case memberSupport(groupMember_: GroupMember?)
       case channelMsg(channelChatItem: ChatItem)
   }
   public func toChatScope() -> GroupChatScope { ... }
   ```

4. `ChatItem` gets two new optional fields decoded from the new chat-item JSON:
   ```swift
   public var commentsTotal: Int = 0       // only meaningful on channel posts
   public var commentsDisabled: Bool = false
   ```
   Both must default to `0` / `false` in the manual `Decodable` init for forward compat.

`apps/ios/Shared/Model/SimpleXAPI.swift`:

- `apiSendMessages` (line 539) — already takes `scope: GroupChatScope?`. **No change.** Comments are sent via `apiSendMessages(type: .group, id: groupId, scope: .channelMsg(channelChatItemId: parentId), ...)`.
- New: `apiSetCommentsDisabled(_ groupId: Int64, _ chatItemId: Int64, _ disabled: Bool) async throws -> ChatItem` calling `.apiSetCommentsDisabled(groupId:chatItemId:disabled:)` and decoding the updated parent post.

`apps/ios/Shared/Model/AppAPITypes.swift`: add the new `ChatCommand` case `apiSetCommentsDisabled(...)` and its serialization.

### iOS state model

`apps/ios/Shared/Model/ChatModel.swift:694` — `matchesSecondaryIM` switch arms:

```swift
switch (cInfoScope, secondaryIM?.secondaryIMFilter) {
case let (.memberSupport, .some(.groupChatScopeContext(.memberSupport(_)))):
    ...existing...
case let (.channelMsg(parentLocal), .some(.groupChatScopeContext(.channelMsg(parentFilter)))):
    (cInfo.id == chatId && parentLocal.itemId == parentFilter.itemId) ? secondaryIM : nil
...
}
```

The chat-event reconciliation already routes scope-tagged events to the secondary IM via this matcher; the new arm is the only addition.

When a `ChatEvent` updates the parent post (e.g. `comments_disabled` toggled), the model's existing `upsertChatItem` path will reconcile the parent's `commentsTotal` / `commentsDisabled` fields automatically, since those are part of `ChatItem` JSON.

### iOS view layer

**Comments button on the channel post bubble.** The entry point is the channel post chat item. The Telegram-style "Comments" button sits below the bubble metadata, only on channel posts.

`apps/ios/Shared/Views/Chat/ChatItem/FramedItemView.swift` (or wherever the per-item meta row lives — locate by searching for `CIMetaView` callsites in channel-direction items):

- Add a small `CommentsButton(parent: ChatItem, groupInfo: GroupInfo)` view that renders only when the parent qualifies as a channel post for this user. The qualifier is: `groupInfo.useRelays && (parent.isChannelPost || (groupInfo.membership.memberRole >= .owner && parent.chatDir is .groupSnd))` — i.e. either the user receives it as a subscriber (`channelRcv` direction) OR the user is the channel owner viewing their own outgoing post. `isChannelPost` is the new computed property added in Slice 5 step 2 below; the owner-side check is composed at the call site.
- The button shows `"comments \(parent.commentsTotal)"` (zero-state shows just `"comments"`).
- On tap:
  ```swift
  let scopeInfo: GroupChatScopeInfo = .channelMsg(channelChatItem: parent)
  ItemsModel.loadSecondaryChat(groupInfo.id, chatFilter: .groupChatScopeContext(groupScopeInfo: scopeInfo)) {
      // navigate to SecondaryChatView with chat constructed below
  }
  ```
- Pushes a `SecondaryChatView` with `Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: scopeInfo), chatItems: [], chatStats: ChatStats())`. **`SecondaryChatView.swift` requires no change** — it already takes any chat with any scope.

**Toolbar.** `ChannelMsgChatToolbar.swift` (new, mirror of `MemberSupportChatToolbar.swift`): tiny 40-line view showing "Comments on:" with a 1-line preview of the parent post text. Wire into `ChatView`'s toolbar selector based on the chat scope.

**Owner controls.** On the channel post bubble, in the existing item context menu (long-press), add an "Disable / enable comments" item, gated to `groupInfo.isOwner || groupInfo.isAdmin || groupInfo.isModerator`. Tapping it calls `apiSetCommentsDisabled` and updates the local state.

**Composer gating.** In `ChatView`'s composer, when `chat.chatInfo.groupChatScope` is `.channelMsg`, the composer is disabled with a banner "Comments are closed" if any of: the parent's `commentsDisabled` is true; the group's comments preference is off; the post's age has exceeded the group's `closeAfter` window.

### iOS three-layer documentation updates

Per `apps/ios/CODE.md` Change Protocol:

- New `apps/ios/spec/client/comments.md` describing the comments view, the API surface, and the state matching logic.
- Update `apps/ios/spec/client/chat-view.md` to mention the comments button and the secondary view path.
- Update `apps/ios/spec/api.md` with the new `apiSetCommentsDisabled` signature and the `.channelMsg` scope variant.
- Update `apps/ios/spec/state.md` with the new `matchesSecondaryIM` arm.
- New `apps/ios/product/views/comments.md` describing the user-facing flow.
- Update `apps/ios/product/concepts.md` adding a "Comments" row.
- Update `apps/ios/product/glossary.md` defining "channel post comment".
- Update `apps/ios/product/rules.md` with the gating invariants (channels-only, role >= commenter, parent.commentsDisabled false, post age within `closeAfter` window).
- Update `apps/ios/spec/impact.md` to map the touched source files to the new product concept.
- Run the within-layer + across-layer review until two consecutive passes find zero issues.

## Implementation steps (slices)

Each slice ends with a build + test run that should pass. The order is chosen so each slice is independently mergeable.

### Slice 1 — Types, migration, role, preference (no UX yet)

1. New SQLite + Postgres migrations.
2. `GroupChatScope`/`Tag`/`Info` extension in `Messages.hs` (including `channelMsgSharedId :: SharedMsgId` field on `GCSIChannelMsg`).
3. `GroupMemberRole.GRCommenter` in `Types/Shared.hs` and its `TextEncoding`.
4. **Atomic with #3:** `Subscriber.hs:1522` `memberCanSend` `memberRole > GRObserver` → `memberRole >= GRAuthor`. This MUST land in the same commit as the role insertion to preserve send-permission semantics; otherwise commenters silently get non-comment send rights.
5. `Commands.hs:5193` role parser arm `" commenter" $> GRCommenter` — same commit as #3 so `/_member` and tests can address commenters.
6. `channelSubscriberRole` default change in `Controller.hs:160` from `GRObserver` to `GRCommenter`.
7. `CommentsGroupPreference` in `Types/Preferences.hs` (~22 mechanical sites). Includes the channel-creation default change in `Store/Groups.hs` so new channel groups default to `comments = CommentsGroupPreference {enable = FEOn, closeAfter = Nothing}` while non-channel groups default to `FEOff`.
8. Bump `commentsVersion :: VersionChat` in `Protocol.hs`, bump `currentChatVersion`.
9. Spot-check existing test fixtures: grep `tests/ChatTests/` for `"observer"` string assertions to confirm none of them depend on the default channel-subscriber role being `observer` (verified — the existing matches are all `/mr ... observer` member-role-change tests, not channel default-role tests).

Note: the new `commentsDisabled` / `commentsTotal` fields on `CIMeta` (and their JSON `omittedField` defaults) are added in Slice 2 step 3, not in Slice 1 — they require chat-item SELECT plumbing that fits naturally with the rest of the wire/scope work in Slice 2.

Build: `cabal build --ghc-options=-O0`.
Test: `cabal test simplex-chat-test --test-options='-m "channels"'` — must continue to pass with no comments tests yet.

### Slice 2 — Wire scope and send/receive plumbing

1. `MsgScope.MSChannelMsg` in `Protocol.hs` with derived JSON and version-gating in chatVersionRange checks.
2. `toMsgScope` arm in `Messages.hs` (using the new `channelMsgSharedId` field — no `error` branch).
3. Add `commentsDisabled :: Bool` and `commentsTotal :: Int` fields to `CIMeta` in `Messages.hs`, with default `False` and `0` for forward compat. Plumb both fields through every chat-item SELECT (`toGroupChatItem`, `toDirectChatItem`, etc.) — they're cheap fields and the existing chat-item SELECTs already enumerate every column individually. Add to `omittedField`-bearing JSON for forward-compat with old remote clients.
4. `getCreateGroupChatScopeInfo` arm + `getGroupChatScopeForItem_` 3-column read in `Store/Messages.hs`. The arm extracts `itemSharedMsgId` and raises `SEChatItemNotFound` if missing.
4a. **Critical insert-path extension** in `createNewChatItem_` (`Store/Messages.hs:581-624`): expand `groupScopeRow` to a 3-tuple, add the `GCSIChannelMsg` arm writing `(Just GCSTChannelMsg_, Nothing, Just chatItemId)`, add `group_scope_chat_item_id` to the INSERT column list, increment placeholder count from 39 to 40. Without this, channel comments would be saved as if they were main channel posts (silent data corruption).
4b. **Read-side plumbing** in `toGroupChatItem` (`Store/Messages.hs:2334`) and any other chat-item SELECT that reads `group_scope_*` columns: extend the row tuple to include `group_scope_chat_item_id`. Same per-SELECT plumbing as for the new `commentsDisabled` / `commentsTotal` columns.
5. `getChatScopeInfo` arm in `Internal.hs:1572` — same SharedMsgId-extraction smart constructor. **Includes signature change**: `getChatScopeInfo :: VersionRangeChat -> User -> GroupInfo -> GroupChatScope -> CM GroupChatScopeInfo`. All 9 call sites must pass `gInfo` (each already has it in scope; verified).
6. `mkGetMessageChatScope` arm in `Internal.hs:1626`.
7. `prohibitedGroupContent` channel-comment branch in `Internal.hs:341` (channels-only, comments-pref-on, parent-not-deleted, parent-comments-disabled). NO commenting-window check — the window check is the new helper below.
8. New `commentsClosed :: GroupInfo -> CChatItem 'CTGroup -> UTCTime -> Bool` helper in `Internal.hs`, called from the send wrapper in `Commands.hs` and the receive wrapper in `Subscriber.hs`.
9. `allowedRole` arm in `Commands.hs:4170`.
10. `memberCanSend` arm in `Subscriber.hs:1519` (the role-comparison fix for the `Nothing` arm already landed in Slice 1).
11. `infoToDeliveryContext` arm in `Delivery.hs:106`.
12. `groupMessageDelete` moderation arm for `MSChannelMsg` in `Subscriber.hs:2125`.
13. `getChatItemIDs` arm in `Store/Messages.hs:1506`.
14. `adjustChannelMsgCommentCount` helper in `Store/Messages.hs` + guarded +1 inside `createNewChatItem_` (after the INSERT, gated on `groupScope = Just (Just GCSIChannelMsg {})`) so both `createNewSndChatItem` and `createNewRcvChatItem` benefit without duplication. Guarded −1 in the deletion path (`deleteChatItemBy*` and `markChatItemDeleted`), only when the previous row was not already marked deleted.
15. **History playback**: extend `includeInHistory` (`Store/Messages.hs:625-628`) with the `GCSIChannelMsg` arm, add `getChannelMsgCommentsForHistory` helper, and extend `getGroupHistoryItems` to merge per-parent comments into the replay stream (see "History playback for new subscribers" section). Tunable constants (`N` recent posts, `M` comments per post) live next to the existing history caps.

Build + test as Slice 1.

### Slice 3 — Per-message disable

1. New `XGrpCommentsDisabled` event in `Protocol.hs`.
2. `xGrpCommentsDisabled` handler in `Subscriber.hs`.
3. `APISetCommentsDisabled` command + handler in `Commands.hs`.
4. `setChannelMsgCommentsDisabled` helper in `Store/Messages.hs`.
5. Wire the event into `processForwardedMsg` for relay forwarding.

Build + test as before.

### Slice 4 — Haskell tests

`tests/ChatTests/Groups.hs` add `describe "channel comments"` block (after `describe "channel message operations"` at line ~281). Tests:

1. `testChannelCommentSubscriberCanComment` — owner posts a channel message; bob (subscriber) comments; cath, dan, eve all receive bob's comment via the relay.
2. `testChannelCommentRcvFromAnotherSubscriber` — cath comments; bob receives via relay; bob sees cath as unknown member.
3. `testChannelCommentQuoteParent` — cath's comment quotes the original channel post; subscribers see the quote shape.
4. `testChannelCommentQuoteAnotherComment` — dan's comment quotes bob's earlier comment; quoting flows correctly.
5. `testChannelCommentEditDelete` — author edits and deletes their own comment; receivers reconcile.
6. `testChannelCommentModerationDelete` — owner deletes someone else's comment via moderation.
7. `testChannelCommentObserverRejected` — in a channel group, downgrade a subscriber to `GRObserver` via `/mr team bob observer`; the subscriber's comment is rejected by `memberCanSend`.
8. `testChannelCommentDisabledRejected` — owner toggles `comments_disabled` on a post; subscriber's comment is rejected by the relay.
9. `testChannelCommentClosingWindow` — group has `comments.closeAfter = 1`; an old post can no longer accept new comments.
10. `testChannelCommentPrefOff` — group `comments.enable = FEOff`; all comment sends are rejected.
11. `testChannelCommentNotInRegularGroup` — a regular group rejects `MSChannelMsg` scope (server-side and client-side preflight).
12. `testChannelCommentCountIncrement` — `comments_total` increments and decrements with insert/delete.
13. `testChannelCommentReplayedToNewSubscriberWithCap` — eve joins after several comments exist on a post; eve receives the parent post AND up to `M` most recent comments (the per-parent cap is enforced and older comments beyond the cap are NOT replayed).
14. `testChannelCommentOwnerSentAsGroupNoLeak` — owner posts a channel message and adds an own comment with `sentAsGroup = True`; subscribers must not see the owner's member identity in the comment. Mirrors `testChannelOwnerReaction` (`tests/ChatTests/Groups.hs:9482`) and `testChannelOwnerQuote` (`tests/ChatTests/Groups.hs:9510`) — required by threat-model item #7.
15. `testChannelCommentRoundtripJSON` — a `JSONTests.hs` round-trip case for `MSChannelMsg` and the new `XGrpCommentsDisabled` event.

Each test follows the existing `testChannelMessage*` pattern: `alice #> "#team post"`, `bob #_> "#team channel_msg <id> reply"`, `[cath, dan, eve] *<# "#team bob> reply [>>]"`, `#$> ("/_get chat #1 scope=channel_msg/<id> count=...", chat, [...])`.

Build + test: `cabal test simplex-chat-test --test-options='-m "channel comments"'`.

### Slice 5 — iOS API types and state

1. `ChatTypes.swift`: `GroupChatScope.channelMsg`, `GroupChatScopeInfo.channelMsg`, `sameChatScope` arm, `toChatScope` arm.
2. `ChatItem` adds `commentsTotal`, `commentsDisabled` decoding with defaults, AND a new computed property `isChannelPost: Bool` returning `chatDir == .channelRcv` (subscriber-side view) — owners checking their own channel posts use a separate predicate at the call site that combines `useRelays` and ownership.
3. `AppAPITypes.swift`: new `apiSetCommentsDisabled` `ChatCommand` case.
4. `SimpleXAPI.swift`: new `apiSetCommentsDisabled(...)` function.
5. `ChatModel.swift:694` `matchesSecondaryIM` arm.

iOS build via Xcode (out of scope for the Haskell test environment; built separately).

### Slice 6 — iOS UI

1. `Comments(N)` button on channel post bubble (locate the right `ChatItem` view file for channel posts in `apps/ios/Shared/Views/Chat/ChatItem/`).
2. New `ChannelMsgChatToolbar.swift` mirroring `MemberSupportChatToolbar.swift`.
3. Wire `SecondaryChatView` push from the Comments button.
4. Composer gating banner when comments are disabled / pref off / commenting window closed.
5. Owner-only "Disable comments" / "Enable comments" item in the per-message context menu.
6. iOS three-layer documentation updates.

### Slice 7 — Adversarial review and final test pass

1. Re-read every changed file once more end-to-end.
2. Run the full `cabal test simplex-chat-test` suite (not just channels).
3. Run `JSONTests.hs` and `ProtocolTests.hs` to catch JSON / protocol regressions.
4. Two consecutive clean adversarial self-review passes.

## Forward compatibility and threat model

**Forward compatibility.** Older clients and relays that have not upgraded past `commentsVersion`:

- Cannot parse `MSChannelMsg` (strict tagged JSON) → would fail-deserialize the entire message.
- Mitigation: relay must NOT forward `XMsgNew` events whose `ExtMsgContent.scope` is `Just MSChannelMsg{}` to subscribers whose chat protocol version is below `commentsVersion`.
- Same gating for `XGrpCommentsDisabled` events.
- The relay's existing per-recipient version check (used for `groupKnockingVersion`, `contentReportsVersion`, etc.) is the place to add the gating.
- New optional `commentsTotal` / `commentsDisabled` fields on chat-item JSON ship with `omittedField` defaults so older remote-connection clients see chat items normally.

**Threat model.**

1. Malicious subscriber tries to flood comments → existing per-relay rate limits apply unchanged. Owner can disable comments per-post or globally via the group preference.
2. Malicious subscriber tries to comment on a non-channel group → `prohibitedGroupContent`'s `not (useRelays' gInfo) -> Just GFComments` arm rejects on both send-side preflight (`Commands.hs sendGroupContentMessages_`) and receive-side (`Subscriber.hs newGroupContentMessage`), saving a `CIRcvGroupFeatureRejected GFComments` item.
3. Malicious subscriber tries to comment on a deleted parent post → cascade FK deletes the comment row when the parent is deleted; the receive path's `mkGetMessageChatScope` returns `SEChatItemNotFound` and the message is dropped.
4. Malicious subscriber tries to disable comments without permission → `xGrpCommentsDisabled` handler asserts `memberRole >= GRModerator`.
5. Malicious relay tries to forward a comment with a forged parent SharedMsgId → resolution fails on receive (`getGroupChatItemBySharedMsgId` returns not-found) and the message is dropped.
6. Malicious owner adversary tries to lie about `comments_disabled` to the relay → no different from any other group governance event; the relay's role check is the trust boundary.
7. Comment author identity leak → comment messages from owners as-channel use the existing `sentAsGroup` path. Comments from owners as-member use the member identity. The existing channel reaction/quote anti-leak tests (`testChannelOwnerReaction`, `testChannelOwnerQuote`) are the template — add equivalent comment leak tests in Slice 4.

8. Cascade FK and comment count drift → when a channel post is deleted, the new `group_scope_chat_item_id ON DELETE CASCADE` removes all child comment rows in a single SQLite cascade. Because the parent row itself goes away, the now-orphan `comments_total` value disappears with it — there is no count drift. For interleaved comment-by-comment deletes the count is maintained incrementally via `adjustChannelMsgCommentCount` and clamped at zero with `MAX(0, ...)` so concurrent transactions that race a +1 against a −1 cannot leave a negative value. Implementation caveat: SQLite cascades through one level only by default, but child→child cascading is not needed (comments do not chain to other comments).

9. Replay attack on `XGrpCommentsDisabled` → if a relay re-broadcasts an old (stale) `XGrpCommentsDisabled false` after a newer `... true` has taken effect, subscribers would silently re-enable a disabled post. Mitigation: the handler reads the current `comments_disabled` value and ignores updates older than the latest known event timestamp (`itemTs` of the carrying message vs. the parent's `chat_ts` of the latest disable event). For MVP, the simpler mitigation is to always trust the latest delivery — channel governance has no replay protection today, and `XGrpCommentsDisabled` is no different from existing relay-broadcast events. Document as a known limitation in `product/rules.md`.

## Out of scope (deferred)

- Subscriber profile dissemination (the existing "unknown member" path is the MVP).
- Kotlin Multiplatform (Android/Desktop) port.
- Notification routing for comments (no per-comment notifications in MVP).
- Comment count indicator on the channel chat list (no per-chat unread badge for comments).
- A "list of comment threads" view for owners (no analog to `MemberSupportView`).
- A separate `DeliveryWorkerScope` for comments (they share the channel post worker for batching).

## Critical files to be modified

Backend:
- `src/Simplex/Chat/Protocol.hs` — `MsgScope`, `commentsVersion`, `XGrpCommentsDisabled`
- `src/Simplex/Chat/Messages.hs` — `GroupChatScope`/`Tag`/`Info`, `toMsgScope`
- `src/Simplex/Chat/Types/Shared.hs` — `GRCommenter`
- `src/Simplex/Chat/Types/Preferences.hs` — `CommentsGroupPreference` (~22 sites)
- `src/Simplex/Chat/Controller.hs` — `channelSubscriberRole` default
- `src/Simplex/Chat/Library/Internal.hs` — `getChatScopeInfo`, `mkGetMessageChatScope`, `prohibitedGroupContent`, `getGroupRecipients`
- `src/Simplex/Chat/Library/Commands.hs` — `allowedRole`, `APISetCommentsDisabled`
- `src/Simplex/Chat/Library/Subscriber.hs` — `memberCanSend`, `xGrpCommentsDisabled`, `groupMessageDelete` moderation arm
- `src/Simplex/Chat/Delivery.hs` — `infoToDeliveryContext`
- `src/Simplex/Chat/Store/Messages.hs` — `getCreateGroupChatScopeInfo`, `getGroupChatScopeForItem_`, `getChatItemIDs`, `adjustChannelMsgCommentCount`, `setChannelMsgCommentsDisabled`
- `src/Simplex/Chat/Store/Groups.hs` — `defaultGroupPrefs` for new channels
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260501_channel_comments.hs` (new)
- `src/Simplex/Chat/Store/Postgres/Migrations/M20260501_channel_comments.hs` (new)
- `src/Simplex/Chat/Store/SQLite/Migrations.hs`, `src/Simplex/Chat/Store/Postgres/Migrations.hs`
- `simplex-chat.cabal`

Tests:
- `tests/ChatTests/Groups.hs` — `describe "channel comments"`
- `tests/JSONTests.hs` — `MSChannelMsg`, `XGrpCommentsDisabled` round trips

iOS:
- `apps/ios/SimpleXChat/ChatTypes.swift` — `GroupChatScope`, `GroupChatScopeInfo`, `ChatItem`
- `apps/ios/Shared/Model/AppAPITypes.swift` — new `ChatCommand` case
- `apps/ios/Shared/Model/SimpleXAPI.swift` — `apiSetCommentsDisabled`
- `apps/ios/Shared/Model/ChatModel.swift` — `matchesSecondaryIM` arm
- `apps/ios/Shared/Views/Chat/ChatItem/<channel post item view>.swift` — Comments button + context-menu items
- `apps/ios/Shared/Views/Chat/Group/ChannelMsgChatToolbar.swift` (new)
- `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift` — composer gating banner
- `apps/ios/spec/client/comments.md` (new), `chat-view.md`, `api.md`, `state.md`, `impact.md`
- `apps/ios/product/views/comments.md` (new), `concepts.md`, `glossary.md`, `rules.md`

## Verification

Backend (run by Claude in this environment):

1. `cabal build --ghc-options=-O0` after each slice; must succeed with no warnings beyond the existing baseline.
2. `cabal test simplex-chat-test --test-options='-m "channels"'` after Slice 4; all existing channel tests still pass and the new `channel comments` describe-block passes.
3. `cabal test simplex-chat-test --test-options='-m "JSON"'` after Slice 2; round-trips for the new `MsgScope` variant and the new event hold.
4. `cabal test simplex-chat-test` (full) after Slice 7; full suite green.
5. After Slice 1, manually inspect the generated `chat_schema.sql` to confirm the new columns and indexes are present.

iOS (run in Xcode):

1. Build the iOS app target in Xcode after Slice 5.
2. Manual test in iOS simulator: create a channel as Alice, join from Bob, post a message from Alice, comment from Bob, verify the comments button shows `comments 1`, open the secondary view, post another comment, edit, delete, quote both the parent post and another comment.
3. As Alice, disable comments on the post, verify Bob's composer goes inactive and a banner appears, verify Bob cannot send a comment, re-enable, verify Bob can send again.
4. Set `comments.closeAfter = 60` in the group preference, post a message, wait, verify commenting is rejected after 60 seconds.
5. Run iOS test suite (`xcodebuild test ...`).

A change is complete only after two consecutive adversarial self-review passes find zero issues at the end of Slice 7.
