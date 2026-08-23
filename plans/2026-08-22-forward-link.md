# Forward attribution: `forwardLink` in MsgContainer

When a message is forwarded from a channel (public group), the sending client
attaches the source channel's name, join link, identity and message id;
recipients see "forwarded from \<name\>" and can open or join the channel.

- The link is attached whenever the source is a public group; for other sources
  only `forward: true` is sent.
- `forward = Just True` is always set alongside `forwardLink`, so old clients
  show plain "forwarded".
- The simplex name is not included: paired with a forwarder-chosen link it
  would be an unverifiable claim. It can be added later as a verifiable claim.
- When a forwarded message is received in a group that prohibits SimpleX links
  for the sender, the link is removed.

## Protocol

`Protocol.hs`. aeson ignores unknown fields and parses an absent field as
`Nothing`, so the addition is compatible in both directions.

```haskell
data ForwardLink = ForwardLink
  { displayName :: Text,
    groupLink :: ShortLinkContact,
    publicGroupId :: B64UrlByteString, -- the recipient looks up the local group by this id, then compares groupLink with the stored link
    memberId :: Maybe MemberId, -- the author, only for items the author sent as themselves
    msgId :: SharedMsgId -- the original item's SharedMsgId
  }
```

`memberId` is absent for items sent as the channel: their authorship is the
channel's, and subscribers do not see the author's member id. The fill rule is
`chatItemMember` (Messages.hs:369): the member for received authored items,
the membership for own items sent as themselves, absent otherwise.

- New field `forwardLink :: Maybe ForwardLink` in `MsgContainer`
  (Protocol.hs:678) after `forward`; `mcSimple` (:695) sets
  `forwardLink = Nothing`.
- `mcForward` (:716) takes `Maybe ForwardLink`:
  `mcForward fl c = (mcSimple c) {forward = Just True, forwardLink = fl}`.
- JSON instances: `deriveJSON defaultJSON ''ForwardLink` before the
  `''MsgContainer` splice (:899).

## CIForwardedFrom

`Messages.hs:1319`:

```haskell
  | CIFFGroup {chatName :: Text, msgDir :: MsgDirection, groupId :: Maybe GroupId,
               chatItemId :: Maybe ChatItemId, memberId :: Maybe MemberId,
               itemSharedMsgId :: Maybe SharedMsgId, chatLinkShared :: BoolDef}
  | CIFFGroupLink {chatName :: Text, msgDir :: MsgDirection,
                   groupLink :: ShortLinkContact, publicGroupId :: B64UrlByteString,
                   memberId :: Maybe MemberId, sharedMsgId :: SharedMsgId}
```

Both variants retain the wire `memberId` and `sharedMsgId`, so a re-forward
re-serializes `ForwardLink` from the CIFF without item lookups.

- `chatLinkShared :: BoolDef` (Types.hs:2267; `omittedField = Just (BoolDef
  False)`, so JSON serialized before this change parses with the field set to
  false). `BoolDef True` records that the sent message included the link.
- `CIFFGroupLink` is the recipient's variant for an unknown channel; the user
  opens it via the connection plan.
- New tag `CIFFGroupLink_` / `"groupLink"` in `CIForwardedFromTag` (:1325).

## Sending

`Commands.hs` `APIForwardChatItems`, `prepareForward` group branch (:1094-1110):

- Local `ciff`: `CIFFGroup` with `memberId = memberId' <$> chatItemMember
  gInfo ci`, the item's `itemSharedMsgId`, and `chatLinkShared = BoolDef
  linkShared` where `linkShared = sourcePublic gInfo && isJust
  itemSharedMsgId` - the same condition under which `ciffForwardLink` later
  returns a link (the link value is computed at the `mcForward` call site,
  after the `ciff` is built).
- The two `mcForward` call sites - `sendContactContentMessages.prepareMsgs`
  (Commands.hs:4772) and `prepareGroupMsg` (Internal.hs:208-209), both matching
  `(Nothing, Just _) -> pure (mcForward mc, Nothing)` on
  `(quotedItemId, itemForwarded)` - compute the link from the
  `CIForwardedFrom` in scope: `ciffForwardLink db ciff` returns the link for
  `CIFFGroup` with `groupId` and `sharedMsgId` set, reading the group profile
  (current name and link), for `CIFFGroupLink` from its stored fields, and
  `Nothing` for other variants. Deriving from the stored `CIForwardedFrom`
  attributes a re-forwarded message to the original source.
- `forwardCIFF` (:1130) already returns the original `CIForwardedFrom` when a
  forwarded item is forwarded again, so a received `CIFFGroupLink` item is sent
  onwards with the same link.

## Receiving

`Store/Messages.hs createNewRcvChatItem` (:563-572), inside the existing DB
transaction:

```haskell
itemForwarded = case chatMsgEvent of
  ACME _ (XMsgNew MsgContainer {forward, forwardLink}) | forward == Just True -> ...
```

1. `forwardLink = Nothing` -> `CIFFUnknown` (today's behavior).
2. Destination is a group where SimpleX links are prohibited for the sender ->
   remove the link: store `CIFFGroup {chatName =
   displayName, msgDir = MDRcv, groupId = Nothing, chatItemId = Nothing,
   chatLinkShared = BoolDef False}` - attribution text only. The check runs
   where `itemForwarded` is computed today: `chatDirection` is in scope, and
   `CDGroupRcv gInfo _ member` (Messages.hs:395) includes both values for
   `groupFeatureMemberAllowed SGFSimplexLinks member gInfo` (the check used by
   `redactedMemberProfile`, Internal.hs:1266). `CDChannelRcv gInfo _` includes
   no sender member (a message from the channel), so the feature's enable
   state is checked without a member role. Direct chats: the link is kept.
3. Lookup by `publicGroupId`: `group_profiles.public_group_id` is a column
   with an existing query that filters on it (Store/Groups.hs:2009-2015). New
   query `getGroupViaPublicGroupId`; on a match, compare the received
   `groupLink` with the stored one (`sameShortLinkContact`); when both match ->
   `CIFFGroup` with `groupId`, the wire `memberId` and `msgId`, `chatLinkShared
   = BoolDef True`, and `chatItemId = ciId_` resolved by the id query factored
   out of `getGroupChatItemBySharedMsgId` (`getGroupChatItemBySharedMsgId_`).
   The author scope: wire `memberId` absent -> `Nothing` (items sent as the
   channel and own items are stored with `group_member_id` NULL); present ->
   the member resolved by `member_id`, with the user's own membership mapped
   to `Nothing`; an unknown member -> no item.
4. Lookup miss, or the link differs from the stored one -> `CIFFGroupLink`
   with the wire fields.

## DB

`chat_items` persists `CIForwardedFrom` as columns (`fwd_from_tag,
fwd_from_chat_name, fwd_from_msg_dir, fwd_from_contact_id, fwd_from_group_id,
fwd_from_chat_item_id`, Store/Messages.hs:606). Migration (SQLite + Postgres,
same shape) adds:

- `fwd_chat_link_shared INTEGER` (0/1; NULL is read as false; `chatLinkShared`)
- `fwd_from_group_link BLOB/BYTEA` (the `ToField (ConnShortLink c)` instance
  stores `Binary . strEncode`, matching `short_link_contact`)
- `fwd_from_public_group_id BLOB/BYTEA`
- `fwd_from_member_id BLOB/BYTEA`
- `fwd_from_shared_msg_id BLOB/BYTEA`

Code changes: the CIFF-to-row tuple (Store/Messages.hs:657-660), the
row-to-CIFF case (:2343-2344), the three SELECT lists (:2696, :3085, :3197),
and the INSERT statement in `createNewChatItem_`. Binary columns use `Binary`
on both backends.

## View / UI

- `View.hs:1010`: render the source name for `CIFFGroup` and `CIFFGroupLink`.
- `/item info` renders "forwarded from: #\<chatName\>" from `itemForwarded`
  when the source item is not stored locally (`CIFFGroupLink` and link-removed
  `CIFFGroup`).
- The `CIForwardedFrom` JSON reaches the apps in `CIMeta`: the iOS
  (`ChatTypes.swift`) and Kotlin (`ChatModel.kt`) mirrors are extended with the
  new field and variant. Header text for `CIFFGroup` and `CIFFGroupLink`:
  "forwarded from \<chatName\>". Tapping the header opens the
  connection plan for `groupLink` (existing planAndConnect paths).

## Tests

`ChatTests/Groups.hs`:
1. Forward from a channel to a direct chat: the recipient item includes
   `CIFFGroupLink` with name/link/publicGroupId/msgId; the view shows
   "forwarded from" with the name.
2. Forward to a group where the recipient is a member of the source channel:
   the recipient stores `CIFFGroup` with the local groupId.
3. Destination group with SimpleX links prohibited: the link is removed;
   attribution text only.
4. Forwarding a received forwarded item again sends the original channel's
   link.
5. Old-client compatibility: a container with `forward: true` and no
   `forwardLink` parses to `CIFFUnknown`.
6. Private (non-public) source group: the container includes no `forwardLink`.
