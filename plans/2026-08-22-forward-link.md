# Forward attribution: `forwardLink` in MsgContainer

When a message is forwarded from a channel (public group), the sending client
attaches the source channel's name, join link, identity and message id;
recipients see "forwarded from \<name\>" or
"forwarded from \<name\> (#\<simplex name\>)" and can open or join the channel.

- The link is attached whenever the source is a public group; for other sources
  only `forward: true` is sent.
- `forward = Just True` is always set alongside `forwardLink`, so old clients
  show plain "forwarded".
- When a forwarded message is received in a group that prohibits SimpleX links
  for the sender, the link and simplex name (a name resolves like a link) are
  removed.

## Protocol

`Protocol.hs`. aeson ignores unknown fields and parses an absent field as
`Nothing`, so the addition is compatible in both directions.

```haskell
data ForwardLink = ForwardLink
  { displayName :: Text,
    groupLink :: ShortLinkContact,
    publicGroupId :: B64UrlByteString, -- the recipient looks up the local group by this id, then compares groupLink with the stored link
    simplexName :: Maybe (StrJSON "SimplexDomain" SimplexDomain),
    msgId :: SharedMsgId -- the original item's SharedMsgId
  }
```

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
               chatItemId :: Maybe ChatItemId, chatLinkShared :: BoolDef}
  | CIFFGroupLink {chatName :: Text, msgDir :: MsgDirection,
                   groupLink :: ShortLinkContact, publicGroupId :: B64UrlByteString,
                   simplexName :: Maybe (StrJSON "SimplexDomain" SimplexDomain),
                   sharedMsgId :: SharedMsgId}
```

- `chatLinkShared :: BoolDef` (Types.hs:2267; `omittedField = Just (BoolDef
  False)`, so JSON serialized before this change parses with the field set to
  false). `BoolDef True` records that the sent message included the link. The
  name is not stored: for a locally known group the UI shows
  "forwarded from \<chatName\>" without the simplex name.
- `CIFFGroupLink` is the recipient's variant for an unknown channel; the user
  opens it via the connection plan.
- New tag `CIFFGroupLink_` / `"groupLink"` in `CIForwardedFromTag` (:1325).

## Sending

`Commands.hs` `APIForwardChatItems`, `prepareForward` group branch (:1094-1110):

- Build `Maybe ForwardLink` from the source group's
  `groupProfile.publicGroup :: Maybe PublicGroupProfile` (Types.hs:872), which
  includes `groupLink`, `publicGroupId`, and
  `publicGroupAccess >>= groupDomainClaim` for the domain (`claimDomain`, sent
  as stored, without regard to local verification state - the recipient
  verifies it when tapping the name). `displayName` from `GroupProfile`. `msgId` = the item's
  `CIMeta.itemSharedMsgId`; if it is `Nothing`, omit the whole `ForwardLink`.
- Local `ciff`: `CIFFGroup ... {chatLinkShared = BoolDef (isJust forwardLink_)}`.
- The two `mcForward` call sites - `sendContactContentMessages.prepareMsgs`
  (Commands.hs:4772) and `prepareGroupMsg` (Internal.hs:208-209), both matching
  `(Nothing, Just _) -> pure (mcForward mc, Nothing)` on
  `(quotedItemId, itemForwarded)` - compute the link from the
  `CIForwardedFrom` in scope: `ciffForwardLink db ciff` returns the link for
  `CIFFGroup` from the group profile read by `groupId` (current name and
  link), for `CIFFGroupLink` from its stored fields, and `Nothing` for other
  variants. Deriving from the stored `CIForwardedFrom` attributes a
  re-forwarded message to the original source.
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
   remove the link and the simplex name: store `CIFFGroup {chatName =
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
   query `getGroupInfoByPublicGroupId`; on a match, compare the received
   `groupLink` with the stored one (`sameShortLinkContact`); when both match ->
   `CIFFGroup {groupId = Just gId, chatItemId = Nothing, chatLinkShared =
   BoolDef True}`.
4. Lookup miss, or the link differs from the stored one -> `CIFFGroupLink`
   with the wire fields.

## DB

`chat_items` persists `CIForwardedFrom` as columns (`fwd_from_tag,
fwd_from_chat_name, fwd_from_msg_dir, fwd_from_contact_id, fwd_from_group_id,
fwd_from_chat_item_id`, Store/Messages.hs:606). Migration (SQLite + Postgres,
same shape) adds:

- `fwd_chat_link_shared INTEGER` (0/1; NULL is read as false; `chatLinkShared`)
- `fwd_from_group_link TEXT` (strEncoded ShortLinkContact)
- `fwd_from_public_group_id BLOB/BYTEA`
- `fwd_from_simplex_name TEXT`
- `fwd_from_shared_msg_id BLOB/BYTEA`

Code changes: the CIFF-to-row tuple (Store/Messages.hs:657-660), the
row-to-CIFF case (:2343-2344), the three SELECT lists (:2696, :3085, :3197),
and the INSERT statement in `createNewChatItem_`. Binary columns use `Binary`
on both backends.

## View / UI

- `View.hs:1010`: render the source name for `CIFFGroup`; for `CIFFGroupLink`
  render the name and, when present, the simplex name.
- The `CIForwardedFrom` JSON reaches the apps in `CIMeta`: the iOS
  (`ChatTypes.swift`) and Kotlin (`ChatModel.kt`) mirrors are extended with the
  new field and variant. Header text: `CIFFGroup` ->
  "forwarded from \<chatName\>"; `CIFFGroupLink` ->
  "forwarded from \<chatName\> (#\<simplexName\>)" when the name is present,
  otherwise "forwarded from \<chatName\>". Tapping the header opens the
  connection plan for `groupLink` (existing planAndConnect paths). The name
  renders as plain text - it is the forwarder's claim, not a verified value.

## Tests

`ChatTests/Groups.hs`:
1. Forward from a channel to a direct chat: the recipient item includes
   `CIFFGroupLink` with name/link/publicGroupId/msgId; the view shows
   "forwarded from" with the name.
2. Forward to a group where the recipient is a member of the source channel:
   the recipient stores `CIFFGroup` with the local groupId.
3. Destination group with SimpleX links prohibited: the link and simplex name
   are removed; attribution text only.
4. Forwarding a received forwarded item again sends the original channel's
   link.
5. Old-client compatibility: a container with `forward: true` and no
   `forwardLink` parses to `CIFFUnknown`.
6. Private (non-public) source group: the container includes no `forwardLink`.
