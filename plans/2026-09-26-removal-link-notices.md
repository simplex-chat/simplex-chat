# Link notices: restrict reconnecting via a link

A group admin removes a member, a group owner deletes the group, or a user
deletes a contact, with an optional notice. The receiving client stores the
notice under the hash of the link it joined or connected through. While the
notice is in effect, the connection plan for that link returns the notice, and
the connect commands fail with it.

Rules:

- Notice key, read only from the receiving client's own records:

```
group removal, group deletion   groups.via_group_link_uri_hash
contact deletion                connections.via_contact_uri_hash of the contact connection
```

- A notice received in a record with a NULL link hash leaves the notices
  unchanged. Group records of members added by direct invitation, and contacts
  connected via invitation links, have a NULL link hash.
- Group deletion: the group links of p2p groups and channels are deleted with
  the group, so their notices have no effect. A business address remains after
  a business chat is deleted, so the notice applies to it.
- Notices apply to all user profiles on the device.
- Trust: the owners of the link are trusted - the group link owner and admins,
  the channel owners and relays, the address owner.
- Expiry: `ttl` added to the message's server timestamp. Absent `ttl`:
  indefinite.
- An expiry after `9999-12-31T23:59:59Z` is stored as indefinite. SQLite stores
  times as text with a padded 4-digit year (`dayToBuilder`, sqlcipher-simple
  `Time/Implementation.hs:125-127`).
- A new notice for the same link replaces the previous one.
- A message without a notice leaves the notices unchanged.
- A malformed notice is ignored; the message is applied.
- Terms: the protocol, types and API use "notice". The UI uses "Ban".

## Protocol

`Protocol.hs`, after the `ReportReason` instances (:279-301):

```haskell
data LinkNotice = LinkNotice
  { ttl :: Maybe Int64, -- seconds
    reason :: Maybe ReportReason
  }
```

- JSON: `deriveJSON defaultJSON ''LinkNotice`.
- `ReportReason` DB instances, TEXT: `toField . safeDecodeUtf8 . strEncode`,
  `fromTextField_ (eitherToMaybe . strDecode . encodeUtf8)`.

Message parameters:

```
x.grp.mem.del   notice
x.grp.del       notice
x.direct.del    notice
x.direct.del    silent
```

- `notice` is parsed as `Right (fromRight Nothing $ opt "notice")`, `silent` as
  `Right (fromRight False $ p "silent")`, following `messages` (:1453).
- `XGrpMemDel :: MemberId -> Bool -> Maybe VersionRoster -> Maybe LinkNotice`
  (:488). Parser :1453, encoder :1528 `("notice" .=? notice)`. Matches:
  `Subscriber.hs:1096`, `:3977`; `Commands.hs:3148`.
- `XGrpDel :: Maybe LinkNotice` (:490). Parser :1455, encoder :1530
  `o $ ("notice" .=? notice) []`. Matches: `Protocol.hs:537`, `:1283`;
  `Subscriber.hs:1100`, `:1123`, `:3979`; `Commands.hs:1394`.
- `XDirectDel :: Bool -> Maybe LinkNotice` (:466). Parser :1428, encoder :1504
  `o $ ("silent" .=? justTrue silent) $ ("notice" .=? notice) []`. Matches:
  `Protocol.hs:1259`; `Subscriber.hs:587`; `Commands.hs:1372`.
- `docs/protocol/simplex-chat.md`: document the parameters (:141, :257, :261).

Old clients read named keys only, so the parameters are ignored.

Chat version 21, `linkNoticeVersion` (:70-141): support for link notices.

## Report reason

The apps send and show `"content"` for "Inappropriate content".

- Core `ReportReason` parser (`Protocol.hs:287-294`): `"illegal"` also parses
  as `RRContent`. The encoding stays `"content"`.
- iOS `ReportReason` (`ChatTypes.swift:5777-5834`): case `illegal` renamed to
  `content`; encodes `"content"`; decodes `"content"` and `"illegal"`. Usages:
  `ChatTypes.swift:5785`, `:5790`; `ComposeView.swift:1424`.
- `/_report` command (`AppAPITypes.swift:266`): the reason is the encoded value,
  instead of the interpolated case name.
- Kotlin `ReportReason` (`ChatModel.kt:5294-5335`): `Illegal` renamed to
  `Content`, `@SerialName("content")`; the serializer decodes `"content"` and
  `"illegal"`. Usages: `ChatModel.kt:5303`, `:5308`; `ComposeView.kt:1241`.
- String keys `report_reason_illegal` and `report_compose_reason_header_illegal`
  are unchanged.
- Apps before this change show `"content"` as raw text.
- `ProtocolTests.hs`: `MCReport` with `"illegal"` parses as `RRContent`.

## Member removal

- `APIRemoveMembers {groupId, groupMemberIds, withMessages, notice :: Maybe LinkNotice}`
  (`Controller.hs:471`).
- Syntax (`Commands.hs:6148`):
  `/_remove #<groupId> <groupMemberIds>[ messages=on|off][ notice=<json>]`.
- `RemoveMembers` (`Controller.hs:620`, `Commands.hs:3248`, `:6248`): the same
  optional `notice=<json>`.
- `deleteMemsSend` (`Commands.hs:3143-3148`) includes the notice in each
  `XGrpMemDel`: current members, and members pending approval or review.
  Members with status `GSMemInvited` are deleted locally (`deleteInvitedMems`).
- `xGrpMemDel` (`Subscriber.hs:3681`): new `Maybe LinkNotice` parameter after
  `rosterVer_`, passed at :1097 and :3977. Membership branch (:3685-3696), in
  the existing `withStore'` block after `updateGroupMemberStatus`:
  `forM_ notice_ $ setGroupLinkNotice db gInfo brokerTs`.
- For forwarded messages `brokerTs` is `fwdBrokerTs`, set by the forwarding
  member (`Subscriber.hs:3930`, `:3977`).

## Group deletion

- `APIDeleteChat {chatRef, chatDeleteMode, notice :: Maybe LinkNotice}`
  (`Controller.hs:433`). Syntax (`Commands.hs:6106`):
  `/_delete <chatRef> <chatDeleteMode>[ notice=<json>]`.
- Groups: the notice is included in `XGrpDel` (`Commands.hs:1391-1394`).
  `DeleteGroup` (`Commands.hs:3262`) passes `Nothing`.
- Contact connections with a notice: command error.
- `xGrpDel` (`Subscriber.hs:3778`): new `Maybe LinkNotice` parameter. In the
  `withStore'` block (:3782), after the status update:
  `forM_ notice_ $ setGroupLinkNotice db gInfo brokerTs`.

## Contact deletion

`APIDeleteChat`, direct chats (`Commands.hs:1341-1368`). `sendDelDeleteConns`
(:1370-1374) sends `XDirectDel` when the contact is ready and active:

```
notify  notice  sent
on      absent  XDirectDel {}
on      set     XDirectDel {notice}
off     set     XDirectDel {silent, notice}   contact chat version >= 21
off     absent  nothing
```

- `deleteAgentConnectionsAsync'` waits for delivery whenever `XDirectDel` is
  sent.
- `xDirectDel` (`Subscriber.hs:2734`): new `Bool` and `Maybe LinkNotice`
  parameters. The notice: `setContactLinkNotice db ct brokerTs notice`.
- `silent`: only the notice is stored. The contact status and connections are
  unchanged; no chat item is created and no event is sent. As with deletion
  without notification, the contact learns of the deletion when sending fails.

## DB

Migration `M20260926_link_notices`, SQLite and Postgres.

```
link_notices
  link_notice_id  INTEGER PRIMARY KEY AUTOINCREMENT | BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  link_hash       BLOB NOT NULL | BYTEA NOT NULL
  expires_at      TEXT | TIMESTAMPTZ         -- NULL: indefinite
  reason          TEXT
  created_at      TEXT NOT NULL | TIMESTAMPTZ NOT NULL
  updated_at      TEXT NOT NULL | TIMESTAMPTZ NOT NULL

UNIQUE INDEX idx_link_notices_link_hash ON link_notices(link_hash)
```

The SQLite table is `STRICT`. Both `chat_schema.sql` files and
`chat_query_plans.txt` are regenerated.

`Store/Groups.hs` and `Store/Direct.hs`:

- `setGroupLinkNotice :: DB.Connection -> GroupInfo -> UTCTime -> LinkNotice -> IO ()`
  reads `groups.via_group_link_uri_hash`.
- `setContactLinkNotice :: DB.Connection -> Contact -> UTCTime -> LinkNotice -> IO ()`
  reads `connections.via_contact_uri_hash` of the contact connection.
- Both call `upsertLinkNotice` when the hash is present:
  `INSERT ... ON CONFLICT (link_hash) DO UPDATE SET expires_at, reason, updated_at`,
  with `expires_at = addUTCTime (fromIntegral ttl) brokerTs`, NULL after
  `9999-12-31T23:59:59Z`.
- `getLinkNotice :: DB.Connection -> UTCTime -> (ConnReqUriHash, ConnReqUriHash) -> IO (Maybe (Maybe UTCTime, Maybe ReportReason))`
  returns `(expires_at, reason)`:
  `link_hash IN (?,?) AND (expires_at IS NULL OR expires_at > ?)`, indefinite
  first, then the latest `expires_at`, `LIMIT 1`.
- `deleteExpiredLinkNotices :: DB.Connection -> UTCTime -> IO ()`.

`cleanupManager` (`Commands.hs:5816-5820`): new step `cleanupLinkNotices`
calls `deleteExpiredLinkNotices`.

## Connection plan

`Controller.hs`:

- `GroupLinkPlan` (:1175):
  `GLPLinkNotice {expiresAt :: Maybe UTCTime, reason :: Maybe ReportReason}`.
- `ContactAddressPlan` (:1166):
  `CAPLinkNotice {expiresAt :: Maybe UTCTime, reason :: Maybe ReportReason}`.
- `connectionPlanProceed` (:1211) returns `False` for both via the existing
  `_ -> False` branches.

`Commands.hs`, after the own-link lookup, `getLinkNotice` with `cReqHashes`:

- `groupJoinRequestPlan` (:4661): a notice -> `GLPLinkNotice`. Group links and
  channel links.
- `contactRequestPlan` (:4638): a notice -> `CAPLinkNotice`. Personal and
  business addresses.
- Otherwise the existing lookups.
- `View.hs` (:2268, :2301 area): render both constructors.

## Connect commands

`checkLinkNotice :: ConnReqContact -> CM ()` in the `processChatCommand`
`where` block, next to `contactCReqSchemas` (`Commands.hs:4689`). It computes
both hashes and throws `CELinkNotice` for a notice in effect.

Call sites (`Commands.hs`):

- `APIConnect`, `SCMContact` branch (:2391), before `connectViaContact`.
- `APIConnectPreparedGroup`, relay branch, after `getShortLinkConnReq` (:2303),
  with `mainCReq`.
- `APIConnectPreparedGroup`, non-relay branch (:2361), before
  `connectViaContact`, with the link from `connLinkToConnect`.
- `APIConnectPreparedContact`, `SCMContact` branch (:2271), before
  `connectViaContact`.
- `connectContactViaAddress` (:3900), before connecting.

`ChatErrorType` (`Controller.hs:1492`):
`CELinkNotice {expiresAt :: Maybe UTCTime, reason :: Maybe ReportReason}`.
`View.hs` (:2822 area): render it.

## API documentation and clients

- `bots/src/API/Docs/Commands.hs`: `APIRemoveMembers` (:118) and
  `APIDeleteChat` (:154) syntax adds `Optional "" (" notice=" <> Json "$0") "notice"`.
- `bots/src/API/Docs/Types.hs` registers `LinkNotice` as `STRecord`.
- Markdown, TypeScript and Python bindings are regenerated.
- New optional `notice` parameter: `apiRemoveMembers` and `apiDeleteChat` in
  `packages/simplex-chat-client/typescript/src/client.ts` (:270, :179) and
  `packages/simplex-chat-nodejs/src/api.ts` (:600, :809).

## Apps

iOS and Kotlin:

- `LinkNotice` type; `ReportReason` reused.
- `Connection.viaUserContactLink` (iOS `ChatTypes.swift:2604`, Kotlin
  `ChatModel.kt:2035`); the core JSON includes it (`Types.hs:1933`).
- Notice parameter: `apiRemoveMembers` (`AppAPITypes.swift:85`, `:302`;
  `SimpleXAPI.swift:2025`; `SimpleXAPI.kt:2452`, `:3952`, `:4168`) and
  `apiDeleteChat` (`AppAPITypes.swift:145`, `:374`; `SimpleXAPI.swift:1259`;
  `SimpleXAPI.kt:1856`, `:4012`, `:4231`).
- `linkNotice` case in `GroupLinkPlan`, `ContactAddressPlan` and
  `ChatErrorType`, with `expiresAt` and `reason`.
- Plan handling shows an alert: `NewChatView.swift` `planAndConnect`,
  `ConnectPlan.kt`. The same alert for `CELinkNotice`, by the link type. The
  alert omits the profile.

```
GLPLinkNotice
Banned
You were banned from this group.            channel: this channel
You can join again after <date, time>.      indefinite: You can't join it again.
Reason: <reason label>.                     absent or unknown reason: line omitted

CAPLinkNotice
Banned
You were banned from connecting via this address.
You can connect again after <date, time>.   indefinite: You can't connect again.
Reason: <reason label>.                     absent or unknown reason: line omitted
```

Dialogs. iOS: a compact sheet in the `DeleteActiveContactDialog` style
(`ChatInfoView.swift:1262-1302`, `.presentationDetents([.fraction])` at
:349-352), replacing the removal and business chat deletion alerts. Kotlin:
`showAlertDialogButtonsColumn` with rows, as `deleteActiveContactDialog`
(`ChatInfoView.kt:306-362`).

- Ban label: "Ban from joining" for groups and channels, "Ban from connecting"
  for business chats and contacts.
- Removal dialog, single and bulk:

```
Remove member?                           bulk: Remove <n> members?
Member will be removed from group - this cannot be undone!

Ban from joining     1 day >             business chat: Ban from connecting
Reason                Spam >             shown when a ban is chosen

[ Remove ]
[ Remove and delete messages ]
[ Cancel ]
```

- Business chat deletion dialog, when the chat is deleted for all members
  (`GroupChatInfoView.swift:904`, `ChatListNavLink.swift:565`;
  `GroupChatInfoView.kt:208`):

```
Delete chat?
<name>

Chat will be deleted for all members - this cannot be undone!

Ban from connecting     No >
Reason                Spam >             shown when a ban is chosen

[ Delete ]
[ Cancel ]
```

- Contact deletion dialog (`DeleteActiveContactDialog`,
  `deleteActiveContactDialog`), rows shown when the contact connection has
  `viaUserContactLink`:

```
Keep conversation           [ ]
Ban from connecting        No >
Reason                   Spam >          shown when a ban is chosen
Delete without notification
Delete and notify contact
Contact will be deleted - this cannot be undone!
```

- p2p group and channel deletion dialogs are unchanged.
- Ban values and `ttl`:

```
No           notice absent   business chat and contact deletion default
1 hour       3600
1 day        86400           removal default
1 week       604800
1 month      2592000
Permanently  ttl absent
```

- Reason values: Spam (default), Inappropriate content, Community guidelines
  violation, Inappropriate profile, Another reason. Existing report reason
  labels. The reason is only chosen from this list.
- The ban rows are hidden for relays and invited members.
- Removal call sites.
  iOS: `GroupChatInfoView.swift:995`, `:1046`; `GroupMemberInfoView.swift:697-724`;
  `ContextPendingMemberActionsView.swift:51`.
  Kotlin: `GroupChatInfoView.kt:274`, `:358`, `:1400`; `GroupMemberInfoView.kt:247`,
  `:342`; `ComposeContextPendingMemberActionsView.kt:78`.

## Tests

`ProtocolTests.hs` (:377, :383, new `x.direct.del`): each message with a
notice, without a notice, and with a malformed notice; `x.direct.del` with
`silent`.

`ChatTests/Groups.hs`:

1. Removal with a notice: the removed member's plan for the group link returns
   `linkNotice` with the expiry; `/_connect` fails with `linkNotice`.
2. The removed member deletes the group: the plan returns `linkNotice`.
3. Another profile of the removed member: the plan returns `linkNotice`.
4. Removal without a notice: the plan returns `ok`.
5. Notice with `ttl` 0: the plan returns `ok`.
6. Repeat removal: the second notice replaces the first.
7. Channel: a subscriber removed with a notice; the plan returns `linkNotice`;
   `APIConnectPreparedGroup` fails.
8. A member pending review rejected with a notice: the plan returns
   `linkNotice`.
9. A member added by invitation removed with a notice: the notices table is
   unchanged.
10. A notice for group A leaves the plan for group B unchanged.
11. Group deleted without a notice: the notices table is unchanged.

`ChatTests/Profiles.hs`, "business address" (:79) and "contact address
connection plan" (:82):

12. Business address: a customer removed with a notice; the plan for the
    address returns the contact address `linkNotice`.
13. Business chat deleted with a notice: the customer's plan for the business
    address returns `linkNotice`; `/_connect` fails.
14. A contact connected via the address, deleted with a notice and notify
    off: the contact's chat is unchanged; its plan for the address returns
    `linkNotice`; `/_connect` fails.
15. The same with notify on: the contact is shown as deleted; the plan returns
    `linkNotice`.
16. A contact with chat version below 21, notify off, with a notice: the
    contact receives no message.

`ChatTests/Direct.hs`:

17. A contact connected via an invitation link, deleted with a notice: the
    notices table is unchanged.