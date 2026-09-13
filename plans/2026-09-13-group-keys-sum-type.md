# Group keys as a sum type

Branch: to be created from `master` after `ep/file-badge-proofs` is merged.

## Summary

`GroupInfo.groupKeys` is removed, and the user's private keys leave every API response and event.

Group keys become a sum type with one constructor per kind of group, each holding the user's member key. The kind is read from `groups.use_relays`. A function that consumes keys takes `GroupKeys` and matches on it; `useRelays'` stays for the sites that branch on the kind alone.

The member key is written at every group insert and generated at the read for rows created before this change. `createUserMemberKey` is removed.

## Terms

- **p2p group** — `use_relays = 0`. The user's member key signs messages.
- **public group** — `use_relays = 1`. Identified by `public_group_id` and the group root key.
- **member key** — `groups.member_priv_key`, the user's own key in the group.
- **root key** — the group's identity key. The owner holds it as `GRKPrivate`; everyone else holds `GRKPublic`.
- **relay request** — a group row a relay creates on `XGrpRelayInv`, before it fetches the group link.

## 1. The type

`Simplex/Chat/Types.hs`.

```haskell
data GroupKeys
  = GKGroup
      { memberPrivKey :: C.PrivateKeyEd25519
      }
  | GKPublicGroup
      { publicGroupId :: B64UrlByteString,
        groupRootKey :: GroupRootKey,
        memberPrivKey :: C.PrivateKeyEd25519,
        rosterVersion :: VersionRoster
      }
```

`PublicGroupKeys` is removed. `GroupRootKey` is unchanged.

`GroupInfo` loses `groupKeys` and `rosterVersion`, and keeps `useRelays`, `relayOwnStatus` and `groupSummary`. Its `deriveJSON` then emits no key material.

`relayOwnStatus` stays on `GroupInfo`: the view reads it (`View.hs:237`, `:1503-1508`), both apps decode it, and it is set on the rows that have no identity, where `updateRelayOwnStatusFromTo` reads it (`Groups.hs:1975-1978`).

`groupMemberKey`, `groupBindingData`, `groupMsgSigning`, `sndGroupChatBinding`, `rcvGroupChatBinding` and `encodeXMemberConnInfo` take `GroupKeys` instead of reading it from `GroupInfo`.

## 2. Reading

`Simplex/Chat/Store/Shared.hs`.

`StoreCxt` gains the generator, named `drg` because `random` collides with `ChatController.random` wherever both records are in scope.

```haskell
data StoreCxt = StoreCxt {vr :: VersionRangeChat, badgeKeys :: Map Int BBSPublicKey, drg :: TVar ChaChaDRG}
```

`mkStoreCxt` takes the generator alongside the config, and its callers pass it: `chatStoreCxt'`, `Web.hs` ×2, `Directory/Util.hs`, and three sites in `tests/ChatTests/Profiles.hs`.

`toGroupInfo` returns `(GroupInfo, GroupKeysData)`, where `GroupKeysData` is the roster version and the three key columns; `toGroupInfo_` returns the group alone. `GroupInfoRow` keeps the columns it already selects, so keys come from the same query as the group.

```haskell
mkGroupKeys :: DB.Connection -> StoreCxt -> GroupInfo -> GroupKeysRow -> ExceptT StoreError IO GroupKeys
```

Rules, in order:

| `use_relays` | `public_group_id` | root key | member key | result |
| --- | --- | --- | --- | --- |
| 0 | — | — | present | `GKGroup` |
| 0 | — | — | absent | generated, stored, `GKGroup` |
| 1 | present | present | present | `GKPublicGroup` |
| 1 | present | present | absent | generated, stored, `GKPublicGroup` |
| 1 | absent or root key absent | | | `SEGroupNotFound {groupId, notReady = True}` |

Generation uses the store's `random` and writes with the statement in section 3.

`GKPublicGroup` takes `roster_version`, and a NULL column is written as 0 and returned as 0. `broadcastRoster` requires the version to be recorded before the events that carry it (`Internal.hs:2569-2572`); this write is the one `Subscriber.hs:908-912` performs by hand today, so that branch collapses to `sendGroupRosterToRelay`. The first version `broadcastRoster` reserves for a channel that held NULL becomes 1.

`applyAtRosterVersion` keeps reading the version from the store (`Subscriber.hs:3345`), deliberately rather than from a batch-constant value.

### Reads

| function | returns |
| --- | --- |
| `getGroupInfoKeys` | `(GroupInfo, GroupKeys)` |
| `getGroupInfo` | `GroupInfo`, as `fst <$> getGroupInfoKeys`, so it rejects a group with no identity |
| `getGroupKeys` | `GroupKeys` |
| `getUserMemberKey` | the member key of a group that may have no identity yet |

Keeping `getGroupInfo` at `GroupInfo` leaves its call sites unchanged while preserving the readiness check; a site that needs keys switches to `getGroupInfoKeys` or reads `getGroupKeys` in a transaction it already opens.

`getConnectionEntity` is unchanged: the receive path needs no keys, per section 7.

### Reads that build the group with `toGroupInfo_`

| function | consumer |
| --- | --- |
| `getBaseGroupDetails` | chat list |
| `getRelayServedGroups` | `checkRelayServedGroups` — profile, group link, relay status |
| `getRelayInactiveGroups` | `checkRelayInactiveGroups` — connection deletion |
| `getAcceptedBusinessChat` | the request entity `REBusinessChat` |
| `toGroupAndMember` (`Connections.hs`) | the connection entity |
| `toGroupInfoRegLink` (directory service) | registration records |

### Keys alone

```haskell
getGroupKeys :: DB.Connection -> StoreCxt -> User -> GroupId -> ExceptT StoreError IO GroupKeys
```

For the sites that hold a group taken from an API type. Selects the same columns and applies the same rules.

### The tolerant read

```haskell
getGroupInfoNotReady :: DB.Connection -> StoreCxt -> User -> GroupId -> ExceptT StoreError IO GroupInfo
```

Returns the group with the identity absent. Used where the identity is yet to arrive:

| site | operation |
| --- | --- |
| `createRelayRequestGroup` (`Groups.hs:1937`) | reads back the row it has just created |
| `allowRelayGroup` (`Groups.hs:2011`) | moves a rejected relay request to `RSInactive` |
| `APIConnectPreparedGroup` (`Commands.hs:2286`) | fetches the link and writes the root key |
| `processRelayRequest` (`Subscriber.hs:4474`) | fetches the link and writes the identity |

`rejectRelayInvitationAsync` (`Internal.hs:1182`) reaches the row through `createRelayRequestGroup`.

## 3. Writing the member key

One statement writes `groups.member_priv_key` after this change, in `setUserMemberKey`:

```sql
UPDATE groups
SET member_priv_key = COALESCE(member_priv_key, ?), updated_at = ?
WHERE group_id = ?
RETURNING member_priv_key
```

`group_members.member_pub_key` for the membership row is set from the returned key.

```haskell
setUserMemberKey :: DB.Connection -> GroupId -> GroupMemberId -> C.PrivateKeyEd25519 -> ExceptT StoreError IO C.PrivateKeyEd25519
```

### Inserts

| insert | change |
| --- | --- |
| `createNewGroup` (`Groups.hs:419`) | unchanged, key passed by `APINewGroup` and `APINewPublicGroup` |
| `createGroupInvitation` (`Groups.hs:497`) | unchanged |
| `createGroup_` from `createPreparedGroup` (`Groups.hs:654`) | unchanged |
| `createGroup_` from `createGroupInvitedViaLink` (`Groups.hs:881`) | unchanged |
| `createBusinessRequestGroup` (`Groups.hs:2195`) | unchanged |
| `createGroup_` from `createRelayRequestGroup` | generates and passes a member key |

`createRelayRequestGroup` also passes that key's public half to `createContactMemberInv_`, closing the TODO it held.

### Updates that stop writing the member key

`updateGroupMemberKeys` is replaced by `setGroupRootKey`, which writes `root_pub_key` alone. `updateRelayGroupKeys` keeps `group_type`, `group_link`, `public_group_id` and calls it.

### Callers that stop generating keys

| caller | change |
| --- | --- |
| `connectPreparedGroup` | the root key is written at prepare, so it generates nothing |
| `createRelayLink` | signs the relay link with the stored member key, read by `getUserMemberKey`, instead of a fresh `sigKeys` |

A relay's member key and its relay-link root key stay the same key. Members read it from the link as `FixedLinkData.rootKey` — `updateRelayMemberData` (`Subscriber.hs:1219`) on the joining side and `setRelayLinkAccepted` (`Subscriber.hs:1250`) on the owner side.

The owner keeps the split: the root key authorises owner keys through `OwnerAuth` (`Internal.hs:1657-1661`), and the member key signs messages and shares.

### `createUserMemberKey`

Removed, with its six calls: `Internal.hs:2633` (`sendGroupMessages`), `Internal.hs:2640` (`sendGroupSignedMessages`), `Commands.hs:3962` (`joinContact`), `Commands.hs:4809` (`sendGroupContentMessages`), `Subscriber.hs:812` (`XGrpLinkInv` from the host), `Subscriber.hs:955` (`sendXGrpLinkMem`).

Each of those callers takes `GroupKeys` from its own read instead.

## 4. The root key at prepare

`createPreparedGroup` writes `root_pub_key` for a public group.

- `GroupShortLinkInfo` gains `rootKey :: Maybe C.PublicKeyEd25519`, set from `FixedLinkData` where the connection plan is built.
- `APIPrepareGroup` gains an optional ` key=` parameter before the link data.
- Both apps pass `groupShortLinkInfo.rootKey` through `apiPrepareGroup`; the directory service passes it from the plan.
- `createPreparedGroup` takes it and calls `setGroupRootKey`.

Channels prepared before this change read through `getGroupInfoNotReady` in `APIConnectPreparedGroup` and gain the root key at `updatePreparedRelayedGroup`, as they do today.

## 5. The error

`Simplex/Chat/Store/Shared.hs:98`.

```haskell
| SEGroupNotFound {groupId :: GroupId, notReady :: Bool}
```

`notReady = True` marks a public group whose identity has yet to arrive. The ten existing construction and match sites are updated.

## 6. Call sites of `useRelays'`

87 sites read `useRelays'`, and none of them reads a key. `useRelays'` (`Types.hs:523`) and the `useRelays` field remain, so 81 of those sites are unchanged — recipients, roles, roster rules, batching, forwarding, admission, rendering.

Six sites take `GroupKeys` and match on it, because they or the functions they call consume keys:

| site | key consumer reached |
| --- | --- |
| `setGroupLinkData` (`Internal.hs:1574`) | `groupLinkData` |
| `updatePublicGroupData` (`Internal.hs:1600`, `:1608`) | `setGroupLinkDataAsync` then `groupLinkData` |
| `groupLinkData` (`Internal.hs:1652`) | root key, member key |
| `allowAgentConnectionAsync` (`Internal.hs:3052`) | `groupMsgSigning` |
| `joinContact` (`Commands.hs:3958`) | `encodeXMemberConnInfo` |

## 7. Consumers of the keys

`groupBindingData` reads `publicGroupId` from `groupProfile.publicGroup`, which holds the same column, so it takes `GroupInfo` and no keys. Everything that only verifies — `verifyGroupSig`, `withVerifiedMsg`, `xInfoMember`, `storeMemberKey`, `verifyKey`, `rcvGroupChatBinding` — therefore needs no keys, and the receive path is unchanged.

| site | uses |
| --- | --- |
| `sndGroupChatBinding` | member key, `publicGroupId` |
| `groupLinkData` | root key as `GRKPrivate`, member key |
| `groupMsgSigning` | member key |
| `groupMemberKey` | member key |
| `encodeXMemberConnInfo` | member key |
| `APIShareChatMsgContent` | root key as the owner test, member key to sign |
| `broadcastRoster` | `rosterVersion` |
| `sendGroupRosterToRelay` | `rosterVersion` |

`xGrpRosterAck` and the roster blob gate read the version from the store rather than from a batch-constant value, as `applyAtRosterVersion` already did.

`groupBindingData` moves to `Protocol.hs` beside `encodeChatBinding`, as the file-badge plan records.

### Functions that gain a `GroupKeys` parameter

`Internal.hs`: `acceptGroupJoinRequestAsync`, `acceptBusinessJoinRequestAsync`, `groupLinkData`, `introduceToModerators`, `introduceToAll`, `introduceToRemaining`, `introduceMember`, `introduceInChannel`, `serveRoster`, `sendInlineBlobChunks`, `sendRelayCapIfNeeded`, `sendGroupMemberMessages`, `sendGroupMessage`, `sendGroupMessage'`, `sendRoster`, `broadcastRoster`, `sendGroupRosterToRelay`, `sendGroupMessages`, `sendGroupSignedMessages`, `sendGroupProfileUpdate`, `sendGroupMessages_`, `groupMsgSigning`, `sndGroupChatBinding`, `encodeXMemberConnInfo`, `allowAgentConnectionAsync` (as `Maybe (GroupInfo, GroupKeys)`).

`Commands.hs`: `delEventSigned`, `changeRoleCurrentMems`, `deleteMemsSend`, `deletePendingMember`, `sendGroupContentMessages_`, `newGroup` (which also loses its `Bool`, deriving the kind from the keys).

`Subscriber.hs`: `sendXGrpLinkMem`, `acceptJoin`, `sendGroupAutoReply`, `leaveChannelRelay`, `leaveGroupSendMsg`.

`saveConnInfo` returns `Maybe (GroupInfo, GroupKeys)`, and `createGroupInvitation` returns the keys it creates, so those handlers use them rather than re-reading.

Sites that hold a group from an API type — the `SFDONE` handler destructuring `GroupChat g` out of an `AChatItem`, and the CONF/INFO handlers holding the connection entity — read keys by group id in a transaction they already open.

## 8. Schema

No migration. The columns keep their meaning:

- `groups.member_priv_key` — written at insert, or at the first read of a row created before this change.
- `groups.root_priv_key` — the owner's root key.
- `groups.root_pub_key` — every other member's copy of the root key.
- `groups.roster_version` — written as 0 at the first read of a public group that held NULL.
- `group_profiles.public_group_id` — the public group identity.

## 9. Tests

- A p2p group created before the change: the first read stores a member key, and a second read returns the same key.
- Concurrent reads of such a row return one key.
- A relay request reads through `getGroupInfoNotReady`, and through `getGroupInfo` fails with `notReady = True`.
- A channel prepared before the change connects and then reads as `GKPublicGroup`.
- A relay's member key equals the key its relay link is signed with, and a member that fetched the link verifies the relay's signed message.
- A channel with NULL `roster_version` reads as version 0, the row holds 0 afterwards, and the next `broadcastRoster` reserves 1.
- `GroupInfo` JSON holds no key material.

## Simplifications the implementation exposed

Recorded after the change was built, for a decision before it is merged.

**1. `publicGroupId` in `GKPublicGroup` restates `GroupInfo`.** `groupProfile.publicGroup` holds the same column, which is why `groupBindingData` needed no keys. Its only reader in the keys is `sndGroupChatBinding`'s channel branch, which can read the profile as `rcvGroupChatBinding` does. Dropping it leaves the constructor holding what is secret or absent from `GroupInfo`.

**2. `rosterVersion` is not a key.** Three of its five readers deliberately read it from the store, because a batch-constant copy is stale on reorder; only `broadcastRoster` and `sendGroupRosterToRelay` use the value in hand, and `broadcastRoster` writes the next version immediately. Moving it out leaves `GroupKeys` holding only keys.

With 1 and 2:

```haskell
data GroupKeys
  = GKGroup {memberPrivKey :: C.PrivateKeyEd25519}
  | GKPublicGroup {groupRootKey :: GroupRootKey, memberPrivKey :: C.PrivateKeyEd25519}
```

`groupPublicId` and `keysRosterVersion` then disappear, and `createNewGroup` takes the kind from `groupProfile.publicGroup` rather than from the keys.

**3. Threading versus reading.** 38 sites read the keys in a transaction they already open, and about 25 functions gained a `GroupKeys` parameter to carry them further. Since nearly every group send already opens a store transaction, the send helpers could read the keys themselves and the parameter could be dropped from their signatures, leaving `GroupKeys` in the six consumers that use it.

**4. `notReady` has no reader.** It is set in one place and inspected nowhere. Either the apps distinguish it from "not found", or the field waits until something reads it.

**5. `toGroupInfo` returning a pair** forced `toGroupInfo_` at six call sites. A separate projection from `GroupInfoRow` to `GroupKeysData` would leave `toGroupInfo` alone.

## Out of scope

- The sum type in API responses and events.
- Moving relay request data out of the `groups` row.
- `groupSummary.publicMemberCount` moving into `GKPublicGroup`, since both apps decode `GroupSummary`.
