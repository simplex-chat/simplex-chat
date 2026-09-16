# Group keys as a sum type

Branch: `master`, on top of `core: refactor groups`.

## Summary

`GroupInfo.groupKeys` is removed, and the user's private keys leave every API response and event.

Group keys become a sum type with one constructor per kind of group, each holding the user's member key.

A group and its keys come from one query. Every read of keys is a read of the group.

The member key is written at every group insert, and generated at the first read of a row created before this change. `createUserMemberKey` is removed.

## Terms

- **p2p group** — `use_relays = 0`. The user's member key signs messages.
- **public group** — `use_relays = 1`. Identified by `public_group_id` and the group root key.
- **member key** — `groups.member_priv_key`, the user's own key in the group.
- **root key** — the group's identity key. The owner holds it as `GRKPrivate`; everyone else holds `GRKPublic`.
- **relay request** — a group row a relay creates on `XGrpRelayInv`, before it fetches the group link.
- **prepared channel** — a public group prepared from a link, before `APIConnectPreparedGroup` stores the root key.

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
        memberPrivKey :: C.PrivateKeyEd25519
      }
  | GKRelayRequest
      { memberPrivKey :: C.PrivateKeyEd25519
      }
  | GKPreparedPublicGroup
      { publicGroupId :: B64UrlByteString,
        memberPrivKey :: C.PrivateKeyEd25519
      }
  deriving (Eq, Show)

isPublicGroup :: GroupKeys -> Bool
```

`PublicGroupKeys` is removed. `GroupRootKey` is unchanged, and its JSON instance is removed with those of `GroupKeys` and `PublicGroupKeys`.

`GroupInfo` loses `groupKeys` and keeps every other field, `rosterVersion` included. Its `deriveJSON` then emits public fields only.

`RequestEntity` becomes `REBusinessChat GroupInfo GroupKeys GroupMember`.

`PreparedChatEntity` becomes `PCEGroup {groupInfo, groupKeys, hostMember}`.

`ReceivedGroupInvitation` gains `groupKeys :: GroupKeys`.

## 2. Reading

`Simplex/Chat/Store/Shared.hs`.

`StoreCxt` gains the generator, named `drg` because `random` collides with `ChatController.random` wherever both records are in scope.

```haskell
data StoreCxt = StoreCxt {vr :: VersionRangeChat, badgeKeys :: Map Int BBSPublicKey, drg :: TVar ChaChaDRG}
```

```haskell
storeCxt :: ChatController -> StoreCxt
```

`toGroupInfo` returns `(GroupInfo, GroupKeysRow)`; `toGroupInfo_` returns the group alone.

```haskell
mkGroupKeys :: DB.Connection -> StoreCxt -> GroupInfo -> GroupKeysRow -> ExceptT StoreError IO GroupKeys
```

The member key is taken from the row, or generated and stored. The constructor follows:

| `use_relays` | `public_group_id` | root key | result |
| --- | --- | --- | --- |
| 0 | — | — | `GKGroup` |
| 1 | present | present | `GKPublicGroup` |
| 1 | present | absent | `GKPreparedPublicGroup` |
| 1 | absent | — | `GKRelayRequest` |

### Reads

| function | returns |
| --- | --- |
| `getGroupInfoRow` | `(GroupInfo, GroupKeysRow)` |
| `getGroupInfoKeys` | `(GroupInfo, GroupKeys)` |
| `getGroupInfo` | `GroupInfo`, as `fst <$> getGroupInfoRow` |
| `getGroupKeys_` | `(Group, GroupKeys)` |
| `getGroup` | `Group`, as `fst <$> getGroupKeys_` |

All five issue one `groupInfoQuery`. `getGroupKeys_` and `getGroup` add the member query.

`getGroupInfoKeys` returns the group with `membership.memberPubKey` set from the member key it materialized, so the pair agrees on a row created before this change.

A site that needs keys switches its existing read to `getGroupInfoKeys` or `getGroupKeys_`.

### Reads that return keys with their entity

| function | returns |
| --- | --- |
| `getConnectionEntityKeys` | `(ConnectionEntity, Maybe GroupKeysRow)` |
| `getConnectionEntity` | `ConnectionEntity`, as `fst <$> getConnectionEntityKeys` |
| `getGroupInvitation` | `ReceivedGroupInvitation`, with `groupKeys` |
| `createGroupInvitation` | `(GroupInfo, GroupKeys, GroupMemberId)` |
| `createBusinessRequestGroup` | `(GroupInfo, GroupKeys, GroupMember)` |
| `updatePreparedRelayedGroup` | `(GroupInfo, GroupKeys)` |
| `getRelayServedGroups` | `[(GroupInfo, GroupKeys)]` |
| `getAcceptedBusinessChat` | `Maybe (GroupInfo, GroupKeysRow)` |
| `getGroupAndRegLink` (directory service) | `(GroupInfo, GroupKeysRow, GroupReg, Maybe GroupLink)` |

### Reads that discard the keys

`toGroupInfo_` builds the group for `getBaseGroupDetails`, `getRelayInactiveGroups` and `toGroupInfoRegLink`.

## 3. Message handling

`processAgentMessageConn` reads the entity with `getConnectionEntityKeys` and passes `CM GroupKeys` to `processGroupMessage` as `getGks`. A handler that signs forces it; the rest pay nothing. `getGks` builds the keys from the row read with the entity. Forcing it opens one store transaction, which writes on the first read of a row created before this change.

A handler with an unsigned path takes `CM GroupKeys` and forces it on the signing path: `xGrpInfo`, `xGrpRosterAck`, `xGrpRosterRequest`, `xGrpLinkAcpt`, `xGrpMemNew`, `xGrpMemRole`, `xGrpMemDel`, `xGrpLeave`, `xGrpMsgForward`, `applyAtRosterVersion`, `bFileChunkGroup`, `receiveRosterChunk`, `rosterCompletion`, and `updatePublicGroupData` in `Internal.hs`. A handler that always signs takes `GroupKeys`.

An entity of a group connection without keys raises `CEInternalError`.

## 4. Writing the member key

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

Every insert writes a member key. `createRelayRequestGroup` generates one and passes its public half to `createContactMemberInv_`, closing the TODO it held.

`createNewGroup` takes a non-optional `GroupKeys` and derives `use_relays` from `isPublicGroup`.

### Updates that stop writing the member key

`updateGroupMemberKeys` is replaced by `setGroupRootKey`, which writes `root_pub_key` alone. `updateRelayGroupKeys` keeps `group_type`, `group_link` and `public_group_id`, and calls it.

### Callers that stop generating keys

| caller | change |
| --- | --- |
| `APIConnectPreparedGroup` | writes the root key alone |
| `createRelayLink` | signs the relay link with the stored member key |

A relay's member key and its relay-link root key are the same key. Members read it from the link as `FixedLinkData.rootKey`.

The owner keeps the split: the root key authorises owner keys through `OwnerAuth`, and the member key signs messages and shares.

### `createUserMemberKey`

Removed, with its six calls. Each caller takes `GroupKeys` from its own read.

## 5. The error

```haskell
| SEGroupNotFound {groupId :: GroupId}
```

A relay request and a prepared channel read as their own constructors, so every read returns them.

## 6. Consumers of the keys

`groupBindingData` reads `publicGroupId` from `groupProfile.publicGroup`, so it takes `GroupInfo`. Everything that verifies — `verifyGroupSig`, `withVerifiedMsg`, `xInfoMember`, `storeMemberKey`, `verifyKey`, `rcvGroupChatBinding` — takes `GroupInfo` alone. The receive path is unchanged.

`sndGroupChatBinding` asserts the user's own member key, which `membership.memberPubKey` holds as the public half.

```haskell
groupMemberKey :: GroupKeys -> MemberKey
```

| site | uses |
| --- | --- |
| `groupLinkData` | root key as `GRKPrivate`, member key |
| `groupMsgSigning` | member key |
| `groupMemberKey` | member key |
| `encodeXMemberConnInfo` | member key |
| `APIShareChatMsgContent` | root key as the owner test, member key to sign |

### Functions that gain a `GroupKeys` parameter

`Internal.hs`: `acceptGroupJoinRequestAsync`, `acceptBusinessJoinRequestAsync`, `groupLinkData`, `setGroupLinkData`, `setGroupLinkData'`, `setGroupLinkDataAsync`, `introduceToModerators`, `introduceToAll`, `introduceToRemaining`, `introduceMember`, `introduceInChannel`, `serveRoster`, `sendInlineBlobChunks`, `sendRelayCapIfNeeded`, `sendGroupMemberMessages`, `sendGroupMessage`, `sendGroupMessage'`, `sendRoster`, `broadcastRoster`, `sendGroupRosterToRelay`, `sendGroupMessages`, `sendGroupSignedMessages`, `sendGroupProfileUpdate`, `sendGroupMessages_`, `groupMsgSigning`, `encodeXMemberConnInfo`, `allowAgentConnectionAsync` (as `Maybe (GroupInfo, GroupKeys)`).

`Commands.hs`: `delEventSigned`, `changeRoleInvitedMems`, `changeRoleCurrentMems`, `deleteMemsSend`, `deletePendingMember`, `blockMembers`, `sendGroupContentMessages`, `sendGroupContentMessages_`, `getCommandGroupChatItems`, `delGroupChatItemsForMembers`, `sendGrpInvitation`, `connectToRelay`, `leaveChannelRelay`, `leaveGroupSendMsg`, `runUpdateGroupProfile`, `newGroup` (which also loses its `Bool`).

`joinContact` takes `Maybe (Maybe (GroupInfo, GroupKeys))` and `Maybe MemberId`.

`Subscriber.hs`: every handler under `processGroupMessage` that sends, as `CM GroupKeys`; `sendXGrpLinkMem`, `acceptJoin`, `sendGroupAutoReply`.

`saveConnInfo` returns `Maybe (GroupInfo, GroupKeys)`.

## 7. Queries

The query count is unchanged. Every site that needs keys takes them from a read it already performs:

| site | before | after |
| --- | --- | --- |
| commands holding a group | `getGroupInfo` | `getGroupInfoKeys` |
| commands holding a group and members | `getGroup` | `getGroupKeys_` |
| `processAgentMessageConn` | `getConnectionEntity` | `getConnectionEntityKeys` |
| `APIJoinGroup` | `getGroupInvitation` | same, with `groupKeys` in the record |
| `APIConnectPreparedGroup` | `getGroupInfo` | `getGroupInfoRow` |
| business request | `getGroupInfo` | `getGroupInfoRow` |
| directory service link update | `getGroupLink` | `getGroupAndRegLink` |

## 8. Schema

The schema is unchanged. The columns keep their meaning:

- `groups.member_priv_key` — written at insert, or at the first read of a row created before this change.
- `groups.root_priv_key` — the owner's root key.
- `groups.root_pub_key` — every other member's copy of the root key.
- `group_profiles.public_group_id` — the public group identity.

## 9. Tests

`testGroupMemberKeyGenerated` (`tests/ChatTests/Groups.hs`): a p2p group whose member key columns are NULL on both sides. The first send stores a key, the profile update carries and is signed by that key, the peer stores it from the update and verifies the next signed event with it, `member_pub_key` of the membership is the public half of `member_priv_key`, and a second send leaves the key unchanged.

Covered by the existing suites: relay request and prepared channel flows (`chat relay tests`), relay link signing (`chat relay tests`), `GroupInfo` JSON (`Bot API docs`, once the generated files are writable).

## Open

`publicGroupId` in `GKPublicGroup` and `GKPreparedPublicGroup` restates `groupProfile.publicGroup` and is unread. Dropping it leaves each constructor holding what is secret or absent from `GroupInfo`.

`bots/api/TYPES.md`, `packages/simplex-chat-client/types/typescript/src/types.ts` and `packages/simplex-chat-python/src/simplex_chat/types/_types.py` still declare `GroupKeys`. The `Bot API docs` test regenerates them once they are writable; they are owned by root.

## Out of scope

- The sum type in API responses and events.
- Moving relay request data out of the `groups` row.
- `groupSummary.publicMemberCount` moving into `GKPublicGroup`, since both apps decode `GroupSummary`.
