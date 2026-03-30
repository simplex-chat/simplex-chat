# Group identity and signature binding

## Problem

Group message signatures bind to a group identity via a prefix:

```
signedBytes = smpEncode (CBGroup, groupIdentity, memberId) <> messageBody
```

Using `groupRootKey` as identity is unstable: the root key is derived from the link's key pair, so link rotation (relay replacement, key compromise recovery) changes it, breaking existing bindings.

Using an arbitrary entity ID is stable but not self-authenticating: any owner could copy another group's ID.

## Design

Use the **hash of the genesis root key** as group identity:

```
groupEntityId = sha256(genesisRootPubKey)
```

- Set at creation, never changes.
- Self-authenticating: derived from a key pair only the creator held.
- Stored as `linkEntityId` in the short link, and in the group profile distributed to all members.
- Used in the signature binding prefix instead of root key.

### Why no validation now

Current clients do not validate that `linkEntityId == sha256(rootKey)` on join. This is unconventional — normally, an unvalidated binding is pointless. Here it is deliberate forward-compatible design, not deferred work:

- **Forward compatibility for joiners**: future link rotation will cause `rootKey` and `linkEntityId` to diverge. Current clients don't know how to verify a rotation chain, so they must accept diverged values. If we validated now, current clients could not join future rotated groups. Mobile clients have slow upgrade cycles and we have no mechanism to force upgrades, so we aim for at least 2-3 months backward compatibility for new features (1 year for existing). Validating now would force a breaking change on rotation.

- **Forward compatibility for groups**: all groups created now have the correct binding (`entityId = sha256(rootKey)`). When a future protocol version introduces rotation and enforces validation, these groups are already compliant. Deferring the entity ID until then would mean some groups have IDs and some don't — a backward-compatibility problem.

The cloning risk (copied entity ID in a malicious group) is acceptable now: groups are small, invite links come from trusted sources, and history merging on re-join is itself a future feature. By the time channels are large enough for cloning to matter, validation will be enforced.

### Key hierarchy context

The root key is a **bootstrap key**: it signs `OwnerAuth` entries to certify owners (see [simplexmq owner chain](https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2025-04-04-short-links-for-groups.md#multiple-owners-managing-queue-data)), then need not be used again. Owner keys sign admin messages, group updates, and future rotation statements. This conceals the creator's identity — all owners are indistinguishable.

Using the genesis root key *hash* as identity aligns with this: after rotation, the root key changes but the identity persists, bridged by owner-signed rotation statements.

### What IS validated now

- **Link vs profile consistency**: joiners validate that `linkEntityId` from the link matches `sharedGroupId` in the group profile. This prevents a directory or listing from substituting a different link for a group — the link is bound to the profile. This check remains valid after rotation (both preserve the original entity ID).

- **Profile update immutability**: `sharedGroupId` in the group profile must not change. Clients reject `XGrpInfo` updates that modify it.

### What is NOT validated now

- **`linkEntityId == sha256(rootKey)`**: not checked on join. See "Why no validation now" above.

## Changes

### Done

1. **Agent API** (`simplexmq`): `prepareConnectionLink` takes caller-provided root key pair and entity ID instead of generating the key internally. Caller controls both.

2. **Link creation** (`Commands.hs`): owner generates root key pair, computes `sharedGroupId = sha256(rootPubKey)`, passes both to `prepareConnectionLink`. The entity ID is baked into signed `FixedLinkData`.

### Remaining

3. **Group profile**: add `sharedGroupId` field to `GroupProfile`, set from `linkEntityId` at genesis, immutable. Reject `XGrpInfo` updates that change it.

4. **Joiner validation**: confirm `linkEntityId` from link matches `sharedGroupId` from group profile.

5. **Signature binding**: change prefix from `smpEncode (CBGroup, groupRootPubKey, memberId)` to `smpEncode (CBGroup, sharedGroupId, memberId)` in both `groupMsgSigning` (signing) and `withVerifiedMsg` (verification).
