# Badge proofs bound to the conversation, and file size limits decided in core

**Date:** 2026-09-04
**Branch:** ep/p2p-group-signing. The work depends on member keys and signed profile messages, which this branch adds.

## Summary

A badge is a credential issued to a user who supports SimpleX Chat. The user shows it to others by putting a proof in their profile. This plan binds every proof to the place where it is shown: a proof in a group profile to the sender's identity in that group, a proof attached to a file to the conversation and to that file. Verification checks the binding.

The same badge raises the size limit for files the user sends. Today each app decides whether a received file is within the limit, using the sender's profile as it is at the moment of display. After this plan, the core library decides once, when the file invitation arrives, from a proof in the invitation; stores the decision with the file; and the apps read it. A second proof arrives with the file description, the record that says where the file's chunks are stored, and is checked before the download starts.

The changes:

1. Three new presentation headers: one for a profile shown in a chat, one for a file invitation, one for a file description. The random header stays valid where there is no chat yet, and for the profile in direct chats until the direct binding reaches the handshake.
2. In p2p groups a badge is accepted only from a message signed by the member. The member connection handshake is signed in both directions, and the profile sent in it is stored, so the badge appears when two members connect.
3. In channels a badge is accepted from any profile message, because member keys there come from the roster, the member list signed by the channel owner.
4. Files above the default limit include a proof in the invitation and a proof in the description, in every chat type. The core library verifies both. The decision is stored on the file and shown by the apps from one field.
5. Forwarding a file above the forwarder's limit is refused with an alert before the forwarding sheet opens, and again, for the chosen destination, before anything is uploaded.
6. A received file keeps its two proofs, so a file re-sent to a new member as part of history keeps them; the sender's own files get fresh proofs from the credential.

Two new columns on `files`, and a new table `file_badge_proofs` holding the invitation proof and the description proof of a file, kept for history. One new function in simplexmq, the hash of the fields shared by all descriptions of one upload.

## Terms

**Core library.** The Haskell library shared by all apps. The apps display what it decides.

**simplexmq.** The library below the core library that transfers messages and files. The core library calls it and never changes its formats without a change there.

**P2p group.** A group whose members connect to each other directly. A new member is introduced to each existing member by the admin who admitted them, and until the two connect, the new member's messages reach the existing member forwarded by an admin.

**Channel.** A group whose messages go through relay servers. Members do not connect to each other. The channel owner signs the member list, the roster, which establishes each member's key.

**Introduction.** The messages by which an admin tells one member about another: `XGrpMemNew` to the existing members, `XGrpMemIntro` and `XGrpMemFwd` to the two members being connected. They include the member's profile and public key.

**Handshake.** The exchange when two members connect directly. Each side sends `XGrpMemInfo` with its group profile.

**File description.** The record the sender sends after an upload completes. It lists where each chunk of the file is stored and the keys to download and decrypt them. A file cannot be downloaded without it. It is sent in parts, in `XMsgFileDescr` messages.

**History.** The recent items the host sends to a member who has just joined. A file item is re-sent as a new invitation together with its description.

**Badge credential.** The secret record issued to the user: an issuer key index, a master key, a BBS signature, and the badge information (type, expiry, extra). Stored in the user's own profile row. Type `BadgeCredential` in `Badges.hs`.

**Badge proof.** A BBS proof generated from the credential for one presentation. It discloses the badge information and hides the master key. Different proofs from one credential cannot be linked. Type `BadgeProof`. In a profile it is sent as `Profile.badge`.

**Badge status.** What a receiver concludes about a proof: `BadgeStatus` (`Badges.hs:114`) — `BSActive`, `BSExpired`, `BSExpiredOld`, `BSFailed`, `BSUnknownKey` — computed by `mkBadgeStatus`, which treats a badge as active for seven days past its expiry.

**Presentation header.** A byte string that is an input to proof generation and to proof verification. A proof verifies only with the header it was generated with. Type `ProofPresHeader` in `Badges.hs`. `PHTest` is a random nonce.

**Chat binding.** The byte string that identifies the sender in one conversation, produced by `encodeChatBinding` (`Protocol.hs:444`). Message signatures and shared contact cards are computed over it. For a direct chat it is `encodeChatBinding CBDirect adHash`, where `adHash` is the hash of the connection's ratchet data, which both sides obtain with `getConnectionRatchetAdHash`. For a p2p group it is `encodeChatBinding CBGroup (smpEncode (memberId, memberKey))`. For a channel it is `encodeChatBinding CBGroup (smpEncode (publicGroupId, memberId))`. `groupBindingData` (`Internal.hs:2261`) computes the inner part for groups.

**Member key.** The Ed25519 key a member holds for one group. It is created when first needed — at group creation on this branch, or by `createUserMemberKey` before the first signed message — and the public key is sent in introductions and in `XInfo`.

**Default limit.** `maxFileSize`, 1GB. A supporter badge raises it to 2GB, a legend badge to 5GB (`maxXFTPFileSize`, `Badges.hs:201`). The three sizes are `FileSizeLimits` in `ChatConfig`, `defaultFileSizeLimits` in production and lowered in tests. The default limit is also the size above which a proof is required.

## 1. Presentation headers

File: `src/Simplex/Chat/Badges.hs`.

Extend `ProofPresHeader`:

```haskell
data ProofPresHeader
  = PHTest ByteString
  | PHChat ByteString
  | PHFileInv ByteString String Integer
  | PHFileDescr ByteString String Integer ByteString (Maybe UTCTime)
  | PHUnknown Char ByteString
```

- `PHChat` holds the chat binding.
- `PHFileInv` holds the chat binding and the file size from the invitation.
- `PHFileDescr` holds the same two values, then the shared description hash (section 8) and the file expiration.

The file name is not part of either header: `validateFileInvitation` replaces it with a name valid on the local file system, and history re-sends the stored name.

One constructor serves every chat type, because the chat binding already encodes the type of chat in its first byte. Each constructor gets a tag character in `ProofPresHeaderTag` and an encoding in the `StrEncoding` instance, in the same style as `PHTest`. The file expiration is optional, because a server may grant none; it is encoded as `strEncode` of the time, or one fixed byte when absent. The badge's own expiry is a time and is encoded with `strEncode` in the disclosed messages (`badgeInfoMessages`, `Badges.hs:296`).

`verifyBadgeWith` today verifies a proof with whatever header the proof contains. After this change the receiver first checks that the header names the sender as the receiver knows them, and only then runs BBS verification with that header. `proofPresHeaderAccepted` is removed; `verifyBadge` runs BBS verification only, and the header rule is applied by its callers:

- A contact request, link data, and the profile in a direct chat: the header must be `PHTest` until direct chats are bound (section 14).
- A file in a direct chat: the receiver has the contact's connection and obtains its ratchet hash from the agent, as `newContentMessage` does for a contact card (`Subscriber.hs:1883`). The binding in the header must equal `encodeChatBinding CBDirect adHash`.
- A profile or a file in a group: the receiver has the `GroupInfo` and the sender's `GroupMember`. The binding in the header must equal `groupBindingData` for that member — for a channel the group's public id and the member id; for a p2p group the member id and the member key.

**The key in a p2p binding.** For a file the receiver may not know the sender's key yet; if it knows one and it differs from the key in the header, the proof fails, otherwise the key in the header is used for this verification and never stored (`proofMemberKey`). For a profile the key is the one the message signature was verified under, section 3; a header key is never used.

The file headers are checked the same way and then further: `PHFileInv` must also name the file size as received; `PHFileDescr` must also hold the hash of the received description and the expiration received with it.

`PHUnknown` fails every check. A proof from a released client, which presents `PHTest` in groups, fails in groups; no badge has been issued yet, so nothing in use is affected. A released client that receives one of the new headers verifies it, because its `proofPresHeaderAccepted` admits unknown tags and BBS verification runs with the header bytes as sent. No protocol version change is needed.

The chat layer computes the expected binding and passes it to the store, so `groupBindingData` and `profileBadgeVerified` stay where they are. `Badges.hs` holds the two header predicates: `unboundProof`, true for a `PHTest` header, and `boundProof binding_`, true when the header equals `PHChat` of the given binding and false when there is none. `verifyBadge_` and `profileBadgeVerified` take the predicate as their first argument.

`SimplexDomainProof` (`Names.hs:37`) also uses `ProofPresHeader`, as an opaque value. Its verification is unchanged.

## 2. Presenting the profile badge

File: `src/Simplex/Chat/Library/Internal.hs`, `presentUserBadge` (`:2237`).

The function generates the proof for an outgoing profile. It takes a new argument, `Maybe GroupInfo`. With `Nothing` it generates `PHTest` as today. With `Just gInfo` it generates `PHChat` of `sndGroupChatBinding gInfo False`, the user's own member binding in that group; the membership key is stored by `mkGroupKeys` when the group is read with its keys, and a membership with no stored public key presents no badge. The proof is generated by `sndBadgeProof`, as file proofs are.

Call sites that send a profile into a group pass the group: `Commands.hs:3976` (join via group link, the relay case), `:4315` (the owner's profile to a relay); `Subscriber.hs:505` (the group case), `:637`, `:826`, `:840`, `:967`, `:1249`, `:3335`; `Internal.hs:2652` (`sendGroupProfileUpdate`). All other call sites send a direct profile and pass `Nothing`.

A join by `XContact` (`Commands.hs:3976`, a p2p group or a group not yet known) sends the profile without a badge, since section 3 does not accept one there. The two handshake sends (section 4) present the badge only when the peer version is at least `relayWebCapVersion`.

## 3. Accepting the profile badge

A received badge is verified today at seven places in the store layer, each verifying the proof with no knowledge of the sender: `profileBadgeVerified` (`Types.hs:851`), `createContact_` (`Store/Shared.hs:421`), `createJoiningMember` (`Store/Groups.hs:2085`), `createNewMemberProfile_` (`Store/Groups.hs:2440`), two contact request sites (`Store/ContactRequest.hs:170, 237`), and `linkDataBadge` (`Internal.hs:2253`).

The direct sites verify with `unboundProof`. The group store functions take the expected binding, `Maybe ByteString`, and verify with `boundProof`: `updateMemberProfile`, `updateContactMemberProfile` (`Store/Groups.hs:3411, 3434`), `createJoiningMember`, `createNewGroupMember`, `createIntroReMember`, and the functions that call `updateMemberProfile` inside the store — `updateUnknownMemberAnnounced`, `updateRosterMemberAnnounced`, `updatePreparedChannelMember`, `setRelayLinkAccepted`, `updateRelayMemberData`; the callers that build a profile from a name alone pass `Nothing`. A badge with no binding, or with a header that names another binding, is stored as failed, as a proof that fails BBS verification is today.

The chat layer computes the binding with `memberChatBinding gInfo memberId key_` (`Internal.hs`): for a channel `encodeChatBinding CBGroup (publicGroupId, memberId)`, whatever the key; for a p2p group `encodeChatBinding CBGroup (memberId, key)`, or `Nothing` without a key. `rcvGroupChatBinding` uses it for its member alternatives. The key passed is the one the message was verified under:

- `xInfoMember` (`Subscriber.hs:2798`) and `xGrpLinkMem` (`:2804`): the stored key when `RcvMessage.msgSigned` is `MSSVerified`; otherwise the key the message delivers, when `storeMemberKey` verified the signature with it and stored it — `storeMemberKey` returns that key; otherwise none.
- The member connection handshake, section 4: the stored key when it verifies the signature.
- `createJoiningMember`, `createNewGroupMember`, `createIntroReMember`, `updateUnknownMemberAnnounced`, `updateRosterMemberAnnounced`, `updatePreparedChannelMember`, and `updateMemberProfile` in `acceptGroupJoinRequestAsync`: no key. In a channel the binding is complete without it; in a p2p group a join by `XContact` (unsigned JSON) or an introduction (the key is asserted by the introducer) yields no binding, and the member's badge arrives at the handshake. A relay's profile, from its link data, passes `Nothing`.

So in a channel a badge is accepted from any profile message: from `XMember` when the member joins, from the introduction by a relay, and from `XInfo`. The binding names the member id and the group only, and a proof can be replayed only for the member who made it. In a p2p group a badge is accepted only from a message whose signature was verified under the member's key; `withVerifiedMsg` treats signatures as optional there, so the check is made at the three sites above.

## 4. The member connection handshake

When two p2p members connect, each sends `XGrpMemInfo` with its group profile. It is sent from two places: the reply on the member connection (`Subscriber.hs:843`) and the join of the member connection and of the direct connection to the same member (`:3336`, both joined with the same message at `:3345-3347`). The four receiving sites — `:615, 647` on the direct connection, `:837, 850` on the member connection — each have a "TODO update member profile" comment.

`XGrpMemInfo` is not in `requiresSignature`: the recipient enforces that list only in channels, where the handshake does not occur, and a signature needs the binary encoding, which the peer version decides. Instead `groupMsgSigning` signs `XGrpMemInfo` when its profile carries a badge, and the badge is presented only when the peer version is at least `relayWebCapVersion`, so a presented badge is always signed. The recipient keeps a badge only from a verified signature, so a profile without a badge needs none.

- **Sign the join side.** `xGrpMemFwd` sends `encodeConnInfo $ XGrpMemInfo ...` (`:3336`), plain JSON. It takes `GroupInfoKeys` from the dispatch and encodes with `encodeSignedConnInfo` when `groupMsgSigning` returns a signing. The agreed version is computed below the send, as `chatV`; that computation moves above it, and the badge is presented when `chatV` is at least `relayWebCapVersion`.
- **The reply side** (`:843`) presents the badge when `peerChatVRange` of the connection allows, and `allowAgentConnectionAsync` signs by the same rule.
- **Parse the signature on CONF.** The member CONF site parses with `parseChatMessage` (`:781`), which discards the signature. Change it to `parseChatMessage'`, as INFO already does (`:850`).
- **Verify and store.** At `:837` and `:850` the signature is verified with the member's stored key, as `storeMemberKey` does; `XGrpMemInfo` names no key, and the handshake follows the introduction, which stored the key. `processMemberProfileUpdate` stores the profile with the binding from the verified key, or with no binding.
- `:615` and `:647` stay as they are. The profile there is the same group profile, received over the direct connection to the member. The contact for a member shares the member's profile row (`createIntroToMemberContact`), so storing it once, on the member connection, updates both.

A member whose key was never introduced shows no badge until a signed `XInfo` that delivers the key arrives.

## 5. The file size limit at send

`checkSndFile` (`Commands.hs:3973`) compares the file size with the sender's limit and is called from the two content send paths only, with `Nothing` for an incognito send (`:4773`, `:4858`). `APIUploadStandaloneFile` (`:3628`) checks the hard limit and never the badge.

The comparison stays where it is, with one change: the limit at send counts a badge as active until one day after its expiry, instead of the seven days `maxXFTPFileSize` allows a receiver. `maxSndXFTPFileSize` in `Badges.hs` computes the send limit with that rule, from `FileSizeLimits` and the current time, and the apps use the same rule for the limit they show on the compose screen (section 11), so the compose screen never offers a size the send refuses.

Standalone uploads do not apply badge limits. `APIUploadStandaloneFile` keeps the hard limit.

## 6. The file invitation proof

**Type.** `FileInvitation` (`Types.hs:1555`) gains `fileBadge :: Maybe BadgeProof`. The JSON instance omits absent fields, so a released client ignores it.

**Generation.** In `xftpSndFileTransfer_` (`Internal.hs:438`), when the file is above the default limit and the send is not incognito, generate a proof with `PHFileInv` from the chat binding and the file size, and set it in the invitation. `sndFileChatBinding` computes the binding from `ContactOrGroup`: `CBDirect` with the ratchet hash of the contact's connection, obtained from the agent as `shareChatBinding` does (`Commands.hs:4685`); `CBChannel` with the public group id for content sent as the group; the member binding otherwise. `CGGroup` gains `ShowGroupAsSender`, which both send paths already hold.

A file sent as the group is bound to the channel because a relay forwards it as `FwdChannel` with no author, so the receiver has no member to rebind to. The receiver picks the same case from the item's `showGroupAsSender`.

**Verification.** A file invitation arrives at three places: `processFileInvitation` (`Subscriber.hs:1957`), for a file in a content message in a direct chat or a group, called with a closure that creates the transfer; `processGroupFileInvitation'` (`:2437`), for the older `XFile` event in a group; and `processFileInvitation'` (`:2422`) for `XFile` in a direct chat. The proof is checked against the file size and against the sender: the connection's ratchet hash for a contact, the group and member for a member. The result is the decision below, passed to `createRcvFileTransfer` or `createRcvGroupFileTransfer`, which gain it as an argument and write it.

**The decision.** A file is either allowed or prohibited; when prohibited, the apps need the limit that applied and why.

```haskell
data FileProhibited = FileProhibited {maxSize :: Int64, badgeStatus :: Maybe BadgeStatus}
```

`Nothing` when allowed; `Just` when prohibited:

- Above the default limit, and the invitation has no proof: the default limit and `Nothing`.
- Above the default limit, and the proof fails the header check or BBS verification: the default limit and `BSFailed`; `BSUnknownKey` when the issuer key index is not configured.
- Above the default limit, and the proof verifies but the badge has expired beyond the grace: the default limit and the expiry status.
- Above the limit that a verified, active badge allows: that limit and `BSActive`.

The sender's profile badge plays no part. An invitation without a proof gets the default limit in every chat type.

**Storage.** Two new columns on `files`: `file_max_size INTEGER`, the limit that applied, and `file_badge_status TEXT`, the badge status, both NULL when the file is allowed. A file is prohibited when `file_max_size` is set; `file_badge_status` is NULL when the invitation had no proof. `BadgeStatus` gains `TextEncoding` and field instances for the column, as `MsgSigStatus` has (`Types/Shared.hs:137`). `createRcvFileTransfer` and `createRcvGroupFileTransfer` (`Store/Files.hs:448, 469`) write both. Sent and local files, and rows from before this change, hold NULL.

A proof that verifies is stored in `file_badge_proofs` (section 12) with kind `inv`, for history (section 9); a proof that failed is not stored, as `files.file_badge_status` records that it failed. A sent file stores its own proof the same way.

**The chat item.** `CIFile` (`Messages.hs:684`) gains `fileProhibited :: Maybe FileProhibited`. `MaybeCIFIleRow` (`Store/Messages.hs:2279`) gains the column, the three queries that select the file columns gain `f.file_max_size, f.file_badge_status`, and the two `maybeCIFile` constructors and the five other `CIFile` constructions (`Internal.hs:449`, `Subscriber.hs:2001, 2456, 2472`, `Commands.hs:5009`) set it.

**Accepting a file.** `acceptFileReceive` (`Internal.hs:746`) fails with `CEFileSize` when the file is prohibited. The apps stop a tap before that, from the same field.

## 7. The file description proof

**Type.** `XMsgFileDescr` (`Protocol.hs:454`) gains `fileBadge :: Maybe BadgeProof`, encoded with `.=?` like `fileExpires`.

**Generation.** The description proof is generated when the upload completes, which may be hours after the send, from the credential as it is then. If the badge expired meanwhile, the proof still verifies at receivers, which allow seven days past expiry; if the user hid the badge, the credential row is still there; if the badge was renewed, the new credential is used. Only a credential deleted outright leaves the file without a description proof, and receivers then prohibit it. Nothing is copied at send.

In the `SFDONE` handler (`Subscriber.hs:209`), when the file is above the default limit, the user holds a credential, and the send was not incognito — the handler has the chat item and its contact or group, so the same condition as at the invitation — generate one proof with `PHFileDescr`: the values of the invitation header, `sharedDescriptionHash` of any one recipient description, and `fileExpires`. `sendFileDescriptions` (`:283`) sets it on the last part for each recipient, in the direct branch (`:237`) and the group branch (`:252`) alike; the parts are split in `splitText` (`:297`).

**Verification.** When the file is allowed and above the default limit:

1. The part that completes the description must have a proof. `processFDMessage` (`Subscriber.hs:1940`) receives every part and calls `receiveViaCompleteFD` when the description is complete and the file was accepted. It verifies the proof on the completing part, before that call, and on success stores it in `file_badge_proofs` with kind `descr`. On failure it records the same decision as a failed invitation proof — the default limit and the badge status on `files` — so the file is refused by `acceptFileReceive` and the apps show the reason from one field. A file that requires a badge is received from the description message: `validateFileInvitation` stores its description as incomplete, so a complete description in an invitation cannot start a download unverified.
2. Check the header: binding, name, size as at the invitation; the hash equal to `sharedDescriptionHash` of the parsed description; the expiration equal to `fileExpires` from the message.
3. If the expiration is present and in the past, fail.
4. If the expiration is absent, accept. Older servers grant no expiration. This is tightened once servers are upgraded.

The error set on failure is a new `FileError` value, so the apps can name the reason.

## 8. The shared description hash

File: `Simplex.FileTransfer.Description` in simplexmq.

`sndFileToDescrs`, in the agent's `Simplex.FileTransfer.Agent`, builds one description per recipient from one set of values. The values common to the sender's description and every recipient's are `size`, `digest`, `key`, `nonce`, `chunkSize`, and for each chunk `chunkNo`, `chunkSize` and `digest`. `party` differs between sender and recipient, `replicas` differ per recipient, and `redirect` is absent from the sender's.

Add `sharedDescriptionHash :: FileDescription p -> ByteString`: SHA-512 over a fixed encoding of those values in that order. Defining it beside the type keeps it in step with the format.

Because the hash ignores replicas, one proof is valid for every recipient's description, including one re-sent later as history. In a channel the sender sends descriptions to the relays (`getGroupRelayMembers`, `Subscriber.hs:260`), which forward them to the members; whichever description a member receives, the hash is the same.

## 9. History

`sendHistory` (`Internal.hs:1366-1481`) re-sends a file item to a new member as a new invitation built from the stored name and size (`invCompleteDescr`, `:1445`) with the description in `XMsgFileDescr` parts (`:1481`). Content is not signed, so nothing from the original messages survives.

Both proofs are read from `file_badge_proofs` by kind. For a file received from another member they are re-sent unchanged: they are bound to the original sender, and history names that sender (`fwdSender`, `:1466`), so the new member verifies them against that member's binding. For the host's own files the stored proofs are re-sent too, and are re-made from the credential over the stored headers only when the badge that made them is past the send grace and the current badge is active — never for a signed item, whose original bytes are forwarded.

`fileExpired` (`:1439-1443`) decides which files history re-sends by the item's age against `rcvFilesTTL`, two days, and ignores the granted expiration stored with the file. That check should use the stored `fileExpires`; it is noted here because it bounds when the stored proofs are read.

## 10. Forwarding

Forwarding a file uploads it again from the local copy, so the forwarder's own limit applies. The forward plan (`APIPlanForwardChatItems`, `Commands.hs:1004-1050`) runs before the destination is chosen; it checks whether the file was received and exists, and nothing about size. A too-large forward fails at send, after the user has chosen the recipient.

Two checks, both before any upload:

- **In the apps, before the sheet.** The app has the file size and the user's own badge, so it decides without calling core, with the same send rule the compose screen uses (section 11). The per-item Forward action, which opens the sheet directly today — `forwardedChatItems = [chatItem]` (`ChatView.swift:2407`), `SharedContent.Forward(listOf(cItem), cInfo)` (`ChatView.kt:625`) — shows the alert instead when the file is above that limit. Multi-select (`ChatView.swift:1515`, `ChatView.kt:347`) makes the same check on the selected items before calling the plan, and shows the same alert with the count when any file is above it. The plan command is unchanged.
- **In the sheet.** When a forwarded item has a file above the default limit, the sheet where the destination is chosen disables every chat in which the user is incognito — a contact with `contactConnIncognito`, a group with `memberIncognito` on the membership — because the badge is not presented there and the file cannot be sent.
- **In the forward command**, `APIForwardChatItems` (`Commands.hs:1052`), once the destination is known: each file is checked against the destination's limit — the default for an incognito membership, as `checkSndFile` decides today — before any item is created or upload started. A file above it fails the command with a new `ChatErrorType` value naming the count, which the apps show as the same alert.

## 11. The apps

The receive decision is computed in eleven places from the sender's profile: `getMaxFileSize(protocol, senderProfile)` (`FileUtils.swift:280`, `Utils.kt:477`) and `fileSizeValid(file, senderProfile)` (`CIFileView.swift:236`, `CIFileView.kt:241`), used in `CIFileView`, `CIImageView`, `CIVideoView`, `ChatView.swift:2323`, `ChatItemView.kt:452`, with a second copy of the check in each video view (`receiveFileIfValidSize`).

- `CIFile` gains `fileProhibited` in `ChatTypes.swift:4681` and `ChatModel.kt:4249`, with `FileProhibited` decoded from core.
- `fileSizeValid` becomes a check that `fileProhibited` is absent and takes no profile. The alert is worded by `badgeStatus` — no badge, unverified, unknown key, expired, or above the badge's limit — with `maxSize` as the figure.
- `getMaxFileSize` loses the profile argument for received files. The compose screen keeps computing the sender's own limit from the user's own badge (`ComposeView.swift:1272`, `ComposeView.kt:1423`), with the one-day rule of section 5 instead of the seven-day status; `ShareModel.swift:448, 539` and `ComposeView.kt:118` start passing the profile, so they stop showing 1GB to a badge holder.
- `ciSenderProfile` and the `senderProfile` parameters are removed from the file, image and video views and their call sites in `FramedItemView` and `ChatPreviewView`.
- `FileError` gains the new value in `ChatTypes.swift` and `ChatModel.kt`, with a message for it.
- The generated API mirrors — `bots/api/TYPES.md`, `types.ts`, `_types.py` — are regenerated for `CIFile`, `FileProhibited` and `FileInvitation`.

## 12. Schema and fixtures

Migration `M20260904_file_badges`, SQLite and Postgres:

```sql
ALTER TABLE files ADD COLUMN file_max_size INTEGER;
ALTER TABLE files ADD COLUMN file_badge_status TEXT;

CREATE TABLE file_badge_proofs(
  badge_proof_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER NOT NULL REFERENCES files ON DELETE CASCADE,
  proof_kind TEXT NOT NULL,
  badge_proof BLOB NOT NULL,
  badge_pres_header BLOB NOT NULL,
  badge_key_idx INTEGER NOT NULL,
  badge_type TEXT NOT NULL,
  badge_expiry TEXT NOT NULL,
  badge_extra TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE UNIQUE INDEX idx_file_badge_proofs_file_id_kind ON file_badge_proofs(file_id, proof_kind);
```

The six proof columns are the fields of `BadgeProof` — the proof, the presentation header, the issuer key index, and the disclosed type, expiry and extra — with a conversion of its own. A file is one direction, so it has at most two proofs; `proof_kind` is `inv` or `descr`, and the unique index makes each a single upsert. The row references the file with `ON DELETE CASCADE`, so it is removed with the file, which is how file rows are removed today — by cascade from chat items, contacts and groups rather than by one function. Postgres uses `BYTEA`, `BIGINT` and `GENERATED ALWAYS AS IDENTITY`. Register in both `Migrations.hs` lists and in `simplex-chat.cabal`. Update both `chat_schema.sql` files and `chat_query_plans.txt`; `SchemaDump.hs` compares them.

## 13. Tests

- `BadgeTests.hs`: each header encodes and decodes; a proof generated with one header fails with another; the key check accepts an unknown key, accepts an equal key, rejects a different one.
- `ChatTests/Profiles.hs`, beside the seven badge tests: a badge in a p2p group appears at the other member after the connection handshake and not before; a proof presented under another member's binding is rejected; a badge in a channel appears on presentation. Existing tests that assert a badge at introduction time are updated.
- `ChatTests/Files.hs`, beside `testXFTPGroupFileTransfer`: a file above the default limit from a badge holder is received in a group and in a direct chat; an invitation whose proof was made for another member is refused; a description with a changed hash fails before download; a file above the limit received as history is received by the new member; a forward into an incognito membership above the default limit fails the command before any upload.
- `ProtocolTests.hs`: the new fields in `FileInvitation` and `XMsgFileDescr`.

## 14. Direct chats

The binding of a direct chat is the ratchet associated data, `rcAD = k1_snd ‖ k1_rcv` (simplexmq `Ratchet.hs:498`), hashed: the value `getConnectionRatchetAdHash` returns, already the `CBDirect` payload of file proofs and shared contact cards. A contact request to an address without ratchet keys has no ratchet yet; its binding is the request itself. Both are made available by the agent before the chat composes the message, so every profile message is bound and nothing is sent after connection. Implementation starts in simplexmq, in `/code/simplexmq-4`; the chat then builds against it.

### 14.1 Agent

**Second verification code.** `RatchetInitParams` gains `rcVerifyCodePQ` and `Ratchet` gains `rcVCPQ :: Maybe Str`. `pqX3dh` expands the KDF to 128 bytes with `hkdf4` and takes the last 32 as the code; the first 96 bytes are the same output as today, so peers on either version derive the same keys, and `rcAD` stays the AEAD associated data. It is not associated data itself — it is exported keying material, the fourth of the paper's mitigations, and it covers every handshake input, the KEM included. A ratchet created before this change decodes with `rcVCPQ = Nothing`, and the value cannot be computed for it afterwards — the handshake secrets are gone — so it appears at the next ratchet resync or not at all. For that reason the chat keeps using the AD code, `codeAD`, for the security code and for badge bindings; `codePQ` is stored now and used when connections have it.

**Columns.** Migration `M20260919_ratchet_ad`, SQLite and Postgres, both schema dumps: `ratchets` gains `ratchet_ad BLOB` and `ratchet_ad_pq BLOB` (`BYTEA` on Postgres). `createRatchet` and `createSndRatchet` write both, also through their `ON CONFLICT` update, which is how a resync recreates the ratchet.

One store function, `getRatchetADs :: DB.Connection -> [ConnId] -> IO (Map ConnId (ByteString, Maybe ByteString))`, serves one id or many, with one SELECT and at most one batched UPDATE:

```sql
SELECT conn_id, ratchet_ad, ratchet_ad_pq, CASE WHEN ratchet_ad IS NULL THEN ratchet_state END
FROM ratchets
WHERE conn_id IN (?, ?, ...)
```

The blob column is NULL in every row that has `ratchet_ad`, so an established connection costs one small row and no JSON decoding. Rows are `(ConnId, Maybe ByteString, Maybe ByteString, Maybe RatchetX448)`; a row with the AD is used as is; a row with the blob is decoded for `rcAD` and `rcADPQ` and collected; a row with neither (a ratchet row holding only x3dh keys, before CONF) is skipped. The collected rows are written back with one `executeMany "UPDATE ratchets SET ratchet_ad = ?, ratchet_ad_pq = ? WHERE conn_id = ?"`. On Postgres the list is `In connIds`; on SQLite the placeholder list is built from the id count, in chunks of 500 to stay under the variable limit. Only a NULL `ratchet_ad` selects the blob: `ratchet_ad_pq` stays NULL for a ratchet created before the change and never causes a second read.

`getConnectionVerifyCodes` uses it with one id, `getConnectionsVerifyCodes` with many; both return `ConnVerifyCodes {codeAD, codePQ}`, defined in `Agent/Protocol.hs`, where `codeAD = sha256 rcAD` and `codePQ` is the PQ code as derived.

**Prepare step.** `prepareConnectionToJoin` returns `(ConnId, ConnVerifyCodes)`, the binding for the message the chat composes next:

- `CRInvitationUri`: creates the sender ratchet (`createRatchet_`, local — the link's keys and a fresh keypair) and returns its codes.
- `CRContactUri` with ratchet keys: the same, from the address keys.
- `CRContactUri` without keys: generates the x3dh keys (`generateRcvE2EParams`, `createRatchetX3dhKeys`) and returns `codeAD = sha256 (k1 ‖ k2 ‖ kem ‖ senderId)` with no `codePQ` — the request's public keys and the queue id from the link's `SMPQueueUri`.

`prepareConnectionToAccept` returns the same pair: for `CRInvitation` it creates the ratchet from the invitation's keys, for `CRInvitationDR` it takes the ratchet stored in the invitation. `startJoinInvitation` reads the ratchet before creating one, as the contact path and its retry branch already do; `createConnReq` reads the x3dh keys before generating them, as `mkJoinInvitation` does. Async joins and accepts then find the ratchet in place. Nothing in the prepare step touches the network.

**Events.** `REQ` gains a `ConnVerifyCodes` field: `smpInvitation` computes `codeAD` from the received `CRInvitationUri` and the queue it arrived on; `smpContactRequest` passes the codes of the ratchet it initialised. `CONF` and `INFO` are unchanged: the receiver's ratchet is stored before the notification, so the getter serves. A request without a ratchet proves no key possession; X448 keys cannot sign, and a forged request with copied public keys shows the badge in the request list and then fails to connect — accepted.

**Tests.** `DoubleRatchetTests`: the parties agree on `rcVerifyCodePQ`, a substituted KEM key makes it differ while `assocData` matches, and a ratchet stored before the change decodes with `rcVCPQ = Nothing`. `FunctionalAPITests`: both peers get the same codes, and codes cleared from a row are recomputed and saved on the next read.

### 14.2 Chat

The chat uses `codeAD` — for the security code it shows today and for every badge binding. `codePQ` is stored by the agent and used later, once connections have it on both sides; a ratchet created before the change never does without a resync.

`presentUserBadge` takes the binding, `Maybe ByteString`, for both chat kinds — `sndGroupChatBinding gInfo False` for a group, `codeAD` for a direct chat — and generates `PHChat`; the `PHTest` branch, `unboundProof` and the `PHTest` acceptance go.

Sending: a join via one-time link (`Commands.hs:3826`, member contact `:3408`, `Subscriber.hs:3933`) and a request (`joinContact`, `Commands.hs:3980`) use the value from `prepareConnectionToJoin`; accepting (`Internal.hs:998, 1010`) the value from `prepareConnectionToAccept`; INFO (`Subscriber.hs:505` direct case, `:624`) the getter on the connection; `XInfo` to contacts (`Commands.hs:4062, 4095`, `presentUserBadgeToContacts`) the bulk getter, once per command.

Receiving: `updateContactProfile` (`Direct.hs:566`), `createContact_` (`Shared.hs:419`), the request sites (`ContactRequest.hs:170, 237`) and `linkDataBadge` take `Maybe ByteString` and verify with `boundProof`; `processContactProfileUpdate` (`Subscriber.hs:2758`) and the contact creation at CONF (`:3173`) pass the getter's value, the request sites the `REQ` field. After a resync the value changes on both sides; a badge received under the old one fails and is re-verified with the next update, which `badgeNeedsReverify` already does.

Link data: bound to the link key with a new `ChatBinding` constructor, `CBLink`, payload `strEncode linkKey` from `CSLContact` or `CSLInvitation`, which both the owner and anyone with the link hold. The key is derived from local data (`encodeSignLinkData`), so `prepareConnectionLink` is generalised to addresses and invitation links and the badge is bound before `createConnectionForLink` uploads the data; updates (`setMyAddressData`, `updatePCCShortLinkData`) already hold the link.

## Out of scope

- Requiring an expiration in the description proof, once servers grant one.
