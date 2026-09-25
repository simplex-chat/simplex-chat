# Badge proofs bound to the conversation, and file size limits decided in core

**Date:** 2026-09-04
**Branch:** ep/p2p-group-signing. The work depends on member keys and signed profile messages, which this branch adds.

## Summary

A badge is a credential issued to a user who supports SimpleX Chat. The user shows it to others by putting a proof in their profile. This plan binds every proof to the place where it is shown: a proof in a group profile to the sender's identity in that group, a proof attached to a file to the conversation and to that file. Verification checks the binding.

The same badge raises the size limit for files the user sends. Today each app decides whether a received file is within the limit, using the sender's profile as it is at the moment of display. After this plan, the core library decides once, when the file invitation arrives, from a proof in the invitation; stores the decision with the file; and the apps read it. A second proof arrives with the file description, the record that says where the file's chunks are stored, and is checked before the download starts.

The changes:

1. Four new presentation headers: a profile shown in a chat, a file invitation, a file description, and a contact request. The random header of released clients stays accepted.
2. Every profile badge is bound to the chat it is shown in: a direct chat, a contact request, a link, or a group membership (section 14). A proof bound to another chat is ignored.
3. In p2p groups a badge is accepted from a message signed by the member, from an introduction under the key it introduces, and from a join request under the request header. In channels it is accepted from any profile message.
4. Files above the default limit include a proof in the invitation and a proof in the description, in every chat type. The core library verifies both. The decision is stored on the file and shown by the apps from one field.
5. Forwarding a file above the forwarder's limit is refused with an alert before the forwarding sheet opens, and again, for the chosen destination, before anything is uploaded.
6. A received file keeps its two proofs, so a file re-sent to a new member as part of history keeps them; the sender's own files get fresh proofs from the credential.

Two new columns on `files`, and a new table `file_badge_proofs` holding the invitation proof and the description proof of a file, kept for history. A new column on `connections`, the request header of a prepared connection. In simplexmq: the hash of the fields shared by all descriptions of one upload, the verification codes of a connection, links and invitations before link data is signed, and the request code of an invitation.

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

**Chat binding.** The byte string that identifies the sender in one conversation, produced by `encodeChatBinding` (`Protocol.hs:444`). Message signatures and shared contact cards are computed over it. For a direct chat it is `encodeChatBinding CBDirect codeAD`, where `codeAD` is the hash of the connection's ratchet data, which both sides obtain with `getConnectionVerifyCodes`. For a p2p group it is `encodeChatBinding CBGroup (smpEncode (memberId, memberKey))`. For a channel it is `encodeChatBinding CBGroup (smpEncode (publicGroupId, memberId))`. For a link it is `encodeChatBinding CBLink linkKey`. `groupBindingData` (`Internal.hs:2322`) computes the inner part for groups.

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
  | PHRequest ByteString
  | PHUnknown Char ByteString
```

- `PHChat` holds the chat binding.
- `PHRequest` holds the request code (section 14.1).
- `PHFileInv` holds the chat binding and the file size from the invitation.
- `PHFileDescr` holds the same two values, then the shared description hash (section 8) and the file expiration.

The file name is not part of either header: `validateFileInvitation` replaces it with a name valid on the local file system, and history re-sends the stored name.

One constructor serves every chat type, because the chat binding already encodes the type of chat in its first byte. Each constructor gets a tag character in `ProofPresHeaderTag` and an encoding in the `StrEncoding` instance, in the same style as `PHTest`. The file expiration is optional, because a server may grant none; it is encoded as `strEncode` of the time, or one fixed byte when absent. The badge's own expiry is a time and is encoded with `strEncode` in the disclosed messages (`badgeInfoMessages`, `Badges.hs:296`).

`verifyBadgeWith` today verifies a proof with whatever header the proof contains. After this change the receiver first checks that the header names the sender as the receiver knows them, and only then runs BBS verification with that header. `proofPresHeaderAccepted` is removed; `verifyBadge` runs BBS verification only, and the header rule is applied by its callers:

- A contact request, link data, and the profile in a direct chat: the header of section 14.
- A file in a direct chat: the receiver has the contact's connection and obtains its ratchet hash from the agent, as `newContentMessage` does for a contact card (`Subscriber.hs:1883`). The binding in the header must equal `encodeChatBinding CBDirect codeAD`.
- A profile or a file in a group: the receiver has the `GroupInfo` and the sender's `GroupMember`. The binding in the header must equal `groupBindingData` for that member — for a channel the group's public id and the member id; for a p2p group the member id and the member key.

**The key in a p2p binding.** For a file the receiver may not know the sender's key yet; if it knows one and it differs from the key in the header, the proof fails, otherwise the key in the header is used for this verification and never stored (`proofMemberKey`). For a profile the key is the one the message signature was verified under, section 3; a header key is never used.

The file headers are checked the same way and then further: `PHFileInv` must also name the file size as received; `PHFileDescr` must also hold the hash of the received description and the expiration received with it.

`PHUnknown` fails every check. `PHTest`, presented by released clients, is accepted in every chat. A released client that receives one of the new headers verifies it, because its `proofPresHeaderAccepted` admits unknown tags and BBS verification runs with the header bytes as sent. No protocol version change is needed.

The expected header is computed in the chat layer and passed to the store, so `groupBindingData` and `profileBadgeVerified` stay where they are. One predicate is defined in `Badges.hs`: `acceptedProof`, true for `PHTest` and for a header equal to the expected one (section 14.3).

`SimplexDomainProof` (`Names.hs:37`) also uses `ProofPresHeader`, as an opaque value. Its verification is unchanged.

## 2. Presenting the profile badge

File: `src/Simplex/Chat/Library/Internal.hs`, `presentUserBadge` (`:2238`).

The proof for an outgoing profile is generated with `sndBadgeProof`, as file proofs are. The header, `Maybe ProofPresHeader`, is a parameter; with `Nothing`, no badge is presented. For a send into a group the header is `groupPresHeader`, `PHChat` of `sndGroupChatBinding gInfo False`, the user's own member binding in that group. The membership key is stored by `mkGroupKeys` when the group is read with its keys; for a membership without a stored public key, no badge is presented. The header of every call site is listed in section 14.3.

The two handshake sends (section 4) present the badge only when the peer version is at least `relayWebCapVersion`.

## 3. Accepting the profile badge

A received badge is verified today at seven places in the store layer, each verifying the proof with no knowledge of the sender: `profileBadgeVerified` (`Types.hs:851`), `createContact_` (`Store/Shared.hs:421`), `createJoiningMember` (`Store/Groups.hs:2085`), `createNewMemberProfile_` (`Store/Groups.hs:2440`), two contact request sites (`Store/ContactRequest.hs:170, 237`), and `linkDataBadge` (`Internal.hs:2253`).

The expected header, `Maybe ProofPresHeader`, is a parameter of the store functions and is checked with `acceptedProof` (section 14.3); `Nothing` is passed where a profile is built from a name alone. A proof rejected by `acceptedProof` is ignored: at creation no badge is stored, at update the stored badge is kept. A proof that fails BBS verification is stored as failed.

The chat layer computes the binding with `memberChatBinding gInfo memberId key_` (`Internal.hs`): for a channel `encodeChatBinding CBGroup (publicGroupId, memberId)`, whatever the key; for a p2p group `encodeChatBinding CBGroup (memberId, key)`, or `Nothing` without a key. `rcvGroupChatBinding` uses it for its member alternatives. The key passed at each site:

- `xInfoMember` (`Subscriber.hs:2801`) and `xGrpLinkMem` (`:2807`): the stored key when `RcvMessage.msgSigned` is `MSSVerified`; otherwise the key the message delivers, when `storeMemberKey` verified the signature with it and stored it — `storeMemberKey` returns that key; otherwise none.
- The member connection handshake, section 4: the stored key when it verifies the signature.
- Introductions — `createNewGroupMember`, `createIntroReMember`, `updateUnknownMemberAnnounced`: the key in the `MemberInfo`.
- Channel sites — `updateRosterMemberAnnounced`, `updatePreparedChannelMember`, a join via a relay (`createJoiningMember`, `updateMemberProfile`): no key; the channel binding is complete without it.
- A join by `XContact` to a p2p group: the request header (section 14.3).
- A relay's profile, from its link data: `Nothing`.

In a channel a badge is accepted from any profile message: from `XMember` when the member joins, from the introduction by a relay, and from `XInfo`. In a p2p group a badge is accepted from a message whose signature was verified under the member's key, from an introduction under the key it introduces, and from a join request under the request header.

## 4. The member connection handshake

When two p2p members connect, each sends `XGrpMemInfo` with its group profile. It is sent from two places: the reply on the member connection (`Subscriber.hs:843`) and the join of the member connection and of the direct connection to the same member (`:3336`, both joined with the same message at `:3345-3347`). The four receiving sites — `:615, 647` on the direct connection, `:837, 850` on the member connection — each have a "TODO update member profile" comment.

`XGrpMemInfo` is not in `requiresSignature`: the recipient enforces that list only in channels, where the handshake does not occur, and a signature needs the binary encoding, which the peer version decides. Instead `groupMsgSigning` signs `XGrpMemInfo` when its profile includes a badge, and the badge is presented only when the peer version is at least `relayWebCapVersion`, so a presented badge is always signed. The recipient keeps a badge only from a verified signature, so a profile without a badge needs none.

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
- `ChatTests/Profiles.hs`, beside the seven badge tests: a badge in a p2p group is shown at the other member from the introduction and from the connection handshake; a proof presented under another member's binding is ignored; a badge in a channel is shown on presentation. The tests of section 14.3 follow.
- `ChatTests/Files.hs`, beside `testXFTPGroupFileTransfer`: a file above the default limit from a badge holder is received in a group and in a direct chat; an invitation whose proof was made for another member is refused; a description with a changed hash fails before download; a file above the limit received as history is received by the new member; a forward into an incognito membership above the default limit fails the command before any upload.
- `ProtocolTests.hs`: the new fields in `FileInvitation` and `XMsgFileDescr`.

## 14. Bindings in every chat

Every profile badge is bound to the chat it is shown in. The sender presents the proof under the header of that chat; the receiver computes the expected header and accepts the proof only under it. Implementation starts in simplexmq, in `/code/simplexmq-4`; the chat then builds against it.

Rules:

1. A ratchet held by both sides: the ratchet code.
2. A connection request before the ratchet: the request code — the proposing party's X448 keys and the id of the queue the joining party sends to.
3. A link shown to anyone who holds it: the link key.
4. A group: the member identity.

| Context | Received by | Header |
|---|---|---|
| Direct chat: `XInfo`, `CONF` and `INFO` replies, accept, one-time link join, member contacts | contact | `PHChat (encodeChatBinding CBDirect codeAD)` |
| Request to an address with ratchet keys | address owner | `PHChat (encodeChatBinding CBDirect codeAD)` |
| Request to an address without ratchet keys | address owner, group host | `PHRequest code` — the joining party's keys, the address queue |
| One-time invitation link data | joining party | `PHRequest code` — the inviter's keys, the invitation queue |
| Address link data, shared address card | anyone with the link | `PHChat (encodeChatBinding CBLink linkKey)` |
| P2p group | members | `PHChat (encodeChatBinding CBGroup (smpEncode (memberId, memberKey)))` |
| Channel | subscribers | `PHChat (encodeChatBinding CBGroup (smpEncode (publicGroupId, memberId)))` |

An address is a contact address, a business address, or a group link. The link key is part of the link: `sha3_256` of the fixed link data — the root key, the entity id, and the connection request with its server and queue id (`ShortLink.hs:61-64`).

### 14.1 Agent: verification codes

Merged in simplexmq `5294b7d8`.

**Second verification code.** `RatchetInitParams` gains `rcVerifyCodePQ` and `Ratchet` gains `rcVCPQ :: Maybe Str`. `pqX3dh` derives the code with a separate HKDF over the same inputs, info `SimpleXVerifyCode`, 32 bytes; the ratchet keys and `rcAD` are unchanged. It is not associated data itself — it is exported keying material, the fourth of the paper's mitigations, and it covers every handshake input, the KEM included. A ratchet created before this change decodes with `rcVCPQ = Nothing`, and the value cannot be computed for it afterwards — the handshake secrets are gone — so it appears at the next ratchet resync or not at all. For that reason the chat keeps using the AD code, `codeAD`, for the security code and for badge bindings; `codePQ` is stored now and used when connections have it.

**Columns.** Migration `M20260919_ratchet_verify_codes`, SQLite and Postgres, both schema dumps:

```sql
ALTER TABLE ratchets ADD COLUMN rc_verify_code_ad BLOB;
ALTER TABLE ratchets ADD COLUMN rc_verify_code_pq BLOB;
```

`BYTEA` on Postgres. `createRatchet` and `createSndRatchet` write both, also through their `ON CONFLICT` update, which is how a resync recreates the ratchet.

One store function, `getRatchetVerifyCodes :: DB.Connection -> [ConnId] -> IO (Map ConnId ConnVerifyCodes)`, serves one id or many:

```sql
SELECT conn_id, rc_verify_code_ad, rc_verify_code_pq, CASE WHEN rc_verify_code_ad IS NULL THEN ratchet_state END
FROM ratchets
```

- A row with `rc_verify_code_ad` is used as is.
- A row with the ratchet state is decoded, and its codes are computed and saved.
- A row with x3dh keys only is skipped.
- SQLite: one SELECT per id; the computed codes are saved with one `executeMany` UPDATE.
- Postgres: one SELECT with `In connIds`; the computed codes are saved with one `UPDATE ... FROM (VALUES ...) RETURNING`; codes saved by a concurrent read are kept.

It is used by `getConnectionVerifyCodes` with one id, with `CONN NOT_FOUND` for a connection without codes, and by `getConnectionsVerifyCodes` with many. The result is `ConnVerifyCodes {codeAD, codePQ}`, defined in `Agent/Protocol.hs`, where `codeAD = sha256 rcAD` and `codePQ` is the PQ code as derived.

**Prepare step.** The result of `prepareConnectionToJoin` is `(ConnId, ContactRequestBinding)`, the binding for the message the chat composes next:

```haskell
data ContactRequestBinding = CRBRatchet ConnVerifyCodes | CRBRequest ByteString
```

- `CRInvitationUri`: the sender ratchet is created (`createRatchet_`, local — the link's keys and a fresh keypair); the binding is `CRBRatchet` of its codes.
- `CRContactUri` with ratchet keys: the same, from the address keys.
- `CRContactUri` without keys: the x3dh keys are generated and stored (`generateRcvE2EParams`, `createRatchetX3dhKeys`); the binding is `CRBRequest (sha256 (smpEncode (k1, k2, kem, senderId)))` — the request's public keys and the queue id from the link's `SMPQueueUri`. The PQ key is removed from this code in section 14.2.

`prepareConnectionToAccept` returns the same pair: for `CRInvitation` it creates the ratchet from the invitation's keys, for `CRInvitationDR` it takes the ratchet stored in the invitation. `startJoinInvitation` reads the ratchet before creating one, as the contact path and its retry branch already do; `createConnReq` reads the x3dh keys before generating them, as `mkJoinInvitation` does. Async joins and accepts then find the ratchet in place. Nothing in the prepare step touches the network.

**Events.** A `ContactRequestBinding` field in `REQ`: `CRBRequest` in `smpInvitation`, computed from the received `CRInvitationUri` and the queue of the request; `CRBRatchet` in `smpContactRequest`, from the ratchet initialised there. `CONF` and `INFO` are unchanged: the receiver's ratchet is stored before the notification, so its codes are available to `getConnectionVerifyCodes`.

**Tests.** `DoubleRatchetTests`: the parties agree on `rcVerifyCodePQ`, a substituted KEM key makes it differ while `assocData` matches, and a ratchet stored before the change decodes with `rcVCPQ = Nothing`. `FunctionalAPITests`: both peers get the same codes, and codes cleared from a row are recomputed and saved on the next read.

### 14.2 Agent: links before link data

Files: `Agent.hs`, `Agent/Protocol.hs`, `tests/AgentTests/FunctionalAPITests.hs`.

Every link, and every invitation, is available to the chat before its link data is signed; each creation makes one network request.

**New links.** The link, and for an invitation the invitation, are returned by `prepareConnectionLink` before the user data is signed, in both connection modes:

```haskell
prepareConnectionLink :: ConnectionModeI c => AgentClient -> UserId -> SConnectionMode c -> C.KeyPairEd25519 -> Maybe ByteString -> Bool -> Maybe CRClientData -> CR.InitialKeys -> UseRatchetKeys -> Maybe SMPServerWithAuth -> AE (CreatedConnLink c, PreparedLinkParams)

createConnectionForLink :: ConnectionModeI c => AgentClient -> NetworkRequestMode -> UserId -> Bool -> CreatedConnLink c -> PreparedLinkParams -> UserConnLinkData c -> SubscriptionMode -> AE (ConnId, CreatedConnLink c)
```

- The link entity id is optional; `Nothing` for an invitation.
- A new field in `PreparedLinkParams`:

  ```haskell
  plpInvitationKeys :: Maybe (RcvE2EPrivRatchetParams 'C.X448)
  ```

- Contact mode is unchanged; the link given to `createConnectionForLink` is returned.
- Invitation mode, prepare:
  - x3dh keys from `CR.generateRcvE2EParams`, PQ support from `CR.initialPQEncryption True pqInitKeys`
  - `connReq = CRInvitationUri crData e2eRcvParams`, queue mode `QMMessaging`
  - the returned link is `CCLink connReq Nothing`; the key is `plpLinkKey`
  - `useDR` is ignored
- Invitation mode, create:
  - `plpInvitationKeys` are stored with `createRatchetX3dhKeys`
  - link data: `SL.encodeSignUserData SCMInvitation`, encrypted with `SL.invShortLinkKdf plpLinkKey`
  - queue request: `CQRMessaging (Just CQRData {linkKey, privSigKey, srvReq = (sndId, srvData)})`
  - the returned link: from `connReqWithShortLink`, moved from `newRcvConnSrv` to top level — `CSLInvitation` with the link id from the server, PQ keys removed from the full link for `IKPQOn`
- Tests: a connection via an invitation made by prepare and create; its link data is read by the joining party; `plpLinkKey` equals the key in the returned `CSLInvitation`. The six existing test calls are updated for the mode and the pair.

**Existing connections.** The link of a connection is returned without a network call:

```haskell
prepareConnShortLink :: AgentClient -> ConnId -> Maybe CRClientData -> Maybe CR.InitialKeys -> AE (ConnShortLink 'CMContact)
```

- A stored link is returned as is.
- Otherwise the address ratchet keys are chosen from `InitialKeys` (`addressRatchetKeys`), and the credentials are created and stored (`newContactLinkCreds`): the signing key pair is generated, the fixed data is built, signed and encrypted (`SL.encryptFixedData`), and `ShortLinkCreds` are stored.
- `setConnShortLink` uses the same two functions for a connection without stored credentials, and then encrypts and uploads the user data with one `LSET`.
- Tests: for an address without a short link, the link is the same from two `prepareConnShortLink` calls and from `setConnShortLink`; a requester connects via it.

**Invitation code.** Exported:

```haskell
invitationRequestCode :: ConnectionRequestUri 'CMInvitation -> ByteString
```

- `requestCode` (`Agent.hs:1423-1424`) of the invitation's X448 keys and the sender id of its first queue.
- The PQ key is removed from `requestCode`: `sha256 (smpEncode (k1, k2, sndId))`, for requests and invitations alike, so the full link of an invitation created with `IKPQOn`, which is stored without the PQ key, gives the same code as its link data.
- Tests: the code of a prepared invitation equals the code of the invitation read from its link data and the code of its full link.

### 14.3 Chat

Todo, in implementation order. At every send, a `Nothing` from a header helper is replaced by `unboundPresHeader`.

**1. Headers** — `Badges.hs`, `Protocol.hs`

- `PHRequest ByteString` in `ProofPresHeader`, tag `'R'`.
- `CBLink` in `ChatBinding`, tag `'L'`; payload: the link key bytes.
- `acceptedProof :: Maybe ProofPresHeader -> BadgeProof -> Bool`: true for `PHTest`, and for a header equal to `strEncode` of the expected one. `unboundProof` and `boundProof` are replaced by it.
- `ToField` and `FromField` for `ProofPresHeader`: the `strEncode` bytes as a blob.

**2. Ignored proofs** — `Types.hs`, store modules

- A proof rejected by `acceptedProof` is ignored.
- At creation no badge is stored: `createContact_`, `createContactRequest` (`ContactRequest.hs:170`), `createJoiningMember`, `createNewMemberProfile_`.
- At update the stored badge is kept: the badge to store and its verification are returned by `profileBadgeVerified` — for a rejected proof, the stored proof and its stored verification. Used in `updateContactProfile`, `updateMemberProfile`, `updateContactMemberProfile`, and the request update (`ContactRequest.hs:237`).
- The predicate is removed from `verifyBadge_`. `BSFailed` is stored only for a failed BBS verification.

**3. Expected header in the store**

The expected header, `Maybe ProofPresHeader`, is a parameter of:

- `updateContactProfile`
- `createDirectContact`
- `createPreparedContact`
- `createOrUpdateContactRequest`
- `updateMemberProfile`
- `updateContactMemberProfile`
- `createJoiningMember`
- `createNewGroupMember`
- `createIntroReMember`
- `updateUnknownMemberAnnounced`
- `updateRosterMemberAnnounced`
- `updatePreparedChannelMember`
- `setRelayLinkAccepted`
- `updateRelayMemberData`

`Nothing` for `createContact`, the preset contact card. The same parameter in `processMemberProfileUpdate`. In group callers the bindings computed today are wrapped with `PHChat <$>`.

**4. Presenting** — `Internal.hs`

```haskell
presentUserBadge :: User -> Maybe i -> Maybe ProofPresHeader -> Profile -> CM Profile
```

With `Nothing`, no badge is presented. Header helpers:

- `groupPresHeader :: GroupInfo -> Maybe ProofPresHeader` — `PHChat <$> sndGroupChatBinding gInfo False`
- `directPresHeader :: ContactRequestBinding -> ProofPresHeader` — `PHChat (encodeChatBinding CBDirect codeAD)` for `CRBRatchet`, `PHRequest code` for `CRBRequest`
- `linkPresHeader :: LinkKey -> ProofPresHeader` — `PHChat (encodeChatBinding CBLink key)`
- a one-time invitation's header: `PHRequest (invitationRequestCode connReq)`
- `unboundPresHeader :: CM ProofPresHeader` — `PHTest` of 16 random bytes
- `connPresHeader :: Connection -> CM (Maybe ProofPresHeader)` — `directPresHeader . CRBRatchet` of `getConnectionVerifyCodes`; `Nothing` on error
- `connsPresHeaders :: [Connection] -> CM (Map ConnId ProofPresHeader)` — the same from `getConnectionsVerifyCodes`

**5. The binding from prepare steps**

- `prepareContact :: User -> ConnReqContact -> PQSupport -> CM (ConnId, VersionChat, ContactRequestBinding)`
- `prepareAgentJoin :: User -> Maybe Connection -> Bool -> ConnectionRequestUri c -> CM ((CommandId, ConnId), Maybe ContactRequestBinding)` — `Nothing` for an existing connection
- `prepareAgentAccept :: User -> Bool -> InvitationId -> PQSupport -> CM ((CommandId, ConnId), ContactRequestBinding)`

**6. Stored request header**

Migration `M20260924_conn_pres_header`, SQLite and Postgres:

```sql
ALTER TABLE connections ADD COLUMN pres_header BLOB;
```

`BYTEA` on Postgres. Both schema dumps and `chat_query_plans.txt` are updated.

- `createConnReqConnection`: the request header is a parameter, written to `pres_header`.
- `getConnReqContact`: the result is `(ConnReqContact, Maybe ProofPresHeader)`, read from `via_contact_uri` and `pres_header`.

**7. Direct sends**

- `joinContact` (`Commands.hs:3973`): the header, `Maybe ProofPresHeader`, is a parameter, set in `connect'`, `joinPreparedConn'` and `connectContactViaAddress`:
  - a relay group: `groupPresHeader`
  - every other join: `directPresHeader` of the `prepareContact` binding for a new connection, the stored header for a retry
- The branch on `gInfo_` for the badge (`Commands.hs:3976-3982`) is removed; the profile is still chosen by `gInfo_`.
- `connectViaInvitation` (`Commands.hs:3819-3832`) and `connectMemberContact` (`:3398-3413`): the prepare binding for a new connection; `connPresHeader` for a prepared one.
- `joinMemberContactAsync` (`Subscriber.hs:3931`): the header is a parameter, set in `xGrpDirectInv` to `directPresHeader` of the `prepareAgentJoin` binding.
- `acceptContactRequest` (`Internal.hs:970-1000`): the prepare binding for a new connection; `connPresHeader` for an existing one.
- `acceptContactRequestAsync` (`Internal.hs:1002-1021`): the profile is built after `prepareAgentAccept`, from its binding.
- `CONF` replies (`Subscriber.hs:505` direct case, `:624`) and `updateContactPrefs` (`Commands.hs:4099`): `connPresHeader`.
- `sendUpdateToContacts` (`Commands.hs:4033-4071`) and `presentUserBadgeToContacts` (`:5200-5216`): one `connsPresHeaders` call per command.

**8. Direct receipts**

- `REQ` (`Subscriber.hs:1396`): `directPresHeader` of the `REQ` binding is a parameter of `profileContactRequest`, passed to `createOrUpdateContactRequest` and to `acceptGroupJoinRequestAsync`.
- `processContactProfileUpdate` (`Subscriber.hs:2758`): `connPresHeader` of the contact's connection, read in the update branch.
- `saveConnInfo` (`Subscriber.hs:3173`): `connPresHeader` of the connection, for `createDirectContact`.

**9. Groups**

- `groupPresHeader` at every send into a group: `Commands.hs:4319`; `Subscriber.hs:505` group case, `:637`, `:826`, `:843`, `:970`, `:1252`, `:3356`; `Internal.hs:2656`.
- `acceptGroupJoinRequestAsync`: the expected header is a parameter, in place of `binding_` (`Internal.hs:1044`) — `PHChat <$> memberChatBinding gInfo joiningMemberId Nothing` in `memberJoinRequestViaRelay`. `Nothing` in `acceptGroupJoinSendRejectAsync`.
- Host `INFO` with `XInfo` (`Subscriber.hs:859-864`): after `storeMemberKey`, the profile is stored by `processMemberProfileUpdate` with `PHChat <$> signedMemberBinding`, under the member's key.
- The profile is also stored by `processMemberProfileUpdate` when the new proof is accepted and its header differs from the stored proof's header.
- Introductions:
  - In `memberInfo` (`Internal.hs:1330`) the stored proof is included when `acceptedProof (PHChat <$> memberChatBinding g memberId memberPubKey)` holds, and omitted otherwise.
  - In `xGrpMemNew`, `xGrpMemIntro` and `xGrpMemFwd` (`Subscriber.hs:3197, 3279, 3341`): `PHChat <$> memberChatBinding gInfo memId key`, with `key` from the `MemberInfo`.
- Invitation via contact:
  - `profile :: Maybe Profile` in `XGrpAcpt`, the optional JSON field `profile`.
  - The invitee (`Commands.hs:2845`, `Subscriber.hs:2697`) includes its group profile, with `groupPresHeader`, when the maximum of the contact connection's `peerChatVRange` is at least `relayWebCapVersion`; the message is encoded with `encodeSignedConnInfo` when a signing is returned by `groupMsgSigning`.
  - `XGrpAcpt` with a badge in its profile is signed by `groupMsgSigning` (`Internal.hs:2316-2319`).
  - The host (`Subscriber.hs:786`) stores the key, then the profile with `PHChat <$> signedMemberBinding`.
  - The host replies with `XGrpMemInfo` and its group profile in place of `XOk` (`Subscriber.hs:791`); the badge is presented when the invitee's version is at least `relayWebCapVersion`, as at `:840-846`.

**10. Link data**

- Invitation links (`APIAddContact`, `recreateConn`): the chat generates the root key pair and calls `prepareConnectionLink SCMInvitation`; the badge is presented with the header of the prepared invitation; then `createConnectionForLink` is called.
- Invitation updates (`updatePCCShortLinkData`, `APISetConnectionIncognito`): the header of the stored invitation.
- Address (`APICreateMyAddress`): `linkPresHeader` of the key in the prepared `CSLContact`.
- Address updates (`setMyAddressData`): the key of the stored short link. For an address without a short link, the key is taken from `prepareConnShortLink`, then `setConnShortLink` is called once.
- Receipts:
  - `linkDataBadge` (`Internal.hs:2251`): the expected header is a parameter — the header of the invitation read with the link data for an invitation (`Commands.hs:4403-4404`), `linkPresHeader` of the link key for an address (`:4457-4458`).
  - `createPreparedContact` (`Commands.hs:2203`): the same headers, from `accLink`; `Nothing` for an address without a short link.
  - `updateContactFromLinkData` (`Internal.hs:1637`): the header of the address link is a parameter.
- Shared address card:
  - `APIShareMyAddress` (`Commands.hs:1243-1255`): the badge is presented with `linkPresHeader` of `connLink`.
  - Receipts (`Subscriber.hs:1910-1916`, `:2217-2221`): the proof in `MCLContact.profile` is kept when accepted under `linkPresHeader` of `connLink` and verified, and removed otherwise.
  - The badge from `profile.badge` is shown in `CIChatLinkHeader`, on iOS and in the multiplatform app.

**11. Tests** — `ChatTests/Profiles.hs`

- Direct: the badge is shown after a request to an address with and without ratchet keys, after accepting, after joining a one-time link, after a retried join, and after a profile update.
- A proof bound to another chat is ignored, and the stored badge is kept.
- P2p group: the joiner's badge is shown at the host at the request and after `INFO`; an introduced member's badge is shown at introduction; the badge is shown on both sides of an invitation via contact.
- Link data: the badge is shown from an invitation link, an address, an address that gets its first short link, and a shared address card.

`PHTest` is still sent by released clients and, through `unboundPresHeader`, on a retry of a connection prepared before this change.

## Out of scope

- Requiring an expiration in the description proof, once servers grant one.
