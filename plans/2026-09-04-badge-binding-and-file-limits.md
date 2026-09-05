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

Two new columns on `files`, a new table `rcv_badge_proofs` holding proofs, and two columns on `rcv_files` referencing the invitation proof and the description proof of a received file, kept for history. One new function in simplexmq, the hash of the fields shared by all descriptions of one upload.

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

**Default limit.** `maxFileSize`, 1GB. A supporter badge raises it to 2GB, a legend badge to 5GB (`maxXFTPFileSize`, `Badges.hs:201`).

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
- `PHFileInv` holds the chat binding, the file name and the file size from the invitation.
- `PHFileDescr` holds the same three values, then the shared description hash (section 8) and the file expiration.

One constructor serves every chat type, because the chat binding already encodes the type of chat in its first byte. Each constructor gets a tag character in `ProofPresHeaderTag` and an encoding in the `StrEncoding` instance, in the same style as `PHTest`. The file expiration is optional, because a server may grant none; it is encoded as `strEncode` of the time, or one fixed byte when absent. The badge's own expiry is a time and is encoded with `strEncode` in the disclosed messages (`badgeInfoMessages`, `Badges.hs:296`).

`verifyBadgeWith` today verifies a proof with whatever header the proof contains. After this change the receiver first checks that the header names the sender as the receiver knows them, and only then runs BBS verification with that header. `proofPresHeaderAccepted` is removed. What the receiver knows is already held by existing types, so no new type is added:

- A contact request, link data, and the profile in a direct chat: the header must be `PHTest`.
- A file in a direct chat: the receiver has the contact's connection and obtains its ratchet hash from the agent, as `newContentMessage` does for a contact card (`Subscriber.hs:1883`). The binding in the header must equal `encodeChatBinding CBDirect adHash`.
- A profile or a file in a group: the receiver has the `GroupInfo` and the sender's `GroupMember`. The binding in the header must equal `groupBindingData` for that member — for a channel the group's public id and the member id; for a p2p group the member id and a key that passes the key check.

**The key check.** A p2p binding contains the sender's member key. The receiver may know that member's key from the introduction or from a signed message, or may not know it yet. If the receiver knows a key and it differs from the key in the header, the proof fails. Otherwise the key in the header is used for this verification and never stored; keys are stored only by the introduction and by `storeMemberKey`.

The file headers are checked the same way and then further: `PHFileInv` must also name the file's name and size as received; `PHFileDescr` must also hold the hash of the received description and the expiration received with it.

`PHUnknown` fails every check. A proof from a released client, which presents `PHTest` in groups, fails in groups; no badge has been issued yet, so nothing in use is affected. A released client that receives one of the new headers verifies it, because its `proofPresHeaderAccepted` admits unknown tags and BBS verification runs with the header bytes as sent. No protocol version change is needed.

`groupBindingData` moves from `Internal.hs` to `Protocol.hs`, beside `encodeChatBinding`, because the store modules import `Protocol` and not `Internal`. Module order fixes where the check is computed: `Badges.hs` is imported by `Types.hs`, which `Protocol.hs` imports, so the header check in `Badges.hs` takes plain values — the expected binding for a channel or a direct chat, or the member id and the stored key for a p2p group — and the callers in the store compute them from `GroupInfo` and `GroupMember` with `groupBindingData`. `profileBadgeVerified` is in `Types.hs` today and cannot call `groupBindingData`; it moves to `Store/Shared.hs`, beside the other badge-verifying store code.

`SimplexDomainProof` (`Names.hs:37`) also uses `ProofPresHeader`, as an opaque value. Its verification is unchanged.

## 2. Presenting the profile badge

File: `src/Simplex/Chat/Library/Internal.hs`, `presentUserBadge` (`:2178`).

The function generates the proof for an outgoing profile. It takes a new argument, `Maybe GroupInfo`. With `Nothing` it generates `PHTest` as today. With `Just gInfo` it generates `PHChat` from the group's chat binding and the user's own member key in that group, calling `createUserMemberKey` first when the group has no key yet.

Call sites that send a profile into a group pass the group: `Commands.hs:3953` (join via group link, the group case), `:4291` (the owner's profile to a relay); `Subscriber.hs:480` (the group case), `:611`, `:799`, `:813`, `:941`, `:1220`, `:3271`; `Internal.hs:2539` (`sendGroupProfileUpdate`). All other call sites send a direct profile and pass `Nothing`.

The profile in a direct chat keeps `PHTest`; moving it to `PHChat` is a later change.

## 3. Accepting the profile badge

A received badge is verified today at seven places in the store layer, each verifying the proof with no knowledge of the sender: `profileBadgeVerified` (`Types.hs:834`), `createContact_` (`Store/Shared.hs:420`), `createJoiningMember` (`Store/Groups.hs:2089`), `createNewMemberProfile_` (`Store/Groups.hs:2459`), two contact request sites (`Store/ContactRequest.hs:169, 236`), and `linkDataBadge` (`Internal.hs:2194`).

The direct sites keep verifying with `PHTest`. The group sites gain the `GroupInfo` and the sender's `GroupMember` where they do not have them already: `updateMemberProfile` and `updateContactMemberProfile` (`Store/Groups.hs:3430, 3453`) have the member and gain the group, and pass both to `profileBadgeVerified`; `createNewMemberProfile_` gains both from `createNewGroupMember`. A badge from a message that was not verified with the member's key is not verified at all: the caller removes it from the profile before storing, so the store function sees no badge.

Where a badge is accepted in a p2p group:

- `xInfoMember` (`Subscriber.hs:2738`): only when the `XInfo` was verified with the member's key. `RcvMessage.msgSigned` is `MSSVerified` when a stored key verified it. When the same message delivers the key, `storeMemberKey` has verified the signature with that key, and the badge is kept on the same basis. Otherwise the badge is removed from the profile before `processMemberProfileUpdate`.
- `xGrpLinkMem` (`:2744`): the host's profile to the joiner, signed on this branch.
- The member connection handshake, section 4.

Where a badge is dropped in a p2p group: `createJoiningMember` and `createNewMemberProfile_`. The profile is stored without the badge, and the member's badge arrives at the handshake.

In a channel a member's profile arrives in three ways: in `XMember` when the member joins, which the member signs and the owner verifies with the roster key (`verifyKey`, `Subscriber.hs:1656`) before `createJoiningMember`; in the introduction from a relay, stored by `createNewMemberProfile_`; and in `XInfo`. A badge in any of them is kept and verified, because member keys in a channel are established by the roster, which the owner signs, and `xGrpMemNew` rejects a relay that asserts a different key (`Subscriber.hs:3127-3134`).

## 4. The member connection handshake

When two p2p members connect, each sends `XGrpMemInfo` with its group profile. It is sent from two places: the reply on the member connection (`Subscriber.hs:816`) and the join of the member connection and of the direct connection to the same member (`:3272`, both joined with the same message at `:3282-3283`). The four receiving sites — `:590, 620` on the direct connection, `:810, 823` on the member connection — each have a "TODO update member profile" comment.

- **Sign the join side.** `xGrpMemFwd` sends `encodeConnInfo $ XGrpMemInfo ...` (`:3272`), plain JSON. Change it to `encodeSignedConnInfo` with `groupMsgSigning` when the agreed version is at least `relayWebCapVersion`. The agreed version is computed three lines below, as `chatV`; move that computation above the send. Call `createUserMemberKey` before signing, here and at the reply site, as every other signing site does.
- **Parse the signature on CONF.** The member CONF site parses with `parseChatMessage` (`:745`), which discards the signature. Change it to `parseChatMessage'`, as INFO already does (`:823`).
- **Verify the signature.** At `:810` and `:823` verify the signed `XGrpMemInfo` with the member's stored key. `XGrpMemInfo` names no key, and the handshake follows the introduction, which stored the key. A member with no stored key is not verified.
- **Store the profile.** At `:810` and `:823` call `processMemberProfileUpdate` with the profile, with the badge removed when the signature did not verify.
- `:590` and `:620` stay as they are. The profile there is the same group profile, received over the direct connection to the member. The contact for a member shares the member's profile row (`createIntroToMemberContact`, `Store/Groups.hs:2684-2685`), so storing it once, on the member connection, updates both.

## 5. The file size limit at send

`checkSndFile` (`Commands.hs:3973`) compares the file size with the sender's limit and is called from the two content send paths only, with `Nothing` for an incognito send (`:4773`, `:4858`). `APIUploadStandaloneFile` (`:3628`) checks the hard limit and never the badge.

The comparison stays where it is, with one change: the limit at send counts a badge as active until one day after its expiry, instead of the seven days `maxXFTPFileSize` allows a receiver. A new function in `Badges.hs` computes the send limit with that rule, and the apps use the same rule for the limit they show on the compose screen (section 11), so the compose screen never offers a size the send refuses.

Standalone uploads do not apply badge limits. `APIUploadStandaloneFile` keeps the hard limit.

## 6. The file invitation proof

**Type.** `FileInvitation` (`Types.hs:1555`) gains `fileBadge :: Maybe BadgeProof`. The JSON instance omits absent fields, so a released client ignores it.

**Generation.** In `xftpSndFileTransfer_` (`Internal.hs:438`), when the file is above the default limit and the send is not incognito, generate a proof with `PHFileInv` from the chat binding — `CBDirect` with the ratchet hash for a contact, the group binding for a group — the file name and the file size, and set it in the invitation. The function gains the binding as an argument; the group send path computes it from the group, and the direct send path obtains the ratchet hash of the contact's connection from the agent, as `shareChatBinding` does (`Commands.hs:4685`).

**Verification.** A file invitation arrives at three places: `processFileInvitation` (`Subscriber.hs:1957`), for a file in a content message in a direct chat or a group, called with a closure that creates the transfer; `processGroupFileInvitation'` (`:2437`), for the older `XFile` event in a group; and `processFileInvitation'` (`:2422`) for `XFile` in a direct chat. All call `validateFileInvitation`, which replaces the file name with a name valid on the local file system. The proof is checked before that, against the name as received, and against the sender: the connection's ratchet hash for a contact, the group and member for a member. The result is the decision below, passed to `createRcvFileTransfer` or `createRcvGroupFileTransfer`, which gain it as an argument and write it.

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

On a received file the proof is stored as received in `rcv_badge_proofs` (section 12), referenced from `rcv_files.badge_inv_proof_id`, for history (section 9). Nothing is stored for a sent file; the sender regenerates proofs from its credential.

**The chat item.** `CIFile` (`Messages.hs:684`) gains `fileProhibited :: Maybe FileProhibited`. `MaybeCIFIleRow` (`Store/Messages.hs:2279`) gains the column, the three queries that select the file columns gain `f.file_max_size, f.file_badge_status`, and the two `maybeCIFile` constructors and the five other `CIFile` constructions (`Internal.hs:449`, `Subscriber.hs:2001, 2456, 2472`, `Commands.hs:5009`) set it.

**Accepting a file.** `acceptFileReceive` (`Internal.hs:746`) fails with `CEFileSize` when the file is prohibited. The apps stop a tap before that, from the same field.

## 7. The file description proof

**Type.** `XMsgFileDescr` (`Protocol.hs:454`) gains `fileBadge :: Maybe BadgeProof`, encoded with `.=?` like `fileExpires`.

**Generation.** The description proof is generated when the upload completes, which may be hours after the send, from the credential as it is then. If the badge expired meanwhile, the proof still verifies at receivers, which allow seven days past expiry; if the user hid the badge, the credential row is still there; if the badge was renewed, the new credential is used. Only a credential deleted outright leaves the file without a description proof, and receivers then prohibit it. Nothing is copied at send.

In the `SFDONE` handler (`Subscriber.hs:209`), when the file is above the default limit, the user holds a credential, and the send was not incognito — the handler has the chat item and its contact or group, so the same condition as at the invitation — generate one proof with `PHFileDescr`: the values of the invitation header, `sharedDescriptionHash` of any one recipient description, and `fileExpires`. `sendFileDescriptions` (`:283`) sets it on the last part for each recipient, in the direct branch (`:237`) and the group branch (`:252`) alike; the parts are split in `splitText` (`:297`).

**Verification.** When the file is allowed and above the default limit:

1. The part that completes the description must have a proof. `processFDMessage` (`Subscriber.hs:1940`) receives every part and calls `receiveViaCompleteFD` when the description is complete and the file was accepted. It verifies the proof on the completing part, before that call, and on success stores it in `rcv_badge_proofs`, referenced from `rcv_files.badge_descr_proof_id`. On failure it sets the chat item file status to `CIFSRcvError` and cancels the transfer, as the digest mismatch does (`resetRcvCIFileStatus`, `:360`). A later accept of a cancelled transfer fails with `CEFileCancelled`.
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

For a file received from another member, `invCompleteDescr` sets `fileBadge` from the proof `rcv_files.badge_inv_proof_id` references, and the last description part gets the proof `rcv_files.badge_descr_proof_id` references. Both are bound to the original sender, and history names the original sender (`fwdSender`, `:1466`), so the new member verifies them against that member's binding. For the host's own files both proofs are generated afresh from the credential, with the same headers.

`fileExpired` (`:1439-1443`) decides which files history re-sends by the item's age against `rcvFilesTTL`, two days, and ignores the granted expiration stored with the file. That check should use the stored `fileExpires`; it is noted here because it bounds when the stored proofs are read. The invitation proof names the file name as sent; `validateFileInvitation` may have changed the stored name on a platform where it was not valid, in which case the re-sent invitation fails the name check for that file.

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
- The generated API mirrors — `bots/api/TYPES.md`, `types.ts`, `_types.py` — are regenerated for `CIFile`, `FileError` and the new chat error.

## 12. Schema and fixtures

Migration `M20260904_file_badges`, SQLite and Postgres:

```sql
ALTER TABLE files ADD COLUMN file_max_size INTEGER;
ALTER TABLE files ADD COLUMN file_badge_status TEXT;

CREATE TABLE rcv_badge_proofs(
  badge_proof_id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER NOT NULL REFERENCES files ON DELETE CASCADE,
  badge_proof BLOB NOT NULL,
  badge_pres_header BLOB NOT NULL,
  badge_key_idx INTEGER NOT NULL,
  badge_type TEXT NOT NULL,
  badge_expiry TEXT,
  badge_extra TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

ALTER TABLE rcv_files ADD COLUMN badge_inv_proof_id INTEGER REFERENCES rcv_badge_proofs ON DELETE SET NULL;
ALTER TABLE rcv_files ADD COLUMN badge_descr_proof_id INTEGER REFERENCES rcv_badge_proofs ON DELETE SET NULL;
```

The six proof columns are the fields of `BadgeProof` — the proof, the presentation header, the issuer key index, and the disclosed type, expiry and extra — with a conversion of its own. The two references follow the pattern of `rcv_files.file_descr_id`, which references `xftp_file_descriptions` the same way (`chat_schema.sql:328`). File rows are removed by cascade from chat items, contacts and groups rather than by one function, and description rows referenced this way are left behind today. The proof row therefore also references the file with `ON DELETE CASCADE`, so it is removed with the file; the two columns on `rcv_files` say which proof is which. Postgres uses `BYTEA`, `BIGINT` and `GENERATED ALWAYS AS IDENTITY`. Register in both `Migrations.hs` lists and in `simplex-chat.cabal`. Update both `chat_schema.sql` files and `chat_query_plans.txt`; `SchemaDump.hs` compares them.

## 13. Tests

- `BadgeTests.hs`: each header encodes and decodes; a proof generated with one header fails with another; the key check accepts an unknown key, accepts an equal key, rejects a different one.
- `ChatTests/Profiles.hs`, beside the seven badge tests: a badge in a p2p group appears at the other member after the connection handshake and not before; a proof presented under another member's binding is rejected; a badge in a channel appears on presentation. Existing tests that assert a badge at introduction time are updated.
- `ChatTests/Files.hs`, beside `testXFTPGroupFileTransfer`: a file above the default limit from a badge holder is received in a group and in a direct chat; an invitation whose proof was made for another member is refused; a description with a changed hash fails before download; a file above the limit received as history is received by the new member; a forward into an incognito membership above the default limit fails the command before any upload.
- `ProtocolTests.hs`: the new fields in `FileInvitation` and `XMsgFileDescr`.

## Out of scope

- Moving the direct chat profile proof to `PHChat`.
- Requiring an expiration in the description proof, once servers grant one.
