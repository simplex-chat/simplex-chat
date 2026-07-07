# Signed file integrity + signed history preservation

Extends [channel message signing](2026-06-04-channel-message-signing.md). Part A must land **before** Part B, because forwarding a signed file post is only meaningful once the signature actually binds the file bytes. Related: [roster catch-up subscribers](2026-06-22-roster-catchup-subscribers.md) (verification depends on the recipient holding the author's key).

## Part A — Sign the file digest (bug, fix first)

### Problem

A "signed" XFTP file message signs nothing about the file itself. The signature covers `XMsgNew`, whose `FileInvitation` is built with `fileDigest = Nothing` (`Types.hs:1524`, `xftpFileInvitation`) and an empty embedded description (`dummyFileDescr`, `Internal.hs:389,410`) — because at send time the file is still uploading async. The real digest/key/servers live only in the later, **unsigned** `XMsgFileDescr` events. So the signature attests only `fileName + fileSize`. A malicious relay can pair the genuine signed `XMsgNew` with a substituted description pointing at different content of the same size, and the recipient displays it as "signed & verified". The file signature is currently meaningless. This affects **live** signed messages, not only history.

### Fix

Sign a **plaintext** content digest in `FileInvitation.fileDigest` (already a field, inside the signed `XMsgNew`), and verify the decrypted file against it on receive.

- Sender: populate `fileDigest` with the sha512 of the unencrypted file content for XFTP sends, so it is covered by the message signature.
- Receiver: after the agent completes the download and decrypt, compare the plaintext digest to `fileDigest`; on mismatch, drop the file and surface a violation event — **regardless of whether the message is signed** (a mismatch is corruption/tampering either way). When the message is signed the digest is unforgeable, so this is a real content guarantee; when unsigned it is corruption detection only.
- Populate `fileDigest` for all XFTP sends (so unsigned messages also get the drop-on-mismatch check); the signature is what upgrades it from corruption-detection to a real guarantee.
- Compatibility: `fileDigest` is an optional field older clients already ignore — forward-compatible; verification runs only on clients that support it.

### Why plaintext, not the existing description digest (verified in simplexmq)

The XFTP description's `digest` is `sha512Hash` of the **encrypted** file (`Agent.hs:449`, `Client/Main.hs:290`), produced with a per-send random `key`+`nonce`; the recipient verifies the reassembled **ciphertext** against it (`Agent.hs:295`). It is therefore upload-specific — any re-encryption/re-upload changes it, so it can never be signed once and preserved across forwards or re-uploads. A **plaintext** digest is encryption-independent (stable across re-forward and re-upload) and verifies the content the recipient actually consumes.

Cost is small: the send-side encrypt pass already streams the whole plaintext (`encryptFile`, `Crypto.hs:36`), so it can emit a running sha512 of the source in the same read; the decrypt pass can do likewise. This is a **coordinated simplexmq + simplex-chat change** (use-local-simplexmq): simplexmq computes and surfaces the plaintext digest on send (to sign) and receive (to verify); simplex-chat places it in the signed `FileInvitation.fileDigest` and checks it after decrypt.

### Threat model

Author honest, relay/forwarder malicious. The relay controls the unsigned description but not the signed invitation. Signing the content digest binds the file end-to-end from author to recipient, independent of any relay.

## Part B — Preserve signatures in history

### Problem

`sendHistory` → `processContentItem` (`Internal.hs:1370`) re-encodes each item's current content via `prepareGroupMsg` and sends it unsigned (`groupMsgSigning False`; the relay has no author key). Catch-up members therefore hold **all** history unsigned, so §7 enforcement (`requireVerifiedEdit`/`requireVerifiedDelete`) never protects their items — and can't heal later, because `updateGroupChatItem_` (`Messages.hs:2766`) does not write `msg_signed`, so a subsequent signed edit does not upgrade the item. Only delivering history signed at creation closes this.

### Design

- **Storage.** Two nullable columns on `chat_items`: `item_msg_body`, `item_signatures`. Written by relays only, for content items only (same `msg_content_tag` / `include_in_history` filter history uses). `chat_binding` is not stored — it is derived as `smpEncode(publicGroupId, authorMemberId)` at send time.
- **Capture.** Write the columns in `createNewChatItem_` when the item is signed; overwrite them in `updateGroupChatItem_` when a signed edit is applied. This keeps the stored bytes tracking the **latest** signed event, so history always forwards current content. Thread the raw `(msg_body, signatures)` onto `RcvMessage` (from `saveGroupFwdRcvMsg`'s `verifiedMsgParts`) and use `SndMessage`'s existing `signedMsg_`/`msgBody`; today both carry only the `msgSigned` status.
- **Forward.** In `sendHistory`, for a signed content item with stored bytes, forward the original bytes as a `VMSigned` `XGrpMsgForward` (reuse the live path's `encodeFwdElement` / `sendFwdMemberMessage`) instead of re-encoding; unsigned items keep the current re-encode path.
- **Edits need only the last event.** Forwarding the latest signed event — `XMsgNew` if never edited, else the latest `XMsgUpdate` — is sufficient: on the recipient a forwarded `XMsgUpdate` for a not-yet-existing item hits the create fallback in `groupMessageUpdate` (`Subscriber.hs:2234` `catchCINotFound` → `saveRcvChatItem'` → `createNewRcvChatItem`), which creates the item with the edit's `msgSigned` and content, marked edited. So one `(body, signatures)` per item, no original+edit replay. Self-consistent because a verified item only ever accepts verified edits (`requireVerifiedEdit`).
- **Files.** No special branch. The forwarded signed `XMsgNew` carries `name + size + digest` (from Part A); the description follows via the existing `XMsgFileDescr` path and may be re-forwarded/re-uploaded freely — it is not covered by the signature, and integrity now comes from the signed digest.
- **Compatibility.** Same wire format as live signed messages. Signing is tied to relay channels (`groupMsgSigning` gates on `useRelays'`, not a member version), so any relay subscriber already receives signed live messages; signed history is identical. No new version gate.

### Result

Catch-up members hold non-edited and edited signed posts as verified with current content, so §7 enforcement protects their edits/deletes — closing the residual documented in the signing plan. Retention is no longer bounded by the 30-day `messages` pruning.

## Implementation steps

Part A:
1. simplexmq: compute and surface the plaintext (source-content) sha512 from the existing encrypt and decrypt passes (`encryptFile` / `decryptChunks`), through the agent's send result and receive completion.
2. simplex-chat: populate `FileInvitation.fileDigest` from that digest on XFTP send (so it is signed); on receive completion, compare the plaintext digest to `fileDigest` and, on mismatch, drop the file and surface a violation event — always, not only when signed.
3. Tests: signed file message verifies; substituted/tampered content fails and the file is dropped; an unsigned mismatch is dropped too.

Part B:
4. Migration: add `item_msg_body`, `item_signatures` to `chat_items` (SQLite + Postgres modules; register in `Migrations.hs`; add to `.cabal`; schema files regenerate via tests).
5. Thread raw signed bytes onto `RcvMessage`; store/overwrite in `createNewChatItem_` and `updateGroupChatItem_` (relay + content-item + signed).
6. `sendHistory`: signed content item with stored bytes → forward `VMSigned`; else re-encode.
7. Tests: catch-up subscriber holds non-edited and edited signed posts as verified/current; a forged unsigned edit/delete of a catch-up item is rejected; a signed file post verifies on catch-up.

## Docs to update on implementation

Signing/files/history spec + product docs; move the digest gap from `product/gaps.md` to fixed; cross-link this plan from [channel message signing](2026-06-04-channel-message-signing.md).
