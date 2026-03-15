# Plan: Signed Message Storage, Forwarding, and Verification

## Context

The protocol types for signatures exist (`MsgSignatures`, `MsgSigData`, `ChatBinding`), the parser handles `/`/`>`/`{` element prefixes, and `verifySig` checks signatures. What's missing:

1. **Signing when sending** — members sign their messages before sending to the relay
2. **Signature storage** — persisting signatures alongside message content
3. **Signature forwarding** — relay preserves and forwards original signatures intact
4. **Binding correctness** — bindings aren't covered by signatures or validated
5. **Required signatures** — admin events must require valid signatures in relay groups
6. **Visibility** — expose signature verification status in chat items

## Design

### A. Binding: Reconstructed, Not Sent

`CBGroup {groupRootKey, senderMemberId}` — both known to verifier from context. Replace with single-byte binding tag on wire.

Wire: `<bindingTag:1> <sigCount:1> (<keyRef><sig:64>)*`

Signed payload (constructed by signer and verifier, not on wire):
```
smpEncode 'G' <> smpEncode (groupRootKey, senderMemberId) <> jsonBody
```

The binding tag is separate from the binding-specific prefix. SMP tuple encoding is concatenation, so `smpEncode ('G', k, m) = smpEncode 'G' <> smpEncode (k, m)` — same bytes either way.

### B. Signing Context — Data, Not Function

A generic record carries key material and binding data for signing:

```haskell
data MsgSigning = MsgSigning
  { sigBindingTag :: BindingTag
  , sigPrefix :: ByteString           -- binding-specific, e.g. smpEncode (rootKey, memberId)
  , sigPrivKey :: C.PrivateKeyEd25519
  }
```

`sigBindingTag` goes into `MsgSignatures` on the wire (tells verifier which binding to reconstruct). `sigPrefix` is the binding-specific bytes. The signing function combines: `smpEncode sigBindingTag <> sigPrefix <> jsonBody`.

Group-specific constructor:
```haskell
groupMsgSigning :: GroupKeys -> GroupMember -> MsgSigning
groupMsgSigning GroupKeys {groupRootKey, memberPrivKey} GroupMember {memberId} =
  MsgSigning BTGroup (smpEncode (groupRootPubKey groupRootKey, memberId)) memberPrivKey
```

For contacts in the future — different constructor, different binding tag, same `MsgSigning` record and same `createSndMessages` path.

### C. Per-Event Signing Decision — Caller, Not Policy

The decision of whether to sign each event lives with the caller, not inside `createSndMessages`. The caller provides `Maybe MsgSigning` per event:

```haskell
createSndMessages :: (MsgEncodingI e, Traversable t)
                  => t (ConnOrGroupId, ChatMsgEvent e, Maybe MsgSigning)
                  -> CM' (t (Either ChatError SndMessage))
```

In `sendGroupMessages_`:
```haskell
let signing evt = case groupKeys gInfo of
      Just gk | requiresSignature (toCMEventTag evt) -> Just (groupMsgSigning gk (membership gInfo))
      _ -> Nothing
    idsEvts = L.map (\evt -> (GroupId groupId, evt, signing evt)) events
```

`requiresSignature` is group policy — only roster-modifying events (`XGrpDel`, `XGrpInfo`, `XGrpPrefs`, `XGrpMemDel`, `XGrpMemRole`, `XGrpMemRestrict`). Content is never signed (deniability). When contact signing is added, a different caller uses a different predicate — `createSndMessages` is mechanical.

### D. Signature Storage — Persisted for History

Signatures are persisted in `msg_sigs BLOB` column alongside `msg_body` in the same INSERT. One DB operation.

**Why persist (not ephemeral):** History delivery needs original signatures. In relay groups, history is forwarded with signatures preserved. In non-relay groups (if signing is extended), own sent signatures must survive for delivery to new members. Persisting from the start avoids losing generality.

`msg_body` remains unchanged (JSON, backward compatible). Content and authentication are orthogonal.

### E. Signing Scope — Deniability vs Authentication

Only roster-modifying messages are signed. Content messages (`XMsgNew` etc.) are NEVER signed.

1. **Deniability** — signing content creates non-repudiable proof of authorship. Anyone with the message bytes could prove who wrote it. Antithetical to SimpleX's privacy model.

2. **Threat model** — relay manipulation of content is detectable post-hoc via cross-relay consistency (multiple independent relays). Sufficient because content is not irreversible. Roster/profile changes are disruptive and irreversible (member removed, role changed, group deleted) — must be authenticated at processing time.

### F. Symmetric Encoding

```haskell
encodeMsgElement :: Maybe MsgSignatures -> ByteString -> ByteString
encodeMsgElement Nothing body = body
encodeMsgElement (Just sigs) body = "/" <> smpEncode sigs <> body
```

Dual of `elementP`'s `'/'`/`'{'` cases. Used by both send batcher (`batchMessages`) and forward batcher (`batchDeliveryTasks1`). No signing logic in any batcher — only structural encoding.

### E. Delivery Tasks: `msgBody` not `chatMessage`

`MessageDeliveryTask` carries `msgBody :: ByteString` (raw JSON from `msg_body`) + `msgSignatures_ :: Maybe MsgSignatures` — NOT `chatMessage :: ChatMessage 'Json`.

**Why `msgBody` is sufficient:**
- All delivery task processing is structural — encode, batch, send. Content decisions happen at task CREATION time (in `processEvent`), not delivery time.
- `DJRelayRemoved` currently wraps `chatMessage` in JSON `XGrpMsgForward` — but should use binary encoding instead (same `><fwd>element` format as normal batching, just single-element). Binary encoding only needs raw bytes + signatures, not parsed ChatMessage.
- More general — works for any future message type without coupling to JSON.
- Eliminates a parse+re-encode cycle (raw bytes → ChatMessage → chatMsgToBody → bytes).

### F. DJRelayRemoved: Binary Encoding

Current: wraps chatMessage in JSON `XGrpMsgForward` event. New: produces binary batch with single `><fwd>/<sigs><json>` element, same as normal forwarding. The receiver already handles binary forwarded elements through `elementP` → `xGrpMsgForward`.

### G. Verification with Binding

```haskell
verifySig gInfo GroupMember {memberPubKey = Just pk, memberId}
          (Just MsgSigData {signatures = MsgSignatures {bindingTag = BTGroup, signatures}, signedBody})
  | Just gk <- groupKeys gInfo =
      let binding = smpEncode ('G', groupRootPubKey (groupRootKey gk), memberId)
       in all (\(MsgSignature KRMember sig) -> C.verify pk sig (binding <> signedBody)) signatures
verifySig _ _ _ = True
```

### H. Signature Enforcement

**Must be signed** (reject if unsigned in relay groups with keys):
- `XGrpDel`, `XGrpInfo`, `XGrpPrefs`, `XGrpMemDel`, `XGrpMemRole`, `XGrpMemRestrict`

**Not signed** (deniability — see §E):
- `XMsgNew` and all other content events

**Conditionally signed:**
- `XGrpMemNew` — not always signed because members/subscribers can join via chat relays. Signed when owners/admins add members directly. Enforcement is context-dependent (checks sender role, not just event tag).

**Channel posts** (`FwdChannel`): validate if signed, strip before forwarding.

### I. Expose in UI

Two display paths in CLI:

**Path 1: Chat item history** (also used by mobile UI)
- `CIMeta.msgSigned :: Bool` — set during chat item creation
- Flow: `VerifiedMsg` → `isJust signedMsg_` → `RcvMessage.msgSigned` → `createNewRcvChatItem` → `createNewChatItem_` (INSERT with `msg_signed`) → SELECT reads `msg_signed` → `mkCIMeta` → View.hs
- Migration: `ALTER TABLE chat_items ADD COLUMN msg_signed` (in `chat_relays` migration)
- Note: `RcvMessage` is a goner (see pending refactor). In future, `msgSigned` flows from `VerifiedMsg` directly.

**Path 2: Immediate CLI events** (ChatEvent/ChatResponse)
- Receive events: add `Bool` to ChatEvent constructors that correspond to signed events
  - `CEvtMemberRole` — XGrpMemRole
  - `CEvtMemberBlockedForAll` — XGrpMemRestrict
  - `CEvtDeletedMemberUser` — XGrpMemDel (self)
  - `CEvtDeletedMember` — XGrpMemDel (other)
  - `CEvtGroupDeleted` — XGrpDel
  - `CEvtGroupUpdated` — XGrpInfo / XGrpPrefs
- Send responses: add `Bool` to ChatResponse constructors for send-side
  - `CRMembersRoleUser` — APIMembersRole
  - `CRMembersBlockedForAllUser` — APIBlockMembersForAll
  - `CRUserDeletedMembers` — APIRemoveMembers
  - `CRGroupDeletedUser` — APIDeleteChat (group)
  - `CRGroupUpdated` — APIUpdateGroupProfile
- Source: receive `msgSigned` from `RcvMessage`; send from `useRelays' gInfo`
- View.hs: append " (signed)" to event text when Bool is True

**Correlation: `requiresSignature` events ↔ CLI display**

| Event | Receive ChatEvent | Send ChatResponse |
|-------|-------------------|-------------------|
| XGrpDel | CEvtGroupDeleted | CRGroupDeletedUser |
| XGrpInfo | CEvtGroupUpdated | CRGroupUpdated |
| XGrpPrefs | CEvtGroupUpdated | CRGroupUpdated |
| XGrpMemDel | CEvtDeletedMember[User] | CRUserDeletedMembers |
| XGrpMemRole | CEvtMemberRole | CRMembersRoleUser |
| XGrpMemRestrict | CEvtMemberBlockedForAll | CRMembersBlockedForAllUser |

### J. Pending Refactor: Remove RcvMessage

`RcvMessage` carries redundant fields (`msgBody`, `authorMember` never read; `chatMsgEvent`, `sharedMsgId_` derivable from `verifiedMsg`). Plan:
1. Remove `RcvMessage` type
2. `NewRcvMessage` = `verifiedMsg` + `brokerTs` + `forwardedByMember` (drop `chatMsgEvent`)
3. `createNewRcvMessage` returns just `msgId`
4. Consumers extract what they need from `verifiedMsg` already in scope

## Implementation Steps

### Step 1: Foundation — Types + Encoding + Storage Schema ✅

- `ChatBinding = CBGroup` with `Encoding` instance (was `BindingTag`)
- `MsgSignatures { chatBinding :: ChatBinding, signatures :: NonEmpty MsgSignature }`
- `MsgSigning { bindingTag, bindingData, keyRef, privKey }` — generic signing context record
- `encodeBatchElement` in `Batch.hs` (moved from Protocol.hs)
- `requiresSignature :: CMEventTag e -> Bool`
- Migration: `ALTER TABLE messages ADD COLUMN msg_sigs BLOB`
- `SndMessage` gains `msgSignatures_ :: Maybe MsgSignatures`
- `createNewRcvMessage`: already accepts and stores `Maybe MsgSignatures`

### Step 2: Sign on Send + Verify with Binding ✅

- `groupMsgSigning :: GroupInfo -> ChatMsgEvent e -> Maybe MsgSigning` in Internal.hs — takes GroupInfo, decides per-event
- `createSndMessages` takes `(ConnOrGroupId, Maybe MsgSigning, ChatMsgEvent e)` triples
- `createNewSndMessage` accepts `Maybe MsgSigning`, signs inline, stores `msg_sigs` in same INSERT
- `batchMessages` encodes elements via `encodeBatchElement` (two parallel lists, encode once per message)
- `verifySig` in Subscriber.hs reconstructs binding prefix from `GroupInfo` + `memberId`, verifies with `C.verify`
- Removed dead code: `signGroupMessages`, `updateSndMsgSignatures`, `groupSignFn`, `signMsgBody`

### Step 3: Store, Forward, Verify — End-to-End

Steps 3-5 from the original plan are one flow. They must ship together because the e2e test — member A signs → relay stores → relay forwards → member B verifies — is the only meaningful test.

#### Critical invariant: original bytes must be preserved

JSON round-trip through aeson doesn't preserve key ordering. Currently `msg_body` is stored via `chatMsgToBody chatMsg` (re-encoded from parsed `ChatMessage`). These bytes may differ from what the sender signed. For signature verification after forwarding, the relay must store the **original** bytes in `msg_body`.

When `elementP` parses a signed element (`/<sigs><json>`), `A.match msgP` captures the exact JSON bytes as `signedBody` in `MsgSigData`. This is what must be stored as `msg_body` for signed messages.

For unsigned messages, `chatMsgToBody chatMsg` is fine — no signature to preserve.

#### E2E Flow

```
Member A                    Relay                       Member B
─────────                   ─────                       ────────
sign(roster event)
  ↓
/<sigs><json> ──────────→ receive + parse (elementP)
                           msgSig_ has signedBody (exact bytes)
                           verify (withVerifiedSig)
                           store signedBody as msg_body ──(a)
                           store MsgSignatures as msg_sigs
                                    ↓
                           read msg_body + msg_sigs from DB ──(b)
                           ><fwd>/<sigs><msg_body> ──────→ receive + parse
                                                           elementP: > → / → json
                                                           msgSig_ has signedBody
                                                           verify (withVerifiedSig)
                                                           store signedBody + sigs ──(c)
```

#### (a) Relay receives signed message → stores with original bytes

**Current call chain** (Subscriber.hs → Internal.hs → Store/Messages.hs):

```
processAChatMsg(line 920)     — has msgSig_ (with signedBody), chatMsg
  │ passes chatMsg only, msgSig_ not threaded
  ▼
processEvent(line 941)        — has chatMsg only
  │ body = chatMsgToBody chatMsg  ← RE-ENCODES, loses original bytes
  ▼
saveGroupRcvMsg(Internal.hs:2218)  — params: user, groupId, member, conn, msgMeta, body, chatMsg
  │                                   no signature parameter
  ▼
createNewMessageAndRcvMsgDelivery(Store/Messages.hs:262)  — no signature parameter
  │ passes Nothing for msgSignatures_
  ▼
createNewRcvMessage(Store/Messages.hs:294)  — HAS Maybe MsgSignatures param, receives Nothing
  │
  ▼
INSERT INTO messages ... msg_body=RE-ENCODED, msg_sigs=Nothing
```

**Changes (6 functions):**

1. **`processAChatMsg`** (Subscriber.hs:920→934): pass `msgSig_` to `processEvent`
   - Current: `processEvent gInfo' m' chatMsg`
   - New: `processEvent gInfo' m' chatMsg msgSig_`

2. **`processEvent`** (Subscriber.hs:941): accept `Maybe MsgSigData`, use `signedBody` when signed
   - Current sig: `GroupInfo -> GroupMember -> ChatMessage e -> CM (Maybe NewMessageDeliveryTask)`
   - New sig: `GroupInfo -> GroupMember -> ChatMessage e -> Maybe MsgSigData -> CM (Maybe NewMessageDeliveryTask)`
   - Current: `let body = chatMsgToBody chatMsg`
   - New: `let body = maybe (chatMsgToBody chatMsg) signedBody msgSig_`
   - Extract: `let sigs_ = signatures <$> msgSig_` (where `signatures :: MsgSigData -> MsgSignatures`)
   - Pass both `body` and `sigs_` to `saveGroupRcvMsg`

3. **`saveGroupRcvMsg`** (Internal.hs:2218): add `Maybe MsgSignatures` parameter
   - Current sig: `User -> GroupId -> GroupMember -> Connection -> MsgMeta -> MsgBody -> ChatMessage e -> CM (...)`
   - New sig: `User -> GroupId -> GroupMember -> Connection -> MsgMeta -> MsgBody -> ChatMessage e -> Maybe MsgSignatures -> CM (...)`
   - Pass to `createNewMessageAndRcvMsgDelivery`
   - 1 caller: Subscriber.hs:944

4. **`createNewMessageAndRcvMsgDelivery`** (Store/Messages.hs:262): add `Maybe MsgSignatures` parameter
   - Current sig: `DB.Connection -> ConnOrGroupId -> NewRcvMessage e -> Maybe SharedMsgId -> RcvMsgDelivery -> Maybe GroupMemberId -> ExceptT StoreError IO RcvMessage`
   - New: add `Maybe MsgSignatures` after `Maybe SharedMsgId`
   - Current: passes `Nothing` to `createNewRcvMessage`
   - New: passes the received `Maybe MsgSignatures`
   - 2 callers: `saveGroupRcvMsg` (Internal.hs:2226) and `saveDirectRcvMSG` (Internal.hs:2215)
   - `saveDirectRcvMSG` passes `Nothing` (direct messages not signed yet)

5. **`createNewRcvMessage`** (Store/Messages.hs:294): no change — already has `Maybe MsgSignatures` param

After change:
```
INSERT INTO messages ... msg_body=ORIGINAL_BYTES, msg_sigs=MsgSignatures
```

#### (b) Relay reads delivery tasks → forwards with preserved signatures

**Current call chain** (Store/Delivery.hs → Delivery.hs → Batch.hs):

```
getMsgDeliveryTask_(Store/Delivery.hs:130)
  │ SQL: SELECT ... msg.msg_body ...  ← no msg_sigs
  │ Row type: ... ChatMessage 'Json ... ← parsed via FromField, RE-ENCODES on read
  ▼
MessageDeliveryTask { chatMessage :: ChatMessage 'Json }  (Delivery.hs:128)
  ▼
batchDeliveryTasks1(Batch.hs:73)
  │ destructures: MessageDeliveryTask {taskId, fwdSender, brokerTs, chatMessage}
  ▼
encodeFwdElement(Batch.hs:96)  — takes GrpMsgForward -> ChatMessage 'Json -> ByteString
  │ ">" <> smpEncode fwd <> chatMsgToBody chatMessage  ← RE-ENCODES AGAIN
  ▼
Wire: ><fwd><re-encoded-json>  ← signature would fail
```

**Changes (5 functions/types):**

6. **`MessageDeliveryTask`** (Delivery.hs:128): replace `chatMessage` field
   - Current: `chatMessage :: ChatMessage 'Json`
   - New: `msgBody :: ByteString, msgSignatures_ :: Maybe MsgSignatures`
   - `chatMessage` used only in 2 places: `batchDeliveryTasks1` (Batch.hs:86) and `DJRelayRemoved` (Subscriber.hs:3375) — both just encode, no content inspection

7. **`MessageDeliveryTaskRow`** (Store/Delivery.hs:128): change column type
   - Current: `... ChatMessage 'Json, BoolInt`
   - New: `... DB.Binary, Maybe MsgSignatures, BoolInt`

8. **`getMsgDeliveryTask_`** (Store/Delivery.hs:130): add `msg.msg_sigs` to SELECT
   - Current SQL: `msg.msg_body, t.message_from_channel`
   - New SQL: `msg.msg_body, msg.msg_sigs, t.message_from_channel`
   - `toTask`: destructure `DB.Binary` as raw bytes, `Maybe MsgSignatures` from `msg_sigs`

9. **`encodeFwdElement`** (Batch.hs:96): take raw bytes + signatures
   - Current sig: `GrpMsgForward -> ChatMessage 'Json -> ByteString`
   - New sig: `GrpMsgForward -> Maybe MsgSignatures -> ByteString -> ByteString`
   - Body: `">" <> smpEncode fwd <> encodeBatchElement sigs_ msgBody`

10. **`batchDeliveryTasks1`** (Batch.hs:73): use new task fields
    - Current: `MessageDeliveryTask {taskId, fwdSender, brokerTs = fwdBrokerTs, chatMessage} = task`
    - New: `MessageDeliveryTask {taskId, fwdSender, brokerTs = fwdBrokerTs, msgBody, msgSignatures_} = task`
    - Current: `msgBody = encodeFwdElement GrpMsgForward {fwdSender, fwdBrokerTs} chatMessage`
    - New: `fwdBody = encodeFwdElement GrpMsgForward {fwdSender, fwdBrokerTs} msgSignatures_ msgBody`

After change:
```
Wire: ><fwd>/<sigs><original-json>  ← signature valid
```

#### (c) Member receives forwarded message → stores with original bytes

**Current call chain** (Subscriber.hs → Internal.hs → Store/Messages.hs):

```
xGrpMsgForward(Subscriber.hs:3159)  — has chatMsg + msgSig_ (with signedBody)
  ▼
processForwardedMsg(Subscriber.hs:3172)  — closure, has chatMsg, msgSig_ in scope but not used
  │ body = chatMsgToBody chatMsg  ← RE-ENCODES
  ▼
saveGroupFwdRcvMsg(Internal.hs:2237)  — no signature parameter
  │ passes Nothing to createNewRcvMessage
  ▼
createNewRcvMessage(Store/Messages.hs:294)  — receives Nothing
  ▼
INSERT INTO messages ... msg_body=RE-ENCODED, msg_sigs=Nothing
```

**Changes (3 functions):**

11. **`processForwardedMsg`** (Subscriber.hs:3172): use `signedBody` when signed, pass sigs
    - `msgSig_` is in scope from `xGrpMsgForward` closure
    - Current: `let body = chatMsgToBody chatMsg`
    - New: `let body = maybe (chatMsgToBody chatMsg) signedBody msgSig_`
    - Extract: `let sigs_ = signatures <$> msgSig_`
    - Pass `sigs_` to `saveGroupFwdRcvMsg`

12. **`saveGroupFwdRcvMsg`** (Internal.hs:2237): add `Maybe MsgSignatures` parameter
    - Current sig: `User -> GroupInfo -> GroupMember -> Maybe GroupMember -> MsgBody -> ChatMessage e -> UTCTime -> CM (Maybe RcvMessage)`
    - New: add `Maybe MsgSignatures` after `UTCTime`
    - Current: passes `Nothing` to `createNewRcvMessage`
    - New: passes the received `Maybe MsgSignatures`
    - 1 caller: Subscriber.hs:3175

13. **`createNewRcvMessage`**: no change — already has param

After change:
```
INSERT INTO messages ... msg_body=ORIGINAL_BYTES, msg_sigs=MsgSignatures
```

#### (d) DJRelayRemoved — binary encoding

**Current** (Subscriber.hs:3371-3382):
```haskell
let MessageDeliveryTask {senderGMId, fwdSender, brokerTs = fwdBrokerTs, chatMessage} = task
    fwdEvt = XGrpMsgForward GrpMsgForward {fwdSender, fwdBrokerTs} chatMessage  ← JSON wrapping
    cm = ChatMessage {chatVRange = vr, msgId = Nothing, chatMsgEvent = fwdEvt}
    body = chatMsgToBody cm  ← RE-ENCODES
createMsgDeliveryJob db gInfo jobScope (Just senderGMId) body
```

**Change** (1 function, same location):

14. **DJRelayRemoved handler** (Subscriber.hs:3374): use binary encoding
    ```haskell
    let MessageDeliveryTask {senderGMId, fwdSender, brokerTs = fwdBrokerTs, msgBody, msgSignatures_} = task
        fwd = GrpMsgForward {fwdSender, fwdBrokerTs}
        body = encodeBinaryBatch [encodeFwdElement fwd msgSignatures_ msgBody]
    createMsgDeliveryJob db gInfo jobScope (Just senderGMId) body
    ```
    Receiver handles via `elementP` → same path as batched forwarding.

#### (e) Enforcement — required signatures

**Current**: `withVerifiedSig` (Subscriber.hs:3203) calls `verifySig` which returns `True` for `Nothing` (unsigned). All unsigned messages pass.

**Change** (1 function):

15. **`withVerifiedSig`** (Subscriber.hs:3203): add unsigned rejection
    - Needs the event tag to check `requiresSignature`
    - Current sig: `GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> Maybe MsgSigData -> UTCTime -> CM a -> CM (Maybe a)`
    - New: add `CMEventTag e` parameter, or pass from caller
    - Logic: if `isNothing msgSig_` AND `groupKeys gInfo` is `Just` AND `requiresSignature tag` → reject

#### (f) Channel stripping

**Current** (Subscriber.hs:3169): `FwdChannel -> processForwardedMsg Nothing` — skips `withVerifiedSig` entirely.

**Change** (in `xGrpMsgForward`):

16. For `FwdChannel`: validate signature if present (call `verifySig`), then call `processForwardedMsg` with `msgSig_` replaced by `Nothing` — strips signatures before storage. Channel posts are anonymous; storing the author's signature would leak identity.

#### Summary: 16 function changes

| # | Function | File | Change |
|---|----------|------|--------|
| 1 | `processAChatMsg` | Subscriber.hs:920 | Pass `msgSig_` to `processEvent` |
| 2 | `processEvent` | Subscriber.hs:941 | Accept `Maybe MsgSigData`, use `signedBody` as body when signed |
| 3 | `saveGroupRcvMsg` | Internal.hs:2218 | Add `Maybe MsgSignatures` parameter (1 caller) |
| 4 | `createNewMessageAndRcvMsgDelivery` | Store/Messages.hs:262 | Add `Maybe MsgSignatures` parameter (2 callers: group passes sigs, direct passes Nothing) |
| 5 | `createNewRcvMessage` | Store/Messages.hs:294 | No change — already has param |
| 6 | `MessageDeliveryTask` | Delivery.hs:128 | `msgBody :: ByteString` + `msgSignatures_` instead of `chatMessage` |
| 7 | `MessageDeliveryTaskRow` | Store/Delivery.hs:128 | `DB.Binary` + `Maybe MsgSignatures` instead of `ChatMessage 'Json` |
| 8 | `getMsgDeliveryTask_` | Store/Delivery.hs:130 | Add `msg.msg_sigs` to SELECT, read `msg_body` as raw bytes |
| 9 | `encodeFwdElement` | Batch.hs:96 | `GrpMsgForward -> Maybe MsgSignatures -> ByteString -> ByteString` |
| 10 | `batchDeliveryTasks1` | Batch.hs:73 | Use task's `msgBody` + `msgSignatures_` |
| 11 | `processForwardedMsg` | Subscriber.hs:3172 | Use `signedBody` as body when signed, pass sigs |
| 12 | `saveGroupFwdRcvMsg` | Internal.hs:2237 | Add `Maybe MsgSignatures` parameter (1 caller) |
| 13 | `createNewRcvMessage` | Store/Messages.hs:294 | No change — already has param |
| 14 | DJRelayRemoved handler | Subscriber.hs:3374 | Binary encoding with `encodeFwdElement` |
| 15 | `withVerifiedSig` | Subscriber.hs:3203 | Reject unsigned messages when `requiresSignature` in relay group with keys |
| 16 | `xGrpMsgForward` FwdChannel | Subscriber.hs:3169 | Validate sig if present, strip before storage |

#### Test

E2E test in relay group with keys:
1. Member A sends `XGrpMemRole` (requires signature) → signed in DB on A
2. Relay receives → verifies → stores `signedBody` as `msg_body` + `MsgSignatures` as `msg_sigs`
3. Relay reads `msg_body` + `msg_sigs` from DB → `><fwd>/<sigs><original-json>` on wire
4. Member B receives → `elementP` parses >→/→json → `signedBody` has original bytes → verifies → stores
5. Unsigned `XGrpDel` from member without keys → rejected by enforcement
6. Channel post with signature → signature stripped before storage

## Files

| File | Step | Changes |
|------|------|---------|
| `Protocol.hs` | 1,2 | `ChatBinding`, `MsgSignatures` encoding, `MsgSigning`, `requiresSignature` |
| `Messages.hs` | 1 | `SndMessage` + `msgSignatures_` |
| `Store/Messages.hs` | 1,2,3 | `createNewSndMessage` signs + stores; `createNewRcvMessage` already has sig param; `createNewMessageAndRcvMsgDelivery` add sig param |
| Migration | 1 | `msg_sigs` column |
| `Internal.hs` | 2,3 | `groupMsgSigning`; `createSndMessages` per-event signing; `saveGroupRcvMsg` + `saveGroupFwdRcvMsg` add sig params |
| `Batch.hs` | 2,3 | `encodeBatchElement` in `batchMessages`; `encodeFwdElement` takes sigs + raw bytes; `batchDeliveryTasks1` uses raw task fields |
| `Subscriber.hs` | 2,3 | `verifySig` with binding; `processAChatMsg`→`processEvent` thread `msgSig_`; `processForwardedMsg` use `signedBody`; `withVerifiedSig` enforcement; channel strip; DJRelayRemoved binary |
| `Delivery.hs` | 3 | `MessageDeliveryTask`: `msgBody` + `msgSignatures_` instead of `chatMessage` |
| `Store/Delivery.hs` | 3 | `MessageDeliveryTaskRow` + `getMsgDeliveryTask_`: read `msg_sigs` + raw `msg_body` |
