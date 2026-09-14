# SimpleX Web Widget: Implementation Plan v2

Revision 2, 2026-05-11

**Status**: Planning
**Parent**: [Product Plan](./2026-03-15-simplex-web-widget-product.md)
**Spike 1 (complete)**: [smp-web spike](../../simplexmq-2/rfcs/2026-03-20-smp-agent-web/2026-03-20-smp-agent-web-spike.md)
**Spec**: [simplexmq spec](../../simplexmq-2/spec/)

## What we know

Spike 1 proved the transport layer: WebSocket to SMP server, handshake with block encryption, server identity verification (challenge-response, v19), short link fetch + decrypt + parse. 24 tests, all byte-for-byte against Haskell.

smp-web (`simplexmq-2/smp-web/`) is the foundation - not throwaway.

## What we're building

A browser-native encrypted chat widget. Visitor opens widget, types name + message, sends it. Owner sees it in SimpleX Chat. Owner responds. Visitor sees the response. Supports direct chats, groups, and business addresses.

## The story

What happens when a visitor clicks "Send":

1. Widget parses the site owner's address (contact, group, or business)
2. Generates X448 key pairs, performs X3DH key agreement
3. Creates a receive queue on an SMP server (preferring a different operator than the owner's server)
4. Encrypts connection request with double ratchet, wraps in agent envelope, encrypts with per-queue E2E
5. Sends to owner's queue via SMP
6. Owner's app receives, decrypts, shows "new contact request"
7. Owner (or bot) accepts - sends reply with their queue info
8. Widget receives reply, completes handshake (KEY + HELLO exchange)
9. Connection is duplex - messages flow both ways, encrypted with double ratchet

What happens when the connection drops:
- SMP client reconnects with exponential backoff
- Resubscribes all queues on that server
- Pending messages in IndexedDB are retried by delivery workers
- No gaps in the integrity chain because messages were encrypted and persisted at enqueue time

What happens on page reload:
- Agent loads connection state, ratchet state, pending messages from IndexedDB
- Reconnects, resubscribes, resumes delivery
- Visitor sees conversation history

## Two packages

**smp-web** (in `simplexmq-2`): protocol encoding, crypto, agent message envelopes, ratchet, agent runtime. Mirrors simplexmq's Haskell. Tested byte-for-byte via `callNode`. The agent is configurable via a single event callback and an externally-provided database handle.

**chat-web** (in `simplex-chat-2`): chat message types, address type handling (contact/group/business), widget UI. Provides the event callback and database to the agent.

```typescript
// smp-web exports
createAgent({
  db,                              // IndexedDB handle, passed in
  onEvent: (event: AgentEvent) => void,  // single callback, sum type
  servers: SMPServer[]
})

// AgentEvent - mirrors Haskell AEvt
type AgentEvent =
  | {type: "message", connId, msgId, msgBody}
  | {type: "connected", connId}
  | {type: "confirmation", connId, confId, connInfo}
  | {type: "sent", connId, msgId}
  | {type: "delivered", connId, msgId}
  | {type: "error", connId, error}
  // ...

// chat-web wires it together
const db = openDatabase()
const agent = createAgent({
  db,
  onEvent: chatHandleEvent,  // chat's handler
  servers
})
```

One event loop. One database. Agent doesn't know about chat. Chat doesn't reimplement the agent. In tests, pass a test handler and verify events.

## Types that make invalid states unrepresentable

```typescript
// Connection states carry their own data - no defensive checks
type Connection =
  | {state: "new", connId: string}
  | {state: "sending", connId: string, sndQueue: SndQueue, rcvQueue: RcvQueue}
  | {state: "duplex", connId: string, sndQueue: SndQueue, rcvQueue: RcvQueue, ratchetId: string}

// A DuplexConnection always has both queues and an initialized ratchet.
// You can't call sendMessage with a SendingConnection - the type prevents it.

// Pending message is always encrypted - delivery worker never touches the ratchet
interface PendingMessage {
  msgId: string
  connId: string
  sndQueueId: string
  encryptedBody: Uint8Array  // encrypted at enqueue time
  createdAt: number
}
```

## Interfaces

**Agent API** (what chat-web calls):

```typescript
interface Agent {
  joinConnection(connReqUri: string, connInfo: Uint8Array): Promise<string>  // returns connId
  sendMessage(connId: string, body: Uint8Array): Promise<string>  // returns msgId
  ackMessage(connId: string, msgId: string): Promise<void>
  deleteConnection(connId: string): Promise<void>
}
```

**Store interface** (what both layers share):

```typescript
interface Store {
  // Connections
  createConnection(conn: Connection): Promise<void>
  getConnection(connId: string): Promise<Connection>
  updateConnection(connId: string, update: Partial<Connection>): Promise<void>

  // Queues
  createQueue(queue: Queue): Promise<void>
  getQueuesForServer(server: string): Promise<Queue[]>

  // Ratchet (atomic read-update-write)
  withRatchet<T>(connId: string, fn: (state: RatchetState) => {result: T, newState: RatchetState}): Promise<T>

  // Messages
  enqueuePendingMessage(msg: PendingMessage): Promise<void>
  getPendingMessages(sndQueueId: string): Promise<PendingMessage[]>
  deletePendingMessage(msgId: string): Promise<void>
  storeMessage(msg: StoredMessage): Promise<void>

  // Commands (for resume on reload)
  enqueuePendingCommand(cmd: PendingCommand): Promise<void>
  getPendingCommands(): Promise<PendingCommand[]>
  deletePendingCommand(cmdId: string): Promise<void>
}
```

The `withRatchet` interface ensures atomicity: the ratchet state is read, the function advances it synchronously (no await inside `fn`), and the new state is written in the same IndexedDB transaction.

## Three phases

### Phase 1: Protocol spike

Extend smp-web with all protocol encodings and crypto needed for connection establishment and messaging. Pure functions, tested against Haskell. No runtime, no persistence, no async.

New dependencies: `@noble/curves` (X448 DH), `@noble/ciphers` (AES-256-GCM for ratchet headers).

#### 1.1 SMP commands (`protocol.ts`)

| Function | Haskell reference | Test |
|----------|-------------------|------|
| `encodeNEW(rcvAuthKey, rcvDhKey, auth_, subMode, queueReqData)` | `encodeProtocol v (NEW ...)` | TS encodes → HS decodes as NEW |
| `decodeIDS(d)` → `{rcvId, sndId, rcvPublicDhKey, queueMode, linkId}` | `smpEncode (IDS ...)` | HS encodes → TS decodes |
| `encodeKEY(senderKey)` | `encodeProtocol v (KEY k)` | TS encodes → HS decodes |
| `encodeSKEY(senderKey)` | `encodeProtocol v (SKEY k)` | TS encodes → HS decodes |
| `encodeSUB()` | `encodeProtocol v SUB` | TS encodes → HS decodes |
| `encodeACK(msgId)` | `encodeProtocol v (ACK msgId)` | TS encodes → HS decodes |
| `encodeSEND(flags, msgBody)` | `encodeProtocol v (SEND flags msg)` | TS encodes → HS decodes |
| `decodeMSG(d)` → `{msgId, msgBody}` | `smpEncode (MSG ...)` | HS encodes → TS decodes |
| Extend `decodeResponse` for IDS, MSG, END, DELD | | |

Notes: NEW encoding is version-dependent (v19). Keys are DER-encoded (Ed25519 for auth, X25519 for DH). `MsgFlags` is a single byte. `auth_` is `Maybe SndPublicAuthKey` for TOFU.

#### 1.2 Agent message envelopes (`agent/protocol.ts`)

Level 1 - `AgentMsgEnvelope` (single-char discriminant):

| Function | Format | Test |
|----------|--------|------|
| `encodeAgentConfirmation(agentVersion, e2eEncryption_, encConnInfo)` | `(version, 'C', e2eParams, Tail encConnInfo)` | TS encodes → HS decodes |
| `decodeAgentConfirmation(d)` | | HS encodes → TS decodes |
| `encodeAgentMsgEnvelope(agentVersion, encAgentMessage)` | `(version, 'M', Tail msg)` | bidirectional |
| `decodeAgentMsgEnvelope(d)` | | |
| `encodeAgentInvitation(agentVersion, connReq, connInfo)` | `(version, 'I', Large connReq, Tail connInfo)` | bidirectional |
| `decodeAgentInvitation(d)` | | |

Level 2 - `AgentMessage`:

| Function | Format | Test |
|----------|--------|------|
| `encodeAgentConnInfo(connInfo)` | `('I', Tail connInfo)` | bidirectional |
| `encodeAgentConnInfoReply(smpQueues, connInfo)` | `('D', smpQueues, Tail connInfo)` | bidirectional |
| `encodeAgentMessage(hdr, aMsg)` | `('M', hdr, aMsg)` | bidirectional |
| `decodeAgentMessage(d)` | discriminant dispatch | |

Level 3 - `AMessage` + `APrivHeader`:

| Function | Format | Test |
|----------|--------|------|
| `encodeAPrivHeader(sndMsgId, prevMsgHash)` | `(sndMsgId, prevMsgHash)` | bidirectional |
| `decodeAPrivHeader(d)` | | |
| `encodeHELLO()` | `"H"` | bidirectional |
| `encodeA_MSG(body)` | `("M", Tail body)` | bidirectional |
| `encodeA_RCVD(receipt)` | `("V", receipt)` | bidirectional |
| `decodeAMessage(d)` | dispatch on H/M/V/etc | |

E2E ratchet params:

| Function | Test |
|----------|------|
| `encodeSndE2ERatchetParams(vRange, x448key1, x448key2)` | bidirectional |
| `decodeSndE2ERatchetParams(d)` | |
| `encodeRcvE2ERatchetParams(vRange, x448key1, x448key2)` | bidirectional |
| `decodeRcvE2ERatchetParams(d)` | |

SMP queue info (for connInfo reply):

| Function | Test |
|----------|------|
| `encodeSMPQueueInfo(queue)` | bidirectional |
| `decodeSMPQueueInfo(d)` | |

#### 1.3 X3DH key agreement (`crypto/ratchet.ts`)

| Function | Haskell reference | Test |
|----------|-------------------|------|
| `generateX448KeyPair()` | via `@noble/curves/ed448` x448 | |
| `x448DH(publicKey, privateKey)` | `dh'` for X448 | Same keys → same shared secret |
| `encodePubKeyX448(rawKey)` → DER | `encodePubKey` for X448 | bidirectional |
| `decodePubKeyX448(der)` → raw | `x509ToPublic'` for X448 | bidirectional |
| `pqX3dhSnd(ourKeys, theirKeys)` → `{rootKey, headerKey, nextHeaderKey}` | `pqX3dhSnd` | Same inputs → identical output |
| `pqX3dhRcv(ourKeys, theirKeys)` → `{rootKey, headerKey, nextHeaderKey}` | `pqX3dhRcv` | Same inputs → identical output |

Notes: 4-DH with X448 (56-byte keys). HKDF-SHA512, info="SimpleXX3DH", output 96 bytes split 32+32+32. Roles reversed from Signal: joiner = Bob = initSndRatchet. PQ KEM deferred.

#### 1.4 Double ratchet (`crypto/ratchet.ts`)

| Function | Haskell reference | Test |
|----------|-------------------|------|
| `initSndRatchet(x3dhResult, ratchetKeyPair)` → `RatchetState` | `initSndRatchet` | Same X3DH output → same initial state |
| `initRcvRatchet(x3dhResult, ratchetKeyPair)` → `RatchetState` | `initRcvRatchet` | |
| `rootKdf(rootKey, dhSecret)` → `{rootKey, chainKey, nextHeaderKey}` | HKDF-SHA512 "SimpleXRootRatchet", 96 bytes | byte-for-byte |
| `chainKdf(chainKey)` → `{chainKey, messageKey, iv1, iv2}` | HKDF-SHA256 "SimpleXCK"/"SimpleXMK", 96 bytes | byte-for-byte |
| `rcEncryptHeader(state, headerPlain)` → `{encHeader, messageKey}` | AES-256-GCM, pad to 88 bytes | |
| `rcDecryptHeader(state, encHeader)` → `{headerPlain, decryptMode}` | Try HKr, NHKr, skipped keys | |
| `rcEncryptMsg(messageKey, iv, body, paddedLen)` → encrypted | AES-256-GCM | |
| `rcDecryptMsg(messageKey, iv, encrypted)` → body | AES-256-GCM | |
| `rcEncrypt(state, body)` → `{encMessage, newState}` | Full encrypt (header + body) | TS encrypt → HS decrypt |
| `rcDecrypt(state, encMessage)` → `{body, newState}` | Full decrypt | HS encrypt → TS decrypt |

Tests: init ratchet pair from X3DH. Encrypt TS → decrypt HS. Vice versa. Out-of-order delivery. Skipped keys (up to maxSkip=512). Chain key advancement matches after N messages.

#### 1.5 Per-queue E2E encryption (`crypto.ts`)

| Function | Haskell reference | Test |
|----------|-------------------|------|
| `encryptForQueue(queueDhKey, senderDhPrivKey, body)` | `encryptSndMsg` (X25519 DH → XSalsa20-Poly1305) | TS encrypt → HS decrypt |
| `decryptFromQueue(rcvDhPrivKey, senderDhPubKey, envelope)` | `decryptRcvMsg` | HS encrypt → TS decrypt |

May reuse xftp-web `cbEncrypt`/`cbDecrypt` if the format matches. Verify exact SMP per-queue envelope format.

#### 1.6 ConnectionRequestUri parsing (`agent/protocol.ts`)

| Function | Haskell reference | Test |
|----------|-------------------|------|
| `decodeConnectionRequestUri(d)` | `smpP @AConnectionRequestUri` | HS encodes → TS decodes |
| `decodeConnReqUriData(d)` → `{agentVRange, smpQueues, clientData}` | `smpP @ConnReqUriData` | |
| `decodeSMPQueueUri(d)` → `{clientVRange, server, senderId, dhPublicKey, queueMode}` | `smpP @SMPQueueUri` | |
| `encodeConnectionRequestUri(cr)` | `smpEncode` | for building invitation connReqs |

Both contact (`'C'`) and invitation (`'I'`) variants.

#### 1.7 Connection request assembly (`agent/protocol.ts`)

| Function | Test |
|----------|------|
| `buildConfirmation(agentVersion, e2eParams, connInfo, ratchetState)` → full AgentConfirmation bytes | HS agent decodes and accepts |
| `buildInvitation(agentVersion, connReq, connInfo)` → full AgentInvitation bytes | HS agent decodes |
| `encodeConnInfo(profile, replyQueues)` → connInfo bytes | |
| `wrapForQueue(queueDhKey, senderDhPrivKey, envelope)` → per-queue encrypted message | |

Integration point: chains X3DH + ratchet encrypt + envelope encode + per-queue E2E. Test against real Haskell agent accepting a connection.

#### 1.8 Chat message encoding (`chat/protocol.ts` in chat-web)

| Function | Direction | Test |
|----------|-----------|------|
| `encodeChatMessage(event, params, msgId?)` → JSON bytes | send | HS decodes |
| `decodeChatMessage(bytes)` → `{event, params}` | receive | HS encodes → TS decodes |
| `encodeXInfo(profile)` | send | |
| `encodeXMsgNew(content)` | send | |
| `decodeXMsgNew(params)` | receive | |
| `decodeXMsgUpdate(params)` | receive only | |
| `decodeXMsgDel(params)` | receive only | |
| `decodeXGrpLinkInv(params)` | receive | business address response |
| `decodeXGrpMemIntro(params)` | receive | group member intro |
| `decodeXGrpMemInv(params)` | receive | group member invitation |
| `decodeXGrpMemFwd(params)` | receive | group member forward |
| `decodeXGrpMemNew(params)` | receive | new member announcement |
| `decodeXGrpAcpt(params)` | receive | group accept |
| `decompressZstd(compressed)` | receive | for compressed messages |

Format: `AppMessageJson {v: ChatVersionRange, msgId?: SharedMsgId, event: string, params: object}`. Agent version: 7. E2E version: 3. Chat version range: current.

#### 1.9 Handwired end-to-end test

Chain all functions: parse address → generate keys → X3DH → build confirmation → encode envelope → per-queue E2E encrypt → send via WebSocket → receive reply → decrypt → HELLO exchange → send encrypted message → receive response.

Test is throwaway. Functions are real. Proves they compose correctly end-to-end against a Haskell agent.

### Phase 2: Shippable agent runtime

The real agent with persistence, delivery workers, reconnection, subscription maintenance. No simplified intermediate. Lives in smp-web.

**2.1 SMP client**

File: `client.ts`

WebSocket connection to one SMP server:
- Command/response correlation via corrId
- Bounded send/receive queues (ABQueue pattern from simplexmq-js)
- Keepalive (PING/PONG)
- Error classification (transient vs permanent)

Test: connect to test SMP server, send commands, verify responses.

**2.2 Connection pool + reconnection**

One SMP client per server. On disconnect:
- Exponential backoff reconnection (base 500ms, max 30s, jitter)
- Resubscribe all queues on that server after reconnect
- Pending deliveries resume automatically (workers read from store)

**2.3 Async command dispatch**

Network commands are never synchronous. Each command:
1. Persisted to IndexedDB (crash-safe)
2. Worker woken to execute
3. On page reload, `getPendingCommands()` resumes all

Commands: NEW, KEY, SKEY, SUB, ACK, SEND (via delivery worker).

**2.4 Delivery workers**

Per-send-queue workers:
- Read pending encrypted messages from IndexedDB
- Send via SMP client
- On OK: delete from store, emit SENT via `onEvent`
- On delivery receipt from peer: emit DELIVERED via `onEvent`
- On transient failure: retry with backoff
- On permanent failure: emit error via `onEvent`

Workers are independent - one server being down doesn't block others.

**2.5 Message receive path**

1. SMP client receives MSG
2. `withRatchet`: decrypt header + body atomically, update ratchet state
3. Verify integrity chain (sndMsgId + prevMsgHash)
4. Store decrypted message
5. Emit event via `onEvent` callback
6. Enqueue ACK as async command

**2.6 Connection state machine**

For joiner (widget is always the joiner):

```
NewConnection
  → join contact/group address
  → create receive queue (NEW)
  → send confirmation (SKEY + SEND)
SndConnection
  → receive reply (MSG with connInfo)
  → register sender key (KEY)
  → send HELLO
DuplexConnection
  → receive HELLO
  → emit CON (connected)
```

Group flow adds: receive `x.grp.mem.intro` → establish separate connections with each member (each goes through the same state machine). Business address: receive `x.grp.link.inv` with `BusinessChatInfo` instead of `x.info`.

**2.7 IndexedDB store**

One database. Implements the `Store` interface. Schema:

| Table | Contents |
|-------|----------|
| `connections` | state, queue refs, metadata, chat-level info |
| `queues` | server, IDs, keys, subscription status |
| `ratchets` | double ratchet state per connection |
| `pendingMessages` | encrypted messages awaiting delivery |
| `messages` | received/sent message history |
| `pendingCommands` | async commands for resume on reload |

`withRatchet` uses a single IndexedDB readwrite transaction to ensure atomicity.

**2.8 Server selection**

When creating the visitor's receive queue, prefer a different operator than the owner's server (mirrors Haskell `getNextServer` logic). For MVP: the widget is configured with a server list. The selection avoids using the same server as the contact address when possible.

**2.9 End-to-end integration test**

TypeScript agent ↔ Haskell agent:
- Connection establishment (contact address)
- Bidirectional message exchange
- Clean integrity chain
- Delivery receipts
- Reconnection + resubscription
- Page reload recovery (persistence)

### Phase 3: Chat protocol and widget

Chat-level semantics wired into the agent via `onEvent`. Lives in chat-web (simplex-chat-2).

**3.1 Chat event handler**

The `onEvent` callback that chat-web provides to the agent:

```typescript
function chatHandleEvent(event: AgentEvent): void {
  switch (event.type) {
    case "message":
      const chatMsg = decodeChatMessage(event.msgBody)
      // dispatch by chat message type
      break
    case "connected":
      // update UI - connection established
      break
    case "confirmation":
      // for groups: auto-accept member introductions
      break
    // ...
  }
}
```

**3.2 Address type handling**

All three types use the same agent `joinConnection`. The differences are in how the response is handled:

- **Contact address**: receive `x.info` (profile) → 1:1 conversation
- **Group link**: receive group info + member introductions → establish per-member connections → group conversation
- **Business address**: receive `x.grp.link.inv` with `BusinessChatInfo` → group conversation that looks like 1:1 to visitor

For groups and business chats: each member introduction triggers a new `joinConnection` through the agent. The agent handles the connection lifecycle. Chat just dispatches.

**3.3 Group member introduction flow**

When visitor joins a group/business chat:
1. Host sends `x.grp.mem.new` to existing members
2. Each existing member sends `x.grp.mem.fwd` with connection request
3. Visitor receives `x.grp.mem.inv` for each member
4. Chat handler calls `agent.joinConnection` for each → separate duplex connection per member
5. Messages in the group are sent to each member's connection independently

This is why per-queue delivery workers matter - each member may be on a different server.

**3.4 Widget UI**

Minimal component library:
- Chat bubble (collapsed, unread indicator)
- Chat window (expanded, message list, input)
- Connection screen (name + incognito + first message)
- Message list with delivery status (single/double checkmark)
- System messages (member joined, connection status)
- Received replies displayed with quoted content
- Dark mode with site sync API
- Accent color customization
- Mobile: full screen when expanded

**3.5 Embedding**

```html
<script
  src="https://simplex.chat/widget.js"
  data-address="simplex:/contact#..."
  data-accent="#0066cc"
  integrity="sha384-..."
  crossorigin="anonymous"
></script>
```

## Build approach

Bottom-up, function-by-function, each tested against Haskell.

Phase 1: pure functions. Each encoding, each crypto operation verified byte-for-byte. The foundation.

Phase 2: the real agent runtime. Tested against live Haskell SMP servers and agents. No throwaway intermediate.

Phase 3: chat semantics + UI. Protocol is proven. This is application logic.

## What's deferred

- **PQ KEM (SNTRUP761)**: X448-only for MVP. Additive - `PQSupport` monotonic.
- **Queue rotation**: additive (QADD/QKEY/QUSE/QTEST).
- **Ratchet synchronization**: MVP shows error, suggests reconnecting.
- **Private routing/proxy**: additive (PRXY/PFWD wraps existing send).
- **File transfer (XFTP)**: separate protocol.
- **Web Push notifications**: via notification router, post-MVP.
- **Migration to app**: QR code transfer, post-MVP.
- **Message forwarding, live messages**: post-MVP.
- **Message editing/deletion on send side**: receive-only for MVP.

## What's NOT deferred

- **Persistence (IndexedDB)**: required for correct integrity chains.
- **Encrypt at enqueue**: required - delivery worker never touches ratchet.
- **Delivery workers with retry**: required for reliability across server failures.
- **Subscription maintenance**: required for message delivery after reconnection.
- **Async command dispatch**: required - sync commands block the event loop.
- **Double ratchet with header encryption**: non-negotiable.
- **X3DH (X448)**: non-negotiable.
- **Agent message envelope format**: byte-compatible with Haskell.
- **Groups + business addresses**: required for MVP.
- **Delivery receipts**: required for checkmark UX.
- **Replies (receive side)**: required for MVP.

## Resolved questions

**Where does code live?** smp-web (simplexmq-2) for protocol + agent runtime. chat-web (simplex-chat-2) for chat message types + widget UI. Agent takes `onEvent` callback + `db` handle from chat-web. One event loop, one database, composed via dependency injection.

**Business address flow**: business address is a contact address where the owner's app creates a group internally. The widget receives `XGrpLinkInv` with `BusinessChatInfo` instead of `XInfo`. Protocol-level, it's the same join flow - the difference is in the chat layer response handling.

**Which server for visitor's queue**: prefer different operator than the owner's server. Widget configured with a server list. Mirrors Haskell `getNextServer` logic.

**Group join**: each member establishes a separate duplex connection with the new joiner. A 3-member group creates ~8 SMP queues for one new joiner (group connections only, no direct connections for widget MVP). Per-member connections go through independent state machines.

**ConnInfo format**: JSON `AppMessageJson {v, msgId, event: "x.info", params: {profile: ...}}`. Same format for contact and group addresses. Business address uses `event: "x.grp.link.inv"` in the response.

**Agent/E2E versions**: agent v7 (`currentSMPAgentVersion`), E2E v3 (`currentE2EEncryptVersion`). Widget advertises current versions.

## Open questions

### Bundle size
libsodium ~550KB unpacked. Need to measure total. May need lazy loading.

### Web Workers
Crypto in Web Worker avoids blocking main thread. Measure first.

### restoreShortLink
Widget needs preset server list for shortened links. How provided?

### CORS
Required for cross-origin embedding. Pattern exists in XFTP server. Not yet in SMP.
