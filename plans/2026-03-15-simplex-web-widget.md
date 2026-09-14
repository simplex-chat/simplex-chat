# SimpleX Web Widget: Master Plan

Revision 2, 2026-05-11

**Status**: Spike 1 complete. Implementation planning.

**Related documents**:
- [Product Plan](./2026-03-15-simplex-web-widget-product.md) -- Users, UX, scope
- [Implementation Plan v2](./2026-03-15-simplex-web-widget/2026-05-11-implementation-v2.md) -- Three-phase plan based on spike findings
- [Spike 1 Plan](../../simplexmq-2/rfcs/2026-03-20-smp-agent-web/2026-03-20-smp-agent-web-spike.md) -- Transport layer proof (complete)
- [Spec](../../simplexmq-2/spec/) -- Agent architecture, connections, message envelopes

## What changed since revision 1

**Spike 1 completed** (2026-03-15 to 2026-04-20): proved transport layer end-to-end. 24 tests against Haskell. WebSocket + SMP handshake + block encryption + server identity verification + short link fetch/decrypt/parse. Code lives in `simplexmq-2/smp-web/`, not throwaway.

**Key decisions made during spike**:
- SMP protocol version bumped to v19 (`webClientSMPVersion`) for server identity challenge-response
- Server challenge sent via URL query parameter (browser WebSocket API doesn't allow custom headers)
- xftp-web reused extensively (encoding, secretbox, identity verification, keys)
- Bottom-up approach validated: each function tested byte-for-byte against Haskell via `callNode`

**Architecture revised**:
- **One event loop, one database** -- the Haskell two-database/two-loop separation was an evolutionary artifact, not a design decision. Browser widget fuses agent + chat into one runtime.
- **Two packages**: smp-web (simplexmq-2) for protocol + agent runtime, chat-web (simplex-chat-2) for chat message types + widget UI. Agent takes a single `onEvent` callback + database handle from chat-web.
- **Persistence from day one** (IndexedDB) -- required for correct integrity chains. Encrypt at enqueue, not at send. Delivery worker is a dumb pump.
- **Real agent, no throwaway intermediate** -- no "simple agent that works under ideal conditions." Phase 1 produces tested pure functions. Phase 2 builds the shippable agent directly.

See [Implementation Plan v2](./2026-03-15-simplex-web-widget/2026-05-11-implementation-v2.md) for the full three-phase plan.

## Overview

This document defines what needs to be built for the web widget at each layer. It is a master plan -- high-level structure, not exhaustive function mapping.

The widget runs entirely in browser. It connects to SMP routers via WebSocket, implements the subset of agent functionality needed for messaging, and provides chat-level features (connection to any address type, message display, delivery status).

**Layers:**

```
┌─────────────────────────────────────────┐
│  Widget UI (React/Preact)               │  ← User interaction
├─────────────────────────────────────────┤
│  Chat Layer (TypeScript)                │  ← Address types, message handling
├─────────────────────────────────────────┤
│  Agent Layer (TypeScript)               │  ← Connection lifecycle, encryption
├─────────────────────────────────────────┤
│  SMP Client Layer (TypeScript)          │  ← WebSocket transport, protocol
└─────────────────────────────────────────┘
                    │
                    ▼ WebSocket (TLS)
            ┌───────────────┐
            │  SMP Router   │
            └───────────────┘
```


## Phase 0: Spike

**Goal**: Prove the whole thing works at minimum cost. No production code -- throwaway.

### What the Spike Does

1. Parse contact/business address URI
2. Open WebSocket to SMP router (direct, no proxy)
3. Send LGET command to fetch link data
4. Decrypt profile data
5. Display in browser console

### What This Proves

- WebSocket transport to SMP router works from browser
- Protocol encoding is correct (LGET parses, response decodes)
- Crypto is compatible (decryption works)
- The whole thing is buildable

### What Spike Does NOT Need

- Sending connection request (more complex, doesn't add to proof)
- SKEY/SEND commands
- Proxy/private routing
- Double ratchet
- Persistence
- Any UI

### Deliverable

- Browser console demo: paste address URI, see profile data
- Throwaway TypeScript code
- Notes on what was tricky (encoding, crypto, WebSocket framing)


## Phase 1: Router Infrastructure

**Prerequisite for spike.** SMP routers need WebSocket upgrade support for web clients.

### Current State

- `Transport/WebSockets.hs` exists but runs in exclusive mode
- Router is EITHER WebSocket OR normal HTTP -- not both
- Info page served on HTTP, no upgrade path for browser JS

### Required Changes

1. **WebSocket upgrade on same port**
   - HTTP request with `Upgrade: websocket` header triggers upgrade
   - Normal browser navigation still shows info page
   - Same TLS connection, just protocol switch

2. **CORS headers**
   - `Access-Control-Allow-Origin` for widget domains
   - Allow cross-origin WebSocket connections

3. **No service certificate changes**
   - Widget uses regular client auth, not service certs
   - No changes needed for MVP

### Implementation Location

- `Simplex.Messaging.Server` -- HTTP handler modification
- `Simplex.Messaging.Transport.WebSockets` -- already has WebSocket logic
- Need to bridge: HTTP request → check upgrade → WebSocket transport

### Deliverable

- SMP routers accept WebSocket upgrade from browsers
- Info page still works for direct navigation
- CORS configured for widget embedding


## Phase 2: SMP Client Layer (TypeScript)

Minimal protocol client that can send/receive SMP commands over WebSocket.

### Transport

- WebSocket connection with TLS (browser handles certificate validation)
- Block framing: 16384 byte blocks, same as native client
- Reconnection with exponential backoff

### Protocol Subset

Commands needed for widget:

| Command | Purpose |
|---------|---------|
| NEW | Create receive queue (for visitor's side of connection) |
| KEY | Register sender key on receive queue |
| SKEY | Authenticate as sender on queue |
| SUB | Subscribe to queue for messages |
| ACK | Acknowledge message receipt |
| SEND | Send message to queue |
| OFF | Disable queue (when visitor deletes conversation) |
| DEL | Delete queue |

Proxied messaging (private routing):

| Command | Purpose |
|---------|---------|
| PRXY | Request proxy session to destination router |
| PFWD | Forward command through proxy |

### Correlation

- Generate correlation IDs for commands
- Match responses to pending requests
- Timeout handling for unresponsive routers

### Deliverable

- `SMPClient` class: connect, disconnect, send command, receive response/event
- Connection pooling (one connection per router)
- Automatic reconnection


## Phase 3: Agent Layer (TypeScript)

Connection lifecycle and encryption. This is the core complexity.

### Key Management

- Generate X25519 key pairs (DH for queue auth)
- Generate X448 key pairs (PQ-resistant ratchet init)
- Store keys in IndexedDB (encrypted with derived key from random secret in localStorage)

### Connection Establishment

Support joining via invitation URI (the common case for widget):

1. Parse invitation URI (extract queue address, DH keys)
2. Create local receive queue (NEW)
3. Initialize sending ratchet (X3DH key agreement)
4. Send confirmation to inviter's queue (SKEY + SEND)
5. Wait for reply with inviter's queue info
6. Complete ratchet initialization
7. Exchange HELLO messages
8. Connection ready

Also support creating invitation (for future use cases):

1. Create receive queue (NEW)
2. Generate invitation URI with queue address + DH keys
3. Wait for joiner's confirmation
4. Complete connection handshake

### Double Ratchet

- Implement Signal double ratchet for message encryption
- Header encryption (prevents metadata leakage)
- Ratchet state persistence in IndexedDB
- Key derivation using HKDF

### Message Processing

Receive path:
1. Decrypt with double ratchet
2. Verify message integrity (sequence + hash chain)
3. Store in local database
4. Emit event to chat layer
5. ACK to router (can be batched)

Send path:
1. Advance ratchet, store pending message with encryption key
2. Encrypt body with stored key
3. Encode agent message envelope
4. SEND to router
5. On OK, mark as sent; on delivery receipt, mark as delivered

### Persistence

- IndexedDB for: keys, ratchet state, messages, connection state
- Cross-tab coordination via BroadcastChannel or SharedWorker
- Single "active" tab owns WebSocket connections, others receive via broadcast

### Deliverable

- `AgentClient` class: createConnection, joinConnection, sendMessage, ackMessage
- Event emitter for received messages and status updates
- Persistence layer with cross-tab sync


## Phase 4: Chat Layer (TypeScript)

Address type handling and message semantics.

### Address Types

Widget must handle any SimpleX address type:

1. **Contact address** -- 1:1 conversation with site owner
2. **Group link** -- Join group, see all members
3. **Business address** -- Hybrid: looks like 1:1 to visitor, but is group internally

All three use same underlying protocol. Difference is in:
- How connection is established (contact request vs group join)
- What metadata is shown (single owner vs multiple agents)
- Message attribution (who sent what)

### Connection Flow by Type

**Contact address:**
- Parse contact URI
- Join connection with chosen display name
- First message included in connection request
- Owner accepts, connection established

**Group link:**
- Parse group link URI
- Join group with chosen display name
- Receive group info, member list
- Can see and message all members

**Business address:**
- Parse business address URI (same format as contact)
- Join with display name
- Internally creates business chat (group with special properties)
- Visitor sees business name, may see individual agent names on messages
- Agents can join/leave without visitor needing to know

### Message Types

Display in widget:

| Type | Handling |
|------|----------|
| Text | Display as message bubble |
| Agent join | System message: "Alex joined the conversation" |
| Delivery receipt | Update checkmarks (single → double) |
| File reference | Link to upload page |
| Reaction | Display on referenced message |
| Reply | Display with quoted content |

Create in widget (MVP):

| Type | Handling |
|------|----------|
| Text | Primary message type |
| Delete notification | When visitor deletes conversation |

Post-MVP:

| Type | Handling |
|------|----------|
| Reaction | Visitor can react to messages |
| Reply | Visitor can quote messages |
| File | Direct upload |

### State Management

- Current connection state (disconnected, connecting, connected)
- Message list with delivery status
- Unread count for bubble indicator
- Business/contact metadata for display

### Deliverable

- `ChatClient` class: connect to address, send message, receive events
- Address parsing for all three types
- Message rendering helpers


## Phase 5: Widget UI

React or Preact component library. Smallest reasonable bundle.

### Components

- `ChatBubble` -- Collapsed state, unread indicator
- `ChatWindow` -- Expanded state, message list, input
- `ConnectionScreen` -- Name input, incognito button, first message
- `MessageList` -- Scrolling message display
- `MessageInput` -- Text entry, send button
- `SystemMessage` -- Agent joins, connection status

### Customization API

```typescript
interface WidgetConfig {
  address: string;           // SimpleX address URI
  accentColor?: string;      // Hex color for buttons, links
  darkMode?: boolean | 'auto'; // Sync with site or explicit
  position?: 'bottom-right'; // MVP: only bottom-right
  welcomeMessage?: string;   // Override from address data
}
```

### Embedding

```html
<script
  src="https://simplex.chat/widget.js"
  data-address="simplex:/contact#..."
  data-accent="#0066cc"
  integrity="sha384-..."
  crossorigin="anonymous"
></script>
```

Script tag creates widget automatically. Integrity hash ensures version pinning.

### Self-hosting

Site owners can host `widget.js` themselves for maximum trust. Same API, different src.

### Deliverable

- Minimal component library
- CSS-in-JS or minimal CSS (no external dependencies)
- Bundle size target: < 100KB gzipped (including crypto)


## Design Principles

### Mirror Haskell Structure

TypeScript code should model Haskell function names and module structure:

- `Simplex.Messaging.Protocol` → `protocol.ts` with same type/function names
- `Simplex.Messaging.Parsers` → `parser.ts`
- `serializeSMPCommand` in Haskell → `serializeSMPCommand` in TypeScript
- `smpCommandP` parser → `smpCommandP`

This enables:
- Easy cross-reference between codebases
- Sync as protocol evolves
- Code review by people who know Haskell side


## Prior Art: xftp-web

`../simplexmq-2/xftp-web/` is the production reference for browser TypeScript.

### Direct Reuse

**Crypto** (`src/crypto/`):
- `secretbox.ts` -- XSalsa20-Poly1305 streaming, matches Haskell Crypto.hs
- `digest.ts` -- SHA-256/512 via libsodium
- `keys.ts` -- Ed25519/X25519, DER encoding, key hash
- `identity.ts` -- X.509 certificate verification

**Encoding** (`src/protocol/encoding.ts`):
- `Decoder` class -- sequential binary parser
- Integer encoding (Word16, Word32, Int64 big-endian)
- ByteString, Large, Tail patterns
- Comments reference Haskell line numbers

### Libraries Used

```json
{
  "libsodium-wrappers-sumo": "^0.7.13",
  "@noble/curves": "^1.4.0"
}
```

libsodium for most crypto, @noble/curves for Ed448 (not in libsodium).


## Prior Art: simplexmq-js

`../simplexmq-js/` contains a working SMP client from ~2021. Protocol has evolved (v0.4 → v19), but patterns transfer.

### Reuse (patterns, not code)

**Parser** (`parser.ts`):
- Combinator-style scanner that avoids string splits
- `&&` chaining models Haskell grammar-based parsers
- Returns `undefined` on failure, enabling clean call sites:
```typescript
MSG: (p) => {
  let msgId, msg: Uint8Array | undefined
  let ts: Date | undefined
  return (
    p.space() && (msgId = p.base64()) && p.space() &&
    (ts = p.date()) && p.space() && (msg = messageP(p)) &&
    cMSG(msgId, ts, msg)
  )
}
```

**Protocol types** (`protocol.ts`):
- One type per command, mirrors Haskell ADT
- Constructors: `cNEW`, `cMSG`, `cLGET`, etc.
- Binary tag tables for serialization

**Buffer utilities** (`buffer.ts`):
- Base64 encode/decode
- Binary concatenation
- Integer encoding

**Async bounded queue** (`queue.ts`):
- `ABQueue` with semaphore-based backpressure
- Async iterator protocol -- `for await (const msg of transport)`
- Clean close semantics with sentinel
- This pattern is solid, likely reusable

### Review Before Reuse

**WebSocket transport** (`transport.ts`):
- `WSTransport` is ~30 lines, simple enough
- Connect, wire handlers to ABQueue, wait for open
- May be fine as-is, review during spike
- Custom RSA handshake (`SMPTransport`) no longer applies -- now TLS
- No socket.io needed -- plain WebSocket is universal

### Do Not Reuse

**Crypto** (`crypto.ts`):
- Was RSA-OAEP + AES-GCM
- Now X25519 + XChaCha20-Poly1305
- Use `@noble/*` libraries instead

### Pre-implementation Task

Scan `simplexmq-js` for utilities worth adapting:
- `Parser` class
- `ABQueue` (async bounded queue)
- Buffer helpers
- Type patterns

Document what transfers vs what needs rewrite.


## Haskell Modules to Reference

TypeScript should mirror these modules:

### Protocol Layer (simplexmq)
| Haskell | TypeScript | Purpose |
|---------|------------|---------|
| `Simplex.Messaging.Protocol` | `protocol.ts` | Commands, responses, encoding |
| `Simplex.Messaging.Transport` | `transport.ts` | Handshake, block framing |
| `Simplex.Messaging.Encoding` | `encoding.ts` | smpEncode/smpP patterns |
| `Simplex.Messaging.Parsers` | `parser.ts` | Parser combinators |
| `Simplex.Messaging.Crypto` | `crypto.ts` | Encryption primitives |
| `Simplex.Messaging.Crypto.ShortLink` | `shortLink.ts` | KDF, link encryption |

### Agent Layer (simplexmq)
| Haskell | TypeScript | Purpose |
|---------|------------|---------|
| `Simplex.Messaging.Agent.Protocol` | `agentProtocol.ts` | Connection types, link data |
| `Simplex.Messaging.Agent.Client` | `agentClient.ts` | Connection management |
| `Simplex.Messaging.Crypto.Ratchet` | `ratchet.ts` | Double ratchet |

### Chat Layer (simplex-chat)
| Haskell | TypeScript | Purpose |
|---------|------------|---------|
| `Simplex.Chat.Protocol` | `chatProtocol.ts` | Message types |
| `Simplex.Chat.Types` | `chatTypes.ts` | Profile, contacts |


## Dependencies and Build

### Crypto Libraries

- `@noble/curves` -- X25519, X448, Ed25519 (audited, pure JS)
- `@noble/ciphers` -- ChaCha20-Poly1305, XSalsa20 (audited, pure JS)
- `@noble/hashes` -- SHA-256, SHA-512, HKDF (audited, pure JS)

These are the same libraries used by many crypto projects, well-audited, no WASM.

### Encoding

- CBOR for message encoding (same as native client)
- Custom binary encoding for SMP protocol frames

### Build

- esbuild or Vite for bundling
- Tree-shaking to minimize size
- Separate chunks for core vs UI (allow headless use)


## Testing Strategy

### Unit Tests

- Protocol encoding/decoding
- Crypto operations (encrypt/decrypt round-trip)
- Ratchet state transitions
- Address parsing

### Integration Tests

- Connect to test router
- Full connection establishment
- Message send/receive
- Reconnection handling

### E2E Tests

- Widget embedding in test page
- User flow: open, enter name, send message
- Cross-tab synchronization
- Persistence across page reload


## Milestones

### M0: Spike -- Prove Buildability

**Objective**: Fetch and display business profile from address URI. Minimum code to prove e2e works.

**Scope**:
- Parse address URI
- WebSocket to SMP router (direct, no proxy)
- LGET command
- Decrypt profile data
- Display in console

**Success criteria**: Paste address URI in browser console, see profile data displayed.

---

### M1: Protocol Foundation
- SMP client with WebSocket transport
- Full command set (NEW, KEY, SKEY, SUB, ACK, SEND, OFF, DEL)
- Connection pooling, reconnection with backoff
- Correlation ID handling

### M2: Connection Establishment
- Key generation and storage
- X3DH key agreement
- Join connection via invitation
- HELLO exchange
- Connection state machine

### M3: Encrypted Messaging
- Double ratchet implementation
- Send and receive messages
- Delivery receipts

### M4: Persistence
- IndexedDB storage
- Cross-tab coordination
- Session continuity

### M5: Chat Layer
- All address types
- Message type handling
- Agent join notifications

### M6: Widget UI
- Component library
- Embedding API
- Customization

### M7: Polish
- Error handling
- Reconnection UX
- Performance optimization
- Documentation


## Open Questions

### Encoding Approach
- XFTP web used direct Haskell-to-TypeScript port
- You mentioned showing a different encoding approach -- waiting for that example

### Bundle Size
- 100KB target may be aggressive with full crypto
- Could split: tiny loader + async load main bundle
- Measure actual size after M1

### Browser Support
- Modern browsers only (ES2020+)
- No IE11, no legacy Edge
- Safari WebSocket behavior needs testing

### Proxy Selection
- How does widget choose which proxy router to use for private routing?
- Hardcoded list? Dynamic discovery? Site owner config?


## Not In Scope

- Web Push notifications (post-MVP, uses notification router)
- Migration to app (post-MVP, QR code transfer)
- File upload (post-MVP, in-widget upload)
- Typing indicators (not supported in protocol yet)
- Full accessibility (deferred)
- White-label (counter to value proposition)
