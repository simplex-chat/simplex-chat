# Service RPC in simplex-chat

Add chat-level support for the agent's Service RPC (single request → single response over a
DR contact address) and reasoned contact rejection, on top of the agent API shipped in simplexmq
`ccc1d7f8` (pinned in `cabal.project:24`).

## Agent API available (simplexmq HEAD, verified)

- `sendServiceRequest :: AgentClient -> NetworkRequestMode -> UserId -> ConnectionRequestUri 'CMContact -> MsgBody -> AE MsgBody` — sync; fails fast on server down; blocks and returns the response.
- `sendServiceRequestAsync :: AgentClient -> UserId -> ConnectionRequestUri 'CMContact -> MsgBody -> AE MsgBody` — enqueues a retried JOIN, blocks on the same TMVar, returns the response.
- `sendServiceReply / sendServiceReplyAsync :: ... -> UserId -> InvitationId -> MsgBody -> AE ()`.
- `rejectServiceRequest / rejectServiceRequestAsync :: ... -> UserId -> InvitationId -> Maybe ByteString -> AE ()`.
- `rejectContact :: AgentClient -> NetworkRequestMode -> UserId -> ConfirmationId -> Maybe ByteString -> AE ()` (already called at `Commands.hs:1448` with `Nothing`).
- Events: `SREQ :: InvitationId -> MsgBody -> AEvent AEConn`; `RJCT :: ConnInfo -> AEvent AEConn` (reason bytes).
- Errors: `A_SERVICE (serviceError :: AgentServiceError)`, `AgentServiceError = ASERejected {rejectReason :: String} | ASETimeout | ASENoPendingRequest | ASENotDRAddress`.
- A service request to a non-DR contact address fails fast with `A_SERVICE ASENotDRAddress` (agent guards this in `serviceRequest_`).

## Agent-side changes required (simplexmq)

These must be implemented in the agent first (rebuild, bump the chat pin):
1. **Per-call request timeout** (D1): add `Maybe NominalDiffTime` to `sendServiceRequest` /
   `sendServiceRequestAsync` and thread it into `serviceRequest_`, overriding
   `serviceRequestTimeout` from config when `Just` (`reqTimeout <- maybe (asks $ serviceRequestTimeout . config) pure timeout_`).
2. **Rejection-reason capability on `REQ`** (item 8): add a `Bool` to `REQ` indicating whether a
   rejection reason can be delivered (i.e. the stored invitation is `CRInvitationDR`), sourced at
   `smpContactRequest`. Update all `REQ` consumers (chat Subscriber.hs:1398 and any others).

## Cross-cutting decisions (resolved)

### D1. Blocking, with per-call timeout override
Commands are blocking (confirmed). `APISendServiceRequest` uses `sendServiceRequestAsync` and blocks
until the response, but takes an explicit `Maybe NominalDiffTime` that overrides the default
`serviceRequestTimeout` per call (agent change #1). Command processing is per-request in
`processChatCommand`; a blocked command does not stall the agent-event subscriber.

### D2. No persistence in chat (confirmed)
Do not persist incoming service requests. Carry the agent `InvitationId` (as `AgentInvId`) through
`CEvtServiceRequest` and back through `APISendServiceResponse`. The agent already persists the
invitation and deletes it on `serviceResponseTimeout`.

### D3. Payload types (resolved) — all `J.Object`
All four service-RPC payloads are `J.Object` (`Data.Aeson.Object`), to guarantee valid JSON on the
wire in both directions:
- `APISendServiceRequest.request :: J.Object` (UI → wire: `J.encode`).
- `CEvtServiceRequest.requestData :: J.Object` (wire → service: parse the received `MsgBody` as a
  JSON object; a non-object / invalid JSON body is an error).
- `APISendServiceResponse.responseData :: J.Object` (service → wire).
- `CRServiceResponse.responseData :: J.Object` (wire → requester: parse the response `MsgBody`).
The agent still carries opaque `MsgBody`; the chat layer encodes/decodes `J.Object` at each boundary.

## Item-by-item plan

### 1 + 2. APISendServiceRequest → CRServiceResponse
- `ChatCommand` (Controller.hs ~534, near the connect commands):
  `APISendServiceRequest {userId :: UserId, sendTarget :: ConnectTarget 'CMContact, requestTimeout :: Maybe NominalDiffTime, request :: J.Object}`.
  Accept the mode-indexed `ConnectTarget 'CMContact` (parser must reject `CMInvitation` by producing
  a parse/command error) — see target resolution below.
- `ChatResponse` (Controller.hs ~830): `CRServiceResponse {user :: User, responseData :: J.Object}`
  (parse the agent's response `MsgBody` as a JSON object; invalid → chat error).
- Parser (`chatCommandP`, Commands.hs ~5479): `"/_service_request "` reading userId + a
  `ConnectTarget 'CMContact` + optional timeout + the JSON request (`jsonP`). Parse the mode-indexed
  target with a `ConnectionModeI m => Parser (ConnectTarget m)` (or `StrEncoding (ConnectTarget m)`
  instance) that parses `AConnectTarget` (`strP`, Types.hs:1826) and narrows via
  `testEquality m (sConnectionMode @m)`, failing on mode mismatch — the pattern of `connReqUriP'`
  (Protocol.hs:1236).
- Processing (Commands.hs): resolve `sendTarget` to a `ConnectionRequestUri 'CMContact` (target
  resolution below), then
  `resp <- withAgent $ \a -> sendServiceRequestAsync a (aUserId user) cReq requestTimeout (LB.toStrict $ J.encode request)`
  and return `CRServiceResponse user resp`.
  `withAgent :: (AgentClient -> ExceptT AgentErrorType IO a) -> CM a` (Controller.hs:1762);
  `sendServiceRequestAsync` returns `AE MsgBody`, so this is `CM MsgBody`. `aUserId user` is the
  agent user id (same as the existing `rejectContact a NRMInteractive (aUserId user) invId ...` at
  Commands.hs:1448). Signature assumes agent change #1 added `Maybe NominalDiffTime`.
- Outcomes surfaced to the caller: **success** → `CRServiceResponse`; **timeout** →
  `A_SERVICE ASETimeout` (also the outcome when the responder does not support service requests — it
  drops the request without a reply, so the sender waits out `requestTimeout`); **non-DR target** →
  `A_SERVICE ASENotDRAddress`. Chat maps these `ChatErrorAgent` cases to chat errors; it does not
  pre-check DR. (`ASERejected` is not produced: chat has no service-rejection-with-reason.)

Target resolution (`ConnectTarget 'CMContact` → `ConnectionRequestUri 'CMContact`). Name types are
`NTContact` and `NTPublicGroup` (Commands.hs:4412–4413 — a channel is `NTPublicGroup`):
- `CTFullContact cr` → `cr` directly (agent checks DR; no resolution).
- `CTShortContact (CTName SimplexNameInfo {nameType = NTContact, nameDomain = d})` → resolve the
  name to its contact link and then to `CRContactUri`. Reuse the resolution used by `connectPlan`
  (Commands.hs:4306; the `SCMContact`/`CTName` branch at 4328–4412): `resolveSimplexName a nm
  (aUserId user) d` → `NameRecord`, take `nrSimplexContact` (the contact link; `firstNameLink
  CCTContact`), then `getShortLinkConnReq nm user sLnk` (Commands.hs:2384) → `cReq`. Require a
  contact link; reject if it resolves only to a channel.
- `CTShortContact (CTName {nameType = NTPublicGroup})` → **fail** ("channel name not supported").
- `CTShortContact (CTLink shortLink)` → **supported**: resolve via `getShortLinkConnReq nm user
  shortLink` (Commands.hs:2384) → `cReq` (a contact short link resolves to a contact URI).
- `CTDomain _` → **fail** ("domain not supported").

### 3 + 7. SREQ event → CEvtServiceRequest, gated by start flag
- Extend `StartChat` (Controller.hs:351) with `processServiceRequests :: Bool` (default `False`);
  keep `mainApp, enableSndFiles`. All `StartChat` sites use record syntax (patterns safe):
  Controller.hs:696 (`StartChat {}` wildcard — no change); Commands.hs:552 (pattern — add the field
  to use it); Commands.hs:5406 (full-form construction — parse and add the field);
  Commands.hs:5407 (`/_start` bare — add `processServiceRequests = False`).
- Thread through `startChatController` (Commands.hs:219 — currently `Bool -> Bool -> CM' (Async ())`;
  becomes `Bool -> Bool -> Bool -> ...`), and add a runtime flag to `ChatController`
  (Controller.hs:277) mirroring `subscriptionMode :: TVar SubscriptionMode` (:291):
  `processServiceRequests :: TVar Bool`. Set it in `startChatController` via `chatWriteVar'`
  (`chatReadVar`/`chatWriteVar` at Controller.hs:1647/1655 take `ChatController -> TVar a`).
  Both callers pass the new arg: `Core.hs:93` (`startChatController True True` → add `False`) and
  `Commands.hs:555` (`startChatController mainApp enableSndFiles` → add the new `StartChat` field).
- The start flag answers exactly one question: **does this instance support service requests?**
- Handle `SREQ` in `processContactConnMessage` (Subscriber.hs:1397 — the `UserContact` entity
  handler), immediately after `REQ` (:1398), since SREQ arrives on the published contact address
  like REQ:
  `SREQ invId payload -> chatReadVar processServiceRequests >>= \case`
    `True  -> toView $ CEvtServiceRequest user (AgentInvId invId) payload`
    `False -> withAgent $ \a -> rejectServiceRequest a NRMBackground (aUserId user) invId Nothing`
  When unsupported, **actively reject** (drop) the request so pending service requests do not
  accumulate on the agent side until timeout. No reason is sent.
  Note: unlike `REQ` (which does `parseChatMessage' conn connInfo`), the SREQ payload is **not** a
  `ChatMessage` — it is the raw request body; do not parse it as one.
- `ChatEvent` (Controller.hs:924): `CEvtServiceRequest {user :: User, requestId :: AgentInvId, requestData :: J.Object}`
  (parse the received `MsgBody` as a JSON object).

### 4. APISendServiceResponse
- `ChatCommand`: `APISendServiceResponse {userId :: UserId, requestId :: AgentInvId, responseData :: J.Object}` (per D3).
- Processing: `withAgent $ \a -> sendServiceReplyAsync a "" (aUserId user) invId (LB.toStrict $ J.encode responseData)`
  (async reply, retried and delivery-bounded by `serviceResponseTimeout`). Return `CRCmdOk`.

### 5. (removed — no service-rejection API)
Per spec, service-request rejection is only the automatic drop in item 3 (when the instance does not
support service requests). There is no app-driven reject API and no service-rejection-with-reason;
`ContactRejectionReason` is **not** used for service requests.

### 6. ContactRejectionReason + reasoned APIRejectContact
- New type (Types.hs), modelled on `GroupRejectionReason` (Types.hs:968):
  `data ContactRejectionReason = CRRUserRejected | CRRUnknown {text :: Text}` with a `StrEncoding`
  instance (`CRRUserRejected -> "user_rejected"`, `CRRUnknown t -> encodeUtf8 t`, and an
  `A.takeByteString` fallback → `CRRUnknown`) and JSON via `strToJSON`/`strParseJSON` — the reason
  encodes to a JSON string. The `strEncode` bytes are the `reason` sent in the agent `AgentRejection`.
- Extend `APIRejectContact {contactReqId :: Int64}` (Controller.hs:411) →
  `APIRejectContact {contactReqId :: Int64, notify :: Bool}`. `notify` chooses whether to send the
  rejection to the requester; the reason is fixed `CRRUserRejected` (the only variant), so the API
  has no reason parameter (mirrors `ChatDeleteMode {notify :: Bool}`, Controller.hs:1100). Call sites
  (1432/2475 are positional → break on the new field):
  - Controller.hs:411 — definition.
  - Commands.hs:1432 — pattern `APIRejectContact connReqId ->` → add `notify`.
  - Terminal `RejectContact` (Controller.hs:564, parser :5712, handler :2473) gains `notify :: Bool`
    (so a notifying rejection is CLI-testable); :2475 forwards it to `APIRejectContact`.
  - Commands.hs:5480 — parser `"/_reject "` → parse `notify` (on/off); :5712 (`/reject @name`) likewise.
- Processing (`rejectCReq`, Commands.hs:1438): `notify = True` sends `CRRUserRejected`, `False` sends
  nothing. **Send the rejection before deleting** the local records: currently deleted at 1444–1446,
  then `rejectContact … Nothing` at :1448; reorder so
  `rejectContact a NRMInteractive (aUserId user) invId (if notify then Just (strEncode CRRUserRejected) else Nothing)`
  runs first, then the deletion. A notifying rejection of a non-DR request makes the agent throw
  `CMD PROHIBITED`; because the send is first, nothing is deleted — surface the error so the UI shows
  the alert and offers a silent (non-notifying) retry. `rejectionSupported` (item 8) drives whether
  the UI offers the notify option up front.
- Requester side (currently no `RJCT` handler in chat): add `RJCT reasonBytes` in `processDirectMessage`
  (Subscriber.hs:441), the `RcvDirectMsgConnection conn contact_` handler, which already has
  `contact_ :: Maybe Contact` — the contact to mark. (Not `processContactConnMessage`, which handles
  the responder's `UserContact` address.) Handling:
  (1) `strDecode` the `ContactRejectionReason` from `reasonBytes` (decode failure → `Nothing`);
  (2) set the contact's status to `CSRejected`; (3) emit `CEvtContactRequestRejected {user, contact,
  rejectionReason :: Maybe ContactRejectionReason}`. The reason is reported in the event only; it is
  not persisted.
- Add `CSRejected` to `ContactStatus` (Types.hs:315), with `textEncode`/`textDecode` `"rejected"`
  (mirrors `GSMemRejected` Types.hs:1356, `RSRejected` Types/Shared.hs:95). No new column — the
  existing `contact_status` text column stores it.

### 8. UI signal: is reasoned rejection possible for a contact request?
Reasoned rejection needs a DR channel back to the requester (the agent's `rejectContact reason` path,
`prepareReply`, requires the stored invitation's `connReq` to be `CRInvitationDR`; a non-DR request
throws "connection has no double ratchet to send reply"). This is per-request, so it must flow from
the agent invitation.
- **Agent change #2**: add a `Bool` to `REQ` signalling rejection-reason-possible, sourced from the
  invitation's `connReq` (DR) at `smpContactRequest`; update all `REQ` consumers.
- Agent change #2: add a `Bool` to `REQ` signalling rejection-reason capability, sourced from the
  invitation's `connReq` (DR) at `smpContactRequest`; update all `REQ` consumers.
- Chat: `profileContactRequest` (Subscriber.hs:1470) → `createOrUpdateContactRequest` stores the flag
  on the contact-request row (new column), surfaced two ways:
  1. On `UserContactRequest {rejectionSupported :: Bool}` (Types.hs:371), in
     `CEvtReceivedContactRequest` (Controller.hs:946).
  2. On the prepared `Contact` that represents the request (`REContact ct`, Subscriber.hs:1482/1490).

Prepared-Contact representation:
- New type `data UserContactRequestRef = UserContactRequestRef { contactRequestId :: Int64, rejectionSupported :: Bool }`.
- Add `contactRequest :: Maybe UserContactRequestRef` to `Contact` (Types.hs:193). Keep the existing
  `contactRequestId :: Maybe Int64` (:207) — mobile/desktop remote-protocol compatibility requires it
  in Haskell — but UI switches to `contactRequest` (whose ref carries the id), stopping use of the
  bare `contactRequestId`.
- Populate via LEFT JOIN, not denormalization: `getContact_` (Store/Direct.hs:963) already selects
  `ct.contact_request_id` and joins `contact_profiles`/`connections`; add
  `LEFT JOIN contact_requests cr2 ON cr2.contact_request_id = ct.contact_request_id`, select
  `cr2.rejection_supported`. `getUserContacts` (:817) calls `getContact` per id, so this is one extra
  join per contact query — no batched-query change. Build
  `contactRequest = UserContactRequestRef <$> ct.contact_request_id <*> rejectionSupported_` — `Just`
  only while the request row exists (pending); `Nothing` once accepted/rejected (row deleted at
  Direct.hs:891).
- DB: add `rejection_supported` to `contact_requests`.

## Implementation order
1. Agent (simplexmq): (a) `Maybe NominalDiffTime` on `sendServiceRequest[Async]` (D1); (b) `Bool` on
   `REQ` for rejection-reason capability (item 8). Rebuild; bump chat's pin.
2. Chat types: `ContactRejectionReason` (StrEncoding); `UserContactRequestRef`; `CSRejected` in
   `ContactStatus`; extend `StartChat` (+`processServiceRequests`), `APIRejectContact`/`RejectContact`
   (+`notify`); add `APISendServiceRequest`, `APISendServiceResponse`; add `CRServiceResponse`,
   `CEvtServiceRequest`, `CEvtContactRequestRejected`; `ChatController` `processServiceRequests :: TVar Bool`;
   `rejectionSupported` on `UserContactRequest`; `contactRequest :: Maybe UserContactRequestRef` on
   `Contact` (keep `contactRequestId`).
3. DB migration: `rejection_supported` on `contact_requests`. (`CSRejected` reuses the existing
   `contact_status` text column — no migration.)
4. Parsers in `chatCommandP`.
5. Processing in `Library/Commands.hs` (send request, send response, reasoned reject-contact, start
   flag threading).
6. Event handling in `Library/Subscriber.hs` (SREQ: gate + active-reject when off; REQ: store the
   capability flag; RJCT: decode reason, set contact `CSRejected`, emit event).
7. Store: `getContact_` adds `LEFT JOIN contact_requests` for `rejection_supported`, builds
   `contactRequest`.
8. `View.hs` rendering for the new responses/events.
9. Tests (functional: send request/response, unsupported-instance drop → timeout, reasoned reject +
   requester `CSRejected`, non-DR reasoned reject error).
Scope: Haskell core, `View.hs`, tests. Bot API is autogenerated (no manual mobile/TS work).

## Verified against code (review loop)
- Agent API names/signatures at simplexmq HEAD `ccc1d7f8` (Agent.hs:508–537, Protocol.hs:418–420,
  2202); chat pins that exact tag (cabal.project:24). Agent changes #1/#2 are additive to that.
- `ConnectTarget 'CMContact` = `CTFullContact | CTShortContact (CTName|CTLink) | CTDomain`
  (Types.hs:1796–1802); parser `strP` at Types.hs:1826. Name types `NTContact`/`NTPublicGroup`
  (Commands.hs:4412–4413).
- `AgentInvId = AgentInvId InvitationId` with `StrEncoding` (Types.hs:1699–1704); already the id type
  on `UserContactRequest` (Types.hs:373).
- `withAgent :: (AgentClient -> ExceptT AgentErrorType IO a) -> CM a` (Controller.hs:1762);
  `CRCmdOk {user_ :: Maybe User}` (Controller.hs:813).
- `SREQ`/`REQ` are received in `processContactConnMessage` on the `UserContact` entity
  (Subscriber.hs:1397–1398); events via `toView`. No `RJCT` handler exists.
- Commands are text-parsed (`parseChatCommand = A.parseOnly chatCommandP`, Commands.hs:411), not JSON
  — so a `ConnectTarget 'CMContact` field is fine (parser uses `AConnectTarget`'s `strP` then requires
  `SCMContact`). `ChatDeleteMode {notify :: Bool}` (Controller.hs:1100) is the notify-choice pattern.
- `SREQ invId payload` (Agent.hs:3968) delivers the full request payload, parallel to `REQ … cInfo`
  (:3936/:3965) — the requester's data reaches the service in the payload; `processDirectMessage`
  (Subscriber.hs:441, `RcvDirectMsgConnection conn contact_`) has the `contact_` for RJCT.
- `StartChat` all record-syntax (Controller.hs:351/696, Commands.hs:552/5406/5407);
  `startChatController` callers Core.hs:93 + Commands.hs:555; runtime flag pattern
  `subscriptionMode :: TVar` (Controller.hs:291) via `chatReadVar`/`chatWriteVar`
  (Controller.hs:1647/1655).
- `APIRejectContact` constructed/matched **positionally** (Commands.hs:1432, 2475) — adding a field
  breaks both; parser at 5480; agent call at 1448.
- `ContactRejectionReason` follows `GroupRejectionReason` (Types.hs:968): `StrEncoding` +
  `strToJSON`/`strParseJSON` (string JSON).
- Incoming contact request creates a prepared `Contact` (`REContact ct`, Subscriber.hs:1482/1490)
  via `createOrUpdateContactRequest`; `Contact` has `preparedContact :: Maybe PreparedContact`
  (Types.hs:206), `contactRequestId :: Maybe Int64` (:207); `PreparedContact` at :227; built by
  `toContact` (Store/Direct.hs), read by `getContact` :960 / `getUserContacts` :817.
- `UserContactRequest` (Types.hs:371) ↔ `Contact` link is bidirectional (`contactId_` / `contactRequestId`).
- Rejected-state precedents: `GSMemRejected` (Types.hs:1356), `RSRejected` (Types/Shared.hs:95);
  `ContactStatus` (Types.hs:315) has no rejected today. Requester prepared contact = `Contact` with
  `ConnPrepared` connection (connectViaContact, Commands.hs:3761/3769).

## Amendment: signed service requests

Pairs with the agent-side signing (simplexmq `plans/2026-07-24-signed-service-requests.md`); pin bumped
to `0cc09fe5`. The agent constructs and verifies the Ed25519 signature — chat only carries the keys.

- `APISendServiceRequest` gains `signKey :: Maybe C.PrivateKeyEd25519`; command syntax
  `sign_key=<key>` (string-encoded private key, via the enabled `StrEncoding (PrivateKey Ed25519)`),
  threaded into `sendServiceRequestAsync`. Absent = unsigned (unchanged behaviour).
- `CEvtServiceRequest` gains `signerKey :: Maybe C.PublicKeyEd25519`, taken from the agent's `SREQ`
  (the verified key, `Nothing` when unsigned); `View.hs` renders a `signed by <key>` line.
- An invalid signature arrives as the generic `A_SERVICE ASEBadSignature` agent error — no dedicated
  chat event; the bot pattern-matches the error.
- Docs updated (commands / events / docs types).
- Test: `testSignedServiceRequest` — end-to-end, a signed request shows `signed by <pubStr>` matching
  the sender's key.
