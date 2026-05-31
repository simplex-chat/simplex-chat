# SimpleX Chat Service Framework

Services are bot contacts hidden from the user. The app sends commands and receives responses programmatically. Messages are stored and visible in dev tools for auditing.

## Services

- Badge - supporter badge credential issuance
- Directory - group/channel discovery (existing bot, to be migrated)
- Name resolution - resolve `#channel.simplex` / `@contact.simplex` to connection links
- Later: telemetry, LLM, translation, notification server migration

## How it works

### Synchronous RPC via TMVar

The relay test code (Commands.hs:1584-1601, Subscriber.hs:423-446) already does this:

1. Handler sends command as text message via `sendDirectContactMessage` (Internal.hs:1913), gets back `SndMessage {sharedMsgId}` and the connection's `AgentConnId`
2. Creates empty `TMVar`, stores in `chatServiceCalls :: TMap (AgentConnId, SharedMsgId) (TMVar Text)` (new field on `ChatController`, same pattern as `chatRelayTests`)
3. Blocks on `timeout 30s $ atomically $ takeTMVar`
4. Subscriber receives reply with `quotedSharedMsgId` referencing the command, looks up `chatServiceCalls` by `(agentConnId, quotedSharedMsgId)`, fills TMVar via `putTMVar`
5. Handler unblocks, parses response, returns `CRServiceResponse`

Keyed by `(AgentConnId, SharedMsgId)` - globally unique, supports concurrent commands to the same service.

For idempotent services (directory, names): if user retries the same command while a previous attempt is pending, key by `(serviceType, commandHash)` so a response to attempt 1 can satisfy attempt 2's TMVar. The answer to the same query is the same regardless of which attempt triggered it. If response arrives after timeout, it's dropped.

### Command/response flow through the app

Kotlin/Swift UI calls `sendCmd(CC.APICallService(...))` (same FFI path as all commands, Core.kt:27). The string `/_service directory /search ...` goes to Haskell via `chatSendCmdRetry`. Each FFI call runs on its own thread (`Dispatchers.IO` on Kotlin, async task on Swift). Haskell blocks that thread on the TMVar until the service replies (up to 30s). Other API calls run concurrently on other threads - no global blocking. Response JSON comes back synchronously. Kotlin/Swift deserializes `CR.CRServiceResponse`.

Send errors: `sendDirectContactMessage` can throw synchronous errors (contact not ready, connection disabled). Async errors - `MERR` (delivery failure, line 478), `ERR` (line 484), connection failures - arrive via Subscriber and should fill the TMVar with an error so the handler fails fast. `DOWN` events should NOT fill with error - the connection may recover within the timeout.

### Badge service: custom APIs with persistence

Badge does NOT use `APICallService`. It has its own commands (`APIIssueBadge`, `APIRenewBadge`, `APIRedeemBadge`, `APIBadgeStripeLink`) that use the same underlying plumbing (send message to service contact, receive reply via Subscriber) but add persistence:

- Before sending, persist `(ms, receipt, status=pending)` in DB
- If response arrives after timeout or restart, Subscriber matches it to pending request, stores credential, notifies UI via event
- On app restart, check for pending requests and re-send commands
- Server must be idempotent: same receipt re-sent returns same credential, not an error
- Stripe flow: `/stripe_link` gets immediate reply with URL, credential arrives as delayed reply to same command after payment

## Types

### Haskell

Per-service types are plain ADTs in their own modules:

```haskell
-- Simplex.Chat.Service.Badge
data BadgeCommand
  = BCIssue ByteString ByteString       -- ms receipt (Apple/Google)
  | BCRenew ByteString ByteString       -- ms receipt
  | BCRedeem Text                        -- redemption code
  | BCStripeLink ByteString              -- ms -> returns checkout URL

data BadgeResponse
  = BRCredential ByteString UTCTime Int  -- signature expiry level
  | BRStripeLink Text                    -- checkout URL
  | BRError Text

-- Simplex.Chat.Service.Directory
data DirectoryCommand = DCSearch Text

data DirectoryResponse = DRResult [GroupInfo]

-- Simplex.Chat.Service.Names
data NamesCommand = NCResolve SimplexNameInfo

data NamesResponse
  = NRResolved AConnectionLink
  | NRNotFound
  | NRError Text
```

Top-level GADT wraps per-service types with the type tag:

```haskell
data ServiceCommand (s :: ServiceType) where
  SCBadge :: BadgeCommand -> ServiceCommand 'STBadge
  SCDirectory :: DirectoryCommand -> ServiceCommand 'STDirectory
  SCNames :: NamesCommand -> ServiceCommand 'STNames

data ServiceResponse (s :: ServiceType) where
  SRBadge :: BadgeResponse -> ServiceResponse 'STBadge
  SRDirectory :: DirectoryResponse -> ServiceResponse 'STDirectory
  SRNames :: NamesResponse -> ServiceResponse 'STNames

data AServiceCommand = forall s. ServiceTypeI s => ASC (SServiceType s) (ServiceCommand s)
data AServiceResponse = forall s. ServiceTypeI s => ASR (SServiceType s) (ServiceResponse s)

-- ChatCommand (Controller.hs ~line 480)
| APICallService UserId AServiceCommand

-- ChatResponse (Controller.hs ~line 764)
| CRServiceResponse User AServiceResponse
```

### Kotlin

```kotlin
// sealed class CC (SimpleXAPI.kt:3643)
class APICallService(val userId: Long, val serviceType: ServiceType, val command: ServiceCmd): CC()

// ServiceType enum
enum class ServiceType { BADGE, DIRECTORY, NAMES }

// ServiceCmd sealed per service type
sealed class ServiceCmd {
  sealed class Badge : ServiceCmd() {
    class Issue(val ms: String, val receipt: String) : Badge()
    class Renew(val ms: String, val receipt: String) : Badge()
    class Redeem(val code: String) : Badge()
    class StripeLink(val ms: String) : Badge()
  }
  sealed class Names : ServiceCmd() {
    class Resolve(val nameInfo: SimplexNameInfo) : Names()
  }
}

// sealed class CR (SimpleXAPI.kt:6347)
@Serializable @SerialName("serviceResponse")
class CRServiceResponse(val user: UserLike, val serviceType: ServiceType, val response: ServiceResp): CR()
```

### Command string format

`/_service badge /issue <base64_ms> <base64_receipt>`

Parser in Commands.hs extracts service type, dispatches to per-service parser:

```haskell
"/_service " *> do
  serviceType_ <* A.space >>= \case
    SSTBadge -> ASC SSTBadge <$> badgeCommandP
    SSTDirectory -> ASC SSTDirectory <$> directoryCommandP
    SSTNames -> ASC SSTNames <$> namesCommandP
```

where `serviceType_` parses `"badge"` / `"directory"` / `"names"` into the singleton.

### Wire format (messages between app and service bot)

Commands sent as text: `/<command> [args...]`

Responses as YAML discriminated union (top-level key is the tag):

```yaml
credential:
  signature: ABase64=
  expiry: 2026-07-31T23:59:59Z
  level: 1
```

```yaml
stripe_link:
  url: https://checkout.stripe.com/c/pay/...
```

```yaml
error:
  type: receipt_already_used
  message: This receipt has already been redeemed
```

Correlation: service responds with reply-to referencing the command message.

## Service contacts

- Service declares `peerType = CPTBot` in its profile (existing field)
- Local `connService :: Bool` flag on `Contact` record (Types.hs). Set when connecting to a known service address. Filters from chat list. Not in the protocol.
- Messages stored like regular chat items - audit trail in dev tools

## Connection

The `APICallService` handler manages connection itself, same as the relay test (Commands.hs:1581-1593) which connects and waits for a response all within one handler. If the service isn't connected, the handler connects via the agent, waits for the connection to be established (TMVar filled by Subscriber on `ContactConnected`), then sends the command and waits for the reply. All within one blocking FFI call.

1. UI calls `/_service badge /issue ...`
2. Haskell handler checks if service contact exists and is connected
3. If not connected: connects, waits for connection, then sends command
4. If connected: sends command directly
5. Blocks on TMVar for reply, returns `CRServiceResponse`
6. Connection stays open for future calls

Service chat is configured with 1-week message retention (existing `chatSettings.ttl` property). Messages auto-delete after a week.

Services can send multiple replies to the same command (same `quotedSharedMsgId`). First reply fills TMVar (synchronous return to caller). Subsequent replies are matched by the Subscriber via pending requests in DB. Example: `/stripe_link` gets an immediate reply with the checkout URL, then a delayed reply with the credential after payment.

Unsolicited messages from services (no `quotedSharedMsgId`) are stored as chat items. Future: specific service event types could be surfaced via a `CR` variant.

Service addresses: badge is hardcoded (not configurable), directory and names are user-configurable (user can point to different providers). Stored in app preferences alongside other server settings.

## Phases

1. Framework: types, `APICallService`/`CRServiceResponse`, TMVar plumbing, `connService` flag, chat list filtering
2. Badge service (first on the framework)
3. Directory migration
4. Name resolution
5. User-configurable addresses in settings UI
