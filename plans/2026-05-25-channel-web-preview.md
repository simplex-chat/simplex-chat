# Channel Web Preview

## Context

SimpleX channels are public - anybody with the link to join and chat relays rebroadcasting the messages can see content. To grow channels, owners need a public web preview (like Telegram's `t.me/s/channelname`) showing the last 50 messages. This lets potential subscribers browse before joining.

The relay already stores all messages in its database. The web preview is a periodic read-and-render loop that writes JSON files served by Caddy, with CORS controlling which domains can embed the preview.

This feature integrates with the `.simplex` namespace (ENS-based names resolving to channel links). A channel's registered domain (`groupDomain`) lives in `PublicGroupAccess` inside `PublicGroupProfile` and is disseminated with the profile. On-chain verification of the domain is deferred until RSLV resolution protocol ships.

## Architecture

```
simplex-chat CLI (--relay --web-json-dir=... --web-base-url=...)
  ├── Main chat loop (existing)
  ├── Relay logic (existing, gated by --relay)
  └── Web preview thread (new, gated by relayWebOptions)
        ├── Periodic: load publishable groups → render JSON → write files
        └── Regenerate Caddy CORS config → caddy reload

Caddy (operator-configured)
  ├── Serves JSON at https://<webDomain>/group/<publicGroupId>.json
  └── Imports generated CORS config file

Channel page (static HTML+JS, hosted by owner or on GitHub)
  ├── Fetches JSON from relay(s) with fallback
  └── Renders messages, shows join button
```

## Data Model Changes

### 1. Extend `PublicGroupProfile` with domain and web access settings

**File:** `src/Simplex/Chat/Types.hs` (line 796)

Current:
```haskell
data PublicGroupProfile = PublicGroupProfile
  { groupType :: GroupType,
    groupLink :: ShortLinkContact,
    publicGroupId :: B64UrlByteString
  }
```

New:
```haskell
data PublicGroupAccess = PublicGroupAccess
  { groupWebPage :: Maybe Text, -- channel's web page URL (adds CORS origin)
    groupDomain :: Maybe Text,  -- domain for this channel (must have link set in domain record in the contract)
    domainWebPage :: Bool,      -- show on the domain's page (e.g. simplexnetwork.org site for simplex TLD domains, or domain site for web domains)
    allowEmbeding :: Bool       -- allow embedding from any origin (CORS: *)
  }

data PublicGroupProfile = PublicGroupProfile
  { groupType :: GroupType,
    groupLink :: ShortLinkContact,
    publicGroupId :: B64UrlByteString,
    publicGroupAccess :: Maybe PublicGroupAccess -- NEW: web preview settings
  }
```

`groupDomain` stores the channel's registered `.simplex` domain name or another supported TLD. It is:
- Set by the owner after registering a name on-chain
- Disseminated to all members via `GroupProfile` (nested in `publicGroup`)
- Used by `simplexnetwork.org/c/<name>` to route to the channel's web preview (for .simplex domain)

JSON instances: TH-derived `$(JQ.deriveJSON defaultJSON ''PublicGroupAccess)`. Existing `$(JQ.deriveJSON defaultJSON ''PublicGroupProfile)` covers the new optional field.

**Migration (SQLite/Postgres):** separate columns, same pattern as `group_type`/`group_link`/`public_group_id`:
```sql
ALTER TABLE group_profiles ADD COLUMN group_web_page TEXT;
ALTER TABLE group_profiles ADD COLUMN group_domain TEXT;
ALTER TABLE group_profiles ADD COLUMN domain_web_page INTEGER;
ALTER TABLE group_profiles ADD COLUMN allow_embedding INTEGER;
ALTER TABLE group_profiles ADD COLUMN group_domain_verified_at TEXT;
```

`group_domain_verified_at` is relay-local verification state (nullable timestamp, NULL = unverified).

**Store changes:**

`src/Simplex/Chat/Store/Shared.hs` line 693 - new constructor alongside `toPublicGroupProfile`:
```haskell
toPublicGroupAccess :: Maybe Text -> Maybe Text -> Maybe BoolInt -> Maybe BoolInt -> Maybe PublicGroupAccess
toPublicGroupAccess groupWebPage groupDomain domainWebPage_ allowEmbeding_
  | isJust groupWebPage || isJust groupDomain || fromBI domainWebPage_ || fromBI allowEmbeding_ =
      Just PublicGroupAccess {groupWebPage, groupDomain, domainWebPage = fromBI domainWebPage_, allowEmbeding = fromBI allowEmbeding_}
  | otherwise = Nothing
  where fromBI = maybe False unBI
```

Extend `toPublicGroupProfile` to accept and pass through `Maybe PublicGroupAccess`.

`GroupInfoRow` type (line 668) gains columns for: `group_web_page`, `group_domain`, `domain_web_page`, `allow_embedding`, `group_domain_verified_at`.

`src/Simplex/Chat/Store/Groups.hs`:
- INSERT (line 367): add all new columns
- SELECT (line 2375): add `gp.group_web_page`, `gp.group_domain`, `gp.domain_web_page`, `gp.allow_embedding`, `gp.group_domain_verified_at`
- UPDATE (line 1922): include new columns in `updateGroupProfile_`

### 2. `RelayCapabilities` record, extend `XGrpRelayAcpt`, new `XGrpRelayCap`

**File:** `src/Simplex/Chat/Protocol.hs`

New record for relay capabilities (extensible for future fields):
```haskell
data RelayCapabilities = RelayCapabilities
  { webDomain :: Maybe Text
  }
```

TH-derived JSON. All fields optional so old relays produce `{}` and new fields are backward compatible.

**`XGrpRelayAcpt`** - carries capabilities at acceptance time:

Current (line 444): `XGrpRelayAcpt :: ShortLinkContact -> ChatMsgEvent 'Json`
New: `XGrpRelayAcpt :: ShortLinkContact -> RelayCapabilities -> ChatMsgEvent 'Json`

Parsing: `XGrpRelayAcpt_ -> XGrpRelayAcpt <$> p "relayLink" <*> (p "relayCap" <|> pure defaultRelayCap)`
Encoding: `XGrpRelayAcpt relayLink cap -> o ["relayLink" .= relayLink, "relayCap" .= cap]`
Backward compatible: old relays omit `relayCap`, parsed as default (all `Nothing`).

**`XGrpRelayCap`** - new message for ongoing capability updates:

```haskell
XGrpRelayCap :: RelayCapabilities -> ChatMsgEvent 'Json
```

Tag: `"x.grp.relay.cap"`
Parsing: `XGrpRelayCap_ -> XGrpRelayCap <$> p "relayCap"`
Encoding: `XGrpRelayCap cap -> o ["relayCap" .= cap]`

Sent by relay to owner only when capabilities change (not periodic). Relay detects change by comparing current config against persisted state on startup.

### 3. Store `webDomain` per relay

**File:** `src/Simplex/Chat/Operators.hs` (line 278)

Current:
```haskell
data GroupRelay = GroupRelay
  { groupRelayId :: Int64,
    groupMemberId :: Int64,
    userChatRelay :: UserChatRelay,
    relayStatus :: RelayStatus,
    relayLink :: Maybe ShortLinkContact
  }
```

Add: `relayCap :: Maybe RelayCapabilities`

Stored as separate columns (same pattern as `PublicGroupAccess`):
**Migration:** `ALTER TABLE group_relays ADD COLUMN base_web_url TEXT`

`relayCap` constructed from columns: `Just RelayCapabilities {webDomain}` when any capability column is non-NULL, `Nothing` otherwise.

**Handlers in `src/Simplex/Chat/Library/Subscriber.hs`:**
- `XGrpRelayAcpt` (line 770): store `RelayCapabilities` in relay record on acceptance
- `XGrpRelayCap` (new handler): update `RelayCapabilities` in relay record; only accepted from relay members (`isRelay m`), owner receives

**Relay-side persistence:** relay persists its current `RelayCapabilities` (derived from `RelayWebOptions`) so it can detect config changes on restart. On startup, if persisted capabilities differ from config, relay sends `XGrpRelayCap` to all group owners it serves.

### 4. CLI options for web preview

**File:** `src/Simplex/Chat/Options.hs`

New record bundling all web preview options:
```haskell
data RelayWebOptions = RelayWebOptions
  { webJsonDir :: FilePath,       -- --web-json-dir: where to write JSON files
    webDomain :: Text,           -- --web-base-url: public URL prefix (sent in XGrpRelayAcpt)
    webCorsFile :: FilePath,      -- --web-cors-file: generated Caddy CORS config path
    webUpdateInterval :: Int      -- --web-update-interval: seconds (default 300)
  }
```

Add as a proper field in `CoreChatOpts`:
```haskell
data CoreChatOpts = CoreChatOpts
  { ...existing...,
    relayWebOptions :: Maybe RelayWebOptions
  }
```

Parsed from CLI: when `--web-json-dir` is provided, all other `--web-*` flags are required. `Nothing` when no web preview flags are set. Only meaningful when `--relay` is also set.

### 5. Web preview thread startup

**File:** `src/Simplex/Chat/Core.hs` (line 74)

Current:
```haskell
runSimplexChat ... = do
    a1 <- runReaderT (startChatController True True) cc
    when (chatRelay && not testView) $ askCreateRelayAddress cc u
    forM_ (postStartHook chatHooks) ($ cc)
    a2 <- async $ chat u cc
    waitEither_ a1 a2
```

Add web preview thread as a third async when config is present:
```haskell
runSimplexChat ... = do
    a1 <- runReaderT (startChatController True True) cc
    when (chatRelay && not testView) $ askCreateRelayAddress cc u
    forM_ (postStartHook chatHooks) ($ cc)
    a2 <- async $ chat u cc
    case relayWebOptions coreOptions of
      Nothing -> waitEither_ a1 a2
      Just webOpts -> do
        a3 <- async $ webPreviewThread webOpts cc
        void $ waitAnyCancel [a1, a2, a3]
```

## New Types for JSON Serialization

**File:** new module `src/Simplex/Chat/Web/Preview.hs`

### Reuse as-is (existing ToJSON instances)

- `GroupProfile` (Types.hs:803) - channel metadata (displayName, fullName, shortDescr, description, image, publicGroup incl. groupDomain)
- `MsgContent` (Protocol.hs:689) - tagged union: MCText, MCLink, MCImage, MCVideo, etc.
- `LinkPreview` (Protocol.hs:256) - `{uri, title, description, image, content}`
- `FormattedText` / `MarkdownList` (Markdown.hs:133/139) - parsed markdown
- `QuotedMsg` / `MsgRef` (Protocol.hs:589) - quoted message context
- `MsgMentions` = `Map MemberName CIMention` (Messages.hs:264)
- `CIMention` (Messages.hs:272) - `{memberId, memberRef}`
- `CIReactionCount` (Messages.hs:338) - `{reaction, userReacted, totalReacted}`

### New types

```haskell
data WebFileInfo = WebFileInfo
  { fileName :: String,
    fileSize :: Integer
  }

data WebMemberProfile = WebMemberProfile
  { memberId :: MemberId,
    displayName :: Text,
    image :: Maybe ImageData
  }

data WebMessage = WebMessage
  { sender :: Maybe MemberId,         -- Nothing for CIChannelRcv (forwarded-from-channel)
    ts :: UTCTime,
    content :: MsgContent,
    formattedText :: Maybe MarkdownList,
    file :: Maybe WebFileInfo,
    quote :: Maybe QuotedMsg,
    mentions :: Map MemberName CIMention,
    reactions :: [CIReactionCount],
    forwarded :: Maybe CIForwardedFrom,
    edited :: Bool
  }

data WebChannelPreview = WebChannelPreview
  { channel :: GroupProfile,       -- NOTE: render loop strips groupDomain until verified
    subscriberCount :: Maybe Int,
    members :: [WebMemberProfile],
    messages :: [WebMessage],
    updatedAt :: UTCTime
  }
```

TH-derived JSON for `WebFileInfo`, `WebMemberProfile`, `WebMessage`, `WebChannelPreview`.

## Render Loop

**File:** new module `src/Simplex/Chat/Web.hs`

Pattern from directory service's `updateListingsThread_` (Service.hs:185-194).

```haskell
webPreviewThread :: RelayWebOptions -> ChatController -> IO ()
webPreviewThread opts cc = forever $ do
    u_ <- readTVarIO $ currentUser cc
    forM_ u_ $ \user -> do
      groups <- getWebPublishGroups cc user
      corsEntries <- forM groups $ \gInfo -> do
        renderGroupPreview opts cc user gInfo
        pure (corsEntry gInfo)
      writeCorsConfig opts corsEntries
    threadDelay (webUpdateInterval opts * 1_000_000)
```

### Loading groups

New store function `getWebPublishGroups`:
```sql
SELECT ... FROM groups g
JOIN group_profiles gp ON g.group_profile_id = gp.group_profile_id
WHERE gp.group_web_page IS NOT NULL
  AND g.user_id = ?
```

Returns `[GroupInfo]`. For each, call `getGroupChat` with `CPLast 50` (Store/Messages.hs:1436) to get chat items.

### Converting CChatItem to WebMessage

For each `CChatItem SMDRcv (ChatItem {chatDir, meta, content, mentions, formattedText, quotedItem, reactions, file})`:

1. **Skip if:**
   - `itemDeleted meta` is `Just _`
   - `itemTimed meta` is `Just _`
   - `content` is not `CIRcvMsgContent mc` (skip `CIRcvGroupEvent`, `CIRcvIntegrityError`, etc.)
   - `mc` is `MCReport` or `MCUnknown`

2. **Extract sender:**
   - `CIGroupRcv member` -> `Just (memberId member)`, collect member into profiles array
   - `CIChannelRcv` -> `Nothing` (channel-forwarded message, no individual sender)

3. **Extract file info:**
   - `file :: Maybe (CIFile 'MDRcv)` has `fileName :: String`, `fileSize :: Integer`
   - Strip `fileSource`, `fileStatus`, `fileProtocol` (download metadata irrelevant for web)

4. **Build WebMessage:**
   ```haskell
   WebMessage
     { sender = senderMemberId
     , ts = itemTs meta
     , content = mc
     , formattedText = formattedText
     , file = (\f -> WebFileInfo (fileName f) (fileSize f)) <$> file
     , quote = quotedItem  -- QuotedMsg reused directly
     , mentions = mentions
     , reactions = reactions
     , forwarded = itemForwarded meta
     , edited = itemEdited meta
     }
   ```

5. **Collect unique senders** into `[WebMemberProfile]` from `GroupMember` records in `CIGroupRcv`.

Also include `CIGroupSnd` items (relay's own sent messages, if any - unlikely but possible for admin announcements).

### Filtering unverified domains

Before serializing, the render loop strips `groupDomain` from the `PublicGroupAccess` included in the profile when not verified:

```haskell
stripUnverifiedDomain :: Maybe UTCTime -> GroupProfile -> GroupProfile
stripUnverifiedDomain verifiedAt gp = case verifiedAt of
  Just _ -> gp  -- domain verified, include as-is
  Nothing -> gp {publicGroup = clearDomain <$> publicGroup gp}
  where
    clearDomain pgp = pgp {publicGroupAccess = clearAccess <$> publicGroupAccess pgp}
    clearAccess acc = acc {groupDomain = ""}  -- or strip the access record entirely
```

The `group_domain_verified_at` timestamp is loaded alongside the group info. Until RSLV ships, this column is always NULL, so all domains are stripped from web export.

`domainWebPage` in CORS config is also gated on verified domain - unverified means no domain-site origin in CORS.

### Writing JSON

- Serialize `WebChannelPreview` to JSON via `Data.Aeson.encode`
- Write atomically (write to temp, rename) to `<webJsonDir>/<publicGroupId>.json`
- `publicGroupId` from `PublicGroupProfile` (base64url-encoded, existing field)

### Generating Caddy CORS config

Write a single file with Caddy `map` directive:

```caddy
map {path} {cors_origin} {
    /<publicGroupId1>.json "https://owner-domain.com"
    /<publicGroupId2>.json "*"
    default ""
}
header /*.json Access-Control-Allow-Origin {cors_origin}
header /*.json Access-Control-Allow-Methods "GET, OPTIONS"
```

CORS origin derivation from `PublicGroupAccess`:
- `allowEmbeding = True` -> `*`
- `groupWebPage = Just url` -> extract origin from URL (+ domain site origin if `domainWebPage` and domain verified)
- `groupWebPage = Nothing, domainWebPage = True` -> domain site origin only (when domain is verified)
- No web page, no embedding, no domain page -> omit from config

After writing, run `caddy reload` if file content changed (compare hash before/after).

## Namespace Integration

`groupDomain` ships now in the profile (inside `PublicGroupAccess`). What's deferred is on-chain verification (RSLV protocol).

### What ships now

1. **`groupDomain :: Text` in `PublicGroupAccess`** - owner sets the registered domain, disseminated to all members
2. **`domainWebPage :: Bool` in `PublicGroupAccess`** - flag stored but has no effect until domain is verified
3. **Relay strips `groupDomain` from web export** - no verification means domain is cleared in JSON, no domain-site CORS origin

### What ships with RSLV

1. **RSLV protocol** - relay queries name servers via SMP proxy to verify domain ownership
2. **`domainWebPage` becomes functional** - enables domain-site hosting (e.g. `simplexnetwork.org/c/<name>`) for verified domains
3. **In-app resolution** - `#name` markdown (already parsed by namespace branch) resolves and connects

### Verification flow (relay-side)

When owner updates profile with `groupDomain`:

1. **Trigger:** Relay receives profile update on owner's connection containing `groupDomain` field
2. **Initiate:** Relay sends `RSLV <namehash>` through SMP proxy (async, on the same owner connection context)
3. **Pending state:** `group_domain_verified_at = NULL` in DB. Web export excludes domain while pending.
4. **Resolution arrives:** `NAME <record>` agent event arrives on the owner's connection (continuation bound to the connection that sent the profile update)
5. **Verify:** Check if `channelLinks` in the NAME response includes this group's `groupLink`
6. **Store result:** Set `group_domain_verified_at = <current_time>` on success, leave NULL on failure
7. **Effect:** Web render loop includes domain in JSON and enables domain-site CORS only when `group_domain_verified_at IS NOT NULL`

Re-verification: periodic (e.g. daily or on each web update cycle) to catch expired/transferred domains. Clear `group_domain_verified_at` when re-verification fails.

### What the namespace branch already provides

- `SimplexNameInfo {nameType, namespace, domain, subDomain}` in Markdown.hs
- `SimplexName` variant in `Format` ADT
- Parser for `#name` / `#name.simplex` / `:name.simplex` syntax
- Forward-compatibility alerts in Kotlin/Swift UI (shows "requires newer app" until resolution is implemented)

## UI Changes (Kotlin/Swift)

### Kotlin types

**File:** `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`

```kotlin
@Serializable
data class PublicGroupAccess(
    val groupWebPage: String? = null,
    val groupDomain: String? = null,
    val domainWebPage: Boolean = false,
    val allowEmbeding: Boolean = false
)

// Extend existing PublicGroupProfile (currently at line 2213):
@Serializable
data class PublicGroupProfile(
    val groupType: GroupType,
    val groupLink: String,
    val publicGroupId: String,
    val publicGroupAccess: PublicGroupAccess? = null  // NEW
)

@Serializable
data class RelayCapabilities(
    val webDomain: String? = null
)

// Extend existing GroupRelay:
@Serializable
data class GroupRelay(
    ...existing fields...,
    val relayCap: RelayCapabilities? = null  // NEW
)
```

### Owner: Channel info page

**File:** `GroupChatInfoView.kt` (around line 604-606)

After existing `ChannelLinkButton(manageGroupLink)`:
```kotlin
ChannelWebPageButton(openChannelWebPage)  // owner only
```

New nav destination opens `ChannelWebPageView`.

### Owner: Channel web page screen

**File:** new `apps/multiplatform/.../views/chat/group/ChannelWebPageView.kt`

- Text field: web page URL (`groupWebPage`)
- Text field: domain (`groupDomain`)
- Toggle: allow embedding (`allowEmbeding`)
- Toggle: show on domain's page (`domainWebPage`) - stored but inert until RSLV ships
- Section: embed snippet (read-only, auto-generated from relay `webDomain` values + `publicGroupId`)
- Save button -> `apiUpdateGroup` with updated `GroupProfile`

### Subscriber: Channel info page

In the top section (around line 607-614), after channel link QR:
```kotlin
val webPageUrl = groupInfo.groupProfile.publicGroup?.publicGroupAccess?.groupWebPage
if (webPageUrl != null) {
    WebPageLinkRow(webPageUrl)  // clickable, opens browser
}
```

## Build Configuration

Web preview code compiles into the main `simplex-chat` library (not conditional). The thread only starts when `relayWebOptions` is set in `CoreChatOpts`. Mobile apps never set this.

No cabal flag needed - the thread startup is gated by `Maybe RelayWebOptions` at runtime (same pattern as `chatRelay` gating relay behavior).

## Caddy Setup (operator documentation)

Main Caddyfile (operator writes once):
```caddy
relay.example.com {
    import /etc/caddy/simplex-cors.conf
    handle /preview/* {
        root * /var/lib/simplex/web/preview
        file_server
    }
}
```

Relay CLI invocation:
```
simplex-chat --relay \
  --web-json-dir /var/lib/simplex/web/preview \
  --web-base-url https://relay.example.com/preview \
  --web-cors-file /etc/caddy/simplex-cors.conf \
  --web-update-interval 300
```

## Channel Page and Embed Code

### Embed snippet (shown to owner)

The "Channel web page" screen auto-generates this from the channel's relay `webDomain` values and `publicGroupId`. Owner copies it into their page:

```html
<div id="simplex-channel"
  data-channel-id="<publicGroupId>"
  data-relays="<webDomain1>,<webDomain2>">
</div>
<script src="https://simplex.chat/channel-preview.js"></script>
```

Example with real values:
```html
<div id="simplex-channel"
  data-channel-id="a1b2c3d4"
  data-relays="https://relay1.example.com/preview,https://relay2.example.com/preview">
</div>
<script src="https://simplex.chat/channel-preview.js"></script>
```

The script fetches `<relay>/a1b2c3d4.json`, renders the preview into the `div`. Tries relays in order, falls back on failure. The owner's domain must match the CORS origin configured by the relay (derived from `groupWebPage`), or `allowEmbeding` must be `True` for `*`.

For iframe embedding (when allowed), the snippet is simpler - just an iframe pointing to the owner's hosted channel page.

### Channel page (static JS)

Separate repo or folder. `channel-preview.js` + minimal CSS:
- Reads config from `data-` attributes on the container div
- Fetches JSON from relays with fallback (try first, fall back to second)
- Renders: channel header (name, avatar, description, subscriber count), message list (text with FormattedText markdown, link previews, file indicators, reactions, quotes)
- Join button: `simplex://` deep link on mobile, QR code on desktop
- Reuses directory page's markdown rendering approach

## Files to Create/Modify

### New files
- `src/Simplex/Chat/Web/Preview.hs` - types: `WebChannelPreview`, `WebMessage`, `WebFileInfo`, `WebMemberProfile`
- `src/Simplex/Chat/Web.hs` - render loop, JSON writing, Caddy config generation
- `apps/multiplatform/.../views/chat/group/ChannelWebPageView.kt`
- `apps/ios/Shared/Views/Chat/Group/ChannelWebPageView.swift`
- Migration files (SQLite + Postgres): `group_web_page`, `group_domain`, `domain_web_page`, `allow_embedding`, `group_domain_verified_at` in group_profiles; `base_web_url` in group_relays
- Channel page static site (separate repo/folder)

### Modified files
- `src/Simplex/Chat/Types.hs` - `PublicGroupAccess` type, extend `PublicGroupProfile` with `publicGroupAccess`
- `src/Simplex/Chat/Protocol.hs` - `RelayCapabilities` record, extend `XGrpRelayAcpt`, add `XGrpRelayCap`
- `src/Simplex/Chat/Options.hs` - `RelayWebOptions` record, `relayWebOptions :: Maybe RelayWebOptions` in `CoreChatOpts`
- `src/Simplex/Chat/Core.hs` - start web preview thread in `runSimplexChat`
- `src/Simplex/Chat/Operators.hs` - `webDomain` in `GroupRelay`
- `src/Simplex/Chat/Store/Groups.hs` - read/write `PublicGroupAccess` columns; `getWebPublishGroups`
- `src/Simplex/Chat/Store/Shared.hs` - `toPublicGroupAccess`, extend `toPublicGroupProfile` and `GroupInfoRow`
- `src/Simplex/Chat/Library/Subscriber.hs` - handle `RelayCapabilities` in `XGrpRelayAcpt` and `XGrpRelayCap`
- `apps/multiplatform/.../model/ChatModel.kt` - `PublicGroupAccess`, `RelayCapabilities`, `PublicGroupProfile.publicGroupAccess`, `GroupRelay.relayCap`
- `apps/multiplatform/.../views/chat/group/GroupChatInfoView.kt` - nav link for web page
- `simplex-chat.cabal` - add `Simplex.Chat.Web.Preview`, `Simplex.Chat.Web` to exposed-modules

## Implementation Order

1. **Data model** - `PublicGroupAccess` in `PublicGroupProfile`, migrations (separate columns), store functions
2. **Protocol** - `RelayCapabilities`, extend `XGrpRelayAcpt`, add `XGrpRelayCap`, handlers in Subscriber.hs
3. **CLI options** - `RelayWebOptions` record, `relayWebOptions` field in `CoreChatOpts`
4. **Web types** - `WebChannelPreview`, `WebMessage`, etc. in new module
5. **Render loop** - thread startup in Core.hs, periodic JSON generation, Caddy config
6. **UI (owner)** - "Channel web page" settings screen
7. **UI (subscriber)** - web page link in channel info
8. **Channel page** - static HTML+JS template
9. **Documentation** - operator setup guide

## Verification

1. **Build**: `cabal build simplex-chat` with new modules compiles
2. **Unit test**: serialize `WebChannelPreview` with sample data, verify JSON matches expected structure
3. **Integration test**: create channel with `publicGroupAccess` set, run relay with `--web-json-dir`, verify JSON file appears at correct path with correct content
4. **CORS test**: verify generated config produces correct `Access-Control-Allow-Origin` for configured domains
5. **UI test**: owner can set web page URL and domain, see embed snippet; subscriber sees clickable link
6. **Channel page test**: serve static page locally against relay's JSON, verify rendering
7. **Domain stripping test**: set `groupDomain` on a channel, verify it is stripped from web export JSON (unverified, `group_domain_verified_at IS NULL`)
