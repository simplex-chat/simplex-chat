# Public Namespaces for SimpleX Network

## Motivation

SimpleX has no user identifiers - users exchange invitation links out-of-band to connect. Short links help but are unmemorable. Public namespaces map human-readable names to SimpleX addresses.

Names also solve censorship at two levels. A short link is controlled by one SMP router - that router can delete it. An on-chain name can't be deleted by any router. If the link is removed, the owner points the name to a new link on a different router. At the network level, links can be URL-filtered, but names resolve through SMP proxy chains - censoring a name requires controlling all resolvers the user can reach.

DNS-based naming is vulnerable to domain seizure and requires WHOIS entries. Blockchains provide censorship-resistant globally unique names.

## Product requirements

### MVP

- **Plain names**: globally unique (e.g., `simplex`, `privacy`, `my channel`). Allowed: letters (Latin, Cyrillic, Arabic, CJK, Kana), digits, hyphens, spaces.
- **Name rules**:
  - Must start with a letter.
  - No dots or punctuation.
  - No digits-only in lookup keys.
  - No leading/trailing/consecutive spaces or hyphens. Rejected at registration, not silently normalized.
  - Hyphens and spaces equivalent in lookup key: `my-channel` = `my channel` = `mychannel`.
- **Display name and lookup key**: owner chooses display name (e.g., "My Channel"). Lookup key derived by lowercasing, stripping spaces/hyphens, collapsing Unicode confusables. `MyChannel`, `my channel`, `My-Channel` all produce lookup key `mychannel`. Once taken, no variant can register. Owner must be able to change display name as long as lookup key does not change. `mychannel` can become `My Channel`.
- **Two address types**: each name stores channel links (set) and contact links (set). Client uses the first; set provides forward-compatible redundancy. Either can be empty.
- **Optional metadata**: admin SimpleX address, admin email.
- **Registration**: commit-reveal to prevent frontrunning. Length-based ETH pricing. Annual renewal. Dutch auction on expiry.
- **Launch gating**: requires SimpleX test NFT. Up to 5 paid + 5 test names per holder. Test names free, auto-removed after 3 months, use `testnet` namespace.
- **Reserved names**: common verticals (books, games, music, movies, news, etc.) reserved for community-operated channels managed by SimpleX Network Consortium.
- Only 7+ character names can be registered during "launch phase".
- **Resolution**: client queries two independent name servers (Ethereum light clients) via two SMP proxies. Agreement = trusted. Disagreement = warning.
- **Double resolution**: name -> short link (on-chain), short link -> connection data (existing protocol).
- **Verification**: if on-chain link matches profile address, name is verified. Manual "verify" button + optional auto-verify on profile open.
- **Markdown**: `#name`, `#'my name'`, `#namespace:name` (e.g., `#testnet:myname`). In CLI, `#` is local in group commands, global in `/c` and message bodies.
- **Search**: `#name` auto-resolves. Disable in "More privacy" settings.
- **Router role**: `names` added to `ServerRoles`. Not all routers support it.
- **Contract**: ENS fork on Ethereum mainnet. ETH payment. Upgradeable.

### Post-MVP

- **Domain names**: dot-separated, requiring DNS domain ownership proof. Dots are prohibited in MVP.
- **Multiple links**: redundant entries per name. Forward-compatible schema in MVP where practical.
- **Contact syntax**: `:name`, `:'my name'`. Same namespace, different link type. - this should be supported in MVP parser, there is no cost. I think no cost to allow resolving contact addresses too. The only cost is UI support.
- **Community Credits**: replace ETH for private registration.
- **Unicode expansion**: add scripts as user base grows.

## Part 1: Blockchain contract

### Overview

ENS fork on Ethereum mainnet. Retains commit-reveal, pricing, expiry, Dutch auction. Compatible with ENS dApp. Upgradeable. All irrelevant ENS features removed.

### Contract state

```
Name record:
  displayName    : string       -- "My Channel"
  lookupKey      : string       -- "mychannel"
  owner          : address
  channelLinks   : string[]
  contactLinks   : string[]
  adminAddress   : string       -- optional
  adminEmail     : string       -- optional
  expiry         : uint256
  isTest         : bool

Global state:
  reservedNames  : mapping(string => bool)
  testNFT        : address
  registrationLimit : uint8     -- 5
  testLimit      : uint8        -- 5
```

There must be maps to track names by owner, but specific contract design should be based on ENS.

### Name normalization

Lookup key determines uniqueness.

Derivation:
1. Lowercase
2. Strip hyphens
3. Strip whitespace
4. Map Cyrillic look-alikes to Latin equivalents (~33 chars: а->a, в->b, е->e, etc.) and accented Latin to base Latin (é->e, ü->u, etc.). Mapping enforced in contract and dApp. SimpleX client only resolves.

Stored as plain string, not hashed.

Display name validation:
1. Starts with letter
2. No leading/trailing spaces or hyphens
3. No consecutive spaces or hyphens
4. No dots or punctuation
5. Not digits-only or digits-plus-hyphens-only
6. Single-script (digits, spaces, hyphens are script-neutral)

### Allowed characters

- Latin (including accented), Cyrillic, Arabic, CJK, Kana
- Digits (0-9)
- Space and hyphen (equivalent in lookup key)

### Registration flow

1. NFT check
2. Limit check (5 paid / 5 test)
3. `commit(hash(lookupKey, owner, secret))`
4. Wait (min 1 minute)
5. `reveal(displayName, owner, secret)` + ETH (zero for test)
6. Validate: well-formed, not taken, not reserved, fee covered
7. Store record

### Pricing

Annual fees by lookup key length:

| Length | Fee |
|---|---|
| 7+ | base |
| 6 | 4x |
| 5 | 16x |
| 4 | 64x |
| 3 | 256x |

Test names: free, expire after 3 months.

### Renewal and expiry

Annual renewal. Grace period, then Dutch auction decaying to base price.

### Updates

Owner can update links, admin address, admin email. Display name immutable. Transfer follows ENS mechanics.

### Reserved names

List for community channels (e.g., `books`, `games`, `music`, `news`):
- Not registrable by users
- Revenue shared with network

### Retained ENS features

- **Resolver pattern**: registry maps name -> (owner, resolver). A SimpleX Resolver contract stores channel links, contact links, admin fields. Allows future extensibility without registry changes.
- **DNS/DNSSEC**: on-chain verification of DNS domain ownership. Enables post-MVP domain name import for signed domains. Oracle path for unsigned domains can be added later.
- **Multicoin address records**: BTC/ETH/XMR donation addresses per name. Subscribers see donation options from name resolution.
- **Text records**: generic key-value store for future metadata without contract upgrades.
- **Reverse resolution**: link -> name lookup. Enables verification and discovery (client can check if a channel has a registered name).
- **Subdomain registrar**: owner of `acme` can create `acme/support`, `acme/sales` without additional on-chain registration. Syntax uses `/` not `.` to distinguish from DNS domains.

### Removed ENS features

- Avatar, contenthash (IPFS pointers) - not needed.
- ENS-specific integrations (e.g., `.eth` TLD infrastructure).

### Governance

SimpleX Chat during testing and launch phases, migration to SimpleX Network Consortium.

## Part 2: SMP protocol extension

### New router role

```haskell
data ServerRoles = ServerRoles
  { storage :: Bool,
    proxy :: Bool,
    names :: Bool
  }
```

Name-capable routers run an Ethereum light client.

### Resolution protocol

Uses existing SMP proxy infrastructure. Client sends queries through a proxy, not directly to name servers.

#### Commands

```
Client -> Proxy -> Name Server:
  RSLV <lookup_key>

Name Server -> Proxy -> Client:
  NAME <name_record>
  ERR AUTH
```

Forwarded via `PRXY`/`PFWD`/`RRES` mechanism.

#### Two-operator resolution

```
Client -> Proxy A (Op 1) -> Name Server X (Op 1)
Client -> Proxy B (Op 2) -> Name Server Y (Op 2)
```

Both read same Ethereum state.

- Agree: trusted
- Disagree: warn, don't use
- One fails: retry with another server or show single result with reduced trust

Proxy sees client IP and session, but not query. Name server sees query, not client IP or session.

#### Name server implementation

1. Runs Ethereum light client (e.g., Helios) tracking SNRC
2. Receives `RSLV` via SMP proxy
3. Returns record from local state

State proofs can be added post-MVP.

#### Configuration

```haskell
data NamesConfig = NamesConfig
  { ethereumEndpoint :: String,
    snrcAddress :: EthAddress,
    cacheSeconds :: Int
  }
```

#### Versioning

New SMP protocol version. Older routers/clients don't advertise the capability.

### Default routers

Default router list updated to include name-capable routers.

## Part 3: UI integration

### Markdown

- `#name`, `#'my name'` - global names
- `#namespace:name` - namespaced (e.g., `#testnet:myname`)
- Rendered as clickable resolve-and-connect links

CLI: `#` = local in group commands, global in `/c` and messages.

`:name`, `:'my name'` - contact addresses (same namespace, different link type). MVP parser supports this syntax; resolution works; UI support is post-MVP.

### Resolution flow

1. Normalize to lookup key
2. `RSLV` to two name servers via two proxies
3. Compare results
4. First channel link -> short link resolution -> connection data
5. Present for joining

### Search

`#` prefix triggers resolution. Disable in "More privacy" settings.

### Verification

On-chain link matches profile address = verified. Only name owner can set on-chain links.

- Manual: "Verify" button resolves and compares
- Auto: optional setting, resolves on profile open

### Display

Show owner's display name and verification status. `#` is syntax, not part of the name.

## Open questions

1. **Contract upgrade mechanism**: proxy pattern with timelock? Migration path for future Community Credits payment and domain name support.
