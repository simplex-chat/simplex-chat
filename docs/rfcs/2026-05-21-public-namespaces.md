# Public Namespaces for SimpleX Network

## Motivation

SimpleX has no user identifiers - users exchange invitation links out-of-band to connect. Short links help but are unmemorable. Public namespaces map human-readable names to SimpleX addresses.

Names also solve censorship at two levels. A short link is controlled by one SMP router - that router can delete it. An on-chain name can't be deleted by any router. If the link is removed, the owner points the name to a new link on a different router. At the network level, links can be URL-filtered, but names resolve through SMP proxy chains - censoring a name requires controlling all resolvers the user can reach.

DNS-based naming is vulnerable to domain seizure and requires WHOIS entries. Blockchains provide censorship-resistant globally unique names.

## Product requirements

### MVP

- **Names**: TLD `.simplex` (e.g., `privacy.simplex`, `my-channel.simplex`). Subdomains: `support.acme.simplex`. In markdown, `.simplex` can be omitted: `#privacy` = `privacy.simplex`.
- **Name rules**: see [Name rules](#name-rules).
- **Two address types**: each name stores channel links (set) and contact links (set). Client uses the first; set provides forward-compatible redundancy. Either can be empty.
- **Optional metadata**: admin SimpleX address, admin email.
- **Registration**: commit-reveal to prevent frontrunning. Length-based ETH pricing. Annual renewal. Dutch auction on expiry.
- **Launch gating**: requires SimpleX test NFT. Up to 5 paid + 5 test names per holder. Test names free, auto-removed after 3 months, use `testing` namespace.
- **Reserved names**: common verticals (books, games, music, movies, news, etc.) reserved for community-operated channels managed by SimpleX Network Consortium.
- Only 7+ character names can be registered during "launch phase".
- **Resolution**: client queries two independent name servers (Ethereum light clients) via two SMP proxies. Agreement = trusted. Disagreement = warning.
- **Double resolution**: name -> short link (on-chain), short link -> connection data (existing protocol).
- **Verification**: if on-chain link matches profile address, name is verified. Manual "verify" button + optional auto-verify on profile open.
- **Markdown**: `#name` (`.simplex` implied), `#name.simplex` (explicit), `#name.testing` for test namespace. In CLI, `#` is local in group commands, global in `/c` and message bodies.
- **Search**: `#name.simplex` auto-resolves. Disable in "More privacy" settings.
- **Router role**: `names` added to `ServerRoles`. Not all routers support it.
- **Contract**: ENS fork on Ethereum mainnet. ETH payment. Upgradeable.

### Post-MVP

- **Multiple links**: redundant entries per name. Forward-compatible schema in MVP where practical.
- **Contact syntax**: `:name.simplex`, `:my-name.simplex`. Same namespace, different link type. MVP parser supports this syntax; resolution works; UI support is post-MVP.
- **Community Credits**: replace ETH for private registration.
- **Unicode expansion**: add scripts as user base grows.

## Part 1: Blockchain contract

### Overview

ENS fork on Ethereum mainnet. Retains commit-reveal, pricing, expiry, Dutch auction. Compatible with ENS dApp. Upgradeable.

ENS source:
- Contracts: https://github.com/ensdomains/ens-contracts
- dApp: https://github.com/ensdomains/ens-app-v3
- JS library: https://github.com/ensdomains/ensjs

### Contract state

```
Name record (ENS structure + SimpleX resolver fields):
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

### Name rules

ENS normalization (ENSIP-15) with additional restrictions enforced in dApp (registration) and resolvers (resolution). Contract follows ENS as-is.

Additional restrictions beyond ENSIP-15:
- No consecutive hyphens.
- No accented characters. Latin is `a-z` only (same as DNS LDH rule).
- Allowed scripts: Latin, Cyrillic, Arabic, Hebrew, Devanagari, Bengali, Thai, Greek, CJK, Hangul, Kana. Expandable as user base grows.

### Registration flow

1. NFT check
2. Limit check (5 paid / 5 test)
3. `commit(hash(name, owner, secret))`
4. Wait (min 1 minute)
5. `reveal(name, owner, secret)` + ETH (zero for test)
6. Validate: well-formed, not taken, not reserved, fee covered
7. Store record

### Pricing

Annual fees by name length:

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

Owner can update links, admin address, admin email. Transfer follows ENS mechanics.

### Reserved names

List for community channels (e.g., `books`, `games`, `music`, `news`):
- Not registrable by users
- Revenue shared with network

### Retained ENS features

- **Resolver pattern**: registry maps name -> (owner, resolver). A SimpleX Resolver contract stores channel links, contact links, admin fields. Allows future extensibility without registry changes.
- **Multicoin address records**: BTC/ETH/XMR donation addresses per name. Subscribers see donation options from name resolution.
- **Text records**: generic key-value store for future metadata without contract upgrades.
- **Reverse resolution**: name lookup by address. Enables verification and discovery.
- **Subdomain registrar**: owner of `acme.simplex` can create `support.acme.simplex`, `sales.acme.simplex` without additional on-chain registration.

### Removed ENS features

- Avatar/image records.
- `.eth` TLD and ENS name imports.
- DNS name registration (DNSSEC imports).

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
  RSLV <namehash>

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

- `#name` or `#name.simplex` - native names (no dot = `.simplex` implied)
- `#my-name` or `#my-name.simplex` - hyphenated names
- `#sub.name.simplex` - subdomains (explicit TLD)
- `#name.testing` - test namespace
- Rendered as clickable resolve-and-connect links

CLI: `#` = local in group commands, global in `/c` and messages.

`:name.simplex`, `:my-name.simplex` - contact addresses (same namespace, different link type). MVP parser supports this syntax; resolution works; UI support is post-MVP.

### Resolution flow

1. Normalize per ENSIP-15, compute namehash
2. `RSLV` to two name servers via two proxies
3. Compare results
4. First channel link -> short link resolution -> connection data
5. Present for joining

### Search

`#...simplex` triggers resolution. Disable in "More privacy" settings.

### Verification

On-chain link matches profile address = verified. Only name owner can set on-chain links.

- Manual: "Verify" button resolves and compares
- Auto: optional setting, resolves on profile open

### Display

Show name and verification status. `#` is syntax, not part of the name.

## Open questions

1. **Contract upgrade mechanism**: proxy pattern with timelock? Migration path for future Community Credits payment and domain name support.
