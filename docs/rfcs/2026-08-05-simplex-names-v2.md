# SimpleX Names v2 — in-app purchase, non-custodial ownership

This is the design for moving SimpleX name registration into the app, with in-app
payment and non-custodial ownership. The wallet underneath is built for where it is
going, not only for names.

## Table of contents

1. [Design decisions at a glance](#1-design-decisions-at-a-glance)
2. [The vision the wallet must survive](#2-the-vision-the-wallet-must-survive)
3. [Unlinkability: what it buys and what it does not](#3-unlinkability-what-it-buys-and-what-it-does-not)
4. [Context](#4-context)
5. [Executive summary](#5-executive-summary)
6. [Decisions taken](#6-decisions-taken)
7. [Architecture](#7-architecture)
8. [Workstream A — contracts](#8-workstream-a--contracts)
9. [Workstream B — client crypto](#9-workstream-b--client-crypto)
10. [Workstream C — the wallet](#10-workstream-c--the-wallet)
11. [Workstream D — names service](#11-workstream-d--names-service)
12. [Workstream E — payments](#12-workstream-e--payments)
13. [Workstream F — UI](#13-workstream-f--ui)
14. [Decisions](#14-decisions)
15. [Sequencing](#15-sequencing)
16. [Verification](#16-verification)
17. [Risks and accepted limitations](#17-risks-and-accepted-limitations)

---

## 1. Design decisions at a glance

The choices this plan rests on, each expanded in a later section:

- **The wallet is chain-generic and stealth-first.** Every chain is modelled as a
  `(spend, view)` key pair per profile account, with one-time destination addresses
  derived by the sender and recoverable by the recipient from the view key alone.
  Ethereum, Bitcoin and Monero all fit this shape. §2, §10.
- **A recipient can decline a name and avoid the on-chain link.** Received names land
  on one-time addresses and are never auto-claimed or auto-displayed. Preventing
  someone from squatting your name and asserting it is yours is *not* attempted — it
  is stated as a non-goal so it is not re-litigated. §3.
- **The sender learns where to transfer via a published stealth meta-address**, so a
  name can be gifted with no handshake and one message; the on-chain announcement
  exists only so the recovery phrase alone can rediscover a gift. No background scan. §7.
- **Acceptance is a paid on-chain write**, not a local action (§7.6); `grantEditCredits`
  **adds** rather than sets, because `renew()` is unauthenticated (§8 A2); a transfer
  requires `to != from` to stop free announcement spam (§8 A1); recovery discovers
  accounts via a cheap `balanceOf` probe and a gap limit (§10 C6); the client signs
  structured intents only, never opaque digests (§10 C3); and `exportOneTimeKey` keeps
  received names non-custodial (§10 C3).
- **A name is the only identity that survives a dead device.** The recovery phrase
  recovers assets, not profiles: the owner re-points the name at a fresh address and
  everyone who knows it finds them again. Most users have no database backup at all,
  so the phrase is the primary recovery path, not a fallback. §5, C6, C8.
- **Registration is funded by on-chain credits, one-time purchases for 1–10 years, a
  permanent beneficiary role, and a staged lockdown** of the deployed contracts. §5, §8.

## 2. The vision the wallet must survive

Names are the first use of the wallet, not its only purpose. The direction is:

- **ERC-20 transfers between SimpleX users with as little linkability as possible.**
- **Bitcoin and Monero**, from the same seed, with as close to the same properties as each chain allows.
- One recovery phrase behind all of it.

All three chains already converge on one shape, which is what makes this tractable rather than
three separate designs:

| Chain | Mechanism | Spend / view keys | Status |
|---|---|---|---|
| Ethereum | ERC-5564 stealth addresses | spending key + viewing key | standard, needs an announcer |
| Bitcoin | BIP-352 silent payments | spend key + scan key | standard, no notification tx needed |
| Monero | native one-time addresses | spend key + view key | built into the protocol |

Monero has done this since inception; BIP-352 is Bitcoin catching up; ERC-5564 is Ethereum's
version. So the abstraction is not speculative — it is the shape all three arrived at
independently.

**The wallet's core type is therefore `(chain, account) → (spend key, view key, discovered
one-time keys)`**, not "an Ethereum key". Adding Bitcoin or Monero later becomes adding a chain
module, not reworking the wallet. That is the whole point of doing this now: the seed layout and
the account model are the two things that cannot be changed later without moving live assets.

Derivation, all hardened, one seed:

```
seed (BIP-39)
└── profile account i                        one per chat profile
    ├── ETH   m/44'/60'/i'/0/0               main account — names you register yourself
    │         m/5564'/60'/i'/0'/0            stealth spend
    │         m/5564'/60'/i'/1'/0            stealth view
    ├── BTC   m/352'/0'/i'/0'/0              silent-payment spend   (BIP-352 layout)
    │         m/352'/0'/i'/1'/0              silent-payment scan
    └── XMR   m/44'/128'/i'/0'  → SHA3 → sc_reduce32 → spend/view, then native subaddresses
```

The `0'` = spend, `1'` = view convention is BIP-352's; reusing it across chains means one mental
model. There is no registered purpose number for ERC-5564, so `5564'` is ours — **define it once,
document it, never change it**, because changing a derivation path after users hold assets is the
one migration with no safe path.

Monero is the awkward one: ed25519 forbids non-hardened public derivation, and Monero applies an
extra SHA3 and `sc_reduce32` to reach a valid scalar. It still hangs off the same seed, which is
what matters.

**Out of scope for this plan.** No ERC-20 transfers, no BTC, no XMR. Only the seed layout, the
account model and the storage schema are being fixed now, because those are what future work
cannot change.

---

## 3. Unlinkability: what it buys and what it does not

**The requirement.** A recipient can decline a name sent to them, and by declining creates no
on-chain link between themselves and it. Acceptance is the act that creates the link, and it is the
recipient's alone.

**The non-goal, stated so it is not re-litigated.** Preventing someone from registering your name
and asserting publicly that it is yours is *not* achievable and is not attempted. The registration
is itself the assertion; no transfer is needed to make it, so no wallet design can prevent it. What
the app does prevent is the assertion having any in-app effect: a name renders against a profile
only with a `SimplexDomainProof` signed by that profile's SimpleX identity key
(`Simplex/Chat/Names.hs:35-41`, `contactDomainVerified` on `LocalProfile`). A squatter cannot make
the app show their name on your profile, whoever holds the token.

### What the meta-address does and does not expose

Deriving a one-time address requires either the sender's ephemeral secret `r` or the recipient's
private viewing key `v`. **A meta-address is neither.** So a holder of `(P_s, P_v)` — which after
§10 C4 means every member of every group you are in — can send you a name, and nothing more:

- they **cannot** find the one-time addresses of names you received,
- they **cannot** link two gifts from different senders to each other,
- they **cannot** tell whether you accepted.

Broad distribution is therefore a spam surface, not a privacy leak. The residual privacy facts are
narrower and both inherent: **the sender always knows**, and can prove the derivation by revealing
`r` — stealth protects against observers, never the counterparty — and **the relayer sees the
destination**, so unlinkability holds against chain analysis, not against SimpleX.

### Rules this imposes

**Never auto-claim, never auto-display.** An unaccepted name is not yours: it must not appear in the
names list or attach to a profile. Declining does nothing on-chain and so leaves no trace.

**Incognito profiles carry no meta-address.** An incognito profile that inherited the user's
meta-address would hand the contact a direct correlator back to the main identity, defeating the
feature. Incognito profiles get no account index and no meta-address field, and cannot receive
names.

**ERC-6538 is not used.** The on-chain meta-address registry buys nothing here — the SimpleX profile
already distributes the meta-address to exactly the people who can send you anything — while making
a permanent public identity binding out of something that has no reason to be public.

---

## 4. Context

SimpleX names ship today in v7.0 (App Store, 2026-08-02) as BETA. The app resolves
`@name.testing` / `#name.testing` over SMP and claims a name on a profile, but registration happens
outside the app: install MetaMask, fund it with ETH, visit `testing-names.simplex.chat`, complete a
commit/reveal. `docs/guide/register-simplex-name.md` is five steps and the first is "Create a wallet".

The target is: press a button, type a name, pay with the store sheet, done. No wallet, no ETH, no
browser — with one exception, an exportable recovery key.

Three constraints shape everything. **Ownership stays non-custodial**: the name is owned by a key
only the user controls, and no later change is possible without their signature. **SimpleX does not
route user funds**: it sells a service and never takes store money in order to send value onward on
a user's behalf — a standing project constraint, not a legal position (§14.4). **Unlinkability is a
requirement, not a nicety**, for the reason in §3.

Verified against mainnet during planning: `SimplexController.register()` takes `owner` as a struct
field and `makeCommitment` hashes the whole struct rather than `msg.sender`, so a registrar can
register on a user's behalf with no user signature; gas is negligible (0.172 gwei base fee, ETH
$1,840 — a registration with records is about $0.15); and Apple explicitly permits using in-app
purchase to sell NFT minting and transferring services.

What does not work against the deployed contracts is everything after registration: no permit, no
ERC-2771, no EIP-712, no ERC-4337. Closing that gap is the technical core.

---

## 5. Executive summary

Build against **`.simplex`**, not yet deployed and therefore still designable. Leave `.testing` on
the dApp flow.

**Registration is funded by on-chain credits, not payment.** The treasury cold wallet grants a
registrar hot wallet a credit balance; a credited registrar registers with no value attached. The
fee would have gone to the controller and been withdrawn straight back to the same treasury, so it
is removed rather than performed. The public payable path stays open.

**Every post-registration action is a one-shot EIP-712 intent** signed by the user's key and relayed
by SimpleX, which pays only gas — `transferWithSig` on the registrar, `setTextWithSig` on a new
signature-capable resolver. No standing delegation, no smart account. Relayed record edits are
metered by on-chain per-name credits.

**The wallet is stealth-first and chain-generic.** One seed, one account per chat profile, a
`(spend, view)` pair per chain. Names you register yourself land on your main account; names sent to
you land on one-time addresses only you can find, and are never claimed without your consent.

**A name is the only identity that survives a dead device.** The recovery phrase brings back the
names but not the profiles or the queues, so the owner re-points the name at a fresh SimpleX address
and everyone who knows it finds them again. Nothing else in the app recovers a relationship after
total device loss, and since most users have no database backup at all (C8), this is the majority
path rather than a corner case.

**Payment is a one-time purchase for 1–10 years**, not a subscription. The store product matrix is
out of scope.

Deploy `.simplex` with `Root` locked, then burn the controller admin key while keeping a permanent
beneficiary role and a timelocked registrar owner.

---

## 6. Decisions taken

| Decision | Choice |
|---|---|
| TLD | `.simplex` only. |
| Registration funding | On-chain credits from the treasury cold wallet. No fee transfer for sponsored registrations. **One credit = one operation**, whatever term is bought. |
| Ownership | Plain EOA per chat profile, from a generic profile seed. |
| Sponsorship | One-shot EIP-712 intents; SimpleX pays gas only. |
| Record edits | On-chain per-name credits, 10 × years, consumed only by relayed edits. Granted by **adding**, never setting. |
| Accepting a gift | A paid on-chain write: a **10-credit** top-up, record rewrite, signed claim. Declining is free and silent. |
| Renewals | Extension purchase, 1–10 years. No subscriptions. |
| **Transfer destination** | **ERC-5564 stealth addresses. Meta-address distributed in the SimpleX profile; never on-chain, never ERC-6538.** |
| **Received names** | **Land on one-time addresses. Never auto-claimed, never auto-displayed.** |
| **Discovery** | **A chat message from the sender. On-chain announcement is the recovery path only — no background scan, viewing key never leaves the device.** |
| **Wallet shape** | **`(chain, account) → (spend, view, discovered one-time keys)`. ETH now; BTC and XMR fit the same shape later.** |
| End state | `Root` locked; controller upgrade-frozen and burned; beneficiary permanent; **`BaseRegistrar`'s owner kept as a timelocked multisig, not burned**. |
| Payments | Store IAP first, one-time products; Stripe stage 2. Matrix out of scope. |
| Transport | Ordinary SimpleX connection to a names service. simplexmq protocol unchanged. |
| Crypto | Vendored `libsecp256k1`; BIP-39/32, keccak, EIP-712 in Haskell. **Done.** |
| Key backup | Optional, persistent reminder until acknowledged. |
| Naming | *seed* / *account* / *wallet*. |

---

## 7. Architecture

### 7.1 Trust and funding

The treasury cold wallet grants **registration credits** to registrar hot wallets. A registrar
spends a credit to register or renew; no ETH price moves. Gas is paid by the registrar from its own
balance — an infrastructure cost paid to validators, not a payment made for a user.

Nothing of value moves on a user's behalf; a compromised registrar can burn credits registering junk
but cannot drain funds or touch existing names; the cold wallet can zero its credits in one
transaction.

After lockdown SimpleX can refuse to relay, refuse to renew, and see which name a paying user
registers. It cannot transfer, re-point or seize a name.

### 7.2 Purchase flow

```
app                        names service                       Ethereum
 |-- quote(name) --------------->|
 |-- IAP purchase, 1-10 years ------------------------------------->  App Store / Play
 |-- register(name, ownerAddr, years, receipt) -->|
 |                                     |-- validate receipt
 |                                     |-- commit(...)   wait >= 60s
 |                                     |-- registerWithCredit(...)   <- no value attached
 |                                     |     consumes 1 registrar credit
 |                                     |     grants 10 x years edit credits to the node
 |<-- registered(txHash, expires) -----|
 |-- RSLV name --------------------------------> SMP names role
 |<-- NameRecord{owner == my address}   (independent confirmation)
```

Seed creation is lazy but happens **before** the request: `makeCommitment` binds `owner`, so the
address must exist by the time the service commits.

### 7.3 Signed intents and edit credits

```solidity
struct TransferName { address from; address to; uint256 tokenId; uint256 nonce; uint256 deadline; }
struct SetText      { bytes32 node; string key; string value; uint256 nonce; uint256 deadline; }
struct SetBatch     { bytes32 node; bytes32 dataHash;         uint256 nonce; uint256 deadline; }
```

`SetBatch` must reuse `multicallWithNodeCheck` semantics — every entry's first 32 bytes must equal
`node` — or a signed batch could write records on nodes the signer does not own.

Edit credits meter the sponsored path only. A direct `setText` from the owner paying their own gas
is never metered: the credit bounds SimpleX's gas exposure, not what an owner may do with their own
name. Credits live on-chain against the node, so they recover with it.

### 7.4 Stealth transfer — how Bob sends Alice a name with no handshake

Alice's app derives a spending key `p` and a viewing key `v` from her profile account and publishes
the **meta-address** `(P_s, P_v)` in her SimpleX profile. It is not an address, never appears
on-chain, and costs no gas.

```
Bob:    r random,  R = r·G,  s = H(r·P_v)
        one-time address = addr(P_s + s·G)
        transferWithSig(..., to = one-time address, ephemeralPubKey = R, viewTag = s[0])

Alice:  for each announcement: if s'[0] != viewTag -> discard   (kills ~255/256 cheaply)
                               s' = H(v·R),  check addr(P_s + s'·G)
        her key for it: p + s' mod n
```

Non-interactive and unlinkable to third parties. `R` is public, so Bob simply sends it in an
ordinary chat message and Alice derives the key immediately — **no scanning in normal operation**.

### 7.5 Why there is no background scan

Bob can only derive the one-time address if he holds Alice's meta-address, and that reaches him
through her profile over an established connection. **The sender therefore always has a channel**,
by construction. Discovery is a message, not a search.

The announcement exists for one purpose — **recovery from the phrase alone**. Restore on a new
device with no chat database and there is no message to read, so `R` must also live on-chain. A scan
runs on seed import, on explicit user request, and never otherwise.

Two gaps this leaves, both benign. The transfer and the message are not atomic, so Bob could submit
and crash before sending; and a contact can forward Alice's meta-address to someone she has no
channel with, who can then send without being able to tell her. In both cases nothing is lost — the
name sits at an address Alice's seed already controls and a rescan finds it — and nothing is
attached to her without consent, because acceptance is a deliberate, paid action (§7.6). An
unannounced incoming name is inert.

Two deliberate choices remain. The announcement is emitted by **our registrar**, not the shared
mainnet ERC-5564 announcer, so a recovery scan covers SimpleX name transfers only rather than all
stealth activity on Ethereum; with view tags discarding ~255/256 by one hash, a full-history scan is
milliseconds. And discovery reuses machinery already needed: scan → candidate addresses →
`balanceOf` + `tokenOfOwnerByIndex` + `labelOf` on the ERC721Enumerable registrar gives the names
with no indexer, which is exactly what recovery-key import needs anyway.

The names service serves announcement ranges; the client does the ECDH. **The viewing key never
leaves the device**, and no delegated-scanning privacy trade arises.

Unlinkability here is against chain observers. The relayer submits the transfer and later relays
Alice's intents from that address, so it can correlate if she uses the same service connection. This
defeats public chain analysis, not the service.

### 7.6 Accepting a received name costs an edit-credit purchase

Holding the token is not enough to use a name. The app renders a name against a profile only with a
`SimplexDomainProof` signed by that profile's identity key (§3), and resolution needs the node's
records pointing at the recipient's address. So **acceptance is an on-chain write** — a relayed
`setTextWithSig` signed by the one-time key — not a local UI action.

Edit credits are granted per node at registration, to whoever registered it. The sender may have
spent them, deliberately or not, so the recipient cannot rely on inheriting any. **Acceptance
therefore buys a small edit-credit top-up through IAP**, priced as the cheapest product in the
matrix. This is the right shape anyway: it prices the gas SimpleX relays on the recipient's behalf,
and it removes any dependence on the sender's leftovers.

Declining costs nothing and writes nothing.

```
Alice: [decline] -> local row deleted, no chain state touched
       [accept]  -> IAP top-up -> grantEditCredits(node) -> setTextWithSig(node, records)
                    -> sign SimplexDomainProof -> name renders on her profile
```

---

## 8. Workstream A — contracts

**A1. `BaseRegistrarImplementation` — `transferWithSig` plus the announcement.**

```solidity
function transferWithSig(address from, address to, uint256 tokenId,
                         uint256 nonce, uint256 deadline, bytes calldata sig,
                         bytes calldata ephemeralPubKey, bytes1 viewTag) external;
event StealthNameTransfer(address indexed to, bytes ephemeralPubKey, bytes1 viewTag, uint256 tokenId);
```

Verify against the grace-aware `ownerOf` (which reverts for an expired name, correctly forbidding
transfer of a lapsed one), consume the nonce, call `_transfer` so the existing auto-reclaim fires and
the registry node and subnames follow the token. Emit the announcement only when
`ephemeralPubKey` is non-empty, so a plain transfer costs nothing extra. Expose `DOMAIN_SEPARATOR()`
and `nonces(address)`. Do **not** implement ERC-4494 permit.

**`require(to != from)`.** Without it, an owner can self-transfer in a loop with a fresh nonce and a
fresh ephemeral key — roughly 50k gas, effectively free at current base fees — and inflate the
recovery-scan set without bound. This one line is what makes the "a full-history scan is
milliseconds" claim in §7.5 hold, because it means every announcement costs a real transfer of a
real name to a different party.

**A2. New `SimplexResolver.sol`** — `PublicResolver` plus `setTextWithSig`, `multicallWithSig`,
`grantEditCredits` (controller only, **adds** to the node's balance) and `editCredits(node)`.

Adding rather than setting is a security requirement, not a generosity choice. `renew()` is
`external payable` with no access control (`ETHRegistrarController.sol:352`) — **anyone can renew
anyone's name** — so with set semantics a stranger renews your ten-year name for the minimum term
and collapses your remaining credits to 10. Adding makes the hostile renewal a gift, which is the
correct outcome for an unauthenticated call. Signatures verify against `ens.owner(node)`. Reuse the in-tree
idiom: `reverseRegistrar/SignatureUtils.sol` already provides `validateSignatureWithExpiry` over OZ
`SignatureChecker`. Keep the standard profiles untouched so `snrc-resolve.py` and the dApp read it
with no changes. Pass `trustedETHController` as ENS does, but **`address(0)` for
`trustedReverseRegistrar`** — that slot is a second permanently-trusted address with authority over
every node, and a probe confirms it is exercisable on the live `.testing` resolver.

**A3. `SimplexController` — beneficiary role, credits, freeze.**

```solidity
address public beneficiary;                                        // permanent, multisig
function setBeneficiary(address) external onlyBeneficiary;
function setRegistrarCredits(address, uint256) external onlyBeneficiary;
function withdraw() public;                                        // pays beneficiary, not owner()
function registerWithCredit(Registration calldata) external;
function renewWithCredit(string calldata, uint256, bytes32) external;
function freezeUpgrades() external onlyOwner;                      // one-way
```

The credited paths are the existing `register` (`:309`) and `renew` (`:411`) with the `msg.value`
checks at `:321`/`:419` replaced by a credit decrement and the refund branches at `:405`/`:425`
removed. One credit = one register or renew call. Keep the payable paths.

**A4. Deploy `Root.sol` and `lock("simplex")`.** `.testing` skipped this.

**A5. Deployment script** — price curve `[0, 0, 4056075240196, 1014018810049, 31688087814]`,
`nftGateEnabled = false`, `minCharLength = 6`, both reverse registrars `address(0)`, resolver =
`SimplexResolver`, reserved names finalised before lockdown, initial credits granted.

**A6. Staged lockdown.** `_register` opens with `require(available(id))`, so **no owner power on
either contract can seize, transfer or re-point a live name** — that is what makes burning safe.
Burn the controller owner: what remains after freeze is nuisance-grade (arbitrary pricing until
`freezePriceOracle()`, reserve-then-mint reaching only unregistered names, monotonic switches).
**`BaseRegistrar`'s owner is kept as a timelocked multisig and is not burned** (§14.2) —
`addController` is the only recovery path from a buggy frozen controller and cannot touch live names,
while `removeController` and `setSubnameHook` are DoS levers the timelock exists to expose. Order:
`freezePriceOracle()` → finalise reserved names → `Root.lock` → `freezeUpgrades()` → burn the
controller owner.

**A7. Docs.** Update `docs/architecture.md`, `docs/security.md`, `docs/sequence-happy-flow.md`;
correct `verification.mainnet.testing.json`.

---

## 9. Workstream B — client crypto (`simplexmq`) — **done**

Merged on `ab/eth-crypto`; 98 tests against published vectors. `Crypto/Secp256k1.hs` (vendored
libsecp256k1 v0.8.0, recoverable ECDSA), `Crypto/BIP39.hs`, `Crypto/BIP32.hs`, `Eth/Keccak.hs`,
`Eth/Address.hs`, `Eth/EIP712.hs`. No RLP — the client signs typed data and never builds a
transaction. See `plans/2026-08-05-eth-crypto-bindings.md`.

**This design adds one primitive:** ECDH on secp256k1 for stealth derivation. libsecp256k1 already provides
`secp256k1_ec_pubkey_tweak_add` and the ecdh module; enabling `ENABLE_MODULE_ECDH` alongside
`ENABLE_MODULE_RECOVERY` is a one-word cabal change. `Eth/Stealth.hs` then holds meta-address
encoding, sender derivation, and the scan-with-view-tag loop.

Cross-compilation: `aarch64-android` compiles and links; `armv7a-android` compiles; Windows is
blocked by a duplicate patch in the pinned haskell.nix branch, unrelated to this work.

---

## 10. Workstream C — the wallet

### C1. Vocabulary

| Term | Meaning |
|---|---|
| **seed** | BIP-39 entropy. Generic, profile-scoped, chain-agnostic. |
| **account** | A profile's slot in the seed, index `i`. Holds a key *per chain*, not one key. |
| **main address** | The account's ordinary address on a chain. Names you register yourself. |
| **meta-address** | The published `(spend, view)` public pair. Not an address; never on-chain. |
| **one-time address** | A destination derived by a sender. Where received names and, later, received funds land. |
| **wallet** | `Simplex.Chat.Wallet` — creation, derivation, storage, scanning, signing. |

### C2. Schema

```sql
CREATE TABLE wallet_seeds (
  wallet_seed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  seed BLOB NOT NULL,
  created_at TEXT NOT NULL,
  backed_up INTEGER NOT NULL DEFAULT 0,
  -- High-water mark for account allocation. Cannot be derived from users.wallet_account_index:
  -- after a phrase-only restore that table is empty while accounts 0..N already hold names,
  -- so a new profile would collide with a recovered account (C6). The recovery probe sets this.
  next_account_index INTEGER NOT NULL DEFAULT 0
);
ALTER TABLE users ADD COLUMN wallet_seed_id INTEGER REFERENCES wallet_seeds ON DELETE RESTRICT;
ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;

-- Destinations learned from a sender's message, or rediscovered by a recovery scan.
-- Chain is carried from day one so BTC and XMR
-- need no migration: only new rows with a different chain value.
CREATE TABLE wallet_one_time_addresses (
  wallet_one_time_address_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  chain TEXT NOT NULL,                      -- 'eth' now; 'btc', 'xmr' later
  address BLOB NOT NULL,
  ephemeral_pub_key BLOB NOT NULL,          -- enough to re-derive the key from the seed
  discovered_at TEXT NOT NULL,
  accepted_at TEXT,                         -- NULL = not accepted; see §3
  UNIQUE (user_id, chain, address)
);
CREATE INDEX idx_wallet_one_time_addresses_user ON wallet_one_time_addresses(user_id, chain);

-- Position of the last recovery scan, so a repeat scan is resumable. Not a live watermark:
-- normal discovery is a chat message, not a scan (§7.5).
ALTER TABLE users ADD COLUMN wallet_scanned_to TEXT;
```

Storing `ephemeral_pub_key` means a private key is never stored — it is re-derived from the seed on
demand, so the table is a cache and the phrase remains sufficient. Losing the table costs a rescan,
not an asset.

The schema allows several seeds; the UI exposes one. Storing secrets in the chat DB follows the
existing pattern (`badge_master_key`, `root_priv_key`, `link_priv_sig_key`), so it is SQLCipher
encrypted and rides `exportArchive` and Migrate-to-another-device with **no new backup code** — with
the large caveat in C8: for a default-configured user those paths are closed, and the phrase is the
only recovery that exists.

### C3. Modules

- **`Simplex.Chat.Wallet`** — seeds, accounts, derivation, signing. Chain-parameterised:
  `data Chain = ChainEth | ChainBtc | ChainXmr` exists from day one even though only `ChainEth` is
  implemented, so the type signatures do not change when the others arrive.

  **The module must not export a digest-signing function.** `signDigest` becomes internal and the
  only public entry point is `signIntent :: WalletAccount -> Intent -> Either String EthSignature`,
  taking the structured intent and hashing it locally. This makes "the app never signs an opaque
  payload" a property of the type signature rather than a rule someone has to remember — it is
  otherwise exactly the mistake a `SetBatch { dataHash }` invites, since a service-supplied hash
  would let a compromised service write arbitrary records on a node the user owns.

  It also exports **`exportOneTimeKey`**: the private key `p + s mod n` for a single received name.
  That is an ordinary secp256k1 key, importable into MetaMask, and it discloses that one address and
  nothing else — not the seed, not the other names. It is what keeps the non-custodial guarantee
  true for received names (§17) and it is the manual escape hatch if the relayer stops serving.
- **`Simplex.Chat.Wallet.Stealth`** — meta-address encode/decode, sender derivation, scan loop.
- **`Simplex.Chat.Store.Wallets`** — persistence; `getOrCreateAccountRef` reuses the first seed and
  allocates the next account index.
- **`Simplex.Chat.Names.Snrc`** — namehash, token id, EIP-712 intents and canonical type strings.
- **`Simplex.Chat.Names.Service`** / `.Mock` / `.Default` — the client interface, an in-memory mock
  that verifies real signatures and enforces both credit types, and the single binding to swap for
  the SMP-backed client.

### C4. Meta-address distribution

Add an optional field to `Profile` carrying the meta-address, following the mechanism already used
twice — `badge` in 6.5, `contactDomain` in 7.0. Field on `Profile` (`Types.hs:693`) and
`LocalProfile` (`:777`), a migration, row plumbing through `Store/Shared.hs` and the three
`updateContactProfile_'` variants in `Store/Direct.hs`, and a redaction decision in
`redactedMemberProfile` (`Internal.hs:1263`).

A profile field might seem to broadcast a persistent on-chain identifier. **A
meta-address is not one** — it never appears on-chain, and holding it does not let anyone find or
link the addresses derived from it (§3).

Two rules for the plumbing. `redactedMemberProfile` (`Internal.hs:1260`) passes `badge` and
`contactDomain` through to group members by default, so the meta-address will reach every member of
every group unless a decision is made. **Pass it through** — the exposure is a spam surface, not a
privacy leak (§3), and redacting it would break gifting for anyone you know only through a group.
And **incognito profiles carry no meta-address and no account index** (§3), which is a positive
check in `fromLocalProfile`, not an omission to be relied on.

### C5. Receiving

Normally a chat message from the sender carries `R`; the app derives the address, confirms the name
is held there, and creates a `wallet_one_time_addresses` row with `accepted_at = NULL`. **Nothing is
claimed, displayed as yours, or attached to a profile until the user accepts** (§3). Declining
deletes the row and touches no chain state.

Accepting runs the sequence in §7.6: an edit-credit top-up purchase, `grantEditCredits`, a relayed
`setTextWithSig` signed by the one-time key, then a `SimplexDomainProof` signed by the profile's
identity key. Only then does the name render.

Scanning is the recovery path only (§7.5): on seed import, and behind an explicit "check for
received names" action. **No background scan, no scheduler.**

### C6. Recovery from the phrase alone

Restoring from the phrase gives no database, so the app does not know how many profiles existed or
which account indices they held. Each index has its own viewing key, and scanning is per-key, so
guessing wide is not free.

**Indices are allocated densely and never reused.** An index is taken when a profile first needs one
— which is when its meta-address is first published, not at first purchase — so the live set is
`0..N` with holes only where profiles were deleted.

**Discovery is a cheap probe before an expensive scan.** Main addresses (`m/44'/60'/i'/0/0`) are
ordinary addresses, so `balanceOf` on the registrar answers "was this index ever used" in one batched
call per index, with no scanning at all. Walk `i = 0, 1, 2, …` until **10 consecutive empty
indices** — the BIP-44 gap-limit idiom — to fix `N`. Only then scan announcements, and only with
those `N` viewing keys.

The one case this misses is a profile that received names but never bought one, whose main address
is empty and reads as a gap. A **"scan further accounts"** action extends the probe by another
10 indices for anyone in that position, which keeps the default cheap without making the edge case
unrecoverable.

Sharing one viewing key across all accounts would collapse the scan to a single pass, and is
**rejected**: the viewing key is published inside the meta-address, so a shared one would let any
contact holding two of your profiles' meta-addresses link them.

**What comes back, and what does not.** The phrase recovers *assets, not identity*. Back: every
account's keys and addresses, which indices were used, the names they hold, each name's remaining
edit credits (on-chain, per node), and any name someone sent that was never seen, since the scan does
not depend on the message having arrived. Gone: everything that made an account a profile — display
name, image, contacts, groups, history, all chat DB — and the SimpleX addresses themselves, which are
queues in the agent DB, so every contact must reconnect. The index-to-profile mapping is gone too;
in practice the names recovered against each index are what identify it.

**Recovered indices must be reserved before new profiles allocate.** `nextAccountIndex`
(`Store/Wallets.hs`) takes `MAX(wallet_account_index) + 1` over `users`, which after a phrase-only
restore is empty — so the first profile created would take index 0 and silently derive the same keys
and the same meta-address as recovered account 0. The high-water mark therefore cannot be inferred
from `users`; it lives on `wallet_seeds` (C2) and the probe sets it. New profiles then allocate above
the recovered range, and **re-adopting a recovered account for a profile is an explicit offered
action**, which is what attaches the names to a profile again.

### C7. Commands

Working in the terminal UI: `/names key`, `key import`, `key saved`, `address`, `quote`, `buy`,
`list`, `info`, `link`, `gift`, each with an `/_name …` API form. The set also includes `/names incoming`,
`/names accept <name>`, `/names decline <name>`, `/names export <name>` (the one-time key, C3),
`/names rescan [more]` (C6), and a meta-address in `/names address`.

### C8. How this fits SimpleX backup and recovery today

The claim that the seed "rides existing backup flows" is true but load-bearing enough to write down
what those flows actually are, because one of them is closed by default.

**There are two, and they are the same mechanism.** `exportArchive` (`Archive.hs:53`) zips the chat
DB, the agent DB, the files folder and wallpaper assets; import replaces both databases wholesale.
Migrate-to-another-device (`MigrateFromDevice.kt:87`) is that same archive stopped, uploaded to XFTP
as a standalone file (`:600`), handed over as a one-time link, and deleted at the source. So nothing
is transfer-only: migration is archive export plus a transport and a cleanup.

**Both are blocked on the default passphrase.** Onboarding leaves users on a random passphrase in
the keychain (`initialRandomDBPassphrase`). Export shows `exportProhibitedAlert()` and redirects to
the encryption screen (`DatabaseView.kt:251`); migration lands in `PassphraseNotSet`
(`MigrateFromDevice.kt:453`). Such a user does not have a stale backup — they have **none**.
Migration still works for them only because it offers to set the passphrase inline, on a device that
still runs. **If the device dies first, everything is gone.**

**An archive also degrades, in a way the wallet does not.** A restored archive carries stale
double-ratchet state, so messaging surfaces decryption errors — `RSAllowed` / `RSRequired`,
"decryption error (connection may be out of sync)" (`CIContent.hs:415-416`, handled at
`Subscriber.hs:669`) — and each contact and group member has to be repaired with `/sync`. Messages
that arrived in the interval are gone; there is no server-side retention to replay.

**The wallet is the resilient part**, which is unusual enough to state. Keys derive from a seed and
hold no live state, so a stale archive yields fully working names and addresses while the chat side
is still limping through resync. The only stale artefact is the `wallet_one_time_addresses` cache,
and C6's rescan rebuilds it from the seed.

### A name is the only identity that survives a dead device

Follow the worst case through. The phone is gone, there was no passphrase and so no archive, and all
that is left is the recovery phrase. C6 brings back the accounts and the names, but not the profiles
and not the queues, so every contact is unreachable and none of them can tell that the person
reappearing is the same one.

**Except that the name still resolves.** The token is still owned by a key derived from the phrase,
so the owner re-points the name at a brand-new SimpleX address and signs a fresh `SimplexDomainProof`
with the new profile's identity key. Everyone who knows the name finds them again.

Nothing else in SimpleX does this. Today a dead device with no backup is a permanent break in every
relationship the user had, and given that most users have no backup at all, this is the majority
path rather than a corner case. **Names are therefore an identity-recovery mechanism, not only an
identifier**, and that is the strongest argument for the feature — stronger than convenience or
vanity.

Two consequences follow, and they pull in opposite directions. Re-pointing costs an edit credit, so
a user whose credits are exhausted must buy a top-up (§7.6) before doing the first thing recovery
requires, and until then the name resolves to a dead queue. And the name key is thereby an
**identity-redirect key**, not merely an asset key: whoever holds it can point everyone who knows
that name at themselves. That is an argument for wording the backup screen around impersonation
rather than loss (§13).

Two conclusions the rest of the plan depends on. **The recovery phrase is not redundant with the
archive**: it is the only recovery path for every user who never set a passphrase, and the only one
that survives a dead device for anyone. And **the backup reminder is not a wallet nicety** — for
default-configured users it is the first backup prompt the app has ever given them, which raises
the bar for how it is worded (§13).

---

## 11. Workstream D — names service

A SimpleX bot plus a registrar/relayer.

- **Bot** — quote, purchase, record-update, transfer, enumeration, credit balance, and
  **announcement ranges for recovery scans**. No HTTP endpoint exposed to clients. Enumeration is advisory:
  the app confirms each name by resolving it, so a lying service can withhold but not invent.
- **Receipt validation** — App Store Server API and Google Play Developer API, with a consumed-receipt
  ledger against replay. Model on `Badges.hs:236`'s `BadgePurchase` sketch, whose `verifyPayment` is
  a stub.
- **Registrar** — hot wallet holding **credits, not spendable value**. Serialised nonces, gas bumping,
  idempotency keys, pre-flight simulation, KMS/HSM custody. Monitor the credit balance and alert with
  generous headroom: exhaustion is a hard service stop only the beneficiary multisig can clear.
- **Scheduler** — `commit`, wait ≥ 60 s, `registerWithCredit` within 24 h. Completes even if the app
  closes.
- **Extension** — `renewWithCredit` on an extension purchase. **No subscription cron.**

There is no scanning service. The bot serves raw announcement ranges and the client does the ECDH,
so a viewing key never leaves the device (§7.5).

**Race handling.** Commitments do not reserve a name. Model the purchase as an entitlement to one
registration of the paid tier and duration, with the name choice reassignable, so losing a race means
choosing again rather than losing money. **Refunds are out of scope** but will be needed.

---

## 12. Workstream E — payments

**Out of scope**, deliberately: this plan starts from "the user paid for a name of N letters for M
years". The product matrix is configured in App Store Connect and Play Console.

In scope is the shape the design assumes: **one-time purchases, not subscriptions**; extension is
another purchase; and a **10-credit edit top-up** as the cheapest product in the matrix, which is what
a recipient buys to accept a name sent to them (§7.6), what a recovering user buys to re-point a name
(C8), and what anyone buys after exhausting a name's credits. StoreKit 2 on iOS (`SettingsView.swift:11` already links the framework); Play
Billing on Android needs a **product flavour split** in `apps/multiplatform/android/build.gradle.kts`
because the library is proprietary and cannot ship in the F-Droid build. Stripe-backed redeem codes
in stage 2, behind the same payment-proof interface.

Hard constraints: **a name must never gate an app feature** — badges are the entitlement mechanism
(they already drive the XFTP size lift in `Badges.hs:184-199`), names are identity, and they must
never overlap or Apple's "NFT ownership must not unlock features" clause bites. Remove or
US-storefront-gate the `register_test_name` external link (`strings.xml:399`) once IAP ships. Sell
the service; keep contract addresses, token ids and marketplace links out of the flow.

---

## 13. Workstream F — UI

Twelve screens are drawn and reviewed, built on the app's own tokens from `Color.kt` and the
`SectionView` idioms `SetSimplexDomainView` already uses. Extend that editor rather than building new
ones: Kotlin `views/usersettings/SetSimplexNameView.kt` with call sites `UserAddressView.kt:373` and
`GroupChatInfoView.kt:181`; Swift `UserAddressView.swift:805-935` with call sites at `:196` and
`GroupChatInfoView.swift:688`.

An **incoming name** screen with two outcomes — decline, which does nothing on-chain and
is free, and accept, which is a purchase (§7.6) and creates the on-chain link, so the screen must say
both things before the sheet opens rather than after. Also: expiry warnings, because
nothing auto-renews now, and a remaining-changes count wherever a record can be edited.

Recovery key shown once after the first purchase with a persistent reminder until acknowledged
(`wallet_seeds.backed_up`). Word it as a recovery key, never a seed phrase or wallet, and make clear
it is independent of the database passphrase — for users on `initialRandomDBPassphrase` it is the
only recovery path there is, in the strict sense established in C8: archive export and migration are
both refused in that state, so this screen is the first and only backup the app has ever offered
them. It should not read like a wallet formality, and it is worth pairing with a prompt to set a
database passphrase, since that is what unlocks backup for everything else they own.

**Word the stakes as impersonation, not loss.** Whoever holds this key can re-point the name at their
own address, so losing it is not "you lose a name" but "someone else can become findable under it"
(C8). That is both the honest framing and the one users act on.

**A post-recovery flow is needed**, and it is the payoff screen for the whole feature: after
importing a phrase on a fresh install, show the recovered names, let the user attach each to a
profile — new or existing — and re-point it at that profile's address in one step, buying a
credit top-up inline if the node has none left. Handle the case where the probe found nothing by
offering "scan further accounts" (C6) rather than declaring the phrase wrong.

---

## 14. Decisions

These four are settled, recorded here with the reasoning that matters downstream:

1. **Credit unit: per operation.** One registration credit buys one `register` or `renew` call,
   regardless of the term bought. Simpler to reason about than per-name-year and adequate for what
   the credit is actually for, which is bounding a compromised registrar's blast radius.
2. **`BaseRegistrar`'s owner is not burned.** It stays a timelocked multisig. `addController` is the
   only recovery path from a frozen, buggy controller, and it cannot touch a live name because
   `_register` requires `available(id)`. `freezeControllers()` and `freezeSubnameHook()` are
   therefore not needed and are dropped from A6.
3. **Acceptance top-up is 10 edit credits**, the cheapest product in the matrix — one year's worth of
   edits, enough for a full record set with headroom (§7.6).
4. **Legal questions are out of scope.** Selling names has been researched separately and is settled;
   this plan does not carry an opinion on it. Registration credits remain in the design on their
   engineering merits — no fee to move, a small blast radius, and a one-transaction kill switch.

---

## 15. Sequencing

1. **Contracts.** Credits, `transferWithSig` + announcement, `SimplexResolver` with edit credits,
   `freezeUpgrades` + beneficiary, `Root`, deployment script, tests. Independent; start now.
2. **Client crypto.** Enable the ECDH module; write `Eth/Stealth.hs` with test vectors. Small.
3. **Wallet.** Chain-parameterised types, the one-time-address table, message-driven receiving, and
   the recovery scan. **This is the
   piece that must be right first** — schema and derivation paths are what later work cannot change.
4. **Names service.** Bot, receipt validation, registrar, scheduler, announcement ranges.
5. **Payments and UI.** The Android flavour split can start early.
6. **Sepolia end-to-end**, then `.simplex` mainnet, then staged lockdown.

---

## 16. Verification

- **Contracts.** A credited registrar registers with zero value and the credit decrements; an
  uncredited caller is rejected; the payable path still works; `transferWithSig` succeeds for the
  owner and fails for wrong signer, replayed nonce, expired deadline and expired name; the
  announcement is emitted only when an ephemeral key is supplied; **`to == from` reverts**;
  `setTextWithSig` fails at zero edit
  credits while a direct `setText` by the owner is not metered; **a renewal by an unrelated address
  adds to the node's edit credits and never reduces them**; auto-reclaim moves the registry node
  and subnames. Fork-test against mainnet state.
- **Stealth.** Round-trip: derive as sender, recover as recipient from the message, sign from the
  recovered key. Check the view tag discards the expected fraction. Cross-check against the ScopeLift
  ERC-5564 SDK so we are interoperable rather than merely self-consistent. **The load-bearing test is
  the recovery path**: wipe the device, restore from the phrase alone, and confirm the probe-then-scan
  of C7 rediscovers every received name across several profiles with no chat history — including a
  profile that only ever received, which the gap limit alone would miss. Confirm `exportOneTimeKey`
  produces a key MetaMask accepts, and that it discloses only that address.
- **Backup interaction** (C8). Export an archive, buy a name, restore the archive: the wallet must
  still derive the same addresses and the name must still be controllable, even while messaging is
  in ratchet resync. Separately, confirm the recovery-key screen appears for a user still on
  `initialRandomDBPassphrase` — the case with no database backup at all, and the one that most needs
  it.
- **Dead-device recovery**, end to end and on a clean install with nothing but the phrase: recover the
  names, attach one to a newly created profile, re-point it, and confirm a third party resolving the
  name reaches the new address. Then create a further profile and confirm it allocates **above** the
  recovered high-water mark — the collision `nextAccountIndex` produces today (C6), which is silent
  and would publish one meta-address under two identities.
- **Resolver compatibility.** Point `snrc-resolve.py` at the local deployment; a name registered
  against `SimplexResolver` must resolve identically with zero script changes.
- **Chat integration.** Extend `tests/ChatTests/Names.hs` with the `tests/NameResolver.hs` Warp
  fixture. The mock already recovers the signer from every intent and enforces both credit types;
  `cabal run simplex-names-demo` walks the journey with no money and no chain, and the tmux TUI
  harness drives the same flow through the real terminal UI.
- **End-to-end on Sepolia.** Buy from a clean install; confirm the owner matches the derived address;
  edit a record and watch the credit decrement; send a name to a second device with no handshake and
  confirm it arrives on one message; accept it; extend and confirm credits reset. Then
  export the recovery key, import it into MetaMask, and confirm the name is transferable — the test
  that proves the non-custodial claim.
- **Stores.** StoreKit sandbox and Play internal testing across the duration matrix.

---

## 17. Risks and accepted limitations

- **Derivation paths and the account model are unchangeable after launch.** Everything else in this
  plan can be revised; these cannot, without moving live assets. That is why §2 fixes them before
  any of it ships.
- **Squatting-as-doxxing is not preventable** and is explicitly not attempted (§3). The mitigation
  is that a squatted name has no in-app effect without the target's own signed claim.
- **Stealth does not protect against the sender**, who can prove the derivation by revealing the
  ephemeral secret. It protects against everyone else, which is what the requirement asks for.
- **Accepting a gifted name costs money** (§7.6). A free gift is not free to use, and that has to be
  said on the screen before the sheet opens.
- **Received names need our software or a reimplementation.** They sit at addresses derived along an
  unregistered path against our own announcer, so no third-party wallet finds them. `exportOneTimeKey`
  keeps the non-custodial guarantee true per name, but not via the phrase alone in someone else's
  wallet.
- **Anyone in a group with you can send you a name.** Spam surface, not a privacy leak (§3), and
  bounded by the sender paying to register.
- **Unlinkability is against chain observers, not against SimpleX.** The relayer sees destinations
  and later relays intents from them.
- **A crash between transfer and message loses the notification**, though not the name (§7.5). The
  recipient hears nothing until the sender resends or a recovery scan runs.
- **Registrar credits are a service stop when exhausted** — only the beneficiary multisig can refill.
- **Names lapse.** No subscriptions means an ignored expiry warning loses the name after grace.
- **Relayer liveness is a single point of failure** for every write. The escape hatch is manual:
  export the key, fund the address, act directly.
- **Nothing ships until `.simplex` is deployed.**
- **The secondary market is outside IAP.** Names are plain ERC-721s; the defence is framing.
- **Purchase and name are linkable to SimpleX.** Blind-signed vouchers would remove this and compose
  with the Community Credits work in `docs/rfcs/2025-12-10-vouchers-2.md`.
- **Expired names keep resolving** until re-registered (`docs/security.md` L7); readers must gate on
  `nameExpires(id) > block.timestamp`.
- **Subnames still need a standing `setApprovalForAll`**, which cannot be relayed — decide before
  lockdown, since `SubnameRegistrar` is immutable.
- **The chat database now holds assets.** A copy plus its passphrase can sign transfers, and an old
  copy after migration keeps working. The threat model of an archive changes character: a leaked
  archive was message history, and is now spendable keys.
- **Most users have no database backup at all** (C8), because export and migration are both refused
  on the default random passphrase. The recovery phrase is therefore the primary recovery mechanism
  for names, not a secondary one, and it carries that weight from the first purchase.
- **The name key is an identity-redirect key.** Whoever holds it can re-point the name at their own
  address and become findable under it. This is the flip side of the recovery property in C8 and it
  makes the phrase materially more sensitive than "a name could be lost" suggests.
- **Recovery restores names, not relationships.** Contacts still have to reconnect; what the name
  buys is that they can find the right person to reconnect *to*.
- **Refunds are unbuilt.**

### Two live `.testing` issues, outside this plan

`.testing`'s three trust anchors are open — the registry root is a plain EOA (`0xDa064C…`,
`eth_getCode` returns `0x`) with no `Root` and no lock, and a simulated `setSubnodeOwner` from it
succeeds; the controller's UUPS authority sits on the hot deployer `0xd83bb610…`; and `setText` on an
arbitrary node succeeds from both the controller proxy and the ReverseRegistrar `0x84c2a977…`.
Separately, `deployments.mainnet.testing.json` gives ENSRegistry `0x58fc46…` while `snrc-resolve.py`
defaults to `0x03f438…` — both live, so production may be resolving against the older deployment.
