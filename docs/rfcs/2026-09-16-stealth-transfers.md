# Stealth transfers

## Problem

A name can be given away. Today that means asking the recipient for an address,
which needs a handshake, and which records them on chain next to the name
before they have agreed to anything. Declining afterwards is not possible: the
transfer already happened and the link is public and permanent.

The recipient also has to find such a name again from the recovery phrase
alone, on a device with no chat database, because that is the only recovery
[wallet keys](./2026-09-10-wallet-keys.md) leaves them.

This adds the keys and the flow for both: sending a name to someone with no
handshake, receiving one without being bound to it, and rediscovering received
names from the phrase. Nothing here is implemented yet. The same mechanism
carries anything else an address can hold, which is why the keys sit where they
do.

## Meta-address

A profile publishes a **meta-address**: a spending public key and a viewing
public key, 33 compressed bytes each, 66 bytes in all. It is not an address. It
never appears on chain and costs no gas.

A sender picks a fresh random scalar `r`, and derives a destination nobody else
can recognise:

```
r random, R = r*G
s = keccak256(x || y of r*P_view)  the point, SEC1 prefix dropped
destination = addr(P_spend + s*G)
view tag = s[0]
```

The recipient recovers the same point from the other side, and with it the key:

```
s' = keccak256(v*R)
their key for it = (p_spend + s') mod n
```

`Simplex.Messaging.Eth.Stealth` in the pinned simplexmq already implements this
(`metaAddress`, `stealthDestination`, `stealthMatch`, `stealthPrivateKey`),
with the encodings pinned to the EIP author's reference implementation and
tested against an independent one. It needs no build flag: the module
multiplies points directly rather than calling `secp256k1_ecdh`, whose built-in
hash is SHA-256 where ERC-5564 wants keccak256. Nothing new is needed one layer
down.

**The destination key is not at a derivation path.** It is the spending key
tweaked by a scalar that only `R` and the viewing key produce. So the wallet
has to hold a key as "spending key plus ephemeral public key" rather than as a
path, which is what the table in the schema section stores. That is also why
the stealth keys belong to a profile and not to a name: a meta-address is an
identity published once, while a name is a thing that arrives many times, each
on its own tweaked key.

## Derivation

Wallet keys reserved `m/5564'/60'/i'/...` and left it unspecified. Filled in:

```
m/44'/60'/0'/0/0      unused
m/44'/60'/0'/0/k      names, k >= 1, counted per device
m/44'/60'/i'/0/j      a profile's own addresses, i >= 1          (still later)
m/5564'/60'/i'/0'/0   a profile's stealth spending key, i >= 1
m/5564'/60'/i'/1'/0   a profile's stealth viewing key, i >= 1
```

`i` is one number per profile, the same under both purposes, so a profile has
one account index and not one per purpose. Account 0 is the device's names
account, so profiles start at 1, and `m/5564'/60'/0'/...` is left unused for
the same reason `m/44'/60'/0'/0/0` is: neither dimension claims the point where
both start.

There is no registered purpose number for ERC-5564. `5564'` is ours. Define it
once and never change it, because a derivation path cannot be migrated once
people hold assets at it.

`0'` for spend and `1'` for view is BIP-352's convention, reused so that one
mental model covers every chain.

**This reintroduces the profile account that wallet keys removed**, and for the
opposite reason. Names were moved to a device counter because which profile
owns a name is in the record, not in the key, so a profile dimension there
carried a mapping nothing read. A meta-address is different: it is published
per profile and it is what tells two profiles apart, so the dimension is load
bearing.

**One meta-address per device is rejected.** It would collapse the recovery
scan to one pass, but the viewing key is published inside the meta-address, so
any contact who saw two of your profiles could link them. The same argument
rejects sharing one viewing key across accounts.

**Indices are allocated densely and never reused.** A profile takes one when it
first publishes a meta-address, not when it is created, so the live set is
`1..N` with holes only where profiles were deleted. Dense allocation is what
makes the gap limit in the recovery section work.

## What the meta-address discloses

Deriving a destination needs either the sender's `r` or the recipient's `v`. A
meta-address is neither. So whoever holds `(P_spend, P_view)` can send you a
name and learn nothing else. They cannot find the destinations of names you
received, cannot link two gifts from different senders, and cannot tell whether
you accepted.

Wide distribution is therefore a spam surface, not a privacy leak, and it is
bounded by the sender having to pay to register what they send.

Two residual facts, both inherent. **The sender always knows**, and can prove
the derivation by revealing `r`: stealth protects against observers, never
against the counterparty. And **the relayer sees the destination**, so
unlinkability holds against chain analysis, not against SimpleX.

Publication is opt-in per profile, and follows `contactDomain`: a field on
`Profile`, carried to group members through `redactedMemberProfile` and
**gated on `allowDirect` the same way**. That refines the earlier plan, which
passed it through
unconditionally so that gifting would work for people known only through a
group. Gating loses nothing: a member who cannot direct message you has no
channel to tell you the ephemeral key either, so the gift would be inert
anyway.

**ERC-6538 is not used.** The on-chain meta-address registry buys nothing here.
The profile already distributes the meta-address to exactly the people who can
send anything, and a registry would make a permanent public identity binding
out of something with no reason to be public.

## Sending

The transfer is the registrar's sponsored path, signed by the name's key and
submitted by the relayer, with the ephemeral key and view tag inside the signed
struct:

```solidity
function transferWithSig(address from, address to, uint256 tokenId,
                         uint256 nonce, uint256 deadline, bytes calldata sig,
                         bytes calldata ephemeralPubKey, bytes1 viewTag) external;
```

```
TransferName(address from,address to,uint256 tokenId,bytes ephemeralPubKey,bytes1 viewTag,uint256 nonce,uint256 deadline)
```

EIP-712 domain `SimplexNames`, version `1`, `verifyingContract` the registrar.
`ephemeralPubKey` is dynamic `bytes`, so the digest carries
`keccak256(ephemeralPubKey)`; `viewTag` is `bytes1`, left aligned. The
ephemeral key is the 33-byte compressed form, exactly what `stealthDestination`
returns.

Signing those two fields is what makes the announcement trustworthy. Outside
the signed struct a relayer could attach a fabricated derivation to a genuine
transfer, or suppress the real one.

An empty `ephemeralPubKey` emits no announcement, so a plain transfer to an
address the recipient gave you costs nothing extra and stays out of everyone's
scan.

Then the sender tells the recipient, in an ordinary chat message carrying the
name and `R`. The view tag does not travel, because the recipient recomputes
it. The encoding of that message belongs to the names protocol and is not
decided here.

**The client keys the signed shape off the deployment it is addressing, not off
its own build.** `BaseRegistrarImplementation` is not upgradeable, so the old
and new typehashes cannot coexist in one deployment and cannot be migrated, and
the live `.testing` registrar keeps the old typehash and a bespoke event
permanently. That is a fork in the client, not a migration.

## Receiving

**The message is not trusted.** The client derives the address itself from `R`
and its own keys, and confirms on chain that the name is held there. A sender
cannot name a gift something it is not.

Only then does the destination become a row, with `accepted_at` null.
**Nothing is claimed, displayed as yours, or attached to a profile until the
user accepts.** An unaccepted name is not yours.

Declining deletes the row. It writes nothing on chain and leaves no trace,
which is the whole point: acceptance is the act that creates the public link,
and it is the recipient's alone.

Accepting is an on-chain write, not a local action. The app renders a name
against a profile only with a `SimplexDomainProof` signed by that profile's
identity key, and resolution needs the resolver's records pointing at the
recipient. So accepting is a relayed `setTextWithSig` signed by the destination
key, then the signed claim.

Relayed edits are metered by the service, ten per name, granted at registration
and replenished by renewal. The sender may have spent them, so the recipient
cannot rely on inheriting any, and acceptance has to be able to buy a top-up.
**This corrects the earlier plan**, which metered edits as on-chain credits
against the node: the contracts no longer grant or count them, and
`grantEditCredits` does not exist.

A free gift is therefore not free to use, and the screen has to say so before
the purchase sheet opens rather than after.

## Recovery from the phrase alone

There is **no background scan and no scheduler**. A sender can only derive the
destination if they hold your meta-address, and that reached them over an
established connection, so the sender always has a channel by construction.
Discovery is a message, not a search.

The on-chain announcement exists for one case: a device restored from the
phrase with no chat database, where there is no message to read. It is
ERC-5564's `Announcement` verbatim, emitted by our registrar rather than the
canonical singleton announcer, so a scan covers SimpleX name transfers only
instead of every stealth transfer on the chain. That is what keeps it cheap.

```solidity
event Announcement(uint256 indexed schemeId, address indexed stealthAddress,
                   address indexed caller, bytes ephemeralPubKey, bytes metadata);
```

`metadata` is `viewTag || 0x23b872dd || registrar || tokenId`, so a match
learns what it was sent without a second lookup. **`caller` is the relayer, not
the gifter**, and a scanner must not read it as the sender.

A scan runs on seed import and behind an explicit action, never otherwise. Per
account it costs one point multiplication and one hash per announcement, and
the view tag discards about 255 in 256 before the point addition. The viewing
key never leaves the device: the service serves announcement ranges and the
client does the arithmetic, so no delegated-scanning privacy trade arises.

Profile accounts are walked `i = 1, 2, 3, ...` until ten consecutive accounts
match nothing, the BIP-44 gap limit idiom, with a "scan further accounts"
action to extend it. The walk needs no profile to exist, because each
candidate's viewing key comes from the seed. For stealth the scan is its own
probe, so an account that only ever received is found rather than missed. It is
still missed if it sits behind ten accounts that received nothing, which is
what the extend action is for.

The registrar's `TransferToSelf` revert is what keeps this bounded. Without it
an owner could self-transfer in a loop with a fresh ephemeral key for the price
of gas and inflate every recipient's scan without limit.

Two gaps remain, both benign. The transfer and the message are not atomic, so a
sender can submit and crash before sending. And a contact can forward your
meta-address to someone you have no channel with, who can then send without
being able to tell you. In both cases nothing is lost, because the name sits at
an address the seed already controls and a rescan finds it, and nothing is
attached without consent.

## Other assets

Names are the first thing this carries, not the only one. Ether and any ERC-20
land at the same destination with no new derivation: the key is an ordinary
secp256k1 key and the address an ordinary address.

Bitcoin and Monero reach the same shape by different standards, so their
branches are fixed now and built later:

```
m/352'/0'/i'/0'/0     BIP-352 silent payments, spend
m/352'/0'/i'/1'/0     BIP-352 silent payments, scan
m/44'/128'/i'/0'      Monero, then SHA3 and sc_reduce32 to spend and view
```

Nothing is designed for them here and no storage anticipates them beyond the
`chain` column below. What is being fixed is the path layout, because that is
the one thing later work cannot change.

## Schema

One migration, sorting after `20260908_wallet_seeds`, in both the SQLite and
Postgres lists.

```sql
-- High-water mark for profile accounts. It cannot be read off users: after a
-- phrase-only restore that table is empty while accounts already hold keys, so
-- the next profile would republish a recovered account's meta-address. The scan
-- sets this, and new profiles allocate above it.
ALTER TABLE wallet_seeds ADD COLUMN next_account_index INTEGER NOT NULL DEFAULT 1;

-- Resume point of the last scan, so a repeat scan is not a rescan from zero.
-- Not a live watermark: normal discovery is a message.
ALTER TABLE wallet_seeds ADD COLUMN scanned_to TEXT;

ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;

ALTER TABLE contact_profiles ADD COLUMN stealth_meta_address BLOB;

-- Destinations learned from a message or rediscovered by a scan. The chain is
-- carried from the start so that BTC and XMR need no migration, only rows with
-- a different value.
CREATE TABLE wallet_destinations (
  wallet_destination_id INTEGER PRIMARY KEY AUTOINCREMENT,
  wallet_seed_id INTEGER NOT NULL REFERENCES wallet_seeds ON DELETE RESTRICT,
  account_index INTEGER NOT NULL,
  chain TEXT NOT NULL,             -- 'eth' now
  address BLOB NOT NULL,
  ephemeral_pub_key BLOB NOT NULL, -- enough to re-derive the key from the seed
  discovered_at TEXT NOT NULL,
  accepted_at TEXT,                -- null until the user accepts
  UNIQUE (wallet_seed_id, chain, address)
) STRICT;

CREATE INDEX idx_wallet_destinations_account
  ON wallet_destinations(wallet_seed_id, account_index);
```

**Destinations hang off the seed and an account index, not off a user.** The
earlier plan keyed them on `users`, which cannot hold what a scan finds: after
a phrase-only restore an account has keys and received names while no profile
exists yet. `users.wallet_account_index` is the join, so adopting a recovered
account for a new profile attaches its destinations with no row rewriting.
`scanned_to` moves onto the seed for the same reason.

No private key is stored. `ephemeral_pub_key` is what re-derives one from the
seed, so the table is a cache: losing it costs a rescan, not an asset.

`ON DELETE RESTRICT` changes what `/_wallet delete` does: it starts failing
once a destination exists, rather than destroying control of the names at them.
That is the intended answer, but it is a change to a command that already
ships, so it is a decision and not a detail.

`stealth_meta_address` is on `contact_profiles` because that is where
`contact_domain` is, and the row plumbing is shared. For a contact it is what
was received and is the only copy. For a profile of your own it is derivable
from the seed and the account index, and stored only so that one column serves
both.

**Which name is at a destination is not stored**, because it is on chain and
the registrar is enumerable. Storing it would mean trusting the sender's word
for it.

## Commands

Internal API, extending `/_wallet`. `export` already names which kind of secret
it returns, so stealth follows that shape.

```
/_wallet meta                      this profile's meta-address
/_wallet meta on                   allocate this profile's account and publish it
/_wallet meta off                  stop publishing it
/_wallet send <name> <meta>        derive a destination and sign the transfer
/_wallet received                  destinations for this profile's account
/_wallet scan                      rediscover destinations from announcements
/_wallet export stealth <id>       one destination's secret, 0x and 64 hex digits
```

Reading never allocates. `meta on` is the opt-in and the only thing that takes
an account index, matching the rule that a seed is never created as a side
effect of reading.

`export stealth` returns an ordinary secp256k1 key that MetaMask imports, and
it discloses that one address and nothing else: not the seed, not the other
names. It is what keeps the non-custodial claim true for a received name, and
the manual escape hatch if the relayer stops serving.

## Incognito and hidden profiles

**An incognito profile carries no meta-address and no account index**, and
cannot receive a name. One that inherited the user's meta-address would hand
the contact a direct correlator back to the main identity, which defeats the
feature. This is a positive check, not an omission to rely on.

Hidden profiles are as wallet keys left them: they should not register a name,
and they should not publish a meta-address either. The seed is the device's, so
whoever unlocks any profile can derive every profile's stealth keys, hidden
ones included.

## What the phrase does not carry

The phrase carries entropy. Which profile accounts were taken is not in it, and
`next_account_index` starts at 1 after an import, so until a scan raises it a
new profile would publish a meta-address that a recovered account already
published. Nothing can act on the stale mark until publishing exists, and the
scan lands with it.

Which profile an account was is gone with the chat database, and what an
account received is the only thing left that identifies it. Names bought on
this device do not help: they are counted per device and hang off no account at
all.

## Risks

- Stealth does not protect against the sender, who can prove the derivation.
- Unlinkability is against chain observers, not against SimpleX: the relayer sees
  destinations and later relays intents from them.
- Anyone who can direct message you can send you a name. Spam surface, bounded by
  the sender paying to register.
- Accepting a gift costs money, so a free gift is not free to use.
- Received names need our software or a reimplementation: they sit at addresses
  that are at no derivation path, announced by our own registrar, so no
  third-party wallet finds them from a phrase. `export stealth` keeps the
  per-name guarantee true, but one address at a time.
- A crash between transfer and message loses the notification, though not the
  name. The recipient hears nothing until the sender resends or a scan runs.
- The derivation paths cannot be changed after anyone holds an asset at one.

## Scope

Here: the stealth keys and their paths, the profile account, meta-address
publication, the send and receive flows, the recovery scan, and the storage all
of that needs.

Not here: buying a name, the names protocol and the encoding of the message
that carries the ephemeral key, the registrar and relayer, the service that
serves announcement ranges, the top-up purchase, any UI, several seeds per
device, a profile's own addresses, Bitcoin, Monero and ERC-20.
