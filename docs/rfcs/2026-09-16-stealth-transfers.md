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
handshake, receiving one without being bound to it, and finding received names
again from the phrase. Nothing here is implemented yet. The same mechanism
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
```

The recipient recovers the same point from the other side, and with it the key:

```
s' = keccak256(v*R)
their key for it = (p_spend + s') mod n
```

`Simplex.Messaging.Eth.Stealth` in the pinned simplexmq already implements this
(`metaAddress`, `stealthDestination`, `stealthPrivateKey`), with the encodings
pinned to the EIP author's reference implementation and tested against an
independent one. It needs no build flag: the module multiplies points directly
rather than calling `secp256k1_ecdh`, whose built-in hash is SHA-256 where
ERC-5564 wants keccak256. Nothing new is needed one layer down.

**The destination key is not at a derivation path.** It is the spending key
tweaked by a scalar that only `R` and the viewing key produce. So the wallet
has to hold a key as "spending key plus ephemeral public key" rather than as a
path, which is what the table in the schema section stores and what the backup
carries. That is also why the stealth keys belong to a profile and not to a
name: a meta-address is an identity published once, while a name is a thing
that arrives many times, each on its own tweaked key.

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

The backup in its own section needs keys that are not secp256k1, so they do not
come from a BIP-32 path. They come from one secret:

```
backup secret = HKDF(bip39 seed, "SimpleX wallet backup"), 32 bytes
```

Everything the backup queue needs is derived from that secret, and nothing else
about it may ever change, for the same reason as a path.

**This reintroduces the profile account that wallet keys removed**, and for the
opposite reason. Names were moved to a device counter because which profile
owns a name is in the record, not in the key, so a profile dimension there
carried a mapping nothing read. A meta-address is different: it is published
per profile and it is what tells two profiles apart, so the dimension is load
bearing.

**One meta-address per device is rejected.** The viewing key is published
inside the meta-address, so any contact who saw two of your profiles could link
them. The same argument rejects sharing one viewing key across accounts.

**Indices are allocated densely and never reused.** A profile takes one when it
first publishes a meta-address, not when it is created, so the live set is
`1..N` with holes only where profiles were deleted.

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
`Profile`, carried to group members through `redactedMemberProfile` and **gated
on `allowDirect` the same way**. That refines the earlier plan, which passed it
through unconditionally so that gifting would work for people known only
through a group. Gating loses nothing: a member who cannot direct message you
has no channel to tell you the ephemeral key either, and without that channel a
gift is unreachable, as the backup section says.

**ERC-6538 is not used.** The on-chain meta-address registry buys nothing here.
The profile already distributes the meta-address to exactly the people who can
send anything, and a registry would make a permanent public identity binding
out of something with no reason to be public.

## Sending

The transfer is the registrar's sponsored path, signed by the name's key and
submitted by the relayer:

```solidity
function transferWithSig(address from, address to, uint256 tokenId,
                         uint256 nonce, uint256 deadline, bytes calldata sig) external;
```

```
TransferName(address from,address to,uint256 tokenId,uint256 nonce,uint256 deadline)
```

EIP-712 domain `SimplexNames`, version `1`, `verifyingContract` the registrar.

**There is no announcement.** The registrar as written today carries the
ephemeral key and a view tag inside the signed struct and emits ERC-5564's
`Announcement`, so that a recipient restoring from the phrase alone can scan
the chain for gifts. That is replaced by the backup below, and the fields and
the event go before `.simplex` is deployed, which is possible because it is not
yet deployed. What that buys, beyond a smaller contract: a stealth transfer is
indistinguishable on chain from a plain one, so the chain does not even record
that a gift happened, and the service needs no scan endpoint.

`.testing` keeps whatever typehash it was deployed with, so the client still
keys the signed shape off the deployment it is addressing, not off its own
build.

The sender tells the recipient in an ordinary chat message carrying the name
and `R`. **The message goes first.** It is handed to the agent, which persists
it, before the transfer is signed and submitted, so a sender that dies in
between leaves a message and no transfer, which is harmless, rather than a
transfer and no message, which would strand the name. The encoding of the
message belongs to the names protocol and is not decided here.

## Receiving

**The message is not trusted.** The client derives the address itself from `R`
and its own keys, and confirms on chain that the name is held there. A sender
cannot name a gift something it is not, and a message whose transfer never
landed is dropped after a while.

Only then does the destination become a row, with `accepted_at` null. **Nothing
is claimed, displayed as yours, or attached to a profile until the user
accepts.** An unaccepted name is not yours.

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

## Backup

The phrase carries entropy and nothing else. Which received names exist, and
which indices are taken, is knowledge the device has and the phrase does not.
The earlier plan put that knowledge on chain, as an announcement per gift, and
rebuilt it on restore by scanning. This puts it where SimpleX already keeps
data that has to outlive a device: the link data of a contact address queue.

**What a contact queue offers.** Its link data is two encrypted blobs, a fixed
one of 2 KB and a mutable one of 13.7 KB, read by `LGET` with no
authentication, addressed by a 24 byte link ID. The client supplies that ID and
the server does not check it (`Server.hs`, `createQueue`); it is the first 24
bytes of an HKDF of the link key, and the encryption key is the rest
(`Crypto/ShortLink.hs`, `contactShortLinkKdf`). Writes go through `LSET` and
need one of the queue's recipient keys, which the client chooses at `NEW`. And
nothing on the server expires a contact queue or its link data: there is no
such config key, no sweep, no deletion by inactivity. A queue goes when its
owner sends `DEL` or an operator removes it.

**The queue is derived from the backup secret.** The link key is `sha3_256` of
the fixed data, and a reader checks that, so the fixed data has to come out
byte for byte the same on every device that holds the seed. Everything in it
comes from the backup secret or is a constant: the root signing key, the e2e
key, the nonce that fixes the sender ID, the server, the entity ID, and version
ranges pinned once and never following the client's. That gives the link ID and
the decryption key. Writing needs more: the queue's recipient key and DH key,
derived from the same secret, and the recipient ID, which the server assigns
and the blob therefore carries. With that, a device holding nothing but the
phrase derives the link ID, reads the blob, decrypts it, and can write it
again.

**Where it lives.** One queue on a server of each preset operator
(`Simplex.Chat.Operators.Presets`, SimpleX Chat and Flux today), under an agent
user of its own, so the session it is written from shares nothing with any
profile. Restore reads every preset server of both operators and takes the
newest blob.

**What is in the blob**, in the order a restore applies it:

```
version                            a counter, newest wins
recipient id                       this queue's, on this server
next_name_index                    the device's names high-water mark
next_account_index                 the profile accounts high-water mark
accounts:  index, display name     which account was which profile
received:  account index, chain, R, accepted
```

Which name sits at a destination is not in it: that is on chain and the
registrar is enumerable, and storing it would trust the sender's word. About
forty bytes per received name leaves room for several hundred. Beyond that a
second link key is derived the same way with an index appended, and never
reused for anything else.

**When it is written.** On every change to any of the above, to both servers.
The blob is padded to its fixed size, so the length never says how much is in
it.

**What an operator sees.** A link ID nobody else ever uses, read and written at
some times. It cannot tell whose it is, what is in it, or that it is a wallet
at all. It can delete it, and it can serve an old version, which the counter
makes visible and a second operator makes survivable.

**What this rests on**, said plainly: availability is now the operators', not
Ethereum's. No code, config or policy promises to keep a queue for years; the
server's own design notes contemplate expiring contact queues after about three
years of inactivity, unimplemented today. A live client touches its backup on
every change, and reading it also counts as activity on the server, so a device
that is in use keeps it alive. A device that is dead for years relies on the
operators having kept it. That is the trade for having no public marker per
gift, no scan, and a restore that also brings back which account was which
profile, which no scan could.

**When the backup is gone**, two things still work. Names bought on this device
are found by the probe that wallet keys deferred to the registrar: `balanceOf`
over `k = 1, 2, 3, ...` until ten in a row own nothing, which also sets
`next_name_index`. And received names come back from their senders, next.

## Senders keep what they sent

The sender always knows `R`. So the sender keeps every gift it sent, keyed by
the verified name of the contact it went to, and when a contact verified under
that name connects again, it sends them all again. A restored recipient is a
new contact, not the old row, which is why the key is the name and not the
contact. The recipient verifies each one as it would any message, and ignores
what it already has or cannot use.

This costs nothing and covers the case the backup cannot: a device dead long
enough for the operators to have dropped the queue. It reaches only recipients
who own a bought name, because that is what lets a contact find them again, and
only gifts whose sender is still around. It is a supplement, not a path.

A gift sent to a forwarded meta-address, by someone with no channel to the
recipient, is unsupported. It is not lost, because the address is one the seed
controls, but nothing will tell the recipient it is there.

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
`chain` column below and the chain tag in the blob. What is being fixed is the
path layout, because that is the one thing later work cannot change.

## Schema

One migration, sorting after `20260908_wallet_seeds`, in both the SQLite and
Postgres lists.

```sql
-- High-water mark for profile accounts. It cannot be read off users: after a
-- restore that table is empty while accounts already hold keys, so the next
-- profile would republish a recovered account's meta-address. The backup
-- restores it, and new profiles allocate above it.
ALTER TABLE wallet_seeds ADD COLUMN next_account_index INTEGER NOT NULL DEFAULT 1;
-- The blob's counter as last written, so a restore knows what is newer.
ALTER TABLE wallet_seeds ADD COLUMN backup_version INTEGER NOT NULL DEFAULT 0;

ALTER TABLE users ADD COLUMN wallet_account_index INTEGER;

ALTER TABLE contact_profiles ADD COLUMN stealth_meta_address BLOB;

-- Destinations learned from a message or restored from the backup. The chain
-- is carried from the start so that BTC and XMR need no migration, only rows
-- with a different value.
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

-- What this device sent, so it can send it again. Keyed by the recipient's
-- verified name, because that is what a restored recipient is found by.
CREATE TABLE wallet_sent (
  wallet_sent_id INTEGER PRIMARY KEY AUTOINCREMENT,
  contact_domain TEXT NOT NULL,
  chain TEXT NOT NULL,
  ephemeral_pub_key BLOB NOT NULL,
  sent_at TEXT NOT NULL
) STRICT;
```

**Destinations hang off the seed and an account index, not off a user.** The
earlier plan keyed them on `users`, which cannot hold what a restore brings
back: an account has keys and received names while no profile exists yet.
`users.wallet_account_index` is the join, so adopting a recovered account for a
new profile attaches its destinations with no row rewriting.

No private key is stored, here or in the blob. `ephemeral_pub_key` is what
re-derives one from the seed, so the table is a cache: losing it costs a
restore, not an asset.

`ON DELETE RESTRICT` changes what `/_wallet delete` does: it starts failing
once a destination exists, rather than destroying control of the names at them.
That is the intended answer, but it is a change to a command that already
ships, so it is a decision and not a detail.

`stealth_meta_address` is on `contact_profiles` because that is where
`contact_domain` is, and the row plumbing is shared. For a contact it is what
was received and is the only copy. For a profile of your own it is derivable
from the seed and the account index, and stored only so that one column serves
both.

The backup queue itself is an ordinary connection in the agent database. What
is new is that its keys are derived rather than drawn, which is the dependency
named under scope.

## Commands

Internal API, extending `/_wallet`. `export` already names which kind of secret
it returns, so stealth follows that shape.

```
/_wallet meta                      this profile's meta-address
/_wallet meta on                   allocate this profile's account and publish it
/_wallet meta off                  stop publishing it
/_wallet send <name> <meta>        derive a destination and sign the transfer
/_wallet received                  destinations for this profile's account
/_wallet backup                    write the blob now, and say where it went
/_wallet restore                   read the blob and apply it
/_wallet export stealth <id>       one destination's secret, 0x and 64 hex digits
```

Reading never allocates. `meta on` is the opt-in and the only thing that takes
an account index, matching the rule that a seed is never created as a side
effect of reading. `backup` exists so the write can be driven and checked; in
use the write is automatic.

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
ones included, and the backup lists every profile's account by name.

## What the phrase does not carry

The phrase carries entropy. The backup carries the rest, and the phrase is what
finds the backup. With both, a restore is complete: indices, accounts, which
profile each was, and every received name, whether or not its message ever
arrived.

With the phrase alone, bought names come back by the probe and received names
only from their senders. Which profile an account was is gone.

## Risks

- Stealth does not protect against the sender, who can prove the derivation.
- Unlinkability is against chain observers, not against SimpleX: the relayer
  sees destinations and later relays intents from them.
- The backup's availability rests on the preset operators. Nothing promises
  years, and a device dead long enough loses it. Two operators and the senders
  are the mitigations, not a guarantee.
- The backup is a map of everything the seed controls, in one place. It is
  encrypted under a key only the seed produces and padded to a fixed size, so
  what is exposed is that a blob exists and when it changes.
- Anyone who can direct message you can send you a name. Spam surface, bounded
  by the sender paying to register.
- Accepting a gift costs money, so a free gift is not free to use.
- Received names need our software or a reimplementation: they sit at
  addresses that are at no derivation path, findable only through the backup
  or the sender, so no third-party wallet finds them from a phrase.
  `export stealth` keeps the per-name guarantee true, but one address at a
  time.
- The registrar changes shape before deployment. What is built and tested
  today carries the announcement; removing it is work in the contracts, and
  `.simplex` must not deploy with it in.
- The derivation paths and the backup secret cannot be changed after anyone
  holds an asset at one.

## Scope

Here: the stealth keys and their paths, the profile account, meta-address
publication, the send and receive flows, the backup and what it carries, the
senders' re-send, and the storage all of that needs.

Not here: buying a name, the names protocol and the encoding of the message
that carries the ephemeral key, the registrar and relayer, the contract change
that removes the announcement, the top-up purchase, any UI, several seeds per
device, a profile's own addresses, Bitcoin, Monero and ERC-20.

Two changes in simplexmq are a dependency and are not designed here: a way to
create a contact link from supplied keys and nonce rather than from the agent's
random generator (`prepareConnectionLink'` draws both), and a way to register a
queue that already exists on the server, for the device that restores.
