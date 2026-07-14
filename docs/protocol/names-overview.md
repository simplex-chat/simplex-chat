Revision 1, 2026-07-14

# SimpleX Public Names for Channels and Businesses

## Table of contents

- [Introduction](#introduction)
  - [What are SimpleX names](#what-are-simplex-names)
  - [Who names are for](#who-names-are-for)
  - [Why names on a blockchain](#why-names-on-a-blockchain)
  - [Non-goals](#non-goals)
- [How names work](#how-names-work)
  - [Names and records](#names-and-records)
  - [Resolution: from name to connection](#resolution-from-name-to-connection)
  - [Private decentralized RPC layer](#private-decentralized-rpc-layer)
  - [Claiming and verification](#claiming-and-verification)
- [Fully decentralized: differences from ENS](#fully-decentralized-differences-from-ens)
- [Security](#security)
  - [Design objectives](#design-objectives)
  - [Threat model](#threat-model)
  - [Current gaps](#current-gaps)
- [Future work](#future-work)
- [Conclusion](#conclusion)


## Introduction

The SimpleX network provides communication without user identifiers - people connect by exchanging links out of band. This is the right default for private communication, but public entities have the opposite need: a channel or a business wants to be found, named, and recognized. Today that means distributing a long link, and a link is neither memorable nor censorship-resistant. SimpleX names close this gap without adding identifiers for people.

### What are SimpleX names

A SimpleX name is a human-readable name, registered on a public blockchain, that resolves to SimpleX links: `#name` opens a channel, `@name` connects to a contact or business address. One name can serve both - a business can publish `@acme` for customer conversations and `#acme` for its announcement channel from a single registration. Names support subnames (`support.acme`), and a separate test namespace (`.testing`) runs the same system for early adopters before the main namespace launches.

A name is not an account and not an identity. It is a pointer, controlled by a cryptographic key that only its owner holds, from a memorable string to the links a channel or business already has. The network does not know or require names; they are a discovery layer on top of it, described in this document.

This document covers the naming layer built on [SimpleX Channels](./channels-overview.md) and the [SimpleX network](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md). For the user-facing registration steps see [Registering a SimpleX name](../guide/register-simplex-name.md); for wire-level detail see the resolver commands in the [SMP protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md).

### Who names are for

Names are for entities that choose to be public:

- **Channels.** A channel reachable as `#name` can be shared in speech, in print, and across platforms. If the channel's link is destroyed or blocked, the owner points the name at a new link and the audience finds it again.

- **Businesses and organizations.** A business address reachable as `@name` gives customers a way to start a conversation without scanning a QR code or trusting a link from a search result, and the name's verification shows they reached the real business.

- **Creators and public figures.** Anyone publishing to an audience can be reachable by name while their audience remains anonymous.

Names are opt-in and only make sense for public entities. Private users do not need them, are never asked to create one, and lose nothing by not having one - private communication in SimpleX remains identifier-free, and no name is ever attached to a person's connections or messages.

### Why names on a blockchain

Names must be globally unique, so they need a shared registry. Every conventional registry design puts the names under someone's control:

- **A platform registry** (usernames in messaging apps) is owned by the operator: names can be revoked, impersonated, or handed over, and the platform sees who looks up whom.

- **DNS** is subject to domain seizure at the registrar and registry level, and requires registrant records. Domains of controversial publishers are routinely taken.

A record on a public blockchain has neither owner nor operator: only the holder of the registration key can change what a name points to, and no authority exists that can delete it. This addresses censorship at every level where links fail:

- **A link is hosted by one SMP router.** That router's operator can delete it, and the link is destroyed for everyone who saved it. A name survives: the owner points it at a new link on a different router, and everyone reconnects by the name they already know.

- **Links can be filtered.** A censor can recognize and block SimpleX links in transit. Name resolution travels inside the SimpleX network's ordinary encrypted traffic, in fixed-size blocks indistinguishable from messages - there is nothing to filter.

- **Registries can be pressured.** An on-chain record cannot be seized from its owner or removed by a court order served on an intermediary, because there is no intermediary.

### Non-goals

Names do not attempt to:

- **Identify users.** There is no user directory, no requirement to register, and nothing links a name to the people who read or contact it.

- **Put communication on the chain.** The blockchain stores only the mapping from a name to links and optional public profile fields. Messages, membership, and channel content never touch it.

- **Certify legal identity.** A verified name proves that the name's owner controls the address it points to - not who the owner is. It is the same trust model as a domain name.

- **Replace links.** Links remain the primary connection mechanism and the more private one, since resolving a name reveals interest in it to one resolver operator. Names are for the public entities that want to be looked up.


## How names work

### Names and records

Names are lowercase labels of ASCII letters, digits, and single hyphens, forming domains under the `.simplex` top-level name (`privacy.simplex`, `my-channel.simplex`), with `.simplex` implied when omitted: `#privacy` means `#privacy.simplex`. The restricted alphabet is deliberate: visually identical names from mixed scripts (homographs) cannot exist as distinct records, so a name that looks right is right.

A name's on-chain record stores:

- **Channel links and contact links** - each an ordered list, primary first. Multiple links give redundancy across SMP routers: clients try them in order, so the loss of one router does not break the name.
- **Optional profile fields** - a display name, website, location.
- **Optional donation addresses** (Bitcoin, Ethereum, Monero, Polkadot) that apps can show to the audience.

Registration is a two-step commit-reveal transaction from the owner's wallet: the first transaction records only a hash, so an observer of the pending registration cannot see the name and front-run it; the second reveals and secures it. The name is held as an ordinary token in the owner's wallet and is renewable and transferable. Subnames are created by the name's owner, follow the name automatically when it is transferred, and cannot be separately sold - the same ownership model as DNS subdomains.

### Resolution: from name to connection

Connecting by name is two independent resolutions:

1. **Name to link.** The client queries the on-chain record through the SimpleX network (next section) and takes the channel or contact link.

2. **Link to connection.** The link resolves through the existing SimpleX short-link protocol: the client retrieves the link's immutable data - cryptographically bound to the link's owner keys - and the entity's profile, and connects.

The second step is unchanged from connecting by link. A name adds discoverability in front of the link protocol; it does not weaken the link's own verification, and everything a client checks when joining by link is still checked when joining by name.

### Private decentralized RPC layer

Reading a blockchain normally means asking an RPC service, and in practice almost all applications - including wallets and name services - use a handful of centralized API providers. Those providers see every query, the address that made it, and the querier's IP address, and can censor or falsify responses. Bootstrapping private communication through a channel that observes and links every lookup would defeat the purpose.

SimpleX resolves names through the network itself. SMP routers can enable a *names role*: a names router runs its own resolver process and its own Ethereum node, holding a full copy of the relevant chain state - operators must not share this backend with other operators. The client's lookup is a small protocol command that travels like any other SimpleX traffic:

1. The client picks a names router among its configured servers and sends the lookup *through an SMP proxy operated by a different operator*, encrypted so the proxy cannot read it.
2. The proxy forwards the lookup without knowing what it is beyond its type; the names router answers from its local chain state.

The result is a knowledge split with no single observer: the proxy sees which client is talking to a names router, but not the name; the names router sees the name, but not who asked - no client address, session, or identity. A passive network observer sees fixed-size encrypted blocks that could be anything. Clients also keep the set of parties that ever see a lookup minimal: a query goes to one names router, and after an authoritative answer the client does not repeat the name to other servers.

Nothing in this layer is specific to names: it is anonymous, operator-diverse read access to blockchain state as a network capability. To our knowledge it has no precedent - every blockchain application today reaches the chain through centralized APIs that see who is asking what. Names are its first use.

### Claiming and verification

A name is trustworthy when two independent statements agree, made in two different places by two different keys:

1. **The on-chain record points at the link.** Only the name owner's wallet key can set this.
2. **The link's profile claims the name.** Only the link owner's keys can publish this, and the claim is signed by the link's owner key, binding it to that specific link.

Clients require both. This mutual binding is what prevents abuse:

- Registering a name that points at someone else's address achieves nothing: their profile does not claim the name, so clients refuse to connect by it and it never appears as theirs.
- Claiming a name in a profile without owning it achieves nothing either: the on-chain record does not point at that profile's address, so the claim fails verification.

Apps display the outcome next to the name - verified (the record and the claim match), failed, or not yet verified - and can re-verify on demand or automatically. Verification is a fresh resolution through the private RPC layer, so it reflects the current on-chain state.

Names bootstrap connections; they do not carry them. Once connected, a contact or channel subscription is an ordinary SimpleX connection, independent of the name. A name expiring or changing hands affects who can be *newly* discovered through it - never existing conversations or subscriptions.


## Fully decentralized: differences from ENS

The on-chain layer is the SimpleX Name Service (SNS), a fork of the [Ethereum Name Service](https://ens.domains) (ENS). ENS is the most proven decentralized naming design, and SNS keeps its foundation: the registry and resolver contracts, commit-reveal registration, and expiry-based ownership.

ENS, however, is not fully decentralized in use. Its applications depend on two off-chain services: an indexer (the subgraph) to answer basic questions such as "which names does this address hold" - without it the ENS app is unusable - and a hosted metadata service to render name tokens in wallets. SNS removes both dependencies, and simplifies the ownership model:

1. **No indexer.** The SNS contracts index names on-chain: reverse lookup from an address to the names it holds, from a token to its plaintext label, and from a name to its subnames are all direct contract reads. Every view in the SNS app - your names, your subnames, a name's records - is served by plain `eth_call`s against any Ethereum node, including your own.

2. **Fully on-chain tokens.** A name's token metadata and image are generated by the contract as inline JSON and SVG. There is no metadata server; a wallet rendering the name needs nothing but the chain.

3. **No name wrapper, no separate subname ownership.** ENS's wrapper system lets subnames be split off and owned independently, at the cost of a second token standard and a complex permission system. SNS subnames belong to the name and follow it - the DNS model - which removes the wrapper entirely.

Resolution differs too. ENS names are resolved by applications through their RPC providers, so lookups are as centralized and observable as the provider. SNS records are read through the private RPC layer described above: many independent operators hold the chain state, and no one - provider, operator, or observer - sees who resolves which name.

The result is a naming system with no off-chain service anywhere in its path: registration, ownership, enumeration, rendering, and resolution are all either on-chain or served by the SimpleX network itself.


## Security

### Design objectives

1. **No seizure.** Only the holder of the name's key can change or transfer the record; there is no registrar, operator, or intermediary with override authority.
2. **No infrastructure censorship.** A name outlives any router: links it points to can be replaced without changing the name, and resolution reaches chain state held by many independent operators.
3. **Impersonation resistance.** A name shows as verified only when the on-chain record and the signed claim in the link's profile agree - neither side can be forged alone.
4. **Lookup privacy.** No single party observes both who is looking up and what is looked up.
5. **Availability.** A record can list multiple links across routers, and any names-capable router of the user's choosing can answer a lookup.

### Threat model

This threat model assumes the [SimpleX network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md) and the [channels threat model](./channels-overview.md#threat-model), and addresses the naming layer.

**A compromised names router (or its chain backend)**

*can:*

- Deny that a name exists, or serve a stale record, to the clients that query it. Detectable by resolving through a different operator's router.
- Serve a false record - effective only against a client that queries this router *and* only if the false record's target cooperates by claiming the name, since the claim check runs against the attacker-supplied record. Resolver agreement across operators ([future work](#future-work)) makes this require collusion of the queried operators.
- Observe which names are looked up, and how often.

*cannot:*

- Learn who is looking a name up - it never sees the client's address, session, or identity.
- Alter the on-chain record, or affect clients resolving through other routers.
- Affect existing connections - they do not depend on names.

**An SMP proxy forwarding lookups**

*can:*

- See that a client communicates with a names router.

*cannot:*

- Read the name being looked up, or the response - both are encrypted between client and names router.

**An impersonator**

*can:*

- Register a confusingly similar name. The restricted alphabet keeps such names visibly different - no homographs; judging similar names remains, as everywhere, with the reader.

*cannot:*

- Register a look-alike of a taken name using other scripts or invisible characters.
- Achieve verified status for a name pointing at someone else's address, or for an address whose name points elsewhere.

**Compromise of the owner's wallet key**

An attacker holding the registration key can repoint or transfer the name. New lookups then lead to the attacker; the true owner's profile claim no longer matches, so the previous verified binding fails rather than transfers. Existing connections are unaffected. This is key custody, the same responsibility as holding the channel's own keys - and an expired name that is not renewed can likewise be registered by someone else, so a name must be treated as an asset: key backed up, renewals kept.

**A passive network observer**

*can:*

- See SimpleX traffic between clients and routers.

*cannot:*

- Distinguish name lookups from any other traffic, or learn which names anyone resolves. Inherited from SMP transport.

### Current gaps

1. **Single-resolver lookups.** A client currently accepts the answer of one names router per lookup. Cross-checking two operators' resolvers is designed but not implemented - see below.
2. **No state proofs.** The names router is trusted to report chain state faithfully; responses do not yet carry proofs verifiable against a chain header.
3. **Early infrastructure.** Names-capable routers are few at launch; resolution requires at least one configured server with the names role.


## Future work

- **Two-resolver agreement.** Resolve each name through two independent operators via two different proxies and compare: agreement is trusted, disagreement is surfaced as a warning and the record is not used. This removes the single-resolver trust noted above and makes record substitution require cross-operator collusion.

- **State proofs.** Responses carrying Merkle proofs against Ethereum state, verified by the client - removing trust in the resolver entirely, with or without agreement.

- **Main namespace launch.** The `.testing` namespace is live for early adopters; the `.simplex` namespace launches after the testing period.

- **Private registration.** Registration currently requires an on-chain transaction from a wallet. A private payment path is planned so that acquiring a name does not itself build a public financial trail.

- **Primary names.** Reverse resolution - from an address to a primary name - retained from ENS, enabling richer display and discovery.

- **DNS-anchored names.** The client already parses arbitrary domains (`name.example.com`) for a future bridge from existing domain ownership to SimpleX names.


## Conclusion

SimpleX names give channels and businesses what public communication needs and links alone cannot provide: an identity that is memorable, that survives the loss of any server, and that no platform, registrar, or court-served intermediary can seize - while the people who look a name up remain invisible, because resolution runs through the same identifier-free network as the communication itself. The naming layer keeps the system honest end to end: fully decentralized on-chain, with no indexer or metadata service, and fully private in access, with no RPC middleman watching who asks. Public names for the few who want them; no identifiers for everyone else.
