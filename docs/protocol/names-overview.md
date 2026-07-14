Revision 1, 2026-07-14

# SimpleX Public Names for Channels and Businesses

## Table of contents

- [Introduction](#introduction)
  - [Names](#names)
  - [Use cases](#use-cases)
  - [On-chain registry](#on-chain-registry)
  - [Privacy considerations](#privacy-considerations)
- [Architecture](#architecture)
  - [Names and records](#names-and-records)
  - [Resolution](#resolution)
  - [Private decentralized RPC layer](#private-decentralized-rpc-layer)
  - [Claiming and verification](#claiming-and-verification)
- [Differences from ENS](#differences-from-ens)
- [Security](#security)
  - [Design objectives](#design-objectives)
  - [Threat model](#threat-model)
  - [Current gaps](#current-gaps)
- [Future work](#future-work)
- [Conclusion](#conclusion)


## Introduction

The SimpleX network provides communication without user identifiers - people connect by exchanging links out of band. This protects private communication, but public entities - channels and businesses - need to be discoverable. Today this requires distributing a long link, which is hard to remember and can be deleted by the router that hosts it. SimpleX names provide discoverability without adding identifiers for people.

### Names

A SimpleX name is a human-readable name, registered on a public blockchain, that resolves to SimpleX links: `#name.simplex` opens a channel (for the `.simplex` namespace, the short variant `#name` can also be used) and `@name.simplex` connects to a contact or business address. One name can include both links - a business can publish `@name.simplex` for customer conversations and `#name.simplex` for its announcement channel from a single registration. Names support subnames (`support.name.simplex`). A separate test namespace (`.testing`) is deployed for early adopters before the main namespace launches.

A name is not an account and not an identity. It is a record that maps a human-readable string to the links a channel or business already has, controlled by a cryptographic key that only its owner holds. The network does not use or require names; they are a discovery layer on top of it, described in this document.

This document covers the naming layer built for [SimpleX Channels](./channels-overview.md) and the [SimpleX network](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md). For the user-facing registration steps see [Registering a SimpleX name](../guide/register-simplex-name.md); for protocol-level details see the resolver commands in the [SMP protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md).

### Use cases

Names are intended for entities that are already public:

- **Channels.** A channel reachable as `#name` can be easily shared in many ways - conversation, print, social media, and messages. If the channel's link is destroyed or blocked, the owner points the name at a new link, and the channel remains reachable by name.

- **Businesses and organizations.** A business address reachable as `@name.simplex` gives customers a way to start a conversation without scanning a QR code or trusting a link from a search result.

- **Creators and public figures.** Anyone publishing to an audience can be reachable by name while their audience remains private.

Names are opt-in and are only useful for public entities. Private users do not need them. Even when an entity uses a name, the network cannot track who connects to it - the communication graph remains private.

### On-chain registry

Names must be globally unique, so they require a shared registry. Every conventional registry design places names under an operator's control:

- **A platform registry** (usernames in messaging apps) is owned by the operator: names can be revoked, impersonated, or reassigned, and the platform sees who looks up whom.

- **DNS** is subject to domain seizure at the registrar and registry level, and requires registrant records.

A record on a public blockchain has no operator: only the holder of the registration key can change what a name points to, and no authority can delete it. This mitigates censorship at the levels where links fail:

- **A link is hosted by one SMP router.** That router's operator can delete it, and the link stops working for everyone who saved it. A name is not affected: the owner points it at a new link on a different router, and clients connect by name as before.

- **Registries can be pressured.** An on-chain record cannot be seized from its owner or removed by a court order served on an intermediary, because there is no intermediary.

### Privacy considerations

Names do not:

- **Identify users.** There is no user directory, no requirement to register, and nothing links a name to the people who read or contact it.

- **Put communication on the chain.** The blockchain stores only the mapping from a name to links and optional public profile fields. Messages, membership, and channel content are never stored on the blockchain.

- **Certify identity.** A verified name proves that the name's owner controls the address it points to - not who the owner is. It is the same trust model as a domain name.

- **Replace links.** Links remain the primary connection mechanism and the more private one, since resolving a name reveals interest in it to one resolver operator. Names are for public entities that choose to be discoverable.


## Architecture

### Names and records

Names are lowercase labels of ASCII letters, digits, and single hyphens, forming domains under the `.simplex` top-level name (`privacy.simplex`, `my-channel.simplex`), with `.simplex` implied when omitted for channels: `#privacy` is interpreted as `#privacy.simplex`. The restricted alphabet prevents homograph attacks: visually identical names from mixed scripts cannot exist as distinct records.

A name's on-chain record stores:

- **Channel links and contact links** - each an ordered list, primary first. Multiple links give redundancy across SMP routers: clients try them in order, so the name remains usable if one router becomes unavailable. Client support for this redundancy is planned for the `.simplex` namespace launch.
- **Optional profile fields** - a display name, website, location.
- **Optional donation addresses** (Monero, Bitcoin, Ethereum, etc.) that apps can show to channel subscribers.

Registration is a two-transaction commit-reveal process from the owner's wallet: the first transaction records only a hash, so an observer of the pending registration cannot see the name and front-run it; the second transaction reveals the name and completes the registration. The name is held as an ordinary token in the owner's wallet and is renewable and transferable. Subnames are created by the name's owner, are transferred together with the name, and cannot be separately sold - the same ownership model as DNS subdomains.

### Resolution

Connecting by name is two independent resolutions:

1. **Name to link.** The client queries the on-chain record through the SimpleX network (next section) and obtains the channel or contact link.

2. **Link to connection.** The link resolves through the existing SimpleX short-link protocol: the client retrieves the link's immutable data - cryptographically bound to the link's owner keys - and the entity's profile, and connects.

The second step is unchanged from connecting by link. A name adds discoverability on top of the link protocol; it does not weaken the link's own verification, and everything a client checks when joining by link is still checked when joining by name.

### Private decentralized RPC layer

Reading blockchain state normally requires querying an RPC service, and in practice almost all applications - including wallets and name services - use a handful of centralized API providers. Those providers see every query, the account it concerns, and the querier's IP address, and can censor or falsify responses. Resolving names through such a provider would reveal every lookup and the client's IP address to a third party.

SimpleX resolves names through the network itself. Operators can enable a *names role* on their SMP routers: a names router runs its own resolver process and its own Ethereum node, holding a full copy of the relevant chain state - operators must not share this backend with other operators. The lookup is a protocol command sent as ordinary SimpleX traffic:

1. The client selects a names router among its configured servers and sends the lookup *through an SMP proxy operated by a different operator*, encrypted so the proxy cannot read it.
2. The proxy forwards the lookup without access to its content; the names router responds from its local chain state.

The result is a knowledge split with no single observer: the proxy sees which client communicates with a names router, but not the name; the names router sees the name, but not which client sent the query - no client address, session, or identity. A passive network observer sees fixed-size encrypted blocks indistinguishable from other traffic. Clients also keep the set of parties that see a lookup minimal: a query is sent to one names router, and after an authoritative response the client does not repeat the name to other servers.

This layer is not specific to names. Anonymous access to blockchain state via independent operators is a network capability that can be used in other contexts in the future.

### Claiming and verification

A name is trustworthy when:

1. **The on-chain record points at the link.** Only the name owner's wallet key can set this.
2. **The link's profile claims the name.** Only the link owner's keys can publish this, and the claim is signed by the link's owner key, binding it to that specific link.

For name resolution to succeed, SimpleX clients require both to be true. This mutual binding prevents abuse, such as:

- Registering a name that points at someone else's address. The profile does not claim the name, so clients refuse to connect to it.
- Claiming a name in a profile without owning it. The on-chain record does not point at that profile's address, so the claim fails verification.

Apps display the outcome next to the name - verified (the record and the claim match), failed, or not yet verified - and can re-verify on demand or automatically. Verification is a fresh resolution through the private RPC layer, so it reflects the current on-chain state.

Names are used only to establish connections, not to deliver messages. Once connected, a contact or channel subscription is an ordinary SimpleX connection, independent of the name. A name expiring or being transferred affects future connections only; existing conversations and subscriptions are not affected.


## Differences from ENS

The on-chain layer is the SimpleX Name Service (SNS), a fork of the [Ethereum Name Service](https://ens.domains) (ENS). ENS is the most widely used decentralized naming system, and SNS retains its core design: the registry and resolver contracts, commit-reveal registration, and expiry-based ownership.

ENS, however, is not fully decentralized in use. Its applications depend on two off-chain services: an indexer (the subgraph) for queries such as listing the names held by an address (without it the ENS app is unusable), and a hosted metadata service to render name tokens in wallets. SNS removes both dependencies, and simplifies the ownership model:

1. **No indexer.** The SNS contracts index names on-chain: reverse lookup from an address to the names it holds, from a token to its plaintext label, and from a name to its subnames are all direct contract reads. Every view in the SNS web app (currently, the app for test names is https://testing-names.simplex.chat) - the list of owned names, their subnames, and each name's records - is served by plain `eth_call`s against any Ethereum node, including a self-hosted one.

2. **Fully on-chain tokens.** A name's token metadata and image are generated by the contract as inline JSON and SVG. There is no metadata server; wallets render the name from chain data alone.

3. **No name wrapper, no separate subname ownership.** ENS's wrapper system lets subnames be split off and owned independently, at the cost of a second token standard and a complex permission system. SNS subnames are owned together with the name and are transferred with it (the DNS model), removing the need for the wrapper.

Resolution also differs. ENS names are resolved by applications through their RPC providers, so lookups are as centralized and observable as the provider. SNS records are read through the private RPC layer described above: many independent operators hold the chain state, and no single party sees who resolves which name.

The result is a naming system with no off-chain services: registration, ownership, enumeration, rendering, and resolution are all either on-chain or served by the SimpleX network itself.

The current implementation uses a web app for registration, which accesses the chain via a centralized RPC provider. We plan to add name purchases to the SimpleX apps, using the same decentralized blockchain access that is already used for name resolution.


## Security

### Design objectives

1. **No seizure.** Only the holder of the name's key can change or transfer the record; there is no registrar, operator, or intermediary with override authority.
2. **No infrastructure censorship.** A name does not depend on any single router: the links it points to can be replaced without changing the name, and resolution uses chain state held by many independent operators.
3. **Impersonation resistance.** A name is shown as verified only when the on-chain record and the signed claim in the link's profile match.
4. **Lookup privacy.** No single party observes both who is looking up and what is looked up.
5. **Availability.** A record can list multiple links across routers, and any names-capable router of the user's choosing can respond to a lookup.

### Threat model

This threat model assumes the [SimpleX network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md) and the [channels threat model](./channels-overview.md#threat-model), and addresses the naming layer.

**A compromised names router (or its chain backend)**

*can:*

- Deny that a name exists, or serve a stale record, to the clients that query it. Detectable by resolving through a different operator's router.
- Serve a false record - effective only against a client that queries this router *and* only if the false record's target cooperates by claiming the name, since the claim check runs against the attacker-supplied record. Resolver agreement across operators ([future work](#future-work)) makes this require collusion of the queried operators.
- Observe which names are looked up, and how often.

*cannot:*

- Learn who is looking up a name - it never sees the client's address, session, or identity.
- Alter the on-chain record, or affect clients resolving through other routers.
- Affect existing connections - they do not depend on names.

**An SMP proxy forwarding lookups**

*can:*

- See that a client communicates with a names router, which is an ordinary SMP router.

*cannot:*

- See that a request forwarded to a destination router is a name resolution request.
- See the name being looked up, or the response - both are encrypted between client and names router.

**An impersonator**

*can:*

- Register a confusingly similar name. The restricted alphabet keeps such names visibly different - homographs are not possible.

*cannot:*

- Register a visually identical variant of an existing name using other scripts or invisible characters.
- Achieve verified status for a name pointing at someone else's address, or for an address whose name points elsewhere.

**Compromise of the owner's wallet key**

An attacker holding the registration key can repoint or transfer the name. New lookups then resolve to the attacker's links; the owner's profile claim no longer matches the record, so clients verifying the owner's profile see a failure. Existing connections are unaffected. Protecting the registration key is the owner's responsibility, the same as protecting the channel's own keys. An expired name that is not renewed can also be registered by someone else, so the key must be backed up and the name renewed on time.

**A passive network observer**

*can:*

- See SimpleX traffic between clients and routers.

*cannot:*

- Distinguish name lookups from any other traffic, or learn which names anyone resolves. Inherited from SMP transport.

### Current gaps

1. **Single-resolver lookups.** A client currently accepts the response of one names router per lookup. Cross-checking two operators' resolvers is designed but not implemented - see below.
2. **No state proofs.** The names router is trusted to report chain state correctly; responses do not yet include proofs verifiable against a chain header.
3. **Self-hosted routers.** All pre-configured routers support name resolution, but most self-hosted servers do not support it yet. Name resolution requires at least one configured server with the names role.


## Future work

- **Two-resolver agreement.** Resolve each name through two independent operators via two different proxies and compare the responses: a match is trusted; a mismatch is shown as a warning and the record is not used. This removes the single-resolver trust noted above and makes record substitution require cross-operator collusion.

- **State proofs.** Responses including Merkle proofs of Ethereum state, verified by the client - removing the need to trust the resolver, with or without agreement.

- **Main namespace launch.** The `.testing` namespace is deployed for early adopters; the `.simplex` namespace launches after the testing period.

- **Registration via an app.** Registration currently requires an on-chain transaction from a wallet.

- **Primary names.** Reverse resolution from an address to a primary name, retained from ENS.

- **DNS-anchored names.** The client already parses arbitrary domains (`name.example.com`) as web links. In the future, it will be possible to register owned domains in the SNS contract to use them as channel and contact names (`#name.example.com` and `@name.example.com`).


## Conclusion

SimpleX names give channels and businesses memorable addresses that do not depend on any single server and cannot be revoked by a platform or registrar. Resolution runs through the SimpleX network, so no party observes who looks up which name. The naming system is fully decentralized on-chain, with no indexer or metadata service, and access to it is private, with no RPC provider observing requests. Names are optional and used only by public entities; private communication in SimpleX remains identifier-free.
