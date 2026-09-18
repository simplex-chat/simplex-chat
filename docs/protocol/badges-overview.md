Revision 1, 2026-09-18

# SimpleX Supporter Badges: unlinkable proofs of support

## Table of contents

- [Introduction](#introduction)
  - [What is a badge](#what-is-a-badge)
  - [What a badge gives](#what-a-badge-gives)
  - [Why a badge does not identify its holder](#why-a-badge-does-not-identify-its-holder)
  - [Comparison with Signal badges](#comparison-with-signal-badges)
  - [Non-goals](#non-goals)
- [Architecture](#architecture)
  - [Participants](#participants)
  - [Buying and redeeming](#buying-and-redeeming)
  - [Monthly issuance](#monthly-issuance)
  - [Binding a proof to its context](#binding-a-proof-to-its-context)
  - [Profiles](#profiles)
  - [Files](#files)
  - [Servers](#servers)
  - [Verification on the receiving side](#verification-on-the-receiving-side)
- [Cryptographic primitives](#cryptographic-primitives)
- [Security](#security)
  - [Design objectives](#design-objectives)
  - [Threat model](#threat-model)
  - [Current gaps](#current-gaps)
- [Future work](#future-work)
- [Conclusion](#conclusion)


## Introduction

The SimpleX network has no user accounts. Every other paid communication service depends on an account to know who paid: the payment, the account and everything done with it become one record held by the operator. SimpleX Chat needs revenue to pay for development and for the network, and a paid feature must not create the record that the rest of the design avoids. Supporter badges are the answer: a user pays, receives a credential, and uses it in a way that reveals nothing about who they are or what they do.

### What is a badge

A badge is a credential issued by SimpleX Chat to a user who pays for support. It is a [BBS signature](https://datatracker.ietf.org/doc/draft-irtf-cfrg-bbs-signatures/) over four values: a secret master key held by the user, the expiry date, the badge type (`supporter` or `legend`), and a reserved field that is empty.

The credential itself is never sent to anyone. Whenever the badge is shown or used, the app generates a proof from it. A proof reveals the badge type and the expiry date and hides the master key and the issuer's signature. Each proof is generated afresh and is bound to the place where it is presented: a proof made for one contact, one file or one server session is valid nowhere else. Proofs generated from the same credential cannot be linked to each other or to the purchase, even by the issuer.

One credential serves two purposes. To people, it is a badge shown on the profile. To servers and to the app on the other side of a file transfer, it is a proof that the holder is entitled to more than the default. In the transport protocols this second role is called an *entitlement*: a name, an expiry and an extra string, disclosed in a proof. The badge is what the chat application makes of an entitlement.

A badge is neither an account nor an identifier, and it has no monetary value. Nothing changes for users without one.

### What a badge gives

- **A badge on the profile**, shown to contacts, group members and channel subscribers.

- **Larger files.** The default limit is 1GB. A supporter can send files up to 2GB, a legend up to 5GB. The recipient's app verifies the sender's proof before accepting a file above the default limit.

- **Longer file storage.** An XFTP server keeps files for 48 hours by default. A server may configure a longer time for each badge type - the reference configuration is 7 days for supporters and 21 days for legends - and grants it when the client presents a proof.

- **Backups** (planned). A backup will be a link whose content is updated in place by the app, so that the same link restores the latest state. The badge will raise the size of the backup and the time the servers keep it - and that time is how long the app can stay offline before the backup is lost.

- **Better terms on servers** (planned). Cheaper creation of messaging queues, file chunks and notification tokens, and more lenient rate limits, on servers that verify the proof. See [Future work](#future-work).

### Why a badge does not identify its holder

Paying for a service usually identifies the payer to it, and identification is what makes every later action attributable. A badge separates two things that conventional services join:

1. **The credential from its presentations.** The service issues a credential to a purchase key that the app generates for the purchase. It never learns where the badge is later shown or used, because a proof discloses nothing that points back to the credential. A contact, a group, a channel relay or a file server that receives a proof learns only the badge type and the expiry.

2. **Presentations from each other.** Every profile update, every file and every server session receives a new proof, and no two proofs can be linked. A server that received a proof in one session cannot tell whether the next session is from the same holder.

What is not separated is the payment from the purchase. The service may hold a card payment beside the purchase key. A user who wants the payment itself to be private pays in Monero. Either way, the payment record ends at the service: it is not linked to a profile, a contact or a server.

The disclosed values are the same for everyone who bought a badge in the same week, because every credential issued in a week has the same expiry. A proof therefore places its holder in the set of all supporters of that week, and no narrower.

This design continues the [commercial model proposed in 2024](../rfcs/2024-04-26-commercial-model.md), in which the software vendor issues certificates that users present to infrastructure operators.

### Comparison with Signal badges

Signal's donation badges are the closest existing design. Signal uses a [receipt credential](https://eprint.iacr.org/2019/1416.pdf): the client obtains it against a payment, without revealing the account, and then presents it once to the account server, which records the credential's serial number, level and expiry against the account and attaches the badge to the account. The payment is not linked to the account cryptographically. The same mechanism funds Signal's paid backups.

The differences follow from Signal having accounts and SimpleX having none:

- **What the server knows.** Signal's server knows every account that holds a badge, since the badge is a property of the account. In SimpleX there is no account, and no server holds a list of badge holders; a server sees a proof for the duration of one session and cannot link it to the next.

- **Who can verify.** Signal's credential can be verified only by the server that issued it, so it can only be presented to Signal. A SimpleX badge is verified against the issuer's public key by anyone who has it: contacts, group members, channel relays and independently operated servers.

- **How many times.** Signal's credential is presented once and the badge is then an attribute of the account. A SimpleX credential is presented again and again, each time with a new unlinkable proof bound to its context.

- **Timing.** Signal's client requests the credential and redeems it in one uninterrupted sequence, so the server can correlate the request made under the payment with the redemption made under the account by time and by network address. The expiry of a Signal credential is rounded up to the next day, which makes all credentials of a day identical. In SimpleX the renewal request and the profile update that follows it fall on different days, and all credentials of a week have the same expiry.

| Property | Signal badges | SimpleX badges |
|---|---|---|
| Account required | Yes | No |
| Operator knows who holds a badge | Yes | No |
| Verified by | The issuing server only | Anyone with the issuer's public key |
| Presentations per credential | One | Unlimited, each unlinkable |
| Proof bound to where it is presented | Not needed | Yes |
| Expiry shared by | All credentials of a day | All credentials of a week |
| Issuance and use on different days | No | Yes |
| Usable on independently operated servers | No | Yes |

### Non-goals

Badges do not attempt to:

- **Restrict anything that is available today.** Every default stays as it is; a badge adds to it.
- **Create an identity.** A badge has no persistent identifier and links nothing across chats. Incognito profiles show no badge.
- **Be transferable.** A code can be redeemed once, by whoever has it. The credential it yields is usable only with the master key it was issued for, which is held by the app that redeemed the code.
- **Exempt anyone from resource limits.** A credential can be presented in any number of sessions that cannot be linked, so a badge can reduce the cost of a resource but cannot remove it.
- **Support revocation.** Credentials are issued for a month at a time instead.

## Architecture

The introduction established what a badge provides and why it does not identify its holder. This section describes how: who takes part, how a badge is bought, how it is renewed, and how proofs are bound to their context and verified.

```
   ┌─────────────────┐   code    ┌──────────────┐  credential   ┌──────────┐
   │  Payment (web)  │ ────────> │   Holder     │ <──────────── │  Issuer  │
   └─────────────────┘           │  (the app)   │  every month  └──────────┘
                                 └──────┬───────┘
                                        │  a new proof each time
              ┌─────────────────────────┼───────────────────────┐
              │                         │                       │
         ┌────▼─────┐             ┌─────▼──────┐          ┌─────▼──────┐
         │ Contacts,│             │ File       │          │ The user's │
         │ groups,  │             │ recipients │          │ own servers│
         │ channels │             └────────────┘          └────────────┘
         └──────────┘
```

### Participants

- **The issuer** is the badge service: a bot on the SimpleX network, reached through a contact address like any other service. It holds the current issuer secret key. Apps and servers hold a list of eight issuer public keys, numbered; every credential names the number of the key that signed it. The service can move to the next key without an app or server release, and a new list is released before the old one is exhausted.

- **The holder** is the app. The credential is stored in the user's profile, proofs are generated on the device, and a background task renews the credential every month.

- **The verifiers** are contacts, group members, channel relays, file recipients and the user's own servers. Each holds the issuer public keys and nothing else.

### Buying and redeeming

A badge is bought as a code on the web, for one or more months, and redeemed in the app. The code is the only thing that passes from the web site to the app: the site has no knowledge of the app, and the app none of the payment.

A code is `SB` followed by twenty characters from an alphabet without ambiguous letters, in four groups of five, with a check character that catches every single mistyped character. It can be typed in either case, and `I`, `L` and `O` are read as `1` and `0`. The service stores only a hash of each code, so a code cannot be recovered from it.

To redeem a code the app generates a master key and a purchase key pair, and sends the code and the master key to the service, signed with the purchase key. The request is sent over the SimpleX network to the service's contact address, on a connection created for this request and deleted after the reply, so the service does not learn the client's network address. The service verifies the signature, signs the credential, and only then records the code as spent - a failure to sign leaves the code unspent. An unknown code and a mistyped one receive the same reply, so a guesser learns nothing from the difference. A code redeemed twice with the same purchase key returns the same credential; redeemed from another device, it is refused.

A compensation badge, or any badge issued without a sale, is a code minted by the operator and redeemed by the same steps.

### Monthly issuance

A credential is issued for one month at a time, however many months were bought. The service holds the balance of paid months against the purchase key and issues the next credential when the app asks for it. The app shows the user one date: the day their support ends.

Issuing monthly is a privacy measure. A credential valid for a year would disclose an expiry date shared with almost no one, and the same date would be disclosed in every proof for a year. Instead the expiry of every credential is the end of the Monday following the end of the paid month (UTC), so all credentials issued in a week have the same expiry, and a proof reveals only that its holder is one of that week's supporters.

Renewal is split over two days for the same reason. The app requests the next credential the day before the expiry of the current one, while the current one is still valid. It starts using the new credential, and sends the updated profile to contacts, when the old one has expired. The request to the service and the profile update that follows it fall on different days, so nobody who could observe both can pair them by time.

The renewal runs in the background and needs no action from the user. If the app is offline when a renewal is due, it renews on the next start. Recipients accept a badge for seven days after its expiry, and servers for one day, so a renewal that is a little late is invisible to contacts.

### Binding a proof to its context

A BBS proof is generated over a context string, and it is valid only under the same string. This is what makes a proof useless anywhere but where it was presented. The context is not sent with the proof to servers - both sides know it - and is sent with the proof between apps, where the recipient checks that it is the context it expects.

The contexts are:

- **A conversation.** The same string that message signatures are computed over: for a direct chat, a hash derived from the end-to-end encryption state that only the two sides hold; for a group, the member's identifier and the member's signing key; for a channel, the channel's identifier and the member's identifier.
- **A file invitation:** the conversation and the file size.
- **A file description:** the conversation, the file size, a hash of the description - the file's size, digest and encryption key, and the digests of its chunks - and the time until which the file is stored on the servers.
- **A server session:** the TLS session identifier, which both the client and the server derive from the TLS handshake and which cannot be reproduced on another connection.

### Profiles

Every time the app sends its profile - when connecting to a contact, joining a group, or updating the profile - it generates a new proof and includes it in the profile. An incognito profile never includes a badge.

Profile proofs are not yet bound to the conversation: they are generated over a random value instead of the conversation's context, so a proof copied from a received profile would be accepted in another profile. Binding profile proofs to the conversation is the next step - a group member's badge accepted only from a message signed with that member's key, a channel member's badge verified against the key established by the channel's roster, and a contact's badge bound to the connection.

### Files

A file above the default limit is sent with two proofs. The first is in the file invitation, the message that announces the file; it is bound to the conversation and to the file size, and the recipient's app verifies it when the invitation arrives and records whether the file may be received. The second is in the file description, the record of where the file's chunks are stored and how to decrypt them; it is bound also to the description's hash and to the file's storage time, and the app verifies it before the download starts. A proof lifted from a profile cannot authorise a file, and a proof made for one file cannot authorise another.

When a file is forwarded to a member joining a group, the forwarding app includes the proofs stored with the file; for a file of its own it generates new proofs when the stored ones have expired.

The sender's app applies the limit too: it refuses to send a file above the limit its own badge allows, and stops applying the badge one day after the badge's expiry, while recipients accept it for seven, so a file sent on the last day is never rejected.

### Servers

The client presents the entitlement proof in the transport handshake, bound to the TLS session identifier. The server verifies it once, when the session is established, and applies the result to everything the client does in that session. A proof naming a badge type the server does not configure is ignored without verification.

The client presents the proof only to its own servers - the servers configured for its profile, identified by the certificate fingerprint that TLS pins - and never to a server whose address it received from a contact or in a file description. A file description from another party therefore cannot direct the client to present its entitlement elsewhere.

On XFTP servers the proof extends file storage time. When the client creates a file it may ask for a storage time; the server grants the smaller of the request and the maximum it configures for the badge type, or that maximum when nothing is requested, and returns the resulting expiry, which the client passes to the recipients. A server refuses to start with a badge maximum below its default, so a proof never shortens storage.

SMP and notification servers accept the same proof in their handshakes since SMP protocol version 22 and notifications protocol version 4, and do not yet make use of it.

### Verification on the receiving side

A received proof is verified against the issuer key it names, and the result is stored with the profile or the file. The badge is then shown as active until seven days after its expiry; between seven and 38 days it is shown as expired; later it is hidden. A proof naming an issuer key this app version does not know is stored as unknown and verified again after an app update; a proof that fails verification is stored as failed. A badge type the app does not know is accepted and kept by its name.

For the exchange between the app and the badge service - commands, responses and errors - see the [badge service protocol](./badges-rpc.md).


## Cryptographic primitives

- **BBS signatures** ([IETF draft](https://datatracker.ietf.org/doc/draft-irtf-cfrg-bbs-signatures/)), BLS12-381-SHA-256 suite, implemented by [libbbs](https://github.com/Fraunhofer-AISEC/libbbs) over [blst](https://github.com/supranational/blst). A credential is a signature over four values under the header `SimpleX badges v1`; a proof discloses three of them and is 304 bytes. BBS was chosen because a proof can be verified by anyone who holds the issuer's public key, discloses only the chosen values, and cannot be linked to another proof of the same credential. Credentials of the kind Signal uses can be verified only by their issuer; single-use tokens such as blind signatures and Privacy Pass are spent when presented and hold no attributes such as an expiry or a type.

- **Ed25519** - purchase keys, which sign requests to the badge service, and member keys, which are part of the conversation context in groups.

- **SHA-256** - hashes of codes. **SHA-512** - the hash of a file description, part of the context of a file proof.

- **TLS channel binding** - the session identifier that binds a proof to one server connection.

- **Double ratchet** (inherited from the [SimpleX agent layer](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md)) - a hash of its associated data is the context of a direct conversation, and its encryption protects every proof sent between apps.


## Security

This section examines what the design protects against, where it holds, and where gaps remain.

### Design objectives

1. **Unlinkability.** A proof reveals nothing that links it to another proof or to the purchase.
2. **Context binding.** A proof authorises nothing outside the session, conversation or file it was made for.
3. **Timing indistinguishability.** All credentials of a week have the same expiry; renewal requests and the profile updates that follow them fall on different days.
4. **Forgery resistance.** Issuer keys are fixed in apps and servers, and the app verifies a credential before storing it.
5. **Absence of identifiers.** A badge adds nothing that persists across chats; incognito profiles are unaffected.
6. **Safe defaults.** A missing or failed proof leaves the default limit in place; a server cannot be configured to give a badge less than the default.
7. **Reliable renewal.** A credential is renewed before its expiry, accepted for a time after, and the background task recovers from any interruption.

### Threat model

This threat model assumes the [SimpleX network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md) and addresses threats specific to badges.

**The badge service**

*can:*

- See purchase keys, master keys and the months bought under each, and hold payment records beside them.
- Issue any credential - it holds the issuer key.
- Refuse to issue.

*cannot:*

- Link a purchase to a profile, a contact, a group or a server session.
- See where a badge is shown or used.
- See the client's network address.

**A contact, group member or channel relay**

*can:*

- See the badge type and expiry.
- Reuse a profile proof in another profile, until profile proofs are bound to the conversation ([gap 1](#current-gaps)).

*cannot:*

- Link the badge to the same person in another chat.
- Reuse a file proof.
- Tell the holder apart from other supporters of the same week by anything in the proof.

**A server operator**

*can:*

- See the badge type and expiry once per session.
- Group the sessions of one week's supporters.

*cannot:*

- Reuse the proof on another connection.
- Link a session to the purchase, to a profile, or to sessions on other servers, beyond the week of expiry.
- Obtain a proof from a client that does not use this server.

**One badge on many machines**

A server cannot count the sessions of one credential, so whatever a badge grants must be bounded in a way that does not depend on counting. This is why a badge reduces a cost and never removes it, and why limits per session stay in place.

**Compromise of the user's device or backup**

An attacker who obtains the credential and the purchase key can use the badge until its expiry and renew it while months remain. The loss is bounded by the months bought. There is no revocation.

**Interception of a code**

A code is a bearer secret until it is redeemed. Once redeemed, it is refused to anyone else.

**A passive network observer**

*can:*

- See SimpleX traffic between clients and servers.

*cannot:*

- See a proof - proofs to servers are inside TLS, proofs to people inside end-to-end encrypted messages - or tell a session with a proof from one without.

### Current gaps

1. **Profile proofs are not bound to the conversation.** A proof copied from a received profile would be accepted in another profile. See [Profiles](#profiles).
2. **SMP and notification servers do not verify the proof.** The handshake field exists; the servers ignore it.
3. **Small weekly sets.** While few badges are sold, the supporters of a week are a small group.
4. **Simultaneous renewal.** Every app whose credential has the same expiry requests renewal at the same moment.
5. **No revocation.** A stolen credential is usable until its expiry.


## Future work

- **Backups** stored on XFTP servers, with the badge extending their size and storage time, as described in [What a badge gives](#what-a-badge-gives).

- **Use on SMP and notification servers.** A server that verifies the proof can charge a badge holder less for creating queues, file chunks and notification tokens - under the planned proof-of-work scheme, the required effort is divided by a factor configured for each badge type - and apply more lenient rate limits and larger quotas. This closes gap 2.

- **Profile proofs bound to the conversation**, and rejection of unbound proofs once released apps present bound ones. This closes gap 1.

- **Renewal requests spread over the day** before expiry, instead of all at once. This closes gap 4.

- **Other ways to pay:** invoices with card and crypto payment in the app, app store purchases, subscriptions, transfer of remaining months to a new device, and pausing a prepaid badge.


## Conclusion

Supporter badges let a user pay SimpleX Chat and receive more from the network without anyone - the badge service, contacts, relays or servers - being able to tell who they are or link what they do. The credential itself is never presented; each use is a fresh proof bound to its context, and all proofs of a week disclose the same values. The same mechanism that shows a badge on a profile proves an entitlement to a server, and is the basis for larger files, longer storage, backups and better terms on servers. These properties depend on a network without user identifiers: a badge on an account would identify its holder to the operator, however the payment was made.
